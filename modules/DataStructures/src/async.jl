mutable struct Atom
  @atomic value::Any
end

function deref(a::Atom)
  # REVIEW: I'm pretty sure :monotonic is correct. All writes are coordinated,
  # but the only guarantee when reading an atom is that you'll get a value that
  # was valid at some time before (including) now.
  #
  # :acquire would ensure that we get a more recent value when there's a lot of
  # write contention, but it could also reduce throughput. How does one go about
  # testing that?
  getfield(a, :value, :monotonic)
end

# Same syntax as jl's Ref
#
# REVIEW: Have I just reimplemented (poorly) something already available?
function getindex(a::Atom)
  deref(a)
end

"""
Doesn't necessarily set the atom. Check the return value.
"""
function trycas!(a::Atom, current, next)
  replacefield!(a, :value, current, next, :acquire_release)
end

"""
Atomically set `a`'s value to f(deref(a), args...).

Spins if `a` is changed while `f` is being computed (cas semantics).

N.B.: this could get ugly if lots of threads are changing `a` simultaneously,
but if contention is low and `f` is fast, it can be very efficient.

Also note that `f` will, in general, be invoked multiple times, so it should be
pure.
"""
function swap!(a::Atom, f, args...)
  i = 0
  # REVIEW: is 2^16 too many tries before failing? Probably.
  # TODO: jl has a builtin ExponentialBackOff iterator. Use it.
  while i < 2^16
    i += 1
    current = deref(a)
    next = f(current, args...)
    res = trycas!(a, current, next)
    if res.success
      return next
    end
  end
  throw("Too much contention in atom, aborting to avoid deadlock.")
end

function reset!(a::Atom, value)
  setfield!(a, :value, value, :release)
end

##### Julia Channels
#
## N.B.: These are not threadsafe in the sense that if you reduce on a channel
## multiple times concurrently the different reductions will get different
## subsets of the value stream and there's no way to tell who will get what.
#
## That might be okay in your use case, but probably isn't.
#
## Also, when reading from multiple channels it waits for a message from each in
## the order they're listed. Since progress can't be made until we have a
## message from each, this isn't so bad.

# TODO: Some sort of subscription system where everybody who is interested in
# the values on a channel gets all the values.
#
# The problem here though: what if one of the consumers is being sluggish? We
# can't buffer indefinitely, and in the case of input streams we can't apply
# back pressure (stuff is happening whether we're ready or not). One option is
# to require all input streams themselves to have buffer and drop policies and
# let backpressure fallback to those.
#
# But that would let one slow consumer limit its peers, and worse, potentially
# prevent them from seeing events that they wouldn't have dropped.

# So it looks like each subscriber needs a dropping policy.


# Julia sucks at recursion, so loopify everything
function ireduce(f, init, ch::Channel)
  v = init
  try
    while true # REVIEW:
      v = f(v, take!(ch))
    end
  catch e
    if e isa InvalidStateException && ch.state === :closed
      return v
    else
      rethrow(e)
    end
  end
end

function ireduce(f, init, chs::Channel...)
  v = init
  try
    while true
      xs = [take!(ch) for ch in chs]
      v = f(v, xs...)
    end
  catch e
    if e isa InvalidStateException && !every(x -> x.state === :open, chs)
      return v
    else
      throw(e)
    end
  end
end

##### Streams pub/sub, and integrations with transduction.

abstract type SubStream <: Stream end

struct TailSubStream <: SubStream
  ch::Channel{Any}
end

lock(x::TailSubStream) = lock(x.ch)
unlock(x::TailSubStream) = unlock(x.ch)
isready(x::TailSubStream) = isready(x.ch)

close(s::TailSubStream) = close(s.ch)

take!(s::TailSubStream) = take!(s.ch)

function put!(s::TailSubStream, x)
  lock(s.ch) do
    if s.ch.n_avail_items === s.ch.sz_max
      take!(s.ch)
    end

    put!(s.ch, x)
  end
end

function sub(; buffer = 32, drop=:oldest)
  if drop === :oldest
    TailSubStream(Channel(buffer))
  else
    throw("not implemented")
  end
end

struct PubStream <: Stream
  state::Atom
end

closedp(s::Channel) = s.state === :closed
closedp(s::SubStream) = s.ch.state === :closed
closedp(s::PubStream) = get(deref(s.state), :state) === :closed

function pub()
  PubStream(Atom(hashmap(:state, :open, :subscribers, emptyset)))
end

function put!(s::PubStream, x)
  v = deref(s.state)
  cleanup = false
  if get(v, :state) === :open
    reduce((_, c) -> begin
        if closedp(c)
           cleanup = true
        else
          put!(c, x)
        end
      end,
      0, get(v, :subscribers)
    )

    if cleanup
      swap!(s.state, update, :subscribers, subs -> remove(closedp, subs))
    end
    return nothing
  else
    throw(InvalidStateException("channel is closed.", get(v, :state)))
  end
end

function subscribe(p::PubStream; buffer = 32, drop = :oldest)
  s = sub(;buffer, drop)
  swap!(p.state, update, :subscribers, conj, s)
  return s
end

function close(s::PubStream)
  v = swap!(s.state, assoc, :state, :closed)
  map(close, get(v, :subscribers))
end

function tap(ch)
  function(emit)
    function inner()
      v = emit()
      put!(ch, v)
      return v
    end
    function inner(result)
      v = emit(result)
      put!(ch, v)
      close(ch)
      return v
    end
    function inner(result, next...)
      v = emit(result, next...)
      put!(ch, v)
      return v
    end
  end
end

ChannelLike = Union{Channel, PubStream, SubStream}

# Helpers to treat all these channellikes the same.
listener(ch::Channel) = ch
listener(s::SubStream) = s.ch
listener(p::PubStream) = subscribe(p)

WriteStream = Union{Channel, PubStream}

function send!(ch::WriteStream, val)
  put!(ch, val)
  return ch
end

function into(to::WriteStream, xform, from...)
  Threads.@spawn begin
    try
      transduce(xform, send!, to, from...)
    catch e
      handleerror(e)
    end
  end
  return to
end

function into(to::WriteStream, xform, from::PubStream...)
  into(to, xform, map(subscribe, from)...)
end

function stream(xform, streams::PubStream...)
  into(pub(), xform, streams...)
end

function ireduce(f, init, s::PubStream)
  ireduce(f, init, subscribe(s))
end

function ireduce(f, init, ss::PubStream...)
  ireduce(f, init, map(x -> subscribe(x).ch, ss)...)
end

function ireduce(f, init, s::SubStream)
  ireduce(f, init, s.ch)
end

function ireduce(f, init, s::SubStream...)
  ireduce(f, init, map(x -> x.ch, s)...)
end

##### Stream Operators

"""
Squggol's `scan` operator. I.e. emits each step of a reduction.
"""
function scan(f, init)
  state = init
  function(emit)
    function inner()
      emit()
    end
    function inner(x)
      emit(x)
    end
    function inner(result, next)
      state = f(state, next)
      emit(result, state)
    end
  end
end

# Notably, you can implement `reduce` as
#
# function reduce(f, init, coll)
#   transduce(scan(f, init), lastarg, nil, coll)
# end
#
# One of those degenerate transductions that keep coming up.
#
# This one in particular is really just a reactive value that updates each time
# a new element of `coll` is processed. There is no reduction step, to speak
# of. Just a sequence of values. Except there is, we've just "pulled it back"
# into the transduction.

"""
Returns a pub stream which listens to all `ps` and emits every message it gets
in the order received. Note, there is no way to tell which message came from
which stream when combined this way.
"""
function interleave(ps::PubStream...)
  ls = map(subscribe, ps)
  out = pub()

  function abort()
    close(out)
    map(close, ls)
  end

  for l in ls
    @async begin
      try
        while true
          if closedp(l) || closedp(out)
            abort()
            break
          else
            put!(out, take!(l))
          end
        end
      catch e
        handleerror(e)
      end
    end
  end

  return out
end

"""
Given a map from names to streams return a pub stream which receives every
message from all named streams tagged with the name of the origin
stream. Messages are in the order received by a gaggle of async waiters, so it
had better not be important.
"""
#FIXME: That's a mouthful
function interleave(streams::Map)
  ls = mapvals(subscribe, streams)
  out = pub()

  function abort()
    close(out)
    map(val ∘ close, ls)
  end

  for l in ls
    @async begin
      try
        while true
          if closedp(val(l)) || closedp(out)
            abort()
            break
          else
            put!(out, hashmap(key(l), take!(val(l))))
          end
        end
      catch e
        handleerror(e)
      end
    end
  end

  return out
end

"""
Given a stream of named messages, emit a map containing the last from each
whenever a new message is received.
"""
combinelast(init) = scan(merge, init)
