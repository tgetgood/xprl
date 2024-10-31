abstract type Queue end

struct PersistentQueue <: Queue
  front
  back
end

const emptyqueue = PersistentQueue(emptyvector, emptyvector)

function queue(seq)
  PersistentQueue(seq, emptyqueue)
end

function count(q::PersistentQueue)
  return count(q.front) + count(q.back)
end

function emptyp(q::Queue)
  return count(q) == 0
end

function conj(q::PersistentQueue, x)
  if emptyp(q)
    return PersistentQueue(vector(x), emptyvector)
  else
    return PersistentQueue(q.front, conj(q.back, x))
  end
end

function rest(q::PersistentQueue)
  nf = rest(q.front)
  if emptyp(nf)
    return PersistentQueue(q.back, emptyvector)
  else
    return PersistentQueue(nf, q.back)
  end
end

function first(q::PersistentQueue)
  first(q.front)
end

function closedp(q::PersistentQueue)
  false
end

function emptyp(q::PersistentQueue)
  count(q.front) == 0 && count(q.back) == 0
end

## Closed Queues

struct ClosedQueue <: Queue
  elements
end

const closedempty = ClosedQueue(emptyqueue)

function closedp(q::ClosedQueue)
  true
end

function close(q::PersistentQueue)
  ClosedQueue(concat(q.front, q.back))
end

function close(q::ClosedQueue)
  q
end

function first(q::ClosedQueue)
  first(q.elements)
end

function rest(q::ClosedQueue)
  ClosedQueue(rest(q.elements))
end

function count(q::ClosedQueue)
  count(q.elements)
end

function emptyp(q::ClosedQueue)
  count(q) == 0
end

function string(q::PersistentQueue)
  # TODO: limit printing on large structures
  "<-<" * transduce(interpose(", "), *, "", concat(q.front, q.back)) * "<-<"
end

## (Semi) Mutable Queues
#
# These are something of an odd beast. We want immutable semantics when reading
# queues, but not when writing. Consequently I've implemented `first`/`rest` to
# work as expected, but these queues do not support `conj`. Rather they have a
# method `put!` which extends the (possibly) shared tail of a queue.
#
# To avoid confusion, you can't call `put!` directly on a queue, so they appear
# to be read-only. Only by having a reference to the tail of the queue can you
# extend it.
#
# Of course these queues have references to their tails (it's conceivable that
# they could be engineered so as to have no knowledge of their tails, but I've
# yet to be that clever), which lets you hack around that. So I need discipline.

struct MutableTail
  lock::ReentrantLock
  ch::Channel
  listeners::Base.Vector{WeakRef}
end

function readyp(t::MutableTail)
  length(t.queue) > 0
end

function rotateinternal(tail::MutableTail)
  vals = []
  i = 1

  # park until we have at least 1 value.
  push!(vals, take!(tail.ch))

  # Take up to 32 values at a time to prevent hijacking by noisy producers.
  while isready(tail.ch) && i < 32
    push!(vals, take!(tail.ch))
  end

  for q in tail.listeners
    if q.value === nothing
      continue
    else
      q = q.value
      q.front = Base.reduce(conj, vals, init=q.front)
    end
  end

  # Clean up GCed listeners
  Base.filter!(x -> x.value !== nothing, tail.listeners)
end

function rotate!(tail::MutableTail)
  lock(() -> rotateinternal(tail), tail.lock)
end

function listen!(tail::MutableTail, x)
  lock(() -> push!(tail.listeners, WeakRef(x)), tail.lock)
end

function put!(tail::MutableTail, v)
  # We depend on the underlying channel impl for back pressure.
  put!(tail.ch, v)
end

abstract type Stream <: Queue end

mutable struct MutableTailQueue <: Stream
  front::Vector
  const tail::MutableTail
end

# Multiple Queues can share a mutable tail.
#
# Unfortunately, this requires some complexity
function mtq(f, t)
  q = MutableTailQueue(f, t)
  listen!(t, q)
  return q
end

function mtq()
  mtq(emptyvector, MutableTail(ReentrantLock(), Channel(32), []))
end

## FIXME: This is broken with channels, but I'm not sure I need it.
# function close(q::MutableTailQueue)
#     xs = lock(() -> Base.reduce(conj, q.tail.queue, init=q.front), q.tail.lock)
#     ClosedQueue(xs)
# end

function emptyp(q::MutableTailQueue)
  # A queue with a mutable tail is never empty, since it's always *possible*
  # more data will be written to it. We're not concerned with what happens to
  # be enqueued just now.
  false
end

function first(q::MutableTailQueue)
  if emptyp(q.front)
    rotate!(q.tail)
  end
  return first(q.front)
end

function rest(q::MutableTailQueue)
  if emptyp(q.front)
    rotate!(q.tail)
  end
  mtq(rest(q.front), q.tail)
end

emptystream = mtq

function stream(val)
  return mtq(vector(val), mtq().tail)
end

function put!(q::MutableTailQueue, x)
  put!(q.tail, x)
end

function bind(q::MutableTailQueue, t::Task)
  @async begin
    wait(t)
    close(q)
  end
end

function string(q::MutableTailQueue)
  "<-(" * transduce(map(string) ∘ interpose(", "), *, "", q.front) * ",...)-<"
end
