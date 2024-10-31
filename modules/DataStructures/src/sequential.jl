abstract type Sequential <: Sexp end

""" Returns `true` iff the collection `x` contains no elements. """
emptyp(x::Sequential) = count(x) == 0
emptyp(x) = length(x) == 0
emptyp(x::Nothing) = true

length(v::Sequential) = count(v)

abstract type EarlyTermination end

struct Reduced{T} <: EarlyTermination
  value::T
end

struct PushbackReduced{T, V} <: EarlyTermination
  value::T
  unconsumed::V
end

struct NoEmission end
const none = NoEmission()

conj(c, _::NoEmission) = c

function reduced(x)
  throw(Reduced(x))
end

function reduced(val, xs...)
  throw(PushbackReduced(val, xs))
end

function reduce(f, coll)
  reduce(f, f(), coll)
end

empty(_::Nothing) = nothing

function handleabort(default, r::EarlyTermination)
  if r.value === none
    default
  else
    r.value
  end
end

function handleabort(_, r)
  rethrow(r)
end

defaultreturn(default, _::NoEmission) = default
defaultreturn(_, v) = v

function reduce(f, init, coll...)
  try
    defaultreturn(init, ireduce(f, init, coll...))
  catch r
    handleabort(init, r)
  end
end

# Fallback reduce impl for anything sequential
function ireduce(f, init, coll)
  if emptyp(coll)
    init
  else
    ireduce(f, f(init, first(coll)), rest(coll))
  end
end

# Fallback impl for multiple generic sequences
function ireduce(f, acc, lists...)
  if every(!emptyp, lists)
    ireduce(f, f(acc, map(first, lists)...), map(rest, lists)...)
  else
    acc
  end
end

##### TODO: split/join functions for the from/to collections to allow parallel
##### transduction. This will require knowing which transducers can be run in
##### parallel.
function itransduce(g, to, from...)
  defaultreturn(to, g(reduce(g, to, from...)))
end

function transduce(xform, f, to, from...)
  itransduce(xform(f), to, from...)
end

function transduce(xform, f, from...)
  g = xform(f)
  itransduce(g, g(), from...)
end

## Fallback `into` implementations.
##
## Collection specific specialisations are recommended.
into() = emptyvector
into(x) = x
into(to, from) = reduce(conj, to, from)
into(to, xform, from...) = transduce(xform, conj, to, from...)

function drop(n)
  function (emit)
    function inner()
      emit()
    end
    function inner(result)
      emit(result)
    end
    function inner(result, next)
      if n === 0
        emit(result, next)
      else
        n -= 1
        result
      end
    end
    return inner
  end
end

drop(n, coll) = into(empty(coll), drop(n), coll)

function take(n)
  function (emit)
    function inner()
      emit()
    end
    function inner(result)
      emit(result)
    end
    function inner(result, next)
      if n === 1
        reduced(emit(result, next))
      else
        n -= 1
        emit(result, next)
      end
    end
    return inner
  end
end

take(n, coll) = into(empty(coll), take(n), coll)

conj() = emptyvector
conj(x) = x

concat(xs, ys) = into(xs, ys)

# This is an odd one. The "natural" implementation for `every` throws away the
# accumulator arg since only the last one matters. But that can't be composed
# with other transducers.
#
# As is, this can be composed, but will short circuit and throw away the
# downstream computation the first time `p` evaluates to `false`. I think that's
# the correct behaviour, but it's still weird...
function every(p)
  started = false
  aborted = false
  function (emit)
    function inner()
      emit()
    end
    function inner(result)
      # Universal quantification over the empty set
      if !started
        # REVIEW: `true` might be the wrong thing to return here. I don't know.
        # I think I'll need to bump into this particular corner before I know
        # how to fix it.
        emit(true)
      elseif aborted
        emit(none)
      else
        emit(result)
      end
    end
    function inner(result, next...)
      started = true
      if p(next...)
        emit(result, next...)
      else
        aborted = true
        reduced(none, next...)
      end
    end
    return inner
  end
end

lastarg(xs...) = xs[end]

function every(p, xs...)
  marker = gensym()
  transduce(every(p), lastarg, marker, xs...) !== marker
end

# REVIEW: I could make this a transducer, but do I have any use for that?
#
# Early abort seems like it would be incorrect in this case since we want the
# downstream computation to consume the entire input stream even if `any` is
# good to return after the first element.
#
# This is the first time I've come across a transducer whose behaviour might be
# different if it has a downstream computation vs not having one. That's a
# little unsettling. It violates the autonomy from within of the computational
# units.
any(p, xs::Sequential...) = !every(!p, xs...)

function cat()
  function (emit)
    function inner()
      emit()
    end
    function inner(result)
      emit(result)
    end
    function inner(result, next)
      reduce(emit, result, next)
    end
    function inner(result, next::Base.Vector)
      Base.reduce(emit, next, init=result)
    end
    return inner
  end
end

function map(f::Function)
  function (emit)
    function inner()
      emit()
    end
    function inner(result)
      emit(result)
    end
    function inner(result, next)
      emit(result, f(next))
    end
    function inner(result, next...)
      emit(result, f(next...))
    end
    inner
  end
end

function map(f, xs::Sequential)
  into(empty(xs), map(f), xs)
end

function map(f, xs::Sequential, more::Sequential...)
  into(empty(xs), map(f), xs, more...)
end

function mapindexed(f::Function)
  index = 0
  function (emit)
    function inner()
      emit()
    end
    function inner(result)
      emit(result)
    end
    function inner(result, next...)
      index += 1
      emit(result, f(index, next...))
    end
    inner
  end
end

function mapindexed(f::Function, xs::Union{Base.Vector, Sequential}...)
  into(empty(xs), mapindexed(f), xs...)
end

function filter(p::Function)
  function (emit)
    function inner()
      emit()
    end
    function inner(result)
      emit(result)
    end
    function inner(result, next)
      if p(next) == true
        emit(result, next)
      else
        result
      end
    end
    inner
  end
end

function filter(p, xs::Sequential)
  into(empty(xs), filter(p), xs)
end

function interpose(delim)
  function (emit)
    started = false
    function inner()
      emit()
    end
    function inner(res)
      emit(res)
    end
    function inner(res, next)
      if started
        return emit(emit(res, delim), next)
      else
        started = true
        return emit(res, next)
      end
    end
    return inner
  end
end

remove(p) = filter(!p)
remove(p, xs) = filter(!p, xs)

function aftereach(delim)
  function (emit)
    function inner()
      emit()
    end
    function inner(res)
      emit(res)
    end
    function inner(res, next)
      emit(emit(res, next), delim)
    end
    return inner
  end
end

function partition(n)
  acc = []
  function (emit)
    function inner()
      emit()
    end
    function inner(result)
      if count(acc) > 0
        emit(emit(result, acc))
      else
        emit(result)
      end
    end
    function inner(result, next)
      push!(acc, next)
      if count(acc) == n
        t = vec(acc)
        acc = []
        emit(result, t)
      else
        result
      end
    end
    return inner
  end
end

function partition(n, xs)
  into(empty(xs), partition(n), xs)
end

function dup(emit)
  function inner()
    emit()
  end
  function inner(acc)
    emit(acc)
  end
  function inner(acc, next)
    emit(emit(acc, next), next)
  end
  return inner
end

function prepend(head)
  function (emit)
    function inner()
      reduce(emit, emit(), head)
    end
    function inner(res)
      emit(res)
    end
    function inner(res, next)
      emit(res, next)
    end
  end
end

function repeat(xs...)
  RepeatingSeq(xs, 1)
end

struct RepeatingSeq
  els::Tuple
  i::Int
end

first(s::RepeatingSeq) = s.els[s.i]

function rest(s::RepeatingSeq)
  l = length(s.els)
  if l === 1
    s
  else
    RepeatingSeq(s.els, (s.i % l) + 1)
  end
end

emptyp(s::RepeatingSeq) = false

# Actually it's infinite... but `Inf` is a float thing
count(_::RepeatingSeq) = typemax(Int)


function interleave()
  function (emit)
    function inner()
      emit()
    end
    function inner(result)
      emit(result)
    end
    function inner(result, xs...)
      reduce(emit, result, xs)
    end
  end
end

function zip()
  function (emit)
    function inner()
      emit()
    end
    function inner(result)
      emit(result)
    end
    function inner(result, xs...)
      emit(result, vec(xs))
    end
  end
end

zip(colls...) = transduce(zip(), conj, emptyvector, colls...)

##### More exotic transduction ideas

"""
Injects a stream `ys` into the pipeline so that the next transducer sees one
more argument at each step.

Terminates pipeline when `ys` is exhausted.
"""
function inject(stream)
  ys = stream
  function (emit)
    function inner()
      emit()
    end
    function inner(result)
      emit(result)
    end
    function inner(result, x...)
      y = first(ys)
      ys = rest(ys)
      v = emit(result, x..., y)

      if emptyp(ys)
        reduced(emit(v))
      else
        v
      end
    end
  end
end

maybe(emit, result, v::NoEmission) = emit(result)
maybe(emit, result, v) = emit(result, v)

stateupdate(_, _, a) = a
stateupdate(x::PushbackReduced, g, a) = g(a, x.unconsumed...)

"""
Breaks a stream into a stream of streams, each the output of one
transduction. If a transduction doesn't call reduced, the next in line will
never see input.

Example:
into(emptyvector, ds.seqcompose(
  (take(2), conj, emptyvector),
  (take(4) ∘ map(inc), conj, emptyvector),
  (take(1) ∘ map(x -> (x,x)), conj, emptymap)
), 1:10)

will output: [[1 2] [4 5 6 7] {7 7}].

N.B.: If a transform doesn't terminate early (call `reduced`) then the
transforms after it will never see data.

'seqcompose' isn't a great name for what this does, but 'andthen' sounds kind of
stupid.
"""
function seqcompose(xforms...)
  active = first(xforms)
  g = active[1](active[2])
  acc = active[3]

  handler(_, _, r) = rethrow(r)
  function handler(emit, result, r::EarlyTermination)
    v = g(r.value)
    ret = maybe(emit, result, v)
    xforms = rest(xforms)
    if emptyp(xforms)
      acc = none
      reduced(ret)
    else
      active = first(xforms)
      g = active[1](active[2])
      acc = stateupdate(r, g, active[3])
    end
    return ret
  end

  function (emit)
    function inner()
      emit()
    end
    function inner(result)
      if acc === none
        emit(result)
      else
        t = g(acc)
        acc = none
        if t === none
          emit(result)
        else
          emit(emit(result, t))
        end
      end
    end
    function inner(result, xs...)
      try
        acc = g(acc, xs...)
        result
      catch r
        handler(emit, result, r)
      end
    end
  end
end

# Works for any collection, but is only sensible for ordered collections.
function split(n::Integer, coll)
  into(emptyvector, seqcompose(
    (take(n), conj, empty(coll)),
    (identity, conj, empty(coll))
  ), coll)
end

function aborton(p)
  function (emit)
    function inner()
      emit()
    end
    function inner(result)
      emit(result)
    end
    function inner(result, xs...)
      if p(xs...)
        reduced(emit(result), xs...)
      else
        emit(result, xs...)
      end
    end
  end
end

takewhile(p) = aborton(!p)

dropwhile(p) = seqcompose(
  (every(p), lastarg, nil),
  (identity, conj, emptyvector)
) ∘ cat()

takewhile(p, coll) = into(empty(coll), takewhile(p), coll)
dropwhile(p, coll) = into(empty(coll), dropwhile(p), coll)

function showrecur(io::IO, depth, x::Any)
  @assert !isa(x, Vector) && !isa(x, Map)

  show(io, "text/plain", x)
end

indent(io, depth) = print(io, repeat("  ", depth))

function showseqinner(io, depth, seq)
  start = false
  for x in seq
    if start
      print(io, "\n")
      indent(io, depth)
    end
    start = true
    showrecur(io, depth+1, x)
  end
end

function showseq(io::IO, depth, s)
  indent(io, depth)
  # REVIEW: Why 33?
  if count(s) > 33
    showseqinner(io, depth, take(16, s))
    print(io, "\n ...\n")
    indent(io, depth)
    showseqinner(io, depth, drop(count(s) - 16, s))
  else
    showseqinner(io, depth, s)
  end
  print(io, "\n")
  indent(io, depth-1)
end

function groupby(keyfn, coll)
  function rf(m, e)
    k = keyfn(e)
    assoc(m, k, conj(get(m, k, empty(coll)), e))
  end

  reduce(rf, emptymap, coll)
end
