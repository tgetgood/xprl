abstract type Set <: Sequential end

# FIXME: Basically don't use hashsets because they're not implemented. This will
# scale *very* poorly.
const arraysetthreashold = typemax(Int)

struct EmptySet <: Set end

emptyset = EmptySet()

empty(s::Set) = emptyset
count(s::EmptySet) = 0

emptyp(s::EmptySet) = true
emptyp(s::Set) = false

seq(s::EmptySet) = emptyvector

disj(s::EmptySet, _) = emptyset

function conj(s::EmptySet, x)
  PersistentArraySet([x])
end

struct PersistentArraySet{T} <: Set
  elements::Base.Vector{T}
end

first(s::Set) = first(seq(s))
rest(s::Set) = rest(seq(s))

## N.B.: `seq` on sets is necessary to facilitate iteration, but sets are
## unordered, and there's no guarantee that the same set will yield the same
## sequence twice.
seq(s::PersistentArraySet) = s.elements

count(s::PersistentArraySet) = length(s.elements)

containsp(s::EmptySet, x) = false
containsp(s::Set, x) = getindexed(s, x) !== :notfound

set(xs...) = into(emptyset, xs)

# FIXME: Check for repeats!!
function arrayset(elements)
  if length(elements) === 0
    emptyset
  else
    PersistentArraySet(unique(elements))
  end
end

function getindexed(s::PersistentArraySet{T}, x::S) where {T, S}
  :notfound
end

function getindexed(s::PersistentArraySet{T}, x::T) where T
  for i in 1:count(s)
    if x == s.elements[i]
      return i
    end
  end
  return :notfound
end

function conj(s::PersistentArraySet, x)
  if containsp(s, x)
    s
  else
    if count(s) == arraysetthreashold
      conj(into(emptyhashset, s), x)
    else
      arrayset(push!(copy(s.elements), x))
    end
  end
end

function disj(s::PersistentArraySet, x)
  i = getindexed(s, x)
  if i === :notfound
    s
  else
    arrayset(append!(s.elements[begin:i-1], s.elements[i+1:end]))
  end
end

function Base.:(==)(x::EmptySet, y::EmptySet)
  true
end

function Base.:(==)(x::PersistentArraySet, y::PersistentArraySet)
  if x === y
    true
  elseif count(x) !== count(y)
    false
  else
    every(xel -> containsp(y, xel), x)
  end
end

function iterate(s::PersistentArraySet)
  iterate(s.elements)
end

function iterate(s::PersistentArraySet, i::Int)
  iterate(s.elements, i)
end

# TODO: These must be changed once there are hash sets!!!!
function into(_::EmptySet, xform, from)
  arrayset(into!([], xform, from))
end

function into(_::EmptySet, c)
  arrayset(c)
end

## FIXME: Not finished.
##### Hash Sets

struct HashSetNode
  ht::Base.Vector
  count::Int
  level::Int
end

# REVIEW: specialise on isbits types and use the actual bits as the hash? That
# would simplify a lot of things, and save a good amount of space for types with
# less than 64 bits.
#
# Not a priority by any means, but an intriguing puzzle.
struct PersistentHashSet
  root::HashSetNode
end

count(s::PersistentHashSet) = s.root.count

getindexed(s::PersistentHashSet, x) = getindexed(s.root, x, 1)

getindexed(s::EmptyMarker, x, _) = :notfound

# REVIEW: I'm not using a special "entry" type to denote a hit. Rather anything
# which is not a recusive node or an empty marker is a value. There's an edge
# there that I think is harmless, but let's see if it cuts me.
getindexed(s, x, l) = l

function getindexed(s::PersistentHashNode, x, _)
  i = nth(hashseq(x), s.level) + 1

  getindexed(s.ht[i], x, s.level + 1)
end

function addtoset(s::EmptyMarker, x, _)

end

function conj(s::PersistentHashSet, x)
  if getindexed(s, x) === :notfound
    PersistentHashSet(addtoset(s.root, x))
  else
    s
  end
end

function disj(s::PersistentHashSet, x)
  throw("not implemented")
end

# Allow nodes to have between nodelength and nodelength/2 elements.
# Each node stores highest and lowest stored value.
# Insert into tree recursively. If a node grows too large, split it and
# propagate the changes to the parent.
#
# So long as we can ensure that no node will ever have less than nodelength/2
# elements (except the rightmost), then we know the tree is balanced and lookup
# and insertion are O(logn).
#
# Caveat: Identity is derived from the comparison function. This might not
# always be what you want.
struct PersistentSortedSet
  root
  by::Function
  lt::Function
end

function string(s::Set)
  "#{" * transduce(map(string) ∘ interpose(", "), *, "", seq(s)) * "}"
end

################################################################################
##### Relational Algebra
################################################################################

### These are stolen, mutatis mutandis, from clojure's set.clj

# REVIEW: There are more efficient ways to do some of these by taking into
# account the underlying set implementation. If performance ever becomes a
# problem, remember that.

union(x::EmptySet, y::EmptySet) = emptyset
union(x::EmptySet, y::Set) = y
union(x::Set, y::EmptySet) = x

function union(x::Set, y::Set)
  if count(x) < count(y)
    into(y, x)
  else
    into(x, y)
  end
end

difference(x::EmptySet, y::EmptySet) = emptyset
difference(x::EmptySet, y::Set) = emptyset
difference(x::Set, y::EmptySet) = x

function difference(x::Set, y::Set)
  if count(x) < count(y)
    difference(y, x)
  else
    reduce(disj, x, y)
  end
end

intersection(x::EmptySet, y::EmptySet) = emptyset
intersection(x::EmptySet, y::Set) = emptyset
intersection(x::Set, y::EmptySet) = emptyset

function intersection(x::Set, y::Set)
  if count(y) < count(x)
    intersection(y, x)
  else
    reduce((acc, e) -> ifelse(containsp(y, e), acc, disj(acc, e)), x, x)
  end
end

function index(rel, ks)
  reduce((m, x) -> begin
         ik = selectkeys(x, ks)
         return assoc(m, ik, conj(get(m, ik, emptyset), x))
         end,
    emptymap, rel
  )
end

join(x::EmptySet, y::EmptySet) = emptyset
join(x::EmptySet, y::Set) = emptyset
join(x::Set, y::EmptySet) = emptyset

"""
Only natural join is implemented.
"""
function join(x::Set, y::Set)
  ks = intersection(
    into(emptyset, keys(first(x))),
    into(emptyset, keys(first(y)))
  )

  (r, s) = ifelse(count(x) < count(y), (x, y), (y, x))

  idx = index(r, ks)

  reduce((acc, x) ->
      reduce((acc, y) ->
          conj(acc, merge(y, x)), acc, get(idx, selectkeys(x, ks))
      ), emptyset, s
  )
end

function project(rel::Set, ks)
  into(emptyset, map(m -> selectkeys(m, ks)) ∘ remove(emptyp), rel)
end

##### Map inversion
#
# This should be in map.jl except for dependencies.

function invert(m::Map)
  reduce(
    (m, x) -> assoc(m, val(x), conj(get(m, val(x), emptyset), key(x))),
    emptymap,
    m
  )
end

#### Set display

function show(io::IO, mime::MIME"text/plain", s::Set)
  showrecur(io, 1, s)
end

function showrecur(io::IO, _, _::EmptySet)
  print(io, "#{}")
end

function showrecur(io::IO, depth, x::Set)
  print(io, string(count(x)) * "-element PersistentSet: {\n")
  showseq(io, depth, seq(x))
  print(io, "}")
end
