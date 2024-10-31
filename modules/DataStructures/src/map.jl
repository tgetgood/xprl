abstract type Map <: Sequential end

abstract type MapNode end

struct MapEntry{K, V} <: MapNode
  key::K
  value::V
end

function key(x::MapEntry)
  x.key
end

function val(x::MapEntry)
  x.value
end

function Base.:(==)(x::MapEntry, y::MapEntry)
  x.key == y.key && x.value == y.value
end

struct EmptyMap <: Map end

const emptymap = EmptyMap()

# 16 was a good value before optimising. Need to retest afterwards.
const arraymapsizethreashold = 16

struct PersistentArrayMap{K, V} <: Map
  kvs::Base.Vector{MapEntry{K, V}}
end

struct EmptyMarker <: MapNode end

const emptymarker = EmptyMarker()

# N.B.: It's important that nodes not be maps themselves since nodes with
# different levels cannot be merged sensibly and so passing subnodes around as
# full fledged maps is bound to end in confusion and disaster.
struct PersistentHashNode <: MapNode
  ht::Base.Vector{MapNode}
  level::Int
  count::Int
end

struct PersistentHashLeaf{K, V} <: MapNode
  ht::Base.Vector{MapEntry{K, V}}
  level::Int
  count::Int
end

const emptyhash::Base.Vector{MapNode} = [emptymarker for i in 1:nodelength]

emptyhashnode(level) = PersistentHashNode(emptyhash, level, 0)

# One extra memory indirection makes all the ensuing code cleaner. I'll consider
# it worth it until proven otherwise.
struct PersistentHashMap <: Map
  root::PersistentHashNode
end

const emptyhashmap = PersistentHashMap(emptyhashnode(1))

struct HashSeq
  hash
  current
end

"""Returns a seq of chunks of `hashbits` bits.
Should be an infinite seq, but in the present implementation runs out after 64
bits."""
hashseq(x) = HashSeq(hash(x), 1)

function first(s::HashSeq)
  if s.current > 14
    # We could return nothing, but that will just lead to an error when it's
    # used which will be hard to debug unless I remember this...
    throw("FIXME: hash streams not implemented")
  else
    s.hash << ((s.current - 1) * hashbits) >> (64 - hashbits)
  end
end

rest(s::HashSeq) = HashSeq(s.hash, s.current + 1)

# This is a kludge. These hashes need to be cached somehow or they'll kill all
# hope of performance.
nth(h::HashSeq, n) = first(HashSeq(h.hash, h.current+n-1))

##### General methods

function hash(m::Map)
  # 0xd45866ec3759ca93 is a random seed. I want any two maps with the same
  # elements to have the same hash and be equal. This seems like a decent way to
  # accomplish that.
  transduce(map(hash), xor, 0xd45866ec3759ca93, m)
end

empty(m::Map) = emptymap

@inline count(m::EmptyMarker) = 0
@inline count(m::PersistentHashNode) = m.count
@inline count(m::MapEntry) = 1
@inline count(m::EmptyMap) = 0
@inline count(m::PersistentArrayMap) = length(getfield(m, :kvs))
@inline count(m::PersistentHashMap) = m.root.count

@inline emptyp(m::Map) = count(m) == 0

conj(m::Map, v::Vector) = conj(m, MapEntry(v[1], v[2]))

assoc(m::Map, k::Nothing, v) = throw("nothing is not a valid map key.")

first(m::Map) = first(seq(m))
rest(m::Map) = rest(seq(m))

conj(m::Map, x::Nothing) = m

getin(m::Nothing, ks, default=nothing) = default

function getin(m::Map, ks, default=nothing)
  if count(ks) == 1
    get(m, first(ks), default)
  else
    getin(get(m, first(ks)), rest(ks), default)
  end
end

containsp(m::Map, k) = getindexed(m, k)[2] !== :notfound

function associn(m::Map, ks, v)
  if count(ks) == 1
    assoc(m, first(ks), v)
  else
    k = first(ks)
    assoc(m, k, associn(get(m, k, emptymap), rest(ks), v))
  end
end

function assoc(m::Map, k, v, kvs...)
  @assert length(kvs) % 2 == 0

  into(assoc(m, k, v), partition(2), kvs)
end

merge(x::Map) = x

function update(m, k, f, v...)
  nv = f(get(m, k), v...)
  assoc(m, k, nv)
end

function updatein(m, ks, f, v...)
  k = first(ks)
  if emptyp(rest(ks))
    update(m, k, f, v...)
  else
    nv =  updatein(get(m, k), rest(ks), f, v...)
    assoc(m, k, nv)
  end
end

function hashmap(args...)
  @assert length(args) % 2 == 0
  into(emptymap, partition(2), args)
end

function string(x::MapEntry)
  string(x.key) * ": " * string(x.value)
end

function showrecur(io::IO, depth, e::MapEntry)
  showrecur(io, depth, e.key)
  print(io, " -> ")
  showrecur(io, depth, e.value)
end

function string(m::Map)
  inner = transduce(
    map(string) ∘ interpose(", "),
    *,
    "",
    m
  )
  return "{" * inner * "}"
end

function showrecur(io::IO, depth, m::EmptyMap)
  print(io, "{}")
end

function showrecur(io::IO, depth, m::Map)
  print(io, string(count(m)) * "-element PersistentMap: {\n")
  showseq(io, depth, seq(m))
  print(io, "}")
end

function show(io::IO, mime::MIME"text/plain", s::Map)
  showrecur(io, 1, s)
end

function keys(m::Map)
  map(x -> x.key, seq(m))
end

function vals(m::Map)
  map(x -> x.value, seq(m))
end

function zipmap(x, y)
  reduce(assoc, emptymap, x, y)
end

function mapkeys(f)
  function(emit)
    function inner()
      emit()
    end
    function inner(result)
      emit(result)
    end
    function inner(result, e)
      emit(result, MapEntry(f(key(e)), val(e)))
    end
  end
end

function mapvals(f)
  function(emit)
    function inner()
      emit()
    end
    function inner(result)
      emit(result)
    end
    function inner(result, e::MapEntry)
      emit(result, MapEntry(key(e), f(val(e))))
    end
    function inner(result, e::Union{Base.Vector, Tuple})
      emit(result, (e[1], f(e[2])))
    end
  end
end

mapkeys(f, m) = into(empty(m), mapkeys(f), m)
mapvals(f, m) = into(empty(m), mapvals(f), m)

##### Empty maps

merge(x::EmptyMap, y::EmptyMap) = emptymap
merge(x::EmptyMap, y::Map) = y
merge(x::Map, y::EmptyMap) = x

merge(x::Nothing, y::Map) = y
merge(x::Map, y::Nothing) = x

assoc(x::Nothing, k::Nothing, v) = throw("nothing is not a valid map key.")
assoc(x::Nothing, k, v) = PersistentArrayMap([MapEntry(k, v)])
assoc(m::EmptyMap, k, v) = PersistentArrayMap([MapEntry(k, v)])

conj(m::EmptyMap, e::MapEntry) = PersistentArrayMap([e])

get(m::Nothing, k) = nil
get(m::Nothing, k, default) = default
get(m::EmptyMap, x) = nil
get(m::EmptyMap, x, default) = default

seq(x::Nothing) = emptyvector
seq(x::EmptyMap) = emptyvector

dissoc(x::EmptyMap, _) = x

function getindexed(m::EmptyMap, k)
  nothing, :notfound
end

@inline containsp(m::EmptyMap, k) = false

Base.:(==)(x::EmptyMap, y::EmptyMap) = true
Base.:(==)(x::EmptyMap, y::Map) = emptyp(y)
Base.:(==)(x::Map, y::EmptyMap) = emptyp(x)

##### Array Maps

@inline function Base.getproperty(m::PersistentArrayMap, k::Core.Symbol)
  if k === :kvs
    return getfield(m, :kvs)
  else
    get(m, k, nothing)
  end
end

first(m::PersistentArrayMap) = getfield(m, :kvs)[1]

eltype(m::PersistentArrayMap{K, V}) where {K, V} = MapEntry{K, V}

"""
Returns the index of k within the underlying vector of arraymap m. 0 for not found.
"""
function getindex(m::PersistentArrayMap, k)
  kvs = getfield(m, :kvs)
  for i in 1:length(kvs)
    if kvs[i].key == k
      return i
    end
  end
  return 0
end

@inline function containsp(m::PersistentArrayMap, k)
  getindex(m, k) != 0
end

function get(m::PersistentArrayMap, k, default=nothing)
  i = getindex(m, k)
  if i == 0
    default
  else
    getfield(m, :kvs)[i].value
  end
end

function conj(m::PersistentArrayMap{K, V}, e::MapEntry{K, V}) where {K, V}
  i = getindex(m, e.key)
  kvs = getfield(m, :kvs)
  len = length(kvs)
  if i == 0
    if count(m) >= arraymapsizethreashold
      return conj(into(emptyhashmap, m), e)
    else
      newkvs = Base.Vector{MapEntry{K, V}}(undef, len + 1)
      for i in 1:len
        newkvs[i] = kvs[i]
      end
      newkvs[len + 1] = e
    end
  else
    newkvs = Base.Vector{MapEntry{K, V}}(undef, length(kvs))
    for j in 1:i-1
      newkvs[j] = kvs[j]
    end
    newkvs[i] = e
    for j in i+1:len
      newkvs[j] = kvs[j]
    end
  end
  PersistentArrayMap(newkvs)
end

function conj(m::PersistentArrayMap{K, V}, e::MapEntry{L, W}) where {K, V, L, W}
  # TODO: performance warnings.
  # @warn "copying to handle heterogeneous types."
  i = getindex(m, e.key)
  kvs = getfield(m, :kvs)
  len = length(kvs)
  NK = typejoin(K, L)
  NV = typejoin(V, W)
  if i == 0
    if count(m) >= arraymapsizethreashold
      return conj(into(emptyhashmap, m), e)
    else
      newkvs = Base.Vector{MapEntry{NK,NV}}(undef, len + 1)
      for i in 1:len
        newkvs[i] = MapEntry{NK, NV}(kvs[i].key, kvs[i].value)
      end
      newkvs[len+1] = MapEntry{NK, NV}(e.key, e.value)
    end
  else
    newkvs = Base.Vector{MapEntry{NK, NV}}(undef, length(kvs))
    for j in 1:i-1
      newkvs[j] = MapEntry{NK, NV}(kvs[j].key, kvs[j].value)
    end
    newkvs[i] = MapEntry{NK, NV}(e.key, e.value)
    for j in i+1:len
      newkvs[j] = MapEntry{NK, NV}(kvs[j].key, kvs[j].value)
    end
  end
  PersistentArrayMap(newkvs)
end

function assoc(m::PersistentArrayMap{K, V}, k::L, v::W) where L <: K where W <: V where {K, V}
  conj(m, MapEntry{K, V}(k, v))
end

function assoc(m::PersistentArrayMap, k, v)
  conj(m, MapEntry(k, v))
end

function dissoc(m::PersistentArrayMap, k)
  i = getindex(m, k)
  if i == 0
    m
  else
    kvs = getfield(m, :kvs)
    newkvs = Base.Vector{eltype(kvs)}(undef, length(kvs) - 1)
    for j in 1:i-1
      newkvs[j] = kvs[j]
    end
    for j in i+1:length(kvs)
      newkvs[j-1] = kvs[j]
    end
    PersistentArrayMap(newkvs)
  end
end

function seq(m::PersistentArrayMap)
  # Array maps have limited length, so I'm not worried about realising them as
  # vectors.
  vec(getfield(m, :kvs))
end

@inline function mapfromunique(kvs)
  if length(kvs) <= arraymapsizethreashold
    PersistentArrayMap(kvs)
  else
    # TODO: This can be made a lot faster.
    into(emptyhashmap, kvs)
  end
end

function merge(x::PersistentArrayMap{K, V}, y::PersistentArrayMap{K, V}) where {K, V}
  a, b = ifelse(count(x) < count(y), (x, y), (y, x))

  vs = Base.Vector{MapEntry{K,V}}(undef, count(x) + count(y))

  as = getfield(a, :kvs)
  bs = getfield(b, :kvs)
  for i in 1:count(a)
    vs[i] = as[i]
  end

  i = count(a)

  for e in bs
    if containsp(a, e.key)
      continue
    else
      i += 1
      vs[i] = e
    end
  end

  if i == length(vs)
    ws = vs
  else
    ws = vs[1:i]
  end

  mapfromunique(ws)
end

# FIXME: There's too much repetition here.
function merge(x::PersistentArrayMap{K, V}, y::PersistentArrayMap{L, W}) where {K, V, L, W}
  a, b = ifelse(count(x) < count(y), (x, y), (y, x))

  S = typejoin(K, L)
  T = typejoin(V, W)

  vs = Base.Vector{MapEntry{S,T}}(undef, count(x) + count(y))

  as = getfield(a, :kvs)
  bs = getfield(b, :kvs)

  if eltype(a) == MapEntry{S,T}
    for i in 1:count(a)
      vs[i] = as[i]
    end
  else
    for i in 1:count(a)
      vs[i] = MapEntry{S,T}(as[i].key, as[i].value)
    end
  end

  i = count(a)

  if eltype(b) == MapEntry{S,T}
    for e in bs
      if containsp(a, e.key)
        continue
      else
        i += 1
        vs[i] = e
      end
    end
  else
    for e in bs
      if containsp(a, e.key)
        continue
      else
        i += 1
        vs[i] = MapEntry{S,T}(e.key, e.value)
      end
    end
  end

  if i == length(vs)
    ws = vs
  else
    ws = vs[1:i]
  end

  mapfromunique(ws)
end

# REVIEW: This is n^2, but could be n*log(n) if we sorted and iterated. However,
# in my basic benchmarks this takes ~70ns for maps of size 16 regardless of key
# order (using Random.shuffle). That's suspicious, but I'm going to leave this
# be until something points at it as a problem.
function Base.:(==)(x::PersistentArrayMap, y::PersistentArrayMap)
  if x === y
    return true
  elseif count(x) != count(y)
    return false
  else
    for i in 1:count(x)
      key = x.kvs[i].key
      j = getindex(y, key)
      if j == 0 || y.kvs[j].value != x.kvs[i].value
        return false
      end
    end
    return true
  end
end

##### Hash Maps

@inline function Base.getproperty(m::PersistentHashMap, k::Core.Symbol)
  if k === :root
    return getfield(m, :root)
  else
    get(m, k)
  end
end

function get(m::PersistentHashMap, k, default=nothing)
  h = hashseq(k)
  getinternal(m.root, k, h, default)
end

function getinternal(m::PersistentHashNode, k, hs, default)
  getinternal(m.ht[first(hs) + 1], k, rest(hs), default)
end

function getinternal(m::MapEntry, k, hs, default)
  ifelse(m.key == k, m.value, default)
end

function getinternal(m::EmptyMarker, k, hs, default)
  default
end

function getindexed(m::MapEntry, k, default, l)
  if m.key == k
    # The level at which an element was found (if found) tells you exactly what
    # it's effective hash is.
    m.value, l
  else
    default, :notfound
  end
end

function getindexed(m::EmptyMarker, _, default, _)
  default, :notfound
end

function getindexed(m::PersistentHashNode, k, default, l)
  next = m.ht[nth(hashseq(k), m.level) + 1]

  getindexed(next, k, default, m.level)
end

function getindexed(m::PersistentHashMap, k, default=nil)
  getindexed(m.root, k, default, 1)
end

function containsp(m::PersistentHashNode, e::MapEntry)
  (v, i) = getindexed(m, key(e), :notfound, m.level)
  i !== :notfound && v == val(e)
end

assoc(m::PersistentHashMap, k, v) = conj(m, MapEntry(k, v))

function addtomap(m::PersistentHashNode, e::MapEntry, _=0)
  h = nth(hashseq(key(e)), m.level) + 1
  next = m.ht[h]

  submap = addtomap(next, e, m.level)
  ht = copy(m.ht)
  ht[h] = submap

  PersistentHashNode(ht, m.level, sum(count, ht))
end

addtomap(x::MapEntry, y::PersistentHashNode, l) = addtomap(y, x, l)

addtomap(m::PersistentHashNode, e::EmptyMarker, _=0) = m
addtomap(e::EmptyMarker, m::PersistentHashNode, _=0) = m

addtomap(m::EmptyMarker, e::MapEntry, _=0) = e
addtomap(e::MapEntry, m::EmptyMarker, _=0) = e

addtomap(x::EmptyMarker, y::EmptyMarker, _=0) = x

function addtomap(e1::MapEntry, e2::MapEntry, l)
  if key(e1) == key(e2)
    e2
  else
    addtomap(addtomap(emptyhashnode(l+1), e1), e2)
  end
end

function addtomap(x::PersistentHashNode, y::PersistentHashNode, l)
  ht = Base.Vector{MapNode}(undef, nodelength)

  for i in 1:nodelength
    ht[i] = addtomap(x.ht[i], y.ht[i], l)
  end

  PersistentHashNode(ht, l, sum(count, ht))
end

function conj(m::PersistentHashMap, e::MapEntry)
  (v, i) = getindexed(m, key(e))
  if i !== :notfound && val(e) == i
    m
  else
    PersistentHashMap(addtomap(m.root, e))
  end
end

function merge(x::PersistentHashMap, y::PersistentHashMap)
  PersistentHashMap(addtomap(x.root, y.root, 1))
end

# TODO: Make merge reasonably fast. There's way too much throwaway allocation
# from using `into` naively.
merge(x::PersistentArrayMap, y::PersistentHashMap) = into(y, x)
merge(x::PersistentHashMap,  y::PersistentArrayMap) = into(x, y)

merge() = emptymap

merge(xs::Map...) = reduce(merge, xs)

function mergewith(f, m1, m2)
  function rf(acc, e)
    if containsp(acc, key(e))
      update(acc, key(e), f, val(e))
    else
      conj(acc, e)
    end
  end

  reduce(rf, m1, m2)
end

function containsp(m::PersistentHashNode, k)
  (v, i) = getindexed(m, k, :notfound, m.level)
  i !== :notfound
end

function removefrommap(n::PersistentHashNode, k)
  # FIXME: This is too unncessarily complicated to be correct, despite the tests
  # passing.
  if containsp(n, k)
    if n.level > 1 && n.count === 1
      emptymarker
    elseif n.level > 1 && n.count == 2
      ne = filter(x -> x !== emptymarker, n.ht)
      if count(ne) == 2
        first(filter(x -> key(x) != k, ne))
      else
        PersistentHashNode(
          into!([], map(x -> removefrommap(x, k)), n.ht),
          n.level,
          n.count - 1
        )
      end
    else
      PersistentHashNode(
        into!([], map(x -> removefrommap(x, k)), n.ht),
        n.level,
        n.count - 1
      )
    end
  else
    n
  end
end

function removefrommap(n::MapEntry, k)
  if key(n) == k
    emptymarker
  else
    n
  end
end

removefrommap(n::EmptyMarker, _) = n

function dissoc(m::PersistentHashMap, k)
  if containsp(m, k)
    PersistentHashMap(removefrommap(m.root, k))
  else
    m
  end
end

function dissoc(m::Map, k, ks...)
  reduce(dissoc, dissoc(m, k), ks)
end

gather(acc, m::EmptyMarker) = acc
gather(acc, m::MapEntry) = conj(acc, m)
gather(acc, m::PersistentHashNode) = reduce(gather, acc, m.ht)

function seq(m::PersistentHashMap)
  gather(emptyvector, m.root)
end

function selectkey(m::Map, k)
  emptymarker = gensym()
  v = get(m, k, emptymarker)
  if v === emptymarker
    nil
  else
    MapEntry(k, v)
  end
end

# TODO: Override `into` for EmptyMap as we did for vectors.
function selectkeys(m::Map, ks)
  into(emptymap, map(x -> selectkey(m, x)) ∘ remove(x -> x === nil), ks)
end

function Base.:(==)(x::PersistentHashMap, y::PersistentHashMap)
  x.root == y.root
end

function Base.:(==)(x::PersistentHashNode, y::PersistentHashNode)
  if count(x) !== count(y)
    return false
  else
    every(==, x.ht, y.ht)
  end
end

function Base.:(==)(x::PersistentArrayMap, y::PersistentHashMap)
  y == x
end

function Base.:(==)(x::PersistentHashMap, y::PersistentArrayMap)
  if count(x) != count(y)
    return false
  else
    # REVIEW: Maps shouldn't contain `nothing` as a value, but I'm not guarding
    # against it properly...
    notfound = gensym()
    every(e -> get(x, e.key, notfound) == e.value, getfield(y, :kvs))
  end
end

iterate(m::EmptyMap) = nothing

function iterate(m::PersistentArrayMap)
  first(m), 2
end

function iterate(m::PersistentArrayMap, k)
  if k > count(m)
    nothing
  else
    getfield(m, :kvs)[k], k + 1
  end
end

function iterate(m::PersistentHashMap)
  first(m), rest(seq(m))
end

# FIXME: This is very inefficient. Store a cursor or something. Better yet let
# the head get collected, but that won't help in this case...
function iterate(m::PersistentHashMap, state)
  if emptyp(state)
    nothing
  else
    first(state), rest(state)
  end
end

depth(m::PersistentArrayMap) = 1
depth(m::PersistentHashMap) = depth(m.root)
depth(n::PersistentHashNode) = 1 + max(map(depth, n.ht)...)
depth(n::MapNode) = 0
