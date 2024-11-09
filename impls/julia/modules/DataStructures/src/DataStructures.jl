baremodule DataStructures

import Base

include(s) = Base.include(DataStructures, s)

import Base: (:)

import Base: string, hash, ==, length, iterate, put!, bind, reverse, get, merge, keys, values, first, map, filter, getindex, eltype, show, last, empty, repeat, split, take!, put!, close, lock, unlock, isready, lastindex, ^, @assert, @MIME_str, @info, @warn, @error, Channel, ReentrantLock, @inline, typemax, @async, @atomic, Threads, AbstractRange, UnitRange, identity, typejoin, convert, vcat, <=, copy, >, >=, <, +, -, *, /, ∘, !==, ===, !=, sum, rethrow, push!, divrem, gensym, !, isempty, %, ifelse, <<, >>, xor, unique, append!, InvalidStateException, print, println

# N.B.: Base.rest is broken for ranges and zip iterators. It returns the
# collection itself. Don't import it.

# How many bits of hash are used at each level of the HAMT?
hashbits = 5
nodelength = 2^hashbits
nil = nothing

include("./util.jl")

abstract type Sexp end

include("./sequential.jl")

include("./sexps.jl")

include("./vector.jl")
include("./transientvector.jl")
include("./list.jl")
include("./map.jl")
include("./set.jl")
include("./queue.jl")
include("./async.jl")
include("./juliatypes.jl")

include("./walk.jl")

# Sequential
export first, rest, take, drop, reduce, transduce, into, map, filter, remove, interpose, dup, cat, partition, seq, seqcompose, zip, split, interleave, inject, takewhile, dropwhile

# Vectors
export emptyvector, nth, vec, vector

# Lists

export list, tolist

# Maps
export emptymap, assoc, update, dissoc, containsp, hashmap, vals, associn, updatein, getin, mapkeys, mapvals

# Sets
export emptyset, disj, set

# Queues
export Queue, queue, emptyqueue, closedp, emptystream, put!, Stream, stream

# Generic
export conj, count, empty, emptyp, nil, keyword, name, symbol, withmeta, meta, every

# Types
export Keyword, Map, Vector, MapEntry, List, Symbol, Sexp

# Transients
export transient!, persist!, conj!

# Async
export Atom, deref, swap!, pub, subscribe, tap

# other
export handleerror

# Mutable Julia collections
export into!

# Walk
export walk, prewalk, postwalk

## Julia conventions vs my tendency to use clojure names...
values(m::Map) = vals(m)

end
