using BenchmarkTools

using DataStructures
import DataStructures as ds


################################################################################
# Quick and dumb micro benchmarks.
#
# tldr; when type inference works, and the destination collection is a
# Base.Vector, I'm in the same ballpark as Base.Iterators when `filter` is
# involved. For just mapping, I'm 10x slower and there's a lot of extra allocation I can't
#
# Not bad, but type inference doesn't work automatically, especially when
# composing transducers.
#
# Weirdly enough Base.map and Base.filter outperform the Base.Iterators versions
# in these tests. That alone should tell me they're not very useful.
#
# TODO: Some less trivial examples.
#
# N.B.: These should be commented out on master because they're not tests.
################################################################################

f = x -> x + 1
p = x -> x % 2 === 0

range = 1:2^15

@benchmark into(emptyvector, map(f), range)

@benchmark into!([], map(f), range)
@benchmark Base.map(f, range)
@benchmark collect(Base.Iterators.map(f, range))

@benchmark into(emptyvector, filter(p), range)

@benchmark into!([], filter(p), range)
@benchmark into!(Int[], filter(p), range)
@benchmark Base.filter(p, range)
@benchmark collect(Base.Iterators.filter(p, range))

@benchmark into(emptyvector, map(f) ∘ filter(p), range)

@benchmark into!([], map(f) ∘ filter(p), range)
@benchmark into!(Int[], map(f) ∘ filter(p), range)
@benchmark Base.filter(p, Base.map(f, range))
@benchmark collect(Base.Iterators.filter(p, Base.Iterators.map(f, range)))
