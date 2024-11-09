using Test

using DataStructures
import DataStructures as ds
import DataStructures: conj, count, nodelength, vec, VectorLeaf, VectorNode, EmptyVector, Vector, sibling, vectornode, vectorleaf, depth, emptyhashmap, reduce, arraymapsizethreashold, conj!, zip, intoemptyvec, groupby

import BenchmarkTools: @benchmark
# TODO: Look into dev modules to separate these tests as the suite gets
# larger. Not really a problem yet, but I have pasting files together.
#
# The fact that the official docs say to use `include` statements internally is
# saddening. Julia is such a forward thinking language in so many ways, but from
# a dependency management and modularity point of view it has a lot of the
# shitty cruft of C and python.
#
# Of course one of the primary design choices of julia is that every function
# symbol is a global mutable variable. The "can't modify without explicit
# import" property helps but is a bit of a kludge.

include("vectortests.jl")
include("maptests.jl")
include("xformtests.jl")
include("settests.jl")
include("asynctests.jl")
include("typeinferencetests.jl")
include("relational.jl")
include("misctests.jl")
