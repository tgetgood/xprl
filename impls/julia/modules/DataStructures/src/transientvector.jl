# N.B.: I think it's a mistake to make transients subtypes of the types they
# mirror. We don't want to accidentally substitute a transient type for a
# persistent type.
abstract type Transient end

# TODO: Learn more about the difference between mutable structs and immutable
# structes containing mutable refs. To be honest, I don't even know if there's
# an effective difference.
struct TransientVector <: Transient
  root::Ref{Any}
  lock::ReentrantLock
  active::Ref{Bool}
end

mutable struct TransientVectorLeaf{T}
  elements::Base.Vector{T}
end

mutable struct TransientVectorNode
  # TransientVectors and PersistentVectors share no supertype, but can share
  # nodes (if that part hasn't been changed yet).
  const elements::Base.Vector{Any}
  const depth::Int8
  count::Int64
end

# Effective type inference requires an empty marker here.
struct TransientEmptyVector end

function checktransience(v::TransientVector)
  if !v.active[]
    throw("Transient has been peristed. Aborting to prevent memory corruption.")
  end
end

function tlwrap(f, v::TransientVector)
  # f()
  try
    lock(v.lock)
    checktransience(v)
    f()
  finally
    unlock(v.lock)
  end
end

count(v::TransientEmptyVector) = 0
count(v::TransientVectorLeaf) = length(v.elements)
count(v::TransientVectorNode) = v.count

depth(v::TransientEmptyVector) = 0
depth(v::TransientVectorLeaf) = 1
depth(v::TransientVectorNode) = v.depth

function depth(v::TransientVector)
  tlwrap(v) do
    depth(v.root[])
  end
end

const transientemptyvector = TransientEmptyVector()


function tvl(elements::Base.Vector{T}) where T
  TransientVectorLeaf{T}(elements)
end

function tvn(elements, count)
  TransientVectorNode(elements, depth(elements[1]) + 1, count)
end

function tv(root)
  TransientVector(root, ReentrantLock(), Ref(true))
end

innertransient!(v::EmptyVector) = transientemptyvector
innertransient!(v::VectorLeaf) = tvl(copy(v.elements))
innertransient!(v::VectorNode) = tvn(copy(v.elements), count(v))

transient!(v::PersistentVector) = tv(innertransient!(v))

function unsafe_persist!(v::PersistentVector)
  v
end

function unsafe_persist!(v::TransientVectorLeaf)
  vectorleaf(v.elements)
end

function unsafe_persist!(v::TransientVectorNode)
  vectornode(map(unsafe_persist!, v.elements))
end

function unsafe_persist!(v::TransientEmptyVector)
  emptyvector
end

function persist!(v::TransientVector)
  tlwrap(v) do
    # a node holds the only references to its children, so unless something very
    # off is happening, we only need to acquire the lock for the root of the
    # tree.
    v.active[] = false
    unsafe_persist!(v.root[])
  end
end

persist!(x::NoEmission) = x
persist!(x::Nothing) = emptyvector

function transientsibling(x, depth)
  if depth == 1
    tvl([x])
  else
    tvn([transientsibling(x, depth - 1)], 1)
  end
end

function replacelastchild!(parent::TransientVector, child)
  parent.root[] = child
end

function replacelastchild!(parent::TransientVectorNode, child)
  parent.elements[end] = child
end

function addtoleaf!(parent, v::TransientVectorLeaf{T}, x::S) where {T, S}
  N = typejoin(S, T)
  els::Base.Vector{N} = copy(v.elements)
  push!(els, x)
  replacelastchild!(parent, tvl(els))
end

function addtoleaf!(_, v::TransientVectorLeaf{T}, x::S) where {T, S <: T}
  push!(v.elements, x)
end

function unsafe_conj!(parent, v::PersistentVector, x)
  child = innertransient!(v)
  replacelastchild!(parent, child)
  unsafe_conj!(parent, child, x)
end

function unsafe_conj!(parent, v::TransientVectorLeaf, x)
  if length(v.elements) == nodelength
    replacelastchild!(parent, tvn([v, tvl([x])], count(v) + 1))

  else
    addtoleaf!(parent, v, x)
  end
end

function unsafe_conj!(parent, v::TransientVectorNode, x)
  if count(v.elements[end]) == nodelength^(v.depth-1)
    if length(v.elements) < nodelength
      push!(v.elements, transientsibling(x, v.depth - 1))
      v.count += 1
    else
      replacelastchild!(
        parent,
        tvn([v, transientsibling(x, depth(v))], count(v) + 1)
      )
    end
  else
    unsafe_conj!(v, v.elements[end], x)
    v.count += 1
  end
end

function unsafe_conj!(parent, v::TransientEmptyVector, x)
  replacelastchild!(parent, tvl([x]))
end

conj!(v) = v

function conj!(v::TransientVector, x)
  tlwrap(v) do
    unsafe_conj!(v, v.root[], x)
    return v
  end
end

empty(v::TransientVector) = tv(transientemptyvector)

# I'm getting use after free errors out of vulkan while using these methods. I
# can't tell why, every test I've cooked up passes. Needs more investigation.

# function into(to::PersistentVector, xform, from...)
#   persist!(transduce(xform, conj!, transient!(to), from...))
# end

# function into(to::PersistentVector, from)
#   persist!(reduce(conj!, transient!(to), from))
# end
