module System

import DataStructures as ds

##### Work scheduling

function runtask(t)
  Threads.@spawn begin
    try
      t()
    catch e
      ds.handleerror(e)
    end
  end
end

function debugruntask(t)
  t()
end

##### Channels

function trysend(c, k, v)
  k = ds.keyword(k)
  if ds.containsp(c, k)
    debugruntask(() -> ds.get(c, k)(v))
  else
    throw("Cannot emit message on unbound channel: " * string(k))
  end
end

function emit(c, kvs...)
  @assert length(kvs) % 2 === 0
  for (k, v) in ds.partition(2, kvs)
    trysend(c, k, v)
  end
end

succeed(c, v) = emit(c, :return, v)

function withcc(m::ds.Map, k, c, kvs...)
  ds.into(
    ds.assoc(m, ds.keyword(string(k)), c),
    ds.partition(2) ∘ ds.map(e -> [ds.keyword(string(e[1])), e[2]]),
    kvs
  )
end

##### Receivers

mutable struct Collector
  @atomic counter::Int
  const vec::Base.Vector
  const next
end

function collector(c, n)
  Collector(0, Vector(undef, n), c)
end

function receive(coll::Collector, i, v)
  coll.vec[i] = v
  @atomic coll.counter += 1

  if coll.counter > length(coll.vec)
    @error "Received too many messages."
  end

  if coll.counter == length(coll.vec)
    emit(coll.next, :return, ds.vec(coll.vec))
  end
end

end # module
