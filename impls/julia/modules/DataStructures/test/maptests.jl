@testset "array maps" begin
  @test hashmap() === emptymap

  @test begin
    a = assoc(emptymap, :key, :value)
    get(a, :key) === :value
  end

  @test get(emptymap, :key, :default) === :default

  c = hashmap(
    :key, 1,
    "string", complex(1f0, 13f-2),
    [1,2,3], Base.Vector,
    vector(1, 2, 3), Vector
  )

  @test get(c, [1,2,3]) === Base.Vector
  @test get(c, vec(1:3)) === Vector
  @test get(c, "string") === complex(1f0, 13f-2)

  @test hashmap(:a, 1, :b, 2) == hashmap(:b, 2, :a, 1)

  @test assoc(assoc(emptymap, :a, 3), :a, 6) == hashmap(:a, 6)

  m = assoc(emptymap, :a, 1, :b, 2, "c", 3)

  @test dissoc(m, :a) == hashmap(:b, 2, "c", 3)
  @test dissoc(m, :b) == hashmap(:a, 1, "c", 3)
  @test dissoc(m, "c") == hashmap(:a, 1, :b, 2)

  inc(x) = x + 1
  fnil(f, default) = x -> x === nothing ? f(default) : f(x)

  @test update(emptymap, "aaa", fnil(inc, 0)) == hashmap("aaa", 1)

  @test conj(emptymap, MapEntry(:a, 1)) == hashmap(:a, 1)
end

@testset "nested maps" begin
  m = hashmap(:a, hashmap(:b, 5))

  @test getin(m, [:a, :b]) === 5
  @test getin(m, [:a, :a], :default) === :default
  @test getin(m, [:b, :a], :default) === :default

  @test getin(associn(m, [:a, :c, :b], :sym), [:a, :c]) == hashmap(:b, :sym)
  @test getin(updatein(m, [:a, :b], +, 1), [:a, :b]) == 6

  m2 = into(emptymap, map(i -> (i,i)), 1:4)

  m3 = merge(m, m2)

  @test count(m3) == count(m) + count(m2)

  @test get(m3, 2) == 2
  @test getin(m3, [:a, :b]) == 5

  @test_throws MethodError updatein(m3, [1, :c], conj, "purple")

  m4 = updatein(m3, [:a, :c], conj, "purple")

  @test count(m4) == count(m3)
  @test count(get(m4, :a)) == count(get(m3, :a)) + 1

  @test getin(m4, [:a, :c]) == vector("purple")
end

@testset "hash maps" begin
  @test get(assoc(assoc(emptyhashmap, :a, 6), :a, 2), :a) == 2

  c = reduce(
    conj,
    emptyhashmap,
    [(:key, 1),
    ("string", complex(1f0, 13f-2)),
     ([1,2,3], Base.Vector),
    (vector(1, 2, 3), assoc(emptyhashmap, :some, "string"))]
  )

  @test c == merge(c,c)

  m = emptyhashmap
  m = conj(m, (1,2))
  m = conj(m, (1,2))

  @test count(m) == 1

  @test get(c, [1,2,3]) === Base.Vector
  @test get(c, "string") === complex(1f0, 13f-2)
  @test getin(c, [vec(1:3), :some]) == "string"

  m = into(emptymap, map(i -> (i, i)), 1:2*arraymapsizethreashold)

  @test m isa ds.PersistentHashMap

  @test count(merge(m, c)) == 2*arraymapsizethreashold + 4
  @test get(m, 443) == nothing
  @test get(m, arraymapsizethreashold) == arraymapsizethreashold

  @test get(assoc(m, "string", vector(:a, 2, "c")), "string") isa Vector

  @test emptyhashmap == emptymap

  a = assoc(assoc(emptyhashmap, :a, 1), :b, 2)
  b = assoc(assoc(emptyhashmap, :b, 2), :a, 1)
  c = hashmap(:b, 2, :a, 1)

  @test hash(a) === hash(b) == hash(c)

  @test a == b == c

  @test assoc(emptyhashmap, a, 1) == assoc(emptyhashmap, c, 1)

  @test merge(c, c) == c

  @test dissoc(a, :a) == hashmap(:b, 2)
  @test dissoc(a, :a, :b) == emptymap

  m = into(emptymap, map(i -> (i, i)), 1:64)
  n = into(emptymap, map(i -> (i, i)), 33:64)

  @test dissoc(m, 1:32...) == n
end

@testset "merging maps" begin
  a = hashmap(
    :test, 1,
    :key, 2
  )

  b = hashmap(
    "test", 1,
    :key, 4
  )

  c = merge(a, b)

  d = into(emptymap, map(x -> [x,x]), 1:1025)

  @test count(c) < count(a) + count(b)
  @test get(c, :key) == 4
  @test get(merge(b, a), :key) == 2

  @test merge(a, emptymap) == merge(emptymap, a)

  @test merge(a, emptyhashmap) == merge(emptyhashmap, a)

  @test merge(a, emptymap) == merge(emptyhashmap, a)

  @test merge(d, b) == merge(b, d)

end

@testset "map helpers" begin
  m = hashmap("A", 1, "B", 2)

  @test mapvals(x -> x + 1, m) == hashmap("A", 2, "B", 3)

  @test mapkeys(k -> k*"s", m) == hashmap("As", 1, "Bs", 2)
end

# This doesn't actually work as a function, but wrapping the code keeps it from running along side the tests
function mapbenchmarks()
  # Array maps

  @benchmark reduce(conj, emptymap, [[1,2], [3,4], [5,6], [7,8]])
  @benchmark into(emptymap, partition(2), 1:8)
  @benchmark into(emptymap, partition(2), 1:32)

  m = into(emptymap, partition(2), 1:32)

  @info typeof(m)

  @benchmark get(m, 1)
  @benchmark get(m, 15)
  @benchmark get(m, 31)

  m = hashmap(1,1)

  @benchmark assoc(m, 2, 2)

  m = into(emptymap, partition(2), 1:16)

  @benchmark assoc(m, 17, 18)

  @benchmark assoc(m, 3, 5)

  m = into(emptymap, partition(2), 1:32)

  @benchmark assoc(m, 15, 11)

  @benchmark assoc(m, :a, 0)

  m1 = into(emptymap, partition(2), 1:16)
  m2 = into(emptymap, partition(2), 17:32)
  m3 = into(emptymap, partition(2), 9:22)
  m4 = hashmap(:a, 1f0, :b, 2f0, :c, 1f2, :d, 1f-10)
  m5 = hashmap(:a, 1, :b, 22, :c, 4, :d, 7)

  @benchmark merge(m1, m1)
  @benchmark merge(m, m)
  @benchmark merge(m1, m3)
  @benchmark merge(m1, m2)
  @benchmark merge(m, m3)
  @benchmark merge(m1, m2, m3)
  @benchmark merge(merge(m1, m2), m3)

  @benchmark merge(m1, m4)
  @benchmark merge(m4, m3)
  @benchmark merge(m1, m3, m4)
  @benchmark merge(m1, m4, m3)
  @benchmark merge(merge(m1, m4), m3)

  # hashmaps
  @benchmark into(emptyhashmap, partition(2), 1:16)

  m = into(emptyhashmap, partition(2), 1:32)

  @benchmark get(m, 1)
  @benchmark get(m, 31)

  @benchmark assoc(m, 17, 4)
  @benchmark assoc(m, 170, 4)
end
