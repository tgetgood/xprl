@testset "Vectors" begin
  # There's only one empty vector
  @test emptyvector === vector()

  @test emptyp(emptyvector)

  @test !emptyp(vector(1))
  @test !emptyp(into(emptyvector, 1:nodelength^2))

  @test vector(1) == conj(emptyvector, 1)

  # proper upgrading of element type
  @test conj(conj(emptyvector, 1), :sym) == vector(1, :sym)

  @test begin
    a = vector(:a, :b, :c)

    b = conj(a, :d)

    count(a) == 3 && count(b) == 4
  end

  a::Vector = 1:nodelength

  @test count(a) == nodelength

  @test nth(a, 1) === first(a)
  @test nth(a, 2) === first(rest(a))

  @test nth(a, 15) == 15

  b = conj(a, :another)

  @test typeof(a) != typeof(b)

  @test first(b.elements) === a

  @test vec(1:nodelength+1) == into(emptyvector, 1:nodelength+1)
  @test vec(1:nodelength^2+1) == into(emptyvector, 1:nodelength^2+1)

  @test nth(vec(1:32^4), 32^3+127) == 32^3+127
  @test nth(vec(1:32^4), 32) == 32
  @test nth(vec(1:32^4), 32*32*8 + 32*5 + 9) == 32^2*8 + 32*5 + 9

  @test count(conj(vec(1:33), "asd")) == 34

  g::Base.Vector{Any} = [i for i in 1:33]
  @test eltype(vec(g)) == Int64

  push!(g, "asd")
  @test eltype(vec(g)) == Any
end

@testset "Balanced Trees" begin
  @test begin
    x::Vector = 1:nodelength
    depth(x) == 1
  end

  @test depth(vec(1:nodelength^2)) == 2

  @test depth(vec(1:nodelength^3)) == 3

  long::Vector = 1:nodelength^3 + 2

  @test count(long) == nodelength^3 + 2
  @test depth(long) == 4
  @test every(x -> isa(x, VectorNode), long.elements)
  @test every(x -> isa(x, VectorNode), last(long.elements).elements)
  @test every(x -> isa(x, VectorLeaf), last(first(long.elements).elements).elements)

  @test count(long) == sum(map(count, long.elements))

end

@testset "Transient Vectors" begin
  v = vec([1])
  t = transient!(v)

  @test t isa ds.TransientVector
  @test !(t isa Vector)

  # Can't use norm transient alteration functions
  @test_throws MethodError conj(t, 4)
  @test_throws MethodError assoc(t, 1, 0)

  @test persist!(t) == v

  @test_throws String persist!(t)
  @test_throws String conj!(t, 5)

  @test depth(reduce(conj!, transient!(emptyvector), 1:31)) == 1
  @test depth(reduce(conj!, transient!(emptyvector), 1:33)) == 2
  @test depth(reduce(conj!, transient!(emptyvector), 1:1025)) == 3

  v = transient!(vector(1,2,3))

  conj!(v, :asd)

  @test persist!(v) == vector(1,2,3,:asd)

  v = transient!(vec(1:65))

  v = conj!(v, "test")

  @test last(persist!(v)) == "test"

  v = transient!(vec(1:1026))

  v = conj!(v, 0x12af)

  @test last(persist!(v)) == 0x12af

  @test persist!(reduce(conj!, transient!(emptyvector), 1:1025)) == vec(1:1025)
end

@testset "Vector Seqs" begin
  #TODO:
end

@testset "into!" begin
  @test into!([], []) == []
  @test into([], map(identity), []) == []

  @test into!([], []; infertype=false) == []
  @test into([], map(identity), [], false) == []
end
