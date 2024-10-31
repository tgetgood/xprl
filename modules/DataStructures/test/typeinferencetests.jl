ints(x) = eltype(x) == Int64
floats(x) = eltype(x) == Float64

even(x) = x % 2 == 0
sq(x) = x^2

@testset "Persistent Vectors ops type inference" begin
  @test ints(into(emptyvector, 1:5))

  @test ints(into(emptyvector, map(sq) ∘ filter(even), 1:10))

  @test floats(into(emptyvector, map(sqrt), 1:10))
end

@testset "Map inference" begin
  #### Array maps
  h = hashmap(:a, 1, :b, 2, :c, 3)

  @test eltype(keys(h)) == Core.Symbol
  @test eltype(vals(h)) == Int

  #### hashmaps

  h = into(emptymap, map(x -> (Core.Symbol(Char(x)), x)), 1:32)

  @test eltype(keys(h)) == Core.Symbol
  @test eltype(vals(h)) == Int
end

@testset "outputting Base.Vectors" begin
  @test ints(into([], 1:5))
  @test ints(into!([], 1:5))

  @test ints(into([], map(sq) ∘ filter(even), 1:5))
  @test ints(into!([], map(sq) ∘ filter(even), 1:5))

  @test floats(into([], map(sqrt), 1:10))
  @test floats(into!([], map(sqrt), 1:10))

  @test eltype(into!([], [1, 2, "4", :f])) == Any

  @test into!([], map(x -> x isa Int ? x + 1 : x), [1, "a", :d]) == [2, "a", :d]
end
