inc(x) = x + 1
even(x) = x % 2 == 0

@testset "Basic transforms" begin
  @test into(emptyvector, map(inc), 1:5) == vec(2:6)
  @test count(into(emptyvector, filter(even), 1:10)) == 5


end

vs(x) = into(emptyvector, map(vec), x)

@testset "Early Abort" begin
  @test take(5, 1:2^60) == vec(1:5)
  @test into(emptyvector, take(5) ∘ map(inc), 1:2^32) == vec(2:6)

  @test every(even, []) == true
  @test every(even, [1]) == false
  @test every(even, [1,2,3]) == false
  @test every(even, [2]) == true
  @test every(even, [2, 3]) == false
  @test every(even, 2:2:199) == true
  @test every(even, 1:2:199) == false

  xform() = every(even) ∘ map(x -> x^2) ∘ take(4)

  @test into(emptyvector, xform(), 2:100) == emptyvector
  @test into(emptyvector, xform(), 2:2:100) == vector(4, 16, 36, 64)

  @test into(emptyvector, take(5) ∘ take(5) ∘ take(5), vec(1:1000)) == vec(1:5)
end

@testset "Multiple Streams" begin
  @test zip(vec(1:6), [:a, :b, :c]) == vs([[1,:a], [2, :b], [3, :c]])

  @test begin
    a = into(emptyvector, interleave(), [1,2,3], ["a", :b, 5])
    b = vector(1, "a", 2, :b, 3, 5)
    c = into(emptyvector, inject(["a", :b, 5]) ∘ interleave(), [1,2,3])
    a == b == c
  end

  @test split(5, 1:2) == vs([[1,2]])
  @test split(5, 1:10) == vector(vec(1:5), vec(6:10))

  xform() = seqcompose(
    (take(2), conj, emptyvector),
    (take(4) ∘ map(inc), conj, emptyvector),
    (map(x -> (x,x)) ∘ take(3), conj, emptymap)
  )

  @test intoemptyvec(xform(), 1:5) == vs([[1,2], [4,5,6]])

  # Here's a curious case. If a transform starts, but gets no input, does it
  # emit nothing, or never exist at all? I'm going the emit nothing (empty
  # stream) route until I see a reason to change that.
  @test intoemptyvec(xform(), 1:2) == vs([[1,2], []])

  @test intoemptyvec(xform(), 1:10) == vec([
    vector(1, 2),
    vector(4, 5, 6, 7),
    hashmap(7,7,8,8,9,9)
  ])


  xform2() = seqcompose(
    (take(2), conj, emptyvector),
    (take(4), conj, emptyvector),
    (take(3), conj, emptyvector)
  ) ∘ map(sum)

  @test intoemptyvec(xform2(), 1:1000) == vector(3, 18, 24)

  v = [2,4,6,7,8,9,0]

  @test takewhile(even, v) == [2,4,6]
  @test dropwhile(even, v) == [7,8,9,0]

end
