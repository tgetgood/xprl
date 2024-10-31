@testset "transduction on channels" begin
  ch = Channel(5)
  for i in 1:5
    put!(ch, i)
  end

  @async close(ch)

  @test reduce(+, 0, ch) == 15

  c2 = Channel(6)
  put!(c2, 0)
  close(c2)

  c3 = Channel(5)
  for i in 1:5
    put!(c3, i)
  end

  @test reduce(+, 0, c2, c3) == 1

  c3 = Channel()
  c4 = Channel()

  @async into(c4, map(x -> x + 2), c3)

  @async for i in 1:10
    put!(c3, i)
  end

  @test take!(c4) == 3
  @test take!(c4) == 4
end

@testset "atoms" begin
  a = Atom(0)

  Threads.@threads for i in 1:2^20
    swap!(a, +, 1)
  end

  @test deref(a) === 2^20
end

@testset "pub/sub" begin
  i = pub()
  # default buffer of 32
  l = subscribe(i)

  @test l === first(get(deref(i.state), :subscribers))

  put!(i, 42)

  @test 42 === take!(l)

  for j = 1:20
    put!(i, j)
  end

  close(l)

  @test reduce(+, 0, l) == reduce(+, 0, 1:20)

  # Dead subscribers aren't removed until the next publication in the present
  # implementation.
  @test 1 == count(get(deref(i.state), :subscribers))
  put!(i, 0)
  @test 0 == count(get(deref(i.state), :subscribers))

  l = subscribe(i; buffer = 3)

  for j = 1:20
    put!(i, j)
  end

  close(l)

  @test reduce(+, 0, l) == 20+19+18

  t2 = stream(map(x -> x^2), i)

  l2 = subscribe(t2)

  put!(i, 9)

  @test take!(l2) == 81

  # (transduce xform send! to from)
  # should always equal
  # (transduce (comp xform tap!(to)) lastarg nil from)
  #
  # In the sense that the `to` channels should always receive the same values.


end

@testset "stream transduction" begin
  input = pub()
  out = stream(map(x -> x^2), input)

  l = subscribe(out)

  put!(input, 3)

  @test take!(l) == 9
end
