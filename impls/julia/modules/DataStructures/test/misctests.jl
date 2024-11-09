@testset "groupby" begin
  g = groupby(x -> x < 0, [1,2,3,-4,7,-8])
  @test g == hashmap(false, [1,2,3,7], true, [-4, -8])

  @test groupby(x -> get(ds.val(x), :type), hashmap(:a, hashmap(:type, 1), :b, hashmap(:type, 2))) == hashmap(1, hashmap(:a, hashmap(:type, 1)), 2, hashmap(:b, hashmap(:type, 2)))
end
