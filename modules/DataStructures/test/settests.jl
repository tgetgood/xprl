@testset "arraysets" begin

  @test count(into(emptyset, [1,1,1,1,1,1,1])) == 1

  @test disj(conj(emptyset, 1), 1) === emptyset
end
