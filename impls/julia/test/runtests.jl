using Test

include(dirname(@__FILE__)*"/../src/core.jl")

@testset "sanity" begin

  for e in tests
    @info string(ds.key(e)), string(ds.val(e))
    x.interpret(sys.withcc(ds.emptymap, :return, x -> @test x == ds.val(e)), env, ds.key(e))
  end
end

end
