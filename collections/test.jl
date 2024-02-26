@use "github.com/jkroso/Rutherford.jl/test" @test testset
@use "github.com/jkroso/JSON.jl/write.jl" json
@use "./Set.jl" Setlet push remove
@use "./Map.jl" Map assoc dissoc

testset("Map") do
  m = Map(:a=>"b",:b=>2)
  @test m[:a] == "b"
  @test haskey(m, :a)
  @test !haskey(m, :c)
  @test Map(:a=>"b",:b=>2) == Map(:a=>"b",:b=>2)
  @test json(Map(:a=>"b",:b=>2)) == """{"b":2,"a":"b"}"""
  @test assoc(Map(:a=>1,:b=>2), :c, 3) == Dict(:a=>1,:b=>2,:c=>3)
  @test assoc(Map(), :c, 3) == Dict(:c=>3)
  @test filter(p->p[1]!=:a, m) == Dict(:b=>2)
  @test dissoc(m, :b) == Map(:a=>"b")
  @test push(Map(:a=>1,:b=>2), :c=>3) == Dict(:a=>1,:b=>2,:c=>3)
end

testset("Set") do
  @test Setlet() == Set([])
  @test Setlet{Symbol}() == Set{Symbol}([])
  @test Setlet((:a,)) == Set([:a])
  @test :a in Setlet([:a,:b,:c])
  @test !(:d in Setlet([:a,:b,:c]))
  @test Setlet([:a,:b,:c]) == Setlet([:a,:b,:c])
  @test Setlet([:a,:b,:c]) == Set([:a,:b,:c])
  @test Setlet([:a,:b,:c]) != Set([:a,:b])
  @test json(Setlet((:a,:b,:c))) == """["a","b","c"]"""
  @test push(Setlet([1,2,3]), 3) == Set([1,2,3])
  @test length(push(Setlet([1,2,3]), 3)) == 3
  @test length(push(Setlet([1,2,3]), 4)) == 4
  @test push(Setlet([1,2,3]), 4) == Set([1,2,3,4])
  @test remove(Setlet((1,2,3)), 4) == Set([1,2,3])
  @test remove(Setlet((1,2,3)), 2) == Set([1,3])
end
