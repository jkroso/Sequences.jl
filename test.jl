@use "github.com/jkroso/Rutherford.jl/test.jl" testset @test @catch
@use "." EOS rest Cons list Sequence take
@use "./stream.jl" Stream @defer
@use "./double.jl" DoublyLinkedList head tail

testset("EmptySequence") do
  @test isa(@catch(first(EOS)), BoundsError)
  @test isa(@catch(rest(EOS)), BoundsError)
end

testset("Cons") do
  @test first(Cons(1)) == 1
  @test rest(Cons(1)) == EOS
  @test first(list(1)) == 1
  @test rest(list(1)) == EOS
  @test typeof(Cons(1)) == Cons{Int}
end

testset("Serialization") do
  @test sprint(show, "text/plain", list(1,2)) == "(1 2)"
  @test sprint(show, "text/plain", list(1)) == "(1)"
  @test sprint(show, "text/plain", list()) == "()"
end

testset("==") do
  @test list(1, 2, "3") == list(1, 2, "3")
end

testset("Array wrapper") do
  @test convert(Sequence, []) == EOS
  @test convert(Sequence, [1,2]) == list(1,2)
end

testset("take") do
  @test take(0, list(1,2,3)) == EOS
  @test take(1, list(1,2,3)) == list(1)
  @test take(2, list(1,2,3)) == list(1,2)
  @test rest(take(1, list(1,2))) == EOS
end

testset("skip") do
  @test skip(0, list(1,2,3)) == list(1,2,3)
  @test skip(1, list(1,2,3)) == list(2,3)
  @test skip(2, list(1,2,3)) == list(3)
  @test skip(3, list(1,2,3)) == list()
  @test isa(@catch(skip(4, list(1,2,3))), BoundsError)
end

testset("Iteration") do
  @test collect(list(1,2,3)) == [1,2,3]
  @test collect(EOS) == []
end

testset("indexing") do
  @test list(1,2,3)[1] == 1
  @test list(1,2,3)[end] == 3
  @test list(1,2,3)[1:end] == list(1,2,3)
  @test list(1,2,3)[1:2] == list(1,2)
  @test isa(@catch(list(1,2,3)[4]), BoundsError)
end

testset("zip") do
  @test zip(list(1), list(2), list(3,4)) == list([1,2,3])
  @test zip(list(1), list(2), list(3)) == list([1,2,3])
  @test zip(list(1), list(2), EOS) == EOS
end

testset("cat") do
  @test cat(Cons(1), Cons(2)) == list(1,2)
  @test cat(Cons(1), EOS) == Cons(1)
  @test cat(EOS, Cons(1)) == Cons(1)
end

testset("map") do
  @test map(identity, list(1,2,3)) == list(1,2,3)
  @test map(tuple, Cons(1), Cons(2), Cons(3)) == list((1,2,3))
end

testset("filter") do
  @test Iterators.filter(isodd, list(1,2,3)) == list(1,3)
  @test Iterators.filter(isodd, EOS) == EOS
end

testset("reduce") do
  @test reduce(+, list(1,2,3)) == 6
end

testset("Stream") do
  @test Stream(1) == list(1)
  @test Stream(1, @defer Stream(2)) == list(1,2)
end

testset("convert(Sequence, io::IO)") do
  @test convert(Sequence{UInt8}, open("./main.jl")) == convert(Sequence, read("./main.jl"))
end

testset("convert(DoublyLinkedList, iter)") do
  @test repr(convert(DoublyLinkedList, (1,2,3))) == "(1 2 3)"
  @test head(convert(DoublyLinkedList, (1,2,3))).value == 1
  @test tail(convert(DoublyLinkedList, (1,2,3))).value == 3
  @test !isempty(convert(DoublyLinkedList, (1,)))
  @test isempty(convert(DoublyLinkedList, ()))
  @test 1 in convert(DoublyLinkedList, (1,))
end

testset("push!(DoublyLinkedList, x)") do
  a = convert(DoublyLinkedList, (1,2,3))
  @test push!(a, 4).value == 4
  @test repr(a) == "(1 2 3 4)"
end
