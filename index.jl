@require "Promises" Promise Result need @defer

abstract Sequence{T}
abstract SequenceNode{T} <: Sequence{T}

# A singalton to mark the end of a list
immutable EmptySequence <: Sequence end
const EOS = EmptySequence()

Base.first(::EmptySequence) = throw(BoundsError())
rest(::EmptySequence) = throw(BoundsError())

test("EmptySequence") do
  @test isa(@catch(first(EOS)), BoundsError)
  @test isa(@catch(rest(EOS)), BoundsError)
end

type Cons{T} <: SequenceNode{T}
  head::T
  tail::Sequence
end

##
# Make the tail optional
#
Cons(first, tail=EOS) = Cons(first, tail)

Base.first(s::Cons) = s.head
rest(s::Cons) = s.tail

##
# Create a List containing all the values passed in
#
list() = EOS
list(head, rest...) = Cons(head, list(rest...))

test("Cons") do
  @test first(Cons(1)) == 1
  @test rest(Cons(1)) == EOS
  @test first(list(1)) == 1
  @test rest(list(1)) == EOS
  @test typeof(Cons(1)) == Cons{Int}
end

##
# Handle pretty printing for the REPL etc..
#
Base.writemime(io::IO, m::MIME"text/plain", seq::EmptySequence) = write(io, "()")
Base.writemime(io::IO, m::MIME"text/plain", seq::SequenceNode) = begin
  write(io, '(')
  while true
    writemime(io, m, first(seq))
    seq = rest(seq)
    seq ≡ EOS && break
    write(io, ' ')
  end
  write(io, ')')
end

test("Serialization") do
  @test sprint(writemime, "text/plain", list(1,2)) == "(1 2)"
  @test sprint(writemime, "text/plain", list(1)) == "(1)"
  @test sprint(writemime, "text/plain", list()) == "()"
end

##
# Define equality
#
Base.(:(==))(a::SequenceNode, b::SequenceNode) = first(a) == first(b) && rest(a) == rest(b)

test("==") do
  @test list(1,2,3) == list(1,2,3)
end

type ArraySeq{T} <: SequenceNode{T}
  xs::AbstractArray{T}
  i::Int
end

Base.convert(::Type{Sequence}, a::AbstractArray) = length(a) ≡ 0 ? EOS : ArraySeq(a, 1)

Base.length(s::ArraySeq) = length(s.xs) - (s.i - 1)
Base.first(s::ArraySeq) = s.xs[s.i]
rest(s::ArraySeq) = s.i < length(s.xs) ? ArraySeq(s.xs, s.i + 1) : EOS

test("Array wrapper") do
  @test convert(Sequence, []) == EOS
  @test convert(Sequence, [1,2]) == list(1,2)
end

type Take{T} <: SequenceNode{T}
  n::Int
  s::Sequence{T}
end

Base.first(t::Take) = first(t.s)
rest(t::Take) = begin
  t.n <= 1 && return EOS
  r = rest(t.s)
  r ≡ EOS && throw(BoundsError())
  Take(t.n - 1, r)
end

##
# Create a copy of `s` with a length == n
#
take(n::Int, s::Sequence) = n < 1 ? EOS : Take(n, s)

test("take") do
  @test take(0, list(1,2,3)) == EOS
  @test take(1, list(1,2,3)) == list(1)
  @test take(2, list(1,2,3)) == list(1,2)
  @test rest(take(1, list(1,2))) == EOS
end

##
# Skip `n` items from the front of `s`
#
skip(n::Int, s::SequenceNode) = n ≡ 0 ? s : skip(n - 1, rest(s))
skip(n::Int, s::EmptySequence) = n ≡ 0 ? EOS : throw(BoundsError())

test("skip") do
  @test skip(0, list(1,2,3)) == list(1,2,3)
  @test skip(1, list(1,2,3)) == list(2,3)
  @test skip(2, list(1,2,3)) == list(3)
  @test skip(3, list(1,2,3)) == list()
  @test isa(@catch(skip(4, list(1,2,3))), BoundsError)
end

##
# Iteration protocol
#
Base.start(s::Sequence) = s
Base.next(::Sequence, s::Sequence) = (first(s), rest(s))
Base.done(::Sequence, ::Sequence) = false
Base.done(::Sequence, ::EmptySequence) = true

test("Iteration") do
  @test collect(list(1,2,3)) == [1,2,3]
  @test collect(EOS) == []
end

Base.isempty(::SequenceNode) = false
Base.isempty(::EmptySequence) = true
Base.endof(s::Sequence) = length(s)
Base.length(s::SequenceNode) = 1 + length(rest(s))
Base.length(s::EmptySequence) = 0
Base.getindex(s::Sequence, n::Int) = n == 1 ? first(s) : getindex(rest(s), n - 1)
Base.getindex(s::Sequence, r::UnitRange{Int}) = take(r.stop, skip(r.start - 1, s))

test("indexing") do
  @test list(1,2,3)[1] == 1
  @test list(1,2,3)[end] == 3
  @test list(1,2,3)[1:end] == list(1,2,3)
  @test list(1,2,3)[1:2] == list(1,2)
  @test isa(@catch(list(1,2,3)[4]), BoundsError)
end

type Zip <: SequenceNode
  ss::(Sequence...)
end

Base.first(z::Zip) = map(first, z.ss)
rest(z::Zip) = zip(map(rest, z.ss)...)

##
# Create a list of rows by combining several lists
#
Base.zip(ss::SequenceNode...) = Zip(ss)
Base.zip(ss::Sequence...) = EOS

test("zip") do
  @test zip(list(1), list(2), list(3,4)) == list((1,2,3))
  @test zip(list(1), list(2), list(3)) == list((1,2,3))
  @test zip(list(1), list(2), EOS) == EOS
end

type Cat{T} <: SequenceNode{T}
  a::SequenceNode{T}
  b::SequenceNode
end

Base.first(s::Cat) = first(s.a)
rest(s::Cat) = begin
  a = rest(s.a)
  a ≡ EOS ? s.b : Cat(a, s.b)
end

Base.cat(a) = a
Base.cat(a::SequenceNode, b::SequenceNode) = Cat(a, b)
Base.cat(a::SequenceNode, b::EmptySequence) = a
Base.cat(a::EmptySequence, b::SequenceNode) = b
Base.cat(a, b, cs...) = Cat(a, cat(b, cs...))

test("cat") do
  @test cat(Cons(1), Cons(2), Cons(3)) == list(1,2,3)
  @test cat(Cons(1), EOS) == Cons(1)
  @test cat(EOS, Cons(1)) == Cons(1)
end

Base.map(f::Function, s::EmptySequence) = EOS
Base.map(f::Function, s::SequenceNode) = Cons(f(first(s)), map(f, rest(s)))
Base.map(f::Function, ss::Sequence...) = map(v -> f(v...), zip(ss...))

test("map") do
  @test map(identity, list(1,2,3)) == list(1,2,3)
  @test map(tuple, Cons(1), Cons(2), Cons(3)) == list((1,2,3))
end

Base.filter(f::Function, s::EmptySequence) = EOS
Base.filter(f::Function, s::SequenceNode) = begin
  head = first(s)
  if f(head)
    Cons(head, filter(f, rest(s)))
  else
    filter(f, rest(s))
  end
end

test("filter") do
  @test filter(bool, list(1,0,2)) == list(1,2)
  @test filter(bool, EOS) == EOS
end

Base.reduce(f::Function, accum, s::EmptySequence) = accum
Base.reduce(f::Function, accum, s::SequenceNode) = reduce(f, f(accum, first(s)), rest(s))

test("reduce") do
  @test reduce(+, list(1,2,3)) == 6
end

Base.start(p::Promise) = start(need(p))
Base.done(p::Promise, s::Sequence) = done(need(p), s)
Base.done(p::Promise, s::EmptySequence) = done(need(p), s)
Base.done(p::Promise, s::Promise) = done(need(p), need(s))
Base.done(s::Sequence, p::Promise) = done(s, need(p))
Base.next(p::Promise, s::Sequence) = (first(s), rest(s))
Base.next(p::Promise, s::Promise) = (first(need(s)), rest(need(s)))
Base.next(s::Sequence, p::Promise) = next(s, need(p))
Base.first(p::Promise) = first(need(p))
rest(p::Promise) = rest(need(p))
Base.length(p::Promise) = length(need(p))

test("Lifting for Promises") do
  @test first(@defer list(1)) == 1
  @test rest(@defer list(1)) == EOS
end

##
# Streams support asynchronous generation. This can be used
# to improve performance by deferring computation your not sure
# you will end up using. Or to define inifinte lists such as the
# the Fibonacci sequence. Or to establish communication channels
# between actors. Streams are pretty usefull
#
abstract StreamNode{T} <: SequenceNode{T}

type Stream{T} <: StreamNode{T}
  head::T
  tail::Union(Sequence, Promise)
end

Stream(first, tail=EOS) = Stream(first, tail)

Base.first(s::Stream) = need(s.head)
rest(s::Stream) = s.tail

##
# Create a List containing all the values passed in
#
stream() = EOS
stream(head, rest...) = Stream(head, stream(rest...))

test("Stream") do
  @test stream(1) == list(1)
  @test stream(1, 2) == list(1,2)
  @test stream(@defer 1) == list(1)
  @test stream(1, @defer 2) == list(1,2)
end

##
# Lazy character streams from IO objects
#
Base.convert(::Type{Sequence}, io::IO) = convert(Sequence{Uint8}, io)
Base.convert{T}(::Type{Sequence{T}}, io::IO) = begin
  if eof(io)
    close(io)
    EOS
  else
    Stream(read(io, T), @defer(convert(Sequence{T}, io)::Sequence))
  end
end

test("convert(Sequence, io::IO)") do
  io = open("./index.jl")
  @test reduce(push!, {}, convert(Sequence, io)) == readall("./index.jl").data
  @test !isopen(io)
  @test collect(convert(Sequence{Char}, open("./index.jl"))) == {readall("./index.jl")...}
end
