@require "github.com/jkroso/Promises.jl" Promise need @defer

abstract type Sequence{T} end

"A singleton to mark the end of a list"
struct EmptySequence{T} <: Sequence{T} end
const EOS = EmptySequence{Any}()
Base.eof(s::Sequence) = s ≡ EOS
Base.eof(s::EmptySequence) = true

Base.first(::EmptySequence) = throw(BoundsError())
rest(::EmptySequence) = throw(BoundsError())

"A simple singly linked list"
struct Cons{T} <: Sequence{T}
  head::T
  tail::Sequence
end

# Make the tail optional
Cons(first, tail=EOS) = Cons(first, tail)

Base.first(s::Cons) = s.head
rest(s::Cons) = s.tail

"Create a List containing all the values passed in"
list() = EOS
list(head, rest...) = Cons(head, list(rest...))

# Handle pretty printing for the REPL etc..
Base.show(io::IO, m::MIME"text/plain", seq::EmptySequence) = write(io, "()")
Base.show(io::IO, m::MIME"text/plain", seq::Sequence) = begin
  write(io, '(')
  while true
    show(io, m, first(seq))
    seq = rest(seq)
    eof(seq) && break
    write(io, ' ')
  end
  write(io, ')')
end

Base.:(==)(a::Sequence, b::Sequence) = first(a) == first(b) && rest(a) == rest(b)
Base.:(==)(a::EmptySequence, b::EmptySequence) = true

"A wrapper to convert an AbstractArray to a Sequence"
struct ArraySeq{T} <: Sequence{T}
  xs::AbstractArray{T}
  i::Int
end

Base.convert(::Type{Sequence}, a::AbstractArray) = length(a) == 0 ? EOS : ArraySeq(a, 1)

Base.length(s::ArraySeq) = length(s.xs) - (s.i - 1)
Base.first(s::ArraySeq) = s.xs[s.i]
rest(s::ArraySeq) = s.i < length(s.xs) ? ArraySeq(s.xs, s.i + 1) : EOS

"Enables you to limit the length of a Sequence"
struct Take{T} <: Sequence{T}
  n::Int
  s::Sequence{T}
end

Base.first(t::Take) = first(t.s)
rest(t::Take) = begin
  t.n <= 1 && return EOS
  r = rest(t.s)
  eof(r) && throw(BoundsError())
  Take(t.n - 1, r)
end

"Create a copy of `s` with a length == n"
take(n::Int, s::Sequence) = n < 1 ? EOS : Take(n, s)

"Skip `n` items from the front of `s`"
Base.skip(n::Int, s::Sequence) = n == 0 ? s : skip(n - 1, rest(s))
Base.skip(n::Int, s::EmptySequence) = n == 0 ? EOS : throw(BoundsError())

# Iteration protocol
Base.start(s::Sequence) = s
Base.next(::Sequence, s::Sequence) = (first(s), rest(s))
Base.done(::Sequence, s::Sequence) = eof(s)

Base.isempty(s::Sequence) = eof(s)
Base.endof(s::Sequence) = length(s)
Base.length(s::Sequence) = eof(s) ? 0 : 1 + length(rest(s))
Base.getindex(s::Sequence, n::Int) = n == 1 ? first(s) : getindex(rest(s), n - 1)
Base.getindex(s::Sequence, r::UnitRange{Int}) = take(r.stop, skip(r.start - 1, s))

"Enables merging of Sequences"
struct Zip{T} <: Sequence{T}
  ss::Vector{Sequence}
end

Base.first(z::Zip) = map(first, z.ss)
rest(z::Zip) = zip(map(rest, z.ss)...)
# Create a list of rows by combining several lists
Base.zip(ss::Sequence...) = any(eof, ss) ? EOS : Zip{Any}(Sequence[ss...])

"Enables joining two Sequences together"
struct Cat{T} <: Sequence{T}
  a::Sequence{T}
  b::Sequence
end

Base.first(s::Cat) = first(s.a)
rest(s::Cat) = begin
  a = rest(s.a)
  eof(a) ? s.b : Cat(a, s.b)
end

Base.cat(a::Sequence, b::Sequence) = Cat(a, b)
Base.cat(a::Sequence, b::EmptySequence) = a
Base.cat(a::EmptySequence, b::Sequence) = b

Base.map(f::Function, s::EmptySequence) = EOS
Base.map(f::Function, s::Sequence) = Cons(f(first(s)), map(f, rest(s)))
Base.map(f::Function, ss::Sequence...) = map(v -> f(v...), zip(ss...))

Iterators.filter(f::Function, s::EmptySequence) = EOS
Iterators.filter(f::Function, s::Sequence) = begin
  head = first(s)
  if f(head)
    Cons(head, Iterators.filter(f, rest(s)))
  else
    Iterators.filter(f, rest(s))
  end
end

Base.reduce(f::Function, accum, s::EmptySequence) = accum
Base.reduce(f::Function, accum, s::Sequence) = reduce(f, f(accum, first(s)), rest(s))

"""
Streams are Sequences where the tail is always a Promise. Thereby enabling the sequence
to be lazily or asynchronously generated
"""
struct Stream{T} <: Sequence{T}
  head::T
  tail::Promise
end
Stream(first, tail=Promise(EOS)) = Stream(first, tail)

Base.first(s::Stream) = s.head
rest(s::Stream) = need(s.tail)

# Lazy character streams from IO objects
Base.convert(::Type{Sequence}, io::IO) = convert(Sequence{UInt8}, io)
Base.convert(::Type{Sequence{T}}, io::IO) where {T} = begin
  if eof(io)
    close(io)
    EOS
  else
    Stream(read(io, T), @defer convert(Sequence{T}, io)::Sequence)
  end
end
