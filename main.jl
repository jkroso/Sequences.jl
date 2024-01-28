@use "github.com/jkroso/Prospects.jl" append prepend

abstract type Sequence{T} end

"A singleton to mark the end of a list"
struct EmptySequence{T} <: Sequence{T} end
const EOS = EmptySequence{Any}()
Base.first(::EmptySequence) = throw(BoundsError())
rest(::EmptySequence) = throw(BoundsError())

"A simple singly linked list"
struct Cons{T} <: Sequence{T}
  head::T
  tail::Sequence
end

Cons(first, tail=EmptySequence{Cons{typeof(first)}}()) = Cons(first, tail)

Base.eltype(::Sequence{T}) where T = T
Base.first(s::Cons) = s.head
rest(s::Cons) = s.tail

"Create a List containing all the values passed in"
list() = EOS
list(head::T) where T = Cons{T}(head, EmptySequence{Cons{T}}())
list(head::T, rest...) where T = Cons{T}(head, list(rest...))

# Handle pretty printing for the REPL etc..
Base.show(io::IO, seq::EmptySequence) = write(io, "()")
Base.show(io::IO, ::MIME"text/html", seq::Sequence) = show(io, seq)
Base.show(io::IO, seq::Sequence) = begin
  write(io, '(')
  first = true
  for x in seq
    if first; first = false; else write(io, ' ') end
    show(io, x)
  end
  write(io, ')')
  nothing
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
  isempty(r) && throw(BoundsError())
  Take(t.n - 1, r)
end

"Create a copy of `s` with a length == n"
take(n::Int, s::Sequence) = n < 1 ? EOS : Take(n, s)

"Skip `n` items from the front of `s`"
Base.skip(n::Int, s::Sequence) = n == 0 ? s : skip(n - 1, rest(s))
Base.skip(n::Int, s::EmptySequence) = n == 0 ? EOS : throw(BoundsError())

Base.iterate(s::Sequence) = iterate(s, s)
Base.iterate(s::EmptySequence) = nothing
Base.iterate(s::Sequence, r) = (first(r), rest(r))
Base.iterate(s::Sequence, r::EmptySequence) = nothing

Base.isempty(s::Sequence) = false
Base.isempty(s::EmptySequence) = true
Base.lastindex(s::Sequence) = length(s)
Base.firstindex(s::Sequence) = 1
Base.length(s::Sequence) = 1 + length(rest(s))
Base.length(::EmptySequence) = 0
Base.getindex(s::Sequence, n::Int) = n == 1 ? first(s) : getindex(rest(s), n - 1)
Base.getindex(s::Sequence, r::UnitRange{Int}) = take(r.stop-r.start+1, skip(r.start - 1, s))

"Enables merging of Sequences"
struct Zip{T} <: Sequence{T}
  ss::Vector{Sequence}
end

Base.first(z::Zip) = map(first, z.ss)
rest(z::Zip) = zip(map(rest, z.ss)...)
# Create a list of rows by combining several lists
Base.zip(ss::Sequence...) = any(isempty, ss) ? EOS : Zip{Any}(Sequence[ss...])

"Enables joining two Sequences together"
struct Cat{T} <: Sequence{T}
  a::Sequence{T}
  b::Sequence
end

Base.first(s::Cat) = first(s.a)
rest(s::Cat) = begin
  a = rest(s.a)
  isempty(a) ? s.b : Cat(a, s.b)
end

Base.cat(a::Sequence, b::Sequence) = Cat(a, b)
Base.cat(a::Sequence, b::EmptySequence) = a
Base.cat(a::EmptySequence, b::Sequence) = b

Base.map(f::Function, s::EmptySequence) = s
Base.map(f::Function, s::Sequence) = Cons(f(first(s)), map(f, rest(s)))
Base.map(f::Function, ss::Sequence...) = map(v -> f(v...), zip(ss...))

Iterators.filter(f::Function, s::EmptySequence) = s
Iterators.filter(f::Function, s::Sequence) = begin
  head = first(s)
  if f(head)
    Cons(head, Iterators.filter(f, rest(s)))
  else
    Iterators.filter(f, rest(s))
  end
end

Base.reduce(f::Function, s::EmptySequence; init) = init
Base.reduce(f::Function, s::Sequence; init=Base.reduce_empty(f, eltype(s))) = reduce(f, rest(s), init=f(init, first(s)))

Base.findfirst(predicate::Function, s::Sequence) = begin
  i = 1
  for x in s
    if predicate(x)
      return i
    end
    i += 1
  end
  nothing
end

append(l::Cons{T}, x) where T = Cons{T}(first(l), append(rest(l), x))
append(l::EmptySequence{Cons{T}}, x) where T = Cons{T}(x, l)
prepend(l::EmptySequence{Cons{T}}, x) where T = Cons{T}(x, l)
prepend(l::Cons{T}, x) where T = Cons{T}(x, l)
