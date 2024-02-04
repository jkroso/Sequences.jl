@use "github.com/jkroso/Prospects.jl" append prepend pop
import Base.Iterators: take, drop
import Base.rest

abstract type Sequence{T} end

"A singleton to mark the end of a list"
struct EmptySequence{T} <: Sequence{T}
  default_type::Type{<:Sequence}
end
Base.first(::EmptySequence) = throw(BoundsError())
rest(::EmptySequence) = throw(BoundsError())

"A simple singly linked list"
struct Cons{T} <: Sequence{T}
  head::T
  tail::Sequence
end

Cons(first::T, tail=EmptySequence{T}(Cons{T})) where T = Cons(first, tail)

Base.eltype(::Sequence{T}) where T = T
Base.first(s::Cons) = s.head
rest(s::Cons) = s.tail

"Create a List containing all the values passed in"
list(itr...) = foldr(Cons, itr, init=EmptySequence{eltype(itr)}(Cons{eltype(itr)}))
const EOS = list()

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
take(s::Sequence, n::Int) = n < 1 ? EOS : Take(n, s)

"Skip `n` items from the front of `s`"
Base.skip(s::Sequence, n::Integer) = n == 0 ? s : skip(rest(s), n - 1)
Base.skip(s::EmptySequence, n::Integer) = n == 0 ? s : throw(BoundsError())
drop(s::Sequence, n::Integer) = skip(s, n)

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
Base.getindex(s::Sequence, r::UnitRange{Int}) = take(skip(s, r.start - 1), r.stop-r.start+1)
Base.last(s::Sequence) = begin
  r = rest(s)
  isempty(r) ? first(s) : last(r)
end

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

"A Path is just a sequence type that makes append efficient rather than prepend"
struct Path{T} <: Sequence{T}
  value::T
  parent::Sequence
end

tocons(p::EmptySequence, out) = out
tocons(p::Path{T}, out=EmptySequence{T}(Cons{T})) where T = tocons(p.parent, Cons{T}(p.value, out))
Base.reverse(p::Path) = tocons(p)
Base.reverse(s::Sequence{T}) where T = foldl(prepend, s, init=empty(s))
Base.empty(s::Sequence{T}) where T = EmptySequence{T}(typeof(s))
Base.empty(s::EmptySequence) = s

Base.convert(::Type{Path}, itr) = convert(Path{eltype(itr)}, itr)
Base.convert(::Type{Path{T}}, itr) where T = foldl(append, itr, init=EmptySequence{T}(Path{T}))

append(p::Path{T}, x) where T = Path{T}(x, p)
prepend(p::Path{T}, x) where T = begin
  if isempty(p.parent)
    Path{T}(p.value, Path{T}(x, p.parent))
  else
    Path{T}(p.value, prepend(p.parent, x))
  end
end

Base.cat(a::Path{T}, b::Path{T}) where T = foldl(append, b, init=a)
pop(a::Path) = a.parent

Base.first(p::Path) = begin
  while !isempty(p.parent)
    p = p.parent
  end
  p.value
end
Base.last(p::Path) = p.value

rest(p::Path{T}) where T = begin
  parent = p.parent
  isempty(parent) && return parent
  Path{T}(p.value, rest(parent))
end

Base.length(p::Path) = length(p.parent) + 1
Base.iterate(p::Path) = iterate(p, reverse(p))
Base.iterate(p::Path, reverse::Sequence) = (first(reverse), rest(reverse))
Base.iterate(p::Path, ::EmptySequence) = nothing

append(l::Cons{T}, x) where T = Cons{T}(first(l), append(rest(l), x))
append(l::EmptySequence, x) = l.default_type(x, l)
prepend(l::Cons{T}, x) where T = Cons{T}(x, l)
prepend(l::EmptySequence, x) = l.default_type(x, l)
pop(l::Sequence) = isempty(rest(l)) ? rest(l) : prepend(pop(rest(l)), first(l))
pop(l::EmptySequence) = throw(BoundsError())
pop(l, n) = reverse(skip(reverse(l), n))
