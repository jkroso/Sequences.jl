@use "." EmptySequence Sequence Cons EOS rest prepend append pop

"A Path is just a sequence type that makes append efficient rather than prepend"
struct Path{T} <: Sequence{T}
  value::T
  parent::Sequence
end

tocons(p::EmptySequence{Path{T}}, out) where T = out
tocons(p::Path{T}, out=EmptySequence{Cons{T}}()) where T = tocons(p.parent, Cons{T}(p.value, out))
Base.reverse(p::Path) = tocons(p)

Base.convert(::Type{Path}, itr) = convert(Path{eltype(itr)}, itr)
Base.convert(::Type{Path{T}}, itr) where T = foldl((p, x)->Path{T}(x, p), itr, init=EmptySequence{Path{T}}())

append(p::Union{Path{T},EmptySequence{Path{T}}}, x) where T = Path{T}(x, p)
prepend(p::EmptySequence{Path{T}}, x) where T = Path{T}(x, EmptySequence{Path{T}}())
prepend(p::Path{T}, x) where T = begin
  if p.parent isa EmptySequence
    Path{T}(p.value, Path{T}(x, p.parent))
  else
    Path{T}(p.value, prepend(p.parent, x))
  end
end

Base.cat(a::Path{T}, b::Path{T}) where T = begin
  for x in b
    a = Path{T}(x, a)
  end
  a
end

pop(a::Path) = a.parent

Base.first(p::Path) = begin
  while !(p.parent isa EmptySequence)
    p = p.parent
  end
  p.value
end

rest(p::Path{T}) where T = begin
  parent = p.parent
  parent isa EmptySequence && return parent
  Path{T}(p.value, rest(parent))
end

Base.length(p::Path) = length(p.parent) + 1
Base.iterate(p::Path) = iterate(p, reverse(p))
Base.iterate(p::Path, reverse::Sequence) = (first(reverse), rest(reverse))
Base.iterate(p::Path, ::EmptySequence) = nothing
