@use "github.com/jkroso/Prospects.jl" prepend
@use "." EmptySequence Sequence Cons EOS

"A Path is just a sequence type that makes append efficient rather than prepend"
struct Path{T} <: Sequence{T}
  value::T
  parent::Sequence{T}
end

tosequence(p::Nothing, out=EOS) = out
tosequence(p::Path{T}, out=EmptySequence{T}()) where T = begin
  tosequence(p.parent, Cons{T}(p.value, out))
end

Base.convert(::Type{Path}, itr) = begin
  T = eltype(itr)
  foldl((p,x)->Path{T}(x, p), itr, init=EmptySequence{T}())
end

convert(Path, (1,2,3))
