@use ".." Cons EmptySequence rest Sequence push step

struct Setlet{V} <: AbstractSet{V}
  values::Sequence{V}
end

Setlet(itr) = Setlet{eltype(itr)}(itr)
Setlet{T}() where T = Setlet{T}(EmptySequence{T}(Cons{T}))
Setlet() = Setlet{Any}()

Base.iterate(d::Setlet) = iterate(d, d.values)
Base.iterate(d::Setlet, seq::EmptySequence) = nothing
Base.iterate(d::Setlet, seq::Sequence) = (first(seq), rest(seq))
Base.length(d::Setlet) = length(d.values)

push(s::Setlet{V}, x) where V = x in s ? s : Setlet{V}(push(s.values, x))
remove(s::Setlet{V}, x) where V = begin
  p = s.values
  new = empty(p)
  itr = step(p)
  while !isnothing(itr)
    first,rest = itr
    first == x && return Setlet{V}(reduce(push, rest, init=new))
    new = push(new, first)
    itr = step(p, rest)
  end
  s
end
