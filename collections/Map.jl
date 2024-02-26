@use "github.com/jkroso/Prospects.jl" assoc dissoc
@use ".." Cons EmptySequence rest Sequence push

"""
An immutable Dictionary optimized for small length use cases. It's similar to
`Base.ImmutableDict` but trades some speed for less idiosyncrasies. If you
want the same `assoc` performance as `Base.ImmutableDict` then you can use
`push(map, key=>value)`
"""
struct Map{K,V} <: AbstractDict{K,V}
  pairs::Sequence{Pair{K,V}}
end

Map{K,V}(pairs::Pair...) where {K,V} = begin
  P = Pair{K,V}
  Map{K,V}(reduce(push, pairs, init=EmptySequence{P}(Cons{P})))
end
Map(pairs::Pair...) = begin
  K = typejoin(map(typeof ∘ first, pairs)...)
  V = typejoin(map(typeof ∘ last, pairs)...)
  Map{K,V}(pairs...)
end

Base.iterate(d::Map) = iterate(d, d.pairs)
Base.iterate(d::Map, seq::EmptySequence) = nothing
Base.iterate(d::Map, seq::Sequence) = (first(seq), rest(seq))
Base.length(d::Map) = length(d.pairs)

Base.get(d::Map, key, default) = begin
  for (k,v) in d.pairs
    k == key && return v
  end
  default
end

Base.getindex(d::Map, key) = get(d, key)

assoc(m::Map{K,V}, k::K, v::V) where {K,V} = begin
  old = m.pairs
  new = empty(old)
  while true
    key,value = first(old)
    old = rest(old)
    if k == key
      new = reduce(push, old, init=push(new, k=>v))
      break
    elseif isempty(old)
      new = push(push(new, key=>value), k=>v)
      break
    else
      new = push(new, key=>value)
    end
  end
  Map{K,V}(new)
end

push(m::Map{K,V}, p::Pair) where {K,V} = Map{K,V}(push(m.pairs, p))

dissoc(m::Map{K,V}, k::K) where {K,V} = begin
  old = m.pairs
  new = empty(old)
  while !isempty(old)
    pair = first(old)
    old = rest(old)
    if pair[1] == k
      new = reduce(push, old, init=new)
      break
    else
      new = push(new, pair)
    end
  end
  Map{K,V}(new)
end

Base.filter(pred::Function, m::Map{K,V}) where {K,V} = Map{K,V}(filter(pred, m.pairs))
