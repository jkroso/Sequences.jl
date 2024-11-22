@use "github.com/jkroso/Prospects.jl" assoc dissoc
@use ".." Cons EmptySequence rest Sequence push Path step

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
  Map{K==Union{} ? Any : K, V==Union{} ? Any : V}(pairs...)
end

Base.iterate(d::Map) = iterate(d, d.pairs)
Base.iterate(d::Map, seq::EmptySequence) = nothing
Base.iterate(d::Map, seq::Sequence) = (first(seq), rest(seq))
Base.length(d::Map) = length(d.pairs)

Base.get(d::Map, key, default) = begin
  itr = step(d.pairs)
  while itr !== nothing
    pair,rest = itr
    pair.first == key && return pair.second
    itr = step(rest, rest)
  end
  default
end

Base.getindex(d::Map, key) = get(d, key)

assoc(m::Map{K,V}, k::K, v::V) where {K,V} = begin
  new = empty(m.pairs)
  itr = step(m.pairs)
  while !isnothing(itr)
    pair,rest = itr
    if k == pair[1]
      return Map{K,V}(reduce(push, rest, init=push(new, k=>v)))
    end
    new = push(new, pair)
    itr = step(rest, rest)
  end
  Map{K,V}(push(new, k=>v))
end

push(m::Map{K,V}, p::Pair) where {K,V} = Map{K,V}(push(m.pairs, p))

dissoc(m::Map{K,V}, k::K) where {K,V} = begin
  rest = m.pairs
  new = empty(rest)
  itr = step(rest)
  while !isnothing(itr)
    pair,rest = itr
    if pair[1] == k
      new = reduce(push, rest, init=new)
      break
    else
      new = push(new, pair)
    end
    itr = step(rest, rest)
  end
  Map{K,V}(new)
end

Base.filter(pred::Function, m::Map{K,V}) where {K,V} = Map{K,V}(filter(pred, m.pairs))

Base.merge(a::Map, b::Map) = Map(cat(a.pairs, b.pairs))
