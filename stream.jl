@use "github.com/jkroso/Promises.jl" Promise need @defer
@use "." Sequence EOS rest

"""
Streams are Sequences where the tail is always a Promise. Thereby enabling the sequence
to be lazily or asynchronously generated
"""
struct Stream{T} <: Sequence{T}
  head::T
  tail::Promise
end
Stream(first, tail=convert(Promise, EOS)) = Stream(first, tail)

Base.first(s::Stream) = s.head
rest(s::Stream) = need(s.tail)

# Lazy character streams from IO objects
Base.convert(::Type{Sequence}, io::IO) = convert(Sequence{UInt8}, io)
Base.convert(::Type{Sequence{T}}, io::IO) where T = begin
  if eof(io)
    close(io)
    EOS
  else
    Stream(read(io, T), @defer convert(Sequence{T}, io)::Sequence)
  end
end
