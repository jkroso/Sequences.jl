@use "github.com/jkroso/Promises.jl" Promise need @defer
@use "." Sequence rest EmptySequence

"""
Streams are Sequences where the tail is always a Promise. Thereby enabling the sequence
to be lazily generated
"""
struct Stream{T} <: Sequence{T}
  head::T
  tail::Promise
end
Stream(first::T, tail=convert(Promise, EmptySequence{T}(Stream{T}))) where T = Stream(first, tail)

Base.first(s::Stream) = s.head
rest(s::Stream) = need(s.tail)

# Lazy character streams from IO objects
Base.convert(::Type{Stream}, io::IO) = convert(Stream{UInt8}, io)
Base.convert(::Type{Stream{T}}, io::IO) where T = begin
  if eof(io)
    close(io)
    EmptySequence{T}(Stream{T})
  else
    Stream(read(io, T), @defer convert(Stream{T}, io)::Sequence)
  end
end
