@use "." Sequence EOS rest

mutable struct DoublyLinkedList{T} <: Sequence{T}
  value::T
  up::Sequence
  down::Sequence
end
DoublyLinkedList(x::T, up) where T = DoublyLinkedList{T}(x, up, EOS)
DoublyLinkedList(x::T) where T = DoublyLinkedList{T}(x, EOS, EOS)

rest(l::DoublyLinkedList) = l.down
Base.first(l::DoublyLinkedList) = l.value

Base.convert(::Type{DoublyLinkedList}, iter) = begin
  start = iterate(iter)
  isnothing(start) && return EOS
  state = start[2]
  first = DoublyLinkedList(start[1], EOS)
  up = first
  while true
    next = iterate(iter, state)
    isnothing(next) && break
    x, state = next
    down = DoublyLinkedList(x, up)
    up.down = down
    up = down
  end
  first
end

head(l::DoublyLinkedList) = l.up === EOS ? l : head(l.up)
tail(l::DoublyLinkedList) = l.down === EOS ? l : tail(l.down)
