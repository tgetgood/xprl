abstract type List end

# Lists conj onto the front, as if they were cons cell linked lists, but they
# are not linked lists.
# Under the hood, they're just vectors where elements are added to the right
# side, but indexing is flipped by the length so that we start reading at the
# right side as well.

# FIXME: This is very old and doesn't play well with Julia's type system.

struct VectorList <: List
  contents::Vector
end

const vlh = 0x06ce3ebcedd51feb

hash(x::VectorList) = xor(vlh, hash(x.contents))

struct VectorListSeq <: List
  contents::Vector
  last
end

emptylist = VectorList(emptyvector)

function count(x::VectorList)
  count(x.contents)
end

function count(x::VectorListSeq)
  x.last
end

function length(x::List)
  count(x)
end

function first(x::VectorList)
  nth(x.contents, count(x.contents))
end

function nth(l::VectorList, i)
  nth(l.contents, count(l) - i + 1)
end

function rest(x::VectorList)
  if count(x) == 0
    emptylist
  else
    VectorListSeq(x.contents, count(x) - 1)
  end
end

function first(x::VectorListSeq)
  if count(x) > 0
    nth(x.contents, x.last)
  else
    nil
  end
end

function rest(x::VectorListSeq)
  if x.last < 1
    emptylist
  else
    VectorListSeq(x.contents, x.last - 1)
  end
end

function conj(l::VectorList, x)
  VectorList(conj(l.contents, x))
end

function list(xs...)
  tolist(xs)
end

function tolist(xs)
  VectorList(reverse(vec(xs)))
end

function vec(args::List)
  r = emptyvector
  for i = count(args):-1:1
    r = conj(r, nth(args, i))
  end
  r
end

function string(x::List)
  "(" * transduce(interpose(" ") ∘ map(string), *, "", x) * ")"
end

function iterate(v::List)
  first(v), rest(v)
end

function iterate(v::List, state)
  if count(state) == 0
    nothing
  else
    first(state), rest(state)
  end
end
