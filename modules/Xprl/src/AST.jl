module AST

import Base: hash, ==, string, length, getindex, eltype, show, get, iterate

import DataStructures as ds
import DataStructures: containsp, walk, emptyp, count, ireduce, first, rest

##### Run time data structures

abstract type Node end

function show(io::IO, mime::MIME"text/plain", s::Node)
  print(io, string(s))
end

### Built ins

abstract type BuiltIn <: Node end

"""
Primitive functions expect their arguments to be literal values. They should
normally be wrapped to make sure args are evaluated.
"""
struct PrimitiveFunction <: BuiltIn
  f::Function
end

"""
Primitive macros operate directly on the AST of the program. They also receive
the lexical envronment when invoked.
"""
struct PrimitiveMacro <: BuiltIn
  f::Function
end

### Immediate (eval asap) forms

struct Immediate <: Node
  form
end

immediate(f) = Immediate(f)

string(f::Immediate) = "~" * string(f.form)

function walk(inner, outer, f::Immediate)
  outer(Immediate(inner(f.form)))
end

### Pairs.
##
## N.B. These are *not* cons cells. They're just pairs. What would be a cons
## list in lisp is here a pair whose tail is a vector (list).
##
## REVIEW: Maybe `head` and `tail` ought to be rethought in light of the above.

struct Pair <: Node
  head
  tail
end

pair(h, t) = Pair(h, t)

function tailstring(x)
  " . " * string(x)
end

function string(c::Pair)
  "(" * string(c.head) * tailstring(c.tail) * ")"
end

function walk(inner, outer, form::Pair)
  outer(Pair(inner(form.head), inner(form.tail)))
end

function tailstring(c::ds.Sequential)
  ds.into("", ds.interleave(), ds.repeat(" "), map(string, c))
end

function Base.:(==)(x::Pair, y::Pair)
  x.head == y.head && x.tail == y.tail
end

const phash = hash("#Pair")

function hash(x::Pair)
  xor(phash, hash(x.head), hash(x.tail))
end

### Application
##
### I.e. things that are to be applied as soon as possible

"""
Represents an application of args to a function-like entity which has not been
performed yet.
"""
struct Application <: Node
  head
  tail
end

function string(x::Application)
  "#Application(" * string(x.head) * ", " * string(x.tail) * ")"
end

function Base.:(==)(x::Application, y::Application)
  x.head == y.head && x.tail == y.tail
end

const apphash = hash("#Application")

function hash(x::Application)
  xor(apphash, hash(x.head), hash(x.tail))
end

### Mu
##
### The basic syntactic combinator of the language.

struct Mu <: Node
  env
  source
  params
  body
end

string(x::Mu) = "(μ " * string(x.params) * " " * string(x.body) * ")"

function Base.:(==)(x::Mu, y::Mu)
  x.params == y.params && x.body == y.body
end

const muhash = hash("#Mu")

function hash(x::Mu)
  xor(muhash, hash(x.params), hash(x.body))
end

### Partially applied Mu
##
### This exists because a Mu is ill defined unless its argument is a symbol. A
### partial μ is a binary node whose left tree will (presumably) evaluate to a
### symbol at some point in the future. At that point we can construct a proper
### μ operator. In the meantime, however, we need to treat the two cases as
### fundamentally different.

struct PartialMu <: Node
  params
  body
end

string(x::PartialMu) = "(μ " * string(x.params) * " " * string(x.body) * ")"

function Base.:(==)(x::PartialMu, y::PartialMu)
  x.params == y.params && x.body == y.body
end

function hash(x::PartialMu)
  xor(muhash, hash(x.params), hash(x.body))
end

##### Helpers

"""
Returns true iff the form cannot be further reduced and contains no immediate
evaluation. (Immediate evaluation just means evaluations that cannot be done yet
but must eventually be performed).
"""
reduced(form) = true
reduced(form::Immediate) = false
reduced(form::Application) = false
reduced(form::Pair) = reduced(form.head) && reduced(form.tail)
reduced(form::Mu) = reduced(form.params) && reduced(form.body)
reduced(form::ds.Vector) = ds.every(reduced, form)
reduced(form::ds.Map) = ds.every(reduced, form)
reduced(form::ds.MapEntry) = reduced(ds.key(form)) && reduced(ds.val(form))

### Debugging and inspection

function space(level)
  if level > 0
    print(repeat(" |", level))
  end
end

function inspect(form::Pair, level=0)
  space(level)
  println("P")
  inspect(form.head, level+1)
  inspect(form.tail, level+1)
end

function inspect(form::Immediate, level=0)
  space(level)
  println("I")
  inspect(form.form, level+1)
end

function inspect(form::ds.Symbol, level=0)
  space(level)
  println("S["*string(form)*"]")
end

function inspect(form::Application, level=0)
  space(level)
  println("A")
  inspect(form.head, level+1)
  inspect(form.tail, level+1)
end

function inspect(form::ds.Vector, level=0)
  space(level)
  println("L")
  for e in form
    inspect(e, level+1)
  end
end

function inspect(form::PartialMu, level=0)
  space(level)
  println("Pμ")
  inspect(form.params, level+1)
  inspect(form.body, level+1)
end

function inspect(form::Mu, level=0)
  space(level)
  println("μ")
  inspect(form.params, level+1)
  inspect(form.body, level+1)
end

function inspect(form::BuiltIn, level=0)
  space(level)
  println("F["*string(form)*"]")
end

function inspect(form, level=0)
  space(level)
  println("V["*string(typeof(form))*"]")
end

end # module
