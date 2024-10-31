function show(io::IO, mime::MIME"text/plain", s::Sexp)
  print(io, string(s))
end

#### Metadata

# I'm not really using this. I think I'll go with a different mechanism in the
# end. Out of band metadata will make some things easier and others harder, but
# I think it's a better approach.

struct MetaExpr <: Sexp
  metadata::Sexp
  content::Sexp
end

function withmeta(f, m)
  MetaExpr(m, f)
end

function meta(x)
  nothing
end

function meta(x::MetaExpr)
  x.metadata
end

# A keyword is a name intended for use soley as a name. It should mean something
# to humans who read it.
struct Keyword <: Sexp
  names::Base.Vector{String}
end

const basehash = hash(":")

hash(k::Keyword) = transduce(map(hash), xor, basehash, k.names)

Base.:(==)(x::Keyword, y::Keyword) = x.names == y.names

function string(s::Keyword)
  into(":", map(string) ∘ interpose("."), s.names)
end

# TODO: Intern keywords
function keyword(names::String...)
  Keyword(names)
end

function splitsymbolic(x::String)
  if x == "."
    ["."]
  else
    split(x, '.')
  end
end

function keyword(name::String)
  Keyword(splitsymbolic(name))
end

keyword(s::Core.Symbol) = keyword(string(s))

function name(x::Keyword)
  x.names[end]
end

#### Symbols

# A Symbol is a name standing in for another value and is not well defined
# without knowing that value.
struct Symbol <: Sexp
  names::Base.Vector{String}
end

function symbol(names::String...)
  Symbol(names)
end

function symbol(k::Keyword)
  Symbol(k.names)
end

function symbol(name::String)
  Symbol(splitsymbolic(name))
end

const symhash = hash("#Symbol")

hash(s::Symbol) = transduce(map(hash), xor, symhash, s.names)

function Base.:(==)(x::Symbol, y::Symbol)
  x.names == y.names
end

function string(s::Symbol)
  into("", map(string) ∘ interpose("."), s.names)
end

function name(x::Symbol)
  x.names[end]
end

#### Other

function name(x::String)
  x
end

function keyword(s::Symbol)
  Keyword(s.names)
end
