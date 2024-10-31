module Reader

import ..AST as ast

import DataStructures as ds

abstract type Stream end

struct BufferedStream <: Stream
  stream::IO
  buffer::Base.Vector{Char}
end

mutable struct StringStream <: Stream
  stream::String
  index::UInt
end

function read1(s::BufferedStream)
  if length(s.buffer) > 0
    return popfirst!(s.buffer)
  else
    return read1(s.stream)
  end
end

function unread1(s::BufferedStream, c::Char)
  pushfirst!(s.buffer, c)
end

function read1(s::StringStream)
  try
    c = s.stream[s.index]
    s.index = nextind(s.stream, s.index)
    return c
  catch e
    if e isa BoundsError
      throw(EOFError())
    else
      rethrow(e)
    end
  end
end

function unread1(s::StringStream, c::Char)
  j = prevind(s.stream, s.index)
  @assert s.stream[j] === c "Cannot pushback char that was not in stream"
  s.index = j
end

function tostream(s::IO)
  BufferedStream(s, [])
end

function tostream(s::String)
  StringStream(s, 1)
end

ts = "łβ∘"

struct ReaderOptions
  until
  env
end

function until(opts, c)
  ReaderOptions(c, opts.env)
end

## This will raise an error on EOF. That's normally the right behaviour, but we
## might need a softer try-read sort of fn.
function read1(stream)
  return Base.read(stream, Char)
end

whitespace = r"[\s,]"

iswhitespace(c) = match(whitespace, string(c)) !== nothing

function firstnonwhitespace(stream)
  c::Char = ' '
  while iswhitespace(c)
    c = read1(stream)
  end
  return c
end

function splitsymbolic(x::String)
  if x == "."
    ["."]
  else
    split(x, '.')
  end
end

function readkeyword(x, _)
  ds.keyword(x)
end

function readsymbol(x, opts)
  ds.symbol(x)
end

function interpret(x::String, opts)
  if startswith(x, ':')
    return readkeyword(x, opts)
  end
  try
    return parse(Int, x)
  catch e
    if e isa ArgumentError
    else
      rethrow(e)
    end
  end

  # TODO: Read in floats as rationals.

  if x == "true"
    return true
  elseif x == "false"
    return false
  else
    return readsymbol(x, opts)
  end
end

function readsubforms(stream, opts)
  forms = []
  while true
    t = read(stream, opts)
    if t === :close
      break
    elseif t === nothing
      continue
    else
      push!(forms, t)
    end
  end
  return forms
end

function readpair(stream, opts)
  subs = readsubforms(stream, until(opts, ')'))

  # REVIEW: This is an ugly hack.
  #
  # But would a null (or other token) terminated list be any better?
  #
  # Remember that the real goal of cons cells here is a syntax to describe
  # applying an arbitrary value to a callable while retaining the near universal
  # convention that callables take a list of arguments passed à la (f x y z).

  n = length(subs)
  if n === 0
    throw("The expression '()' is meaningless.")
  elseif n === 1
    ast.pair(subs[1], ds.emptyvector)
  # dot application: (f . x)
  elseif n === 3 && subs[2] == ds.symbol(".")
    ast.pair(subs[1], subs[3])
  # list catenation: (f x y z . more)
  elseif subs[end-1] == ds.symbol(".")
    ast.pair(subs[1], ds.into(ds.vec(subs[2:end-2]), subs[end]))
  # standard application: (f x y z)
  else
    ast.pair(subs[1], ds.vec(subs[2:end]))
  end
end

function readvector(stream, opts)
  ds.vec(readsubforms(stream, until(opts, ']')))
end

specialchars = Dict(
  't' => '\t',
  'r' => '\r',
  'n' => '\n',
  'b' => '\b',
  'f' => '\f',
  '"' => '"',
  '\\' => '\\'
)

function stopcondition(base)
  if base == 8
    return function(next)
      !(47 < Int(next) < 56)
    end
  elseif base == 16
    return function(next)
      i = Int(next)
      !(47 < i < 56 || 96 < i < 103 || 64 < i < 71)
    end
  end
end

function unicodestep(stream, sum, base)
  next = read1(stream)
  if stopcondition(base)(next)
    unread1(stream, next)
    sum, true
  else
    sum*base + parse(Int, "0x"*next), false
  end
end

function readunicodehex(stream, ch)
  done = false
  num = parse(Int, "0x"*ch)
  i = 0
  while (!done && i < 3)
    num, done = unicodestep(stream, num, 16)
    i = i + 1
  end
  return Char(num)
end

function readunicodeoctal(stream, ch)
  @assert 47 < Int(ch) && Int(ch) < 56 "Invalid digit"

  out, done = unicodestep(stream, parse(Int, ch), 8)
  if done
    return Char(out)
  end

  out, done = unicodestep(stream, out, 8)

  if out > 377
    throw("Octal escapes must be in the range [0, 377]")
  else
    return Char(out)
  end
end

function readstring(stream, opts)
  buf = []
  c = read1(stream)
  while (c != '"')
    if c == '\\'
      next = read1(stream)
      char = Base.get(specialchars, next, nothing)
      if char !== nothing
        push!(buf, char)
      elseif next == 'u'
        next = read1(stream)
        @assert !stopcondition(16)(next) "Invalid unicode escape"
        char = readunicodehex(stream, next)
        push!(buf, char)
      elseif isdigit(next)
        char = readunicodeoctal(stream, next)
        push!(buf, char)
      else
        throw("Invalid escape char: " * next)
      end
    else
      push!(buf, c)
    end
    try
      c = read1(stream)
    catch e
      @error e
    end
  end
  return string(buf...)
end

function readmap(stream, opts)
  elements = readsubforms(stream, until(opts, '}'))
  @assert length(elements) % 2 === 0 "a map literal must contain an even number of entries"

  res = ds.emptymap
  for i in 1:div(length(elements), 2)
    res = assoc(res, popfirst!(elements), popfirst!(elements))
  end
  return res
end

function readset(stream, opts)
  throw("not implemented")
end

function readcomment(stream, opts)
  c = read1(stream)
  while c != '\n' && c != '\r'
    c = read1(stream)
  end
end

function readanddiscard(stream, opts)
  read(stream, opts)
  return nothing
end

indirectdispatch = Dict(
  '_' => readanddiscard,
  '{' => readset
)

function readdispatch(stream, opts)
  c = read1(stream)
  reader = Base.get(indirectdispatch, c, nothing)
  if reader === nothing
    throw("Invalid dispatch macro: " * c)
  else
    reader(stream, opts)
  end
end

function readmeta(stream, opts)
  meta = read(stream, opts)
  val = read(stream, opts)
  if isa(meta, ds.Map)
    withmeta(val, meta)
  else
    withmeta(val, assoc(ds.emptymap, meta, true))
  end
end

function readimmediate(stream, opts)
  ast.immediate(read(stream, opts))
end

dispatch = Dict(
  '(' => readpair,
  '[' => readvector,
  '"' => readstring,
  '{' => readmap,
  '#' => readdispatch,
  ';' => readcomment,
  '^' => readmeta,
  '~' => readimmediate
)

delimiter = r"[({\[;]"

function istokenbreak(c)
  iswhitespace(c) ||  match(delimiter, string(c)) !== nothing
end

function readtoken(stream, opts)
  out = ""

  while true
    try
      c = read1(stream)
      if istokenbreak(c) || c === opts.until
        unread1(stream, c)
        break
      else
        out = out*c
      end
    catch e
      if typeof(e) == EOFError
        break
      else
        rethrow(e)
      end
    end
  end

  return out
end

function readinner(stream::Stream, opts::ReaderOptions)
  c = firstnonwhitespace(stream)

  if opts.until !== nothing && c === opts.until
    return :close
  end

  sub = Base.get(dispatch, c, nothing)

  if sub === nothing
    unread1(stream, c)
    return interpret(readtoken(stream, opts), opts)
  else
    return sub(stream, opts)
  end
end

function read(stream::Stream, opts::ReaderOptions)
  form = nothing
  try
    while form === nothing
      form = readinner(stream, opts)
    end
    form
  catch e
    if e isa EOFError
      nothing
    else
      rethrow(e)
    end
  end
end

function read(stream::Stream)
  read(stream, ReaderOptions(nothing, ds.emptymap))
end

function read(s::String)
  read(tostream(s))
end

function read(s::IO)
  read(tostream(s))
end

function readall(io::Stream)
  forms = []
  form = read(io)
  while form !== nothing
    push!(forms, form)
    form = read(io)
  end
  forms
end

function readall(io)
  readall(tostream(io))
end

end #module
