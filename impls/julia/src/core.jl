import Xprl as x
import Xprl.AST: inspect
import Xprl.System as sys
import Xprl.Reader as r

import DataStructures as ds

const srcpath = dirname(@__FILE__) * "/../../../src/"

env = Ref{Any}(x.DefaultEnv.default)

core = r.readall(open(srcpath * "core.xprl"))
tests = r.read(open(srcpath * "test.xprl"))

function eset(e)
  env[] = e
  nothing
end

o = Ref{Any}()

rc = sys.withcc(
  ds.emptymap,
  :env, eset,
  :return, inspect
)

rcc = sys.withcc(rc, :return, x -> o[] = x)

function evalseq(root, envvar, forms)
  function next(x)
    if x !== nothing
      inspect(x)
      if ds.count(forms) > 1
        evalseq(root, envvar, ds.rest(forms))
      end
    else
      @info "EOF"
    end
  end

  @info "compiling: " * string(first(forms))

  x.interpret(
    sys.withcc(root, :return, next, :env, x -> envvar[] = x),
    envvar[],
    first(forms)
  )
end

"""
Reads file `fname` and merges it into the given environment.
"""
function loadfile(envvar, fname)
  forms = r.readall(open(fname))

  evalseq(rc, envvar, forms)
end

function repl(envvar)
  print("> ")
  form = r.read(stdin)
  if form !== nothing
    function next(x)
      println(string(x))
      repl(envvar)
    end
    x.interpret(
      sys.withcc(ds.emptymap, :env, x -> envvar[] = x, :return, next),
      envvar[],
      form
    )
  end
end

loadfile(env, srcpath * "core.xprl")

# repl(env)
