# Shameless ripoff of Stuart Sierra's walk in clojure.

function walk(inner, outer, form::Sequential)
  outer(into(empty(form), map(inner), form))
end

function walk(inner, outer, e::MapEntry)
  outer(MapEntry(inner(e.key), inner(e.value)))
end

function walk(inner, outer, v)
  outer(v)
end

function postwalk(f, form)
  walk(x -> postwalk(f, x), f, form)
end

function prewalk(f, form)
  walk(x -> prewalk(f, x), identity, f(form))
end
