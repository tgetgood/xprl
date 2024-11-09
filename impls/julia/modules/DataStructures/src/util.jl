# Things that have nothing to do with datastructures, but expose the fact that
# I'm really implementing a new language.

function handleerror(e)
  showerror(stderr, e)
  print(stderr, "\n")
  show(stderr, "text/plain", stacktrace(catch_backtrace()))
end
