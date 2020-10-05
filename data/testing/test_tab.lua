function foo(x,y)
  x[y] = 42
  x[42] = 43
  x[43] = y
  return x
end
