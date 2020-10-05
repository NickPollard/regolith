function add(x)
  return function(y)
    return x + y
  end
end

function add2()
  return add(2)
end

function add3()
  return add(3)
end

function main()
  print "hello, world"
end
