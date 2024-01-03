program test
  real :: a(3)
  a = [1,2,3]
  print *, a
  print *, reshape(a, [1, size(a)], order=[1,2])
end program
