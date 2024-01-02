program test
  real :: a(2,2), b(2,2), c(2,2)
  a = reshape([1,2,2,1], [2,2])
  b = reshape([0,1,0,0], [2,2])
  c = a*b
  print *, c
end program
