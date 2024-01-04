program test
  real :: a(3)
  a = [1,2,3]
  print *, sign(0d0, 0.0d0)
  print *, sign(-2.50d0, -2.5d0)
  print *, sign(2.3d0, 2.3d0)

  print *, sign(0d0, 0.0d0)
  print *, sign(-2.50d0, -2.5d0)
  print *, sign(2.3d0, 2.3d0)
end program
