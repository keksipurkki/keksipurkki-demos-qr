#include "assertions.F"

program eigenvalues_test
  use display
  use assertions
  use eigenvalues
  use iso_fortran_env
  implicit none

  call test_suite_start()
  call test(assert_eig_hessenberg__some_matrix__is_expected)
  call test(assert_eig__epsilon_perturbation__no_overflow)
  call test_suite_end()

contains

  function assert_eig_hessenberg__some_matrix__is_expected() result(ok)
    real(real64) :: input(3,3), expected(3,3), actual(3,3)
    real(real64) :: vec(9)
    logical :: ok

    input = 1.0
    input(3,3) = 5.0

    actual = eig_hessenberg(input)

    vec = [1.0000000000000000d0,-1.4142135623730945d0,0.0000000000000000d0, &
          -1.4142135623730945d0,3.9999999999999978d0,-1.9999999999999996d0, &
          1.1102230246251565D-016,-1.9999999999999996d0,2.0000000000000000d0]

    expected = reshape(vec, [3,3])

    ok = all(actual == expected)
  end

  function assert_eig__epsilon_perturbation__no_overflow() result(ok)
    logical :: ok
    real(real64), parameter :: eps = epsilon(1.0_real64)
    real(real64) :: actual(2), expected(2)
    real(real64) :: input(2,2)

    expected = [2.0d0, -eps/2.0d0]

    input(1,:) = [1.0d0, 1.0d0 + eps]
    input(2,:) = [1.0d0, 1.0d0]

    actual = eig(input)
    ok = all(actual == expected)
  end function

end program
