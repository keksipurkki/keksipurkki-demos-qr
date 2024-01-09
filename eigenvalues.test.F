#include "assertions.F"

program eigenvalues_test
  use display
  use assertions
  use eigenvalues
  use iso_fortran_env
  implicit none

  call test_suite_start()
  call test(assert__eig_hessenberg__some_matrix__is_expected)
  call test(assert__eig_hessenberg__twice__same_up_to_sign)
  call test(assert__eig__epsilon_perturbation__no_overflow)
  call test(assert__eig__singular__zero_eigenvalue)
  call test(assert__eig__repeated__expected)
  call test_suite_end()

contains

  function assert__eig_hessenberg__some_matrix__is_expected() result(ok)
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

  function assert__eig_hessenberg__twice__same_up_to_sign() result(ok)
    real(real64) :: input(3,3), expected(3,3), actual(3,3)
    real(real64) :: vec(9)
    logical :: ok

    call random_number(input)

    actual = eig_hessenberg(input)
    expected = eig_hessenberg(actual)

    ok = all(abs(actual) == abs(expected))
  end

  function assert__eig__epsilon_perturbation__no_overflow() result(ok)
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

  function assert__eig__singular__zero_eigenvalue() result(ok)
    logical :: ok
    real(real64), parameter :: eps = epsilon(1.0_real64)
    real(real64) :: actual(2), expected(2)
    real(real64) :: input(2,2)

    expected = [2.d0, 0.d0]

    input(1,:) = [1.0d0, 1.0d0]
    input(2,:) = [1.0d0, 1.0d0]

    actual = eig(input)

    ok = all(actual == expected)
  end function

  function assert__eig__repeated__expected() result(ok)
    logical :: ok
    real(real64), parameter :: eps = epsilon(1.0_real64)
    real(real64) :: actual(2), expected(2)
    real(real64) :: input(2,2)

    expected = [1.d0, 1.d0]

    input(1,:) = [1.0d0, 3.0d0]
    input(2,:) = [0.0d0, 1.0d0]

    actual = eig(input)

    ok = all(actual == expected)
  end function

end program
