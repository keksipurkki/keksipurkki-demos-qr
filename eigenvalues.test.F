#include "assertions.F"

program eigenvalues_test
  use display
  use assertions
  use eigenvalues
  use utils
  use iso_fortran_env
  implicit none

  integer :: info

  call test_suite_start()
  call test(assert_eig_hessenberg_matrix_identity_noop)
  call test(assert_eig_hessenberg_matrix_some_matrix_is_expected)
  call test(assert_eig_hessenberg_matrix_twice_same_up_to_sign)
  call test(assert_eig_epsilon_perturbation_no_overflow)
  call test(assert_eig_big_identity_matrix_works)
  call test(assert_eig_rectangular_matrix_does_not_work)
  call test(assert_eig_bad_itermax_does_not_work)
  call test(assert_eig_itermax_one_no_convergence)
  call test(assert_eig_singular_zero_eigenvalue)
  call test(assert_eig_repeated_expected)
  call test_suite_end()

contains

  function assert_eig_hessenberg_matrix_identity_noop() result(ok)
    logical :: ok
    ok = all(eig_hessenberg_matrix(eye(20)) == eye(20))
  end

  function assert_eig_hessenberg_matrix_some_matrix_is_expected() result(ok)
    real(real64) :: input(3,3), expected(3,3), actual(3,3)
    real(real64) :: vec(9)
    logical :: ok

    input = 1.0
    input(3,3) = 5.0

    actual = eig_hessenberg_matrix(input)

    vec = [1.0000000000000000d0,-1.4142135623730945d0,0.0000000000000000d0, &
          -1.4142135623730945d0,3.9999999999999978d0,-1.9999999999999996d0, &
          1.1102230246251565D-016,-1.9999999999999996d0,2.0000000000000000d0]

    expected = reshape(vec, [3,3])

    ok = all(actual == expected)
  end

  function assert_eig_hessenberg_matrix_twice_same_up_to_sign() result(ok)
    real(real64) :: input(3,3), expected(3,3), actual(3,3)
    logical :: ok

    call random_number(input)

    actual = eig_hessenberg_matrix(input)
    expected = eig_hessenberg_matrix(actual)

    ok = all(abs(actual) == abs(expected))
  end

  function assert_eig_big_identity_matrix_works() result(ok)
    integer, parameter :: s = 2000
    logical :: ok
    real(real64), allocatable :: identity(:,:)
    real(real64) :: expected(s), actual(s)
    identity = eye(s)

    expected = 1.0d0
    actual = eig_hessenberg(identity, info)
    ok = all(expected == actual)
  end function

  function assert_eig_epsilon_perturbation_no_overflow() result(ok)
    logical :: ok
    integer :: info
    real(real64), parameter :: eps = epsilon(1.0_real64)
    real(real64) :: actual(2), expected(2)
    real(real64) :: input(2,2)

    expected = [2.0d0, -eps/2.0d0]

    input(1,:) = [1.0d0, 1.0d0 + eps]
    input(2,:) = [1.0d0, 1.0d0]

    actual = eig(input, info)
    ok = all(actual == expected)
  end function

  function assert_eig_singular_zero_eigenvalue() result(ok)
    logical :: ok
    real(real64), parameter :: eps = epsilon(1.0_real64)
    real(real64) :: actual(2), expected(2)
    real(real64) :: input(2,2)

    expected = [2.d0, 0.d0]

    input(1,:) = [1.0d0, 1.0d0]
    input(2,:) = [1.0d0, 1.0d0]

    actual = eig(input, info)

    ok = all(actual == expected)
  end function

  function assert_eig_repeated_expected() result(ok)
    logical :: ok
    real(real64), parameter :: eps = epsilon(1.0_real64)
    real(real64) :: actual(2), expected(2)
    real(real64) :: input(2,2)

    expected = [1.d0, 1.d0]

    input(1,:) = [1.0d0, 3.0d0]
    input(2,:) = [0.0d0, 1.0d0]

    actual = eig(input, info)

    ok = all(actual == expected)
  end function

  function assert_eig_rectangular_matrix_does_not_work() result(ok)
    logical :: ok
    real(real64), allocatable :: L(:)
    integer :: info
    real(real64) :: input(2,1) = 0.0d0
    L = eig(input, info=info)
    ok = info == EIG_ERR_RECTANGULAR_MATRIX
  end

  function assert_eig_bad_itermax_does_not_work() result(ok)
    logical :: ok
    real(real64), allocatable :: L(:)
    integer :: a, b, expected(2)
    real(real64) :: input(2,2) = 0.0d0
    expected = EIG_ERR_NO_CONVERGENCE
    L = [eig(input, itermax=0, info=a), eig(input, itermax=-1, info=b)]
    ok = all([a,b] == expected)
  end

  function assert_eig_itermax_one_no_convergence() result(ok)
    logical :: ok
    real(real64), allocatable :: L(:)
    real(real64) :: input(5,5) = 0.0d0
    call random_number(input)
    input = 10*input
    L = eig(input, itermax=1, info=info)
    ok = info == EIG_ERR_NO_CONVERGENCE
  end

end program
