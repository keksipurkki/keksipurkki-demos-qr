#include "assertions.F"

program eigenvalues_test
  use display
  use assertions
  use eigenvalues
  use utils
  use iso_fortran_env
  implicit none

  call test_suite_prepare()
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
  call test_suite_run()

contains

  subroutine assert_eig_hessenberg_matrix_identity_noop(it)
    type(test_run), intent(inout) :: it
    logical :: ok
    ok = all(eig_hessenberg_matrix(eye(20)) == eye(20))
    call it%ok(ok, 'Hessenberg form of the identity is the identity')
  end

  subroutine assert_eig_hessenberg_matrix_some_matrix_is_expected(it)
    type(test_run), intent(inout) :: it
    real(real64) :: input(3,3), expected(3,3), actual(3,3)
    real(real64) :: vec(9)
    logical :: ok

    input = 1.0
    input(3,3) = 5.0
    input(1,1) = -5.0

    actual = eig_hessenberg_matrix(input)

    vec = [-5.000000000000000d0,-1.414213562373094d0, 0.000000000000000d0,&
      -1.414213562373094d0, 3.999999999999998d0,-2.000000000000000d0,&
       0.000000000000000d0,-2.000000000000000d0, 2.000000000000000d0]

    expected = reshape(vec, [3,3])

    ok = all(abs(actual - expected) < 1e-15)
    call it%ok(ok)
  end

  subroutine assert_eig_hessenberg_matrix_twice_same_up_to_sign(it)
    type(test_run), intent(inout) :: it
    real(real64) :: input(3,3), expected(3,3), actual(3,3)
    logical :: ok

    call random_number(input)

    actual = eig_hessenberg_matrix(input)
    expected = eig_hessenberg_matrix(actual)

    ok = all(abs(actual) == abs(expected))
    call it%ok(ok)
  end

  subroutine assert_eig_big_identity_matrix_works(it)
    type(test_run), intent(inout) :: it
    integer, parameter :: s = 2000
    real(real64), allocatable :: identity(:,:)
    real(real64) :: expected(s), actual(s)
    logical :: ok
    identity = eye(s)

    expected = 1.0d0
    actual = eig_hessenberg(identity)
    ok = all(expected == actual)

    call it%ok(ok, 'Should be fast')
  end

  subroutine assert_eig_epsilon_perturbation_no_overflow(it)
    type(test_run), intent(inout) :: it
    real(real64), parameter :: eps = epsilon(1.0_real64)
    real(real64) :: actual(2), expected(2)
    real(real64) :: input(2,2)

    expected = [2.0d0, -eps/2.0d0]

    input(1,:) = [1.0d0, 1.0d0 + eps]
    input(2,:) = [1.0d0, 1.0d0]

    actual = eig(input)
    call it%ok(all(actual == expected))
  end

  subroutine assert_eig_singular_zero_eigenvalue(it)
    type(test_run), intent(inout) :: it
    logical :: ok
    real(real64), parameter :: eps = epsilon(1.0_real64)
    real(real64) :: actual(2), expected(2)
    real(real64) :: input(2,2)

    expected = [2.d0, 0.d0]

    input(1,:) = [1.0d0, 1.0d0]
    input(2,:) = [1.0d0, 1.0d0]

    actual = eig(input)

    ok = all(actual == expected)
    call it%ok(ok)
  end

  subroutine assert_eig_repeated_expected(it)
    type(test_run), intent(inout) :: it
    logical :: ok
    real(real64), parameter :: eps = epsilon(1.0_real64)
    real(real64) :: actual(2), expected(2)
    real(real64) :: input(2,2)

    expected = [1.d0, 1.d0]

    input(1,:) = [1.0d0, 3.0d0]
    input(2,:) = [0.0d0, 1.0d0]

    actual = eig(input)

    ok = all(actual == expected)
    call it%ok(ok)
  end

  subroutine assert_eig_rectangular_matrix_does_not_work(it)
    type(test_run), intent(inout) :: it
    real(real64), allocatable :: L(:)
    real(real64) :: input(2,1) = 0.0d0
    !L = eig(input)
    call it%skip()
  end

  subroutine assert_eig_bad_itermax_does_not_work(it)
    type(test_run), intent(inout) :: it
    call it%skip()
  end

  subroutine assert_eig_itermax_one_no_convergence(it)
    type(test_run), intent(inout) :: it
    real(real64), allocatable :: yolo(:)
    real(real64) :: input(5,5) = 0.0d0
    call random_number(input)
    input = 10*input
    allocate(yolo(1))
    yolo = eig(input, itermax=1)
    call it%skip()
  end

end program
