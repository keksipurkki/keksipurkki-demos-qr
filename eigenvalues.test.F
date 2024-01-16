#include "assertions.F"

program eigenvalues_test
  use display
  use assertions
  use eigenvalues
  use utils
  use iso_fortran_env
  implicit none

  type(test_suite_config) :: cfg = test_suite_config()
  cfg%only => null()
  cfg%skip = [integer::]

  call test_suite_prepare(cfg)
  call test(assert_eig_hessenberg_matrix_identity_noop)
  call test(assert_eig_hessenberg_matrix_some_matrix_is_expected)
  call test(assert_eig_hessenberg_matrix_twice_same_up_to_sign)
  call test(assert_eig_epsilon_perturbation_no_overflow)
  call test(assert_eig_big_identity_matrix_works)
  call test(assert_eig_rectangular_matrix_early_return)
  call test(assert_eig_bad_itermax_no_convergence)
  call test(assert_eig_singular_zero_eigenvalue)
  call test(assert_eig_repeated_expected)
  call test(assert_eig_complex_eigenvalues_strictly_conjugates)
  call test(assert_eig_reference_matrix_expected)
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
    complex(real64) :: expected(s), actual(s)
    logical :: ok
    identity = eye(s)

    expected = cmplx(1.0d0, kind=real64)
    actual = eig_hessenberg(identity)
    ok = all(expected == actual)

    call it%ok(ok, 'Should be fast')
  end

  subroutine assert_eig_epsilon_perturbation_no_overflow(it)
    type(test_run), intent(inout) :: it
    real(real64), parameter :: eps = epsilon(1.0_real64)
    real(real64), allocatable :: actual(:), expected(:)
    real(real64) :: input(2,2)

    expected = [2.0d0, -eps/2.0d0]

    input(1,:) = [1.0d0, 1.0d0 + eps]
    input(2,:) = [1.0d0, 1.0d0]

    call eig(input, actual)
    call it%ok(all(actual == expected))
  end

  subroutine assert_eig_singular_zero_eigenvalue(it)
    type(test_run), intent(inout) :: it
    logical :: ok
    real(real64), parameter :: eps = epsilon(1.0_real64)
    real(real64), allocatable :: actual(:), expected(:)
    real(real64) :: input(2,2)

    expected = [2.d0, 0.d0]

    input(1,:) = [1.0d0, 1.0d0]
    input(2,:) = [1.0d0, 1.0d0]

    call eig(input, actual)

    ok = all(actual == expected)
    call it%ok(ok)
  end

  subroutine assert_eig_repeated_expected(it)
    type(test_run), intent(inout) :: it
    logical :: ok
    real(real64), parameter :: eps = epsilon(1.0_real64)
    real(real64), allocatable :: actual(:), expected(:)
    real(real64) :: input(2,2)

    expected = [1.d0, 1.d0]

    input(1,:) = [1.0d0, 3.0d0]
    input(2,:) = [0.0d0, 1.0d0]

    call eig(input, actual)

    ok = all(actual == expected)
    call it%ok(ok)
  end

  subroutine assert_eig_rectangular_matrix_early_return(it)
    type(test_run), intent(inout) :: it
    logical :: ok
    real(real64), allocatable :: L(:)
    real(real64) :: input(2,1) = 0.0d0
    call eig(input, L)
    ok = all(L == 0.0d0)
    call it%ok(ok, 'Expected early return')
  end

  subroutine assert_eig_bad_itermax_no_convergence(it)
    type(test_run), intent(inout) :: it
    real(real64), allocatable :: L(:)
    logical :: ok(3)
    integer :: itermax
    real(real64) :: input(50, 50)

    call random_number(input)
    input = 10.0d0*input

    do itermax = 1, -1, -1
      call eig(input, L, itermax=itermax)
      ok(itermax + 2) = all(L == 0.0d0)
      deallocate(L)
    enddo

    call it%ok(all(ok), 'Expected early return')
  end

  subroutine assert_eig_complex_eigenvalues_strictly_conjugates(it)
    real(real64), parameter :: tol = 2*epsilon(1.0_real64)
    type(test_run), intent(inout) :: it
    complex(real64), allocatable :: L(:)
    real(real64), allocatable :: X(:, :)
    real(real64) :: N(2,2), tr, det
    logical :: ok

    allocate(X(2,2), source=0.d0)
    X(1,:) = [1d0, -1d0]
    X(2,:) = [1d0, 1d0]

    call random_number(N)
    X = X + 1e-3*N

    call eig(X, L)

    det = X(1,1)*X(2,2) - X(1,2)*X(2,1)
    tr = X(1,1) + X(2,2)
    print *, 'Should be zero', abs(L*L - tr*L + det)
    ok = L(1) == conjg(L(2)) .and. aimag(L(1)) /= 0.0d0
    call it%ok(ok, 'Expected complex conjugates')
  end

  subroutine assert_eig_reference_matrix_expected(it)
    type(test_run), intent(inout) :: it
    real(real64), target, allocatable :: X(:, :), Y(:, :)
    real(real64), pointer :: diag(:)
    real(real64), allocatable :: actual(:), expected(:)
    real(real64) :: err, tol

    expected = [1d0, 2d0, 3d0]

    X = reshape([0.3561220940595548d0,-0.4798716541404344d0,0.801810607110657d0,&
                 0.801810607110657d0, 0.5975763087872217d0,0.001518387657449716d0,&
                -0.4798716541404345d0, 0.6423595182829955d0,0.5975763087872218d0],&
                [3,3], order=[2,1])

    allocate(Y(3,3), source=0.0d0)
    diag => diagonal(Y)
    diag = expected

    Y = matmul(X, matmul(Y, transpose(X)))
    call eig(Y, actual)

    actual = sorted(actual)

    ! Error bound from similarity transforming X back to Y
    Y = matmul(transpose(X), matmul(Y, X))
    diag => diagonal(Y)

    tol = maxval(abs(diag - expected))
    err = maxval(abs(expected - actual) / abs(expected))
    print *, err, tol
    call it%ok(err <= tol, 'Relative error should less or equal to '//tostring(tol))
  end

end program
