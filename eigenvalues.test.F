#include "assertions.F"

program eigenvalues_test
  use display
  use assertions
  use eigenvalues
  use utils
  use iso_fortran_env
  implicit none

  character(*), parameter :: test_output_file = 'test_output.txt'
  character(:), allocatable :: test_output
  character(len=4096) :: filename
  integer :: io, rc = 0, length, pos

  open(newunit=io, file=test_output_file, action='write')
  call disp_set(unit=io)

  call test_suite_start()
  call test(assert_eig_hessenberg_some_matrix_is_expected)
  call test(assert_eig_hessenberg_twice_same_up_to_sign)
  call test(assert_eig_epsilon_perturbation_no_overflow)
  call test(assert_eig_big_identity_matrix_works)
  call test(assert_eig_rectangular_matrix_does_not_work)
  call test(assert_eig_bad_itermax_does_not_work)
  call test(assert_eig_singular_zero_eigenvalue)
  call test(assert_eig_repeated_expected)
  call test_suite_end()

  write(output_unit, *)

  write(output_unit, '(A, A)') 'Start: ', test_output_file
  write(output_unit, '(66("-"))')

  inquire(io, size=length)
  inquire(io, name=filename)

  allocate(character(length)::test_output)

  close(io, status='keep')
  open(newunit=io, file=test_output_file, access='stream', form='unformatted', action='read')
  read(io) test_output

  write(output_unit, '(A)', advance='no') test_output

  write(output_unit, '(66("-"))')
  write(output_unit, '(A,A)') 'End: ', test_output_file
  write(output_unit, *)

  call assert(all_pass, 'Some tests failed')
  close(io, status='delete')

contains

  function assert_eig_hessenberg_some_matrix_is_expected() result(ok)
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

  function assert_eig_hessenberg_twice_same_up_to_sign() result(ok)
    real(real64) :: input(3,3), expected(3,3), actual(3,3)
    real(real64) :: vec(9)
    logical :: ok

    call random_number(input)

    actual = eig_hessenberg(input)
    expected = eig_hessenberg(actual)

    ok = all(abs(actual) == abs(expected))
  end

  function assert_eig_big_identity_matrix_works() result(ok)
    integer, parameter :: s = 2000
    logical :: ok
    real(real64) :: identity(s,s)
    real(real64) :: expected(s), actual(s)
    identity = eye(s)

    expected = 1.0d0
    actual = eig(identity)

    ok = all(expected == actual)
  end function

  function assert_eig_epsilon_perturbation_no_overflow() result(ok)
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

  function assert_eig_singular_zero_eigenvalue() result(ok)
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

  function assert_eig_repeated_expected() result(ok)
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

  function assert_eig_rectangular_matrix_does_not_work() result(ok)
    logical :: ok
    real(real64), allocatable :: L(:)
    real(real64) :: input(2,1)
    L = eig(input)
    ok = all(L == [0.0d0, 0.0d0])
  end

  function assert_eig_bad_itermax_does_not_work() result(ok)
    logical :: ok
    real(real64), allocatable :: L(:)
    real(real64) :: input(2,2) = 0.0d0

    ok = .true.

    L = [eig(input, itermax=0), eig(input, itermax=-1)]
    ok = all(L == 0.0d0)
  end

end program
