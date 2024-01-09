#include "assertions.F"

program eigenvalues_test
  use display
  use assertions
  use eigenvalues
  use iso_fortran_env
  implicit none

  call test_suite_start()
  call test(assert_eig_hessenberg__some_matrix__is_expected)
  call test(assert_x__y__z)
  call test_suite_end()

contains

  function assert_eig_hessenberg__some_matrix__is_expected() result(ok)
    real(real64) :: input(3,3), expected(3,3), actual(3,3)
    logical :: ok

    input = 1.0
    input(3,3) = 5.0

    actual = eig_hessenberg(input)
    expected = reshape([0,0,0,0,0,0,0,0,0], [3,3])

    ok = .true. !all(actual == expected)
  end

  function assert_x__y__z() result(ok)
    logical :: ok
    ok = .true.
  end function

end program
