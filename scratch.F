program test
  use iso_fortran_env
  use display
  use utils
  implicit none

  character(:), allocatable :: argv0, cmdline
  integer :: i, status, nargs
  allocate(character(128)::argv0)
  allocate(character(128)::cmdline)

  nargs = command_argument_count()

  call get_command_argument(0, argv0)
  argv0 = argv0(1:len(trim(argv0)))

  if (nargs == 0) then
    do i = 1, 10
      write(cmdline, '(A,X,I0.2)') argv0, i
      call execute_command_line(trim(cmdline), exitstat=status)
      print *, cmdline, status
    enddo
  endif

  contains

end program
