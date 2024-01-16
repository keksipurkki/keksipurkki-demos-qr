#define test(test_case) test_suite___(test_case, 'test_case')
#define test_suite_prepare(cfg) test_suite_prepare___(__FILE__, cfg)
#define assert(ok, message) assert___(__FILE__, __LINE__, ok, message)

module assertions
  use iso_fortran_env

  private
  public :: test_suite_config
  public :: test_run
  public :: test_suite_prepare___, test_suite___, test_suite_run
  public :: assert___

  type test_suite_config
    private

    procedure(assertion), public, nopass, pointer    :: only => null()
    type(integer), public, allocatable, dimension(:) :: skip

    character(:), allocatable :: test_suite_heading
    type(test_case), allocatable :: test_cases(:)
    integer :: num_tests = 0
  end type

  type test_case
    procedure(assertion), nopass, pointer :: assertion
    character(:), allocatable :: name
    integer :: test_id
  end type

  type test_run
    type(test_case) :: test_case
    logical :: outcome = .false.
    character(:), allocatable :: message
  contains
    private
    procedure, public :: ok
    procedure, public :: skip
    procedure :: execute
    procedure :: to_string
    procedure :: description
    procedure :: status
  end type

  abstract interface
    subroutine assertion(run)
      import :: test_run
      type(test_run), intent(inout) :: run
    end
  end interface

  type(test_suite_config) :: config = test_suite_config()

  character(4) :: ansi_reset
  character(5) :: ansi_red_fg

contains

  subroutine execute(self, argv0, rc)
    class(test_run) :: self
    character(*), intent(in) :: argv0
    integer, intent(out) :: rc
    character(128) :: command
    character(128) :: message

    if (any(config%skip == self%test_case%test_id)) then
      call self%skip()
      return
    endif

    write(command, '(A,X,A)') trim(argv0), self%test_case%name
    call execute_command_line(command, wait=.true., exitstat=rc)
    write(message, '(A,X,I0)') 'Test run crashed with status code', rc

    if (rc > 1) then
      call self%ok(.false., trim(message), no_backtrace=.true.)
    endif
  end

  subroutine ok(self, is_ok, message, no_backtrace)
    class(test_run) :: self
    logical :: is_ok
    character(*), optional, intent(in) :: message
    logical, value, optional :: no_backtrace

    if (.not.present(no_backtrace)) no_backtrace = .false.

    self%outcome = is_ok

    if (present(message)) then
      self%message = message
    else
      self%message = ''
    endif

    write(output_unit, '(A)') self%to_string()
    if (.not.is_ok .and. .not.no_backtrace) then
      write(error_unit, '(A)') ansi_red_fg
      call backtrace()
      write(error_unit, '(A)') ansi_reset
    endif
  end

  subroutine skip(self)
    class(test_run) :: self
    call self%ok(.true., '# SKIP')
  end subroutine

  function to_string(self) result(str)
    class(test_run) :: self
    character(128) :: fmt, tmp
    character(:), allocatable :: str
    fmt = "(A, X, I0, A)"
    write(tmp, fmt) self%status(), self%test_case%test_id, ' - '//self%description()
    allocate(character(len_trim(tmp))::str)
    str = trim(tmp)
  end

  function description(self)
    class(test_run), intent(in) :: self
    character(:), allocatable :: description
    description = self%test_case%name
    if (len_trim(self%message) > 0) then
      description = description // ' - ' // self%message
    endif
  end

  function status(self)
    class(test_run), intent(in) :: self
    character(:), allocatable :: status
    if (self%outcome) then
      status = 'ok'
    else
      status = 'not ok'
    endif
  end

  subroutine assert___(fname, linenr, ok, message)
    character(*), intent(in) :: fname, message
    integer, intent(in) :: linenr
    logical, intent(in) :: ok

    if (ok) then
      return
    else
      write(error_unit, '(3(G0,:,":"))') fname, linenr, message
      call exit(1)
    end if
  end subroutine

  subroutine test_suite___(it, test_case_name)
    procedure(assertion) :: it
    character(*), intent(in) :: test_case_name
    type(test_case), allocatable :: copy(:)
    integer :: num_tests

    num_tests = config%num_tests

    if (.not.allocated(config%test_cases)) then
      allocate(config%test_cases(10))
    endif

    if (num_tests >= size(config%test_cases)) then
      copy = config%test_cases
      deallocate(config%test_cases)
      allocate(config%test_cases(2*num_tests))
      config%test_cases(1:num_tests) = copy
    endif

    num_tests = num_tests + 1

    config%num_tests = num_tests
    config%test_cases(num_tests) = test_case(assertion=it, test_id=num_tests, name=test_case_name)

  end

  subroutine test_suite_prepare___(heading, cfg)
    integer, parameter :: SIGFPE = 8
    integer, parameter :: SIGSEGV = 11
    character(*), intent(in), optional :: heading
    type(test_suite_config), intent(in), optional :: cfg

    call signal(SIGFPE, signal_handler)
    call signal(SIGSEGV, signal_handler)
    ansi_red_fg = transfer([integer(int8) ::  27, 91, 51, 49, 109], ansi_red_fg)
    ansi_reset = transfer([integer(int8) :: 27, 91, 48, 109], ansi_reset)

    config = cfg
    config%test_suite_heading = heading
  end subroutine

  ! POSIX.1-2017:  void (*func)(int)
  subroutine signal_handler(signum) bind(C)
    use iso_c_binding, only: c_int
    integer(c_int), value :: signum
    error stop signum
  end subroutine

  recursive subroutine test_suite_run()

    ! Cmdline
    integer :: nargs
    character(128) :: argv0, arg

    if (associated(config%only)) then
      call test_suite_run_only(config%only)
      call test_suite_run()
      return
    endif

    nargs = command_argument_count()

    select case(nargs)
      case (0)
        write(output_unit, '(A, A)') 'Test suite: ', config%test_suite_heading
        write(output_unit, '(80("-"))')
        call get_command_argument(0, argv0)
        call run(trim(argv0))
        return
      case (1)
        call get_command_argument(0, argv0)
        call get_command_argument(1, arg)
        call run(trim(argv0), test_case_name=trim(arg))
        return
      case default
        call assert(nargs < 2, 'Expected zero or one arguments')
    end select

  end subroutine

  subroutine run(cmd, test_case_name)
    character(*), intent(in) :: cmd
    character(*), optional, intent(in) :: test_case_name
    integer :: i, rc, num_tests
    type(test_case), allocatable :: test_cases(:)
    type(test_case), allocatable :: it
    type(test_run) :: r
    logical :: ok

    test_cases = config%test_cases
    num_tests = config%num_tests

    if (.not.present(test_case_name)) then

      ok = .true.

      do i = 1, num_tests
        r = test_run(test_case=test_cases(i))
        call r%execute(cmd, rc)
        if (rc /= 0) ok = .false.
      enddo

      if (.not.ok) then
        write(error_unit, '(/,80("-"), /, A ,/ ,80("-"))') 'Some tests failed'
        call assert(ok, 'Some tests failed')
      endif

    else

      do i = 1, num_tests
        if (test_cases(i)%name == test_case_name) it = test_cases(i)
      enddo

      call assert(allocated(it), 'No such test: ' // test_case_name)
      r = test_run(test_case=it)

      call it%assertion(r)
      ok = r%outcome

      ! NB: The stop and error stop intrinsics output noise
      if (.not.ok) call exit(1)
      if (ok) call exit(0)

    endif
  end

  subroutine test_suite_run_only(it)
    procedure(assertion), pointer :: it
    integer :: i

    do i = 1, config%num_tests
      if (associated(config%test_cases(i)%assertion, it)) then
        exit
      endif
    enddo

    config%test_cases = [config%test_cases(i)]
    config%num_tests = size(config%test_cases)
    config%only => null()
  end

end module
