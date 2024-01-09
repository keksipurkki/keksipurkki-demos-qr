module assertions
contains

  subroutine assert(ok, message)
    logical, intent(in) :: ok
    character(*), intent(in) :: message
    if (ok) then
      return
    else
      print '(A)', message
      stop 1
    end if
  end subroutine

end module
