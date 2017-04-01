program test_pcre

  use, intrinsic :: iso_c_binding
  use pcre_module

  implicit none

  type(pcre_type) :: regex

  character(len=*), parameter :: pattern = "(a..)"
  character(len=*), parameter :: subject = "what is this?"

  integer, parameter :: ovecsize = 30
  integer, dimension(0:ovecsize-1) :: ovector

  integer :: error

  regex = pcre_compile(pattern, 0)

  if (.not. c_associated(regex%regex)) then
     print*,"PCRE compilation failed"
     stop 1
  end if

  error = pcre_exec(regex, c_null_ptr, subject, 0, 0, ovector)

  if (error < 0) then
     select case(error)
     case (PCRE_ERROR_NOMATCH)
        print*,"No match"
     case default
        print*,"Matching error ", error
     end select
     stop 1
  end if

  print('(A,I0)'),"Match succeeded at offset ", ovector(0)

  if (error == 0) then
     error = ovecsize / 3
     print('(A,I0,A)'),"ovector only has room for ", error - 1, " captured substrings"
  end if

  block
    integer :: i
    integer :: substring_start
    integer :: substring_length

    do i = 0, error-1
       substring_start = ovector(2*i) + 1
       substring_length = ovector(2*i+1) - ovector(2*i) + 1
       print('(I0,": ", A)'), i,  subject(substring_start:substring_length)
    end do
  end block

end program test_pcre
