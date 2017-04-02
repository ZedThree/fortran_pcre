program test_pcre

  use, intrinsic :: iso_c_binding
  use pcre_module

  implicit none

  type(pcre_type) :: regex

  character(len=*), parameter :: pattern &
       = "[ \s,]*(~@|[\[\]{}()'`~^@]|""(?:\\.|[^\\""])*""|;.*|[^ \s\[\]{}('""`,;)]*)"
  character(len=*), parameter :: subject = "(+ 2 (* 3 4))"
  integer, parameter :: subject_length = len_trim(subject)

  integer, parameter :: ovecsize = 30
  integer, dimension(0:ovecsize-1) :: ovector

  integer :: error

  print*, ""

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
    integer :: substring_end

    do i = 0, error-1
       substring_start = ovector(2*i) + 1
       substring_end = ovector((2*i)+1)
       print('(I0,": ", A)'), i,  subject(substring_start:substring_end)
    end do
  end block

  block
    integer :: status
    integer(c_int), target :: namecount
    type(c_ptr) :: c_namecount

    c_namecount = c_loc(namecount)

    status = pcre_fullinfo(regex, c_null_ptr, PCRE_INFO_NAMECOUNT, c_namecount)

    if (namecount <= 0) then
       print*, "No named substrings"
    else
       ! Get named substrings
    end if

  end block

  block

    integer :: options
    integer :: start_offset

    do
       options = 0
       start_offset = ovector(1)

       ! If the previous match was for an empty string, we are finished if we are
       ! at the end of the subject. Otherwise, arrange to run another match at the
       ! same point to see if a non-empty match can be found.

       if (ovector(0) == ovector(1)) then
          if (ovector(0) == subject_length) exit
          ! options = PCRE_NOTEMPTY_ATSTART | PCRE_ANCHORED;
          options = 268435464
       end if

       ! Run the next matching operation

       error = pcre_exec(regex, c_null_ptr, subject, start_offset, options, ovector)

       if (error == PCRE_ERROR_NOMATCH) then
          if (options == 0) exit
          ovector(1) = start_offset + 1
          continue
       end if

       if (error < 0) then
          print('(A,I0)'), "Matching error ", error
          stop 1
       end if

       print*,""
       print('(A,I0)'), "Match succeeded again at offset ", ovector(0)

       block
         integer :: i
         integer :: substring_start
         integer :: substring_end

         do i = 0, error-1
            substring_start = ovector(2*i) + 1
            substring_end = ovector((2*i)+1)
            print('(I0,": ", A)'), i,  subject(substring_start:substring_end)
         end do
       end block
       print*, "No named substrings"
    end do
  end block

end program test_pcre
