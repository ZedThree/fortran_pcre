program test_pcre

  use, intrinsic :: iso_c_binding
  use pcre_module

  implicit none

  type(pcre_type) :: regex
  type(c_ptr) :: c_regex

  character(len=*), parameter :: pattern = ".*(a..).*"
  character(len=*), parameter :: subject = "what is this?"

  integer, parameter :: ovecsize = 30
  integer, dimension(ovecsize) :: ovector

  integer :: error

  ! character(len=*) :: f_pattern = pattern // c_null_char
  character(len=len_trim(pattern)+1, kind=c_char) :: c_pattern
  character(len=len_trim(subject)+1, kind=c_char) :: c_subject

  regex = pcre_compile(pattern, 0)

  error = pcre_exec(regex, c_null_ptr, subject, 0, 0, ovector)

  print*, ovector, error

  ! NULL-terminate pattern
  c_pattern = trim(pattern) // c_null_char
  c_subject = trim(subject) // c_null_char

  c_regex = c_my_pcre_compile(c_pattern)
  error = c_my_pcre_exec(c_regex, c_subject)
  print*, c_regex, error

  error = c_my_together(c_pattern, c_subject)
  print*, error

end program test_pcre
