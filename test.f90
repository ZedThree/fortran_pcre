program test_pcre

  use pcre_module

  implicit none

  type(pcre_type) :: regex

  character(len=*), parameter :: pattern = "([0-9]+)"

  regex = pcre_compile(pattern, 0)

end program test_pcre
