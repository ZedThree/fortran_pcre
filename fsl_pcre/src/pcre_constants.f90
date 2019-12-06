module pcre_constants

  use, intrinsic :: iso_c_binding, only : c_int

  implicit none

  ! Possible errors
  integer(c_int), parameter :: PCRE_ERROR_NOMATCH          = -1
  integer(c_int), parameter :: PCRE_ERROR_NULL             = -2
  integer(c_int), parameter :: PCRE_ERROR_BADOPTION        = -3
  integer(c_int), parameter :: PCRE_ERROR_BADMAGIC         = -4
  integer(c_int), parameter :: PCRE_ERROR_UNKNOWN_OPCODE   = -5

  ! Request types for pcre_fullinfo
  integer(c_int), parameter :: PCRE_INFO_OPTIONS             = 0
  integer(c_int), parameter :: PCRE_INFO_SIZE                = 1
  integer(c_int), parameter :: PCRE_INFO_CAPTURECOUNT        = 2
  integer(c_int), parameter :: PCRE_INFO_BACKREFMAX          = 3
  integer(c_int), parameter :: PCRE_INFO_FIRSTBYTE           = 4
  integer(c_int), parameter :: PCRE_INFO_FIRSTTABLE          = 5
  integer(c_int), parameter :: PCRE_INFO_LASTLITERAL         = 6
  integer(c_int), parameter :: PCRE_INFO_NAMEENTRYSIZE       = 7
  integer(c_int), parameter :: PCRE_INFO_NAMECOUNT           = 8
  integer(c_int), parameter :: PCRE_INFO_NAMETABLE           = 9
  integer(c_int), parameter :: PCRE_INFO_STUDYSIZE           = 10
  integer(c_int), parameter :: PCRE_INFO_DEFAULT_TABLES      = 11
  integer(c_int), parameter :: PCRE_INFO_OKPARTIAL           = 12
  integer(c_int), parameter :: PCRE_INFO_JCHANGED            = 13
  integer(c_int), parameter :: PCRE_INFO_HASCRORLF           = 14
  integer(c_int), parameter :: PCRE_INFO_MINLENGTH           = 15
  integer(c_int), parameter :: PCRE_INFO_JIT                 = 16
  integer(c_int), parameter :: PCRE_INFO_JITSIZE             = 17
  integer(c_int), parameter :: PCRE_INFO_MAXLOOKBEHIND       = 18
  integer(c_int), parameter :: PCRE_INFO_FIRSTCHARACTER      = 19
  integer(c_int), parameter :: PCRE_INFO_FIRSTCHARACTERFLAGS = 20
  integer(c_int), parameter :: PCRE_INFO_REQUIREDCHAR        = 21
  integer(c_int), parameter :: PCRE_INFO_REQUIREDCHARFLAGS   = 22
  integer(c_int), parameter :: PCRE_INFO_MATCHLIMIT          = 23
  integer(c_int), parameter :: PCRE_INFO_RECURSIONLIMIT      = 24
  integer(c_int), parameter :: PCRE_INFO_MATCH_EMPTY         = 25

  ! Public options
  integer(c_int), parameter :: PCRE_CASELESS          = int(z'00000001')
  integer(c_int), parameter :: PCRE_MULTILINE         = int(z'00000002')
  integer(c_int), parameter :: PCRE_DOTALL            = int(z'00000004')
  integer(c_int), parameter :: PCRE_EXTENDED          = int(z'00000008')
  integer(c_int), parameter :: PCRE_ANCHORED          = int(z'00000010')
  integer(c_int), parameter :: PCRE_DOLLAR_ENDONLY    = int(z'00000020')
  integer(c_int), parameter :: PCRE_EXTRA             = int(z'00000040')
  integer(c_int), parameter :: PCRE_NOTBOL            = int(z'00000080')
  integer(c_int), parameter :: PCRE_NOTEOL            = int(z'00000100')
  integer(c_int), parameter :: PCRE_UNGREEDY          = int(z'00000200')
  integer(c_int), parameter :: PCRE_NOTEMPTY          = int(z'00000400')
  integer(c_int), parameter :: PCRE_UTF8              = int(z'00000800')
  integer(c_int), parameter :: PCRE_UTF16             = int(z'00000800')
  integer(c_int), parameter :: PCRE_UTF32             = int(z'00000800')
  integer(c_int), parameter :: PCRE_NO_AUTO_CAPTURE   = int(z'00001000')
  integer(c_int), parameter :: PCRE_NO_UTF8_CHECK     = int(z'00002000')
  integer(c_int), parameter :: PCRE_NO_UTF16_CHECK    = int(z'00002000')
  integer(c_int), parameter :: PCRE_NO_UTF32_CHECK    = int(z'00002000')
  integer(c_int), parameter :: PCRE_AUTO_CALLOUT      = int(z'00004000')
  integer(c_int), parameter :: PCRE_PARTIAL_SOFT      = int(z'00008000')
  integer(c_int), parameter :: PCRE_PARTIAL           = int(z'00008000')

  integer(c_int), parameter :: PCRE_NEVER_UTF         = int(z'00010000')
  integer(c_int), parameter :: PCRE_DFA_SHORTEST      = int(z'00010000')

  integer(c_int), parameter :: PCRE_NO_AUTO_POSSESS   = int(z'00020000')
  integer(c_int), parameter :: PCRE_DFA_RESTART       = int(z'00020000')

  integer(c_int), parameter :: PCRE_FIRSTLINE         = int(z'00040000')
  integer(c_int), parameter :: PCRE_DUPNAMES          = int(z'00080000')
  integer(c_int), parameter :: PCRE_NEWLINE_CR        = int(z'00100000')
  integer(c_int), parameter :: PCRE_NEWLINE_LF        = int(z'00200000')
  integer(c_int), parameter :: PCRE_NEWLINE_CRLF      = int(z'00300000')
  integer(c_int), parameter :: PCRE_NEWLINE_ANY       = int(z'00400000')
  integer(c_int), parameter :: PCRE_NEWLINE_ANYCRLF   = int(z'00500000')
  integer(c_int), parameter :: PCRE_BSR_ANYCRLF       = int(z'00800000')
  integer(c_int), parameter :: PCRE_BSR_UNICODE       = int(z'01000000')
  integer(c_int), parameter :: PCRE_JAVASCRIPT_COMPAT = int(z'02000000')
  integer(c_int), parameter :: PCRE_NO_START_OPTIMIZE = int(z'04000000')
  integer(c_int), parameter :: PCRE_NO_START_OPTIMISE = int(z'04000000')
  integer(c_int), parameter :: PCRE_PARTIAL_HARD      = int(z'08000000')
  integer(c_int), parameter :: PCRE_NOTEMPTY_ATSTART  = int(z'10000000')
  integer(c_int), parameter :: PCRE_UCP               = int(z'20000000')

  integer(c_int), parameter :: PCRE_CONFIG_UTF8                   =  0
  integer(c_int), parameter :: PCRE_CONFIG_NEWLINE                =  1
  integer(c_int), parameter :: PCRE_CONFIG_LINK_SIZE              =  2
  integer(c_int), parameter :: PCRE_CONFIG_POSIX_MALLOC_THRESHOLD =  3
  integer(c_int), parameter :: PCRE_CONFIG_MATCH_LIMIT            =  4
  integer(c_int), parameter :: PCRE_CONFIG_STACKRECURSE           =  5
  integer(c_int), parameter :: PCRE_CONFIG_UNICODE_PROPERTIES     =  6
  integer(c_int), parameter :: PCRE_CONFIG_MATCH_LIMIT_RECURSION  =  7
  integer(c_int), parameter :: PCRE_CONFIG_BSR                    =  8
  integer(c_int), parameter :: PCRE_CONFIG_JIT                    =  9
  integer(c_int), parameter :: PCRE_CONFIG_UTF16                  = 10
  integer(c_int), parameter :: PCRE_CONFIG_JITTARGET              = 11
  integer(c_int), parameter :: PCRE_CONFIG_UTF32                  = 12
  integer(c_int), parameter :: PCRE_CONFIG_PARENS_LIMIT           = 13

end module pcre_constants
