program recursive_elemental_prog

   implicit none

! INTERFACES
!   abstract interface
!   end abstract interface

!   interface
!   end interface

! TYPES
!   type TYPEA
!      TYPE1, private :: TYPE1VAR
!   contains
!      procedure(PINT), deferred, pass(this) :: procedure1
!   end type TYPEA

! VARIABLES
!   TYPE1 :: VARTYPE1

contains

   recursive function fib(n) result(res)
      integer, intent(in) :: n
      integer :: res


   end function fib
! PROCEDURES

!pure elemental function F1(A1) result (R1)
!   TYPEA1, optional, intent(in) :: A1
!   TYPER1 :: R1
!end function F1

end program recursive_elemental_prog

