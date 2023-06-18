module sortedlist_mod

   use list_mod, only: List

   implicit none

   type, abstract, extends(List) :: SortedList
   contains 
      procedure, private, deferred, pass(this) :: sort
      procedure, public, deferred, pass(this) :: add
      procedure, public, deferred, pass(this) :: remove
   end type :: SortedList

end module sortedlist_mod
