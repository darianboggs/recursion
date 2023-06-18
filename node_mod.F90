module node_mod

   implicit none 

   public :: Node

   private

   type, abstract :: Node
   contains
      procedure, public, deferred, pass(this) :: get_next
      procedure, public, deferred, pass(this) :: get_previous
      procedure, public, deferred, pass(this) :: get_value
      procedure, public, deferred, pass(this) :: is_head
      procedure, public, deferred, pass(this) :: is_tail
      procedure, public, deferred, pass(this) :: set
      procedure, public, deferred, pass(this) :: clone
   end type Node

end module node_mod
