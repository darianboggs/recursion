module list_mod

   use node_mod, only: Node

   implicit none

   type, abstract :: List
      private
      integer :: size_
   contains
      procedure, public, pass(this) :: get_size
      procedure, public, pass(this) :: is_empty
      procedure, public, deferred, pass(this) :: get_head
      procedure, public, deferred, pass(this) :: get_tail
      procedure, public, deferred, pass(this) :: push
      procedure, public, deferred, pass(this) :: pop
      procedure, public, deferred, pass(this) :: append
      procedure, public, deferred, pass(this) :: shift
      procedure, public, deferred, pass(this) :: get_node
   end type List

contains

   function get_size(this) result(sz)
      type(List), intent(in) :: this
      integer :: sz
      
      sz = this % size_

   end function get_size
   
   function is_empty(this)
      type(List), intent(in) :: this
      logical :: is_empty

      is_empty = .not. associated(this)

   end function is_empty
      
end module list_mod
