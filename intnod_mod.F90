module intnode_mod

   use node_mod, only: Node

   implicit none 

   public :: IntNode

   private 

   type, extends(Node) :: IntNode
      private
      integer :: value_
      type(IntNode), pointer :: next_
   contains
      procedure, public, pass(this) :: get_next => get_next_intnode
      procedure, public, pass(this) :: get_value => get_value_intnode
   end type IntNode

contains

   function get_next_intnode(this) result(ptr)
      type(IntNode), intent(in) :: this
      type(IntNode), pointer :: ptr

      ptr => this % next_ 

   end function get_next_intnode

   function get_value_intnode(this) this(val)
      type(IntNode), intent(in) :: this
      integer :: val

      val = this % value_

   end function get_value_intnode

end module intnode_mod
