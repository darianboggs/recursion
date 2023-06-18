module intnode_mod

   use node_mod, only: Node

   implicit none 

   public :: IntNode

   private 

   type, extends(Node) :: IntNode
      private
      integer :: value_
      type(IntNode), pointer :: next_
      type(IntNode), pointer :: previous_
   contains
      procedure, public, pass(this) :: get_next => get_next_intnode
      procedure, public, pass(this) :: get_previous => get_previous_intnode
      procedure, public, pass(this) :: get_value => get_value_intnode
      procedure, public, pass(this) :: set => set_intnode
      procedure, public, pass(this) :: is_head => is_head_intnode
      procedure, public, pass(this) :: is_tail => is_tail_intnode
      procedure, public, pass(this) :: clone => clone_intnode
   end type IntNode

   interface IntNode
      module procedure :: construct_intnode
   end interface IntNode

contains

   function construct_intnode(val) result(new_node, next, previous)
      integer, intent(in) :: val
      type(IntNode), pointer, optional, intent(in) :: next
      type(IntNode), pointer, optional, intent(in) :: previous
      type(IntNode) :: new_node
      
      new_node % value_ = val
      nullify(this % next_)
      nullify(this % previous_)
      if(present(next)) this % next_ => next
      if(present(previous)) this % previous_ => previous % next

   end function construct_intnode

   function get_next_intnode(this) result(ptr)
      type(IntNode), intent(in) :: this
      type(IntNode), pointer :: ptr

      ptr => this % next_ 

   end function get_next_intnode

   function get_previous_intnode(this) result(ptr)
      type(IntNode), intent(in) :: this
      type(IntNode), pointer :: ptr

      ptr => this % previous_ 

   end function get_previous_intnode

   function get_value_intnode(this) this(val)
      type(IntNode), intent(in) :: this
      integer :: val

      val = this % value_

   end function get_value_intnode

   subroutine set_intnode(this, val, next, previous) result(is_successful)
      type(IntNode), intent(inout) :: this
      integer, optional, intent(in) :: val
      type(IntNode), pointer, optional, intent(in) :: next
      type(IntNode), pointer, optional, intent(in) :: previous
      logical :: is_successful

      is_successful = .FALSE.
      if(present(val)) this % value_ = val
      if(present(next) this % next_ => next
      if(present(previous) this % previous_ => previous
      is_successful = .TRUE.

   end function set_intnode

   function is_head_intnode(this) result(lvalue)
      type(IntNode), intent(in) :: this
      logical :: lvalue

      lvalue = allocated(this % head_)

   end function is_head_intnode

   function is_tail_intnode(this) result(lvalue)
      type(IntNode), intent(in) :: this
      logical :: lvalue

      lvalue = allocated(this % tail_)

   end function is_tail_intnode

   function clone_intnode(this, next, previous) result(clone)
      type(IntNode), intent(in) :: this
      type(IntNode), pointer, optional, intent(in) :: next
      type(IntNode), pointer, optional, intent(in) :: previous
      type(IntNode) :: clone

      clone = IntNode(this % get_value(), next, previous)

   end function clone_intnode

end module intnode_mod
