module intlist_mod

   use intnode_mod, only: IntNode
   use node_mod, only: Node
   use sortedlist_mod, only: SortedList

   implicit none 

   type, extends(SortedList) :: IntList
      private
      type(IntNode), pointer :: head_
   contains
      procedure, public, pass(this) :: get_head => get_head_intlist
      procedure, public, pass(this) :: get_tail => get_tail_intlist
      procedure, public, pass(this) :: push => push_intnode
      procedure, public, pass(this) :: pop => pop_intnode
      procedure, public, pass(this) :: append => append_intnode
      procedure, public, pass(this) :: shift => shift_intnode
      procedure, private, pass(this) :: sort => sort_intlist
      procedure, public, pass(this) :: add => add_intnode
      procedure, public, pass(this) :: remove => remove_intnode
   end type List

   interface IntList
      module procedure :: construct_IntList_empty
      module procedure :: construct_IntList_ptr
      module procedure :: construct_IntList_obj
   end interface IntList

contains

   function construct_IntList_empty() result(il)
      type(IntList) :: il

      nullify(il % head_)
      il % sz = 0

   end function construct_IntList_empty

   function construct_IntList_ptr(head) result(il)
      type(IntNode), pointer, intent(in) :: head
      type(IntNode) :: il
     
      if(associated(head)) then
         il % head_ => head
         il % sz_ = 1
         return
      end if

      il = IntList()

   end function construct_IntList_ptr

   function construct_IntList_obj(head) result(il)
      type(IntNode), target, intent(in) :: head
      type(IntNode) :: il
      type(IntNode), pointer :: ptr

      ptr => head
      il = IntList(ptr)      

   end function construct_IntList_obj

   function get_head_intlist(this) result(head)
      type(IntList), intent(in) :: this
      type(IntNode), pointer :: head
      
      head => this % head_

   end function get_head_intlist

   function get_tail_intlist(this) result(tail)
      type(IntList), intent(in) :: this
      type(IntNode), pointer :: tail
      
      tail => this % tail_

   end function get_tail_intlist

   function push_intnode(this, new_node) result(is_successful)
      type(IntList), target, intent(in) :: this
      type(IntNode), pointer, intent(in) :: new_node
      type(IntNode), pointer :: ptr
      logical :: lvalue

      is_successful = .FALSE.
      ptr => this
      if(.not. new_node % set(new_node, previous = ptr)) return

      if(associated(this % head_)) then
         ptr => this
      else
         this % head_ => new_node
      end if

      this % sz = this % get_size() + 1

      is_successful = .TRUE.
   end function push_intnode

   ! return the last value <= the value of this
   ! if the list is empty, return null
   function find_position_val(this, val) result(ptr)
      type(IntList), intent(in) :: this
      integer, intent(in) :: val
      type(IntNode), pointer :: ptr

      ! initialize
      nullify(ptr)

      ! if
      if(this % is_empty()) return
      
      ptr => this % get_head()
      do while (ptr % get_value() <= val)
         if(ptr % is_tail()) exit
         ptr => ptr % get_next()
         
      if(this % get_size() == 0) then
         ptr 
   end function find_position_val
end module intlist_mod
