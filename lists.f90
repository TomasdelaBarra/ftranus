! ************************************************************************
! * TRANUS - Integrated Land Use and Transport Model
! *
! * Copyright (C) 1983-2011 Modelistica, Caracas
! * Copyright (C) 1985-2011 Juancarlo Añez
! * Copyright (C) 1983-2002 Beatriz Perez
! * Some rights reserved.
! *
! * (cc) This work is distrubuted under a Creative Commons
! *      Attribution-ShareAlike 2.0 license
! *      http://creativecommons.org/licenses/by-sa/2.0/
! ************************************************************************
module lists
!Singly-linked lists without centinel.
use debugm
public

    type ListCell
      integer :: refs
      integer :: value
      type(ListCell), pointer :: next
      type(ListCell), pointer, private :: chain
    end type

    type(ListCell), pointer, private :: free_list => NULL()
    type(ListCell), pointer, private :: recovery_chain => NULL()
    logical, private :: first_time = .true.


contains
    subroutine list_initialize()
        implicit none
        type(ListCell), pointer :: p

        call verbose('initializing lists')

        if (first_time) then
            first_time = .false.
            nullify(free_list)
            nullify(recovery_chain)
            recovery_chain => null()
        end if

        call verbose('recovering cells')
        free_list => null()
        p => recovery_chain
        do while(associated(p))
            p%next => free_list
            free_list => p
            p => p%chain
        enddo

        call verbose('initializing ends')
    end subroutine

    function listnew(value)
        implicit none
        type(ListCell), pointer :: listnew
        integer, intent(in), optional :: value
        type(ListCell), pointer :: p

        nullify(p)
        if (present(value)) then
            p => listcons(value, p)
            call assert(associated(p), 'listcons returns null')
        endif
        listnew => p
    end function
    
    function list_is_empty(list)
        implicit none
        logical :: list_is_empty
        type(ListCell), pointer, intent(in) :: list

        list_is_empty = .not. associated(list)
    end function
    
    function listlen(list)
        implicit none
        integer :: listlen
        type(ListCell), pointer, intent(in) :: list
        integer :: length
        type(ListCell), pointer :: p

        length = 0
        p => list
        do while(associated(p))
            length = length + 1
            p => p%next
        end do
        listlen = length
    end function

    function allocate_cell()
        implicit none
        type(ListCell), pointer :: allocate_cell
        type(ListCell), pointer :: p

        if (first_time.or..not.associated(free_list)) then
            if(first_time) then
                call list_initialize()
            endif
            allocate(p)
            p%chain => recovery_chain
            recovery_chain => p
        else
            p => free_list
            free_list => p%next
        end if
        p%refs = 0
        p%value = 0
        nullify(p%next)
        allocate_cell => p
    end function

    function listcons(value, list)
        implicit none
        type(ListCell), pointer :: listcons
        integer, intent(in) :: value
        type(ListCell), pointer, intent(in) :: list
        type(ListCell), pointer :: p

        p => allocate_cell()
        call assert(associated(p))
        p%refs = 1
        p%value = value
        p%next  => list
        listcons => p
        if(associated(list)) then
            list%refs = list%refs+1
        end if
    end function

    integer function listhead(list)
        type(ListCell), pointer, intent(in) :: list
        call assert(associated(list))

        listhead = list%value
    end function

    function listtail(list)
        implicit none
        type(ListCell), pointer :: listtail
        type(ListCell), pointer, intent(in) :: list
        call assert(associated(list))

        listtail => list%next
    end function

    subroutine listfree(list)
        implicit none
        type(ListCell), pointer :: list
        type(ListCell), pointer :: last

        if (.not. associated(list)) return

        list%refs = list%refs-1
        if (list%refs > 0) return

        last => list
        do while(associated(last%next))
            last%next%refs = last%next%refs-1
            if (last%next%refs == 0) then
                last => last%next
            else
               exit 
            endif
        enddo
        last%next => free_list
        free_list => list
        nullify(list)
    end subroutine

end module
