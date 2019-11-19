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
!Integer singly-linked lists without centinel.

    integer, parameter :: NULL = 0

private
    type Node
      integer :: value
      integer :: next
    end type

    type(Node), allocatable :: nodes(:)
    integer :: free = NULL

contains
    subroutine list_initialize()
        if (allocated(nodes)) deallocate(nodes)
        free = NULL
    end subroutine

    subroutine allocate_more_nodes()
        integer :: first, size
        type(Node), allocatable :: tmp(:)
        if (free /= NULL) return

        if (.not.allocated(nodes)) then
            size = 1024
            allocate(nodes(size))
            first = 1
        else
            ! reallocate
            size = 2*ubound(nodes, 1)
            allocate(tmp(size))
            tmp(:) = nodes(:)
            allocate(nodes(2*size))
            nodes(1:size) = tmp(:)
            first = size + 1
            size = 2*size
        end if
        nodes(first)%next = NULL
        do i=first+1,size
            nodes(i)%next = first
            first=i
        end do
        free = size
    end subroutine
    
    logical function listempty(list)
        integer, intent(in) :: list

        !assert(list > 0)
        listempty = nodes(list)%next == NULL
    end function

    integer function get_free_node()
        integer :: n
        if (free == NULL) then
            call allocate_more_nodes()
        end if
        n =  free
        free = nodes(free)%next
        nodes(n)%next = NULL
        get_free_node = n
        return
    end function

    integer function listnew()
        listnew = get_free_node()
        return
    end function

    subroutine listinsert(list, value)
        integer, intent(in) :: list, value

        n = get_free_node()
        nodes(n)%value = value
        nodes(n)%next = nodes(list)%next
        nodes(list)%next = n
    end subroutine

    subroutine listfree(list)
        integer :: list
        integer :: last

        last = list
        do while(nodes(last)%next /= NULL)
            last = nodes(last)%next
        enddo
        nodes(last)%next = free
        free = list
        list = NULL
    end subroutine

end module
