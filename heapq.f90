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

module heapq
use debugm
public
! Heap algorithm modeled by Python's implementation.

    type heap_item
        real(8) :: weight
        integer :: info
    end type

    type(heap_item), allocatable :: heap(:)
    integer :: heaplen = 0
    integer :: maxlen = 0

contains
    integer function heapmax()
        heapmax = ubound(heap,1)
    end function

    logical function heapempty()
        heapempty = heaplen == 0
    end function

    subroutine heapalloc(size)
        integer :: size
        if (allocated(heap)) then
            if (ubound(heap,1) < size) then
                deallocate(heap)
                allocate(heap(size))
            end if
        else
            allocate(heap(size))
        end if
        call heapinit
    end subroutine

    subroutine heapinit
        heaplen = 0
    end subroutine

    subroutine pushdown(j)
        type(heap_item) :: item
        integer :: i
        integer :: j

        item = heap(j)
        do while (j > 1)
            i = j / 2
            if (heap(i)%weight > item%weight) then
                heap(j) = heap(i)
                j = i
            else
                exit
            end if
        end do
        heap(j) = item
        if (debugging >= dbg_debug) then
            call heapcheck('pushdown')
        end if
    end subroutine

    subroutine percolate(pos)
        integer, intent(in) :: pos
        integer :: i, j
        type(heap_item) :: last

        last = heap(pos)
        i = pos
        j = 2*i
        do while (j <= heaplen)
            if (j+1 <= heaplen .and. heap(j+1)%weight < heap(j)%weight) then
               j = j+1
            end if
            if (heap(j)%weight < last%weight) then
                heap(i) = heap(j)
                i = j
                j = 2*i
            else
                exit
            endif
        enddo
        heap(i) = last
    end subroutine

    subroutine heappush(weight, info)
        real(8), intent(in)  :: weight
        integer :: info
        type (heap_item):: item
        integer :: i
        integer :: j

        item%weight = weight
        item%info = info

        heaplen = heaplen + 1
        if (heaplen > ubound(heap,1)) call fail('Heap full')
        maxlen = max(maxlen, heaplen)
        j = heaplen
        heap(j) = item
        call pushdown(j)
    end subroutine

    subroutine heappop(weight, info)
        real(8), intent(out)  :: weight
        integer :: info
        integer :: i, j

        if (heaplen < 1) call fail('HEAP Empty')
        weight = heap(1)%weight
        info = heap(1)%info

        heap(1) = heap(heaplen)
        heaplen = heaplen - 1

        call percolate(1)

        if (debugging >= dbg_debug) then
           call heapcheck('heappop')
        end if
    end subroutine

    subroutine heapify
       integer :: i

       do i = 1, heaplen / 2
           call percolate(i)
       enddo
    end subroutine

   subroutine heapcheck(msg)
       character(*), optional :: msg
       integer :: i, j

       do j=2, heaplen
           i = j / 2
           if (heap(i)%weight > heap(j)%weight) then
               print *
               if (present(msg)) print *, msg
               print *, i, j,heaplen
               call fail('Heap damaged')
           endif
       enddo
   end subroutine

end module heapq
