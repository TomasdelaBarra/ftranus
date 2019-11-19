! ************************************************************************
! * TRANUS - Integrated Land Use and Transport Model
! * 
! * $Id$
! * 
! * Copyright (C) 1983-2007 Modelistica, Caracas
! * Copyright (C) 1983-2007 Tomas de la Barra
! * Copyright (C) 1985-2007 Juancarlo Añez
! * Copyright (C) 1983-2002 Beatriz Perez
! * Some rights reserved.
! * 
! * (cc) This work is distrubuted under a Creative Commons
! *      Attribution-ShareAlike 2.0 license
! *      http://creativecommons.org/licenses/by-sa/2.0/
! ************************************************************************
MODULE HEAP
USE MENSAMOD
PUBLIC
    character(80) :: HEAP_RCS_ID = & 
      "$Id$" 

                
      type HeapRec
        integer :: index
        real(8) :: cost
      end type
      
      integer ::  heapCount
      integer ::  MXHEAP = 0
      integer, allocatable :: iHeap(:)
      type (HeapRec), allocatable :: HeapR(:)
      
      private :: heapCount, iHeap, HeapRec, HeapR

CONTAINS
       subroutine INITHEAP(imax, rmax)
         integer, intent(in) :: imax, rmax

         if (allocated(iHeap)) deallocate(iHeap)
         if (allocated(HeapR)) deallocate(HeapR)

         allocate(iHeap(rmax))
         allocate(HeapR(rmax))
         
         MXHEAP = rmax

         heapCount = 0
         HeapR(:)= HeapRec(0,0.0)
       end subroutine

       integer FUNCTION NHEAP()
         NHEAP=heapCount
       end function 

!      !======================================
      subroutine pheap(cost, index)
       real(8) ::  cost
       integer index

         real(8) ::  icc
         INTEGER ii, jj, indexa, ihj, ihja

        ! index puede ser negativo
        ! precondicion, cost .le. ICHEAP(abs(index))
        ! garantiza: orden parcial en el heap

        indexa = abs(index)
        icc = cost
        !*65536.0
        ii  = HeapR(indexa)%index     ! posicion correspondiente en el heap
        if (ii.eq.0) then
           ! el valor no estaba en el heap
           heapCount=heapCount+1
           if(heapCount > MXHEAP)then
             WRITE(*,*)
             CALL MENSA(3014,-1) ! P07: Excedida cap m x del programa
           endif
           ii  = heapCount ! nuevo, le toca la ultima posicion
        elseif (HeapR(indexa)%cost/icc < 0.999991) then
          call mensa(3060,-1)
        end if
        if(heapCount > ihMax) ihMax = heapCount

        HeapR(indexa)%cost =icc      ! colocar el nuevo costo
        do while (ii > 1)
          jj=ii/2
          ihj  = iheap(jj)
          ihja = abs(ihj)
          if (HeapR(ihja)%cost.le.icc) EXIT  ! fin
          iheap(ii)=ihj
          HeapR(ihja)%index =ii
          ii=jj
        end do
        iheap(ii)=index
        HeapR(indexa)%index=ii    ! el elemento 'index' quedo en la posicion ii
                          ! del heap
      end subroutine


      subroutine HeapMax(imax)
      integer :: imax
        imax = ihMax
      end subroutine

      subroutine GHEAP(COST, INDEX)
       real(8) ::  cost
       integer index

        real(8) ::  icc, ic, ic1
        INTEGER ii, jj, jj1
        INTEGER inx, inxa, ihj,ihja, ihj1,ihja1,indexa

        if (heapCount.le.0) call mensa(3045,-1)
!      Toma el elemento de costo minimo que esta en la posicion 1
        index = iheap(1)
        indexa = abs(index)
        cost  =  HeapR(indexa)%cost
        !/65536.0
        HeapR(indexa)%index = 0


!      Reacomoda el heap
        inx  = iheap(heapCount)
        inxa = abs(inx)
        icc  = HeapR(inxa)%cost
        heapCount   = heapCount-1
        if (heapCount.le.0) return
        ii=1
        jj=2
        do while(jj.le.heapCount)
          ihj=iheap(jj)
          ihja=abs(ihj)
          ic = HeapR(ihja)%cost
          jj1 = jj+1
          if (jj1.le.heapCount) then
             ihj1= iheap(jj1)
             ihja1=abs(ihj1)
             ic1 = HeapR(ihja1)%cost
             if (ic1.lt.ic) then
                ic   = ic1
                jj   = jj1
                ihj  = ihj1
                ihja = ihja1
             endif
          end if
          if(icc.le.ic) exit

  ! Cambia elemento de la posicion jj a la ii
          iheap(ii)=ihj
          HeapR(ihja)%index = ii

          ii=jj
          jj = ii+ii
        end do
        iheap(ii)=inx
        HeapR(inxa)%index=ii

      end subroutine



      subroutine checkHeap
        integer :: i, ihi, ihj

        do i=2, heapCount
          ihi = abs(iheap(i))
          ihj = abs(iheap(i/2))
          if(HeapR(ihi)%cost.lt.HeapR(ihj)%cost) then
             call mensa(msg_HeapDamaged, mensa_Aborta)
          endif
        enddo
      end subroutine

END MODULE HEAP
