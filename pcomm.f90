! ************************************************************************
! * TRANUS - Integrated Land Use and Transport Model
! * 
! * $Id$
! * 
! * Copyright (C) 1983-2011 Modelistica, Caracas
! * Copyright (C) 1983-2007 Tomas de la Barra
! * Copyright (C) 1985-2011 Juancarlo A�ez
! * Copyright (C) 1983-2002 Beatriz Perez
! * Some rights reserved.
! * 
! * (cc) This work is distrubuted under a Creative Commons
! *      Attribution-ShareAlike 2.0 license
! *      http://creativecommons.org/licenses/by-sa/2.0/
! ************************************************************************
MODULE PCOMM
USE PARAM
    character(80) :: PCOMM_RCS_ID = & 
      "$Id$" 

      integer, parameter ::   &
         ADYA_RATIO = 24 ! Adyancencies per linkroute

        integer(2) ::  nps
        integer(2) ::  ncol(MXPAS)
        integer    matpas(MXPAS,MXARC)
        integer    ipord(MXPAS)
        real(8) ::     cospas(MXPAS)
        real(4) ::     path_delay(MXPAS)   ! Delay en intersecciones acumulado

        TYPE GraphNode
          integer(4) :: linkrut
          real(8) ::  cost
          real(4) ::  delay
        END TYPE
        TYPE (GraphNode), allocatable :: graph(:) 
        integer :: MXGNODES = 0 ! Max connected linkroutes
        integer :: numnodes = 0  ! Number of connections


        TYPE TLnrRec
        SEQUENCE
           integer :: connected
           integer :: iover
           real(8) :: cosOvr
           real(8) :: cosTot
        END TYPE
        TYPE (TLnrRec), allocatable :: lnrData(:)

        integer ::    &
         MATZ(MXZON)       ! Primer enlace-ruta conectada en graph()
                           ! por zona

       integer, allocatable :: &
        iozon(:),   & ! Zona origen de un enlace o CERO
        idzon(:)     ! Zona destino de un enlace o CERO

        real, allocatable    :: gcmin(:,:)
        integer, allocatable :: gpmin(:,:)

CONTAINS

    subroutine InitSearchStructures(zmax, lmax, lnrmax)
        integer, intent(in) :: zmax, lmax, lnrmax

        allocate(iozon(lmax))
        allocate(idzon(lmax))
        allocate(lnrData(lnrmax))
        MXGNODES = ADYA_RATIO*lnrmax
        allocate(graph(MXGNODES))

        allocate(gpmin(zmax, lnrmax))
        allocate(gcmin(zmax, lnrmax))
    end subroutine

END MODULE PCOMM
