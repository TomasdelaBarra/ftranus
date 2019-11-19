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
MODULE MECOMM
USE PARAM

    character(80) :: MECOMM_RCS_ID = & 
      "$Id$" 

      integer, parameter :: NIMP=9
      LOGICAL   LOPER,LCAT,IMP,ESCOGE,LMODE
      LOGICAL   NE
      INTEGER AUXOPER,AUXPROP,IopImpre
      REAL MAT,DTC
      real(8) :: MATAG,CC,FILAS
      CHARACTER(32) NOMAG
      CHARACTER*128 :: SALIDA
      
      integer :: ITR
      dimension :: LOPER(MXOPER),LCAT(MXPROP),imp(NIMP),ESCOGE(NLMAX),     &
        MAT(MXZON,MXZON),MATAG(MXZON,MXZON),IAG(MXZON),LMODE(MXMOD),    &
        VIAJOP(MXZON,MXZON),CC(MXZON),FILAS(MXZON),NUMAG(MXZON),        &
        NOMAG(MXZON),AUXOPER(MXOPER),AUXPROP(MXPROP),DTC(MXPAS)

      real(8) :: OperOper(0:MXOPER, MXOPER) ! matriz de transferencias entre operadores

      !! reporting options
      integer, parameter :: opt_ReportNodeFlows    = 8
      integer, parameter :: opt_ReportNodeVehicles = 9
      logical :: ReportNodeFlows     = .false.
      logical :: ReportNodeVehicles  = .false.
      logical :: UseMatsDat          = .false.
      logical :: HaveFileName        = .false.

    type TCatOpSummary
        real :: vehicles
        real :: volume
        real :: vehdist
        real :: voldist
        real :: voltime
        real :: volcost
    end type

    type(TCatOpSummary) :: CatOpSummary(MXPROP, MXOPER)

END MODULE MECOMM
