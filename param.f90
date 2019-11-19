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
MODULE PARAM
USE RELEASE
    character(80) :: PARAM_RCS_ID = &
      "$Id$" 

      ! Version actual de los archivos
      integer(2), parameter ::     &
         FileMajor   =  6,   &
         FileMinor   =  8,   &
         FileRelease =  1

      real(4), parameter ::    &
         CEROMAS=1.0E-9,      & ! Real chiquito positivo
         CEROMEN=-1.0E-9,     & ! Real chiquito negativo
         ICERUN=0,            & ! Cero 1byte
         RCERO=0.,ICERO=0,    & ! Cero real y entero
         RINF=9E9,INF=9999,   &
         EPS = 1.E-6            ! Infinito real y entero

      character(1), parameter :: TAB = CHAR(9)

      real, parameter :: VEL0=0.1

      integer, parameter ::   &
         IUNO=1,UNOUNO=1      ! Uno de 2bytes y de 1byte

     ! Constantes para tipos de archivos
      integer, parameter :: &
         IO_FMT= 1,         &
         IO_UNF= 0,         &
         IO_BIN= 2

      ! Formatos de archivos reconocidos por GUS
      integer, parameter :: &
        ifmt_P0S= 1,        &
        ifmt_PxS= 2,        &
        ifmt_L1S= 3
     
      ! Constantes de  Tipo de Escenario previo
      integer, parameter :: &
         pol_Any       = 0, &
         pol_SameYear  = 1, &
         pol_OtherYear = 2

      integer, parameter ::   &
         MXNODES=16*1024,     & ! Maximum number of nodes
         MXNODTIP=8,          & ! Maximum number of node types
         MXADM=16,            & ! Maximum nuber of transport administrators
         MXARC=4096,          & ! Maximum number of links in a path
         MXCON=32,            & ! Maximum number of connected links
         MXGIR=64,            & ! Maximum number of prohibited turns
         MXMOD=32,            & ! Maximum number of modes
         MXOPER=64,           & ! Maximum number of operators
         MXPAS=16,            & ! Maximum number of paths per mode
         MXPROP=64,           & ! Maximum number of transport categories
         MXRUN=6,             & ! Maximum number of scenarios
         MXRUT=8*1024,        & ! Maximum number of public transport routes
         MXRUTINL=64,         & ! Maximum number of operators/routes in a link
         MXSEC=64,            & ! Maximum numbder of economic sectors
         MXSUST=MXSEC*4,      & ! Maximum number of substitutes
         MXTIP=128,           & ! Maximum number of link types
         MXZON=1100,          & ! Maximum number of zones
         NLMAX=35*1024,       & ! Maximum number of links
         MXLNRUT=8*NLMAX,     & ! Maximum number of link-routes
         MXCOUNT=32             ! Maximum number of traffic counts

CONTAINS

subroutine param_init()
end subroutine

END MODULE PARAM
