! ************************************************************************
! * TRANUS - Integrated Land Use and Transport Model
! * 
! * $Id: Release.template.f90 4 2007-06-12 16:52:12Z juancarlo.anez $
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
!  NOTE!!!:
!  Automatically generated release information.
!  Don not modify.
!
MODULE Release

   integer(2), parameter ::              &
     vermajor = 12
   integer(2), parameter ::              &
     verminor = 11
   integer(2), parameter ::              &
     revision = 1
   integer(2), parameter ::              &
     revminor = 0

   character(*), parameter :: &
     ReleaseStr = '12.11.1'
   character(*), parameter :: &
     ReleaseDateStr = '2012-11-14'
   integer, parameter :: &
     ReleaseYear = 2012
   character(*), parameter :: &
     ReleaseName = ''

END MODULE Release
