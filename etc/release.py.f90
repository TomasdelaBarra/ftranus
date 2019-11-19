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

   integer(2), parameter :: vermajor = {vermajor}
   integer(2), parameter :: verminor = {verminor}
   integer(2), parameter :: revision = {revision}
   integer(2), parameter :: revminor = 0

   character(*), parameter :: ReleaseStr = '{version}'
   character(*), parameter :: ReleaseDateStr = '{datetag}'
   integer, parameter      :: ReleaseYear = {year}
   character(*), parameter :: ReleaseName = '{nametag}'

END MODULE Release
