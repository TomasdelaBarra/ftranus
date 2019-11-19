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
MODULE OPTIONSM

      logical :: use_powit_trans = .false.
      logical :: use_powit_loc   = .false.
      logical :: use_powit = .false.

      real :: waiting_slope_factor = 8.0

    character(1) :: C_DIRSEP = ' '

CONTAINS


  character(1) function dirsep()
    logical :: isdir
    if (C_DIRSEP /= '/' .and. C_DIRSEP /= '\') then
        inquire(file='./.', exist = isdir)
        if (isdir) then
           C_DIRSEP = '/'
        else
            inquire(file='.\.', exist = isdir)
            if (isdir) then
                C_DIRSEP = '\'
            else
                STOP 'Cannot determine directory separator'
            end if
        end if
    end if
    dirsep = C_DIRSEP
    return
  end function

END MODULE OPTIONSM
