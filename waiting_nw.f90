! ************************************************************************
! * TRANUS - Integrated Land Use and Transport Model
! * 
! * $Id: waiting.f90 138 2010-05-26 23:45:10Z juancarlo.anez $
! * 
! * Copyright (C) 1983-2007 Modelistica, Caracas
! * Copyright (C) 1983-2007 Tomas de la Barra
! * Copyright (C) 1985-2007 Juancarlo A�ez
! * Copyright (C) 1983-2002 Beatriz Perez
! * Some rights reserved.
! * 
! * (cc) This work is distrubuted under a Creative Commons
! *      Attribution-ShareAlike 2.0 license
! *      http://creativecommons.org/licenses/by-sa/2.0/
! ************************************************************************
MODULE WAITING
USE PARAM
    character(80) :: WAITING_RCS_ID = & 
      "$Id: waiting.f90 138 2010-05-26 23:45:10Z juancarlo.anez $" 

CONTAINS

real function WaitingTime(Boarding, Seats, Waiting)
	real, intent(in) :: Boarding
	real, intent(in) :: Seats
	real, intent(in) :: Waiting

	real :: VC, S

	if (S == 0) then
		S = 0.0001
	end if
	VC = Boarding/S

    WaitingTime = Waiting * exp(waiting_slope_factor*(VC-1))
end function

END MODULE WAITING
    
