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
PROGRAM DIMEN
USE DEBUGM
USE GETOPTM
USE PARAM
USE GENER

    character(80) :: DIMEN_RCS_ID = & 
      "$Id$" 
    character*(32) :: f901 = '((I8),'' '',$)'


      CALL MENSA(-1,0)    ! Logo

!  Dimensiones localizaci¢n
      
     CALL MENSA(18080,0)

     WRITE(*,f901) MXZON
     CALL MENSA(18081,0)  
     WRITE(*,f901) MXSEC
     CALL MENSA(18082,0)
     WRITE(*,f901) MXSUST
     CALL MENSA(18083,0)
     WRITE(*,*)

!  Dimensiones de transporte

     WRITE(*,f901) MXMOD
     CALL MENSA(18084,0)
     WRITE(*,f901) MXPROP
     CALL MENSA(18085,0)
     WRITE(*,f901) MXOPER
     CALL MENSA(18086,0)
     WRITE(*,f901) MXRUT
     CALL MENSA(18087,0)
     WRITE(*,f901) MXADM
     CALL MENSA(18088,0)
     WRITE(*,f901) MXTIP
     CALL MENSA(18089,0)
      WRITE(*,*)


     WRITE(*,f901) MXNODES
     CALL MENSA(18103,0)
     WRITE(*,f901) NLMAX
     CALL MENSA(18090,0)
     WRITE(*,f901) MXLNRUT
     CALL MENSA(18091,0)
     WRITE(*,f901) MXGIR
     CALL MENSA(18092,0)
     WRITE(*,f901) MXCON
     CALL MENSA(18101,0)
     WRITE(*,f901) MXPAS
     CALL MENSA(18093,0)
     WRITE(*,f901) MXARC
     CALL MENSA(18094,0)
     WRITE(*,f901) MXRUTINL
     CALL MENSA(18095,0)
      WRITE(*,*)

!  OJO: FALTAN
!        MXNODTIP=20,          ! N£mero m ximo de tipos de nodo
!        MXNOD=1000,           ! N£mero m ximo de nodos en la red

!  Dimensiones evaluaci¢n

     WRITE(*,f901) MXRUN
     CALL MENSA(18100,0)
      WRITE(*,*)

      WRITE(*,*) '   '
 END PROGRAM DIMEN
