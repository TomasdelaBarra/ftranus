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
MODULE IPCOMM
USE PARAM
USE RCOMM
USE TPARC
USE GENER
USE MENSAMOD
USE IO_LIST

    character(80) :: IPCOMM_RCS_ID = & 
      "$Id$" 

! Definicion de estructura enlace/ruta por categoria
      TYPE, PUBLIC :: TLNRIndic
      SEQUENCE
       real :: trips   ! suma de los viajes que transitan, hayan o no abordado en el enlace/ruta
       real :: board   ! suma de los que abordan
       real :: unboard ! suma de los que bajan
       real :: income  ! ingreso que la categoria aporta a la ruta en el enlace
      END TYPE TLNRIndic

      TYPE (TLNRIndic), PUBLIC :: LNRIndic(MXLNRUT,MXPROP)

      character*80 :: rcs_id = "$Id$"


CONTAINS

SUBROUTINE LimpiaLNRIndic 
! blanquea los arreglos
  integer :: lr, ip

  do lr = 1, MXLNRUT
     do ip =1, MXPROP
         LNRIndic(lr,ip) = TLNRIndic(0,0,0,0)
     enddo
  enddo
END SUBROUTINE

SUBROUTINE GrabaLNRIndic(iun) 
! Graba los indicadores en T5S
  integer :: iun

  integer :: lr, ip

  call WriteListBegin(iun, NLINKRUT)
  do lr=1, NLINKRUT
     call WriteListItem(iun, lr)
     call WriteListBegin(iun, NPROP)
     do ip=1, NPROP
         call WriteListItem(iun, ip)
         write(iun) LNRIndic(lr, ip)
     enddo
     call WriteListEnd(iun, NPROP)
  enddo
  call WriteListEnd(iun, NLINKRUT)
END SUBROUTINE

SUBROUTINE LeeLNRIndic(iun)  
! Lee los indicadores grabados en T5S
  integer :: iun

 integer :: lr, ip

  call CheckListBegin(iun, NLINKRUT)
  do lr=1, NLINKRUT
     call CheckListItem(iun, lr)
     call CheckListBegin(iun, NPROP)
     do ip=1, NPROP
        call CheckListItem(iun, ip)
        read(iun) LNRIndic(lr, ip)
     enddo
     call CheckListEnd(iun,   NPROP)
  enddo
  call CheckListEnd(iun, NLINKRUT)

END SUBROUTINE

SUBROUTINE ImprimeLNRIndic(iun)  
! Imprime la lista de indicadores por enlace/ruta y categoria en archivo ASCII
  integer :: iun

  integer   :: lr, ip
  integer   ::  l, ir, io
  character ::  TAB
  TAB = CHAR(9)


  write(iun, 101) 'LinkId', TAB, 'Orig', TAB, 'Dest', TAB, &
       'Tip', TAB, 'Dist', TAB, &
       'RutId',  TAB, 'Rut', TAB,  &
       'OperId', TAB, 'Oper', TAB, &
       'CatId',  TAB, 'Cat', TAB,  & 
       'Unit-Dist', TAB, 'Board', TAB, 'Unboard', TAB, 'Income'
  do lr = 1, NLINKRUT
     l  = ILink(lr)
     ir = IRoute(lr)
     io = IOpRut(ir)
     do ip =1, NPROP
         if (LNRIndic(lr,ip)%trips /= 0) then
             write(iun,100) LinkId(l), TAB, IOR(L), TAB, IDES(l), TAB, &
               NumTip(ITIP(l)), TAB, DIS(l), TAB, &
               NumRut(ir), TAB, TRIM(NomRut(ir)), TAB,    &
               NumOp(io), TAB,  TRIM(NomOp(io)), TAB,      & 
               NumCat(ip), TAB, TRIM(NomCat(ip)), TAB,    &
               LNRIndic(lr,ip)%trips*DIS(l),   TAB, & 
               LNRIndic(lr,ip)%board,   TAB,        & 
               LNRIndic(lr,ip)%unboard, TAB,        &
               LNRIndic(lr,ip)%income
        endif
     enddo
  enddo
100 FORMAT(1X, 4(I6,A), (F10.4,A), 3(I6,3A), F16.4, A, 2(F16.4, A), F16.4)
101 FORMAT(1X, 40(A))
END SUBROUTINE


END MODULE IPCOMM
