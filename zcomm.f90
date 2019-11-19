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
MODULE ZCOMM
USE PARAM
USE GENER
    character(80) :: ZCOMM_RCS_ID = & 
      "$Id$" 

!  ZCOMM4.INC - COMMON PARA EL MANEJO DE ZONAS - TRANUS 4.0
      integer :: NUMZON(MXZON)
      character(32) :: NOMZON(MXZON)
      integer :: JER1(MXZON),JER2(MXZON)
      INTEGER NZ1,NZ2,NZN

!  JER*    Expresa la jerarqu¡a de zonas en numeraci¢n interna (zona)
!  NUMZON  N£mero externo de las zonas (zonas)
!  NOMZON  Nombre de las zonas (zonas)
!  NZ1     Ultima zona del primer nivel
!  NZ2     Ultima zona del segundo nivel
!  NZN     N£mero total de zonas = £ltima zona externa

      logical(1) ::  zexter(mxzon) ! Es una zona externa ?
      logical(1) ::  conSubZonas(mxzon) ! Se desagrega en microzonas?
      logical(1) ::  esSubZona(mxzon) ! Pertenece a una macro zona ?

      ! si no hay niveles jerarquicos conSubZonas y esSubZona son siempre FALSE
      ! una zona i es de primer nivel si esSubZona(i)=FALSE
      ! una zona es agregada si conSubZonas(i)=TRUE

CONTAINS

      SUBROUTINE LEEZ1E
!     ===============
         CHARACTER(32) :: NOM
         INTEGER i
         integer :: IOS, N, I1, I2

!  LEE LOS DATOS DE ZONAS DEL ARCHIVO Z1E

!  REQUERIMIENTOS DE LA LIBRERIA TRANUS:
!    CHECK   (Subr)  Verifica los finales de secci¢n

! INICIALIZACION

      NUMZON=ICERO
      NOMZON='        '
      DO I=1,MXZON
         JER1(I)=I
         JER2(I)=I
      ENDDO
      zexter = .FALSE.
      ConSubZonas = .FALSE.
      EsSubZona = .FALSE.

! SECCION 1.0 - PRIMER NIVEL JERARQUICO

      READ(3,'(////)',END=999)
      NZ1=ICERO
3     READ(3,*,END=999,ERR=100,IOSTAT=IOS)N,NOM
      NZ1=NZ1+1
      IF (NZ1.GT.MXZON) THEN
         WRITE(*,*)'Z1E(1.0) - Zon:',NZ1,'   Max:',MXZON
         CALL MENSA(9,-1) ! EG9: Excedida dimensi¢n m xima'
      ENDIF
      NUMZON(NZ1)=N
      NOMZON(NZ1)=NOM
      GO TO 3
100   CALL CHECK(1.0,IOS,'Z1E')

! SECCION 2.0 - SEGUNDO NIVEL JERARQUICO, SI EXISTE

      READ(3,*,ERR=999,END=999)
      READ(3,*,ERR=999,END=999)
      NZ2=NZ1
4     READ(3,*,END=999,ERR=200,IOSTAT=IOS) &
           I1,(NUMZON(I2),NOMZON(I2),I2=NZ2+1,MXZON)
      I2=INTNUM(I1,NUMZON,NZ1)   ! busca el No interno de la zona I1
      IF(I2.GT.NZ1.OR.(JER1(I2).NE.JER2(I2)))THEN
         WRITE(*,*)'Z1E(2.0) - Zon:',I1
         CALL MENSA(6,-1)  ! Definici¢n ilegal
      ENDIF
      JER1(I2)=NZ2+1
!  Recorre las subzonas
10    IF(NUMZON(NZ2+1).EQ.ICERO)GO TO 4 
      NZ2=NZ2+1
      IF (NZ2.GT.MXZON) THEN
         WRITE(*,*)'Z1E(2.0) - Zon:',NZ2,'   Max:',MXZON
         CALL MENSA(9,-1)  !  G05: Excedida dimensi¢n m xima
      ENDIF
      JER2(I2)=NZ2
      JER1(NZ2)=I2
      JER2(NZ2)=I2
      GO TO 10
200   CALL CHECK(2.0,IOS,'Z1E')

! SECCION 3 - ZONAS EXTERNAS, SI LAS HAY

      NZN=NZ2
      READ(3,*,ERR=999,END=999)
      READ(3,*,ERR=999,END=999)
30    READ(3,*,END=999,ERR=300,IOSTAT=IOS)N,NOM
      NZN=NZN+1
      IF (NZN.GT.MXZON) THEN
         WRITE(*,*)'Z1E(3.0) - Zon:',NZN,'   Max:',MXZON
         CALL MENSA(9,-1)  ! EG05: Excedida la dimensi¢n m xima
      ENDIF
      NUMZON(NZN)=N
      NOMZON(NZN)=NOM
      GO TO 30
300   CALL CHECK(3.0,IOS,'Z1E')

      call ZonSubZon()

      RETURN
999   CALL MENSA(10,-1)  ! EG10: Archivo incompleto
      END SUBROUTINE

      SUBROUTINE ZonSubZon()
       INTEGER i
         do i=1, nzn
           if (i.gt.nz1) then
             if (i.le.nz2) then
               EsSubZona(i)=.TRUE.
             else
               zexter(i)=.TRUE.
             endif
           endif
           ConSubZonas(i) = (i.le.nz1).and.(jer1(i).ne.jer2(i))
         enddo
      RETURN
      END SUBROUTINE


END MODULE ZCOMM
