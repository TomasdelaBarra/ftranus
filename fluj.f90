! ************************************************************************
! * TRANUS - Integrated Land Use and Transport Model
! * 
! * $Id$
! * 
! * Copyright (C) 1983-2010 Modelistica, Caracas
! * Copyright (C) 1983-2010 Tomas de la Barra
! * Copyright (C) 1985-2010 Juancarlo A~nez
! * Copyright (C) 1983-2010 Beatriz Perez
! * Some rights reserved.
! * 
! * (cc) This work is distrubuted under a Creative Commons
! *      Attribution-ShareAlike 2.0 license
! *      http://creativecommons.org/licenses/by-sa/2.0/
! ************************************************************************
PROGRAM FLUJ
USE DEBUGM
USE GETOPTM
USE PARAM
USE MENSAMOD
USE GENER
USE CONTROL
USE IO_LIST
USE TPARC
USE FCOMM
USE ZCOMM
USE ASCII
USE NODES


character(80) :: FLUJ_RCS_ID = & 
  "$Id$" 

      CHARACTER IIP*1
      INTEGER   IPOL
      LOGICAL   HayFlujos, RETMOD(MXMOD), RETCAT(MXPROP)
      INTEGER   FZON, RZON
      REAL      RETORNO(MXZON)
      LOGICAL   LFLAG
      integer(2) :: I2
      integer :: IAN,MES,IDIA
      integer :: IHR,MINS,ISEC,MILESM
      integer(2) :: status
      integer   I

      character, parameter :: FLUJ_OPTS*(*) = 'I' // STD_OPTIONS
      external Usage

!  DENOMINACION DE LOS ARCHIVOS
!  Unidad Nombre Tipo Formato    Contenido
!     1   MENSA.DAT      con     Mnesajes
!     3    T1E     E     con     Par�metros de transporte
!     3    F1E     E     con     Par�metros de FLUJ
!     4    L2S     E     sin     Flujos por categor�a socio-econ�mica
!     5    F1S     S     sin     Flujos por categor�a de transporte

!  REQUERIMIENTOS DE LA LIBRERIA TRANUS:
!    ABRE    (Subr)  Abre un archivo OLD con o sin formato
!    CONTROL (Subr)  Lee el archivo de control CONTROL.DAT
!    LEEZ1E  (Subr)  Lee el archivo de zonas Z1E
!    LEEF1E  (Subr)  Lee el archivo de par�metros F1E
!    CHECK   (Subr)  Verifica los finales de secci�n
!    MENSA   (Subr)  Emite los mensajes



!  LEE EL ARCHIVO DE CONTROL

      IIP=' '
      CALL INIT(POL,IIP,.true.,2001, Usage, FLUJ_OPTS)

      HayFlujos = HasOpts() .or. IIP.ne.'I'
      call GetOptions()
      call InitPol(POL,IPOL)

      if (.not. HayFlujos) then
        WRITE(*,*) 'TRANSPORT ONLY APPLICATION! ',IIP
      endif

      call GetCurrentDate(IAN,MES,IDIA)
      call GetCurrentTime(IHR, MINS, ISEC, MILESM)

!  LECTURA DEL ARCHIVO DE ZONAS Z1E

      if (debugging >= dbg_normal) then     
          WRITE(*,*)'_________________________________________'
          CALL MENSA(1004,0)  ! LECTURA DE PARAMETROS Y DATOS
      endif
      call OpenPolFile(3, 0, 'Z1E', IO_FMT)
      CALL LEEZ1E
      CLOSE(3)

      CALL LecturaT1E
      CALL LecturaL2S
      CALL LecturaF1E
      CALL LecturaL1S
      CALL Itera

      if (debugging >= dbg_normal) then     
          WRITE(*,*)'_________________________________________'
          CALL TIEJEC(IHR,MINS,ISEC,MILESM)
          CALL MENSA(8,0)  ! FINAL NORMAL DE
          WRITE(*,*)'F L U J'
      endif
      STOP
333   WRITE(*,*)'****L1E****'
      CALL MENSA(10,-1)  ! ERROR G06: Archivo incompleto
444   WRITE(*,*)'****T1E****'
      CALL MENSA(10,-1)  ! ERROR G06: Archivo incompleto
CONTAINS

SUBROUTINE LecturaT1E
!  LEE T1E PARA ENTERARSE DE LAS CATEGORIAS Y MODOS DE TRANSPORTE

      call FindPolFile(3, IPOL, 'P0E', IO_FMT,POL_ANY)
      CALL LEEP0E
      CLOSE(3)
      call FindPolFile(3, IPOL, 'T1E', IO_FMT,POL_ANY)
      CALL LEET1E
      CLOSE(3)
      RETMOD=.FALSE.   ! Modos con operadores con retornos vacios
      RETCAT=.FALSE.   ! Categorias que pueden usar modos con retornos

      DO IO=1,MXOPER
         IF(PARCON(IO).LT.0) CYCLE
         RETMOD(MODOPER(IO))=.TRUE.
      ENDDO
      
      DO IP=1,NPROP
         DO K=1,MXMOD
            IF(RETMOD(K))THEN
               IF(MODO(IP,K))RETCAT(IP)=.TRUE.
            ENDIF
         ENDDO
      ENDDO
END SUBROUTINE LecturaT1E

SUBROUTINE LecturaF1E
!  LECTURA DEL ARCHIVO DE PARAMETROS F1E

      call FindPolFile(3, IPOL, 'F1E', IO_FMT, pol_OtherYear)
      CALL LEEF1E(HayFlujos)

!  Hace primera lectura de los viajes ex�genos para saber si los hay
!  Secci�n 3.1 viajes exogenos por categor�a de transporte
      XOR=.FALSE.    ! Or�genes/propositos con viajes ex�genos
      XORM=.FALSE.   ! Or�genes/propositos con viajes ex�genos por modo
      READ(3,*)
      READ(3,*)
      READ(3,*)
350   READ(3,*,END=777,ERR=351,IOSTAT=IOS)I,J,M
      ICAT=INTNUM(I,NUMCAT,NPROP)
      IF(ICAT.GT.NPROP)THEN
         WRITE(*,*)'F1E(3.1) - Cat:',I
         CALL MENSA(6,-1)  ! ERROR G04: Definici�n ilegal
      ENDIF
      KOR=INTNUM(J,NUMZON,NZN)
      IF(KOR.GT.NZN)THEN
         WRITE(*,*)'F1E(3.1) - Orig:',J
         CALL MENSA(6,-1)  ! ERROR G04: Definici�n ilegal
      ENDIF
      KDES=INTNUM(M,NUMZON,NZN)
      IF(KDES.GT.NZN)THEN
         WRITE(*,*)'F1E(3.1) - Dest:',M
         CALL MENSA(6,-1)  ! ERROR G04: Definici�n ilegal
      ENDIF
      XOR(KOR,ICAT)=.TRUE.
      GO TO 350
351   CALL CHECK(3.1,IOS,'F1E')

!  Secci�n 3.2 viajes exogenos por categor�a y modo de transporte
      READ(3,*)
      READ(3,*)
360   READ(3,*,END=777,ERR=361,IOSTAT=IOS)I,J,M,K
      ICAT=INTNUM(I,NUMCAT,NPROP)
      IF(ICAT.GT.NPROP)THEN
         WRITE(*,*)'F1E(3.2) - Cat:',I
         CALL MENSA(6,-1)  ! ERROR G04: Definici�n ilegal
      ENDIF
      KOR=INTNUM(J,NUMZON,NZN)
      IF(KOR.GT.NZN)THEN
         WRITE(*,*)'F1E(3.2) - Orig:',J
         CALL MENSA(6,-1)  ! ERROR G04: Definici�n ilegal
      ENDIF
      KDES=INTNUM(M,NUMZON,NZN)
      IF(KDES.GT.NZN)THEN
         WRITE(*,*)'F1E(3.2) - Dest:',M
         CALL MENSA(6,-1)  ! ERROR G04: Definici�n ilegal
      ENDIF
      IMO=INTNUM(K,NUMMOD,NTM)
      IF(IMO.GT.NTM)THEN
         WRITE(*,*)'F1E(3.2) - Mod:',K
         CALL MENSA(6,-1)  ! ERROR G04: Definici�n ilegal
      ENDIF

!  verifica si los viajes ex�genos se asignan a modos disponibles a la cat
      IF(.NOT.MODO(ICAT,IMO))THEN
         WRITE(*,*)'F1E(3.2) - Cat:',ICAT,' Mod:',K
         CALL MENSA(6,-1)  ! ERROR G04: Definici�n ilegal
      ENDIF

      XORM(KOR,ICAT)=.TRUE.
      GO TO 360
361   CALL CHECK(3.2,IOS,'F1E')
      RETURN

777   WRITE(*,*)'****F1E****'
      CALL MENSA(10,-1)  ! ERROR G06: Archivo incompleto

END SUBROUTINE LecturaF1E

SUBROUTINE LecturaL2S
      IF(HayFlujos)THEN
         call OpenPolFile(3, IPOL, 'L2S', IO_BIN)
         READ(3)I,NFLU,I2,I2,I2,I2,I2
         call ReadListBegin(3, NS)
         do ip=1, NS
            call CheckListItem(3, ip)
            read(3) NUMSEC(IP),NOMSEC(IP),LFLU(IP)
         enddo
         call CheckListEnd(3, NS)
         CLOSE(3)
      ENDIF
END SUBROUTINE LecturaL2S

SUBROUTINE LecturaL1S
!  LECTURA DEL ARCHIVO L1S PARA VERIFICACION

!  En una aplicacion solo transporte, no hay flujos de LOC
!  se identifica dejando en blanco el 1er grupo de F1E
!  en ese caso, el programa LEEF1E asigna HayFlujos=.FALSE.
!
!  HayFlujos=.T. si es aplicacion completa con uso del suelo y transporte

      FZON=NZN

      IF(.not.HayFlujos)FZON=1 !indica a TRANS que no hay flujos endogenos

!  Si hay flujos se lee L1S para verificar parametros de uso del suelo
!  LFLU es .T. si el sector genera flujos, viene de LEEL1E
!  NFLU es el n�mero de sectores que generan flujos

      IF(HayFlujos)THEN
         DO J=1,NS
            P1=0.
            DO I=1,NPROP
               P1=P1+PROUT(I,J)+PROIN(I,J)
            ENDDO
            IF(P1.LT.CEROMAS.AND.LFLU(J))THEN
               WRITE(*,'('' Sect'',I6)')NUMSEC(J)
               CALL MENSA(2011,-1)
            ENDIF
         ENDDO
      ENDIF
END SUBROUTINE LecturaL1S


SUBROUTINE Itera
!  ABRE LOS ARCHIVOS DE ENTRADA Y SALIDA

      IF(HayFlujos)THEN
         call OpenPolFile(4, IPOL, 'L2S', IO_BIN)
      ENDIF
      call NewPolFile(5, IPOL, 'F1S', IO_BIN)
      WRITE(5)NZN,int2(IAN),int2(MES),int2(IDIA),int2(IHR),int2(MINS)
      call WriteListBegin(5, NPROP)
      do m=1, NPROP
        call WriteListItem(5, m)
        write(5) NUMCAT(M),NOMCAT(M)
      enddo
      call WriteListEnd(5, NPROP)

      call WriteListBegin(5, NTM)
      do k=1, NTM
        call WriteListItem(5, k)
        write(5) NUMMOD(K),NOMMOD(K)
        call WriteListBegin(5, NPROP)
        do m=1, NPROP
          call WriteListItem(5, m)
          write(5) MODO(M,K)
        enddo
        call WriteListEnd(5, NPROP)
      enddo
      call WriteListEnd(5, NTM)

!  ITERACIONES RESPECTO A LAS CATEGORIAS DE TRANSPORTE 

      if (debugging >= dbg_normal) then     
          WRITE(*,*)
          WRITE(*,*)'_________________________________________'
      endif
      call WriteListBegin(5, NPROP)
      DO 500 M=1,NPROP
        call WriteListItem(5, m)
      RZON=1
      IF(RETCAT(M))RZON=NZN   ! Indica que la categ tiene retornos
      if (debugging >= dbg_normal) then     
          WRITE(*,'('' Categ:'',I6,3X,A8 )')NUMCAT(M),NOMCAT(M)
      endif

      FLUTRA = 0. ! Blanquea matriz de flujos por cat. transp

!  Lee el encabezamiento de L2S (si es que existe)

      IF(HayFlujos)THEN
        READ(4)NN,NN,I2,I2,I2,I2,I2
        call CheckListBegin(4, NS)
        do nn=1, NS
           call CheckListItem(4, nn)
           read(4) NUMSEC(nn),NOMSEC(nn),LFLU(nn)
        enddo
        call CheckListEnd(4, NS)
      ENDIF

!  Itera respecto a los sectores socioecon�micas
!  y multiplica las proporciones por el factor de volumen
!  FVOLUM es el factor de volumen = unids transporte/unids usosuelo
!  FTIEM = tiempo transporte/tiempo usuelo
!  PROUT = proporci�n de la cat s-eco en el sentido q viene de loc
!  PROIN = idem en sentido contrario

      IF(HayFlujos)THEN   ! Beatriz agrega por BUG en Chile
      call CheckListBegin(4, NS)
      DO N=1,NS    !Sectores Socioeconomicos
         call CheckListItem(4, N)
         read(4) LFLAG ! FLFU(N), para ObjTranus
         IF(.NOT.LFLU(N))CYCLE
         IF(.NOT.TIPO(M,N))FTIEMP(M,N)=1.
         PROUT(M,N)=PROUT(M,N)*FVOLUM(M,N)/FTIEMP(M,N)
         PROIN(M,N)=PROIN(M,N)*FVOLUM(M,N)/FTIEMP(M,N)
!  Lee la matriz de flujos de la categor�a s-econ�mica N
         call CheckListBegin(4, NZN)
         DO I=1,NZN
            call CheckListItem(4, i)
            read(4)LFLAG
            IF(JER1(I).NE.JER2(I))CYCLE
            READ(4,END=555,ERR=666,IOSTAT=IOS)(FLUSEC(I,J),J=1,NZN)
         ENDDO
         call CheckListEnd(4, NZN)
!  Acumula FLUSEC*PROUT en FLUTRA
         DO I=1,NZN
            IF(JER1(I).NE.JER2(I))CYCLE
            DO J=1,NZN
               FLUTRA(I,J)=FLUTRA(I,J)+FLUSEC(I,J)*PROUT(M,N)+FLUSEC(J,I)*PROIN(M,N)
            ENDDO ! Destinos
         ENDDO ! Or�genes
      ENDDO  ! Sectores socioeconomicos
      ENDIF  ! hay flujos agrega Bea por BUG en Chile
      REWIND(4)

      call Graba(M)

500   CONTINUE ! fin categorias IP
      call WriteListEnd(5, NPROP)
      CLOSE(5)
      RETURN

666   WRITE(*,*)'****L2S****'
      CALL MENSA(3,-1)   ! ERROR G03: Problema de lectura

555   WRITE(*,*)'****L2S****'
      CALL MENSA(10,-1)  ! ERROR G06: Archivo incompleto

END SUBROUTINE Itera


SUBROUTINE Graba(M)
integer :: M

!  Graba los flujos por categor�a transporte M en F1S
!  Si es que los hay, lee viajes ex�genos (si XOR,XORM=.T.)
!  El retorno es = a la transpuesta de los flujos
      call debug('writing mode')
      XVIAJES = 0
      XVIAMOD =0
      RETORNO = 0
      call debug('calling leeviajes')
      CALL LEEVIAJES(M)
      call debug('returned from leeviajes')
      call WriteListBegin(5, NZN)
      DO I=1,NZN
         call debug('writing zone')
         call WriteListItem(5, i)
         LFLAG = (JER1(I).eq.JER2(I))
         write(5) LFLAG ! para obj tranus
         IF(JER1(I).NE.JER2(I))CYCLE

         DO J=1,NZN
            IF(RETCAT(M))RETORNO(J)=FLUTRA(J,I)
         ENDDO
         write(5) &
             ( FLUTRA(I,J), XVIAJES(I,J), RETORNO(j), &
               (XVIAMOD(I,J,K), K=1, NTM),   &
               j=1, NZN)
      ENDDO   ! Fin do or�genes i
      call WriteListEnd(5, NZN)

END SUBROUTINE Graba



SUBROUTINE LEEVIAJES(M)
      CHARACTER*4 STRING
        integer::i

!  BUSCA EN F1E(3.1,3.2) VIAJES EXOGENOS DE LA CATEGORIA M, ORIGEN IO
!  La sección 3.1 contiene viajes exóg por categ a copiar en XVIAJES
!  que serán utilizados por TRANS antes de la separación modal.
!  La sección 3.2 contiene viajes exóg por categ y modo a copiar en
!  XVIAMOD que serán utilizados por TRANS después de la sep modal.

      REWIND(3)

!  Lee secci�n 3.1

      DO 12 I=1,3
14    READ(3,'(A4)')STRING
      IF(STRING(2:4).NE.'---')GO TO 14
12    CONTINUE
      READ(3,*)
      READ(3,*)
      READ(3,*)
20    READ(3,*,END=999,ERR=21,IOSTAT=IOS)ICAT,KOR,KDES,F,F1
      ICAT=INTNUM(ICAT,NUMCAT,NPROP)
      KOR=INTNUM(KOR,NUMZON,NZN)
      KDES=INTNUM(KDES,NUMZON,NZN)
      IF(ICAT.EQ.M)THEN
         XVIAJES(KOR,KDES)=F*F1
      ENDIF
      GO TO 20
21    CALL CHECK(3.1,IOS,'F1E')

!  Lee secci�n 3.2

      READ(3,*)
      READ(3,*)
30    READ(3,*,END=999,ERR=31,IOSTAT=IOS)ICAT,KOR,KDES,IMOD,F,F1
      ICAT=INTNUM(ICAT,NUMCAT,NPROP)
      KOR=INTNUM(KOR,NUMZON,NZN)
      KDES=INTNUM(KDES,NUMZON,NZN)
      IMOD=INTNUM(IMOD,NUMMOD,NTM)
      IF(ICAT.EQ.M)THEN
         if (debugging >= dbg_debug) then
            print *, 'xviamod', icat, kor, kdes, imod
         endif
         XVIAMOD(KOR,KDES,IMOD)=F*F1
      ENDIF
      GO TO 30
31    CALL CHECK(3.2,IOS,'F1E')
      RETURN

999   WRITE(*,*)'****F1E****'
      CALL MENSA(10,-1)  ! ERROR G06: Archivo incompleto
      END SUBROUTINE

subroutine GetOptions()
      if (.not. hasopts()) then
          return
      endif
      do i = 1, optc()
          select case(optv(i))
              case ('I')
                 HayFlujos = .false.
              case ('g','q','v')
                ! nothing
              case default
                 call usage
          end select
      enddo
 return
 end subroutine
END PROGRAM FLUJ

 subroutine Usage
 USE GETOPTM
 character(32) prog
    prog = argv(0)

    print *
    print '(A,'' - Transformation of Flows TRANUS(r)'')', trim(prog)

    print *,'usage:'

    print '(4X, A,''  <scen> <command> [options]'')', trim(prog)
    print *
    print *, 'If no commands are given, the program enters interactive mode'


    print *
    print *, 'Commands are:'
    print *, '  -I        : There are no flows from land use'

    print *
    print *, 'Options are:'
    call ExplainStdOptions

    STOP 02
 end subroutine

