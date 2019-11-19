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
PROGRAM IMPAS
USE DEBUGM
USE GETOPTM
USE PARAM
USE RCOMM
USE PCOMM
USE ZCOMM
USE TPARC
USE CONTROL
USE GENER
USE IO_LIST
USE ASCII

character(80) :: IMPAS_RCS_ID = & 
  "$Id$" 


!  IMPZON  Es .T. si hay que imprimir esa combinaci¢n de zona or-des
!  IMPMOD  Es .T. si hay que imprimir ese modo
      LOGICAL IMPZON(MXZON,MXZON), IMPMOD(MXMOD)

common /IMPASCOMM/ impzon

      CHARACTER IUN(6)*1,SALIDA*32,IIP*1, SI*1
      INTEGER IPOL
      INTEGER status
      LOGICAL NE
      DATA IUN/'1','2','3','4','5','6'/
      integer(2) :: IDIA,MES,IAN,IHR,MINS,I2

      integer IMP_OPTION, LASTOR
      logical useImpasDat
      character, parameter :: IMPAS_OPTIONS*(*) = STD_OPTIONS // 'l:Lo:p:Pm:M'
      external Usage

!  DENOMINACION DE LOS ARCHIVOS

!  Unidad   Nombre   Tipo   Formato   Contenido
! ---------------------------------------------------------
!    1    MENSA.DAT           con    Mensajes
!    2    SALIDA                     Seg£n opci¢n (pant, imp, o archivo)
!    3     Z1E       Entra    con    Zonificacion
!    3     IMPAS.DAT Entra    con    Opciones de impresion (opcional)
!    3     P0S       Entra    sin    Red de transporte
!    3     PnS,..    Entra    sin    Pasos por modo n

!  REQUERIMIENTOS DE LA LIBRERIA TRANUS:
!    ABRE    (Subr)  Abre un archivo OLD con o sin formato
!    CONTROL (Subr)  Lee el archivo de control CONTROL.DAT
!    SALI    (Subr)  Pregunta por el archivo de salida
!    LEEZ1E  (Subr)  Lee el archivo de zonas Z1E
!    TARIFA  (Subr)  Calcula la tarifa marginal decreciente
!    CHECK   (Subr)  Verifica los finales de secci¢n
!    INTNUM  (Func)  Calcula No interno que corresponde a uno externo
!    MENSA   (Subr)  Emite los mensajes ( <0=err, 0=adv, 1=?, >1=graba )

      CALL INIT(POL,IIP,.FALSE.,8000, Usage, IMPAS_OPTIONS)
      CALL InitPol(POL, IPOL)

!  LEE ENCABEZAMIENTO DE P0S PARA SABER LOS PARAMETROS

      call OpenPolFile(3, IPOL, 'P0S', IO_BIN)
      CALL RDTPAR(3,status,I,I2,I2,I2,I2,I2)
      call CheckStatus(status)
      CLOSE(3)

      SI=CHAR(15)
      SALIDA = AREA // POL // '.PAS'

      call GetOptions(SALIDA)

      if (debugging >= dbg_Debug) then
        print *, 'Report No. is ', IMP_OPTION, LASTOR
      endif

!  Opciones de ingreso de datos (1=manual, 2=IMPAS.DAT)
      if (.not. hasOpts()) then
539     WRITE(*,*)
        WRITE(*,*)'___________________________________________'
        CALL MENSA(8001,0)  ! Opciones para ingreso de datos: ...
        WRITE(*,*)'___________________________________________'
        CALL MENSA(7,1)     !    Opci¢n --->
        READ(*,'(I2)')IP
        IF(IP.LT.ICERO.OR.IP.GT.1)GO TO 539
        IF(IP.EQ.1) THEN
          INQUIRE(FILE='IMPAS.DAT',EXIST=NE)
          IF(.NOT.NE)THEN
             CALL MENSA(8002,0)  ! No existe el archivo IMPAS.DAT
             GO TO 539
          ENDIF
          useImpasDat = .true.
        ENDIF
        CALL SALI(SALIDA)
      else
        CALL SALI(SALIDA, .false.)
      endif

! ESPECIFICA EL ARCHIVO DE SALIDA

      IF(SALIDA(1:3).EQ.'PRN')WRITE(2,'('' '',A)')SI



!  Ingreso de datos por archivo de parametros IMPAS.DAT

      IF(useImpasDat) then
        OPEN(3,FILE='IMPAS.DAT')
        CALL MENU(IMP_OPTION,LASTOR)
        CLOSE(3)
      ELSEIF (.not. hasOpts()) then
!  Ingreso de datos por pantalla
        CALL PASMENU(IMP_OPTION,NODORIG,NODEST,LASTOR)
      ENDIF

!  Escribe el encabezamiento

      if (debugging >= dbg_Debug) then
        print *, 'Writing output header'
      endif

      WRITE(*,*)'___________________________________________'
      CALL MENSA(8310,2)
      CALL MENSA(8000,2)
      WRITE(*,*)'___________________________________________'

!  LEE ARCHIVO DE LA RED P0S e imprime si IMP_OPTION=3

      call OpenPolFile(3, IPOL, 'P0S', IO_BIN)
!  Lee encabezamiento
      CALL RDTPAR(3,status,I,IDIA,MES,IAN,IHR,MINS)
      CALL CheckStatus(status)

      LINMAX=54   ! M ximo n£mero de l¡neas por p gina
      IF(SALIDA(1:3).EQ.'CON')LINMAX=20
      NARCO=ICERO
      LINEAS=ICERO
      CALL RDREDANT(3)

      if (debugging >= dbg_Debug) then
        print '(''Checking options'')'
      endif
      CALL LINKCOSTS() ! Calcular los costos de los arco-rutas
      DO II=1,NLINK
         NUOPEN=NRUTIN(II)
         NUGIR=ICERO
         DO K=1,MXGIR
            IF(NOGIR(II,K).NE.ICERO)NUGIR=NUGIR+1
         ENDDO
         LINEAS=LINEAS+1
         IF(IMP_OPTION.EQ.3.AND.LINEAS.EQ.1.AND.SALIDA(1:3).EQ.'CON')CALL MENSA(8015,2)
         IF(IMP_OPTION.EQ.3)WRITE(2,'(1X,2I8,I4,F6.1,I8,F8.0,20I8)') &
           IOR(II),IDES(II),NUMTIP(ITIP(II)),DIS(II),CAP(II),VEL(II), &
           (NUMRUT(IROUTE(LNR)),LNR=RUTPRI(II),RUTULT(II)), &
           (-NOGIR(II,K),K=1,NUGIR)
         IF(IOR(II).EQ.NODORIG.AND.IDES(II).EQ.NODEST)NARCO=II
         IF(IMP_OPTION.EQ.3.AND.LINEAS.GE.LINMAX)THEN
            LINEAS=ICERO
         ENDIF
      ENDDO
306   CLOSE (3)
      IF(IMP_OPTION.EQ.3)GO TO 1551
      IF(NARCO.EQ.ICERO.AND.IMP_OPTION.EQ.2)THEN
        WRITE(*,'('' Orig:'',I6,''   Dest:'',I6)')NODORIG,NODEST
        CALL MENSA(8003,-1)  ! No encontr‚ este enlace
      ENDIF

!  IMPRIME LOS RESULTADOS DE LOS PASOS DEL MODO K

      DO 10 K=1,NTM
      if (debugging >= dbg_Debug) then
        print *, 'Checking mode ', K
      endif
      IF(.NOT.IMPMOD(K))GO TO 10
      if (debugging >= dbg_Debug) then
        print *, 'Reporting mode ', K, 1
      endif
!  Abre el archivo PKS
      CALL CALCKCOST(K)
      ! Abre el archivo de pasos PkS
      call OpenPolFile(3, IPOL, 'P' // IUN(K) // 'S', IO_BIN)
      call SkipPathHeader(3)
!  Escribe los encabezamientos de acuerdo a la opci¢n
      WRITE(2,*)
      IF(IMP_OPTION.EQ.1)THEN
         CALL MENSA(8005,2)  ! PASOS ENTRE ZONAS POR MODO
         WRITE(2,'(I4,1X,A8,2X,I2.2,''-'',I2.2,''-'',I4,1X,I2.2,' &
          // ''':'',I2.2)')NUMMOD(K),NOMMOD(K),IDIA,MES,IAN,IHR,MINS
      ELSEIF(IMP_OPTION.EQ.2)THEN
         CALL MENSA(8006,2)  ! PASOS QUE UTILIZAN UN ENLACE POR MODO
         WRITE(2,'(I4,1X,A8,2X,I8,''->'',I8,'  &
           // '2X,I2.2,''-'',I2.2,''-'',I4,1X,I2.2,'':'',I2.2)') &
        NUMMOD(K),NOMMOD(K),NODORIG,NODEST,IDIA,MES,IAN,IHR,MINS
      ENDIF
      WRITE(2,790)
790   FORMAT(' ',80('-'))
      WRITE(2,*)
!  Si la opci¢n es 1 o 2 escribe los pasos
      IF(IMP_OPTION.LE.2)THEN
         if (debugging >= dbg_Debug) then
           print *, 'Calling pasimp', LASTOR
         endif
         CALL PASIMP(K,IMP_OPTION,NARCO,LASTOR)
      ENDIF
10    CONTINUE   ! Fin do modos k

      SI=CHAR(18)
      IF(SALIDA(1:3).EQ.'PRN'.OR.SALIDA(1:3).EQ.'prn')WRITE(2,1550)SI
1550  FORMAT(' ',A1/'1')
1551  if (debugging >= dbg_Normal) then
        CALL MENSA(8,0)  ! FINAL NORMAL DE
        WRITE(*,*)' I M P A S '
     endif
     STOP

CONTAINS

      SUBROUTINE MENU(IMP_OPTION,LASTOR)
!     ===============
      INTEGER AUX(MXZON)

!  Lectura de datos del archivo IMPAS.DAT

      READ(3,'(///)',END=999,ERR=999)

! SECCION 1.0 - Opci¢n principal (0=algunos pasos, 1=todos los pasos)

      IMP_OPTION=1
101   READ(3,*,END=999,ERR=100,IOSTAT=IOS)IMP_OPTION
      GO TO 101
100   CALL CHECK(1.0,IOS,'IMP')
      IF (IMP_OPTION.GT.1.OR.IMP_OPTION.LT.ICERO)THEN
         WRITE(*,'(''IMPAS.DAT(1.0) - Op:'',I3)')IMP_OPTION
         CALL MENSA(6,-1)  ! ERROR G04: Definici¢n ilegal
      ENDIF

! SECCION 2.0 - Lee los modos a imprimir (0=todos los modos)

      DO I=1,NTM
         AUX(I)=ICERO
      ENDDO
      READ(3,*)
200   READ(3,*,END=999,ERR=201,IOSTAT=IOS)(AUX(I),I=1,NTM)
      GO TO 200
201   CALL CHECK(2.0,IOS,'IMP')
      IF(AUX(1).EQ.ICERO)THEN
         DO I=1,NTM
            IMPMOD(I)=.TRUE.
         ENDDO
      ELSE
         DO I=1,NTM
            IF(AUX(I).NE.ICERO)THEN
               IF(INTNUM(AUX(I),NUMMOD,NTM).GT.NTM)THEN
                  WRITE(*,*)'IMPAS.DAT(2.0) - Mod:',AUX(I)
                  CALL MENSA(6,-1)  ! ERROR G04: Definici¢n ilegal
               ENDIF
               IMPMOD(INTNUM(AUX(I),NUMMOD,NTM))=.TRUE.
            ENDIF
         ENDDO
      ENDIF

! Origenes y destinos a imprimir

      IF(IMP_OPTION.EQ.1)THEN
        DO I=1,NZN
           DO J=1,NZN
              IMPZON(I,J)=.TRUE.
           ENDDO
        ENDDO
        LASTOR=NZN
        RETURN
      ENDIF

! Secci¢n 3.0 -  zonas a imprimir

      LASTOR=ICERO
      READ(3,'(/)')
      DO 2 I=1,NZN
      DO J=1,NZN
         AUX(J)=ICERO
      ENDDO
      READ(3,*,END=999,ERR=300)IORIG,(AUX(J),J=1,NZN)
      II=INTNUM(IORIG,NUMZON,NZN)
      IF(II.GT.NZN)THEN
         WRITE(*,*)'IMPAS.DAT(3.0) - Orig:',IORIG
         CALL MENSA(6,-1)  ! ERROR G04: Definici¢n ilegal
      ENDIF
      IF(II.GT.LASTOR)LASTOR=II
      IF(AUX(1).EQ.ICERO)THEN
         DO J=1,NZN
            IMPZON(II,J)=.TRUE.
         ENDDO
      ELSE
         DO 4 J=1,NZN
         IF(AUX(J).EQ.ICERO)GO TO 2
         JJ=INTNUM(AUX(J),NUMZON,NZN)
         IF(JJ.GT.NZN)THEN
            WRITE(*,*)'IMPAS.DAT(3.0) - Dest:',AUX(J)
            CALL MENSA(6,-1)  ! ERROR G04: Definici¢n ilegal
         ENDIF
4        IMPZON(II,JJ)=.TRUE.
      ENDIF
2     CONTINUE
300   CALL CHECK(3.0,IOS,'IMP')
      RETURN
999   WRITE(*,*)'IMPAS.DAT'
      CALL MENSA(10,-1)  ! ERROR G06: Archivo incompleto
      END SUBROUTINE


      SUBROUTINE PASMENU(IMP_OPTION,NODORIG,NODEST,LASTOR)
!     ==================

      INTEGER AUX(MXZON)

!  Opci¢n de impresi¢n de pasos (0=algunos, 1=todos, 2=enlace, 3=red)

539   WRITE(*,'(///)')
      WRITE(*,*)'___________________________________________'
      CALL MENSA(8008,0)  ! Opciones de impresi¢n de pasos:...
      WRITE(*,*)'___________________________________________'
      CALL MENSA(7,1)
      READ(*,'(I2)')IMP_OPTION
      IF(IMP_OPTION.LT.ICERO.OR.IMP_OPTION.GT.4)GO TO 539
      IF(IMP_OPTION.EQ.3)RETURN
!  Pregunta por los modos a imprimir

      WRITE(*,*)
      WRITE(*,*)
      WRITE(*,*)'___________________________________________'
      CALL MENSA(8009,0)  ! Ingrese lista de los modos a imprimir...
      DO 500 I=1,NTM
500   WRITE(*, '('' [''I1,'']  '',A8,''                         '')' &
               // '')I,NOMMOD(I)
      WRITE(*,*)'___________________________________________'
503   CALL MENSA(7,1)  !    Opci¢n --->
      READ(*,*)(AUX(I),I=1,NTM)
      IF(AUX(1).GT.ICERO)THEN
         DO 501 I=1,NTM
         IF(AUX(I).EQ.ICERO)GO TO 502
         II=INTNUM(AUX(I),NUMMOD,NTM)
         IF(II.GT.NTM)THEN
            WRITE(*,*)'Mod:',NUMMOD(I)
            CALL MENSA(6,0)  ! ERROR G04: Definici¢n ilegal
            GO TO 503
         ENDIF
501      IMPMOD(II)=.TRUE.
      ELSE
         DO 520 I=1,NTM
520      IMPMOD(I)=.TRUE.
      ENDIF

!  Opci¢n todas las zonas

502   IF(IMP_OPTION.EQ.1)THEN
         DO 530 I=1,NZN
         DO 530 J=1,NZN
530      IMPZON(I,J)=.TRUE.
         LASTOR=NZN
         RETURN
      ENDIF

!  Opci¢n algunas zonas

      IF(IMP_OPTION.EQ.ICERO)THEN
         LASTOR=ICERO
         WRITE(*,*)
         WRITE(*,*)
         WRITE(*,*)
         WRITE(*,*)'___________________________________________'
         CALL MENSA(8010,0)  ! Especifique los or¡genes y destinos ..
         WRITE(*,*)'___________________________________________'
         DO 505 I=1,NZN
         DO 506 J=1,NZN
506      AUX(J)=ICERO
507      CALL MENSA(8011,1)  !  Origen (0 para terminar) --->
         READ(*,*)IORIG
         IF(IORIG.EQ.ICERO)GO TO 510
         II=INTNUM(IORIG,NUMZON,NZN)
         IF(II.GT.NZN)THEN
            WRITE(*,*)'Zon:',IORIG
            CALL MENSA(6,0) ! ERROR G04: Definici¢n ilegal
            GO TO 507
         ENDIF
         IF(II.GT.LASTOR)LASTOR=II
         CALL MENSA(8012,0)  !  Lista de destinos..
         READ(*,*)(AUX(J),J=1,NZN)
         IF(AUX(1).EQ.ICERO)THEN
            DO 508 J=1,NZN
508         IMPZON(II,J)=.TRUE.
         ELSE
            DO 509 J=1,NZN
            IF(AUX(J).EQ.ICERO)GO TO 512
            JJ=INTNUM(AUX(J),NUMZON,NZN)
            IF(JJ.GT.NZN)GO TO 509
            IMPZON(II,JJ)=.TRUE.
509         CONTINUE
512         CONTINUE
         ENDIF
505      CONTINUE        
510      RETURN
      ENDIF

!  Opcion de pasos por un enlace

      IF(IMP_OPTION.EQ.2)THEN
         WRITE(*,*)
         WRITE(*,*)'___________________________________________'
         CALL MENSA(8013,0)  ! Indique los nodos de origen y destino
         WRITE(*,*)'___________________________________________'
         WRITE(*,'(''    Orig -----> ''$)')
         READ(*,*)NODORIG
         WRITE(*,'(''    Dest -----> ''$)')
         READ(*,*)NODEST
         RETURN
      ENDIF
      END SUBROUTINE


      SUBROUTINE PASIMP(K,IMP_OPTION,NARCO,LASTOR)
!     =================

      integer DESTINOS(MXARC)
      INTEGER RUTAS(MXARC)
      CHARACTER*1 SI
        integer   IOANT
        integer   LNR, LNRA, IR, IO, KARCO, I, J, K3
        real      COSMON, COSCAN, COSGEN
        integer(1) :: ioverl
        LOGICAL   LL
        logical :: OperSeen(NOPER)

      SI=CHAR(12)
      I5=ICERO
      I6=ICERO

      DO 70 I=1,NZN   ! Iteraciones r/zona de origen I
      IF(JER1(I).NE.JER2(I))GO TO 70

      if (debugging >= dbg_Normal) then
         todo = 1.0*ntm*nzn*nzn
         algo = 1.0*(k-1)*nzn*nzn+(i-1)*nzn+j
         WRITE(*, '(A1, ''Reading paths '', I3,''% '',$)') &
                13, INT(100.0*algo/todo)
      endif
      if (debugging >= dbg_Debug) then
        print *, 'pasimp zone ',I, LASTOR, IMP_OPTION
      endif
      IF(I.GT.LASTOR.AND.IMP_OPTION.NE.2)RETURN
      DO 18 J=1,NZN   ! Iteraciones r/zona de destino J
      IF(JER1(J).NE.JER2(J).OR.I.EQ.J)GO TO 18
      if (debugging >= dbg_Debug) then
        print '(''Reading path from'', I6, '' to '', I6 )', I, J
      endif
      read(3) &
       nps, &
       ( & 
            path_delay(l), &
            ncol(l), &
          ( matpas(l,ic), &
            ioverl, &
          ic=1,ncol(l)), &
       l=1,nps)

!  Opci¢n 2: Pasos que utilizan un enlace
      IF(IMP_OPTION.EQ.2)THEN
         DO L=1,NPS
            LL=.FALSE.
            DO K3=1,NCOL(L)
               IF(ILINK(ABS(MATPAS(L,K3))).EQ.NARCO)THEN
                 WRITE(2,5002)NUMZON(I),NUMZON(J),NUMRUT(IRoute(ABS(MATPAS(L,K3)))),L,NPS
               ENDIF
            ENDDO
5002        FORMAT('    Orig:',I8,'   Dest:',I8,'  Rut:',I4,'  Pas:',I4,' (/',I2,')')
         ENDDO
         GO TO 18
      ENDIF

!  Otras opciones

      IF(.NOT.IMPZON(I,J))GO TO 18   ! No le tocaba este par or-des
      IF(I5.EQ.I)GO TO 199
      WRITE(2,*)
      WRITE(2,103)
      I6=I6+2
103   FORMAT(' ',80('-'))
199   I5=I
      I6=I6+1
      I6=I6+1

! Recorre cada paso para calcular valores e imprimirlos

      DO 22 L=1,NPS
      DISTPAS=RCERO   ! Distancia del paso L
      TMPAS=RCERO     ! Tiempo del paso L
      ESPAS=RCERO     ! Espera en el paso L
      CMPAS=RCERO     ! Costo monetario del paso L
      PEAJE=RCERO     ! Costo del peaje en el paso L
      UPEAJE=0        ! Peaje cobrado al usuario
      CGPAS=RCERO     ! Costo generalizado en el paso L
      IRANT=ICERO     ! Ruta anterior en el paso

! Recorre cada enlace/ruta del paso
      OperSeen = .FALSE.
      DO K1=1,NCOL(L)
         LNR  =MATPAS(L,K1)
         LNRA =ABS(LNR)
         KARCO=ILINK(LNRA)  ! Arco en el paso
         IR=IROUTE(LNRA)    ! Ruta en el paso
         IO=IOPRUT(IR)     ! Operador de la ruta
         TIEME=0
         TIEMV=TMV(LNRA)
         TARPRE=TARIFA(LNRA)
         VELOC=VEL(KARCO)*SPEED(IO,ITIP(KARCO))
         IF(ITIPOP(IO).EQ.4)VELOC=VELTIP(ITIP(KARCO))*SPEED(IO,ITIP(KARCO))
         IT=ITIP(KARCO)
         DISTAN=DIS(KARCO)

          COSCAN = 0
          COSMON = TARIFA(LNRA)
          IF(IRANT.NE.IR.OR.LNR.LT.0) THEN
             IF (IRANT.NE.0) THEN
               IOANT=IOPRUT(IRANT) !operador de ruta anterior
             ELSE
               IOANT=IO
             ENDIF
             TIEME=ESPERA(LNRA)
             COSMON=COSMON+COSMIN(IOANT,IO)
             COSCAN=COSCAN+CTME(LNRA)
             if (.not. OperSeen(io)) then
               COSCAN = COSCAN + OpCatASC(ip, io)
               OperSeen(io) = .TRUE.
             endif
          ENDIF
          COSGEN=CTMV(LNRA)+COSCAN+COSMON


! Acumula los valores del paso
         DISTPAS=DISTPAS+DIS(KARCO)    ! Distancia total acumulada
         TMPAS=TMPAS+TIEMV             ! Tiempo de viaje acumulado
                                       ! Tiempo de espera acumulado
         ESPAS=ESPAS+TIEME
         CMPAS=CMPAS+COSMON            ! Costo monetario acumulado
         PJE= DIS(KARCO)*CARGOS(IO,IT) ! Peaje 
         PEAJE=PEAJE+ PJE              ! Peaje acumulado
         UPEAJE=PEAJE+PJE*TARCOP(IO)   ! Peaje al usuario acumulado
         CGPAS=CGPAS+COSGEN            ! Costo generalizado acumulado

         IRANT=IR

      ENDDO   ! Fin do enlaces K1 del paso L
      ! agregar el valor del total de los retrasos en intersecciones
      CGPAS = CGPAS + path_delay(l) * votv(ip)
      
      CALL MENSA(8014,2)  ! Orig Dest  Modo      Paso   Dist...
      WRITE(2,'(1X,2I8,I5,1X,A8,I3,10F8.2)')NUMZON(I),NUMZON(J),NUMMOD(K), &
        NOMMOD(K),L,DISTPAS,TMPAS,ESPAS,CMPAS,PEAJE,UPEAJE,CGPAS
      DO K3=1,NCOL(L)
         KARCO=ILINK(ABS(MATPAS(L,K3)))
         DESTINOS(K3)=IDES(KARCO)
         IF(MATPAS(L,K3).LT.ICERO)DESTINOS(K3)=-DESTINOS(K3)
         IR=IROUTE(ABS(MATPAS(L,K3)) )
         RUTAS(K3)=NUMRUT(IR)
      ENDDO
      IF(NOP(K).GT.1)THEN
        WRITE(2,101)(RUTAS(K3),DESTINOS(K3),K3=1,NCOL(L))
        I6=I6+(NCOL(L)/14)+2
      ELSE
        WRITE(2,102)(DESTINOS(K3),K3=1,NCOL(L))
        I6=I6+(NCOL(L)/25)+2
      ENDIF
      I6=ICERO
      WRITE(2,'(A/)')SI

22    CONTINUE   ! Fin do pasos L

18    CONTINUE   ! Fin do zonas de destino J

70    CONTINUE   ! Fin do zonas de origen I

101   FORMAT((3X,8(I5,':',I8,' >')))
102   FORMAT(3X,25I8)
      WRITE(2,'(A/)')SI
      RETURN
999   CALL MENSA(4005,-1)  ! T03:  Error de lectura en archivo de pasos
      END SUBROUTINE




subroutine GetOptions(SAL)
USE ZCOMM
 character SAL*(*)
      integer n, im, idor, iddes, i, j, iopt
      character*(MXARGLEN) buf

      if (.not. hasOpts()) then
          return
      endif

      useImpasDat = .false.
      IMPMOD=.false.
      IMPZON=.false.
      LASTOR=0
      NODORIG=0
      NODODES=0

      do iopt = 1, optc()
          select case(optv(iopt))
              case ('d')
                 useImpasDat = .true.
                 call verbose('using IMPAS.DAT ')
              case ('l')
                 IMP_OPTION=2
                 call optargirange(iopt, idor,iddes)
                 if (debugging >= dbg_Verbose) then
                   print '(''Reporting link ('',2I8,'')'')', idor, iddes
                 endif
                 NODORIG=idor
                 NODODES=iddes
              case ('L')
                 IMP_OPTION=3
              case ('m')
                 n  = ioptarg(iopt)
                 im = iFindNum(n, NumMod, NTM)
                 if (im > 0) then
                    IMPMOD(im)=.TRUE.
                 else
                   print '(''Mode'', I4, '' not found '')', n
                   STOP 01
                 endif
              case ('M')
                 IMPMOD = .true.
              case ('o')
                 SAL=trim(optarg(iopt))
                 if (SAL == '-') then
                    SAL = 'CON'
                 endif
              case ('p')
                 IMP_OPTION=0
                 call optargirange(iopt, idor,iddes)
                 if (debugging >= dbg_Verbose) then
                   print '(''Reporting path ('',2I8,'')'')', idor, iddes
                 endif
                 i = iFindNum(idor,  NumZon, NZ1)
                 if (i <= 0) then
                   print '(''Zone'', I4, '' not found '')', i
                   STOP 01
                 endif
                 j = iFindNum(iddes, NumZon, NZ1)
                 if (j <= 0) then
                   print '(''Zone'', I4, '' not found '')', i
                   STOP 01
                 endif
                 IMPZON(i, j) = .true.
                 LASTOR=max(LASTOR, max(i,j))
                 if (debugging >= dbg_Verbose) then
                   print '(''Reporting od pair ('',2I8,'')'')', i, j
                 endif
              case ('P')
                 IMP_OPTION=1
                 IMPZON(1:NZN,1:NZN)=.true.
                 LASTOR=NZN
                 if (debugging >= dbg_Verbose) then
                   print *, 'Reporting  all paths', NZN
                 endif
          end select
      enddo
      SAL = trim(SAL)
 return
 end subroutine
 
END PROGRAM ImPas

 subroutine Usage
 USE GETOPTM
 character*32 prog
    prog = argv(0)

    print *
    print '(A,'' - Path reports for TRANUS(r)'')', trim(prog)

    print *,'usage:'

    print '(4X, A,''  <scen> [options]'')', trim(prog)
    print *
    print *, 'If no commands are given, the program enters interactive mode'


    print *
    print *, 'Options are:'
    print *, '  -d              : Read options from IMPAS.DAT'
    print *, '  -o <name>       : Write reports to file <name>. Default is "AREAPOL.PAS"'
    print *, '  -o -            : Write reports to standard output.'
    print *, '  -l <id>,<id>    : Include paths that use link <id>,<id> in report.'
    print *, '  -L              : Report list of links.'
    print *, '  -p <ior>,<ides> : Include path from <ior> to <ides> in report.'
    print *, '  -P              : Include all paths in report.'
    print *, '  -m <id>         : Include mode <id> in report.'
    print *, '  -M              : Include all modes in report.'
    call ExplainStdOptions

    STOP 02
 end subroutine

