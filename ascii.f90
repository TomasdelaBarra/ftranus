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
MODULE ASCII
USE PARAM
USE GENER
USE ZCOMM
USE CONTROL
USE MENSAMOD
USE TPARC
USE RCOMM
 
    character(80) :: ASCII_RCS_ID = & 
      "$Id$" 

CONTAINS

      SUBROUTINE LEET1E
      integer :: IOS

! Valores por default de las variables

      ALFA=0          ! Par metro restric de capac (% reducc si V/C=1)
      REDVELMAX=0     ! Par metro restric de capac (% máximo de reducción vel)
      GAMA=0          ! Par metro restric de capac (V/C en reducción máxima)
      CONV=.05        ! Criterio de convergencia
      DELTA=0         ! Elasticidad generaci¢n de viajes
      FACTIEM=0       ! Factor de tiempo por operador
      FFCAP=1.        ! Factor de capacidad de la v¡a
      GENMIN=0        ! Tasa generaci¢n m¡nima
      GENMAX=0        ! Tasa generaci¢n m xima
      LAMAS=1.        ! Elasticidad selecci¢n de pasos
      LAMB=1.         ! Elasticidad selecci¢n modal
      MODELgSc=1.     ! Escalamiento en seleccion pasos
      PATHLgSc=1.     ! Escalamiento en seleccion pasos
      NIT=20          ! N£mero de iteraciones
! Grupo 1.0 - PARAMETROS GLOBALES

      READ(3,'(////)',END=999)
      TransDampFactor = -1 
102   READ(3,*,END=999,ERR=101,IOSTAT=IOS)NIT,CONV, TransDampFactor
      if (TransDampFactor <= 0) then
         TransDampFactor = 1
      endif
      GO TO 102
101   CALL CHECK(2.0,IOS,'T1E')

! Grupo 2.1 - PARAMETROS DE RESTRICCION A LA ESPERA

      READ(3,'(//)',END=999)
611   READ(3,*,END=999,ERR=610,IOSTAT=IOS)N1,F1,F2
      IO=INTNUM(N1,NUMOP,NOPER)
      IF(IO.GT.NOPER)THEN
         WRITE(*,*)'T1E(2.1) - Oper:',N1
         CALL MENSA(6,-IUNO)  ! G04: Definici¢n ilegal
      ENDIF
      FACTIEM(IO)=F1         ! Factor entre dato freq. y tiempo Trans
      PARCON(IO) =F2       ! Par metro consolidaci¢n
      IF(F2.GT.1.0)THEN
         WRITE(*,*)'T1E(2.2) - Oper:',N1,' ',NOMOP(IO)
         CALL MENSA(4007,-IUNO)    ! T02: Par m consolidaci¢n > 1
      ENDIF
      GO TO 611
610   CALL CHECK(2.1,IOS,'T1E')
      do io=1, noper
        if(factiem(io).le.0) stop 'FACTIEM = 0'
      enddo

! Grupo 2.2 - PARAMETROS DE RESTRICCION A LA VIA

      READ(3,'(//)',END=999)
620   READ(3,*,END=999,ERR=621,IOSTAT=IOS)N1,F1,F2,F3,F4   

      if(F1 <= 0 .or. F1 >= 1       &
        .or. F2 <= F1             &
        .or. F3 <= 1 .or. F4 < 1)  &
      then
        write(*,*)' Capacity restriction params error IDB: Info/Link Types, File T1E'
        write(*,*)' % speed when V/C=1 (Alfa) must be: 0<Alfa<1'
        write(*,*)' Max Speed Reduction must be >0 and < Alfa' 
        write(*,*)' V/C when speed tends to zero (Gama) must be >1' 
        write(*,*)' Capacity factor must be > 1'
6201    format(1X,'Tip', I6,' Alfa:',F7.3, ' Max Vel Red', F7.3,' Gama:', F7.3, ' VCMin:', F7.3, ' FCap:', F7.3)
        write(*,6201)N1,F1,F2,F3,F4
        stop
      endif

      IF(N1.EQ.ICERO)THEN
         DO IT=1,NTIP
            ALFA(IT)=F1   
            REDVELMAX(IT)=F2
            GAMA(IT)=F3
            FFCAP(IT)=F4
         ENDDO
      ELSE
         IT=INTNUM(N1,NUMTIP,NTIP)
         IF(IT.GT.NTIP)THEN
            WRITE(*,*)'T1E(2.2) - Tip:',N1
            CALL MENSA(6,-IUNO)  ! G04: Definici¢n ilegal
         ENDIF
         ALFA(IT)=F1   
         REDVELMAX(IT)=F2
         GAMA(IT)=F3
         FFCAP(IT)=F4
      ENDIF
      GO TO 620
621   CALL CHECK(2.2,IOS,'T1E')

! Grupo 2.3 - PARAMETROS DE GENERACION Y SELECCION MODAL

      READ(3,'(/)',END=999)
630   READ(3,*,END=999,ERR=631,IOSTAT=IOS)N1,F1,F2,F3,F4,F6,F5
      IP=INTNUM(N1,NUMCAT,NPROP)
      IF(IP.GT.NPROP)THEN
         WRITE(*,*)'T1E(2.3) - Categ:',N1
         CALL MENSA(6,-IUNO)  ! G04: Definici¢n ilegal
      ENDIF
      GENMIN(IP)=F1        ! Tasa generaci¢n m¡nima
      GENMAX(IP)=F2-F1     ! Tasa generaci¢n m xima
      DELTA(IP)=F3         ! Elasticidad generaci¢n
      LAMB(IP)=F4          ! Par metro elasticidad
      PCAU(IP)=F5          ! Disponibilidad vehicular
      MODELgSc(IP)=F6      ! Escalamiento en logits 0..1
      IF(LAMB(IP).LT.CEROMAS)THEN
         WRITE(*,'(''T1E(2.3) - Cat:'',I5,1X,A,''   Par='',F10.4)') &
            NUMCAT(IP),NOMCAT(IP),LAMB(IP)
         CALL MENSA(4014,-IUNO)  !  T07: Par sel modal debe ser > 0
      ENDIF
      IF(MODELgSc(IP).lt.0.or.MODELgSc(IP).gt.1.)THEN
         WRITE(*,'(''T1E(2.4) - Mod:'',I5,1X,A,''   Logit ='',F10.4)') &
           NUMCAT(IP),MODELgSc(IP)
         CALL MENSA(6,-IUNO)  ! Definicion ilegal
      ENDIF
      GO TO 630
631   CALL CHECK(2.3,IOS,'T1E')

! Grupo 2.4 - PARAMETROS DE ASIGNACION

      READ(3,'(/)',END=999)
640   READ(3,*,END=999,ERR=641,IOSTAT=IOS)N1,F1,F2
      IP=INTNUM(N1,NUMCAT,NPROP)
      IF(IP.GT.NPROP)THEN
         WRITE(*,*)'T1E(2.4) - Cat:',N1
         CALL MENSA(6,-IUNO)  ! G04: Definici¢n ilegal
      ENDIF
      LAMAS(IP)=F1
      PATHLgSc(IP)=F2
      IF(LAMAS(IP).LT.CEROMAS)THEN
         WRITE(*,'(''T1E(2.4) - Mod:'',I5,1X,A,''   Par='',F10.4)') &
           NUMCAT(IP),LAMAS(IP)
         CALL MENSA(4015,-IUNO)  ! T08: Par asignaci¢n debe ser > 0
      ENDIF
      IF(PATHLgSc(IP).lt.0.or.PATHLgSc(IP).gt.1.)THEN
         WRITE(*,'(''T1E(2.4) - Mod:'',I5,1X,A,''   Logit ='',F10.4)') &
           NUMCAT(IP),PATHLgSc(IP)
         CALL MENSA(4015,-IUNO)  ! T08: Par asignaci¢n debe ser > 0
      ENDIF
      GO TO 640
641   CALL CHECK(2.4,IOS,'T1E')
      RETURN

999   WRITE(*,*)'T1E'
      CALL MENSA(10,-IUNO)
      END SUBROUTINE



      SUBROUTINE LEEP0E
      ! Tambien limpia las variables correspondientes al T1E
      !
      CHARACTER(32) NOM
      INTEGER NAUX(MXMOD)

! Valores por default de las variables

      ALFA=0          ! Par metro restric de capac (% reducc si V/C=1)
      GAMA=0          ! Par metro restric de capac (V/C en que Vel=0)
      CARGOS=0        ! Peajes por unidad de distancia
      CMINMAN=0       ! Costo m¡nimo de mantnm por tipo enlace
      CMANMAR=0       ! Costo marginal de mantnm por oper y tipo enlc
      CONV=.05        ! Criterio de convergencia
      COPDIS=0        ! Costo de operaci¢n por distancia
      COPMIN=0        ! Costo de operaci¢n m¡nimo
      COPTIE=0        ! Costo de operaci¢n por tiempo
      COSMIN=RINF     ! Tarifa m¡nima de transferencia
      COSTEN=0        ! Precio unitario de la energ¡a
      COSTIEM=0       ! Tarifa por unidad de tiempo
      DELTA=0         ! Elasticidad generaci¢n de viajes
      DREL=0          ! Tarifa por unidad de distancia
      ESPMIN=0        ! Espera m¡nima
      ESPMAX=RINF     ! Espera m xima
      FACTIEM=0       ! Factor de tiempo por operador
      FFCAP=1.        ! Factor de capacidad de la v¡a
      GENMIN=0        ! Tasa generaci¢n m¡nima
      GENMAX=0        ! Tasa generaci¢n m xima
      IOPRUT=ICERO    ! Operador al que pertenece cada ruta
      LAMAS=1.        ! Elasticidad selecci¢n de pasos
      LAMB=1.         ! Elasticidad selecci¢n modal
      LPUB=.FALSE.    ! Modos que contienen operador p£blico
      MODO=.FALSE.    ! Modos que puede utilizar una categor¡a
      NIT=20          ! N£mero de iteraciones
      NOP=ICERO       ! N£mero de operadores por modo
      NRUTEN=ICERO    ! N£mero de rutas por enlace
      PAREN1=0        ! Consumo m¡nimo de energ¡a por distancia
      PAREN2=0        ! Consumo m ximo de energ¡a por distancia
      PAREN3=0        ! Pendiente de curva de consumo de energ¡a
      SPEED=0         ! Relaci¢n VelOper/VELTIP
      TARCOP=0        ! Transforma el costo operaci¢n en tarifa
      VELTIP=0        ! Velocidad de referencia a flujo libre
      VS=0            ! Veh¡culos est ndar
      PENLACE=1.0     ! Penalizacion enlaces
      
      OzMode   = 0    ! Overlapping por modo
      OzLtOper = 0    ! Overlapping por tipo de via y modo
      FreqMin  = 0    ! Frecuencia minima
      FreqMax  = 0    ! Frecuencia maxima
      TOCPromedio = 0 ! Tasa de ocupacion objetivo
      FleetMax   = 0  ! Flota maxima
      RutScheduled = .FALSE.

      CatPenalFactor  = 1 ! Factor penalizacion por cat/oper
      CatTariffFactor = 1 ! Factor tarifa por cat/oper
      MODE_ASC        = 0

      
! Grupo 1.0 - PARAMETROS GLOBALES
! Lee NPUBEN, el n£mero m ximo de operadores p£blicos por enlace

      READ(3,'(////)',END=999)
102   READ(3,*,END=999,ERR=101,IOSTAT=IOS)NRUTEN
      IF(NRUTEN.GT.MXRUTINL)THEN
         WRITE(*,*)'P0E(1.0) - MXRUTINL',Nruten,'   Max ',MXRUTINL
         CALL MENSA(9,-IUNO)  ! Excedida dimens¢n m xima'
      ENDIF
      GO TO 102
101   CALL CHECK(1.0,IOS,'P0E')

! Grupo 2.1 - Modos de transporte (N£mero, Nombre, NoPasos, Overlapping)

      READ(3,'(//)',END=999)
      NTM=ICERO  ! N£mero de modos
211   READ(3,*,END=999,ERR=212,IOSTAT=IOS) N1,NOM,N2,OV1,ASC
      NTM=NTM+1
      IF(NTM.GT.MXMOD)THEN
         WRITE(*,*)'P0E(2.1) - Mod:',NTM,'   Max:',MXMOD
         CALL MENSA(9,-IUNO)  ! G05: Excedida dimensi¢n
      ENDIF
      NUMMOD(NTM)=N1       ! N£mero externo de un modo
      NOMMOD(NTM)=NOM      ! Nombre del modo
      NPAS(NTM)=N2         ! N£mero de pasos
      OzMode(NTM)=OV1      ! Factor de overlapping
      MODE_ASC(NTM) = ASC
      GO TO 211
212   CALL CHECK(2.1,IOS,'P0E')
      IF(NTM.LE.ICERO)THEN
         WRITE(*,*)'P0E(2.1)'
         CALL MENSA(15,-IUNO)  ! G07: Esta secci¢n no puede quedar vac¡a
      ENDIF

!  Grupo 2.2 - Operadores de transporte

      READ(3,'(/)',END=999)
      NOPER=ICERO          ! N£mero de operadores
      NRUTAS=ICERO         ! N£mero de rutas
220   READ(3,*,END=999,ERR=222,IOSTAT=IOS) N1,NOM,N2,N3,F1,F2,F3,F4,F5
      NOPER=NOPER+1
      IF(NOPER.GT.MXOPER)THEN
         WRITE(*,*)'P0E(2.2) - Oper:',NOPER,'   Max:',MXOPER
         CALL MENSA(9,-IUNO)  ! G05: Excedida dimensi¢n
      ENDIF
! Verifica consistencia del modo al que pertenece el operador
      K=INTNUM(N2,NUMMOD,NTM)
      IF(K.GT.NTM)THEN
         WRITE(*,*)'P0E(2.2) - Mod:',N2
         CALL MENSA(6,-IUNO)  ! G04: Definici¢n ilegal
      ENDIF
! Verifica consistencia del tipo de operador
      !J.95.03.28 agregado operador tipo 5, para cambio de modo
      IF(N3.LT.1.OR.N3.GT.5)THEN
         WRITE(*,*)'P0E(2.2) - Oper:',N1,'   TipOp:',N3
         CALL MENSA(6,-IUNO)  ! G04: Definici¢n ilegal
      ENDIF
! Asigna las caracter¡sticas
      NOP(K)=NOP(K)+1           ! N£mero de operadores por modo
      MODOPER(NOPER)=K          ! Modo al que pertenece el operador
      NUMOP(NOPER)=N1           ! N£mero externo de un operador
      NOMOP(NOPER)=NOM          ! Nombre del operador
      TOC(NOPER)=F1             ! Tasa ocupaci¢n
      ITIPOP(NOPER)=N3          ! Tipo de operador
      OPERPathASC(NOPER)=F5     ! Constante aditiva para pasos
      IF(N3.NE.3)THEN           ! Si no es tipo 3, lo asigna como ruta
         NRUTAS=NRUTAS+1
         NUMRUT(NRUTAS)= -N1    ! N£mero de la ruta
         NOMRUT(NRUTAS)= NOM     ! Nombre de la ruta '::' para interna
         IOPRUT(NRUTAS)=NOPER   ! Operador al que pertenece la ruta
      ENDIF
! Si el operador es tipo 2 o 3, el modo se define como p£blico
      IF(ITIPOP(NOPER).GE.2)LPUB(K)=.TRUE.
      IF (F1.LT.CEROMAS)THEN
         WRITE(*,*)'P0E(2.2) - Oper:',N1,' ',NOM
         CALL MENSA(4004,-IUNO)    ! T01: Tasa de opcupaci¢n debe ser > 0
      ENDIF
      CONSTM(NOPER)=F2          ! Penalizaci¢n
      ESPMIN(NOPER)=F3
      ESPMAX(NOPER)=F4
           
      GO TO 220
222   CALL CHECK(2.2,IOS,'P0E')
      IF(NOPER.LE.ICERO)THEN
         WRITE(*,*)'P0E(2.2)'
         CALL MENSA(15,-IUNO)  ! G07: Esta secci¢n no puede quedar vac¡a
      ENDIF

! Grupo 2.3  Rutas de transporte p£blico

      READ(3,'(/)',END=999)
       
230   F1 = 0
      F2 = 0
      F3 = 0
      F4 = 0
      F5 = 0
      READ(3,*,END=999,ERR=231,IOSTAT=IOS)N1,NOM,N2,F1,F2,F3,F4,F5
      NRUTAS=NRUTAS+1
      IF(NRUTAS.GT.MXRUT)THEN
         WRITE(*,*)'P0E(2.3) - Rut:',NRUTAS,'  Max:',MXRUT
         CALL MENSA(9,-IUNO)       ! G05: Excedida dimensi¢n
      ENDIF
      NUMRUT(NRUTAS)=N1
      NOMRUT(NRUTAS)=NOM
      K=INTNUM(N2,NUMOP,NOPER)
      IF(K.GT.NOPER.OR.ITIPOP(K).NE.3)THEN
         WRITE(*,*)'P0E(2.3) - Rut:',N1,'  Oper:',N2
         CALL MENSA(6,-IUNO)       ! G04: Definici¢n ilegal
      ENDIF
      IOPRUT(NRUTAS)=K          ! Operador de la ruta
      IF(F1 <= 0)THEN
         WRITE(*,*)'P0E(2.3) - Rut:',N1,'  ',NOM
         CALL MENSA(4001,-IUNO)    ! T05: La frecuencia debe ser > 0
      ENDIF
      FreqMin(NRUTAS)=F1
      if (F2 < 0) then
         F2 = RINF
      elseif (F2 < F1) then !!!OJO: Se está tragando el error sin decirle al usuario!
        F2 = F1
      endif
      FreqMax(NRUTAS)=F2
      if (F3 > 0 .and. F3 <= 1) then
           TOCPromedio(NRUTAS) = F3
      else
         WRITE(*,*)'P0E(2.3) - Rut:',N1,'  ',NOM, F3
         CALL MENSA(10015,-IUNO)    
      endif
      if (F4 < 0) F4 = -1
      FleetMax(NRUTAS) = F4

      RutScheduled(NRUTAS) = (F5 .ne. 0)
      GO TO 230
231   CALL CHECK(2.3,IOS,'P0E')

! Grupo 2.4  Administradores

      READ(3,'(/)',END=999)
      ADMIN=ICERO
240   READ(3,*,END=999,ERR=241,IOSTAT=IOS) N1,NOM
      ADMIN=ADMIN+1
      IF(ADMIN.GT.MXADM)THEN
         WRITE(*,*)'P0E(2.4) - Admin:',ADMIN,'   Max:',MXADM
         CALL MENSA(9,-IUNO)  ! G05: Excedida dimensi¢n
      ENDIF
      NUMAD(ADMIN)=N1
      NOMAD(ADMIN)=NOM
      GO TO 240
241   CALL CHECK(2.4,IOS,'P0E')
      IF(ADMIN.LE.ICERO)THEN
         WRITE(*,*)'P0E(2.4)'
         CALL MENSA(15,-IUNO)  ! G07: Esta secci¢n no puede quedar vac¡a
      ENDIF

! Grupo 2.5  Tipos de v¡a

      READ(3,'(/)',END=999)
      NTIP=ICERO
250   READ(3,*,END=999,ERR=251,IOSTAT=IOS) N1,NOM,N2,F2
      NTIP=NTIP+1
      IF(NTIP.GT.MXTIP)THEN
         WRITE(*,*)'P0E(2.5) - Tip:',NTIP,'   Max:',MXTIP
         CALL MENSA(9,-IUNO)  ! G05: Excedida dimensi¢n
      ENDIF
      NUMTIP(NTIP)=N1
      NOMTIP(NTIP)=NOM
      K=INTNUM(N2,NUMAD,ADMIN)
      IF(K.GT.ADMIN.and.N2.ne.0)THEN
        WRITE(*,*)'P0E(2.5) - Adm:',N2
        CALL MENSA(6,-IUNO)  ! G04: Definici¢n ilegal
      ENDIF
      IAD(NTIP)=K         ! Administrador por tipo de v¡a
      CMINMAN(NTIP)=F2    ! Costo m¡nimo de mantenimiento por tipo
      GO TO 250
251   CALL CHECK(2.5,IOS,'P0E')
      IF(NTIP.LE.ICERO)THEN
         WRITE(*,*)'P0E(2.5)'
         CALL MENSA(15,-IUNO)  ! G07: Esta secci¢n no puede quedar vac¡a
      ENDIF

! Grupo 3.0 - CATEGORIAS DE TRANSPORTE
! Calcula VOTME, VOTMV, NPROP y MODO

      READ(3,'(/)',END=999)
      NPROP=ICERO         ! N£mero de categor¡as
300   NAUX=ICERO
      READ(3,*,END=999,ERR=302,IOSTAT=IOS) N1,NOM,F2,F3, &
        (NAUX(K),K=1,NTM)
      NPROP=NPROP+1
      IF(NPROP.GT.MXPROP)THEN
         WRITE(*,*)'P0E(3.0) - Categ:',NPROP,'   Max:',MXPROP
         CALL MENSA(9,-IUNO)  ! G05: Excedida dimensi¢n
      ENDIF
      NUMCAT(NPROP)=N1
      NOMCAT(NPROP)=NOM
      VOTV(NPROP)=F2         ! Valor tiempo de viaje
      VOTE(NPROP)=F3         ! Valor del tiempo de espera
      if (VOTV(NPROP) <= 0) then
         WRITE(*,*)' P0E(3.0) - Cat:',N1,NOM,' Mod:',NAUX(K)
         WRITE(*,*)' Value of Travel Time must be greater than zero'
         CALL MENSA(6,-IUNO)  ! G04: Definici¢n ilegal
      endif
      NMOD=ICERO
      DO K=1,NTM
         IF(NAUX(K).LE.ICERO)EXIT
         NMOD=NMOD+IUNO
         N1=INTNUM(NAUX(K),NUMMOD,NTM)
         IF(N1.GT.NTM)THEN
            WRITE(*,*)' P0E(3.0) - Cat:',N1,NOM,' Mod:',NAUX(K)
            CALL MENSA(6,-IUNO)  ! G04: Definici¢n ilegal
         ENDIF
         MODO(NPROP,N1)=.TRUE.
      ENDDO
      IF(NMOD.LE.ICERO)THEN
          WRITE(*,*)' P0E(3.0) - Cat:',N1,NOM
          CALL MENSA(6,-IUNO)  ! G04: Definici¢n ilegal
      ENDIF
      GO TO 300
302   CALL CHECK(3.0,IOS,'P0E')
      IF(NPROP.LE.ICERO)THEN
         WRITE(*,*)'P0E(3.0)'
         CALL MENSA(15,-IUNO)  ! G07: Esta secci¢n no puede quedar vac¡a
      ENDIF

      DO K=1,NTM
         VOTMV(K)=0    
         VOTME(K)=0    
         N=ICERO
         DO IP=1,NPROP
            IF(MODO(IP,K))THEN
               VOTMV(K)=VOTMV(K)+VOTV(IP)
               VOTME(K)=VOTME(K)+VOTE(IP)
               N=N+1
            ENDIF
         ENDDO
         IF(N.LE.ICERO)THEN
            WRITE(*,*)'P0E(3.0) - Mod',NUMMOD(K),' ',NOMMOD(K)
            CALL MENSA(4008,-1)  ! Ninguna categor¡a puede usar este modo
         ENDIF
         VOTMV(K)=VOTMV(K)/N
         VOTME(K)=VOTME(K)/N
      ENDDO

! Grupo 4.1 - Par metros de consumo de energ¡a

      READ(3,'(//)',END=999)
410   READ(3,*,END=999,ERR=411,IOSTAT=IOS)N1,F1,F2,F3,F4,F5,F6
      IO=INTNUM(N1,NUMOP,NOPER)
      IF(IO.GT.NOPER)THEN
         WRITE(*,*)'P0E(4.1) - Oper:',N1
         CALL MENSA(6,-IUNO)  ! G04: Definici¢n ilegal
      ENDIF
      PAREN1(IO)=F1
      PAREN2(IO)=F2-F1
      PAREN3(IO)=F3
      COSTEN(IO)=F4
      COPMIN(IO)=F5
      COPTIE(IO)=F6
      GO TO 410
411   CALL CHECK(4.1,IOS,'P0E')

! Grupo 4.2 - Condiciones y costos de operaci¢n por enlace

      READ(3,'(/)',END=999)
420   READ(3,*,END=999,ERR=421,IOSTAT=IOS)N1,N2,F1,F2,F3,F4,F5,F6
      IT=INTNUM(N1,NUMTIP,NTIP)
      IF(IT.GT.NTIP)THEN
         WRITE(*,*)'P0E(4.2) - Tip:',N1
         CALL MENSA(6,-IUNO)  ! G04: Definici¢n ilegal
      ENDIF
      IO=INTNUM(N2,NUMOP,NOPER)
      IF(IO.GT.NOPER)THEN
         WRITE(*,*)'P0E(4.2) - Oper:',N2
         CALL MENSA(6,-IUNO)  ! G04: Definici¢n ilegal
      ENDIF
      SPEED(IO,IT)=F1      ! Por ahora, velocidad de flujo libre
      VS(IO,IT)=F2         ! Veh¡culos equivalentes
      COPDIS(IO,IT)=F3     ! Costo de operaci¢n por distancia
      CARGOS(IO,IT)=F4     ! Peajes
      CMANMAR(IO,IT)=F5    ! Costo marginal de mantenimiento por veh/tip
      PENLACE(IO,IT)=F6    ! Penalizaci¢n por tipo de enlace
      GO TO 420
421   CALL CHECK(4.2,IOS,'P0E')

!  Para cada tipo de enlace, busca la >velocidad y la asigna a VELTIP
!  Luego divide SPEED por VELTIP para que quede la relaci¢n
      DO IT=1,NTIP
         DO IO=1,NOPER
            IF(SPEED(IO,IT).GT.VELTIP(IT))VELTIP(IT)=SPEED(IO,IT)
         ENDDO
         IF(VELTIP(IT).LT.CEROMAS)THEN
            WRITE(*,*)'P0E(4.2) - Tip',NUMTIP(IT)
            CALL MENSA(3017,-IUNO)  ! P10: Ning£n oper puede usar este arco
         ENDIF
         DO IO=1,NOPER
            SPEED(IO,IT)=SPEED(IO,IT)/VELTIP(IT)
         ENDDO
      ENDDO
                    
                    
! Grupo 4.3 - Overlapping por tipo de via y operador
      Sect = GetNextFileSection(3)
      if (Sect.eq.4.3) then    
         call ReadOzLtOper(3)
         call CHECKIU(3, 4.3,IOS,'P0E')
      else 
         backspace(3) 
      endif
      call SetupOzFactors
! Seccion 5.1 - TARIFAS LINEALES POR OPERADOR

      call CheckNextFileSection(3, 5.0)
      call CheckNextFileSection(3, 5.1)
      READ(3,*,END=999)
510   READ(3,*,END=999,ERR=515,IOSTAT=IOS) N1,F1,F2,F3,F4
      IO=INTNUM(N1,NUMOP,NOPER)
      IF(IO.GT.NOPER)THEN
         WRITE(*,*)'P0E(5.1) - Oper:',N1
         CALL MENSA(6,-IUNO)    ! G04: Definici¢n ilegal
      ENDIF
      if(F1.lt.0) F1 = RINF
      COSTIEM(IO)=F2         ! Tarifa por tiempo
      DREL(IO)=F3            ! Tarifa por por distancia
      VTARCOP(IO) = F4
      TARCOP(IO)=F4/TOC(IO)  ! Multiplica costo de operaci¢n
                             ! y de una vez los divide por la tasa ocup
      BoardTariff(IO) = F1
      DO I=0,NOPER
          OPTARIFF(I,IO) = F1
         !!!!! 
         ! Es incorrecto sumar el costo de operación constante en cada enlace
         ! Eliminado hasta tener esto mejor craneado
         ! COSMIN(I,IO)=F1+COPMIN(IO)*TARCOP(IO)   
      ENDDO
      GO TO 510
515   CALL CHECK(5.1,IOS,'P0E')

! Grupo 5.2 - Tarifas integradas

      READ(3,'(/)',END=999)
520   READ(3,*,END=999,ERR=521,IOSTAT=IOS)N1,N2,F1
      IO=INTNUM(N1,NUMOP,NOPER)
      IF(IO.GT.NOPER)THEN
         WRITE(*,*)'P0E(5.2) - Oper:',N1
         CALL MENSA(6,-IUNO)  ! G04: Definici¢n ilegal
      ENDIF
      K=INTNUM(N2,NUMOP,NOPER)
      IF(K.GT.NOPER)THEN
         WRITE(*,*)'P0E(5.2) - Oper:',N2
         CALL MENSA(6,-IUNO)  ! G04: Definici¢n ilegal
      ENDIF
      if(f1.lt.0) then
        OPTARIFF(IO,K) = RINF
        cosmin(io,k) = F1
      else
        OPTARIFF(IO,K) = F1
        COSMIN(IO,K)=F1          ! Costo de transferencia
      endif
      GO TO 520
521   CALL CHECK(5.2,IOS,'P0E')

! Grupo 5.3 - Tarifas por categoria
      Sect = GetNextFileSection(3)
      if (Sect.eq.5.3) then    
         call ReadCatTariffs(3)
         call CHECKIU(3, 5.3,IOS,'P0E')
      else 
         backspace(3) 
      endif

      RETURN

999   WRITE(*,*)'P0E'
      CALL MENSA(10,-IUNO)
      END SUBROUTINE

      SUBROUTINE SetupOzFactors
         integer it, io
         
         do io =1, NOPER
             do it = 1, NTIP
                if ( OzLtOper(io, it).eq.0) then
                    OzLtOper(io, it) = OzMode(ModOper(io))
                endif
             enddo
         enddo
      RETURN
      END SUBROUTINE

      SUBROUTINE ReadOzLtOper(iUn)
        integer iUn
        integer nt, no, it, io
        real    Oz
        
        read(iUn, *) ! Saltar la línea de títulos
        do while (.TRUE.)
          read(iUn,*,END=499,ERR=431,IOSTAT=IOS)nt,no,Oz
                             
          it = iFindNum(nt, NumTip, NTip)
          io = iFindNum(no, NumOp, NOper)
          
          if(it.eq.0) then
               WRITE(*,*)'P0E(4.3) - Tip:',nt
               CALL MENSA(6,-IUNO)  ! G04: Definici¢n ilegal
          endif
          
          if(io.eq.0) then
               WRITE(*,*)'P0E(4.3) - Op:',no
               CALL MENSA(6,-IUNO)  ! G04: Definici¢n ilegal
          endif

          OzLtOper(io, it) = Oz
        enddo 
431     continue
499     continue 
      RETURN
      END SUBROUTINE



SUBROUTINE ReadCatTariffs(iUn)
        integer iUn
        integer :: np, no, ip, io
        real    :: tf, pf, asc
        
        read(iUn, *) ! Saltar la línea de títulos
        do while (.TRUE.)
          read(iUn,*,END=499,ERR=431,IOSTAT=IOS) np, no, pf, tf, asc
                             
          ip = iFindNum(np, NumCat, NProp)
          io = iFindNum(no, NumOp,  NOper)
          
          if(ip == 0) then
               WRITE(*,*)'P0E(5.3) - Cat:',np
               CALL MENSA(6,-IUNO)  ! G04: Definici¢n ilegal
          endif
          
          if(io == 0) then
               WRITE(*,*)'P0E(5.3) - Op:',no
               CALL MENSA(6,-IUNO)  ! G04: Definici¢n ilegal
          endif

          if (pf < 0 .or. tf < 0) then
               WRITE(*,103) np, no, pf, tf
               CALL MENSA(6,-IUNO)  ! G04: Definici¢n ilegal
          endif

          CatPenalFactor(ip, io)  = pf
          CatTariffFactor(ip, io) = tf
          OpCatASC(ip, io) = asc

        enddo 
431     continue
499     continue 
103     format(' P0E(5.3) - Cat:', I3,' Op:',I3,' ',2F7.2)
      RETURN
END SUBROUTINE
      

      SUBROUTINE AddLinkRoute(NL, IR,LnR)
        INTEGER NL, IR, LnR

            if (RutPri(nLink).gt.0) then
                  do i=RutPri(nLink), RutUlt(nLink)
                     if(Abs(IROUTE(i)).eq.Abs(IR)) then
                        ! error suave, no lo reportamos
                        RETURN
                     endif
                  enddo
            endif
            IF(LnR+1.GT.MXLNRUT)THEN
                WRITE(*,'('' P1E(3.0): Or'',I6,''  Des'',I6, ''  Tip'',I6)')IOR(NL),IDES(NL),NUMTIP(ITIP(NL))
                CALL MENSA(3018,-1)  ! P11 demasiados enlace-ruta
            ENDIF
            LnR=LnR+1
            IROUTE(LnR)=Abs(IR)
            ILINK(LnR)=NL
            if (RutPri(NL).eq.0) then
              RutPri(NL) = LnR
              RutUlt(NL) = LnR
              NRutin(NL) = 1
            else
              RutUlt(NL) = RutUlt(NL)+1
              NRutin(NL) = NRutin(NL)+1
            endif
            NoStops(LnR)=IR.lt.0
      END SUBROUTINE

      SUBROUTINE LEERED(IUP1E, IUP0S)
      integer :: IUP1E
      integer,optional :: IUP0S
      integer   NOOR(NLMAX),NODES(NLMAX)
          integer, parameter :: MXRUTGIR=MXRUT+MXGIR
          logical   SECC,REPE
          INTEGER   RutGir(MXRUTGIR+1)
          INTEGER   status, PrimerGiro
          INTEGER   LnR, nGir
          INTEGER   FromNode, ToNode1, ToNode2
          REAL      Delay
          logical   REDANT

      REDANT = present(IUP0S)

      status = msg_OK

      IOR =0
      IDES=0
      LINKID = 0
      ITIP=0
      DIS=0
      CAP=0
      VEL=0
      LinkDelay = 0
      NRUTIN=0
      IROUTE=0
      ESPERA=0
      VOL=0
      VEH=0
      NOOR=0
      NODES=0
      NLINK=0
      NLINKRUT=0
      NRUTIN=0
      RUTPRI=0
      RUTULT=-1
      NOGIR=0
      turn_delay=0
      Congest   = 0 
      Queue     = 0
      QTime     = 0
      Suben  = 0
      LnFlow    = 0
      Espera    = 0

!  Secciones 1.0 y 2.0 - enlaces eliminados o modificados
      READ(IUP1E,'(////)',END=309)   ! encabezamiento
      NLMOD=0                ! n£mero de enlaces eliminados/modif
      SECC=.TRUE.                ! sirve para saber si Secci¢n 1 o 2
      A=1.0
      IN =1 ! Trabajamos con un solo sentido
100   DO L=1,NLMAX
         READ(IUP1E,*,END=309,ERR=110,IOSTAT=IOS)IO,ID,IdentGIS
         NLMOD=NLMOD+1
         IF(NLMOD.GT.NLMAX)THEN
            WRITE(*,'(''P1E('',F3.1,'' - MXLINKS'',I6)')A,NLMAX
            CALL MENSA(9,-1) ! G05: Excedida dimensi¢n m xima
         ENDIF
         NOOR(NLMOD)=IO
         NODES(NLMOD)=ID
         IF(.FALSE. .and. IN.EQ.2)THEN
            NLMOD=NLMOD+1
            IF(NLMOD.GT.NLMAX)THEN
               WRITE(*,'('' P1E('',F3.1,'') - MXLINKS'',I6)')A,NLMAX
               CALL MENSA(9,-1) ! G05: Excedida dimensi¢n m xima
            ENDIF
            NOOR(NLMOD)=ID
            NODES(NLMOD)=IO
         ENDIF
      ENDDO                           ! Fin do enlaces
110   CALL CHECKIU(IUP1E,A,IOS,'P1E')

!  Si SECC=.T. se ha leido la Secc 1.0 pero falta la 2.0 (regresa a 100)

      IF(SECC)THEN
         SECC=.FALSE.
         A=2.0
         READ(IUP1E,'(/)',END=309)
         GO TO 100
      ENDIF
      if(.NOT.REDANT.AND.NLMOD.GT.0) then
          CALL MENSA(3006,-1) ! P02: Modificaci¢n sin red anterior
      endif

!  Lee red anterior P0S(5) -si REDANT=.T.- y copia a P0S(4) los enlaces
!  que ni se eliminan ni se modifican

      NLINK=  0
      NLINKRUT=0
      NRUTIN=0
      RUTPRI=0
      RUTULT=-1
      NOGIR=0
      TwoWayLink = .False.
      IF(REDANT) THEN
         CALL RDREDANT(IUP0S, NLMOD, NOOR, NODES)
         call CheckStatus(status)
      ENDIF   ! Fin de IF(REDANT) lectura y copia de la red anterior


!  Lee los enlaces nuevos o modificados de P1E(3) y los a¤ade en P0S(4)

      REWIND(IUP1E)
      READ(IUP1E,'(////)')   ! encabezamiento
      DO I=1,NLMAX       ! pasa por bola la secci¢n 1.0
         READ(IUP1E,*,END=309,ERR=410,IOSTAT=IOS)IIO,ID,IdentGIS
      ENDDO
410   CALL CHECKIU(IUP1E,1.0,IOS,'P1E')

!  Secciones 2.0 (enlaces que se modifican) y 3.0 (nuevos enlaces)

      SECC=.TRUE.             ! sirve para saber si es la secci¢n 2 o 3
      A=2.0
400   READ(IUP1E,'(/)',END=309)   ! encabezamiento

      DO             ! enlaces
         RutGir=0
         READ(IUP1E,*,END=309,ERR=420,IOSTAT=IOS)IIO,ID,IdentGIS,IT,D,CV, Delay, &
              (RutGir(k), k=1, MXRUTGIR)
!       Verifica consistencia del tipo de v¡a
         ITINT=INTNUM(IT,NUMTIP,NTIP)  ! No interno tipo v¡a
         IF(ITINT.GT.NTIP.OR.D.LE.0)THEN
            WRITE(*,'('' P1E('',F3.1,'')  Orig'',I6,''   Dest'',I6, ''  Tip'',I6)')A,IIO,ID,IT
            CALL MENSA(6,-1)  ! G04: definici¢n ilegall
         ENDIF
!     Verifica que las rutas especificadas sean legales
         PrimerGiro = 0 ! Comienzo de restricciones de giro
         DO K=1,MXRUTGIR
            IF(RutGir(K).EQ.0) then
               PrimerGiro = K+1
               EXIT
            elseif(NRUTEN.gt.0.and.k.gt.NRUTEN)then
               ! Viejo formato de rutas/giros prohibidos
               PrimerGiro = K
               EXIT
            endif

            IR=INTNUM(ABS(RutGir(K)),NUMRUT,NRUTAS)
            if (IR.gt.NRUTAS) then
                ! Buscar si es una ruta 'interna', o sea de un
                ! operador que no es tipo 3. Su intnum es negativo
                IR=INTNUM(-ABS(RutGir(K)),NUMRUT,NRUTAS)
            endif
            if (IR.gt.NRUTAS) then
               WRITE(*,'('' P1E('',F3.1,''): Or'',I6,''  Des'',I6, ''  Tip'',I6,''  Rut'',I6)')A,IIO,ID,IT,RutGir(K)
               CALL MENSA(6,-1)  ! G04: definici¢n ilegal
            ELSE
               IO=IOPRUT(IR)
               IF(SPEED(IO,ITINT).le.0) then
                   WRITE(*,'('' P1E('',F3.1,''): Or'',I6,''  Des'',I6, ''  Tip'',I6,''  Rut'',I6)')A,IIO,ID,IT,RutGir(K)
                   CALL MENSA(6,-1)  ! G04: definici¢n ilegal
                endif
!                 Preservar prohibiciones de parada
                if (RutGir(K).gt.0) then
                   RutGir(K)=IR
                else
                   RutGir(K)=-IR
                endif
            ENDIF
         ENDDO ! MXRUTGIR
!  Asigna las caracter¡sticas del enlace nuevo o modificado
         NLINK=NLINK+1
         IF(NLINK.GT.NLMAX)THEN
            WRITE(*,'('' P1E('',F3.1,'') - MXLINKS'',I6)')A,NLMAX
            CALL MENSA(9,-1)  ! G05: Excedida dimensi¢n m xima
         ENDIF
         IF(CV.EQ.0)THEN
            WRITE(*,'('' P1E('',F2.1,'')  Orig'',I6,''   Dest'',I6, ''   Tip'',I6)')A,IIO,ID,IT
            CALL MENSA(3005,-1)  ! P02: Enlace c/capacidad f¡sica cero
         ENDIF
         IOR(NLINK)=IIO
         IDES(NLINK)=ID
         LINKID(NLINK) = IdentGIS
         ITIP(NLINK)=ITINT
         DIS(NLINK)=D
         CAP(NLINK)=CV
         VEL(NLINK)=VELTIP(ITINT)
         LinkDelay(NLINK)=Delay

!          Salta lista de ceros despues de rutas
         do while (RutGir(PrimerGiro).eq.0.and.PrimerGiro.lt.MXRUTGIR)
           PrimerGiro = PrimerGiro+1
         enddo
!          Asigna prohibiciones de giro, que estan en la parte final
!          de RutGir()
         nGir = 0
         DO K=PrimerGiro, MXRUTGIR
           if (RutGir(k).eq.0) EXIT
           nGir = nGir+1
           if(nGir.gt.MXGIR) then
             WRITE(*,'('' P1E('',F3.1,'') - MXGIR'',I6)')A,MXGIR
             write(*, '(''link'', I8, ''-'', I8 )') IOR(NLINK),IDES(NLINK)
             call MENSA(9,-1)
           endif
           NOGIR(NLINK,nGir)=RutGir(K)
           turn_delay(NLINK, nGir) = -1
         ENDDO

!         ! Crea los enlaces ruta
         LnR=NLINKRUT
         DO k=1,PrimerGiro-1
            if(RutGir(k).eq.0) EXIT
            call AddLinkRoute(NLINK,RutGir(k), LnR)
         ENDDO   ! Fin do rutas IR

!            ! verificar si el usuario quiere que se asignen automaticamente
!            ! al enlace los operadores con velocidad distintas de 0
         if (NRUTEN.ne.0) then
            DO IR=1,NRUTAS
               IO=IOPRUT(IR)
               IF(SPEED(IO,ITINT).LT.CEROMAS)CYCLE
               IF(ITIPOP(IO).EQ.3) CYCLE

               !else
               call AddLinkRoute(NLINK,IR, LnR)
            ENDDO   ! Fin do rutas IR
         endif


         IF (LnR.GT.NLINKRUT) THEN
           NLINKRUT=LnR
         ENDIF

!  Si el enlace es doble sentido, crea un nuevo enlace invertido

         IF(.FALSE. .and. IN.EQ.2)THEN
            TwoWayLink(NLINK) = .True.
            NLINK=NLINK+1
            IF(NLINK.GT.NLMAX)THEN
               WRITE(*,*)'P1S(3.0) - MXLINKS'
               CALL MENSA(9,-1)  ! G9: Excedida dimensi¢n m xima
            ENDIF
            TwoWayLink(NLINK) = .True.
            IOR(NLINK) =IDES(NLINK-1)
            IDES(NLINK)=IOR(NLINK-1)
            ITIP(NLINK)=ITIP(NLINK-1)
            DIS(NLINK) =DIS(NLINK-1)
            CAP(NLINK) =CAP(NLINK-1)
            VEL(NLINK) =VEL(NLINK-1)
            LinkDelay(NLINK)=LinkDelay(NLINK-1)
            ! Copiar los enlace-ruta
            if(RutPri(nLink-1).ne.0) then  
              do i=RutPri(nLink-1), RutUlt(nLink-1)
                  call AddLinkRoute(nLink, IRoute(i), NLinkRut)
              enddo
            endif
         ENDIF
      ENDDO                   ! fin do enlaces IL
420   CALL CHECKIU(IUP1E,A,IOS,'P1E')

! Si SECC=.T. se ha leido la Secci¢n 2 pero falta la 3.0 (regresa a 400)

      IF(SECC)THEN
         SECC=.FALSE.
         A=3.0
         GO TO 400
      ENDIF

      if (debugging >= dbg_normal) then
        WRITE(*,*)
        CALL MENSA(8155,0)  !  VERIFICANDO ENLACES REPETIDOS
        WRITE(*,*)
      endif
      REPE=.FALSE.
      DO I=1,NLINK-1  ! Recorre cada enlace
         DO J=I+1,NLINK
            IF(IOR(I).EQ.IOR(J).AND.IDES(I).EQ.IDES(J))THEN
               WRITE(*,*)
               WRITE(*,'('' Or'',I6,''  Des'',I6,''  Itip'',I6)') &
                         IOR(I),IDES(I),ITIP(I)
               REPE=.TRUE.
            ENDIF
         ENDDO  !  Fin do enlaces de destino J
      ENDDO     !  Fin do enlaces de origen I
      IF(REPE) THEN
! necesito que no se pare por enlaces repetidos para SACOG
! provisional, juanca 95.01.17
         CALL MENSA(8156,0)  ! Este enlace est  repetido
      ENDIF


      ! Calculo de las esperas
      ESPERA=0
      VOL=0
      VEH=0
      DO LnR=1, NLINKRUT
          IR=IROUTE(LnR)
          IO=IOPRUT(IR)
          ESPERA(LnR)=ESPMIN(IO)
          ! Eliminado calculo de la espera porque hacia que pasos
          ! descartara rutas de baja frecuencia que podrian resultar
          ! buenas en una red cargada
          !IF(ITIPOP(IO).EQ.3) THEN
          !   if (.not. RutScheduled(IR).and.FreqMin(IR).ne.0) then
          !      ESPERA(LnR) = ESPERA(LnR) + (1/(2*FreqMin(IR)) ) 
          !   endif
          !ENDIF
      ENDDO

      ! Read intersection delays
      Sect = GetNextFileSection(IUP1E)
      if (Sect == 4.0) then
         call debug('Reading intersection delays')
         READ(IUP1E,*) ! skip title line
         do 
             READ(IUP1E,*,END=309,ERR=609) FromNode, ToNode1, ToNode2, Delay
             if (debugging >= dbg_debug) then
                WRITE(*,*) 'delay', FromNode, ToNode1, ToNode2, Delay
             endif
             L = FindLink(FromNode, ToNode1)
             if (L <= 0) then
                write(*, '(1X, I4, ''->'', I4,''?'')') FromNode, ToNode1
                continue
             endif
             do i =1, MXGIR
               if (NoGir(L,i) == 0 .or. NoGir(L,i) == ToNode2) then
                  NoGir(L,i) = ToNode2
                  turn_delay(L, i) = Delay
                  exit
               ! else skip
               endif
               if(i >= MXGIR) then
                   WRITE(*,'('' P1E('',F3.1,'') - MXGIR'',I6)')4.0,MXGIR
                   call MENSA(9,-1)
                   endif
             enddo
         enddo
609      call CHECKIU(IUP1E, 4.0,IOS,'P1E')
      endif

!  Final normal
789  if (debugging >= dbg_normal) then
        CALL MENSA(3007,0)  ! N£mero total de enlaces en la red
        WRITE(*,*)NLINK,NLINKRUT
      endif
      RETURN

!  Final con errores
309   WRITE(*,*)'P1E'
      CALL MENSA(10,-1)    ! G6: Archivo incompleto
102   WRITE(*,*)'T3S'
      CALL MENSA(3,-1)     ! G3: Problema de lectura
260   WRITE(*,*)'P0S-Prev'
      CALL MENSA(3,-1)     ! G3: Problema de lectura
500   CALL MENSA(3004,-1)

      END SUBROUTINE


!//////////////////////////////////////////////////////////////////////
      subroutine LeeG1E(iun)
        integer, intent(in) :: iun
        character(32) :: NOM
        logical :: LNOD
        logical :: ERRO
       
      NUMNOD=0
      NOMNOD='        '
      NODOS=0

! Secci¢n 1.0 - N£mero y nombre de los tipos de nodos
      READ(iun,'(////)',END=999)
      NODTIP=0       ! N£mero de tipos de nodos
!  Inicia el primer tipo de nodo 0 para centroide
101   READ(iun,*,END=999,ERR=100)N1,NOM
      NODTIP=NODTIP+IUNO
      IF(NODTIP.GT.MXNODTIP)THEN
         WRITE(*,*)'G1E(1.0) - MAXNODTIP=',MXNODTIP
         CALL MENSA(9,-1)  ! Excedida dimens¢n m xima'
      ENDIF
      NUMNOT(NODTIP)=N1    ! n£mero externo de los tipos de nodos
      NOMNOT(NODTIP)=NOM  ! nombre de los tipos de nodos
      GO TO 101
100   CALL CHECK(1.0,IOS,'G1E')

! Secci¢n 2:  Caracter¡sticas de las coordenadas
      READ(iun,'(/)',END=999)
201   READ(iun,*,END=999,ERR=200)ESCALA
      GO TO 201    ! Para obligarlo a caer en check
200   CALL CHECK(2.0,IOS,'G1E')

!  Secci¢n 3: coordenadas de los nodos
      NUMNOD=0
      NOMNOD='                            '
      READ(iun,'(/)',END=999)
301   READ(iun,*,END=999,ERR=300)N1,NOM,N2,N3,N4
      n5=0
      do i=1, nodtip
        if(numnot(i).eq.n2) then
          n5=i
          exit
        endif
      enddo
      IF(N5.eq.0)THEN
         ERRO=.TRUE.
         WRITE(*,*)'G1E(3.0) - TipNod:',N2
         CALL MENSA(6,0)  ! G04: Definici¢n ilegal
      ENDIF
      LNOD=.FALSE.
      DO I=1,NODOS   !  Busca el nodo en la lista
         IF(NUMNOD(I).EQ.N1)THEN   ! El nodo estaba en la lista
            LNOD=.TRUE.
            EXIT
         ENDIF
      ENDDO
      IF(LNOD)THEN      ! El nodo no estaba en la lista
        WRITE(*,*) N1, ' NODO REPETIDO'
        GOTO 999
      ELSE
         NODOS=NODOS+1
         I = NODOS
         NUMNOD(I)=N1
         NOMNOD(I)=NOM   ! Nombre del nodo
         TIPNOD(I)=N5    ! Tipo interno del nodo
         XNOD(I)=N3      ! Coordenada x del nodo
         YNOD(I)=N4      ! Coordenada y del nodo
      ENDIF
      GO TO 301
300   CALL CHECK(3.0,IOS,'G1E')

!  Verifica que todas las coordenadas est‚n definidas
      DO I=1,NODOS
         IF(XNOD(I).LE.ICERO.AND.YNOD(I).LE.ICERO)THEN
            WRITE(*,*)'G1E(3.0) - Nod:',NUMNOD(I)
            CALL MENSA(8103,0)  ! Este nodo no tiene coordenadas
         ENDIF
      ENDDO

999   return
      end subroutine



END MODULE ASCII
