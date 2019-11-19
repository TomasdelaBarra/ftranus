! ************************************************************************
! * TRANUS - Integrated Land Use and Transport Model
! * 
! * $Id$
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
MODULE TPARC !  COMMON DE LOS PARAMETROS DE TRANSPORTE
USE PARAM
USE GENER
USE ZCOMM
USE CONTROL
USE IO_LIST

    character(80) :: TPARC_RCS_ID = & 
      "$Id$" 

      CHARACTER(32) ::  NOMMOD,NOMCAT,NOMOP,NOMTIP,NOMAD,NOMRUT
      logical(1) ::  MODO,LPUB
      REAL LAMB,LAMAS
      REAL MODELgSc, PATHLgSc, MODE_ASC
      integer(1) ::  ITIPOP
      INTEGER   IVERH, IVERL

      REAL :: REDVELMAX

      real ::                  &
         SPEED(MXOPER,MXTIP),  & ! Relaci�n VelOper/VELTIP
         CARGOS(MXOPER,MXTIP), & ! Peajes por unidad de distancia
         VS(MXOPER,MXTIP)        ! Veh�culos est�ndar

      DIMENSION                &
         COPDIS(MXOPER,MXTIP), & ! Costo de operaci�n por distancia
         PENLACE(MXOPER,MXTIP),& ! Penalizaci�n por tipo-enlace/operador
         NUMTIP(MXTIP),        & ! N�mero externo del tipo de enlace
         NOMTIP(MXTIP),        & ! Nombre del tipo de enlace
         VELTIP(MXTIP),        & ! Velocidad de referencia a flujo libre
         IAD(MXTIP),           & ! Administrador por tipo de v�a
         CMINMAN(MXTIP),       & ! Costo m�nimo de mantenim por tipo enlac
         ALFA(MXTIP),          & ! Par�metros restricci�n de capacidad
         GAMA(MXTIP),          & ! Par�metros restricci�n de capacidad
         REDVELMAX(MXTIP),     & ! Restriccion de capacidad: % velocidad en GAMA
         FFCAP(MXTIP)            ! Factor de capacidad de las v�as

      DIMENSION                &
         CMANMAR(MXOPER,MXTIP),   & ! Costo mantnm marginal por oper y tip enl
         OPTARIFF(0:MXOPER,MXOPER), & ! Tarifa de abordaje (tambien integradas)
         COSMIN(0:MXOPER,MXOPER),   & ! Costo minimo de transferencia
         DREL(MXOPER),         & ! Tarifa marginal de cada tramo tarifario
         MODO(MXPROP,MXMOD),   & ! Modos que puede usar una CategTransp
         ITIPOP(MXOPER),       & ! Tipo de oper (1libre 2p�bl 3ruta 4peat)
         CONSTM(MXOPER),       & ! Constante penalizaci�n
         COSTIEM(MXOPER),      & ! Tarifa por unidad de tiempo
         BoardTariff(MXOPER),  & ! Tarifa de abordaje del operador
         TOC(MXOPER)             ! Tasa de ocupaci�n

      DIMENSION                &
         PAREN1(MXOPER),       & ! Par�metros de consumo de energ�a
         PAREN2(MXOPER),       & ! Par�metros de consumo de energ�a
         PAREN3(MXOPER),       & ! Par�metros de consumo de energ�a
         PARCON(MXOPER),       & ! Par�metro consolidaci�n de carga
         COPMIN(MXOPER),       & ! Costo m�nimo de operaci�n
         COPTIE(MXOPER),       & ! Costo de operaci�n por tiempo
         NUMOP(MXOPER),        & ! N�mero externo de los operadores
         NOMOP(MXOPER),        & ! Nombre de los operadores
         COSTEN(MXOPER),       & ! Precio unitrario del combustible
         ENERQUIV(MXOPER),     & ! Transforma a unidades energ equivalnts
         VTARCOP(MXOPER),      & ! Transforma el costo de op en tarifa vehiculo
         TARCOP(MXOPER),       & ! Transforma el costo de op en tarifa
         ESPMIN(MXOPER),       & ! Espera m�nima y m�xima
         ESPMAX(MXOPER),       & ! Espera m�nima y m�xima
         FACTIEM(MXOPER),      & ! Factor entre dato frec y tiempo TRANS
         MODOPER(MXOPER),      & ! Modo al que pertence cada operador
         OPERPathASC(MXOPER)     ! Constante aditiva solo para PASOS

      DIMENSION                &
         NUMAD(MXADM),         & ! N�mero externo de los administradores
         NOMAD(MXADM),         & ! Nombre de los administradores
         VOTME(MXMOD),         & ! Promedio modal del valor tiempo espera
         VOTMV(MXMOD),         & ! Promedio modal del valor tiempo viaje
         VOTV(MXPROP),         & ! Valor del tiempo de viaje
         VOTE(MXPROP),         & ! Valor del tiempo de espera
         NUMCAT(MXPROP),       & ! N�mero externo de las CategTransp
         NOMCAT(MXPROP),       & ! Nombre externo de las CategTransp
         PCAU(MXPROP),         & ! Proporci�n de cautividad TranspP�blico
         DELTA(MXPROP),        & ! Par�metro elasticidad generaci�n
         GENMIN(MXPROP),       & ! Tasa generaci�n m�nima
         GENMAX(MXPROP),       & ! Tasa generaci�n m�xima
         LAMB(MXPROP),         & ! Par�metro separaci�n modal
         MODELgSc(MXPROP),     & ! Uso de escalamiento en logit
         LAMAS(MXPROP)           ! Par�metro asignaci�n

      DIMENSION                &
         PATHLgSc(MXMOD),      & ! Escalamiento en Logit 0..1
         NPAS(MXMOD),          & ! N�mero m�ximo de pasos por modo
         OzMode(MXMOD),        & ! Factor de overlapping en los pasos
         OzLtOper(MXOPER,MXTIP), & ! Oz factor por tipo de v�a y operador
         NOP(MXMOD),           & ! N�mero de operadores por modo
         NUMMOD(MXMOD),        & ! N�mero externo de cada modo
         NOMMOD(MXMOD+1),      & ! Nombre de cada modo
         LPUB(MXMOD),          & ! .T. si el modo tiene opers p�blico
         MODE_ASC(MXMOD),      & ! Alternative specific constant for modes
         NUMRUT(MXRUT),        & ! N�mero externo de cada ruta
         NOMRUT(MXRUT),        & ! Nombre de cada ruta
         IOPRUT(MXRUT),        & ! Operador a que pertenece cada ruta
         FreqMin(MXRUT),       & ! Frecuencia m�nima de cada ruta
         FreqMax(MXRUT),       & ! Frecuencia m�xima de cada ruta
         FREQNew(MXRUT),       & ! Frecuencia calculada
         TOCPromedio(MXRUT),   & ! Tasa de ocupacion promedio
         FleetMax(MXRUT)         ! Flota m�xima de la ruta

      real :: &
         CatTariffFactor(MXPROP, MXOPER), & ! Factor de tarifa por cat/oper
         CatPenalFactor(MXPROP, MXOPER),  & ! Factor de penalizacion por cat/oper
         OpCatASC(MXPROP, MXOPER)           ! Constante aditiva solo para TRANUS

      logical(1) ::             &
         RutScheduled(MXRUT)     ! TRUE si la ruta tiene horario

      INTEGER ::               &
         NOPER,                & ! N�mero total de operadores
         NTM,                  & ! N�mero total de modos
         NRUTEN,               & ! N�mero de rutas por enlace
         NRUTAS,               & ! N�mero total de rutas
         NTIP,                 & ! N�mero de tipos de v�a
         NPROP,                & ! N�mero de categor�as de transporte
         NIT,                  & ! N�mero iteraciones
         ADMIN                   ! N�mero de administradores
      real :: CONV              ! Convergencia

       real :: TransDampFactor = 1

CONTAINS

SUBROUTINE RDTPAR(IUN,status,ITER,IDIA,MES,IAN,IHR,MINS)
      integer   iun, status, iter
      integer(2) ::  idia, mes, ian, ihr, mins

        integer(2) :: lmayor,lmenor,lrev
        integer    :: fmt
        character(3) ::  spol
        character(80) :: snombre
        integer      mcheck,check,irec,inum
        integer(2) ::     ReadVerMajor, ReadVerMinor, ReadRevision, ReadRevMinor
        integer      i,j

      status = msg_ReadError

      ! Encabezamiento
      ! Datos de la version de TRANUS en VERSION.FD
      READ(IUN) lmayor,lmenor,lrev,fmt

      IF (lmayor.ne.FileMajor.or.lmenor.ne.FileMinor.or.lrev.ne.FileRelease) THEN
100     FORMAT('   v',I1,2('.',I2.2))
        WRITE(*,100)lmayor,lmenor,lrev
        status = msg_IncorrectFileVersion
        return
      ENDIF
      READ(IUN) ReadVerMajor, ReadVerMinor, ReadRevision, ReadRevMinor
200   FORMAT(' Produced by v',I1,2('.',I2.2))

      READ(IUN,ERR=999)ITER,IAN,MES,IDIA,IHR,MINS

      READ(IUN,ERR=999)AREA,ESTUDIO,SPOL,SNOMBRE

      ! Escenarios
      call SkipPolInfo(iun)

      ! Categorias
         status = msg_ErrorReadingCategories
      READ(IUN,ERR=999) &
       NPROP,mcheck, &
       (irec,NUMCAT(I),NOMCAT(I),VOTV(I),VOTE(I), &
       PCAU(I),GENMIN(I),GENMAX(I),DELTA(I), &
       LAMB(I), ModeLgSc(I), LAMAS(I), PathLgSc(I), &
       I=1,NPROP), &
       check
      call checkRead(nprop,mcheck,check, status)

      ! Administradores
      status = msg_ErrorReadingAdmins
      READ(IUN,ERR=999) &
       ADMIN,mcheck, &
      (irec,NUMAD(I),NOMAD(I),I=1,ADMIN), &
       check
      call checkRead(admin,mcheck,check, status)

      ! Modos
         status = msg_ErrorReadingModes
      READ(IUN,ERR=999) &
        NTM, mcheck, &
        (irec,NUMMOD(I),NOMMOD(I),VOTMV(I),VOTME(I), &
        NPAS(I),OzMode(I),LPUB(I), &
        MODE_ASC(I), &
        (irec,inum,MODO(J,I),J=1,NPROP), &
        I=1,NTM), &
        check
      call checkRead(ntm,mcheck,check, status)

      ! Operadores
         status = msg_ErrorReadingOperators
      READ(IUN,ERR=999) &
        NOPER,mcheck, &
        (irec,NUMOP(I),NOMOP(I),MODOPER(I), &
        ITIPOP(I),TOC(I),DREL(I), &
        BoardTariff(I),           &
        CONSTM(I),PAREN1(I),PAREN2(I),PAREN3(I),COSTEN(I),VTARCOP(I), &
        COPTIE(I),COSTIEM(I), ESPMIN(I),ESPMAX(I),COPMIN(I), &
        PARCON(I),FACTIEM(I), &
        OPERPathASC(I), &
        mcheck, mcheck,          &
          ( irec, mcheck,          &
            CatTariffFactor(J, I), &
            CatPenalFactor(J, I),  &
            OpCatASC(J,I),         &
          J=1, NPROP),             &
          mcheck,                  &
        I=1,NOPER), &
        check

        NOP = 0
        do i=1, NOPER
           ModOper(I)      = IntNum(ModOper(I), NumMod, NTM)
           NOP(MODOPER(I)) = NOP(MODOPER(I))+1
           TARCOP(I)       = VTARCOP(I)/TOC(I)
        enddo

      READ(IUN,ERR=999) &
        NOPER,mcheck, &
        ( &
        check, inum, (inum, OPTARIFF(I,J), J=1, NOPER), &
        I=1,NOPER), &
        check
      call checkRead(noper,mcheck,check, status)
      do i=0,NOPER
         do j = 1, NOPER
            COSMIN(I,J) = OPTARIFF(I,J)+COPMIN(J)*TARCOP(J)
         enddo
      enddo


      ! Tipos de via
         status = msg_ErrorReadingLinkTypes
      READ(IUN,ERR=999) &
       NTIP,mcheck, &
       (irec,NUMTIP(I),NOMTIP(I),IAD(I),VELTIP(I), &
       CMINMAN(I), ALFA(I),GAMA(I), FFCAP(I), REDVELMAX(I), &
       (  irec, &
          inum, &
          SPEED(J,I),VS(J,I),CARGOS(J,I),COPDIS(J,I),PENLACE(J,I), &
    !&    OzLtOper(J,I),
          J=1,NOPER), &
       I=1,NTIP), &
       check
      call checkRead(ntip,mcheck,check, status)
      do i=1,NTIP
         IAd(i) = IntNum(IAd(i),NumAd, ADMIN)
      enddo



      ! Rutas
      status = msg_ErrorReadingRoutes
      FleetMax = 0
      READ(IUN,ERR=999)&
       NRUTAS,mcheck, &
       (irec, &
       NUMRUT(I),NOMRUT(I),IOPRUT(I), &
       FreqMin(I), FreqMax(I), FREQNew(I),  &
       TOCPromedio(I), FleetMax(I),  &
       RutScheduled(I), &
       I=1,NRUTAS), &
       check
      call checkRead(nrutas,mcheck,check, status)

       do i=1, NRUTAS
          IOpRut(i) = IntNum(IOpRut(i), NumOp, NOPER)
       enddo


      ! Zonas
         status = msg_ErrorReadingZones
      READ(IUN,ERR=999) &
       NZN,mcheck, &
       NZ1,NZ2, &
       (irec,NUMZON(I),NOMZON(I),JER1(I),JER2(I),I=1,NZN), &
       check
      call checkRead(nzn,mcheck,check, status)
      call ZonSubZon() ! en GENER.FOR

      status = msg_Ok
999   RETURN
END SUBROUTINE



SUBROUTINE WRTPAR(IUN,status,ITER, day, month, year, hour, minute)
        integer   iun, status, iter
        integer, intent(in), value :: day, month, year, hour, minute
        integer(2) ::  idia, mes, ian, ihr, mins

        idia = day
        mes  = month
        ian  = year
        ihr  = hour
        mins = minute

!  Graba el primer registro con los par�metros de transporte en unidad IUN

      status = msg_WriteError
      ! Encabezamiento
      ! Datos de la version de TRANUS en VERSION.FD
      WRITE(IUN,ERR=999) FileMajor, FileMinor, FileRelease, ifmt_P0S
      WRITE(IUN,ERR=999) vermajor, verminor, revision, revminor
      WRITE(IUN,ERR=999) ITER,IAN,MES,IDIA,IHR,MINS
      WRITE(IUN,ERR=999) AREA,ESTUDIO,POL,NOMBRE

      ! Escenarios
      call WritePolInfo(iun)

      ! Categorias
      WRITE(IUN,ERR=999) &
      NPROP,-NPROP, &
      (I,NUMCAT(I),NOMCAT(I),VOTV(I),VOTE(I), &
      PCAU(I),GENMIN(I),GENMAX(I),DELTA(I), &
      LAMB(I), ModeLgSc(I), LAMAS(I), PathLgSc(I), &
      I=1,NPROP), &
      NPROP

      ! Administradores
      WRITE(IUN,ERR=999) &
       ADMIN,-ADMIN, &
       (I,NUMAD(I),NOMAD(I),I=1,ADMIN), &
       ADMIN 

      ! Modos
      WRITE(IUN,ERR=999) &
        NTM,-NTM, &
        (I,NUMMOD(I),NOMMOD(I),VOTMV(I),VOTME(I), &
        NPAS(I),OzMode(I),LPUB(I), &
        MODE_ASC(I), &
        (J,NumCat(J),MODO(J,I),J=1,NPROP), &
        I=1,NTM), &
        NTM

      ! Operadores
      WRITE(IUN,ERR=999) &                                        
        NOPER,-NOPER, &
        (I,NUMOP(I),NOMOP(I),NumMod(MODOPER(I)), &
        ITIPOP(I),TOC(I),DREL(I), &
        BoardTariff(I),           &
        CONSTM(I),PAREN1(I),PAREN2(I),PAREN3(I),COSTEN(I),VTARCOP(I), &
        COPTIE(I),COSTIEM(I), ESPMIN(I),ESPMAX(I),COPMIN(I), &
        PARCON(I),FACTIEM(I), &
        OPERPathASC(I), &
          NPROP, -NPROP,           &
          ( J, NumCat(J),          &
            CatTariffFactor(J, I), &
            CatPenalFactor(J, I),  &
            OpCatASC(J,I),         &
          J=1, NPROP),             &
          NPROP,                   &
        I=1,NOPER), &
        NOPER

      WRITE(IUN,ERR=999) &
        NOPER,-NOPER, &
        ( &
        I,NumOp(I),(NumOp(J),OPTARIFF(I,J), J=1, NOPER), &
        I=1,NOPER), &
        NOPER 

      ! Tipos de via
      WRITE(IUN,ERR=999) &
       NTIP,-NTIP, &
       (I,NUMTIP(I),NOMTIP(I),NumAd(IAD(I)),VELTIP(I), &
        CMINMAN(I), ALFA(I),GAMA(I), FFCAP(I), REDVELMAX(I), & 
       (  J, &
          NumOp(J), &
          SPEED(J,I),VS(J,I),CARGOS(J,I),COPDIS(J,I),PENLACE(J,I), &
    !     OzLtOper(J,I), &
          J=1,NOPER), &
       I=1,NTIP), &
       NTIP

      ! Rutas
      WRITE(IUN,ERR=999) &
       NRUTAS,-NRUTAS, &
       (I, &
       NUMRUT(I),NOMRUT(I),NumOp(IOPRUT(I)), &
       FreqMin(I), FreqMax(I), FREQNew(I),   &
       TOCPromedio(I), FleetMax(I),  &
       RutScheduled(I), &
       I=1,NRUTAS), &
       NRUTAS

      ! Zonas
      WRITE(IUN,ERR=999) &
        NZN,-NZN, &
        NZ1,NZ2, &
        (I,NUMZON(I),NOMZON(I), JER1(I),JER2(I),I=1,NZN), &
        NZN

       status = msg_Ok

999   RETURN
END SUBROUTINE



END MODULE TPARC
