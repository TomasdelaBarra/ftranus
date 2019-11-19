! ************************************************************************
! * TRANUS - Integrated Land Use and Transport Model
! * 
! * $Id$
! * 
! * Copyright (C) 1983-2007 Modelistica, Caracas
! * Copyright (C) 1983-2007 Tomas de la Barra
! * Copyright (C) 1985-2010 Juancarlo Añez
! * Copyright (C) 1983-2002 Beatriz Perez
! * Some rights reserved.
! * 
! * (cc) This work is distrubuted under a Creative Commons
! *      Attribution-ShareAlike 2.0 license
! *      http://creativecommons.org/licenses/by-sa/2.0/
! ************************************************************************
PROGRAM IMPLOC
USE DEBUGM
USE GETOPTM
USE PARAM
USE GENER
USE MENSAMOD
USE CONTROL
USE IO_LIST
USE MLOGIT
USE ZCOMM
USE LPARC
USE LCOMM

     character(80) :: IMPLOC_RCS_ID = &
        "$Id$"

      integer, parameter :: MXOPT = 7 

      CHARACTER SALIDA*32,IIP*1,STR1*9,SI*1
      logical(1) :: IMP(MXOPT),LSEC(MXSEC), SecInteres(MXSEC)
      REAL MATRIZ(MXZON,MXRUN), MAT(mxzon,mxsec)
      real(8) ::  consUnitario, consTotal, demPotencial
      CHARACTER*3 PERS(MXRUN)
      CHARACTER*8 NOMAUX(MXSEC)
      DIMENSION IAUX(MXSEC), AUXMIN(MXSEC),AUXMAX(MXSEC),AUXELS(MXSEC),TOTCONSU(MXSEC)
      INTEGER   IPOL
      integer :: IAN,MES,IDIA,IHR,MIN,ISEC,MILESM

      character*(*), parameter :: IMPLOC_OPTS = 'SJPQICTo:p:s:' // STD_OPTIONS
      external Usage

      call debug('About to Init')
      CALL INIT(POL,IIP,.FALSE.,1007, Usage, IMPLOC_OPTS)
      call debug('About to InitPol')
      call InitPol(POL,IPOL)

      CALL GetCurrentDate(IAN,MES,IDIA)
      CALL GetCurrentTime(IHR,MIN,ISEC,MILESM)

      SI=' '

!  Lee las opciones de la pantalla

      if (.not. hasOpts()) then
         IMP=.FALSE.
         WRITE(*,'(///)')
         CALL MENSA(1033,0)  ! Opciones de impresi¢n:
40       CALL MENSA(7,1)  !      Opci¢n --->
         READ(*,'(I2)')IPP
         IF(IPP.lt.1.or.IPP.gt.MXOPT)GO TO 40
         IMP(IPP)=.TRUE.
      endif

!  LECTURA ARCHIVO DE ZONAS

      call OpenPolFile(3, 0, 'Z1E', IO_FMT)
      CALL LEEZ1E
      CLOSE(3)
!
!  LECTURA DE PARAMETROS L1E

      if (debugging >= dbg_Normal) then
        CALL MENSA(1004,0)  ! LECTURA DE PARAMETROS Y DATOS
      endif
      call OpenPolFile(3, IPOL, 'L1E', IO_FMT)
      CALL LEEL1E
      CLOSE(3)

      PERS='   '
      PERS(1)=POL
      NPERS=1

      SecInteres = .FALSE.
      DO I=1,NS ! Sectores consumidos
         DO J=1,NS ! sectores consumidores
            IF(demax(I,J).gt.0)THEN
               SecInteres(I)=.TRUE.
               exit
            ENDIF
         ENDDO ! J consumidores
      ENDDO ! I consumidos
      SI=CHAR(15)
      call PolFileName(IPOL, 'LOC', SALIDA)
      if (getOptions(SALIDA)) then
         OPEN(2,FILE=SALIDA,STATUS='UNKNOWN')
      else
         CALL SALI(SALIDA)
      endif
      IF(SALIDA(1:3).EQ.'PRN')WRITE(2,'('' '',A)')SI

      !  Si la opci¢n es 3 (produccion en varios periodos) pregunta por los periodos y sectores

      IF(IMP(3) .and. .not. hasOpts())THEN
         WRITE(*,'(///)')
         CALL MENSA(1034,0)  ! Ingrese lista de per¡odos
         WRITE(*,*)'Max=',MXRUN
         READ(*,'(6(A,1X))')(PERS(I),I=2,MXRUN)
         DO I=2,MXRUN
            IF(PERS(I).NE.'   ')THEN
               NPERS=NPERS+1
               CALL UPPER(PERS(I))
            ENDIF
         ENDDO
         LSEC=.FALSE.
14       WRITE(*,'(///)')
         NSECAG=ICERO  ! No de sectores que se agregan
         DO I=1,NS
            WRITE(*,*)NUMSEC(I),' ',NOMSEC(I)
         ENDDO
         CALL MENSA(1035,0)  ! Lista de sectores a sumar
         READ(*,*)(IAUX(I),I=1,MXSEC)
         DO I=1,MXSEC
            IF(IAUX(I).EQ.ICERO)EXIT
            I2=INTNUM(IAUX(I),NUMSEC,NS)
            IF(I2.GT.NS)THEN
               WRITE(*,*)'Sect:',IAUX(I)
               CALL MENSA(6,0)  ! G04 Definici¢n ilegal
               GO TO 14
            ENDIF
            LSEC(I2)=.TRUE.
            NSECAG=NSECAG+1
         ENDDO
      ENDIF

!  LECTURA DE DATOS DEL L1S QUE SE DESEA IMPRIMIR

      MATRIZ=RCERO
      DO IP=1,NPERS
         call FindPol(PERS(IP), NP)
         call OpenPolFile(3, NP, 'L1S', IO_BIN)
         CALL LEEL1S(3,ITER, .TRUE., idia, mes, ian, ihr, min)
         CLOSE(3)
         IF(IMP(3))THEN
            DO I=1,NZN
               DO IS=1,NS
                  IF(LSEC(IS))THEN
                     MATRIZ(I,IP)=MATRIZ(I,IP)+XPRO(I,IS)+PRO(I,IS)
                  ENDIF
               ENDDO
            ENDDO
         ENDIF
      ENDDO  ! Fin do per¡odos IP


    IF(IMP(1).or.IMP(7))THEN
        call OpenPolFile(3, IPOL, 'L1S', IO_BIN)
        call LEEL1S(3,ITER, .TRUE., idia, mes, ian, ihr, min)
        close(3)
    endif

    if (.not. suppressHeaders .and..not. IMP(7)) then
!  Write output header
      WRITE(2,*)'_________________________________________'
      WRITE(2,*)'T R A N U S                (c)MODELISTICA'
      CALL MENSA(1007,2)
      WRITE(2,*)'_________________________________________'
      WRITE(2,5)ESTUDIO,NOMBRE
5     FORMAT(' ',A30,3X,'POLITICA: ',A18)
      STR1='   ITER '
7     FORMAT(A9,A)
      CALL MENSA(1010,2)
      CALL MENSA(8004,2)
      WRITE(2,8)AREA,POL,IDIA,MES,IAN,IHR,MIN,STR1,ITER
8     FORMAT('  ',A3,5X,A,8X,I2,'-',I2,'-',I4,1X,I6,':',I2,A8,I5)
    endif

    if (IMP(7)) then
        call AllInformationDelimited(2, POL)
    endif

    if (IMP(1)) then
       call AllInformation(2)
    endif

!  OPCION 2 - Producci¢n Total por sector y zona

      IF(IMP(2))THEN

      WRITE(2,'(70(1H-))')
      WRITE(2,'(''  Zon'',100(A,A9))')(TAB,NOMSEC(I),I=1,NS)
      WRITE(2,'(70(1H-))')
      TPRO=RCERO
      DO I=1,NZN
         WRITE(2,'(I5,100(A,G18.9))')NUMZON(I),(TAB,XPRO(I,N)+PRO(I,N),N=1,NS)
         IF(I.GT.NZ1)CYCLE
         DO N=1,NS
            TPRO(N)=TPRO(N)+XPRO(I,N)+PRO(I,N)
         ENDDO
      ENDDO ! Fin do zonas I
      WRITE(2,'(70(1H-))')
      WRITE(2,'(''  TOT'',100(A,G18.9))')(TAB,TPRO(N),N=1,NS)
      WRITE(2,'(70(1H-))')
      WRITE(2,*)

      ENDIF ! Fin Opci¢n 2

!  OPCION 3 - Producci¢n total por per¡odo

      IF(IMP(3))THEN

      TPRO=RCERO
      WRITE(2,*)
      WRITE(2,'(70(1H-))')
      WRITE(2,'('' Sects:'',100(I4,1X,A))')(IAUX(I),NOMSEC(IAUX(I)),I=1,NSECAG)
      WRITE(2,'(70(1H-))')
      WRITE(2,'('' Zon'',6(A,6X,A))')(TAB,PERS(I),I=1,NPERS)
      DO I=1,NZN
         WRITE(2,'(I5,6(A,G18.9))')NUMZON(I),(TAB,MATRIZ(I,J),J=1,NPERS)
         IF(I.GT.NZ1)CYCLE
         DO J=1,NPERS
            TPRO(J)=TPRO(J)+MATRIZ(I,J)
         ENDDO
      ENDDO
      WRITE(2,'(70(1H-))')
      WRITE(2,'(''  TOT'',100(A,G18.9))')(TAB,TPRO(N),N=1,NPERS)
      WRITE(2,'(70(1H-))')
      WRITE(2,*)

      ENDIF  ! Fin opci¢n 3

! OPCION 4 - Toda la informaci¢n por sector y zona + variables internas

      IF(IMP(4))THEN

      DO N=1,NS
         WRITE(2,'(70(1H-))')
         WRITE(2,'('' Sect'',I4,1X,A)')NUMSEC(N),NOMSEC(N)
         WRITE(2,'(70(1H-))')
         IF(TAB.EQ.' ')THEN
            CALL MENSA(1017,2)
         ELSE
            CALL MENSA(1018,2)
         ENDIF
         TTPRO=RCERO
         TTDEM=RCERO
         DO I=1,NZN
            WRITE(2,'(I5,2(A,G18.9),4(A,G18.9),A,F6.1,''%'',3(A,G18.9))') &
              NUMZON(I),TAB,XPRO(I,N)+PRO(I,N), &
              TAB,XDEM(I,N)+DEM(I,N),TAB,COSPRO(I,N),TAB,PRECIO(I,N), &
              TAB,RMIN(I,N),TAB,RMAX(I,N),TAB,100*AJUSTE(I,N)/PRECIO(I,N),TAB, &
              COSCON(I,N),TAB,UTCON(I,N),TAB,ATRAC(I,N)
            IF(I.GT.NZ1)CYCLE
            TTPRO=TTPRO+XPRO(I,N)+PRO(I,N)
            TTDEM=TTDEM+XDEM(I,N)+DEM(I,N)
         ENDDO ! I
         WRITE(2,'(70(1H-))')
         WRITE(2,'(''  TOT'',2(A,G18.9))')TAB,TTPRO,TAB,TTDEM
         WRITE(2,'(70(1H-))')
         WRITE(2,*)
      ENDDO ! N

      ENDIF ! Fin Opci¢n 4

! OPCIONES 5 y 6 - Consumo unitario o total de los sectores con demanda elastica

      IF(IMP(5).or. IMP(6))THEN

      DO M=1,NS
         MAT=0.
         if(.not.SecInteres(M))cycle
         WRITE(2,*)
         WRITE(2,'(70(1H-))')
         WRITE(2,'('' Sect'',I4,1X,A)')NUMSEC(M),NOMSEC(M)
         WRITE(2,'(70(1H-))')
         WRITE(2,*)
         TOTCONSU=0.
         DO I=1,NZN
           if (conSubZonas(i)) CYCLE
           if(.not.ZExter(i)) then
                NN=0
                DO N=1,NS
                   if(demax(m,n).le.0.) CYCLE
                   NN=NN+1
                   NOMAUX(NN)=NOMSEC(N)
                   AUXMIN(NN)=DEMIN(M,N)
                   AUXMAX(NN)=DEMAX(M,N)+DEMIN(M,N)
                   AUXELS(NN)=DELAS(M,N)
   
                   CALL CONSUMO(I,M,N,consUnitario, demPotencial)
                   consTotal = MAX(1.d+0*STOCK(I,N),PRO(I,N)+XPRO(I,N))*consUnitario

                   IF (IMP(5))THEN
                      MAT(I,NN)= demPotencial
                  ELSE
                     MAT(I,NN)= consTotal
                     TOTCONSU(NN)=TOTCONSU(NN)+consTotal
                  ENDIF
                enddo ! N sectores consumidores
           endif ! zonas internas
         enddo ! zonas I

! Escribe el nombre de los sectores y la matriz
     IF(NN.GT.0)THEN
         WRITE(2,'('' Zon'',40(A,1X,A10))')(TAB,NOMAUX(J),J=1,NN)
       IF(IMP(5))THEN
            WRITE(2,'('' Min'',40(A,1X,G18.9))')(TAB,AUXMIN(J),J=1,NN)
            WRITE(2,'('' Max'',40(A,1X,G18.9))')(TAB,AUXMAX(J),J=1,NN)
            WRITE(2,'('' Els'',40(A,1X,G18.9))')(TAB,AUXELS(J),J=1,NN)
       ENDIF
         WRITE(2,'(70(1H-))')
         DO I=1,NZN
           if (conSubZonas(i)) CYCLE
           if(.not.ZExter(i)) then
            if(IMP(5))then
                WRITE(2,'(I5,40(A,G18.9))') NUMZON(I),(TAB,MAT(I,J),J=1,NN)
           else
             WRITE(2,'(I5,40(A,G18.9))') NUMZON(I),(TAB,MAT(I,J),J=1,NN)
           endif ! opciones 5 o 6
           endif ! zonas internas
         enddo ! zonas I
       IF(IMP(6))THEN
           WRITE(2,'(70(1H-))')
          WRITE(2,'(5HTOTAL,40(A,G18.9))') (TAB,TOTCONSU(J),J=1,NN)
        ENDIF
     ENDIF
      enddo ! M sectores consumidos

         WRITE(2,'(70(1H-))')
         WRITE(2,*)

      ENDIF ! Fin Opci¢n 5

      SI=CHAR(18)
      IF(SALIDA(1:3).EQ.'PRN')WRITE(2,1550)SI
1550  FORMAT(' ',A1/'1')
1551  if (debugging >= dbg_Normal) then
        CALL MENSA(8,0)  ! FINAL NORMAL DE
        WRITE(*,*)' I M P L O C'
      endif
      STOP 0
10    WRITE(*,*) SALIDA
      CALL MENSA(0003,-1) !

CONTAINS

subroutine AllInformation(output)
    integer, intent(in) :: output
      DO N=1,NS
         write(output,'(70(1H-))')
         write(output,'('' Sect'',I4,1X,A)')NUMSEC(N),NOMSEC(N)
         write(output,'(70(1H-))')

905      FORMAT(A5,8(A,A),'%')
         write(output,905) &
         'Zone', TAB, 'TotProd', TAB,'TotDem',TAB,'ProdCost',TAB,   &
         'Price',TAB,'Supply',TAB,'Stock',TAB, 'Unstock', TAB,'Adjust'

         TTPRO=RCERO
         TTDEM=RCERO
         DO I=1,NZN
            AJ = AJUSTE(I,N)
            if (PRECIO(I,N).ne.0) then
               AJ = 100.*AJ/PRECIO(I,N)
            elseif (AJ >= 0) then
               AJ = 999.9
            else
               AJ = 999.9 
            endif
910         FORMAT(I5,7(A,G18.9),A,F6.1,'%')
920         FORMAT(I5,2(A,G18.9),2(A,G18.9),A,G18.9,A,G18.9)
            write(output,910)&
              NUMZON(I),TAB,XPRO(I,N)+PRO(I,N), &
              TAB,XDEM(I,N)+DEM(I,N),TAB,COSPRO(I,N),TAB,PRECIO(I,N), &
              TAB,RMAX(I,N),TAB, STOCK(I,N), TAB, UNSTOCK(I,N), TAB,AJ
            IF(I.GT.NZ1)CYCLE
            TTPRO=TTPRO+XPRO(I,N)+PRO(I,N)
            TTDEM=TTDEM+XDEM(I,N)+DEM(I,N)
         ENDDO ! I
         write(output,'(70(1H-))')
         write(output,'(''  TOT'',2(A,G18.9))')TAB,TTPRO,TAB,TTDEM
         write(output,'(70(1H-))')
         write(output,*)
      ENDDO ! N
end subroutine 

subroutine AllInformationDelimited(output, spol)
    integer, intent(in) :: output
    character(*), intent(in) :: spol
    
    300 format(A,9(', ', A))
    write(output,300) &
         'Scen', 'Sector', 'Zone', &
         'TotProd', 'TotDem', &
         'ProdCost', 'Price', &
         'MinRes', 'MaxRes', &
         'Adjust'

    do n=1, NS
        do I=1,NZN
           AJ = AJUSTE(I,N)
           if (PRECIO(I,N).ne.0) then
              AJ = 100.*AJ/PRECIO(I,N)
           elseif (AJ >= 0) then
              AJ = 999.9
           else
              AJ = 999.9 
           endif
400        format( A, 2(', ', A, X, A), 7(', ',E24.16))
           write(output,400) &
              trim(POL), &
              trim(IntegerToString(NUMSEC(N))), trim(NOMSEC(N)), &
              trim(IntegerToString(NUMZON(I))), trim(NOMZON(I)), &
              XPRO(I,N)+PRO(I,N), XDEM(I,N)+DEM(I,N), &
              COSPRO(I,N),PRECIO(I,N), &
              RMIN(I,N),RMAX(I,N), &
              AJ
         enddo
     enddo
end subroutine 

logical function GetOptions(SAL)
 character SAL*(*)
      integer i, INSEC, n, is

      getOptions = .false.
      if (.not. hasOpts()) then
          return
      endif
      getOptions = .true.

      INSEC   = 0
      IMP = .false.
      LSEC    = .false.

      do i = 1, optc()
          select case(optv(i))
              case ('I')
                 IMP(1) = .true.
              case ('P')
                 IMP(2) = .true.
              case ('Q')
                 IMP(3) = .true.
              case ('S')
                 IMP(4) = .true.
              case ('C')
                 IMP(5) = .true.
              case ('T')
                 IMP(6) = .true.
              case ('J')
                 IMP(7) = .true.
              case ('o')
                 SAL=trim(optarg(i))
                 if (SAL == '-') then
                    SAL = 'CON'
                 else
                    suppressHeaders = .true.
                 endif
              case ('p')
                NPERS=NPERS+1
                if(NPERS > MXRUN) call fail('Too many scenarios.')
                PERS(NPERS) = trim(optarg(i))
                call Upper(PERS(NPERS))
                call verbose('Including scenario ' // PERS(NPERS))
              case ('s')
                n = ioptarg(i)
                is = iFindNum(n, NumSec, NS)
                if (is == 0) then
                  print '(A,I4,$)', 'Sector ', n
                  call fail('not found.')
                endif
                INSEC = INSEC+1
                if(INSEC > MXSEC) call fail('Too many sectors.')
                LSEC(INSEC) = n /= 0
                SecInteres(INSEC) = .true.
                call verbose('Including sector' // trim(optarg(i)) // ' ' // trim(NomSec(is)))
          end select
      enddo
 return
 end function

END PROGRAM IMPLOC

 subroutine Usage
 USE GETOPTM
 character*32 prog
    prog = argv(0)

    print *
    print '(A,'' - Assignment reports for TRANUS(r)'')', trim(prog)

    print *,'usage:'

    print '(4X, A,''  <scen> <command> [options...]'')', trim(prog)
    print '(4X, A,''  <scen> -S [options...]'')', trim(prog)
    print '(4X, A,''  <scen> -P [options...]'')', trim(prog)
    print '(4X, A,''  <scen> -Q -p <scen>... -s <id>... [options...]'')', trim(prog)
    print '(4X, A,''  <scen> -I [options...]'')', trim(prog)
    print '(4X, A,''  <scen> -C [options...]'')', trim(prog)
    print '(4X, A,''  <scen> -T [options...]'')', trim(prog)

    print *, 'If no commands are given, the program enters interactive mode'
    print *
    print *, 'Commands are:'
    print *, '  -I          : All information by sector and zone'
    print *, '  -J          : All information, comma-delimited'
    print *, '  -P          : Total production by sector and zone'
    print *, '  -Q          : Total production by year/policy'
    print *, '  -S          : Internal information by sector and zone'
    print *, '  -C          : Consumption coeficients by sector'
    print *, '  -T          : Total consumption by sector and zone'
    print *

    print *
    print *, 'Options are:'
    print *, '  -o <name>      : Write reports to file <name>. Default is "AREAPOL.ESP"'
    print *, '  -o -           : Write reports to standard output.'
    print *, '  -p <scen>      : Include scenario <scen> in report.'
    print *, '  -s <id>        : Include sector <id> in report.'
    call ExplainStdOptions

    STOP 02
 end subroutine
