! ************************************************************************
! * TRANUS - Integrated Land Use and Transport Model
! * 
! * $Id$
! * 
! * Copyright (C) 1983-2019 Modelistica, Caracas
! * Copyright (C) 1983-2019 Tomas de la Barra
! * Copyright (C) 1985-2007 Juancarlo Añez
! * Copyright (C) 1983-2002 Beatriz Perez
! * Some rights reserved.
! * 
! * (cc) This work is distrubuted under a Creative Commons
! *      Attribution-ShareAlike 2.0 license
! *      http://creativecommons.org/licenses/by-sa/2.0/
! ************************************************************************
PROGRAM IMPTRA  ! Outputs text reports from the results of the transport model
USE DEBUGM
USE GETOPTM
USE PARAM
USE RCOMM
USE TCOMM
USE IPCOMM
USE TPARC
USE CONTROL
USE GENER
USE IO_LIST
USE ASCII
USE TINDICS

      character(80) :: IMPTRA_RCS_ID = &
         "$Id$" 

      integer, parameter :: NIMPOP = 11

      logical(1) :: IMPTIP,IMPOP,LRUTAS(MXRUT)
      COMMON NODORIG(NLMAX),NODODES(NLMAX),IMPTIP(MXTIP),IMPOP(NIMPOP)

      REAL SpeedMin(20), SpeedMax(20)
      REAL DCOP(MXLNRUT),ESPERIN(MXLNRUT),VEHSTOP(MXLNRUT),ENEROUTE(MXLNRUT)
      REAL CAPOP(MXLNRUT),VELOP(MXLNRUT)
      COMMON /IMPT0/ DCOP,ESPERIN,VEHSTOP,CAPOP,VELOP
      INTEGER      IPOL, STATUS
	  INTEGER      RUTNUM
      CHARACTER*32 SALIDA,AUX
      CHARACTER*1 ZZ,IIP
	  CHARACTER*10 NAMELINK
	  CHARACTER*6 IORTXT
	  CHARACTER*6 IDESTXT
      LOGICAL   NE
      integer(4) :: CV,INF4
      integer(2) :: IDIA,MES,IAN,IHR,MINS
	  CHARACTER*15 IDLINK 
      character, parameter :: imptraOpts*(*) &
        = STD_OPTIONS // 'ALTDIJCRSPdf:l:t:o:r:w:'
      external Usage
      logical useImptraDat
      character*(1) OP

      INF4=999999
      CALL INIT(POL,IIP,.FALSE.,8060, Usage, imptraOpts)
      call InitPol(POL,IPOL)

!  Options to input report requests 

      useImptraDat = .false.
      if (.not. hasOpts()) then
          WRITE(*,*)
          WRITE(*,*)'__________________________________________'
          CALL MENSA(8061,0)  ! From screen or from file imptra.dat?
          WRITE(*,*)'___________________________________________'
539       CALL MENSA(7,1)
          READ(*,'(I2)')IP
          IF(IP.LT.0.OR.IP.GT.1)GO TO 539
          IF(IP.EQ.1) THEN
            INQUIRE(FILE='IMPTRA.DAT',EXIST=NE)
            IF(.NOT.NE)THEN
               WRITE(*,*)'IMPTRA.DAT'
               CALL MENSA(1,0)
               GO TO 539
            ENDIF
            useImptraDat = .true.
          ENDIF
      endif

!  READS ZONES FILE

!      call OpenPolFile(3, 0, 'Z1E', IO_FMT)
!      CALL LEEZ1E
!      CLOSE(3)

!  Reads file T3S for the transport parameters and assignment results link-by-link

      call OpenPolFile(3, IPOL, 'T3S', IO_BIN)
      CALL RDTPAR(3,status,ITER,IDIA,MES,IAN,IHR,MINS)
         call CheckStatus(status)
      CALL RDREDANT(3,0)
         call CheckStatus(status)
      CLOSE(3)


    SALIDA= trim(AREA) // trim(POL) // '.TRA'
    if (.not. getOptions(SALIDA)) then
      AUX='                                '
      WRITE(*,*)
      WRITE(*,*)'Suggested output file: ',SALIDA
      WRITE(*,*)'Enter to accept or type new name (up to 32 chars)'
      WRITE(*,*)
547   WRITE(*,'(''       ----->  ''$)')
        READ(*,'(A)')AUX
      IF(AUX(1:3).NE.'   ')SALIDA(1:32)=AUX(1:32)
        INQUIRE(FILE=SALIDA,EXIST=NE)
        IF(NE)THEN
            CALL MENSA(12,1)
            READ(*,'(A)')OP
            IF(OP.EQ.'N'.OR.OP.EQ.'n')GO TO 547
        ENDIF
    endif
    OPEN(2,FILE=SALIDA,STATUS='UNKNOWN')

    if (.not.suppressHeaders) then
      !  Outputs a header for the file
       WRITE(2,*)ESTUDIO
       CALL MENSA(8062,2)  ! Iteration   Area  Pol  date/time sim
       WRITE(2,'(I8,6X,A3,3X,A3,I4.2''-'',I2.2,''-'',2I4.2,'':'',I2.2,4X,2A)') &
               ITER,AREA,POL,IDIA,MES,IAN,IHR,MINS,ReleaseStr, ReleaseName
       WRITE(2,*)
    endif

!  Reads output options from file IMPTRA.DAT


      if(useImptraDat) then
        OPEN(3,FILE='IMPTRA.DAT')
        CALL MENU(VCMIN,VCMAX,IFOROP,SpeedMin,SpeedMax)
      elseif (.not. hasOpts()) then
!  Read output options  interactively from screen
        call info('ingresando a CONMEN')
        CALL CONMEN(VCMIN,VCMAX,IFOROP,LRUTAS)
      endif

! If option is 5 call the indicators routine

      IF(IMPOP(5))THEN
         call OpenPolFile(4, IPOL, 'T2S', IO_BIN)
         CALL INDICS(2, ipol)
         call IndicatorsByCategoryOperator(2, ipol)
         CLOSE(4)
      ENDIF

!  If option is 6, call the cordons routine

      IF(IMPOP(6))THEN
         CALL CORDON(AREA,POL,ITER,IAN,MES,IDIA,IHR,MINS,TAB)
      ENDIF

!  If option is 7, call the routine to output transit routes

      IF(IMPOP(7))THEN
         CALL REPORUT(AREA,POL,ITER,IAN,MES,IDIA,IHR,MINS,TAB,LRUTAS)
      ENDIF  
      
! If option is 8, call the routine to output vehicles-km by speed class

      IF (IMPOP(8))THEN
          CALL VEHSPEED(AREA,POL,ITER,IAN,MES,IDIA,IHR,MINS,TAB,SpeedMin,SpeedMax)
      ENDIF
	  
! If option is 9, call the routine to output route profiles	  

      IF (IMPOP(9))THEN
          CALL PerfilEnlaceRuta(2)
      ENDIF
	  
! If option is 10, call the routine to output route profiles in csv format	  

      IF (IMPOP(10))THEN
         call CSVRouteProfile(AREA,POL,ITER,IAN,MES,IDIA,IHR,MINS,TAB,LRUTAS)
      ENDIF
	  
! If option is 11, call the routine to output transport indicators	  

      if (IMPOP(11)) then
         call OpenPolFile(4, IPOL, 'T2S', IO_BIN)
         CALL INDICS(2, ipol, TRA_FMT_CSV)
         call IndicatorsByCategoryOperator(2, ipol, TRA_FMT_CSV)
         CLOSE(4)
      endif

!  OUTPUTS ASSIGNMENT RESULTS ACCORDING TO OPTIONS 1 TO 4

      NE=.TRUE.
      DO 532 I=1,4
532   IF(IMPOP(I))NE=.FALSE.
      IF(NE)GO TO 100

! Minimum output      
      IF(IFOROP.EQ.1)WRITE(2,*)'Id',',','Orig',',','Dest',',','Type', &
                     ',','Dist', &
                     ',','LinkCap',',','Dem/Cap',',','StVeh',',', &
                     'TotVeh',',','ServLev'
					 
! Medium output					 
      IF(IFOROP.EQ.2)WRITE(2,'(100A)')'Id',',','Orig',',','Dest',',','Type', &
                     ',','Dist', &
                     ',','LinkCap',',','Dem/Cap',',','StVeh',',', &
                     'TotVeh',',','ServLev',',','RouteId',',','RouteName',',',  &
                     'Demand',',','Vehics',',', 'Dem/Cap'
					 
! Maximum output					 
      IF(IFOROP.EQ.3)WRITE(2,'(100A)')'Id',',','Orig',',','Dest',',','Name',',','Type', &
                     ',','Dist', &
                     ',','LinkCap',',','Dem/Cap',',','StVeh',',','TotVeh',&
                     ',','ServLev',',','OperId',',','OperName',',','RouteId',',','RouteName',',','Capac', &
                     ',','Demand',',','Vehics',',','Dem/Cap',',','StVeh',  &
                     ',','IniSpeed',',','FinSpeed',',','IniWait', &
                     ',','FinWait',',','Energy'

!  Loops over links

      DO 300 L=1,NLINK

!  Calculates variables, whatever the format

      VEHTOT=0.
      VSTAN=0.
	  NAMELINK='          '
      VC=0.               ! Volume/capacity ratio
      NURUTAS=0           ! Number of routes in the link
      NURUTAS=NRUTIN(L)
      VELREF=VEL(L)
      CV    =CAP(L)
      IT    =ITIP(L)
      DO LNR=RUTPRI(L), RUTULT(L)           ! Goes through the routes in the link
         IR=IROUTE(LNR)                     ! Identifies the route
         IO=IOPRUT(IR)                      ! Identifies the operator to which the route belongs
         CAPOP(LNR)=VOL(LNR)                ! Initializes the capacity equals volume
!  Calculates capacity according to the type of operator
         IF(ITIPOP(IO).LE.3)CAPOP(LNR)=VEH(LNR)*TOC(IO)   ! motorized operators equals the number of vehicles * occupancy rate
         IF(ITIPOP(IO).EQ.4)CAPOP(LNR)=VOL(LNR)           ! non-motorized equals volume
         DCOP(LNR)=0.
         IF(CAPOP(LNR).GT.CEROMAS) THEN
             DCOP(LNR)=VOL(LNR)/CAPOP(LNR)   ! Demand/capacity of the route
         ENDIF
         VELOP(LNR)=VELREF*SPEED(IO,IT)      ! Final speed of the route
         IF(ITIPOP(IO).EQ.4)VELOP(LNR)=VELTIP(IT)*SPEED(IO,IT)  ! Except non-motorized
         VEHSTOP(LNR)=VEH(LNR)*VS(IO,IT)     ! Standard vehicles of the route
! If operator is type 4 (non-motorized) it does not add to total vehicles
         VEHTOT=VEHTOT+VEH(LNR)              ! Total vehicles on link
         VSTAN=VSTAN+VEHSTOP(LNR)            ! Total standard vehicles on link
         ESPERIN(LNR)=ESPERA(LNR)
         IF((ITIPOP(IO).EQ.3))THEN           ! Transit routes
             ESPERIN(LNR)=ESPMIN(IO)
         ENDIF
! Calculates energy of route in link
		 ENEROUTE(LNR)=(PAREN1(IO)+PAREN2(IO)*EXP(-PAREN3(IO)*VELOP(LNR)))*DIS(L)*VEH(LNR)
      ENDDO    ! End loop over routes LNR
      ZZ='A'   ! Level-of-service of link
!  VELTIP is free-flow speed and VELREF is congested speed (smaller) - Hence VEL_SP is between zero and one
      IF(VELTIP(IT).GT.CEROMAS)VEL_SP=VELREF/VELTIP(IT)
      IF(VEL_SP.LT..875)ZZ='B'
      IF(VEL_SP.LT..700)ZZ='C'
      IF(VEL_SP.LT..550)ZZ='D'
      IF(VEL_SP.LT..425)ZZ='E'
      IF(VEL_SP.LT..325)ZZ='F'
      IF(VEL_SP.LT..250)ZZ='G'
      IF(VEL_SP.LT..180)ZZ='H'
      IF(CV.GT.0)VC=VSTAN/CV

!  Option 1: all links
      IF(IMPOP(1))GO TO 800

!  Option 2:  by link type
      IF(IMPOP(2).AND.IMPTIP(IT))GO TO 800

!  Option 3:  by volume/capacity ratio
      IF(IMPOP(3).AND.VC.GE.VCMIN.AND.VC.LE.VCMAX)GO TO 800

!  Option 4:  list of links
      IF(IMPOP(4))THEN
         DO N=1,NLMAX
            IF(NODORIG(N).EQ.0)GO TO 300
            IF(IOR(L).EQ.NODORIG(N).AND.IDES(L).EQ.NODODES(N))GO TO 800
         ENDDO
      ENDIF
      GO TO 300

800   CONTINUE
      
!  Outputs the link according to format option

!  Format 1: minimum

      IF(IFOROP.EQ.1)THEN
        WRITE(2,450)LINKID(L),',',IOR(L),',',IDES(L),',', &
           NUMTIP(IT),',',DIS(L),',',CV,',',VC,',', &
           VSTAN,',',VEHTOT,',',ZZ
450      FORMAT(4(I8,A),F8.2,A,I6,A,F4.2,A,F8.0,A,F8.0,2X,A,A)
      ELSE

!  Format 2 or 3: medium or maximum

      DO LNR=RUTPRI(L), RUTULT(L)           ! Goes through the routes in the link
         IR=IROUTE(LNR)                     ! Identifies the route
         IO=IOPRUT(IR)                      ! Identifica the operator
		 
         IF(IFOROP.EQ.2) THEN               ! Medium format
           WRITE(2,460)LINKID(L),',',IOR(L),',',IDES(L),',',NUMTIP(IT), &
             ',', DIS(L), &
             ',',CV,',',VC,',',VSTAN,',',VEHTOT,',',ZZ,',',NUMRUT(IR), ',', &
             NOMRUT(IR),',',VOL(LNR),',',VEH(LNR),',',DCOP(LNR) 
460          FORMAT(4(I8,A),F8.2,A,I6,A,F4.2,A,F8.0,A,F8.0,3A, &
                I4,A,A6,A,F9.0,A,F8.0,A,F6.1)
         ELSE    ! Maximum format
		   ! Cast IDLINK
		   WRITE(IDLINK,'(I0,A,I0)') IOR(L),'-',IDES(L)
		   RUTNUM = NUMRUT(IR)
		   IF(NUMRUT(IR) < 0) THEN 
			RUTNUM = NUMRUT(IR)*(-1)
		   END IF
           WRITE(2,400)IDLINK,',',IOR(L),',',IDES(L),',',NAMELINK,',',NUMTIP(IT),','	&
		     ,DIS(L),',',CV,',',VC,',',VSTAN,',',VEHTOT,',' &
			 ,'Level '//ZZ,',',NUMOP(IO),',',NOMOP(IO),',',RUTNUM,',',NOMRUT(IR),',' &
			 ,CAPOP(LNR),',',VOL(LNR),',',VEH(LNR),',',DCOP(LNR),',',VEHSTOP(LNR),',' &
			 ,VELTIP(IT)*SPEED(IO,IT),',',VELOP(LNR),',',ESPERIN(LNR),',',ESPERA(LNR),',' &
			 ,ENEROUTE(LNR)
400          FORMAT(A,A,I8,A,I8,A,A,A,I8,A, &
F8.2,A,I6,A,F4.2,A,F8.0,A,F8.0,A, &
A,A,I8,A,A,A,I8,A,A,A, &
F8.0,A,F9.0,A,F8.0,A,F7.2,A,F8.2,A, &
F8.1,A,F6.0,A,F6.0,A,F7.2,A,F8.2)
         ENDIF  ! If format is 2 or 3
      ENDDO     ! Enddo routes in link

      ENDIF     ! If format is 1 or 2+3

300   CONTINUE

100   CONTINUE
!      IF(IMPOP(1).OR.IMPOP(2).OR.IMPOP(3).OR.IMPOP(4))THEN
!         CALL MENSA(8063,2)  ! Number of links
!         WRITE(2,*)NARCOS
!      ENDIF
      if (debugging >= dbg_Normal) then
          CALL MENSA(8,0)  ! NORMAL END OF
          print *, 'I M P T R A'
      endif
      STOP
CONTAINS

      SUBROUTINE CONMEN(VCMIN,VCMAX,IFOROP,LRUTAS)
      logical(1) :: IMPTIP,IMPOP,LRUTAS(MXRUT)
      COMMON NODORIG(NLMAX),NODODES(NLMAX),IMPTIP(MXTIP),IMPOP(NIMPOP)

      INTEGER AUX(MXTIP)
      LOGICAL NE

!  Options menu

      DO 500 I=1, NIMPOP
      IMPOP(I)=.FALSE.
500   AUX(I)=0
      WRITE(*,*)
      WRITE(*,*)
      WRITE(*,*)'_____________________________________________'
      CALL MENSA(8064,0)  ! Output options for assignment
      WRITE(*,*)'_____________________________________________'
539   WRITE(*,'(''     -----> ''$)')
      READ(*,*)(AUX(I),I=1,NIMPOP)
      DO 540 I=1,NIMPOP
      IF(AUX(I).LT.0.OR.AUX(I).GT.NIMPOP.OR.AUX(I).EQ.6 .OR. AUX(I) == 8)THEN
         WRITE(*,*)'Op:',AUX(I)
         CALL MENSA(6,0)  ! ERROR G04: Illegal definition
         GO TO 539
      ENDIF
      IF(AUX(I).EQ.0)GO TO 541
      IMPOP(AUX(I))=.TRUE.
540   CONTINUE

541   WRITE(*,*)
      WRITE(*,*)

!  If option is 2, asks for link types

      IF(IMPOP(2))THEN
12       CALL MENSA(8065,0)  ! Specify link types...
         WRITE(*,'(''     -----> ''$)')
         AUX=0
         READ(*,*)(AUX(I),I=1,MXTIP)
         DO I=1,MXTIP
           IF(AUX(I).EQ.0)CYCLE
           IT1=INTNUM(AUX(I),NUMTIP,NTIP)
           IF(IT1.GT.NTIP)THEN
             WRITE(*,*)'  Tip:',AUX(I)
             CALL MENSA(6,0)  ! ERROR G04: Illegal definition
             GO TO 12
           ENDIF
           IMPTIP(IT1)=.TRUE.
         ENDDO
      ENDIF

!  If option is 3 asks for the V/C ratio

      IF(IMPOP(3))THEN
         CALL MENSA(8066,0)  ! Introduce V/C ratio
         WRITE(*,'(''   Min ---> ''$)')
         READ(*,*)VCMIN
         WRITE(*,'(''   Max ---> ''$)')
         READ(*,*)VCMAX
      ENDIF

!  If option is 4 asks for the links

      IF(IMPOP(4))THEN
         NODORIG=0
         NODODES=0
         CALL MENSA(8067,0)  ! Identify links ...
         DO 130 I=1,NLMAX
         WRITE(*,'(''     Orig  ---> ''$)')
         READ(*,*)NODORIG(I)
         IF(NODORIG(I).EQ.0)GO TO 131
         WRITE(*,'(''     Dest  ---> ''$)')
         READ(*,*)NODODES(I)
130      CONTINUE
131      CONTINUE
      ENDIF

! If option is 7 asks for routes

      IF(IMPOP(7))THEN
         WRITE(*,'((5(I8,1X,A8)))')(NUMRUT(IR),NOMRUT(IR),IR=1,NRUTAS)
127      LRUTAS=.FALSE.
         AUX=0
         CALL MENSA(8073,0)
         READ(*,*)AUX
         IF(AUX(1).EQ.0)THEN
            LRUTAS=.TRUE.
         ELSE
            DO IR=1,NRUTAS
               IF(AUX(IR).EQ.0)EXIT
               IT1=INTNUM(AUX(IR),NUMRUT,NRUTAS)
               IF(IT1.GT.NRUTAS)THEN
                  WRITE(*,*)'  Rut:',AUX(I)
                  CALL MENSA(6,0)  ! ERROR G04: Definicion ilegal
                  GO TO 127
               ENDIF
               LRUTAS(IT1)=.TRUE.
            ENDDO
         ENDIF
      ENDIF

! Asks for the output format, if options are 1 to 4

      NE=.FALSE.
      DO i=1,4
        NE = NE .or. IMPOP(I)
      ENDDO
      IF (.not. NE) RETURN

      WRITE(*,*)
      WRITE(*,*)
          WRITE(*,*)'__________________________________________'
      CALL MENSA(8068,0)  ! Output format option: ...
          WRITE(*,*)'__________________________________________'
529   CALL MENSA(7,1)
      READ(*,'(I2)')IFOROP
      IF(IFOROP.LE.0.OR.IFOROP.GT.3)GO TO 529

      RETURN
      END SUBROUTINE



      SUBROUTINE MENU(VCMIN,VCMAX,IFOROP,SpeedMin,SpeedMax)
      logical(1) :: IMPTIP,IMPOP
      COMMON NODORIG(NLMAX),NODODES(NLMAX),IMPTIP(MXTIP),IMPOP(NIMPOP)
      REAL SpeedMin(20), SpeedMax(20)

      INTEGER CONTR(NIMPOP)
      CHARACTER*10 TIT

      VCMIN=0
      VCMAX=0
      IMPOP=.FALSE.
      CONTR=0

!  READS FILE IMPTRA.DAT AND CHECKS THE OPTIONS

      READ(3,'(///)',END=999)
1     READ(3,*,END=999,ERR=2)I,TIT,CONTR(I)
      IF(I.LT.0.OR.I.GT.NIMPOP)THEN
        WRITE(*,*)'IMPTRA.DAT (0.0)'
        CALL MENSA(3,-1)  ! ERROR G03: Read problem
      ENDIF
      GO TO 1
2     CALL CHECK(0.0,IOS,'IMP')

      WRITE(*,*)

      IF(CONTR(1).LT.0.OR.CONTR(1).GT.2)STOP'Op 1 inval'
      IF(CONTR(2).LT.0.OR.CONTR(2).GT.1)STOP'Op 2 inval'
      IF(CONTR(3).LT.0.OR.CONTR(3).GT.1)STOP'Op 3 inval'
      IF(CONTR(4).LT.0.OR.CONTR(4).GT.3)STOP'Op 4 inval'
      IF(CONTR(5).LT.0.OR.CONTR(5).GT.1)STOP'Op 5 inval'
      IF(CONTR(6).LT.0.OR.CONTR(6).GT.1)STOP'Op 6 inval'
      IF(CONTR(7).NE.0)STOP'Op 7 inval'
      IF(CONTR(7).LT.0.OR.CONTR(7).GT.1)STOP'Op 8 inval'

      IF(CONTR(1).EQ.1)IMPOP(4)=.TRUE.
      IF(CONTR(1).EQ.2)IMPOP(1)=.TRUE.
      IF(CONTR(2).EQ.1)IMPOP(2)=.TRUE.
      IF(CONTR(3).EQ.1)IMPOP(3)=.TRUE.
      IFOROP=CONTR(4)
      IF(CONTR(5).EQ.1)IMPOP(5)=.TRUE.
      IF(CONTR(6).EQ.1)IMPOP(6)=.TRUE.
      IF(CONTR(8).EQ.1)IMPOP(8)=.TRUE.
      IF(CONTR(9).EQ.1)IMPOP(9)=.TRUE.
      IF(CONTR(10).EQ.1)IMPOP(10)=.TRUE.

!  SECTION 1 - List of links to report

      READ(3,'(/)',END=999)
      DO 100 I=1,NLMAX
      NODORIG(I)=0
100   NODODES(I)=0
      LINK=0
110   LINK=LINK+1
      IF(LINK.GT.NLMAX)CALL MENSA(8069,-1)  ! IMPTRA.DAT(1): Too msny links
      READ(3,*,END=999,ERR=120,IOSTAT=IOS)I,J,ISENTIDO
      NODORIG(LINK)=I
      NODODES(LINK)=J
!  If two-way has been indicated, generates the reverse
      IF(ISENTIDO.EQ.2)THEN 
         LINK=LINK+1
         IF(LINK.GT.NLMAX)CALL MENSA(8069,-1)  ! IMPTRA.DAT(1): Too many links
         NODORIG(LINK)=J
         NODODES(LINK)=I
      ENDIF
      GO TO 110
120   CALL CHECK(1.0,IOS,'IMP')

!  SECTION 2 - List of link types
200   IMPTIP = .FALSE.
      READ(3,'(/)',END=999)
210   READ(3,*,END=999,ERR=220,IOSTAT=IOS)IO
      IO1=INTNUM(IO,NUMTIP,NTIP)
      IF(IO1.GT.NTIP)THEN
         WRITE(*,*)'IMPTRA.DAT(2.0) - Tip:',IO
         CALL MENSA(6,-1)  ! ERROR G04: Illegal definition
      ENDIF
      IMPTIP(IO1)=.TRUE.
      GO TO 210
220   CALL CHECK(2.0,IOS,'IMP')

!  SECTION 3 - Demand/Capacity range

      READ(3,'(/)',END=999)
310   READ(3,*,END=999,ERR=300,IOSTAT=IOS)VCMIN,VCMAX
      IF(VCMIN.GT.VCMAX)THEN
        WRITE(*,'(''IMPTRA.DAT(3.0)  Min='',F6.2,''   Max='',F6.2)') &
             VCMIN,VCMAX
        CALL MENSA(6,-1)  ! ERROR G04: Illegal definition
      ENDIF
      GO TO 310
300   CALL CHECK(3.0,IOS,'IMP')       

! SECTION 4 - Speed ranges

      READ(3,'(/)',END=998)               
      IO=0
410   IO=IO+1
      IF(IO.GT.20)THEN
         WRITE(*,*)' IMPTRA.DAT(4): Max = 20'
         CALL MENSA(6,-1)  ! ERROR G04: Illegal definition
      ENDIF   
      READ(3,*,END=998,ERR=400,IOSTAT=IOS)SpeedMin(IO),SpeedMax(IO)
      GO TO 410
400   CALL CHECK(4.0,IOS,'IMP')
998   RETURN  


999   WRITE(*,*)'IMPTRA.DAT'
      CALL MENSA(10,-1)  ! ERROR G06: Incomplete file
      END SUBROUTINE



      SUBROUTINE CORDON(AREA,POL,ITER,IAN,MES,IDIA,IHR,MINS,TAB)
      integer(2) :: IAN,MES,IDIA,IHR,MINS
!     IMPRIME LOS CORDONES
      INTEGER NOR(NLMAX),NDES(NLMAX),NCOUNT(NLMAX)
      CHARACTER*3 AREA,POL
      CHARACTER*1 TAB
      CHARACTER*8 NOMBRE,SENTIDO

      integer :: TOTNCOUNT

!  Reads each cordon from IMPTRA.DAT and counts the number of links
      READ(3,'(/)')

      WRITE(2,*)
      WRITE(2,*)
      CALL MENSA(8070,2)  ! RESULTS IN CORDONS...
      WRITE(2,'(I8,6X,A3,3X,A3,I4.2''-'',I2.2,''-'',2I4.2,'':'',I2.2)') &
              ITER,AREA,POL,IDIA,MES,IAN,IHR,MINS

100   TOTSTD=0.      ! Total standard vehicles in cordon
      TOTVEH=0.      ! Total vehicles in cordon
      NOR=0          ! Origin node in cordon
      NDES=0         ! Destination nod in cordon
      NCOUNT=0       ! Observed count
      NCORLINK=0
      TOTNCOUNT=0
      READ(3,*,END=999,ERR=333,IOSTAT=IOS) &
        NCOR,NOMBRE,SENTIDO,(NOR(L),NDES(L),NCOUNT(L),L=1,NLMAX)
      if (debugging >= dbg_debug) then
         print *, 'Read cordon ', NCOR, NOMBRE
      endif
      DO L=1,NLMAX
         IF(NOR(L).EQ.0)EXIT
         NCORLINK=NCORLINK+1
         TOTNCOUNT = TOTNCOUNT + NCOUNT(L)
      ENDDO

!  After reading conrdons, reads network and outputs links

      CALL MENSA(8071,2)  ! Cordon    Direction
      WRITE(2,'(2X,A6,4X,A6)')NOMBRE,SENTIDO
      WRITE(2,'(100A)')'Orig',TAB,'Dest',TAB,'StVehs',TAB,'TotVehs', &
                       TAB, 'Count'
      
!  Finds out if link is in the cordon

      DO L=1, NCORLINK
         DO IX=1, NLINK
           IF(NOR(L).EQ.0)EXIT
           IF(NOR(L).EQ.IOR(IX).AND.NDES(L).EQ.IDES(IX))THEN
              NCORLINK=NCORLINK-1
        ! Calculates VehStd and VehTot
              VEHSTD=0.
              VEHTOT=0.
              IT    =ITIP(IX)    
              DO LNR=RUTPRI(IX), RUTULT(IX)
                 IR=IROUTE(LNR)              ! Identifies route
                 IO=IOPRUT(IR)               ! Identifies operator
                 IF(ITIPOP(IO).NE.4)THEN     ! Pedestrians dont count
                   VEHTOT=VEHTOT+VEH(LNR)              ! Total vehicles in link
                   VEHSTD=VEHSTD+VEH(LNR)*VS(IO,IT)    ! Standard vehicles in link
                   TOTVEH=TOTVEH+VEH(LNR)              ! Total vehicles in cordon
                   TOTSTD=TOTSTD+VEH(LNR)*VS(IO,IT)    ! Total standard vehicles in count
                 ENDIF
              ENDDO  ! End do routes K
        ! Outputs the link
              WRITE(2,'(I8,A,I8,A,F10.0,A,F10.0,A,I10)') &
              IOR(IX),TAB,IDES(IX),TAB,VEHSTD,TAB,VEHTOT,TAB,NCOUNT(L)
           ENDIF
         ENDDO
      ENDDO
      WRITE(2,'(''TOTAL'',A,A,F10.0,A,F10.0,A,I10)') &
                    TAB,TAB,TOTSTD,TAB,TOTVEH, TAB, TOTNCOUNT
      GOTO 100

333   CALL CHECK(5.0,IOS,'IMP')
      RETURN

999   WRITE(*,*)'IMPTRA.DAT'
      CALL MENSA(10,-1)  ! ERROR G06: Incomplete file
      END SUBROUTINE


      SUBROUTINE REPORUT(AREA,POL,ITER,IAN,MES,IDIA,IHR,MINS,TAB,LRUTAS)
      integer(2) :: IAN,MES,IDIA,IHR,MINS
!     REPORTA LAS RUTAS
      CHARACTER*3 AREA,POL
      CHARACTER*1 TAB
      logical(1) :: LRUTAS(MXRUT)
      INTEGER NURUTS(MXOPER)
      REAL TDISRUT(MXOPER),TTIERUT(MXOPER),TPASKM(MXOPER), &
         TVEHKM(MXOPER),TVEHOR(MXOPER),TFLOTA(MXOPER)
      real :: TotalSuben, AvgOccup

      WRITE(2,*)
      WRITE(2,*)
      CALL MENSA(8072,2)  ! ROUTES PROFILES...
      WRITE(2,'(I8,6X,A3,3X,A3,I4.2''-'',I2.2,''-'',2I4.2,'':'',I2.2)') &
               ITER,AREA,POL,IDIA,MES,IAN,IHR,MINS

      NURUTS=0
      TDISRUT=0.
      TTIERUT=0.
      TPASKM=0.
      TVEHKM=0.
      TVEHOR=0.
      TFLOTA=0.

      DO IRUT=1,NRUTAS
         IF(.NOT.LRUTAS(IRUT))CYCLE
         DISRUT=0.
         TIEMRUT=0.
         PASAJKM=0.
         VEHICKM=0.
         VEHORAS=0.
         ICRITOR=0
         ICRIDES=0
         CRITICO=0.
         DCCRIT =0.
         FLOTA  =0.
         TotalSuben=0.
         AvgOccup=0.
         IO=IOPRUT(IRUT)
         IF(ITIPOP(IO).NE.3)CYCLE
         NURUTS(IO)=NURUTS(IO)+1  ! Number of routes reported
         WRITE(2,'('' Route'',I8,1X,A,''  Freq='',F5.1,''  Oper'',I8,' &
           // '1X,A)')NUMRUT(IRUT),NOMRUT(IRUT),FREQNew(IRUT), &
           NUMOP(IO),NOMOP(IO)
         WRITE(2,'(100A)')'Orig',TAB,'Dest',TAB,'Dist',TAB,'Capac',TAB,'Demand',TAB,'Dem/Cap',TAB,  &
               'Speed',TAB,'SpareCap',TAB,'Board',TAB,'Alight',TAB,  &
              'FinalCap',TAB,'WaitTime'
      
!  Goes through links looking for route IRUT

         DO L=1,NLINK

!  Calculates variables and reports

         VELREF=VEL(L)
         IT    =ITIP(L)

         DO LNR=RUTPRI(L), RUTULT(L)  ! goes through the routes in link

            IR=IROUTE(LNR)            ! Identifies route
            IF(IR.NE.IRUT)CYCLE
            CAPOPER=VEH(LNR)*TOC(IO)  ! Capacity of route
            Puest=CAPOPER-(Vol(LnR)-Suben(LnR))
            if (Puest < 0) Puest = 0
            DCOPER=0.                 ! Demand/capacity of route
            IF(CAPOPER.GT.CEROMAS)DCOPER=VOL(LNR)/CAPOPER
            VELOPER=VELREF*SPEED(IO,IT)   ! Final speed of route
            DISRUT=DISRUT+DIS(L)
            PASAJKM=PASAJKM+VOL(LNR)*DIS(L)
            TPASKM(IO)=TPASKM(IO)+VOL(LNR)*DIS(L)
            VEHICKM=VEHICKM+VEH(LNR)*DIS(L)
            TVEHKM(IO)=TVEHKM(IO)+VEH(LNR)*DIS(L)
            VEHORAS=VEHORAS+VEH(LNR)*(DIS(L)/VELOPER)
            TVEHOR(IO)=TVEHOR(IO)+VEH(LNR)*(DIS(L)/VELOPER)
            IF(VELOPER.GT.CEROMAS)TIEMRUT=TIEMRUT+DIS(L)/VELOPER
            IF(VOL(LNR).GT.CRITICO)THEN
               CRITICO=VOL(LNR)
               ICRITOR=IOR(L)
               ICRIDES=IDES(L)
               DCCRIT =DCOPER
            ENDIF
            TotalSuben = TotalSuben + Suben(LnR)
            AvgOccup   = AvgOccup + DIS(L)*DCOPER

            AEH  = ESPERA(LnR)
            NNEH = AEH
            NNEM = 60. * (ESPERA(LnR) - NNEH)
            NNES = 3600. * (ESPERA(LnR) - NNEH - NNEM/60.)

            WRITE(2,7771) &
             IOR(L),TAB,IDES(L),TAB,DIS(L),TAB,CAPOPER,TAB,VOL(LNR),TAB,DCOPER, &
             TAB,VELOPER,TAB,Puest,TAB,Suben(LnR),TAB,Bajan(LnR), TAB,  &
             Puest-Suben(LnR)+Bajan(LnR),TAB, NNEH,NNEM,NNES
7771        format(I6,A,I6,A,F8.2,A,F8.0,A,F8.0,A,F8.2,5(A,F8.0),A, I12, 2(':',I2.2) )
         ENDDO    ! End DO routes LNR

         ENDDO    ! End DO links L
         
         if (DISRUT /= 0) AvgOccup = AvgOccup / DISRUT

         WRITE(2,*)
         WRITE(2,'(''TotalDist'',A,F12.1)')TAB,DISRUT
         WRITE(2,'(''TotalTime'',A,F12.1)')TAB,TIEMRUT
         WRITE(2,'(''Pass-Dist'',A,F12.1)')TAB,PASAJKM
         WRITE(2,'(''Vehic-Dist'',A,F12.1)')TAB,VEHICKM
         WRITE(2,'(''Vehic-Hours'',A,F12.1)')TAB,VEHORAS
         A=0.
         IF(VEHORAS.GT.CEROMAS)A=PASAJKM/VEHORAS
         WRITE(2,'(''PasDis/VehHrs'',A,F12.1)')TAB,A
         A=0.
         IF(TIEMRUT.GT.CEROMAS)A=DISRUT/TIEMRUT
         WRITE(2,'(''AvSpeed'',A,F12.1)')TAB,A
         WRITE(2,'(''MinFreq'',A,F12.1)')TAB,FreqMin(IRUT)
         WRITE(2,'(''MaxFreq'',A,F12.1)')TAB,FreqMax(IRUT)
         WRITE(2,'(''Frequency'',A,F12.1)')TAB,FreqNew(IRUT)
         IF(FreqNew(IRUT).GT.0.)THEN
            FLOTA=TIEMRUT/(1./FreqNew(IRUT))
            IF(MOD(TIEMRUT,1./FreqNew(IRUT)).GT.0.1) THEN
               FLOTA=INT(FLOTA+1.)
            ENDIF
         ENDIF
         WRITE(2,'(''Fleet'',A,F12.0)')TAB,FLOTA
         WRITE(2,'(''CritVol'',A,F12.0,A,F6.2,A,I6,''-'',I6)') &
           TAB,CRITICO,TAB,DCCRIT,TAB,ICRITOR,ICRIDES
         WRITE(2,'(''Boardings'',A,F12.0)')TAB,TotalSuben
         WRITE(2,'(''TargetOccup'',A,F12.1,''%'')')TAB,100.*TOCPromedio(IRUT)
         WRITE(2,'(''AvgOccup'',A,F12.1,''%'')') TAB,100.*AvgOccup
         WRITE(2,*)
         TDISRUT(IO)=TDISRUT(IO)+DISRUT    ! Total length of routes
         TTIERUT(IO)=TTIERUT(IO)+TIEMRUT   ! Total time of routes
         TFLOTA(IO)=TFLOTA(IO)+FLOTA       ! Total fleet
      ENDDO      ! DO routes IRUT

      WRITE(2,*)'GLOBAL STATISTICS BY OPERATOR'
      WRITE(2,*)

      DO IO=1,NOPER
         IF(NURUTS(IO).LE.0)CYCLE
         WRITE(2,'(2A,I11,A)')'Oper',TAB,NUMOP(IO),TAB,NOMOP(IO)
         WRITE(2,*)
         WRITE(2,'(''NoRoutes'',A,I11)')TAB,NURUTS(IO)
         WRITE(2,'(''TotalDist'',A,F12.0)')TAB,TDISRUT(IO)
         WRITE(2,'(''TotalTime'',A,F12.1)')TAB,TTIERUT(IO)
         WRITE(2,'(''Pass-Dist'',A,F12.1)')TAB,TPASKM(IO)
         WRITE(2,'(''Vehic-Dist'',A,F12.1)')TAB,TVEHKM(IO)
         WRITE(2,'(''Vehic-Hrs'',A,F12.1)')TAB,TVEHOR(IO)
         A=0.
         IF(TVEHOR(IO).GT.CEROMAS)A=TPASKM(IO)/TVEHOR(IO)
         WRITE(2,'(''PasDism/VehHr'',A,F12.1)')TAB,A
         A=0.
         IF(TTIERUT(IO).GT.CEROMAS)A=TDISRUT(IO)/TTIERUT(IO)
         WRITE(2,'(''AvSpeed'',A,F12.1)')TAB,A
         WRITE(2,'(''Fleet'',A,F12.0)')TAB,TFLOTA(IO)
         write(2,*)
      ENDDO

      RETURN
      END SUBROUTINE

      SUBROUTINE VEHSPEED(AREA,POL,ITER,IAN,MES,IDIA,IHR,MINS,TAB,SpeedMin,SpeedMax)
      integer(2) :: IAN,MES,IDIA,IHR,MINS

!     REPORTS VEHICLES-KM (OR MILES) BY SPEED CLASS AND OPERATOR
      CHARACTER*3 AREA,POL
      CHARACTER*1 TAB
      INTEGER NSpeedClasses
      REAL SpeedMin(20), SpeedMax(20)
      REAL VEHDIST(20,MXOPER)
      
      WRITE(2,*)
      WRITE(2,*)
      WRITE(2,*)' DISTANCE TRAVELLED BY OPERATORS BY SPEED CLASS'
      WRITE(2,*)'  Iter     Area    Pol   Date/time of simulation'
      WRITE(2,'(I8,6X,A3,3X,A3,I4.2''-'',I2.2,''-'',2I4.2,'':'',I2.2)') &
              ITER,AREA,POL,IDIA,MES,IAN,IHR,MINS              
     
!     Checks ranges and counts the number of speed clases

      NSpeedClasses=0
      DO I=1,20            
         IF(I.GT.1.AND.SpeedMax(I).LT.SpeedMin(I))THEN
            WRITE(2,*)' Inconsistent class: ', SpeedMin(I),SpeedMax(I)
            STOP 01
         ENDIF                                             
         IF(SpeedMax(I).EQ.0)EXIT
         NSpeedClasses=NSpeedClasses+1
      ENDDO                                   
      IF(NSpeedClasses.LE.0)THEN
         WRITE(2,*) 'No speed classes requested'
         STOP 01                    
      ENDIF
         
! Builds matrix VEHDIST (Vehicule-Km by speed class)

      VEHDIST=0.        
      
!  Loops over links

      DO L=1,NLINK       

         DO LNR=RUTPRI(L), RUTULT(L)           ! Goes through routes in link

            IR=IROUTE(LNR)                     ! Identifies the route
            IO=IOPRUT(IR)                      ! Identifies the operator of route IR
            VELOPER=VEL(L)*SPEED(IO,ITIP(L))   ! Final speed of route
!     Looks for the speed class and loads it in the matrix            
            DO IS=1,NSpeedClasses
             IF(VELOPER.GE.SpeedMin(IS).AND.VELOPER.LT.SpeedMax(IS))THEN
                   VEHDIST(IS,IO)=VEHDIST(IS,IO)+VEH(LNR)*DIS(L)
                   EXIT
             ENDIF
            ENDDO
            
         ENDDO    ! End DO routes LNR
      ENDDO       ! End DO links L
      
! Outputs the resulting matrix

      WRITE(2,'(''SpeedMin'',A,''SpeedMax'',50(A,A10))') &
            TAB,(TAB,NOMOP(IS),IS=1,NOPER)
      DO IS=1,NSpeedClasses
      WRITE(2,'(F8.2,A,F8.2,50(A,F10.0))')SpeedMin(IS),TAB,SpeedMax(IS), &
           (TAB,VEHDIST(IS,IO),IO=1,NOPER)                
      ENDDO
     
      RETURN
      END SUBROUTINE

      SUBROUTINE PerfilEnlaceRuta(Output)
         integer, intent(in)     :: Output

         call OpenPolFile(10, IPOL, 'T5S', IO_BIN)
         call LeeLNRIndic(10)
         close(10)
         call ImprimeLNRIndic(Output)
      END SUBROUTINE

character*(80) function quote(text)
  character*(*), intent(in) :: text
  quote = trim(text)
  return
end function  


SUBROUTINE CSVRouteProfile(AREA,POL,ITER,IAN,MES,IDIA,IHR,MINS,TAB,LRUTAS)
      integer(2) :: IAN,MES,IDIA,IHR,MINS
      CHARACTER*3 AREA,POL
      CHARACTER*1 TAB
      logical(1) :: LRUTAS(MXRUT)

! Outputs routes profiles in CSV format

      if (.false.) then
        ! write the header
        WRITE(2,*)
        WRITE(2,*)
        CALL MENSA(8072,2)  ! ROUTES PROFILES...
        WRITE(2,'(I8,6X,A3,3X,A3,I4.2''-'',I2.2,''-'',2I4.2,'':'',I2.2)') &
                 ITER,AREA,POL,IDIA,MES,IAN,IHR,MINS
      endif

100   FORMAT(2(A6,','),A32,',',A6,',',A32,18(',',A18))
      WRITE(2, 100) &
            quote('Scen'), & 
            quote('RutId'),quote('Name'),quote('OpId'),quote('Name'), &
            quote('Freq'),quote('FreqMin'),quote('FreqMax'), &
            quote('TotalDist'),quote('TotalTime'),quote('Pass-Dist'), &
            quote('Vehic-Dist'),quote('Vehic-Hours'), &
            quote('PasDis/VehHrs'),quote('AvgSpeed'), &
            quote('Fleet'),quote('CritVol'),quote('CritVo/Cap'),quote('CriOr'),quote('CritDes'), &
            quote('Boardings'), &
            quote('TargetOccup'), &
            quote('AvgOccup') 
      
      LRUTAS=.true.
      DO IRUT=1,NRUTAS
         IF(.NOT.LRUTAS(IRUT))CYCLE
         IO=IOPRUT(IRUT)
         IF(ITIPOP(IO).NE.3)CYCLE
         DISRUT=0.
         TIEMRUT=0.
         PASAJKM=0.
         VEHICKM=0.
         VEHORAS=0.
         ICRITOR=0
         ICRIDES=0
         CRITICO=0.
         DCCRIT =0.
         FLOTA  =0.
         TotalSuben=0.
         AvgOccup=0.
      
!  Loops over links looking for route IRUT

         DO L=1,NLINK

!  Calculates variables and reports

         VELREF=VEL(L)
         IT    =ITIP(L)

         DO LNR=RUTPRI(L), RUTULT(L)      ! Goes through routes in link L
            IR=IROUTE(LNR)                ! Identifies the route
            IF(IR.NE.IRUT)CYCLE
            CAPOPER=VEH(LNR)*TOC(IO)      ! Capacity of route
            Puest=CAPOPER-(Vol(LnR)-Suben(LnR))
            if (Puest < 0) Puest = 0
            DCOPER=0.                     ! Demand/capacity of route
            IF(CAPOPER.GT.CEROMAS)DCOPER=VOL(LNR)/CAPOPER
            VELOPER=VELREF*SPEED(IO,IT)   ! Final speed of route
            DISRUT=DISRUT+DIS(L)
            PASAJKM=PASAJKM+VOL(LNR)*DIS(L)
            VEHICKM=VEHICKM+VEH(LNR)*DIS(L)
            VEHORAS=VEHORAS+VEH(LNR)*(DIS(L)/VELOPER)
            IF(VELOPER.GT.CEROMAS)TIEMRUT=TIEMRUT+DIS(L)/VELOPER
            IF(VOL(LNR).GT.CRITICO)THEN
               CRITICO=VOL(LNR)
               ICRITOR=IOR(L)
               ICRIDES=IDES(L)
               DCCRIT =DCOPER
            ENDIF
            TotalSuben = TotalSuben + Suben(LnR)
            AvgOccup   = AvgOccup + DIS(L)*DCOPER

            AEH  = ESPERA(LnR)
            NNEH = AEH
            NNEM = 60. * (ESPERA(LnR) - NNEH)
            NNES = 3600. * (ESPERA(LnR) - NNEH - NNEM/60.)
         ENDDO    ! End DO routes LNR

         ENDDO    ! End DO links L

         if (DISRUT /= 0) AvgOccup = AvgOccup / DISRUT

         PasDisVehH = 0.
         IF(VEHORAS.GT.CEROMAS)PasDisVehH=PASAJKM/VEHORAS
         AvgSpeed=0.
         IF(TIEMRUT.GT.CEROMAS)AvgSpeed=DISRUT/TIEMRUT
         IF(FreqNew(IRUT).GT.0.)THEN
            FLOTA=TIEMRUT/(1./FreqNew(IRUT))
            IF(MOD(TIEMRUT,1./FreqNew(IRUT)).GT.0.1) THEN
               FLOTA=INT(FLOTA+1.)
            ENDIF
         ENDIF
200      FORMAT(        A, ',',                                   &
                        I6,',',A32,',',I6,',',A32,                &
                        3(',',F18.1),                             &
                        ',',F18.1,                                &
                        ',',A18,                                  &
                        3(',',F18.0),                             &
                        2(',',F18.1),                             &
                       ',',F18.1,',',F18.0,',',F18.2,2(',',I18),  &
                        ',',F18.0,                                &
                        2(',',F18.1))
         WRITE(2,200) POL, &
                      NUMRUT(IRUT),quote(NOMRUT(IRUT)), &
                      NUMOP(IO),quote(NOMOP(IO)),       &
                      FREQNew(IRUT),FreqMin(IRUT),FreqMax(IRUT), &
                      DISRUT,HoursToString(TIEMRUT), &
                      PASAJKM,VEHICKM,VEHORAS, &
                      PasDisVehH,AvgSpeed, &
                      FLOTA,CRITICO,DCCRIT,ICRITOR,ICRIDES, &
                      TotalSuben, &
                      100.*TocPromedio(IRUT), &
                      100.*AvgOccup
                      
      ENDDO      ! End DO routes IRUT

RETURN
END SUBROUTINE

 

logical function GetOptions(SAL)
 character SAL*(*)
      character  flag
      integer n, node, cat, oper, mode
      real min
      character*(MXARGLEN) buf

      getOptions = .false.
      if (.not. hasOpts()) then
          return
      endif

      useImptraDat = .false.
      getOptions = .true.

      IMPOP   = .false.
      IMPTIP  = .false.
      LRUTAS  = .false.
      NODORIG = 0
      NODODES = 0
      LINKCOUNT=0
      VCMIN  = 0
      VCMAX  = 999
      IFOROP = 1

      do i = 1, optc()
          select case(optv(i))
              case ('A')
                 impop(1) = .true.
              case ('T')
                 impop(2) = .true.
              case ('D')
                 impop(3) = .true.
              case ('L')
                 impop(4) = .true.
              case ('I')
                 impop(5) = .true.
              case ('C')
                 impop(6) = .true.
              case ('R')
                 impop(7) = .true.
              case ('P')
                 impop(9) = .true.
              case ('S')
                 impop(10) = .true.
              case ('J')
                 impop(11) = .true.
              case ('t')
                 n = ioptarg(i)
                 it = iFindNum(n, NumTip, NTIP)
                 if (it > 0) then
                    IMPTIP(it)=.true.
                 else
                   print '(''Link Type'', I4, '' not found '')', n
                   stop 01
                 endif
              case ('d')
                 useImptraDat = .true.
                 call verbose('using IMPTRA.DAT ')
              case ('f')
                 IFOROP = ioptarg(i)
              case ('l')
                 call optargirange(i, idor,iddes)
                 if (debugging >= dbg_Verbose) then
                   print '(''Reporting link ('',2I8,'')'')', idor, iddes
                 endif
                 LINKCOUNT=LINKCOUNT+1
                 NODORIG(LINKCOUNT)=idor
                 NODODES(LINKCOUNT)=iddes
              case ('o')
                 SAL=trim(optarg(i))
                 if (SAL == '-') then
                    SAL = 'CON'
                 endif
              case ('r')
                 n = ioptarg(i)
                 ir = iFindNum(n, NumRut, NRUTAS)
                 if (ir > 0) then
                    LRUTAS(ir)=.TRUE.
                 else
                   print '(''Route'', I4, '' not found '')', n
                   STOP 01
                 endif
              case ('w')
                 call optargfrange(i, VCMIN, VCMAX)
                 if (debugging >= dbg_Verbose) then
                   print '(''VC range is '',2F8.2)', VCMIN, VCMAX
                 endif
          end select
      enddo
      SAL = trim(SAL)
 return
 end function


END PROGRAM IMPTRA

 subroutine Usage
 USE GETOPTM
 character*(32) prog
    prog = argv(0)

    print *
    print '(A,'' - Assignment reports for TRANUS(r)'')', trim(prog)

    print *,'usage:'

    print '(4X, A,''  <scen> <command> [options...]'')', trim(prog)
    print '(4X, A,''  <scen> -L [options...]'')', trim(prog)
    print '(4X, A,''  <scen> -T -t <id>... [options...]'')', trim(prog)
    print '(4X, A,''  <scen> -D -x <id> [options...]'')', trim(prog)
    print '(4X, A,''  <scen> -I [options...]'')', trim(prog)
    print '(4X, A,''  <scen> -C [options...]'')', trim(prog)
    print '(4X, A,''  <scen> -R -r <id>... [options...]'')', trim(prog)
    print '(4X, A,''  <scen> -P [options...]'')', trim(prog)
    print '(4X, A,''  <scen> -S '')', trim(prog)

    print *, 'If no commands are given, the program enters interactive mode'
    print *
    print *, 'Commands are:'
    print *, '  -A          : Report all links'
    print *, '  -C          : Cordons (only with IMPTRA.DAT)'
    print *, '  -D          : Filter links by Demand/Capacity range'
    print *, '  -I          : Table of indicators'
    print *, '  -J          : Indicators in comma delimited format'
    print *, '  -L          : Report specified links'
    print *, '  -P          : Link-Route & Category profile'
    print *, '  -R          : Transit Routes profiles'
    print *, '  -S          : Route profile in comma delimited format'
    print *, '  -T          : Report specified link types'
    print *

    print *
    print *, 'Options are:'
    print *, '  -d             : Read options from IMPTRA.DAT'
    print *, '  -f <c>         : Report output format'
    print *, '  -f 1           :    minimum output'
    print *, '  -f 2           :    normal output'
    print *, '  -f 3           :    verbose output'
    print *, '  -l <id>,<id>   : Include link <id>:<id> in report.'
    print *, '  -o <name>      : Write reports to file <name>. Default is "AREAPOL.TRA"'
    print *, '  -o -           : Write reports to standard output.'
    print *, '  -r <id>        : Include route <id> in report.'
    print *, '  -t <id>        : Include link type <id> in report.'
    print *, '  -w <min>,<max> : Report links with V/C between <min> and <max>.'
    call ExplainStdOptions

    STOP 02
 end subroutine
