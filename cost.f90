! ************************************************************************
! * TRANUS - Integrated Land Use and Transport Model
! * 
! * $Id$
! * 
! * Copyright (C) 1983-2010 Modelistica, Caracas
! * Copyright (C) 1983-2010 Tomas de la Barra
! * Copyright (C) 1985-2010 Juancarlo Añez
! * Copyright (C) 1983-2002 Beatriz Perez
! * Some rights reserved.
! * 
! * (cc) This work is distrubuted under a Creative Commons
! *      Attribution-ShareAlike 2.0 license
! *      http://creativecommons.org/licenses/by-sa/2.0/
! ************************************************************************
program Cost
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

      character(80) :: COST_RCS_ID = & 
         "$Id$" 

      LOGICAL HAYFLUJOS
      CHARACTER IIP*1
      INTEGER   IPOL
      INTEGER   status
      LOGICAL   LFLAG
      integer :: IAN,MES,IDIA
      integer :: IHR,MINS,ISEC,MILESM
      integer(2) :: I2

      real :: COSMON(MXZON,MXZON),DEMAN(MXSEC),BETALOC(MXSEC)
      real(8) :: COSTRA(MXZON,MXZON),AA8
      
!
!  FILES
!
!  Unit  Name    Type Format    Contenido
!     1   MENSA.DAT      yes     Messages
!     3    L1E     in    yes     Land-use parameters
!     3    F1E     in    yes     Data and parameters for flows, including external trips
!     3    T1S     in     no     Costs by transport categories
!     4    C1S     out    no     Costs by socioeconomic sectors

!  REQUIREMENTS FROM THE TRANUS LIBRARY:
!    ABRE    (Subr)  Opens and OLD file with or without format
!    INIT    (Subr)  Initilizes TRANUS programs
!    CONTROL (Subr)  Reads control file CONTROL.DAT
!    LEEZ1E  (Subr)  Reads zones file Z1E
!    LEEF1E  (Subr)  Reads parameters file F1E
!    CHECK   (Subr)  Verifies end-of-section
!    TIEJEC  (Subr)  Calculates running time of a program
!    INTNUM  (Func)  Calculates an internal number corresponding to an external one
!    MENSA   (Subr)  Emite los mensajes

      CALL INIT(POL,IIP,.FALSE.,6000)
      call InitPol(POL,IPOL)
      CLOSE(3)
!
!  READS ZONES FILE Z1E

      call GetCurrentDate(IAN,MES,IDIA)
      call GetCurrentTime(IHR, MINS, ISEC, MILESM)
      if (debugging >= dbg_normal) then
          WRITE(*,*)'_________________________________________'
          CALL MENSA(1004,0)  ! READING PARAMETERS AND DATA
      endif
      call OpenPolFile(3, 0, 'Z1E', IO_FMT)
      CALL LEEZ1E
      CLOSE(3)

!  Reads L1E to indentify land-use sectors
!  LFLU is .T. if sector generates flows, ie if P1 or P2 are positive
!  NFLU is the number of sectors that generate flows
!  Read distribution parameters BETALOC to aggregate disutilities

      call OpenPolFile(3, IPOL, 'L1E', IO_FMT)
!  Section 1
      READ(3,'(////)')
      READ(3,*,END=333,ERR=100,IOSTAT=IOS)
      GO TO 101
100   CALL CHECK(1.0,IOS,'L1E')
!  Secci�n 2.1
101   READ(3,'(////)',END=333)
      NS=1
210   P1=0
      P2=0
      READ(3,*,END=333,ERR=2111,IOSTAT=IOS)NUMSEC(NS),NOMSEC(NS),P1,P2
      DEMAN(NS)=RCERO
      BETALOC(NS)=P1+P2   ! only to find out if sector is transportable
      NS=NS+1
      GO TO 210
2111  CALL CHECK(2.1,IOS,'L1E')
      NS=NS-1
!  Secci�n 2.2
      READ(3,'(/)',END=333)
2220  P1=0
      P2=0
      READ(3,*,END=333,ERR=221,IOSTAT=IOS)I,J1,P1,P2
      J=INTNUM(J1,NUMSEC,NS)
      IF(J.GT.NS)THEN
         WRITE(*,*)'L1E(2.2) - Sect:',J1
         CALL MENSA(6,-1)  ! ERROR G04: Illegal definition
      ENDIF
      DEMAN(J)=DEMAN(J)+P1+P2
      GO TO 2220
221   CALL CHECK(2.2,IOS,'L1E')
!  Calculates LFLU and NFLU (LFLU is .T. if sector generates flows)
      NFLU=ICERO
      DO 2222 I=1,NS
      LFLU(I)=.FALSE.
      IF(BETALOC(I).GT.CEROMAS.AND.DEMAN(I).GT.CEROMAS)THEN
         LFLU(I)=.TRUE.
         NFLU=NFLU+1
      ENDIF
2222  CONTINUE
      CLOSE (3)

!  READS HEADING OF T1S TO IDENTIFY TRANSPORT CATEGORIES AND OTHERS

      if (debugging >= dbg_Debug) print *, 'T1S First Read'
      call OpenPolFile(3, IPOL, 'T1S', IO_BIN)
      CALL RDTPAR(3,status,I,I2,I2,I2,I2,I2)   ! Dummy for ITER1,IAN,MES,IDIA,IHR,MINS,
      CLOSE(3)

!  READS PARAMETERS FILE F1E

      HAYFLUJOS=.TRUE.
      call FindPolFile(3, IPOL, 'F1E', IO_FMT, pol_Any)
      CALL LEEF1E(HAYFLUJOS)
      CLOSE(3)
      if (debugging >= dbg_normal) then
          WRITE(*,*)'_________________________________________'
      endif

!  OPENS INPUT AND OUTPUT FILES

      call OpenPolFile(3, IPOL, 'T1S', IO_BIN)
      call NewPolFile(4, IPOL, 'C1S', IO_BIN)
      WRITE(4)NZN,NFLU,int2(IAN),int2(MES),int2(IDIA),int2(IHR),int2(MINS)
      call WriteListBegin(4, NS)
      do ip=1, NS
        call WriteListItem(4, ip)
        write(4) NUMSEC(IP),NOMSEC(IP),LFLU(IP)
      enddo
      call WriteListEnd(4, ns)

!  LOOPS OVER SOCIOECONOMIC SECTORS M

      call WriteListBegin(4, NS)
      DO 500 M=1,NS  
         call WriteListItem(4, M)
         LFLAG = LFLU(M)
         write(4) LFLAG 
         IF(.NOT.LFLU(M))GO TO 500
         if (debugging >= dbg_normal) then      
             WRITE(*,'('' Sectr'',I6,3X,A)')NUMSEC(M),NOMSEC(M)
         endif

!  Initiates matrix of costs by socioeconomic sector

      DO I=1,NZN
         DO J=1,NZN
            COSMON(I,J)=RCERO
            FLUSEC(I,J)=RCERO
         ENDDO
      ENDDO

      if (debugging >= dbg_Debug) print *, 'T1S RDTPAR'
!  Reads heading of T1S
      CALL RDTPAR(3,status,I,I2,I2,I2,I2,I2)   ! Dummy for ITER1,IAN,MES,IDIA,IHR,MINS,

!  Iterates over transport categories IP
!  FVOLUM default=1

      call CheckListBegin(3, NPROP)
      DO 510 IP=1,NPROP  
         call CheckListItem(3, ip)
         IF(TIPO(IP,M))FTIEMP(IP,M)=1. 
           PROUT(IP,M)=PROUT(IP,M)* FVOLUM(IP,M)
           PROIN(IP,M)=PROIN(IP,M)* FVOLUM(IP,M)

!  Reads matrices of costs of category IP

         call CheckListBegin(3, NZN, 'zones i')
         DO I=1,NZN
           if (debugging >= dbg_Debug) print *, 'T1S I', I
           call CheckListItem(3, i, 'i')
           call CheckListBegin(3, NZN, 'zones j')
           do j=1,NZN
             if (debugging >= dbg_Debug) print *, 'T1S I,J', I,J
              call CheckListItem(3, j, 'j')
              READ(3,END=888,ERR=888)FLUTRA(I,J)
              if(flutra(i,j).ge.rinf.or.flutra(i,j).lt.0) then
                 write(*,*) ' LECTURA de T1S', i,j,flutra(i,j)
              endif
              READ(3,END=888,ERR=888)COSTRA(I,J)
              if (debugging >= dbg_Debug) print *, 'T1S I,J to modes', I,J
              call CheckListBegin(3, NTM, 'modes')
              DO K=1,NTM
                 call CheckListItem(3, k, 'k')
                 READ(3,END=888,ERR=888)AA8
              ENDDO
              call CheckListEnd(3, NTM,'end modes')
           enddo
           call CheckListEnd(3, NZN, 'end zones j')
         ENDDO
         call CheckListEnd(3, NZN, 'end zones i')

!  Transformation from transport category to socioeconomic sector
!  Only if  FVOLUM > 0
         DO I=1,NZN
            IF(JER1(I).NE.JER2(I))CYCLE
            DO J=1,NZN
               COSMON(I,J)=COSMON(I,J)+COSTRA(I,J)*PROUT(IP,M) &
               *FTIEMP(IP,M)+COSTRA(J,I)*PROIN(IP,M)*FTIEMP(IP,M)
            if(flutra(i,j).ge.rinf.or.flutra(i,j).lt.0) then
            write(*,*) i,j,flutra(i,j),prout(ip,m),proin(ip,m)
            endif
               FLUSEC(I,J)=FLUSEC(I,J)+FLUTRA(I,J)*PROUT(IP,M) &
               *FTIEMP(IP,M)+FLUTRA(J,I)*PROIN(IP,M)*FTIEMP(IP,M)
            ENDDO
         ENDDO


510   CONTINUE  !End of category IP
      call CheckListEnd(3, NPROP)

!  Calculates costs of Macrozones from subzones

      DO I=1,NZN
         DO J=1,NZN
         IF (J.EQ.I)CYCLE
            IF(JER1(I).EQ.JER2(I))THEN 
               IF(JER1(J).NE.JER2(J))THEN 
               IF(JER1(I).EQ.J)CYCLE 
                  NJ=ICERO 
                  CJ=RCERO
                  SUMJ=RCERO
                  DO J1=JER1(J),JER2(J)
                     NJ=NJ+1
                     CJ=CJ+COSMON(I,J1)
                     SUMJ=SUMJ+FLUSEC(I,J1)
                  ENDDO ! J1
                  FLUSEC(I,J)=SUMJ/NJ
                  COSMON(I,J)=CJ/NJ
               ENDIF  ! JER1(J).NE.JER2(J)
            ELSE  ! JER1(I).EQ.JER2(I)
               IF(JER1(J).EQ.I)CYCLE
               NI=ICERO 
               CI=RCERO
               SUMI=RCERO
               DO I1=JER1(I),JER2(I)
                  IF(JER1(J).NE.JER2(J))THEN 
                     NI1=ICERO
                     CI1=RCERO
                     SUMI1=RCERO
                     DO J1=JER1(J),JER2(J)
                        NI1=NI1+1
                        CI1=CI1+COSMON(I1,J1)
                        SUMI1=SUMI1+FLUSEC(I1,J1)
                     ENDDO  ! J1
                     COSMON(I1,J)=CI1/NI1
                     FLUSEC(I1,J)=SUMI1/NI1
                  ENDIF  !JER1(J).NE.JER2(J)
                     NI=NI+1
                     CI=CI+COSMON(I1,J)
                     SUMI=SUMI+FLUSEC(I1,J)
               ENDDO  ! I1
               COSMON(I,J)=CI/NI
               FLUSEC(I,J)=SUMI/NI
            ENDIF  ! JER1(I).EQ.JER2(I)
         ENDDO ! J
      ENDDO ! I

!  Internal cost of zones

      DO I=1,NZ1
         A=RINF
         B=RINF
         DO J=1,NZ1
            IF(I.EQ.J)CYCLE
            IF(FLUSEC(I,J).LT.A) A=FLUSEC(I,J)
            IF(COSMON(I,J).LT.B) B=COSMON(I,J)
         ENDDO
         FLUSEC(I,I)=A*ZonCosIn(I)
         COSMON(I,I)=B*ZonCosIn(I)
      ENDDO

!  Internal cost of disaggregated zones

      DO I1=1,NZ1
         IF(JER1(I1).EQ.JER2(I1))CYCLE
         DO I=JER1(I1),JER2(I1)
            A=RINF
            B=RINF
            DO J=JER1(I1),JER2(I1)
               IF(I.EQ.J)CYCLE
               IF(FLUSEC(I,J).LT.A) A=FLUSEC(I,J)
               IF(COSMON(I,J).LT.B) B=COSMON(I,J)
            ENDDO
            FLUSEC(I,I)=A*COSIN2
            COSMON(I,I)=B*COSIN2
         ENDDO
      ENDDO

!  Outputs matrices by socioeconomic sector

      call WriteListBegin(4,NZN)
      do I=1,NZN
         call WriteListItem(4, i)
         do j=1,NZN
            WRITE(4)FLUSEC(I,J)
            WRITE(4)COSMON(I,J)
         enddo
      enddo
      call WriteListEnd(4, NZN)

      REWIND(3)
500   CONTINUE    ! End DO socioeconomic categories M
      call WriteListEnd(4, NS)

      if (debugging >= dbg_normal) then
          WRITE(*,*)'_________________________________________'
          CALL TIEJEC(IHR,MINS,ISEC,MILESM)
          CALL MENSA(8,0)  !  Normal end of
          WRITE(*,*)' C O S T'
      endif
      STOP
888   WRITE(*,*)'T1S'
      CALL MENSA(3,-1)  ! ERROR G03: Reading problem 
333   WRITE(*,*)'L1E'
      CALL MENSA(10,-1) ! ERROR G06: Incomplete file
999   WRITE(*,*)'T1E'
      CALL MENSA(10,-1) ! ERROR G06: Incomplete file
END PROGRAM COST

