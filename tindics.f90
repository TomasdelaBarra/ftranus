! ************************************************************************
! * TRANUS - Integrated Land Use and Transport Model
! * 
! * $Id: imptra.f90 159 2010-10-05 13:28:26Z juancarlo.anez $
! * 
! * Copyright (C) 1983-2010 Modelistica, Caracas
! * Copyright (C) 1983-2007 Tomas de la Barra
! * Copyright (C) 1985-2010 Juancarlo An~ez
! * Copyright (C) 1983-2002 Beatriz Perez
! * Some rights reserved.
! * 
! * (cc) This work is distrubuted under a Creative Commons
! *      Attribution-ShareAlike 2.0 license
! *      http://creativecommons.org/licenses/by-sa/2.0/
! ************************************************************************
MODULE TINDICS
USE CONTROL
USE DEBUGM
USE PARAM
USE RCOMM
USE TCOMM
USE IPCOMM
USE TPARC
USE GENER

    character(80), parameter :: IMPTRAMOD_RCS_ID = &
      '$Id$' 

    integer, parameter :: TRA_FMT_CLASSIC = 1
    integer, parameter :: TRA_FMT_CSV = 2

    real*8 :: COPassengers(MXPROP, MXOPER)
    real*8 :: OPassengers(MXOPER)
    real*8 :: CPassengers(MXPROP)
    real*8 :: TotalPassengers

    real*8 :: COBoardings(MXPROP, MXOPER)
    real*8 :: OBoardings(MXOPER)
    real*8 :: CBoardings(MXPROP)
    real*8 :: TotalBoardings

    real*8 :: COIncome(MXPROP, MXOPER)
    real*8 :: OIncome(MXOPER)
    real*8 :: CIncome(MXPROP)
    real*8 :: TotalIncome

    real*8 :: COTimeT(MXPROP, MXOPER)
    real*8 :: OTimeT(MXOPER)
    real*8 :: CTimeT(MXPROP)
    real*8 :: TotalTimeT

    real*8 :: COTimeE(MXPROP, MXOPER)
    real*8 :: OTimeE(MXOPER)
    real*8 :: CTimeE(MXPROP)
    real*8 :: TotalTimeE

    real*8 :: COTime(MXPROP, MXOPER)
    real*8 :: OTime(MXOPER)
    real*8 :: CTime(MXPROP)
    real*8 :: TotalTime

    real*8 :: COPasngKm(MXPROP, MXOPER)
    real*8 :: OPasngKm(MXOPER)
    real*8 :: CPasngKm(MXPROP)
    real*8 :: TotalPasngKm

    real :: TIEMPV(MXPROP),TIEMPE(MXPROP), TTOTV(MXPROP)
    real :: TOTREP(MXPROP),TOTM(MXMOD), TTOTM
    real :: TOTKM(MXPROP)
    real :: DISTPRO(MXPROP), COSPRO(MXPROP), TVIAPRO(MXPROP), TESPRO(MXPROP), TDESUT(MXPROP)
    real :: TVEHKM(MXOPER), TVEHOR(MXOPER)

      
CONTAINS

     SUBROUTINE IndicatorsByCategoryOperator(output, ipol, fmt)
        integer, intent(in) :: output
        integer, intent(in) :: ipol
        integer, intent(in), optional :: fmt

        real*8 :: time, pkm
        integer :: l, lnr, ir, io, ip, k
        character(MXPOLNAME) :: spol
        spol = NOMPOL(ipol)


         call OpenPolFile(10, IPOL, 'T5S', IO_BIN)
         call LeeLNRIndic(10)
         close(10)

         COBoardings = 0
         OBoardings = 0
         CBoardings = 0
         TotalBoardings = 0

         COIncome = 0
         OIncome = 0
         CIncome = 0
         TotalIncome = 0

         COTimeT = 0
         OTimeT = 0
         CTimeT = 0
         TotalTimeT = 0

         COTimeE = 0
         OTimeE = 0
         CTimeE = 0
         TotalTimeE = 0

         COTime = 0
         OTime = 0
         CTime = 0
         TotalTime = 0
         
         COPasngKm = 0
         OPasngKm = 0
         CPasngKm = 0
         TotalPasngKm = 0

         do ip=1, NPROP
           do lnr=1, NLINKRUT
              l  = abs(ilink(lnr))
              ir = abs(iroute(lnr))
              io = ioprut(ir)
              k  = modoper(io)

              if (.not. modo(ip,k)) continue

              COPassengers(ip, io) = COPassengers(ip, io) + LNRIndic(lnr,ip)%trips
              OPassengers(io) = OPassengers(io) + LNRIndic(lnr,ip)%trips
              CPassengers(ip) = CPassengers(ip) + LNRIndic(lnr,ip)%trips
              TotalPassengers = TotalPassengers + LNRIndic(lnr,ip)%trips

              COBoardings(ip, io) = COBoardings(ip, io) + LNRIndic(lnr,ip)%board
              OBoardings(io) = OBoardings(io) + LNRIndic(lnr,ip)%board
              CBoardings(ip) = CBoardings(ip) + LNRIndic(lnr,ip)%board
              TotalBoardings = TotalBoardings + LNRIndic(lnr,ip)%board

              COIncome(ip, io) = COIncome(ip, io) + LNRIndic(lnr,ip)%income
              OIncome(io) = OIncome(io) + LNRIndic(lnr,ip)%income
              CIncome(ip) = CIncome(ip) + LNRIndic(lnr,ip)%income
              TotalIncome = TotalIncome + LNRIndic(lnr,ip)%income

              if (dis(l) > 0) then
                 if (LRVel(lnr) > 0) then
                   time = LNRIndic(lnr,ip)%trips*dis(l) / LRVel(lnr)
                   COTimeT(ip, io) = COTimeT(ip, io) + time
                   OTimeT(io) = OTimeT(io) + time
                   CTimeT(ip) = CTimeT(ip) + time
                   TotalTimeT = TotalTimeT + time
                 endif

                 time = LNRIndic(lnr,ip)%board*Espera(lnr)
                 COTimeE(ip, io) = COTimeE(ip, io) + time
                 OTimeE(io) = OTimeE(io) + time
                 CTimeE(ip) = CTimeE(ip) + time
                 TotalTimeE = TotalTimeE + time
  
                 COTime(ip, io) = COTimeT(ip,io) + COTimeE(ip, io)
                 OTime(io) = OTimeT(io) + OTimeE(io) 
                 CTime(ip) = CTimeT(ip) + CTimeE(ip)
                 TotalTime = TotalTimeT + TotalTimeE
  
                 pkm = dis(l)*LNRIndic(lnr,ip)%trips
                 COPasngKm(ip, io) = COPasngKm(ip, io) + pkm
                 OPasngKm(io) = OPasngKm(io) + pkm
                 CPasngKm(ip) = CPasngKm(ip) + pkm
                 TotalPasngKm = TotalPasngKm + pkm
              endif
           enddo
         enddo
    
         if (.not.present(fmt) .or. fmt == TRA_FMT_CLASSIC) then
             call PrintClassicIndicatorsByCategoryOperator(output, spol)
         elseif (fmt == TRA_FMT_CSV) then
            call PrintCSVIndicatorsByCategoryOperator(output, spol)
         else
             STOP 'Unknown output format'
         end if
     END SUBROUTINE
         

     subroutine PrintClassicIndicatorsByCategoryOperator(output, spol)
         integer, intent(in) :: output
         character(*), intent(in) :: spol

         integer :: io, ip
         character(20) :: title

100      format(A21,100(A1,I4,1X,A16) )
200      format(I4,1X,A16,100(A1, F21.0) )
201      format(A21,100(A1, F21.0) )


         WRITE(output,*)
         WRITE(output,*)' BOARDINGS BY CATEGORY & OPERATOR'

         Title = 'CAT/OP'
         write(Output, 100) Title, (TAB, NUMOP(io),NOMOP(io), io=1, NOPER)
         do ip=1, NPROP
             write(Output, 200) NUMCAT(ip),NOMCAT(ip), (TAB, COBoardings(ip, io), io=1, NOPER), TAB, CBoardings(ip)
         enddo
         Title = 'TOTAL'
         write(Output, 201) Title, (TAB, OBoardings(io), io=1, NOPER), TAB, TotalBoardings

         WRITE(output,*)
         WRITE(output,*)' MONETARY COST BY CATEGORY & OPERATOR'
         Title = 'CAT/OP'
         write(Output, 100) Title, (TAB, NUMOP(io),NOMOP(io), io=1, NOPER)
         do ip=1, NPROP
             write(Output, 200) NUMCAT(ip),NOMCAT(ip), (TAB, COIncome(ip, io), io=1, NOPER), TAB, CIncome(ip)
         enddo
         Title = 'TOTAL'
         write(Output, 201) Title, (TAB, OIncome(io), io=1, NOPER), TAB, TotalIncome


         WRITE(output,*)
         WRITE(output,*)' PASSENGER TRAVEL TIME BY CATEGORY & OPERATOR'
         Title = 'CAT/OP'
         write(Output, 100) Title, (TAB, NUMOP(io),NOMOP(io), io=1, NOPER)
         do ip=1, NPROP
             write(Output, 200) NUMCAT(ip),NOMCAT(ip), (TAB, COTimeT(ip, io), io=1, NOPER), TAB, CTimeT(ip)
         enddo
         Title = 'TOTAL'
         write(Output, 201) Title, (TAB, OTimeT(io), io=1, NOPER), TAB, TotalTimeT


         WRITE(output,*)
         WRITE(output,*)' PASSENGER WAITING TIME BY CATEGORY & OPERATOR'
         Title = 'CAT/OP'
         write(Output, 100) Title, (TAB, NUMOP(io),NOMOP(io), io=1, NOPER)
         do ip=1, NPROP
             write(Output, 200) NUMCAT(ip),NOMCAT(ip), (TAB, COTimeE(ip, io), io=1, NOPER), TAB, CTimeE(ip)
         enddo
         Title = 'TOTAL'
         write(Output, 201) Title, (TAB, OTimeE(io), io=1, NOPER), TAB, TotalTimeE


         WRITE(output,*)
         WRITE(output,*)' PASSENGER TOTAL TIME BY CATEGORY & OPERATOR'
         Title = 'CAT/OP'
         write(Output, 100) Title, (TAB, NUMOP(io),NOMOP(io), io=1, NOPER)
         do ip=1, NPROP
             write(Output, 200) NUMCAT(ip),NOMCAT(ip), (TAB, COTime(ip, io), io=1, NOPER), TAB, CTime(ip)
         enddo
         Title = 'TOTAL'
         write(Output, 201) Title, (TAB, OTime(io), io=1, NOPER), TAB, TotalTime


         WRITE(output,*)
         WRITE(output,*)' PASSENGERS DISTANCE BY CATEGORY & OPERATOR'
         Title = 'CAT/OP'
         write(Output, 100) Title, (TAB, NUMOP(io),NOMOP(io), io=1, NOPER)
         do ip=1, NPROP
             write(Output, 200) NUMCAT(ip),NOMCAT(ip), (TAB, COPasngKm(ip, io), io=1, NOPER), TAB, CPasngKm(ip)
         enddo
         Title = 'TOTAL'
         write(Output, 201) Title, (TAB, OPasngKm(io), io=1, NOPER), TAB, TotalPasngKm

    end subroutine


    subroutine PrintCSVIndicatorsByCategoryOperator(output, spol)
        integer, intent(in) :: output
        character(*), intent(in) :: spol

        call PrintCSVCatOptIndicator(output, spol, 'Boardings',  COBoardings)
        call PrintCSVCatOptIndicator(output, spol, 'MonCost',    COIncome)
        call PrintCSVCatOptIndicator(output, spol, 'TravTime',   COTimeT)
        call PrintCSVCatOptIndicator(output, spol, 'WaitTime',   COTimeE)
        call PrintCSVCatOptIndicator(output, spol, 'TotalTime',  COTime)
        call PrintCSVCatOptIndicator(output, spol, 'Units-Dist', COPasngKm)
    end subroutine

    subroutine PrintCSVCatOptIndicator(output, spol, title, indic)
        integer, intent(in) :: output
        character(*), intent(in) :: spol, title
        real*8, intent(in) :: indic(MXPROP, MXOPER)

        integer :: ip, io
        character(24) :: val

300     format(2(A, ', '), 2(A, X, A, ', '), A)

         do ip=1, NPROP
              do io =1, NOPER
                 write(output, 300) &
                    trim(spol), &
                    trim(title) // ' by cat/op', &
                    trim(IntegerToString(NUMCAT(ip))), trim(NOMCAT(ip)), &
                    trim(IntegerToString(NUMOP(io))), trim(NOMOP(io)), &
                    trim(fmtval(indic(ip,io)))
              end do
         end do
    end subroutine


      SUBROUTINE OperatorVehicleIndicators(TVEHKM, TVEHOR)
        real :: TVEHKM(MXOPER), TVEHOR(MXOPER)
        integer :: LNR, L, IT, IR, IO
        real(8) :: VELOPER

        TVEHKM=0.
        TVEHOR=0.
        do LNR=1, NLINKRUT
          L  = ILINK(LNR)
          IT = ITIP(L)
          IR = IROUTE(LNR)
          IO = IOPRUT(IR)

          TVEHKM(IO)=TVEHKM(IO)+VEH(LNR)*DIS(L)

          VELOPER=VEL(L)*SPEED(IO,IT)   ! Velocidad final de la ruta
          TVEHOR(IO)=TVEHOR(IO)+VEH(LNR)*(DIS(L)/VELOPER)
        enddo
      END SUBROUTINE


      SUBROUTINE INDICS(output, ipol, fmt)
        integer, intent(in) :: output, ipol
        integer, intent(in), optional :: fmt


        integer(2) :: IDIA,MES,IAN,IHR,MINS
        integer :: status
        integer :: ncheck, mcheck, icheck, irec
        character(MXPOLNAME) :: spol
        spol = NOMPOL(ipol)

! Encabezamiento
      CALL RDTPAR(4,status,ITER,IAN,MES,IDIA,IHR,MINS)
      READ(4)CONV,CONV2,MAXOR,MAXDES,CONVEL,MAXVOR,MAXVDES

!  Viajes totales por categoria y modo

      TTOTV=0.
      NOMMOD(NTM+1)='TOTAL'
      READ(4) &
        ncheck,mcheck, &
        (irec, irec, irec, &
            (irec, &
                 TOTV(IP,K),REPRIMIDA(IP,K), &
             K=1,NTM), &
         irec, &
       IP=1,NPROP), &
       icheck
       call CheckRead(ncheck, mcheck, icheck, msg_IncorrectFileFormat)

       READ(4) &
        ncheck, mcheck, &
        (irec, &
           DISTAN(IP),CPROM(IP),TIEMPV(IP),TIEMPE(IP),TUTIL(IP), &
        IP=1, NPROP), &
        icheck

       call CheckRead(ncheck, mcheck, icheck, msg_IncorrectFileFormat)

       READ(4) &
       ncheck,mcheck, &
       (irec, &
            VIAJ(IP),PK(IP),EN(IP),COP(IP),OPING(IP), &
       IP=1,NOPER), &
       icheck
       call CheckRead(ncheck, mcheck, icheck, msg_IncorrectFileFormat)


!  kilometros, ingresos y costos por administrador

      READ(4) &
       ncheck,mcheck, &
       (irec, &
           DISTIP(K),  & ! Lee los kil�metros por tipo v�a
       K=1,NTIP), &
       icheck
       call CheckRead(ncheck, mcheck, icheck, msg_IncorrectFileFormat)

      READ(4) &
       ncheck,mcheck, &
       (irec, &
            CMAN(K),ADING(K), &
       K=1,ADMIN), & ! Costos e ingresos
       icheck
       call CheckRead(ncheck, mcheck, icheck, msg_IncorrectFileFormat)

      call OperatorVehicleIndicators(TVEHKM, TVEHOR)

      TOTM=0.
      TTOTM=0.
      DO IP=1,NPROP
         TOTREP(IP)=0.
         DO K=1,NTM
            TTOTV(IP)=TTOTV(IP)+TOTV(IP,K)
            TOTM(K)=TOTM(K)+TOTV(IP,K)
            TTOTM=TTOTM+TOTV(IP,K)
            IF(REPRIMIDA(IP,K).GT.TOTV(IP,K)+1.)THEN
               REPRIMIDA(IP,K)=REPRIMIDA(IP,K)-TOTV(IP,K)
               TOTREP(IP)=TOTREP(IP)+REPRIMIDA(IP,K)
            ELSE
               REPRIMIDA(IP,K)=0.
            ENDIF
         ENDDO
      ENDDO

      DISTPRO=0.
      COSPRO=0.
      TVIAPRO=0.
      TESPRO=0.
      TDESUT=0.
      DO IP=1,NPROP
         IF(TTOTV(IP).GT.0.)THEN
            DISTPRO(IP)=DISTAN(IP)/TTOTV(IP)
            COSPRO(IP)=CPROM(IP)/TTOTV(IP)
            TVIAPRO(IP)=TIEMPV(IP)/TTOTV(IP)
            TESPRO(IP)=TIEMPE(IP)/TTOTV(IP)
            TDESUT(IP)=TUTIL(IP)/TTOTV(IP)
         ENDIF
      ENDDO

      TOTKM=0.      ! Para sumar los km del administrador
      DO I=1,ADMIN
         DO K=1,NTIP
            IF(IAD(K).EQ.I)TOTKM(I)=TOTKM(I)+DISTIP(K)
         ENDDO
      ENDDO

         if (.not.present(fmt) .or. fmt == TRA_FMT_CLASSIC) then
             if (.not. suppressHeaders) then
                CALL MENSA(4017,2)  ! ObjConv  ConvFluj   ConvVel
                WRITE(output,'(1X,F8.5,3X,F8.5,'' ('',2I5,'')''' &
                    // ',3X,F8.5,''  ('',2I5,'')'')') &
                    CONV,CONV2,MAXOR,MAXDES,CONVEL,MAXVOR,MAXVDES
                WRITE(output,*)
             endif
             call PrintClassicIndicators(output, spol)
         elseif (fmt == TRA_FMT_CSV) then
            call PrintCSVIndicators(output, spol)
         else
            STOP 'Unknown output format'
         end if
    END SUBROUTINE

    subroutine PrintClassicIndicators(output, spol)
        integer, intent(in) :: output
        character(*), intent(in) :: spol

      CALL MENSA(4009,2)  ! VIAJES TOTALES POR CATEGORIA Y MODO...
      WRITE(output,'(100A)')'CatId',TAB,'CatName',(TAB,NOMMOD(K),K=1,NTM+1)

      DO IP=1,NPROP
         WRITE(output,'(I4,A,A8,20(A,F10.0))') &
            NUMCAT(IP),TAB,NOMCAT(IP),(TAB,TOTV(IP,K),K=1,NTM),TAB, &
           TTOTV(IP)
      ENDDO  ! Fin categorias IP

      WRITE(output,'(A,''TOTAL'',20(A,F10.0))')TAB, &
        (TAB,TOTM(K),K=1,NTM),TAB,TTOTM

!  Demanda reprimida por categoria

      WRITE(output,*)
      WRITE(output,*)
      WRITE(output,*)'SUPPRESSED DEMAND BY CATEGORY AND MODE'
      WRITE(output,'(100A)')'CatId',TAB,'CatName',TAB,'Trips',TAB,'% of Total'
      DO IP=1,NPROP
         PORCEN=0.
         IF(TOTREP(IP)+TTOTV(IP) > 0.) THEN
           PORCEN=TOTREP(IP)/(TOTREP(IP)+TTOTV(IP))
         ENDIF
         WRITE(output,'(I4,A,A8,A,F10.0,A,F5.2)') &
           NUMCAT(IP),TAB,NOMCAT(IP),TAB,TOTREP(IP),TAB,PORCEN
      ENDDO

!  Distancia, costo, tiempos y utilidad por categoria

      WRITE(output,*)
      WRITE(output,*)
      WRITE(output,*)'STATISTICS BY TRANSPORT CATEGORY (TOTALS)'
      WRITE(output,'(100A)')'CatId',TAB,'CatName',TAB,'Distance',TAB,'Cost',TAB, &
                'TravTime',TAB,'WaitTime',TAB, 'Disutil'
      DO IP=1,NPROP
         WRITE(output,'(I4,A,A8,5(A,F15.0))') &
            NUMCAT(IP),TAB,NOMCAT(IP),TAB,DISTAN(IP),TAB,CPROM(IP),TAB, &
            TIEMPV(IP),TAB,TIEMPE(IP),TAB,TUTIL(IP)
      ENDDO

!  Promedios por categoria

      WRITE(output,*)
      WRITE(output,*)
      WRITE(output,*)'STATISTICS BY TRANSPORT CATEGORY (AVERAGES)'
      WRITE(output,'(100A)')'CatId',TAB,'CatName',TAB,'Distance',TAB,'Cost',TAB, &
                'TravTime',TAB,'WaitTime',TAB,'Disutil'
      DO IP=1,NPROP
         WRITE(output,'(I4,A,A8,5(A,F17.2))') &
           NUMCAT(IP),TAB,NOMCAT(IP),TAB,DISTPRO(IP),TAB,COSPRO(IP),TAB,TVIAPRO(IP), &
           TAB,TESPRO(IP),TAB,TDESUT(IP)
      ENDDO

!  Viajes, unids-km, energ, costos e ingresos x oper

      WRITE(output,*)
      WRITE(output,*)
      WRITE(output,*)'STATISTICS BY TRANSPORT OPERATOR'
      WRITE(output,'(100A)')'OperId',TAB,'OperName',TAB,'Trips',TAB,'Units-Dist',  &
               TAB,'Energy',TAB,'Costs',TAB,'Income',TAB,'Revenue', &
                    TAB, 'VehKm',TAB, 'VehHrs'
      DO IP=1,NOPER
         WRITE(output,'(I4,A,A8,12(A,F15.0))') &
            NUMOP(IP),TAB,NOMOP(IP),TAB,VIAJ(IP),TAB,PK(IP),TAB,EN(IP),TAB, &
            COP(IP),TAB,OPING(IP),TAB,OPING(IP)-COP(IP), &
            TAB,TVEHKM(IP),TAB,TVEHOR(IP)
      ENDDO

      WRITE(output,*)
      WRITE(output,*)
      WRITE(output,*)'STATISTICS BY TRANSPORT ADMINISTRATOR'
      WRITE(output,'(100A)')'AdmId',TAB,'AdmName',TAB,'Length',TAB,'Income',TAB,'MaintCost',TAB, &
            'Revenue'
      DO I=1,ADMIN
         WRITE(output,'(I4,A,A8,4(A,F15.0))') &
            NUMAD(I),TAB,NOMAD(I),TAB,TOTKM(I),TAB,ADING(I),TAB,CMAN(I), &
            TAB,ADING(I)-CMAN(I)
      ENDDO
      END SUBROUTINE


    subroutine PrintCSVIndicators(output, spol)
        integer, intent(in) :: output
        character(*), intent(in) :: spol

200     format(A,4(', ',A))
        write(output, 200) &
          'Scenario', 'Table', 'Object', 'SubObject', 'Value'
300     format(2(A, ', '), 2(A, X, A, ', '), A)
        do ip=1,NPROP
            do k=1, NTM
                write(output, 300) &
                    trim(spol), &
                    'Trips by cat/mode', &
                    trim(IntegerToString(NUMCAT(ip))), trim(NOMCAT(ip)), &
                    trim(IntegerToString(NUMMOD(k))), trim(NOMMOD(k)), &
                    trim(fmtval(real(TOTV(ip,k), 8)))
            enddo
        enddo

        call PrintCSVCategoryIndicator(output, spol, 'Supressed', TOTREP)
        call PrintCSVCategoryIndicator(output, spol, 'Distance',  DISTAN)
        call PrintCSVCategoryIndicator(output, spol, 'Cost',      CPROM)
        call PrintCSVCategoryIndicator(output, spol, 'TravTime',  TIEMPV)
        call PrintCSVCategoryIndicator(output, spol, 'WaitTime',  TIEMPE)
        call PrintCSVCategoryIndicator(output, spol, 'Disutil',   TUTIL)

        call PrintCSVOperatorIndicator(output, spol, 'Boardings',  VIAJ)
        call PrintCSVOperatorIndicator(output, spol, 'Units-dist', PK)
        call PrintCSVOperatorIndicator(output, spol, 'Energy',     EN)
        call PrintCSVOperatorIndicator(output, spol, 'Cost',       COP)
        call PrintCSVOperatorIndicator(output, spol, 'Income',     OPING)
        call PrintCSVOperatorIndicator(output, spol, 'Veh-dist',   TVEHKM)
        call PrintCSVOperatorIndicator(output, spol, 'Veh-hour',   TVEHOR)

        call PrintCSVAdministratorIndicator(output, spol, 'Distance',  TOTKM)
        call PrintCSVAdministratorIndicator(output, spol, 'Income',    ADING)
        call PrintCSVAdministratorIndicator(output, spol, 'MaintCost', CMAN)
    end subroutine

    subroutine PrintCSVIndicator__(output, spol, title, n, num, nom, indic)
        integer, intent(in) :: output
        character(*), intent(in) :: spol, title
        integer, intent(in) :: n, num(n)
        character(*), intent(in) :: nom(n)
        real, intent(in) :: indic(n)

        integer :: i

400     format(2(A, ', '), (A, X, A, ', All, '), A)

        do i=1,n
            write(output, 400) &
                trim(spol), trim(title), &
                trim(IntegerToString(num(i))), trim(nom(i)), &
                trim(fmtval(real(indic(i),8)))
        enddo
    end subroutine
    

    subroutine PrintCSVCategoryIndicator(output, spol, title, indic)
        integer, intent(in) :: output
        character(*), intent(in) :: spol, title
        real, intent(in) :: indic(MXPROP)

        call PrintCSVIndicator__( &
                                output, spol, &
                                title // ' by cat', &
                                NPROP, NUMCAT, NOMCAT, &
                                indic )
    end subroutine

    subroutine PrintCSVOperatorIndicator(output, spol, title, indic)
        integer, intent(in) :: output
        character(*), intent(in) :: spol, title
        real, intent(in) :: indic(MXOPER)

        call PrintCSVIndicator__( &
                                output, spol, &
                                title // ' by oper', &
                                NOPER, NUMOP, NOMOP, &
                                indic )
    end subroutine

    subroutine PrintCSVAdministratorIndicator(output, spol, title, indic)
        integer, intent(in) :: output
        character(*), intent(in) :: spol, title
        real, intent(in) :: indic(MXADM)

        call PrintCSVIndicator__( &
                                output, spol, &
                                title // ' by admin', &
                                ADMIN, NUMAD, NOMAD, &
                                indic )
    end subroutine
END MODULE

