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
MODULE LCOMM
USE PARAM
USE CONTROL
USE IO_LIST
USE LPARC
USE ZCOMM
USE MLOGIT
USE DEBUGM

      !COMMON BLOCKS PARA EL PROGRAMA LOC - VERSION 4.0

      character(80) :: LCOMM_RCS_ID = &
        "$Id$" 
      
      integer, parameter :: stNORMAL = 0, &
                            stSTOCK  = 1, &
                            stTRANSF = 2

      real(8) :: XPRO,PROAN,PRO,PRECIO,PREAN,AJUSTE,COSPRO,UTCON
      dimension :: &
                  XPRO(MXZON,MXSEC),   PROAN(MXZON,MXSEC), &
                  PRO(MXZON,MXSEC),    COSCON(MXZON,MXSEC), &
                  XDEM(MXZON,MXSEC),   DEM(MXZON,MXSEC),    &
                  PRECIO(MXZON,MXSEC), ATRAIN(MXZON,MXSEC), &
                  PREAN(MXZON,MXSEC),  AJUSTE(MXZON,MXSEC), &
                  ATRAC(MXZON,MXSEC),  COSPRO(MXZON,MXSEC), &
                  RMAX(MXZON,MXSEC),   RMIN(MXZON,MXSEC),   &
                  VALAG(MXZON,MXSEC),  UTCON(MXZON,MXSEC),  &
                  COSTRA(MXZON,MXZON), UTRA(MXZON,MXZON),   &
                  FLUJO(MXZON),                             &
                  TPRO(MXSEC),         CREPRO(MXSEC),       &
                  RESTOT(MXSEC),       CRECON(MXSEC),       &
                  CREMAX(MXSEC),       CREMIN(MXSEC),       &
                  prebase(MXZON,MXSEC),probase(MXZON,MXSEC)

      ! Land Markets, etc. Stock de un per�odo a otro
      real :: STOCK(MXZON, MXSEC)
      ! Cantidad del stock consumindos por sectores tipo 2, transformadores
      real :: UNSTOCK(MXZON, MXSEC)

      ! Decrecimiento de la producci�n
      REAL :: &
                  DREPRO(MXSEC),       &
                  DRECON(MXSEC),       &
                  DREMIN(MXSEC),       &
                  DREMAX(MXSEC),       &
                  DXPRO(MXZON, MXSEC), &
                  DXDEM(MXZON, MXSEC)

     logical :: IsExogSector(MXSEC)

     double precision :: DampFactor

!  ATRAC    Atractor (zona,sector)
!  AJUSTE   atractor calibrado por LCAL
!  COSTRA   Una fila de las desutilidades de transporte (zona)
!  CRECON   Crecimiento global del consumo ex�geno (sector)
!  CREMAX   Crecimiento global de restricci�n m�xima (sector)
!  CREMIN   Crecimiento global de restricci�n m�nima (sector)
!  CREPRO   Crecimiento global de producci�n ex�gena (sector)
!  DEM      Demanda inducida (zona,sector)
!  FLUJO    Una fila de los flujos (zona)
!  PREAN    Precio en la iteraci�n anterior (zona,sector)
!  PRECIO   Precio en la iteraci�n actual (zona,sector)
!  PRO      Producci�n inducida actual (zona,sector)
!  PROAN    Producci�n inducida en la iteraci�n anterior (zona,sector)
!  RENTAN   Renta de la iteraci�n anterior (zona,sector)
!  RESTOT   Suma de las restricciones m�ximas (sector)
!  RMAX     Restricci�n m�xima a la producci�n (zona,sector)
!  RMIN     Restricci�n m�nima a la producci�n (zona,sector)
!  TPRO     Producci�n total en la iteraci�n actual (sector)
!  XDEM     Demanda ex�gena (zona,sector)
!  XPRO     Producci�n ex�gena (zona,sector)
CONTAINS


      ! Escribir los parametros de uso del suelo
      SUBROUTINE WRLPAR(IUN,ITER,IDIA,MES,IAN,IHR,MINS)
      integer(2) :: IDIA,MES,IAN,IHR,MINS
      
       integer   status
       integer   i, j, k

!  Graba el primer registro con los par�metros de transporte en unidad IUN

      status = msg_WriteError
      ! Encabezamiento
      WRITE(IUN,ERR=999) FileMajor, FileMinor, FileRelease, ifmt_L1S
      WRITE(IUN,ERR=999) ITER,IAN,MES,IDIA,IHR,MINS
      WRITE(IUN,ERR=999) AREA,ESTUDIO,POL,NOMBRE

      ! Escenarios
      call WritePolInfo(iun)

      ! Sectores
      WRITE(IUN,ERR=999) NS,-NS, NFLU
        WRITE(IUN,ERR=999) (I, NUMSEC(I), NOMSEC(I), &
                               LFLU(I),              &
                               BETA(I,1),BETA(I,2),  &
                               GAMA(I,1),GAMA(I,2),  &
                               MinPriceToCostRatio(I), &
                               SectorType(I),          &
                               TargetSector(I),        &
                            I=1,NS)
      WRITE(IUN,ERR=999) NS

      ! funciones de demanda
      WRITE(IUN,ERR=999) NS,-NS
        WRITE(IUN,ERR=999) (I, &
                              (J, &
                                  DEMIN(I,J),DEMAX(I,J), &
                                  DELAS(I,J),SELAS(I,J), SUSLgSc(I,J), &
                                  XALFA(I,J,1),XALFA(I,J,2), &
                                  XALFAPRO(I,J),XALFAPRE(I,J), &
                                  XALFACAP(I,J), &
                                  ALFA(I,J,1),ALFA(I,J,2), &
                                  MXSUST, &
                                  (NSUST(I,J,K), &
                                   K=1,MXSUST), &
                              J=1,NS), &
                           I=1,NS)
      WRITE(IUN,ERR=999) NS
      status = msg_OK

999   call CheckStatus(status)
      RETURN
      END SUBROUTINE


      ! Escribir los parametros de uso del suelo
      SUBROUTINE RDLPAR(IUN,ITER,IDIA,MES,IAN,IHR,MINS)
      integer(2) :: IAN,MES,IDIA,IHR,MINS
       integer   status
       integer   i, j, k
       integer(2) :: lmayor,lmenor,lrev
       integer   fmt
       character(3)  spol
       character(80) snombre
       integer   icheck, mcheck, irec, itrash

!  Graba el primer registro con los par�metros de transporte en unidad IUN

      status = msg_ReadError
      ! Encabezamiento
      ! Datos de la version de TRANUS en VERSION.FD
      READ(IUN) lmayor,lmenor,lrev,fmt

      IF (lmayor.ne.FileMajor.or.lmenor.ne.FileMinor.or.lrev.ne.FileRelease) THEN
100     FORMAT('   v',I1,2('.',I2.2))
        WRITE(*,100)lmayor,lmenor,lrev
        call CheckStatus( msg_IncorrectFileVersion)
      ENDIF
      if (fmt.ne. ifmt_L1S) then
        call CheckStatus( msg_IncorrectFileFormat)
      ENDIF

      READ(IUN,ERR=999) ITER,IDIA,MES,IAN,IHR,MINS
      READ(IUN,ERR=999) AREA,ESTUDIO,SPOL,SNOMBRE

      ! Escenarios
      call SkipPolInfo(iun)

      ! Sectores
      READ(IUN,ERR=999) NS, mcheck, NFLU
        READ(IUN,ERR=999) (irec, NUMSEC(I), NOMSEC(I), &
                                 LFLU(I), &
                                 BETA(I,1),BETA(I,2), &
                                 GAMA(I,1),GAMA(I,2), &
                                 MinPriceToCostRatio(I), &
                                 SectorType(I),          &
                                 TargetSector(I),        &
                            I=1,NS)
      READ(IUN,ERR=999) icheck

      ! funciones de demanda
      READ(IUN,ERR=999) icheck,mcheck
        READ(IUN,ERR=999) (irec, &
                               (irec, &
                                   DEMIN(I,J),DEMAX(I,J), &
                                   DELAS(I,J),SELAS(I,J), SUSLgSc(I,J), &
                                   XALFA(I,J,1),XALFA(I,J,2), &
                                   XALFAPRO(I,J),XALFAPRE(I,J), &
                                   XALFACAP(I,J), &
                                   ALFA(I,J,1),ALFA(I,J,2), &
                                   itrash, &
                                   (NSUST(I,J,K), &
                                    K=1,MXSUST), &
                               J=1,NS), &
                            I=1,NS)
      READ(IUN,ERR=999) icheck
      status = msg_OK

999   call CheckStatus(status)
      RETURN
      END SUBROUTINE

      SUBROUTINE GRAL1S(IUN, ITER, DAY, MONTH, YEAR, HOUR, MINUTE)
       integer   IUN, ITER
       integer :: DAY, MONTH, YEAR, HOUR, MINUTE
       integer(2) :: IDIA,MES,IAN,IHR,MIN

       IDIA = DAY
       MES  = MONTH
       IAN  = YEAR
       IHR  = HOUR
       MIN  = MINUTE

!  Graba El Encabezamiento En L1S
      CALL WRLPAR(iun, ITER, IAN, MES, IDIA, IHR, MIN)

      WRITE(IUN,ERR=999) &
        NZN,-NZN, &
        NZ1,NZ2, &
        (I,NUMZON(I),NOMZON(I), JER1(I),JER2(I),I=1,NZN), &
        NZN


      WRITE(iun,ERR=999)NZN,NS
      DO I=1,NZN
          WRITE(iun,ERR=999)I,NUMZON(I)
          DO N=1,NS
            WRITE(iun,ERR=999) &
             N,XPRO(I,N),PROBASE(I,N),PRO(I,N),COSPRO(I,N), &
             PREBASE(I,N),PRECIO(I,N), &
             XDEM(I,N),DEM(I,N)-XDEM(I,N),COSCON(I,N), &
             UTCON(I,N),RMIN(I,N),RMAX(I,N),ATRAC(I,N),VALAG(I,N), &
             AJUSTE(I,N),ATRAIN(I,N), &
             STOCK(I,N), UNSTOCK(I,N)
         ENDDO ! N
      ENDDO ! I

      RETURN
999   CALL MENSA(1010,-1)  ! G06: Archivo incompleto
      END SUBROUTINE

! Restriccion de capacidad usada por LCAL y LOC
      SUBROUTINE RESTPRO(CONV, CVPREC,CVPROD,ITER,esLOC)
        logical esLOC
!     ==================

!  AJUSTA LOS PRECIOS POR RESTRICCION Y CALCULA CONVERGENCIA

!  REQUERIMIENTOS DE LA LIBRERIA TRANUS:
!    MENSA  (Subr)  Emite los mensajes

!  VARIABLES:
!  CVPREC   Peor convergencia-precios en esta iteraci�n
!  ICPREZ   Zona en la que se da la peor convergencia-precios
!  ICPRES   Sector en el que se da la peor convergencia-precios
!  CVPROD   Peor convergencia-producci�n en esta iteraci�n
!  ICPROS   Sector en el que se da la peor convergencia-producci�n
!  ICPROZ   Zona en la que se da la peor convergencia-producci�n
         REAL ::  conprean(MXSEC) = 0
         REAL ::  convAnt = 1
         real(8) :: prodReal, POSPRE,ANTEPRE, MinPrice
         real(8) :: damp, inertia

!  En las primeras iteraciones no ajusta precios
      ITCLAR=20

      CVPROD=0
      CVPREC=0
      if (debugging >= dbg_normal) then
         CALL MENSA(1009,0)  ! Indicadores de convergencia
      endif

!  Iteraciones para cada sector N

      damp = 1./(1.+DampFactor)

      DO 31 N=1,NS

         CONPRE=0
         CONPRO=0
         ICPREZ=ICERO
         ICPROZ=ICERO
         TXPRO=0
         TPROD=0

         DO 30 I=IUNO,NZN
            IF(ConSubZonas(I)) CYCLE ! no aplicar a macro-zonas
            TXPRO=TXPRO+XPRO(I,N)
            TPROD=TPROD+PRO(I,N)
            prodReal=pro(i,n)
            A=1.                          
            ANTEPRE=PRECIO(I,N)+AJUSTE(I,N)
            IF(ITER.LE.ITCLAR)GO TO 33
            ! La producci�n efectiva nunca es menor al stock
            PR=PRO(I,N)+XPRO(I,N)


            IF (IsExogSector(n) .or. ATRAIN(i,n) == 0) THEN
                !Nadie lo consume o nadie lo produce.
                !No afecta la cadena
                pospre  = cospro(i,n)
            ELSEIF(RMIN(I,N).LE.0.AND.RMAX(I,N).GE.RINF)THEN
                   ! sector no restringido
                if (esLOC) then
                    pospre = MAX(0.d+0, 1*cospro(i,n)+ajuste(i,n))
                else
                    pospre = cospro(i,n)
                endif
            ELSEIF(RMAX(i,n) < RINF .and. PR > RMAX(I,N) )THEN
                !  Caso de violaci�n de la restricci�n m�xima
                if (antepre == 0) then
                   pospre = 1
                elseif (RMAX(I,N) <= 0) then
                   pospre = 2*antepre
                else
                   pospre = antepre*PR/RMAX(I,N)
                endif
            ELSEIF(RMIN(i,n) > 0 .and. PR < RMIN(I,N) )THEN
                !  Caso de violaci�n de la restricci�n m�nima
                pospre=antepre*PR/RMIN(I,N)
            ELSE
                ! leave unchanged
                pospre  = antepre
            ENDIF
            if (MinPriceToCostRatio(n) > 0) then
               ! Verificar aqu� que el precio no sea menor que el m�nimo
               MinPrice = (cospro(i,n)-valag(i,n))*MinPriceToCostRatio(n)
               if (pospre < MinPrice) then
                  pospre = MinPrice
               endif
            endif

             
            inertia = cospro(i,n)
            IF (esLOC) THEN 
               inertia = cospro(i,n) + ajuste(i,n)
               pospre = inertia*conprean(n) + pospre * (1.-conprean(n))
            ELSE
               ! mientras peor es la convergencia, menos se aleja el precio
               ! del costo de producci�n
               pospre = inertia*conprean(n) + pospre * (1.-conprean(n))
            ENDIF
            pospre = antepre*(1.-damp) + pospre * damp

            if (pospre < 0) pospre = 0

            if(esLOC) then
                precio(i,n)=pospre-ajuste(i,n)
                IF(PRECIO(I,N) < 0.)PRECIO(I,N)=0
            else
               precio(i,n) = cospro(i,n)
               if (IsExogSector(n) .or. ATRAIN(i,n) == 0) then
                  !Nadie lo consume o nadie lo produce.
                  !No afecta la cadena
                  ajuste(i,n) = 0
               else
                  ajuste(i,n) = pospre-precio(i,n)
               endif
            endif

            if (precio(i,n)+ajuste(i,n)  < 0) then
               stop 'price < 0'
            endif

!  Calcula la convergencia de precios
            IF(ANTEPRE.NE.0) then
               a=(POSPRE-ANTEPRE)/ANTEPRE
            ELSEIF(POSPRE.NE.0) THEN
               a=(POSPRE-ANTEPRE)/POSPRE
            ELSE
               a=0
            ENDIF
33          aux=a
            IF(abs(AUX).GT.abs(CONPRE))THEN
               CONPRE=AUX
               ICPREZ=NUMZON(I)
            ENDIF
            aux=abs(aux)
            IF(AUX.GT.CVPREC)CVPREC=AUX

            convAnt = max(abs(CVPREC), abs(CONPRE))

!  Calcula la convergencia-producci�n

            AUX=0
            if(prodReal.ne.0.0) then
              AUX=(prodReal-PROAN(I,N))/prodReal
            elseif(PROAN(I,N).ne.0.0) then
              AUX=(prodReal-PROAN(I,N))/PROAN(I,N)
            else
              aux = 0
            endif
            IF(abs(AUX).GT.abs(CONPRO))THEN
               CONPRO=AUX
               ICPROZ=NUMZON(I)
            ENDIF
            aux=abs(aux)
            IF(AUX.GT.CVPROD)CVPROD=AUX

30       CONTINUE   !Fin de zona I

!  Cálculo de variables de Macrozona a partir de sus subzonas

      DO I=IUNO,NZN
         IF(I.LE.NZ1.AND.JER1(I).NE.JER2(I))THEN
            SUMA=0
            SUMD=0
            PRO(I,N)=0
            STOCK(I,N) = 0
            PRECIO(I,N)=0
            COSPRO(I,N)=0
            COSCON(I,N)=0
            UTCON(I,N)=0
            AJUSTE(I,N)=0
            DEM(I,N)=0
            DO J=JER1(I),JER2(I)
               PRO(I,N)=PRO(I,N)+PRO(J,N)
               DEM(I,N)=DEM(I,N)+DEM(J,N)
               STOCK(I,N) = STOCK(I,N)+STOCK(J,N)
               IF(PRO(J,N)+XPRO(J,N).GT.0)THEN
                 PRECIO(I,N)=PRECIO(I,N)+PRECIO(J,N)*(PRO(J,N)+XPRO(J,N))
                 AJUSTE(I,N)=AJUSTE(I,N)+AJUSTE(J,N)*(PRO(J,N)+XPRO(J,N))
                 COSPRO(I,N)=COSPRO(I,N)+COSPRO(J,N)*(PRO(J,N)+XPRO(J,N))
                 COSCON(I,N)=COSCON(I,N)+COSCON(J,N)*(DEM(J,N)+XDEM(J,N))
                 UTCON(I,N)=UTCON(I,N)+UTCON(J,N)*(DEM(J,N)+XDEM(J,N))
                 SUMA=SUMA+PRO(J,N)+XPRO(J,N)
                 SUMD=SUMD+DEM(J,N) ! DEM ya tiene XDEM
               ENDIF
            ENDDO  ! J de subzonas de I
            IF(SUMA.GT.0)THEN
               PRECIO(I,N)=PRECIO(I,N)/SUMA
               AJUSTE(I,N)=AJUSTE(I,N)/SUMA
               COSPRO(I,N)=COSPRO(I,N)/SUMA
            ENDIF
            IF(SUMD.GT.0)THEN
               COSCON(I,N)=COSCON(I,N)/SUMD
               UTCON(I,N)=UTCON(I,N)/SUMD
            ENDIF
         ENDIF !Zona desagregada
         PROAN(I,N)=PRO(I,N)
         PREAN(I,N)=PRECIO(I,N)
      ENDDO  ! Fin zona I

!  Emite mensajes de convergencia por sector

      if (debugging >= dbg_normal) then
          WRITE(*,'(I4,1X,A8,F10.6,I5,2X,F10.6,I5,2X,2F10.0,F10.6)') &
            NUMSEC(N),NOMSEC(N),CONPRE,ICPREZ,CONPRO,ICPROZ,TXPRO,TPROD
      endif

      conprean(n) = min(0.99, abs(conpre))

31    CONTINUE   ! Fin de sector N
      if (debugging >= dbg_normal) then
        WRITE(*,*)'_________________________________________'
        WRITE(*,'('' Iter'',I6)')ITER
      endif

      RETURN  ! Regresa al MAIN para evaluaci�n de convergencia
      END SUBROUTINE


      SUBROUTINE DEMANDA(M)
!     ==================

      INTEGER M
      real(8) ::  consUnitario, consTotal, demPotencial


      do i=1,NZN
        UNSTOCK(i,m)=0 
        if (conSubZonas(i)) CYCLE
        dem(i,m)=0.
        if(.not.ZExter(i)) then
           if (.not.lflu(m).and.atrain(i,m).le.0) CYCLE
           do n=1,NS   

             CALL CONSUMO (I,M,N,consUnitario, demPotencial)

             COSPRO(I,N)=COSPRO(I,N)+consUnitario*COSCON(I,M)
             ! El m�ximo entre el stock y la producci�n calculada
             ! determinan la demanda
             consTotal = MAX(1.d+0*STOCK(I,N),PRO(I,N)+XPRO(I,N))*consUnitario
             DEM(I,M)=DEM(I,M)+consTotal 
             if (SectorType(N) == stTRANSF & 
             .and. SectorType(M) == stSTOCK &
             .and. consTotal > 0) then
                ! Los sectores tipo 2 reducen el stock en el siguiente per�odo
                UNSTOCK(I,M) = UNSTOCK(I,M) + consTotal
             endif
           enddo ! N sectores demandantes
        endif
        dem(i,m)=dem(i,m)+xdem(i,m)
      enddo ! I zonas totales

      RETURN
      END SUBROUTINE


      SUBROUTINE CONSUMO (I,M,N, consUnitario, demPotencial)
!     ==================

      INTEGER I,M,N
      real(8) ::  consUnitario, demPotencial
      real(8) ::  prob(MXSEC),coef(MXSEC),ut
      real(8) ::  scost(MXSEC), cgen
      INTEGER scount, is
      REAL    satrac(MXSEC)
      REAL    melas,xelas,celas

        if(demin(m,n)+demax(m,n).le.0) then
            demPotencial = 0
            consUnitario = 0
            return
        endif

          satrac=1.

          melas = -delas(m,n)
          celas = melas/150.0
          xelas = 0.

          if(demax(m,n).le.0.)then ! Beatriz agregaesto
             coef(1)= demin(m,n)    ! y esto
          else
             coef(1)=  demin(m,n) +demax(m,n)*exp(melas*utcon(i,m))
          endif
          scost(1)=utcon(i,m)*coef(1)*SusPen(m,n)    
          if (.not.lflu(m)) satrac(1)=atrain(i,m)

          scount=1
          do is=1,MXSUST
            m1=nsust(m,n,is)
            if(m1.eq.0) EXIT
            if (.not.lflu(m1).and.atrain(i,m1).le.0) CYCLE

            scount=scount+1
            if (.not.lflu(m1)) satrac(scount)=atrain(i,m1)

            ut=utcon(i,m1)
            melas = -delas(m1,n)
            celas = melas/150.0
            xelas = 0.
            if(demax(m1,n).le.0.)then    ! Beatriz agrega esto
               coef(scount)= demin(m1,n) ! con esto
            else
               coef(scount)= demin(m1,n) +demax(m1,n)*exp(melas*ut)
            endif
            scost(scount)= ut*coef(scount)*SusPen(m1,n)

         enddo ! is sustitutos

         call logit( scount, selas(m,n), SUSLgSc(m,n),satrac, scost, prob, cgen)

         if (cgen.lt.0) then
           ! ERROR
           write(*,*)
           write(*,'('' Substitutes From Sec:'',I4,'' To Sec:'',I4,'' Zon:'',I4)') &
             numsec(m), numsec(n),numzon(i)
           call DistribError(cgen)
         endif
         demPotencial=coef(1)
         consUnitario = coef(1)*prob(1)
      RETURN
      END SUBROUTINE

! --- DISTRIB  -----------------------------------------------------------
!     Distribuir la demanda del sector N a la zona I a las zonas de
!     produccion J.  Si el sector no genera flujos, asigna la demanda
!     a la propia zona
!     Si la zona de origen i es interna, distribuye la producci�n a todas
!     las zonas j;  si no, s�lo a las zonas internas (exportaciones)
!     Factor s�lo transportable dentro de sus propias subzonas (Beta<0)
!     JER1 de una subzona tiene el No. de la macrozona a la que pertenece
!     JER1 de una zona no desagregada contiene el No. de ella misma
!     JER1 de una zona desagregada contiene el No. de su 1a. subzona
!     Las zonas desagregadas no caen en esta rutina, pero si sus subzonas
! -------------------------------------------------------------------------
      SUBROUTINE DISTRIB(i,m)
      INTEGER i,m
    !  requerimientos de la libreria tranus
      real(8) ::  util(mxzon)   ! desutilidad por zona
      real(8) ::  prob(mxzon) ! probabilidades de distribucion de
                          !   todas las zonas
      INTEGER numz1       ! numero de zonas nivel 1 a las que
                          !   se distribuye la demanda
      REAL    a(MXZON)
      real(8) ::  p(MXZON),c(MXZON)
      INTEGER iz(MXZON), inx
      INTEGER j1,j2
      logical(1) :: distriA(MXZON)

      if(ConSubzonas(i)) then
         WRITE(*,*) 'DIST : ConSubzonas(i), can''t happen'
         call mensa(3333,-1)
      endif

      do j1=1, nzn
        distriA(j1)=.not.esSubZona(j1).and.atrac(j1,m).gt.0.and. &
                      .not.(zExter(i).and.zExter(j1))
      enddo
      util  =0
      prob=0

    !  Si el sector no genera flujos, asigna la demanda a la propia zona
      if(.not.lflu(m))then
        pro(i,m) = dem(i,m)
        utcon(i,m)=precio(i,m)+ajuste(i,m)
        return
      endif

    !  zonas con distribucion solo interna
    ! solo se distribuye a su propia macro zona
      if(beta(m,1).le.0) then
         j1 = jer1(i)
         call probint(i,j1,m,util,prob)
         utcon(i,m)=util(j1)
      else
       ! distribuir a todas las zonas de primer nivel
       ! busca la desutilidad minima
        umin1=rinf
        numz1=0
        do j1=1,nzn
          if(distriA(j1)) then
             numz1=numz1+1
             call probint(i,j1,m,util,prob)
             iz(numz1)=j1
             c(numz1)=util(j1)
             a(numz1)=atrac(j1,m)
             if(c(numz1).lt.0)then
               write(*,*)' zone', numzon(numz1), ' sector', numsec(m)
               write(*,*)' disutility less than zero:', c(numz1)
               write(*,*)' transport desut',utra(i,j1),' price', &
                precio(j1,m), ' shadow', ajuste(j1,m), ' gama',gama(m,1)
                stop 1
             endif
          endif
        enddo

        call logit(numz1, beta(m,1), SECLgSc(m),a, c, p, utcon(i,m))
        if (utcon(i,m).lt.0) then
          ! ERROR
          write(*,*)
          write(*,'('' Distribution Sec:'', I4,'' Zone:'',I4)') &
                        numsec(m), numzon(i)
          do j1=1, nzn
             write(*,*) NumZon(j1), distriA(j1), esSubZona(j1), atrac(j1,m), zExter(j1)
          enddo
          call DistribError(utcon(i,m))
        endif

        ! desagregar los datos del logit
        do inx=1,numz1
           util(iz(inx))=c(inx)
           prob(iz(inx))=p(inx)
        enddo
      endif

    ! distribucion a zonas de primer nivel
      do j1=1, nzn
        if(.not.EsSubZona(j1)) then
          flujo(j1)=dem(i,m)*prob(j1)
          if(numsec(m).gt.0) pro(j1,m)=pro(j1,m)+flujo(j1)
        endif
      enddo

    ! distribucion a zonas de segundo nivel
      do j2=nz1+1, nz2
        flujo(j2)=flujo(jer1(j2))*prob(j2)
        if(numsec(m).gt.0) pro(j2,m)=pro(j2,m)+flujo(j2)
      enddo
      RETURN
      END SUBROUTINE
! --- DIST ----------------------------------------------------




! --- PROBINT ----------------------------------------------------
!    El costo agregado de un sector en una macro-zona.  Para ello
        ! calcula las probabilidades de distribucion interna para
        ! una zona de primer nivel
        ! si no tiene subzonas, se toma la desutilidad directa
! ----------------------------------------------------------------
      SUBROUTINE PROBINT( i, j, m, util, prob)
       INTEGER  i           !  zona de origen
       INTEGER  j           !  macro-zona de destino
       INTEGER  m           !  sector economico en la zona j
       real(8) ::   util(mxzon)   !  desutilidad por zona
       real(8) ::   prob(mxzon) !  probabilidades de distribucion de
                            !   todas las zonas

           INTEGER j2
           REAL   a(MXZON)
           real(8) :: p(MXZON),c(MXZON)
           INTEGER iz(MXZON)
           INTEGER numz2, inx

           if(.not.lflu(m)) then
               WRITE(*,*) 'PROBINT : not lflu(m), can''t happen'
               call mensa(3333,-1)
           endif
           if(EsSubZona(j)) then
               WRITE(*,*) 'PROBINT : EsSubZona(j), can''t happen'
               call mensa(3333,-1)
           endif

         ! calcular las probabilidades de  distribucion interna
           if (.not.ConSubzonas(j)) then
                 ! no tiene sub zonas, costo directo
                 ! se usa gama de primer nivel
             util(j)=utra(i,j)+(precio(j,m)+ajuste(j,m))*gama(m,1)
           else
            !    si tiene subzonas
            ! agrupar las sub zonas
              umin2=rinf
              numz2=0
              do j2=jer1(j),jer2(j)
                if(atrac(j2,m).le.0) cycle
                numz2=numz2+1
                util(j2)=utra(i,j2)+(precio(j2,m)+ajuste(j2,m))*gama(m,2)
                iz(numz2)=j2  ! guardar la zona a la que corresponde
                c(numz2)=util(j2)
                a(numz2)=atrac(j2,m)
  
             if(c(numz2).lt.0)then
                write(*,*)' zone', numzon(numz2), ' sector', numsec(m)
                write(*,*)' disutility less than zero:', c(numz2)
                write(*,*)' transport desut',utra(i,j2),' price', &
                   precio(j2,m), ' shadow', ajuste(j2,m), ' gama',gama(m,2)
                stop 1
             endif
  
                
                
              enddo

              call logit(numz2,beta(m,2),SECLgSc(m),a,c,p,util(j))
              if (util(j).lt.0) then
                ! ERROR
                write(*,*)
                write(*,'('' Distribution Level 2 Sec:'', I4,' &
                        // ''' Zone:'',I4, '' Sub Zone:'',I4)') &
                        numsec(m), numzon(i), numzon(j)
                call DistribError(util(j))
              endif

           ! desagrupar los datos de las subzonas
              do inx=1, numz2
                util(iz(inx))=c(inx)
                prob(iz(inx))=p(inx)
              enddo
           endif
        return
      END SUBROUTINE PROBINT


      SUBROUTINE LEEL1E
!     =================

!  LECTURA DE PARAMETROS DEL ARCHVIO L1E

!  REQUERIMIENTOS DE LA LIBRERIA:
!    CHECK  (Subr)    Verifica los finales de secci�n
!    INTNUM (Fun)     Busca el n�mero interno de una categor�a externa
!    MENSA  (Subr)    Emite los mensajes
      INTEGER M2(MXSUST)
      CHARACTER*32 NOM
      INTEGER ISUS, IST, ITSec

!  INICIALIZACION DE VARIABLES

      NIT=30
      CONV=0.001
      OSCMAX=0.2
      NSUST=ICERO
      NFLU=ICERO
      LFLU=.FALSE.
      BETA=0
      GAMA=0
      DEMIN=0
      DEMAX=0
      DELAS=0
      SELAS=0
      SUSLgSc=1.
      SUSPEN=1.
      XALFA=0
      ALFA=0
      FactorAtrac = 1
      IsExogSector = .TRUE.

!  SECCION 1.0 - PARAMETROS GLOBALES
!     NIT    N�mero m�ximo de iteraciones
!     CONV   Criterio de convergencia
!     OSCMAX Oscilaci�n m�xima permitida en los precios

      READ(3,'(////)',END=999)
      READ(3,*,END=999,ERR=100,IOSTAT=IOS)NIT,CONV,DampFactor
      IF(NIT.LE.ICERO)NIT=20
      GO TO 101
100   CALL CHECK(1.0,IOS,'L1E')

!  SECCION 2.1 - PARAMETROS DE LAS FUNCIONES DE UTILIDAD
!     NUMSEC   N�mero externo del sector (sector)
!     NOMSEC   Nombre del sector (sector)
!     BETA     Par�metro costo de transporte (sector, nivel)
!     GAMA     Par�metro precio (sector, nivel)
!     NS       N�mero de sectores

101   READ(3,'(////)',END=999)
      NS=ICERO
210   A1=0
      A2=0
      A3=0
      A4=0
      A5=0
      A6 = 0
      IST=0
      ITSec = 0
      FAtrac = 1
      READ(3,*,END=999,ERR=211,IOSTAT=IOS)I1,NOM,A1,A2,A3,A4,A5,FAtrac,A6,IST,ITSec
      NS=NS+1
      IF(NS.GT.MXSEC)THEN
         WRITE(*,*)'L1E(2.1) - NSect:',NS,'   Max:',MXSEC
         CALL MENSA(9,-1)  ! Excedida dimension maxima
      ENDIF
      NUMSEC(NS)=I1
      NOMSEC(NS)=NOM
      BETA(NS,1)= A1
      BETA(NS,2)= A2
      GAMA(NS,1)= A3
      GAMA(NS,2)= A4
      SECLgSc(NS) = A5
      MinPriceToCostRatio(NS) = A6
      FactorAtrac(NS) = FAtrac
      SectorType(NS) = IST
      IF(A5.lt.0.or.A5.gt.1)THEN
         WRITE(*,*)'L1E(2.1) - NSect:',NS,'   Logit Sc:',A5
         CALL MENSA(9,-1)  ! Excedida dimension maxima
      ENDIF
      IF(A6.lt.0.or.A6.gt.2)THEN
         WRITE(*,*)'L1E(2.1) - NSect:',NS,'   Price-Cost Ratio:',A6
         CALL MENSA(9,-1)  ! Excedida dimension maxima
      ENDIF
      IF(IST.lt.0.or.IST.gt.2)THEN
         WRITE(*,*)'L1E(2.1) - NSect:',NS,'   Sector Type:',IST
         CALL MENSA(9,-1)  ! Excedida dimension maxima
      ENDIF
!!!      TargetSector(NS) = INTNUM(ITSec, NUMSEC, NS)
!!!      if (TargetSector(NS) >= NS)TargetSector(NS) = 0
!!!      if (SectorType(NS) == stTRANSF .and. TargetSector(NS) == stNORMAL) then
!!!         WRITE(*,*)'L1E(2.1) - NSect:',NS,'   Target Sector:',ITSec
!!!         CALL MENSA(9,-1)  ! Excedida dimension maxima
!!!      endif
      GO TO 210
211   CALL CHECK(2.1,IOS,'L1E')
      IF(NS.LE.ICERO)THEN
         WRITE(*,*)'L1E(2.1)'
         CALL MENSA(15,-1)   ! Esta seccion no puede quedar vacia
      ENDIF

!  SECCION 2.2 - PARAMETROS DE LAS FUNCIONES DE DEMANDA
!     DEMIN    Demanda m�nima (sector, sector)
!     DEMAX    Demanda m�xima (sector, sector)
!              Apenas leido se le resta DEMIN por que as� se usa
!     DELAS    Elasticidad al precio (sector, sector)
!     SELAS    Elasticidad a la sustitucion (sector, sector)
!     SUSPEN   Penalizacion a la sustitucion (sector,sector)
!     LFLU     .T. si el sector tiene funciones que lo demanden,
!              o si el sector tiene parametro de transporte positivo
!     NFLU     es el n�mero de sectores que generan flujos
!     NSUST    sectores sustitutivos

      READ(3,'(/)',END=999)
      NSUST=ICERO
220   M2=0  ! Limpia variable auxiliar de sustitutos para evitar basura
      A2=0. ! evita ingresar demanda m�xima en coeficientes fijos
      A3=0. ! evita ingresar elasticidad en coeficientes fijos
      A4=0. ! si el usuario no los coloca en el archivo

      READ(3,*,END=999,ERR=221,IOSTAT=IOS)J1,I1,A1,A2,A3,A4, &
                                         (M2(K),K=1,MXSUST)
      if(A2.eq.0.)A2=A1    ! Demanda maxima igual a la minima
      I=INTNUM(I1,NUMSEC,NS)
      IF(I.GT.NS)THEN
         WRITE(*,*)'L1E(2.2) - Sect:',I1
         CALL MENSA(6,-1)
      ENDIF
      IsExogSector(I) = .FALSE.
      J=INTNUM(J1,NUMSEC,NS)
      IF(J.GT.NS)THEN
         WRITE(*,*)'L1E(2.2) - Sect:',J1
         CALL MENSA(6,-1)
      ENDIF
      IF(A2.EQ.0.)A2=A1 ! si no se introdujo demanda m�xima
      IF(A2.LT.A1)THEN
         WRITE(*,*)'L1E(2.2) - Sect:',J1
         CALL MENSA(6,-1)
      ENDIF
      if (SectorType(J) == stTRANSF .and. SectorType(I) /= stSTOCK) then
         WRITE(*,*)'L1E(2.2) - Sect:',J1, 'Input:', I1
         WRITE(*,*)'Transforming sector can only consume Stock sectors'
         CALL MENSA(9,-1)  ! Excedida dimension maxima
      endif
      if (SectorType(I) == stTRANSF .and. SectorType(J) /= stSTOCK) then
         WRITE(*,*)'L1E(2.2) - Sect:',J1, 'Input:', I1
         WRITE(*,*)'Transforming sectors can only be consumed by Stock sectors'
         CALL MENSA(9,-1)  ! Excedida dimension maxima
      endif
      DEMIN(I,J)=A1
      DEMAX(I,J)=A2-A1
      DELAS(I,J)=A3
      SELAS(I,J)=A4
! Guarda en NSUST los sectores sustitutos de I para el sector j
      ISUS=0
      DO K=1,MXSUST
         IF(M2(K).EQ.ICERO)CYCLE
         M=INTNUM(M2(K),NUMSEC,NS)
         IF(M.GT.NS)THEN
            WRITE(*,*)'L1E(2.2) - Sect:',M2(K)
            CALL MENSA(6,-1)
         ENDIF
         IF(M.EQ.I) then
            CYCLE ! Sustituto de si mismo, no lo guardamos
         endif
         ISUS=ISUS+1
         NSUST(I,J,ISUS)=M
      ENDDO

      GO TO 220
221   CALL CHECK(2.2,IOS,'L1E')
!  Calcula LFLU y NFLU
      NFLU=ICERO
      DO 222 I=1,NS
      LFLU(I)=.FALSE.
      IF(BETA(I,1).ne.0 .or. BETA(I,2).ne.0)THEN
         DO 223 J=1,NS
         IF(DEMIN(I,J)+DEMAX(I,J).GT.CEROMAS)THEN
            LFLU(I)=.TRUE.
            NFLU=NFLU+1
            GO TO 222
         ENDIF
223      CONTINUE
         WRITE(*,*)'Sect:',NUMSEC(I)
         CALL MENSA(1031,-1)
      ENDIF

222   CONTINUE

      if (debugging >= dbg_verbose) then
          ! Reportar sectores exogenos y no-transportables por pantalla
          write(*,*)
          do I=1,NS
            if (IsExogSector(I)) then
                write(*,9222) NumSec(I), ' Exogenous'
            elseif (.not. LFLU(I)) then
                write(*,9222) NumSec(I), ' Non-transportable'
            endif
          enddo
      endif
9222  format(' Sec:', I6, A)
      write(*,*)

! ********************************************************
!  SECCION 2.3 - SUBSTITUCIONES
!
!
!
!  Seccion 2.3 Substituciones
      Sect = GetNextFileSection(3)
      if (Sect.eq.2.3) then
         call ReadSubst(3)
         call CHECKIU(3, 2.3,IOS,'L1E')
      else
         backspace(3)
      endif


!  SECCION 3.1 - ATRACTORES INCREMENTOS DE PRODUCCION EXOGENA
!  XALFA  Par�metro (sec origen, sec des, nivel)

      READ(3,'(//)',END=999)
310   A1=0.
      A2=0.
      A3=0.
      A4=0.
      A5=0.
      READ(3,*,END=999,ERR=311,IOSTAT=IOS)I1,J1,A1,A2,A3,A4,A5
      I=INTNUM(I1,NUMSEC,NS)
      IF(I.GT.NS)THEN
         WRITE(*,*)'L1E(3.1) - Sect:',I1
         CALL MENSA(6,-1)
      ENDIF
      J=INTNUM(J1,NUMSEC,NS)
      IF(J.GT.NS)THEN
         WRITE(*,*)'L1E(3.1) - Sect:',J1
         CALL MENSA(6,-1)
      ENDIF
      XALFA(I,J,1)=A1
      XALFA(I,J,2)=A2
      XALFAPRO(I,J)=A3
      XALFAPRE(I,J)=A4
      XALFACAP(I,J)=A5
      GO TO 310
311   CALL CHECK(3.1,IOS,'L1E')


!  SECCION 3.2 - ATRACTORES PARA LOS FLUJOS DE PRODUCCION

      READ(3,'(/)',END=999)
320   A1=0.
      A2=0.
      READ(3,*,END=999,ERR=321,IOSTAT=IOS)I1,J1,A1,A2   
      I=INTNUM(I1,NUMSEC,NS)
      IF(I.GT.NS)THEN
         WRITE(*,*)'L1E(3.2) - Sect:',I1
         CALL MENSA(6,-1)
      ENDIF
      J=INTNUM(J1,NUMSEC,NS)
      IF(J.GT.NS)THEN
         WRITE(*,*)'L1E(3.2) - Sect:',J1
         CALL MENSA(6,-1)
      ENDIF
      ALFA(I,J,1)=A1
      ALFA(I,J,2)=A2
      GO TO 320
321   CALL CHECK(3.2,IOS,'L1E')

      RETURN
999   CALL MENSA(10,-1)  ! Archivo incompleto
      END SUBROUTINE LEEL1E

      SUBROUTINE ReadSubst(iUn)
        integer iUn
        integer ns1, ns2(mxsust), n1, n2(mxsust)
        real    elas, pen(mxsust), LgSc

        ! blanquear matrices que van a ser leidas
        SusPen = 1
        Seelas = 0
        NSust  = 0

        read(iUn, *) ! Saltar la l�nea de t�tulos
        do while (.TRUE.)
          ns2=0
          pen=0.
          elas=0.
          LgSc=1.
          read(iUn,*,END=499,ERR=431,IOSTAT=IOS)ns1,elas,LgSc, (ns2(i), pen(i),i=1,mxsust)
          n1 = INTNUM(ns1, NumSec, NS)
          if(n1.gt.NS) then
               WRITE(*,*)'L1E(2.3) - Sec:',ns1
               CALL MENSA(6,-IUNO)  ! G04: Definici�n ilegal
          endif
          if (LgSc.lt.0.or.LgSc.gt.1) then
             write(*,*)'L1E(2.3) - Sect:',ns1, ' LgSc:',LgSc
             call mensa(6,-1)
          endif
          n2 = 0
          do j=1,mxsust
            if (ns2(j).eq.0) exit

            n2(j) = INTNUM(ns2(j), NumSec, NS)
            if(n2(j).gt.NS) then
               WRITE(*,*)'L1E(2.3) - Input:',ns2(j)
               CALL MENSA(6,-1)  ! G04: Definici�n ilegal
            endif
          enddo ! j substitutos

          do j=1,mxsust
               if(n2(j).eq.0)exit
               if(n2(j).eq.n1) cycle !! Error del usuario
               SusPen(n2(j), n1)  = Pen(j)
               Selas(n2(j),n1)    = Elas
               SUSLgSC(n2(j), n1) = LgSc
               isus = 0
               do k = 1, mxsust !k - los sustitutos de consumir n2(j)
                  if(n2(k).eq.0) exit
                  if(k.eq.j) cycle
                  if(n2(k).eq.n2(j)) cycle !! Error del usuario

                  isus=isus+1
                  NSust(n2(j),n1, isus)=n2(k)
               enddo
          enddo ! j
        enddo ! while true
431     continue ! Error
499     continue ! End
      RETURN
      END SUBROUTINE


      SUBROUTINE LEEL1S(iun, IT, NewStocks,IDIA,MES,IAN,IHR,MIN)
!     =================
        integer, intent(in)  :: iun
        integer, intent(out) :: it
        logical, intent(in)  :: NewStocks
        integer, optional, intent(out) :: idia, mes, ian, ihr, min
        INTEGER   I,check, mcheck, irec, inum, status
        integer(2) :: i1,i2,i3,i4,i5


!  LEE EL ARCHIVO DE RESULTADOS DE L1S
!  per�odo anterios si ejecuci�n nueva; per�odo actual si adicional

!  REQUERIMIENTOS DE LA LIBRERIA:
!    MENSA  (Subr)    Emite los mensajes

!  LEE ENCABEZAMIENTO

      CALL RDLPAR(iun, IT, I1, I2, I3, I4, I5)
      if (present(MIN)) then
        idia = i1
        mes  = i2
        ian  = i3
        ihr  = i4
        min  = i5
      endif

!  SECCION UNICA - PRODUCCION EXOGENA E INDUCIDA Y PRECIOS

      ! Zonas
         status = msg_ErrorReadingZones
      READ(IUN,ERR=999) &
       NZN,mcheck, &
       NZ1,NZ2, &
       (irec,NUMZON(I),NOMZON(I),JER1(I),JER2(I),I=1,NZN), &
       check
      call checkRead(nzn,mcheck,check, status)
      call ZonSubZon() ! en GENER.FOR

      READ(iun,END=999,ERR=999) check,mcheck
      if (check.ne.NZN.or.mcheck.ne.NS) then
            call Mensa(msg_IncorrectFileFormat, mensa_Aborta)
      endif
      DO I=1,NZN
         READ(iun,END=999,ERR=999)irec,inum
         if(irec.ne.I) then
            call Mensa(msg_IncorrectFileFormat, mensa_Aborta)
         endif
         DO N=1,NS
            READ(iun,END=999,ERR=999) &
               irec,XPRO(I,N),PROBASE(I,N),PRO(I,N),COSPRO(I,N), &
               PREBASE(I,N),PRECIO(I,N), &
               XDEM(I,N),DEM(I,N),COSCON(I,N), &
               UTCON(I,N),RMIN(I,N),RMAX(I,N),ATRAC(I,N),VALAG(I,N), &
               AJUSTE(I,N),ATRAIN(I,N), &
               STOCK(I,N), UNSTOCK(I,N)
               if(irec.ne.N) then
                  call Mensa(msg_IncorrectFileFormat, mensa_Aborta)
               endif
               PREAN(I,N)=PRECIO(I,N)
               PROAN(I,N)=PRO(I,N)
         ENDDO ! I
      ENDDO ! N
      status = msg_OK

888   Call CheckStatus(status)
      if (NewStocks) then
         CALL CalculateNewStocks
      endif
      RETURN
999   CALL MENSA(1010,-1)  ! G06: Archivo incompleto
      END SUBROUTINE LEEL1S

      SUBROUTINE LOCIN(M, LastIter)
        logical :: LastIter
!  PARA CADA SECTOR M DISTRIBUYE LA DEMANDA A
!  ZONAS DE PRODUCCION, CALCULA LOS COSTOS DE CONSUMO Y GRABA LOS FLUJOS

        logical MicroZone

!  Iteraciones respecto a cada zona de origen I

      if (LastIter .and. LFLU(M)) call WriteListBegin(4, NZN)

      DO 20 I=1,NZN
        MicroZone = (JER1(I).eq.JER2(I))
        if (LFLU(M)) then
            if(LastIter) call WriteListItem(4, i)
            if(LastIter) write(4) MicroZone  ! para ObjTranus
        endif
        if(.not.MicroZone) CYCLE
        COSPRO(I,M)=COSPRO(I,M)+VALAG(I,M)

!  Inicializa el vector de flujos en cero

      DO J=1,NZN
         FLUJO(J)=0.
      END DO

!  Llama a la rutina de distribuci�n si la demanda > 0.

      IF(DEM(I,M).GT.CEROMAS)THEN
         CALL DISTRIB(I,M)

!  Calculo costos y utilidades de consumo para la pr�xima iteraci�n

         IF(LFLU(M))THEN ! Factor transportable
            AUX= 0.
            DO J=1,NZN
               IF(JER1(J).NE.JER2(J))CYCLE
               AUX=AUX+FLUJO(J)*(COSTRA(I,J)+PRECIO(J,M))
               !!!propuesta Juanca 1999/03/05: AUX=AUX+FLUJO(J)*(COSTRA(I,J)+PRECIO(J,M)+AJUSTE(J,M))
            ENDDO  ! J
            COSCON(I,M)=AUX/DEM(I,M)
            ! La utilidad de consumo la calcula DISTRIB
         ELSE  ! Factor no transportable
            COSCON(I,M)=PRECIO(I,M)
            !!!propuesta Juanca 1999/03/05: COSCON(I,M)=PRECIO(I,M)+AJUSTE(I,M)
         ENDIF  ! LFLU
      ENDIF ! Factores con demanda

!  Graba el vector de flujos, si es que el sector M genera flujos
      if(LastIter) then
         IF(LFLU(M)) then
            do j=1, NZN
               WRITE(4)FLUJO(J)
            enddo
          endif
      endif
20    CONTINUE
      if (LastIter .and. LFLU(M)) call WriteListEnd(4, NZN)

      RETURN
      END SUBROUTINE

      SUBROUTINE CalculateNewStocks
      integer :: N, I

      DO N=1,NS
          if (SectorType(N) == stSTOCK) then
             DO I=1,NZN
                STOCK(I,N) = MAX(1.d+0*STOCK(I,N), PRO(I,N)+XPRO(I,N)) - UNSTOCK(I,N)
             ENDDO
          endif
      ENDDO
!!! Un sector transformador retira un tipo de stock para que otro lo consuma
!!! no hace falta tener un TargetSector. El target sector es el sector de stock
!!! que consume al transformador y por lo tanto aumenta su stock
!!!      DO N=1,NS
!!!          if (SectorType(N) == 2) then
!!!             DO I=1,NZN
!!!                M = TargetSector(N)
!!!                STOCK(I,M) = STOCK(I,M) + PRO(I,N) + XPRO(I,N)
!!!             ENDDO
!!!          endif
!!!      ENDDO
      DO N=1,NS
          if (SectorType(N) == stSTOCK) then
             DO I=1,NZN
                STOCK(I,N) = min(STOCK(I,N), RMAX(I,N))
                if (STOCK(I,N) < RMIN(I,N)) then
                   !!! OJO: Esto como que debe ser CAN'T HAPPEN
                   STOCK(I,N) = RMIN(I,N)
                endif
             ENDDO
          endif
      ENDDO
      
      END SUBROUTINE CalculateNewStocks

END MODULE LCOMM
