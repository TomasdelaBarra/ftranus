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
MODULE LPARC
USE PARAM
!  COMMON BLOCK /LPAR/:  PARAMETROS USO DEL SUELO
    character(80) :: LPARC_RCS_ID = & 
      "$Id$" 

      LOGICAL(1) :: LFLU
      CHARACTER(32) NOMSEC
      REAL         SELAS
      integer ::   NFLU,NIT,NS
      real ::      CONV,OSCMAX

      dimension :: ALFA(MXSEC,MXSEC,2),   XALFA(MXSEC,MXSEC,2),   &
                   XALFAPRO(MXSEC,MXSEC), XALFAPRE(MXSEC,MXSEC),  &
                   XALFACAP(MXSEC,MXSEC),                         &
                   DEMIN(MXSEC,MXSEC),    DEMAX(MXSEC,MXSEC),     &
                   DELAS(MXSEC,MXSEC),    SELAS(MXSEC,MXSEC),     &
                   SUSLgSc(MXSEC,MXSEC),                          &
                   LFLU(MXSEC),                                   &
                   BETA(MXSEC,2),         GAMA(MXSEC,2),          &
                   FactorAtrac(MXSEC),                            & !Exponente para los atractores
                   NOMSEC(MXSEC),         NUMSEC(MXSEC),          &
                   NSUST(MXSEC,MXSEC,MXSUST), &
                   SUSPEN(MXSEC,MXSEC),                           &
                   SECLgSc(MXSEC)  ! Escalamiento en logit 0=No..1=Si

!  ALFA   Par metros atracci¢n para los flujos (sector,sector,nivel)
!  BETA   Par metro funci¢n de utilidad para transporte (sector,nivel)
!  CONV   Criterio de convergencia
!  DELAS  Elasticidad de la demanda al precio (sector,sector)
!  SELAS  Elasticidad de la sustitucion en la demanda (sector,sector) 
!  SUSPEN Penalización de la sustitucion (sector,sector)
!  DEMAX  M ximo de la funci¢n de demanda (sector,sector)
!  DEMIN  M¡nimo de la funci¢n de demanda (sector,sector)
!  GAMA   Par metro funci¢n de utilidad para precio (sector,nivel)
!  LFLU   .T. si el sector genera flujos (sector)
!  NIT    N£mero m ximo de iteraciones
!  NOMSEC Nombre de cada sector (sector)
!  NS     N£mero total de sectores
!  NSUST  Sectores de consumo sustitutivo
!  NUMSEC N£mero externo de cada sector (sector)
!  OSCMAX Oscilaci¢n m xima permitida en los precios
!  XALFA  Par metros atracci¢n para prod.ex¢gena (sector,sector,nivel)


   ! Variables usadas en Complex Land Markets
   integer :: SectorType(MXSEC)          ! Tipo de sector (0=Normal, 1=Con Stock, 2=Transf)
   integer :: TargetSector(MXSEC)        ! Target sector for transforming sectors
   real    :: MinPriceToCostRatio(MXSEC) ! % minimo del costo de prod
                                         !   al que puede reducirse el precio de un sector

END MODULE LPARC
