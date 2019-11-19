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
MODULE WAITING
USE PARAM
    character(80) :: WAITING_RCS_ID = & 
      "$Id$" 

integer, parameter :: MaxFact = 150
double precision :: FactTable(0:MaxFact) 
logical :: MadeFactTable = .false.


CONTAINS


subroutine MakeFactTable()

    integer :: i

    if ( .not. MadeFactTable) then
        FactTable(0) = 1
        do i = 1, MaxFact
            FactTable(i) = i * FactTable(i - 1)
        enddo
        MadeFactTable = .True.
    end if
end subroutine MakeFactTable

double precision function Fact(i)
integer, intent(in) :: i
    double precision :: R
    integer :: K

    call MakeFactTable
    R = 1
    K = i
    do while (K > MaxFact)
        R = R * K
        K = K - 1
    enddo
    Fact = R * FactTable(K)
end function Fact

double precision &
function Poisson(K, LT)
integer, intent(in) :: K
double precision :: LT

    if (LT < 0.00001) then
       if (K == 0) then
          Poisson = 1
       else
          Poisson = 0
       endif
    else
       Poisson = (LT ** K) * Exp(-LT) / Fact(K)
    endif
end function Poisson

double precision &
function WaitForK(K, Dem, Puestos, Freq)
integer, intent(in) :: K
double precision, intent(in) :: Dem, Puestos, Freq

    double precision :: T, Espera
    integer ::          N

    T = 1 / Freq

    N   = (K / Puestos)

    ! SUMA(i*T), i=0...N-1
    Espera = T * N*(N-1)/2

    ! Probabilidad por espera promedio
    WaitForK = Poisson(K, T * Dem) * Espera

end function WaitForK

double precision &
function WaitTime(Dem, Puestos, Freq)
double precision, intent(in) :: Dem, Puestos, Freq

    integer :: K
    double precision :: W, WK, N, F, P

    if (Dem < 0.0001) then
      WaitTime = 0
      RETURN
    endif


    W = 0
    WK = 0

    ! Escalar todo para evitar errores numericos
    N = 1
    F = Freq
    P = Puestos

    if (P <= 0) then
        P= 0.0001
    endif
    do while (P > 8)
        F = F * 2
        P = P / 2
        N = N * 2
    enddo

    do K = 1, MaxFact
        WK = (WaitForK(K, Dem, P, F) + WK * (K - 1)) / K
        W = W + WK
    enddo

    WaitTime = N * W
End Function




double precision function TomasQueueTime(Vol, Freq, SeatsHr, Period)
  real, intent(in) :: Vol
  real, intent(in) :: Freq
  real, intent(in) :: SeatsHr
  real, intent(in) :: Period

  double precision, parameter :: Alfa = 2.0, Beta = 0.2, Gamma = 2.0

  double precision :: T, W, VolHr
  double precision :: WInf, Ratio

  Winf = 2*Period
  VolHr = Vol / Period
  if (Freq <= 0 .or. SeatsHr <= 0) then 
      W = WInf
  else
      T     = 1 / Freq
      Ratio = VolHr / SeatsHr

      if (Ratio > 2 .or. Ratio > 1.3 .and. Freq > 30) then
         W = WInf
      else
         W = T * Alfa * Ratio**(Beta * (Freq ** Gamma))
         if(W > WInf) W = WInf
      endif
  endif
  TomasQueueTime = W  
end function TomasQueueTime

double precision function NewQueueTime(Vol, Freq, SeatsHr, Period)
  real, intent(in) :: Vol
  real, intent(in) :: Freq
  real, intent(in) :: SeatsHr
  real, intent(in) :: Period

  double precision :: D, P, F, W

  D = Vol / Period
  P = SeatsHr / Freq
  F = Freq

  W = WaitTime(D, P, F)

  if (W > 1.0E20*Period) then
    W = 1.E20*Period
  endif

  NewQueueTime = W
end function NewQueueTime

double precision function FindNewFreq(Vol, SeatsHr, Period, Wq)
  real, intent(in) :: Vol
  real, intent(in) :: SeatsHr
  real, intent(in) :: Period
  real, intent(in) :: Wq

  real :: F, P, W

    F = Vol / (Period * SeatsHr) !Ratio 1:1
    P = F
    do while ((F > 0) .and. (P > 0.00001))
        W = NewQueueTime(Vol, F, SeatsHr, Period)
        do while (W > Wq)
            F = F + P
            W = NewQueueTime(Vol, F, SeatsHr, Period)
        end do
        P = P / 2
        F = F - P
    end do
    FindNewFreq = F
end function FindNewFreq

END MODULE WAITING
    
