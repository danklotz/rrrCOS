C***********************************************************************
!> # Run
!>    - subroutines to compute runoff of subbasin
!>    - This subroutine represents the sufarce-flow module of the
!!      CODIRO Model consisting of a linear reservoir with two outlets.
!!      An exact analytical solution is used.
!!      If necessary the model time-step is split into two parts
!!      T1 and T2, with different states (water level above or below
!!      outlet height)
C***********************************************************************
      subroutine toy(timesteps, par, bw1_in, bw2_in, q_in, q_out)
c ----------------------------------------------------------------------
c pre definitions:
        implicit none
        ! inputs:
        integer, intent(in) :: timesteps
        real(kind = 8), intent(in), dimension(4) :: par
        real(kind = 8), intent(in), dimension(timesteps) :: q_in
        ! mixed:
        real(kind = 8) :: bw1_in, bw2_in
        ! internal variables:
        real(kind = 8) :: k1, k2, k3, h
        real(kind = 8) :: qab1_out, qab2_out
        real(kind = 8) :: qvs1_out, qvs2_out
        real(kind = 8) :: bw1_out, bw2_out
        integer :: t
        ! out:
        real(kind = 8), intent(out), dimension(timesteps) :: q_out

c ----------------------------------------------------------------------

c ----------------------------------------------------------------------
c code:
        k1 = par(1)
        k2 = par(2)
        h = par(3)
        k3 = par(4)
        bw1_out = bw1_in
        bw2_out = bw2_in
        do t = 1, timesteps
          bw1_in = bw1_out
          bw2_in = bw2_out
          call route1(k1, k2, h, q_in(t), bw1_in,
     o                qab1_out, qvs1_out, bw1_out)
          call route2(0.0d+0, k3, qvs1_out, bw2_in,
     o                qab2_out, bw2_out)
          q_out(t) = qab1_out + qab2_out
        enddo
        return
      end subroutine

C***********************************************************************
!> # rout1
!>    - subroutines to compute runoff of subbasin
!>    - This subroutine represents the sufarce-flow module of the
!!      CODIRO Model consisting of a linear reservoir with two outlets.
!!      An exact analytical solution is used.
!!      If necessary the model time-step is split into two parts
!!      T1 and T2, with different states (water level above or below
!!      outlet height)
C***********************************************************************
       subroutine route1(k1, k2, h, q_in, bw_in,
     o                    qab_out,qvs_out,bw_out)
c ----------------------------------------------------------------------
c pre definitions:
        implicit none
        !
        real(kind = 8), intent(in) :: k1, k2, h
        real(kind = 8), intent(in) :: q_in, bw_in
c       local variables:
        integer :: IZ
        real(kind = 8) :: bw_mod
        real(kind = 8) :: QAB_T1, QVS_T1, QAB_T2, QVS_T2
        real(kind = 8) :: dt, T, t1, T2
        real(kind = 8) :: V1, V2
c       output:
        real(kind = 8), intent(out) :: qab_out, qvs_out, bw_out
c ----------------------------------------------------------------------

c ----------------------------------------------------------------------
c code:
c      presets
       ! check if module is used:
       if (k2 <= 0.d+0) then
         qab_out = 0.d+0
         qvs_out = q_in
         goto 10     ! skip modelling of reservoir
       endif
       ! set variables:
       dt = 1.d+0
       V1 = 1.d+0/K1 + 1.d+0/K2
       V2 = q_in + H/K1
       bw_mod = bw_in
c      initial reservoir level above outlet H
       if (bw_mod > h) then
         if (H/K2 > q_in) then
           T1 = -1.d+0/V1 * log(max(0.d+0,
     +                             ((V2-H*V1)/(V2-bw_mod*V1))))
           T1 = min(T1,DT)
         else
           T1 = DT
         endif
         T2 = DT - T1
         T = T1
         QAB_T1 = V2*T/(V1*K1) -
     +        (V2-bw_mod*V1)/(V1**2*K1)*(1.-exp(-T*V1)) -
     +        H*T/K1
         QVS_T1 = V2*T/(V1*K2) -
     +        (V2-bw_mod*V1)/(V1**2*K2)*(1.-exp(-T*V1))
         if (T2 > 0.d+0) then
           bw_mod = bw_mod + q_in*T1 - QAB_T1 - QVS_T1
           T = T2
           QAB_T2 = 0.d+0
           QVS_T2 = q_in*T - (K2*q_in-bw_mod)*(1.-exp(-T/K2))
         else
           QAB_T2 = 0.d+0
           QVS_T2 = 0.d+0
         endif
       else
c      initial reservoir level below outlet H *****
         if (H/K2.lt.q_in) then
           T1 = K2*log((q_in-bw_mod/K2)/(q_in-H/K2))
           T1 = min(T1,DT)
         else
           T1 = DT
         endif
         T2 = DT - T1
         T = T1
         QAB_T1 = 0.d+0
         QVS_T1 = q_in*T - (K2*q_in-bw_mod)*(1.-exp(-T/K2))
         if (T2 > 0.d+0) then
           bw_mod = bw_mod + q_in*T1 - QAB_T1 - QVS_T1
           T = T2
           QAB_T2 = V2*T/(V1*K1) -
     +       (V2-bw_mod*V1)/(V1**2d+0*K1)*(1.d+0-exp(-T*V1)) -
     +       H*T/K1
           QVS_T2 = V2*T/(V1*K2) - (
     +       V2-bw_mod*V1)/(V1**2d+0*K2)*(1.d+0-exp(-T*V1))
         else
           QAB_T2 = 0.d+0
           QVS_T2 = 0.d+0
         endif
       endif
c      assign output
       qab_out = QAB_T1 + QAB_T2
       qvs_out = QVS_T1 + QVS_T2
       bw_out = bw_mod + q_in - qab_out - qvs_out
10    continue
      return
      end subroutine

c***********************************************************************
c* Subroutine to compute the outflow Q1 of a linear reservoir with     *
c* one outlet. The inflow QZU is assumed to be constant over the       *
c* time-step.                                                          *
c*                                                                     *
* variables and parameters:                                           *
c*    DT........length of model time-step [h]                          *
c*    DTD.......length of computation time-step [h]                    *
c*    QZU.......sum of inflow over time DT [mm]                        *
c*    Q1........sum of outflow over time DTD from outlet 1 [mm]        *
c*    h.........level of outlet 1 [mm]                                 *
c*    k.........reservoir recession constant of outlet 1 [h]           *
c*    BW_IN.....water level in the reservoir at the start of DTD [mm]  *
c*    BW_OUT....water level in the reservoir at the end of DTD [mm]    *
c*                                                                     *
c***********************************************************************
       subroutine route2(h, k, q_in, bw_in, q_out, bw_out)
c ----------------------------------------------------------------------
c pre definitions:
        implicit none
        ! inputs:
        real(kind = 8), intent(in) :: h, k, bw_in, q_in
        !internal:
        real(kind = 8) :: DT, DTD
        !output:
        real(kind = 8), intent(out) :: q_out, bw_out
c ----------------------------------------------------------------------

c ----------------------------------------------------------------------
c code:
      DT = 1.0
      DTD = 1.0
      q_out = q_in/DT*DTD +
     +        k*q_in/DT*exp(-DTD/k) -
     +        k*q_in/DT +
     +        (bw_in-h) -
     +        (bw_in-h)*exp(-DTD/k)



      bw_out = bw_in + q_in/DT*DTD - q_out
      return
      end
