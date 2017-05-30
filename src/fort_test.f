
! ---------------------------------------------------------------------------
      ! mean squared error 
      subroutine f_mse(XSIM,XOBS,maxday,NDSTART,NDEND,EVAL,mse)
        implicit none
        integer :: maxday
        integer :: N, ND, NDSTART, NDEND
        integer :: EVAL(MAXDAY)
        real(kind = 8) :: sum_a
        real(kind = 8) :: XSIM(MAXDAY), XOBS(MAXDAY)
        ! output:
        real(kind = 8) :: mse
        !******
        N = 0
        sum_a = 0.
        do ND=NDSTART, NDEND
         if (EVAL(ND).eq.1) then
           N = N + 1
           sum_a = sum_a + (XSIM(ND)-XOBS(ND))**2
         endif
        enddo
        MSE = sum_a/N
      end subroutine

! ---------------------------------------------------------------------------
      ! Nash-Sutcliffe Efficiency
      subroutine f_nse(XSIM,XOBS,maxday,NDSTART,NDEND,EVAL,NSE)
        implicit none
        integer maxday
        integer N, ND, NDSTART, NDEND
        integer EVAL(MAXDAY)
        real(kind = 8) MOBS, SUMA, SUMB
        real(kind = 8) XSIM(MAXDAY), XOBS(MAXDAY)
        !
        real(kind = 8) NSE
        !******
        MOBS = 0.
        N = 0
        do ND=NDSTART, NDEND
         if (EVAL(ND).eq.1) then
           N = N + 1
           MOBS = MOBS + XOBS(ND)
         endif
        enddo
        MOBS = MOBS/N
        SUMA = 0.
        SUMB = 0.
        do ND=NDSTART, NDEND
         if (EVAL(ND).eq.1) then
           SUMA = SUMA + (XSIM(ND)-XOBS(ND))**2
           SUMB = SUMB + (XOBS(ND)-MOBS)**2
         endif
        enddo
        NSE = 1. - SUMA/SUMB

        return
      end subroutine

! ---------------------------------------------------------------------------
      ! Bias between mean simulated and observed values.
      subroutine f_bias(XSIM,XOBS,maxday,NDSTART,NDEND,EVAL,bias)
        implicit none
        integer :: maxday
        integer :: N, ND, NDSTART, NDEND
        integer :: EVAL(MAXDAY)
        real(kind = 8) :: sum_a
        real(kind = 8) :: XSIM(MAXDAY), XOBS(MAXDAY)
        !
        real(kind = 8) :: bias
        !******
        N = 0
        sum_a = 0.
        do ND = NDSTART, NDEND
         if (EVAL(ND).eq.1) then
           N = N + 1
           sum_a = sum_a + (XSIM(ND)-XOBS(ND))
         endif
        enddo
        BIAS = sum_a/N
        return
      end subroutine

! ---------------------------------------------------------------------------
      ! Bias between mean simulated and observed values.
      subroutine f_pbias(XSIM,XOBS,maxday,NDSTART,NDEND,EVAL,pbias)
        implicit none
        integer :: maxday
        integer :: N, ND, NDSTART, NDEND
        integer :: EVAL(MAXDAY)
        real(kind = 8) :: sum_a, sum_b
        real(kind = 8) :: XSIM(MAXDAY), XOBS(MAXDAY)
        !
        real(kind = 8) :: pbias
        !******
        N = 0
        sum_a = 0.0
        sum_b = 0.0
        do ND = NDSTART, NDEND
          if (EVAL(ND).eq.1) then
            N = N + 1
            sum_a = sum_a + (XSIM(ND)-XOBS(ND))
            sum_b = sum_b + XOBS(ND)
          endif
        enddo
        pbias = 100.0d+0 * sum_a/sum_b
        return
      end subroutine

! ---------------------------------------------------------------------------
      ! Kling-Gupta Efficiency between mean simulated and observed values.
      subroutine f_kge(XSIM,XOBS,maxday,NDSTART,NDEND,EVAL,kge)
        implicit none

        integer :: maxday
        integer NDSTART, NDEND
        integer:: EVAL(MAXDAY)
        real(kind = 8) :: XSIM(MAXDAY), XOBS(MAXDAY)
        real(kind = 8) :: RHO, ALPHA, BETA
        real(kind = 8) :: SIGSIM, SIGOBS, MSIM, MOBS
        ! functions:
        real(kind = 8) CORR, SIGX, MEANX
        ! outputs:
        real(kind = 8) :: kge
        !******
        RHO = CORR(XSIM,XOBS,maxday,NDSTART,NDEND,EVAL)
        SIGSIM = SIGX(XSIM,maxday,NDSTART,NDEND,EVAL)
        SIGOBS = SIGX(XOBS,maxday,NDSTART,NDEND,EVAL)
        ALPHA = SIGSIM/SIGOBS

        MSIM = MEANX(XSIM,maxday,NDSTART,NDEND,EVAL)
        MOBS = MEANX(XOBS,maxday,NDSTART,NDEND,EVAL)
        BETA = MSIM/MOBS

        KGE = 1. - max(0., ( (RHO-1.)**2 + (ALPHA-1.)**2 +
     +           (BETA-1.)**2 ))**0.5 !mathew !§ D: Care! why is the max here needed?
        return
      end

!      module of_helpers
!        implicit none

!      interface CORR
!        module procedure CORR
!      end interface

!      interface MEANX
!        module procedure MEANX
!      end interface

!      interface SIGX
!        module procedure SIGX
!      end interface


!      contains
        ! -------------------------------------------------------
        ! Correlation coefficient.
        real*8 function CORR(XSIM,XOBS,maxday,NDSTART,NDEND,EVAL)
        implicit none

        integer :: maxday
        integer N, ND, NDSTART, NDEND, EVAL(MAXDAY)
        real(kind = 8) :: MSIM, MOBS, SUMA, SUMB, SUMC
        real(kind = 8) :: XSIM(MAXDAY), XOBS(MAXDAY)
        !******
        N = 0
        MSIM = 0.
        MOBS = 0.
        do ND=NDSTART, NDEND
         if (EVAL(ND).eq.1) then
          N = N + 1
          MSIM = MSIM + XSIM(ND)
          MOBS = MOBS + XOBS(ND)
         endif
        enddo
        MSIM = MSIM/N
        MOBS = MOBS/N

        SUMA = 0.
        SUMB = 0.
        SUMC = 0.
        do ND=NDSTART, NDEND
         if (EVAL(ND).eq.1) then
          SUMA = SUMA + (XSIM(ND)-MSIM)*(XOBS(ND)-MOBS)
          SUMB = SUMB + (XSIM(ND)-MSIM)**2
          SUMC = SUMC + (XOBS(ND)-MOBS)**2
         endif
        enddo
        CORR = SUMA / ((SUMB**.5)*(SUMC**.5))

        return
        end function

        ! -------------------------------------------------------
        ! Compute mean value of variable.
        ! author: H. Kling (1/2009)
        !§ D: care! maybe make a module with "usefull functions" for this kind of stuff
        real*8 function MEANX(X,maxday,NDSTART,NDEND,EVAL)
        implicit none

        integer :: maxday
        integer N, ND, NDSTART, NDEND, EVAL(MAXDAY)
        real(kind = 8) MX, X(MAXDAY)
        !******
        N = 0
        MX = 0.
        do ND=NDSTART, NDEND
         if (EVAL(ND).eq.1) then
          N = N + 1
          MX = MX + X(ND)
         endif
        enddo
        MX = MX/N
        MEANX = MX
        return
        end function

        ! Compute standard deviation of variable.
        ! author: H. Kling (1/2009)
        real*8 function SIGX(X,maxday,NDSTART,NDEND,EVAL)
        implicit none

        integer :: maxday
        integer N, ND, NDSTART, NDEND, EVAL(MAXDAY)
        real(kind = 8) MX, SX, X(MAXDAY)
        real(kind = 8) MEANX
        !******
        MX = MEANX(X,maxday,NDSTART,NDEND,EVAL)
        N = 0
        SX = 0.
        do ND=NDSTART, NDEND
         if (EVAL(ND).eq.1) then
          N = N + 1
          SX = SX + (X(ND)-MX)**2
         endif
        enddo
        SX = max(0., (SX/N))**0.5 !mathew
        SIGX = SX
        return
        end function
!      end
