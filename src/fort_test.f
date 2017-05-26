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
