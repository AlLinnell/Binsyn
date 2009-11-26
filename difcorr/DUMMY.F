      SUBROUTINE DUMMY ( IFORM )

C     This subroutine is used by the calling program to point to the
C     next data item. Therefore DUMMY reads 'away' the lines not needed

C     Version  Feb.1, 1986  by Josef Kallrath

      CHARACTER * 80 DUM

      COMMON  /NUNIT/ NU, NUOUT, IFOUT

      DO  I = 1,IFORM
       READ ( NU, '(A)' ) DUM
       IF (IFOUT .EQ. 1) WRITE ( NUOUT, '(A)' ) DUM
      END DO

      RETURN

      END
