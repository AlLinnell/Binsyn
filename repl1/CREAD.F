      SUBROUTINE CREAD ( CGET )

C     This subroutine reads character constants wit variable length
C     Version  Feb.1,1986  by Josef Kallrath

      CHARACTER * 64 DUMMY
      CHARACTER * (*) CGET

      COMMON  /NUNIT/ NU, NUOUT, IFOUT

      READ ( NU, '(A64,A20)' ) DUMMY, CGET

      IF (IFOUT .EQ. 1)  WRITE ( NUOUT, '(A64,A20)' ) DUMMY, CGET

      RETURN

      END
