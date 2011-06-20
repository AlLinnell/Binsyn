      SUBROUTINE  FREAD ( DGET, FORM )

C     This subroutine reads in double precision variables with variable Format
C     Version  Feb. 1, 1986 by Josef Kallrath


      CHARACTER * 64  DUMMY
      CHARACTER * (*) FORM
      CHARACTER * 80  F0

      COMMON  /NUNIT/ NU, NUOUT, IFOUT

      F0 = '( A64, ' // FORM // ')'

      READ ( NU, F0 ) DUMMY, DGET

      IF (IFOUT .EQ. 1)  WRITE ( NUOUT, F0 ) DUMMY, DGET

      RETURN

      END
