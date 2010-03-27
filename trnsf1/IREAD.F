      SUBROUTINE IREAD ( IGET )

C     This subroutine reads in integer variables with Format I4
C     Version  Feb.1, 1986 by Josef Kallrath
C     New version Apr.29,1988 by A. Linnell

      CHARACTER * 64 DUMMY

      COMMON  /NUNIT/ NU, NUOUT, IFOUT

      READ ( NU, '(A64,I4)' ) DUMMY, IGET

      IF (IFOUT .EQ. 1) WRITE ( NUOUT, '(A64,I4)' ) DUMMY, IGET

      RETURN

      END
