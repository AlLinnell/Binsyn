      SUBROUTINE IDREAD ( IGET )

C     This subroutine reads in integer variables with Format I8
C     Version  Feb.1, 1986 by Josef Kallrath (IREAD)
C     New version Apr.29,1988 by A. Linnell  (IREAD)
C     Version IDREAD Feb.18,2011 by A. Linnell

      CHARACTER * 64 DUMMY

      COMMON  /NUNIT/ NU, NUOUT, IFOUT

      READ ( NU, '(A64,I8)' ) DUMMY, IGET

      IF (IFOUT .EQ. 1) WRITE ( NUOUT, '(A64,I8)' ) DUMMY, IGET

      RETURN

      END
