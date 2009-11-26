      SUBROUTINE   GTLBL ( LABEL, LBLERR )

C    This subroutine looks for a 7-character-long indicated label

C    Version Feb.1,  1986  by Josef Kallrath


      CHARACTER * 7   INPUT, LABEL

      COMMON /NUNIT/ NU, NUOUT, IFOUT

10    READ ( NU,  '(A20)',  END=20 ) INPUT

      IF  ( INPUT .NE. LABEL ) GO TO 10

      CALL DUMMY ( 4 )

      RETURN

20    LBLERR = -1

      RETURN

      END
