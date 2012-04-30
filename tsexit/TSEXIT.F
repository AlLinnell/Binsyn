      PROGRAM TSEXIT
C     Program for insertion in script sequence calls to terminate run if
C     there is a single orbital longitude.
C     This procedure avoids a call to program SPT which otherwise will bomb.
  106 FORMAT (2I5)
      OPEN (UNIT=55, FILE='../oa/PGBX3.DAT', STATUS='OLD')
      READ (55,106) KCTRL,NPSI
      CLOSE (55, STATUS ='KEEP')
      IF (NPSI.EQ.1) STOP 1
      END
