      PROGRAM GRPE
C**********************************************************************
C     Program to run the programs in GRPE
      DIMENSION FL1(2)
      INTEGER (4) RESULT
      CHARACTER*80 PROGNAME,BFFR
      CHARACTER * 24 FL1
      LOGICAL A
      DATA IN1/1/
      DATA IN8,IN9,IN10,IN11/8,9,10,11/
      DATA IN16,NOUT/16,17/
  100 FORMAT (A80)
  101 FORMAT (I2)
C**********************************************************************
      OPEN (18, FILE = 'C:\LGTSYN\GRPE\GRPE.ILS', STATUS = 'OLD')
      READ (18, 104) (FL1(J),J=1,2)
  104 FORMAT (A24)
      OPEN (UNIT = NOUT, FILE = FL1(2), STATUS = 'REPLACE')
      OPEN (UNIT = IN1,  FILE = FL1(1), STATUS = 'OLD')
      READ (IN1,101) KEYV
      IF (KEYV.NE.0) THEN
            KEYV=0
            BACKSPACE IN1
            WRITE (IN1,101) KEYV
      END IF
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C
C     Set .ILS file for PGCT1L
      OPEN (IN8, FILE = 'C:\LGTSYN\GRPE\PGC.REF', STATUS = 'OLD')
      NRF=1
      DO J=1,NRF
            READ (IN8, 100) BFFR
      END DO
      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN9, FILE = 'C:\LGTSYN\PGC\PGCA.ILS', STATUS = 'REPLACE')
      WRITE (IN9, 100) BFFR
      CLOSE (IN9, STATUS = 'KEEP')
      CLOSE (IN8, STATUS = 'KEEP')
      CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 1
C     Run PGC
      progname = 'C:\LGTSYN\PGC\DEBUG\PGC.EXE'
      RESULT = SYSTEMQQ(progname)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set .ILS file for PGDT1L
      OPEN (IN10, FILE = 'C:\LGTSYN\GRPE\PGD.REF', STATUS = 'OLD')
      NRF=1
      DO J=1, NRF
            READ (IN10, 100) BFFR
      END DO
      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN11, FILE = 'C:\LGTSYN\PGD\PGDA.ILS', STATUS = 'REPLACE')
      WRITE (IN11, 100) BFFR
      CLOSE (IN11, STATUS = 'KEEP')
      CLOSE (IN10, STATUS = 'KEEP')
      CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 2
C     Run PGD
      progname = 'C:\LGTSYN\PGD\DEBUG\PGD.EXE'
      RESULT = SYSTEMQQ(progname)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C
C     Set .ILS file for PGCT1S
      OPEN (IN8, FILE = 'C:\LGTSYN\GRPE\PGC.REF', STATUS = 'OLD')
      NRF=2
      DO J=1,NRF
            READ (IN8, 100) BFFR
      END DO
      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN9, FILE = 'C:\LGTSYN\PGC\PGCA.ILS', STATUS = 'REPLACE')
      WRITE (IN9, 100) BFFR
      CLOSE (IN9, STATUS = 'KEEP')
      CLOSE (IN8, STATUS = 'KEEP')
      CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 3
C     Run PGC
      progname = 'C:\LGTSYN\PGC\DEBUG\PGC.EXE'
      RESULT = SYSTEMQQ(progname)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set .ILS file for PGDT1S
      OPEN (IN10, FILE = 'C:\LGTSYN\GRPE\PGD.REF', STATUS = 'OLD')
      NRF=2
      DO J=1, NRF
            READ (IN10, 100) BFFR
      END DO
      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN11, FILE = 'C:\LGTSYN\PGD\PGDA.ILS', STATUS = 'REPLACE')
      WRITE (IN11, 100) BFFR
      CLOSE (IN11, STATUS = 'KEEP')
      CLOSE (IN10, STATUS = 'KEEP')
      CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 4
C     Run PGD
      progname = 'C:\LGTSYN\PGD\DEBUG\PGD.EXE'
      RESULT = SYSTEMQQ(progname)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C
C     Set .ILS file for PGCT2L
      OPEN (IN8, FILE = 'C:\LGTSYN\GRPE\PGC.REF', STATUS = 'OLD')
      NRF=3
      DO J=1,NRF
            READ (IN8, 100) BFFR
      END DO
      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN9, FILE = 'C:\LGTSYN\PGC\PGCA.ILS', STATUS = 'REPLACE')
      WRITE (IN9, 100) BFFR
      CLOSE (IN9, STATUS = 'KEEP')
      CLOSE (IN8, STATUS = 'KEEP')
      CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 5
C     Run PGC
      progname = 'C:\LGTSYN\PGC\DEBUG\PGC.EXE'
      RESULT = SYSTEMQQ(progname)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set .ILS file for PGDT2L
      OPEN (IN10, FILE = 'C:\LGTSYN\GRPE\PGD.REF', STATUS = 'OLD')
      NRF=3
      DO J=1, NRF
            READ (IN10, 100) BFFR
      END DO
      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN11, FILE = 'C:\LGTSYN\PGD\PGDA.ILS', STATUS = 'REPLACE')
      WRITE (IN11, 100) BFFR
      CLOSE (IN11, STATUS = 'KEEP')
      CLOSE (IN10, STATUS = 'KEEP')
      CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 6
C     Run PGD
      progname = 'C:\LGTSYN\PGD\DEBUG\PGD.EXE'
      RESULT = SYSTEMQQ(progname)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C
C     Set .ILS file for PGCT2S
      OPEN (IN8, FILE = 'C:\LGTSYN\GRPE\PGC.REF', STATUS = 'OLD')
      NRF=4
      DO J=1,NRF
            READ (IN8, 100) BFFR
      END DO
      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN9, FILE = 'C:\LGTSYN\PGC\PGCA.ILS', STATUS = 'REPLACE')
      WRITE (IN9, 100) BFFR
      CLOSE (IN9, STATUS = 'KEEP')
      CLOSE (IN8, STATUS = 'KEEP')
      CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 7
C     Run PGC
      progname = 'C:\LGTSYN\PGC\DEBUG\PGC.EXE'
      RESULT = SYSTEMQQ(progname)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set .ILS file for PGDT2S
      OPEN (IN10, FILE = 'C:\LGTSYN\GRPE\PGD.REF', STATUS = 'OLD')
      NRF=4
      DO J=1, NRF
            READ (IN10, 100) BFFR
      END DO
      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN11, FILE = 'C:\LGTSYN\PGD\PGDA.ILS', STATUS = 'REPLACE')
      WRITE (IN11, 100) BFFR
      CLOSE (IN11, STATUS = 'KEEP')
      CLOSE (IN10, STATUS = 'KEEP')
      CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 8
C     Run PGD
      progname = 'C:\LGTSYN\PGD\DEBUG\PGD.EXE'
      RESULT = SYSTEMQQ(progname)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C
C     Set .ILS file for PGCA1L
      OPEN (IN8, FILE = 'C:\LGTSYN\GRPE\PGC.REF', STATUS = 'OLD')
      NRF=5
      DO J=1,NRF
            READ (IN8, 100) BFFR
      END DO
      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN9, FILE = 'C:\LGTSYN\PGC\PGCA.ILS', STATUS = 'REPLACE')
      WRITE (IN9, 100) BFFR
      CLOSE (IN9, STATUS = 'KEEP')
      CLOSE (IN8, STATUS = 'KEEP')
      CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 9
C     Run PGC
      progname = 'C:\LGTSYN\PGC\DEBUG\PGC.EXE'
      RESULT = SYSTEMQQ(progname)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set .ILS file for PGDA1L
      OPEN (IN10, FILE = 'C:\LGTSYN\GRPE\PGD.REF', STATUS = 'OLD')
      NRF=5
      DO J=1, NRF
            READ (IN10, 100) BFFR
      END DO
      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN11, FILE = 'C:\LGTSYN\PGD\PGDA.ILS', STATUS = 'REPLACE')
      WRITE (IN11, 100) BFFR
      CLOSE (IN11, STATUS = 'KEEP')
      CLOSE (IN10, STATUS = 'KEEP')
      CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 10
C     Run PGD
      progname = 'C:\LGTSYN\PGD\DEBUG\PGD.EXE'
      RESULT = SYSTEMQQ(progname)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C
C     Set .ILS file for PGCA1S
      OPEN (IN8, FILE = 'C:\LGTSYN\GRPE\PGC.REF', STATUS = 'OLD')
      NRF=6
      DO J=1,NRF
            READ (IN8, 100) BFFR
      END DO
      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN9, FILE = 'C:\LGTSYN\PGC\PGCA.ILS', STATUS = 'REPLACE')
      WRITE (IN9, 100) BFFR
      CLOSE (IN9, STATUS = 'KEEP')
      CLOSE (IN8, STATUS = 'KEEP')
      CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 11
C     Run PGC
      progname = 'C:\LGTSYN\PGC\DEBUG\PGC.EXE'
      RESULT = SYSTEMQQ(progname)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set .ILS file for PGDA1S
      OPEN (IN10, FILE = 'C:\LGTSYN\GRPE\PGD.REF', STATUS = 'OLD')
      NRF=6
      DO J=1, NRF
            READ (IN10, 100) BFFR
      END DO
      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN11, FILE = 'C:\LGTSYN\PGD\PGDA.ILS', STATUS = 'REPLACE')
      WRITE (IN11, 100) BFFR
      CLOSE (IN11, STATUS = 'KEEP')
      CLOSE (IN10, STATUS = 'KEEP')
      CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 12
C     Run PGD
      progname = 'C:\LGTSYN\PGD\DEBUG\PGD.EXE'
      RESULT = SYSTEMQQ(progname)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C
C     Set .ILS file for PGCA2L
      OPEN (IN8, FILE = 'C:\LGTSYN\GRPE\PGC.REF', STATUS = 'OLD')
      NRF=7
      DO J=1,NRF
            READ (IN8, 100) BFFR
      END DO
      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN9, FILE = 'C:\LGTSYN\PGC\PGCA.ILS', STATUS = 'REPLACE')
      WRITE (IN9, 100) BFFR
      CLOSE (IN9, STATUS = 'KEEP')
      CLOSE (IN8, STATUS = 'KEEP')
      CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 13
C     Run PGC
      progname = 'C:\LGTSYN\PGC\DEBUG\PGC.EXE'
      RESULT = SYSTEMQQ(progname)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set .ILS file for PGDA2L
      OPEN (IN10, FILE = 'C:\LGTSYN\GRPE\PGD.REF', STATUS = 'OLD')
      NRF=7
      DO J=1, NRF
            READ (IN10, 100) BFFR
      END DO
      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN11, FILE = 'C:\LGTSYN\PGD\PGDA.ILS', STATUS = 'REPLACE')
      WRITE (IN11, 100) BFFR
      CLOSE (IN11, STATUS = 'KEEP')
      CLOSE (IN10, STATUS = 'KEEP')
      CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 14
C     Run PGD
      progname = 'C:\LGTSYN\PGD\DEBUG\PGD.EXE'
      RESULT = SYSTEMQQ(progname)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C
C     Set .ILS file for PGCA2S
      OPEN (IN8, FILE = 'C:\LGTSYN\GRPE\PGC.REF', STATUS = 'OLD')
      NRF=8
      DO J=1,NRF
            READ (IN8, 100) BFFR
      END DO
      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN9, FILE = 'C:\LGTSYN\PGC\PGCA.ILS', STATUS = 'REPLACE')
      WRITE (IN9, 100) BFFR
      CLOSE (IN9, STATUS = 'KEEP')
      CLOSE (IN8, STATUS = 'KEEP')
      CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 15
C     Run PGC
      progname = 'C:\LGTSYN\PGC\DEBUG\PGC.EXE'
      RESULT = SYSTEMQQ(progname)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set .ILS file for PGDA2S
      OPEN (IN10, FILE = 'C:\LGTSYN\GRPE\PGD.REF', STATUS = 'OLD')
      NRF=8
      DO J=1, NRF
            READ (IN10, 100) BFFR
      END DO
      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN11, FILE = 'C:\LGTSYN\PGD\PGDA.ILS', STATUS = 'REPLACE')
      WRITE (IN11, 100) BFFR
      CLOSE (IN11, STATUS = 'KEEP')
      CLOSE (IN10, STATUS = 'KEEP')
      CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 16
C     Run PGD
      progname = 'C:\LGTSYN\PGD\DEBUG\PGD.EXE'
      RESULT = SYSTEMQQ(progname)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C
C     Set .ILS file for PGCG1L
      OPEN (IN8, FILE = 'C:\LGTSYN\GRPE\PGC.REF', STATUS = 'OLD')
      NRF=9
      DO J=1,NRF
            READ (IN8, 100) BFFR
      END DO
      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN9, FILE = 'C:\LGTSYN\PGC\PGCA.ILS', STATUS = 'REPLACE')
      WRITE (IN9, 100) BFFR
      CLOSE (IN9, STATUS = 'KEEP')
      CLOSE (IN8, STATUS = 'KEEP')
      CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 17
C     Run PGC
      progname = 'C:\LGTSYN\PGC\DEBUG\PGC.EXE'
      RESULT = SYSTEMQQ(progname)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set .ILS file for PGDG1L
      OPEN (IN10, FILE = 'C:\LGTSYN\GRPE\PGD.REF', STATUS = 'OLD')
      NRF=9
      DO J=1, NRF
            READ (IN10, 100) BFFR
      END DO
      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN11, FILE = 'C:\LGTSYN\PGD\PGDA.ILS', STATUS = 'REPLACE')
      WRITE (IN11, 100) BFFR
      CLOSE (IN11, STATUS = 'KEEP')
      CLOSE (IN10, STATUS = 'KEEP')
      CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 18
C     Run PGD
      progname = 'C:\LGTSYN\PGD\DEBUG\PGD.EXE'
      RESULT = SYSTEMQQ(progname)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C
C     Set .ILS file for PGCG1S
      OPEN (IN8, FILE = 'C:\LGTSYN\GRPE\PGC.REF', STATUS = 'OLD')
      NRF=10
      DO J=1,NRF
            READ (IN8, 100) BFFR
      END DO
      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN9, FILE = 'C:\LGTSYN\PGC\PGCA.ILS', STATUS = 'REPLACE')
      WRITE (IN9, 100) BFFR
      CLOSE (IN9, STATUS = 'KEEP')
      CLOSE (IN8, STATUS = 'KEEP')
      CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 19
C     Run PGC
      progname = 'C:\LGTSYN\PGC\DEBUG\PGC.EXE'
      RESULT = SYSTEMQQ(progname)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set .ILS file for PGDG1S
      OPEN (IN10, FILE = 'C:\LGTSYN\GRPE\PGD.REF', STATUS = 'OLD')
      NRF=10
      DO J=1, NRF
            READ (IN10, 100) BFFR
      END DO
      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN11, FILE = 'C:\LGTSYN\PGD\PGDA.ILS', STATUS = 'REPLACE')
      WRITE (IN11, 100) BFFR
      CLOSE (IN11, STATUS = 'KEEP')
      CLOSE (IN10, STATUS = 'KEEP')
      CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 20
C     Run PGD
      progname = 'C:\LGTSYN\PGD\DEBUG\PGD.EXE'
      RESULT = SYSTEMQQ(progname)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C
C     Set .ILS file for PGCG2L
      OPEN (IN8, FILE = 'C:\LGTSYN\GRPE\PGC.REF', STATUS = 'OLD')
      NRF=11
      DO J=1,NRF
            READ (IN8, 100) BFFR
      END DO
      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN9, FILE = 'C:\LGTSYN\PGC\PGCA.ILS', STATUS = 'REPLACE')
      WRITE (IN9, 100) BFFR
      CLOSE (IN9, STATUS = 'KEEP')
      CLOSE (IN8, STATUS = 'KEEP')
      CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 21
C     Run PGC
      progname = 'C:\LGTSYN\PGC\DEBUG\PGC.EXE'
      RESULT = SYSTEMQQ(progname)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set .ILS file for PGDG2L
      OPEN (IN10, FILE = 'C:\LGTSYN\GRPE\PGD.REF', STATUS = 'OLD')
      NRF=11
      DO J=1, NRF
            READ (IN10, 100) BFFR
      END DO
      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN11, FILE = 'C:\LGTSYN\PGD\PGDA.ILS', STATUS = 'REPLACE')
      WRITE (IN11, 100) BFFR
      CLOSE (IN11, STATUS = 'KEEP')
      CLOSE (IN10, STATUS = 'KEEP')
      CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 22
C     Run PGD
      progname = 'C:\LGTSYN\PGD\DEBUG\PGD.EXE'
      RESULT = SYSTEMQQ(progname)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C
C     Set .ILS file for PGCG2S
      OPEN (IN8, FILE = 'C:\LGTSYN\GRPE\PGC.REF', STATUS = 'OLD')
      NRF=12
      DO J=1,NRF
            READ (IN8, 100) BFFR
      END DO
      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN9, FILE = 'C:\LGTSYN\PGC\PGCA.ILS', STATUS = 'REPLACE')
      WRITE (IN9, 100) BFFR
      CLOSE (IN9, STATUS = 'KEEP')
      CLOSE (IN8, STATUS = 'KEEP')
      CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 23
C     Run PGC
      progname = 'C:\LGTSYN\PGC\DEBUG\PGC.EXE'
      RESULT = SYSTEMQQ(progname)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set .ILS file for PGDG2S
      OPEN (IN10, FILE = 'C:\LGTSYN\GRPE\PGD.REF', STATUS = 'OLD')
      NRF=12
      DO J=1, NRF
            READ (IN10, 100) BFFR
      END DO
      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN11, FILE = 'C:\LGTSYN\PGD\PGDA.ILS', STATUS = 'REPLACE')
      WRITE (IN11, 100) BFFR
      CLOSE (IN11, STATUS = 'KEEP')
      CLOSE (IN10, STATUS = 'KEEP')
      CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 24
C     Run PGD
      progname = 'C:\LGTSYN\PGD\DEBUG\PGD.EXE'
      RESULT = SYSTEMQQ(progname)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C
C
      icount = 25
C     Run PGH8
      progname = 'C:\LGTSYN\PGH8\DEBUG\PGH8.EXE'
      RESULT = SYSTEMQQ(progname)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
CC
      icount = 26
C     Run PGH9
      progname = 'C:\LGTSYN\PGH9\DEBUG\PGH9.EXE'
      RESULT = SYSTEMQQ(progname)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C
      icount = 27
C     Run PGH10
      progname = 'C:\LGTSYN\PGH10\DEBUG\PGH10.EXE'
      RESULT = SYSTEMQQ(progname)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
      icount = 28
C     Run PGH11
      progname = 'C:\LGTSYN\PGH11\DEBUG\PGH11.EXE'
      RESULT = SYSTEMQQ(progname)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
      icount = 29
C     Run PGH12
      progname = 'C:\LGTSYN\PGH12\DEBUG\PGH12.EXE'
      RESULT = SYSTEMQQ(progname)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
      icount = 30
C     Run PGH13
      progname = 'C:\LGTSYN\PGH13\DEBUG\PGH13.EXE'
      RESULT = SYSTEMQQ(progname)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
      icount = 31
C     Run PGH14
      progname = 'C:\LGTSYN\PGH14\DEBUG\PGH14.EXE'
      RESULT = SYSTEMQQ(progname)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Restore default .ILS files
C
      OPEN (IN8, FILE = 'C:\LGTSYN\GRPE\PGC.REF', STATUS = 'OLD')
      NRF=13
      DO J=1,NRF
            READ (IN8, 100) BFFR
      END DO
      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN9, FILE = 'C:\LGTSYN\PGC\PGCA.ILS', STATUS = 'REPLACE')
      WRITE (IN9, 100) BFFR
      CLOSE (IN9, STATUS = 'KEEP')
      CLOSE (IN8, STATUS = 'KEEP')
      CLOSE (IN16, STATUS = 'KEEP')
C
      OPEN (IN10, FILE = 'C:\LGTSYN\GRPE\PGD.REF', STATUS = 'OLD')
      NRF=13
      DO J=1, NRF
            READ (IN10, 100) BFFR
      END DO
      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN11, FILE = 'C:\LGTSYN\PGD\PGDA.ILS', STATUS = 'REPLACE')
      WRITE (IN11, 100) BFFR
      CLOSE (IN11, STATUS = 'KEEP')
      CLOSE (IN10, STATUS = 'KEEP')
      CLOSE (IN16, STATUS = 'KEEP')
C
      WRITE (NOUT, 103)
  103 FORMAT (1X,'End of Program GRPE')
   99 CLOSE (UNIT = IN1,   STATUS = 'KEEP')
  999 CLOSE (UNIT = NOUT,  STATUS = 'KEEP')
      WRITE (*,'(1X,A,I2)') 'Value of LINKV.DAT=',KEYV
      STOP
      END
C
      SUBROUTINE TSOUT(RESULT, icount, NOUT)
      INTEGER(4) RESULT
      IF (RESULT.LT.0) THEN
            WRITE (NOUT, 101) icount, RESULT
  101 FORMAT (1X,'Unable to run, icount=',I4,2X,'RESULT=',I4)
            STOP
      ELSE
            WRITE (NOUT, 102) icount
  102 FORMAT (1X,'Successfully completed icount=',I4)
            RETURN
      ENDIF
      END PROGRAM GRPE
