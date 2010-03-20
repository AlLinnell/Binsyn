      PROGRAM GRPA
C**********************************************************************
C     Program to run the programs in GRPA
	DIMENSION FL1(2)
	INTEGER (4) RESULT
	CHARACTER*80 BFFR
	CHARACTER * 24 FL1
      CHARACTER * 256 CWD
	DATA IN1,IN2,IN3,IN4,IN5,IN6,IN7/1,2,3,4,5,6,7/
	DATA IN8,IN9,IN10,IN11,IN12,IN13,IN14,IN15/8,9,10,11,12,13,14,15/
	DATA IN16,NOUT/16,17/
  100 FORMAT (A80)
  101 FORMAT (I2)
C**********************************************************************
C     Store the current working directory (CWD).  NOTE: relative
C     filenames may be re-opend during execution, so this is critical.
      CALL GetCWD(CWD, RESULT)
	OPEN (18, FILE = 'GRPA.ILS', STATUS = 'OLD')
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
C     Set .ILS file for CALPT
      OPEN (IN2, FILE = 'CALPT.REF', STATUS = 'OLD')
	NRF=1
	DO J=1,NRF
		READ (IN2, 100) BFFR
	END DO
	OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
	OPEN (IN3,  FILE = 'CALPTA.ILS', 
     $STATUS = 'REPLACE')
      WRITE (IN3, 100) BFFR
	CLOSE (IN3, STATUS ='KEEP')
	CLOSE (IN2, STATUS = 'KEEP')
	CLOSE (IN16,STATUS = 'KEEP')
C
      icount = 1
C     RUN CALPT
      CALL ChDir("../calpt/")
      CALL system("./calptscr" , RESULT)
      CALL ChDir(CWD)
	call tsout(RESULT, icount, NOUT)
	OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1,101) KEYV
	IF (KEYV.NE.0) GO TO 99
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
C			      
C     Set .ILS file for TRNSF1
      OPEN (IN12, FILE = 'TRNSF1.REF', STATUS = 'OLD')
	NRF=1
	DO J=1,NRF
		READ (IN12, 100) BFFR
	END DO
	OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
	OPEN (IN13, FILE ='TRANSF1A.ILS', 
     $STATUS = 'REPLACE')
	WRITE (IN13, 100) BFFR
	CLOSE (IN13, STATUS = 'KEEP')
	CLOSE (IN12, STATUS = 'KEEP')
	CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 2
C     Run TRNSF1
C     Output new PGACR.IUP
      CALL ChDir("../trnsf1/")
      CALL system("./trnsf1scr" , RESULT)
      CALL ChDir(CWD)
	call tsout(RESULT,icount,NOUT)
	OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1, 101) KEYV
	IF (KEYV.NE.0) GO TO 99
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set the .ILS file for REPL4
      OPEN (IN14, FILE = 'REPL4.REF', STATUS = 'OLD')
	NFR=1
	DO J=1,NRF
		READ (IN14, 100) BFFR
	END DO
	OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
	OPEN (IN15, FILE = '../repl4/REPL4A.ILS', 
     $STATUS = 'REPLACE')
	WRITE (IN15, 100) BFFR
	CLOSE (IN15, STATUS = 'KEEP')
	CLOSE (IN14, STATUS = 'KEEP')
	CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 3
C     Run REPL4
C     Copy PGACR.IUP to PGACR.INF
      CALL ChDir("../repl4/")
      CALL system("./repl4scr" , RESULT)
      CALL ChDir(CWD)
	call tsout(RESULT, icount, NOUT)
	OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1, 101) KEYV
	IF (KEYV.NE.0) GO TO 99
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set .ILS file for PGA
      OPEN (IN4, FILE = '../pga/PGA.REF', STATUS = 'OLD')
	NRF=1
	DO J=1,NRF
		READ (IN4, 100) BFFR
	END DO
	OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
	OPEN (IN5, FILE = '../pga/PGAA.ILS', STATUS = 'REPLACE')
	WRITE (IN5, 100) BFFR
	CLOSE (IN5, STATUS = 'KEEP')
	CLOSE (IN4, STATUS = 'KEEP')
	CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 4
C     Run PGA
      CALL ChDir("../pga/")
      CALL system("./pgascr" , RESULT)
      CALL ChDir(CWD)
	call tsout(RESULT, icount, NOUT)
	OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1, 101) KEYV
	IF (KEYV.NE.0) GO TO 99
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set .ILS file for PGB
      OPEN (IN6, FILE = '../gpra/PGB.REF')
	NRF=1
	DO J=1,NRF
		READ (IN6, 100) BFFR
	END DO
	OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
	OPEN (IN7, FILE = '../pgb/PGBA.ILS', STATUS = 'REPLACE')
	WRITE (IN7, 100) BFFR
	CLOSE (IN7, STATUS = 'KEEP')
	CLOSE (IN6,STATUS = 'KEEP')
	CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 5
C     Run PGB
      CALL ChDir("../pgb/")
      CALL system("./pgbscr" , RESULT)
      CALL ChDir(CWD)
	call tsout(RESULT, icount, NOUT)
	OPEN (IN1, FILE = FL1(1),STATUS = 'OLD')
	READ (IN1, 101) KEYV
	IF (KEYV.NE.0) GO TO 99
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set .ILS file for PGC
      OPEN (IN8, FILE = 'PGC.REF', STATUS = 'OLD')
	NRF=1
	DO J=1,NRF
		READ (IN8, 100) BFFR
	END DO
	OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
	OPEN (IN9, FILE = '../pgc/PGCA.ILS', STATUS = 'REPLACE')
	WRITE (IN9, 100) BFFR
	CLOSE (IN9, STATUS = 'KEEP')
	CLOSE (IN8, STATUS = 'KEEP')
	CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 6
C     Run PGC
      CALL ChDir("../pgc/")
      CALL system("./pgcscr" , RESULT)
      CALL ChDir(CWD)
	call tsout(RESULT, icount, NOUT)
	OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1, 101) KEYV
	IF (KEYV.NE.0) GO TO 99
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set .ILS file for PGD
      OPEN (IN10, FILE = 'PGD.REF', STATUS = 'OLD')
	NRF=1
	DO J=1, NRF
		READ (IN10, 100) BFFR
	END DO
	OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
	OPEN (IN11, FILE = '../pgd/PGDA.ILS', STATUS = 'REPLACE')
	WRITE (IN11, 100) BFFR
	CLOSE (IN11, STATUS = 'KEEP')
	CLOSE (IN10, STATUS = 'KEEP')
	CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 7
C     Run PGD
      CALL ChDir("../pgd/")
      CALL system("./pgdscr" , RESULT)
      CALL ChDir(CWD)
	call tsout(RESULT, icount, NOUT)
	OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1, 101) KEYV
	IF (KEYV.NE.0) GO TO 99
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     The central reference run now is finished.
C     Do the run for the higher potential for star I
C
C     Set .ILS file for CALPT
      OPEN (IN2, FILE = 'CALPT.REF', STATUS = 'OLD')
	NRF=2
	DO J=1,NRF
		READ (IN2, 100) BFFR
	END DO
	OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
	OPEN (IN3,  FILE = '../calpt/CALPTA.ILS', 
     $STATUS = 'REPLACE')
      WRITE (IN3, 100) BFFR
	CLOSE (IN3, STATUS ='KEEP')
	CLOSE (IN2, STATUS = 'KEEP')
	CLOSE (IN16,STATUS = 'KEEP')
C
      icount = 8
C     RUN CALPT
      CALL ChDir("../calpt/")
      CALL system("./calptscr" , RESULT)
      CALL ChDir(CWD)
	call tsout(RESULT, icount, NOUT)
	OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1,101) KEYV
	IF (KEYV.NE.0) GO TO 99
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
C			      
C     Set .ILS file for TRNSF1
      OPEN (IN12, FILE = 'TRNSF1.REF', STATUS = 'OLD')
	NRF=2
	DO J=1,NRF
		READ (IN12, 100) BFFR
	END DO
	OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
	OPEN (IN13, FILE = '../trnsf1/TRANSF1A.ILS', 
     $STATUS = 'REPLACE')
	WRITE (IN13, 100) BFFR
	CLOSE (IN13, STATUS = 'KEEP')
	CLOSE (IN12, STATUS = 'KEEP')
	CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 9
C     Run TRNSF1
C     Output new PGAH1.IUP
      CALL ChDir("../trnsf1/")
      CALL system("./trnsf1scr" , RESULT)
      CALL ChDir(CWD)
	call tsout(RESULT,icount,NOUT)
	OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1, 101) KEYV
	IF (KEYV.NE.0) GO TO 99
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set the .ILS file for REPL4
      OPEN (IN14, FILE = 'REPL4.REF', STATUS = 'OLD')
	NFR=2
	DO J=1,NRF
		READ (IN14, 100) BFFR
	END DO
	OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
	OPEN (IN15, FILE = '../repl4/REPL4A.ILS', 
     $STATUS = 'REPLACE')
	WRITE (IN15, 100) BFFR
	CLOSE (IN15, STATUS = 'KEEP')
	CLOSE (IN14, STATUS = 'KEEP')
	CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 10
C     Run REPL4
C     Copy PGAH1.IUP to PGAH1.INF
      CALL ChDir("../trnsf1/")
      CALL system("./trnsf1scr" , RESULT)
      CALL ChDir(CWD)
	call tsout(RESULT, icount, NOUT)
	OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1, 101) KEYV
	IF (KEYV.NE.0) GO TO 99
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set .ILS file for PGA
      OPEN (IN4, FILE = 'PGA.REF', STATUS = 'OLD')
	NRF=2
	DO J=1,NRF
		READ (IN4, 100) BFFR
	END DO
	OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
	OPEN (IN5, FILE = '../pga/PGAA.ILS', STATUS = 'REPLACE')
	WRITE (IN5, 100) BFFR
	CLOSE (IN5, STATUS = 'KEEP')
	CLOSE (IN4, STATUS = 'KEEP')
	CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 11
C     Run PGA
      CALL ChDir("../pga/")
      CALL system("./pgascr" , RESULT)
      CALL ChDir(CWD)
	call tsout(RESULT, icount, NOUT)
	OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1, 101) KEYV
	IF (KEYV.NE.0) GO TO 99
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set .ILS file for PGB
      OPEN (IN6, FILE = 'PGB.REF')
	NRF=2
	DO J=1,NRF
		READ (IN6, 100) BFFR
	END DO
	OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
	OPEN (IN7, FILE = '../pgb/PGBA.ILS', STATUS = 'REPLACE')
	WRITE (IN7, 100) BFFR
	CLOSE (IN7, STATUS = 'KEEP')
	CLOSE (IN6,STATUS = 'KEEP')
	CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 12
C     Run PGB
      CALL ChDir("../pgb/")
      CALL system("./pgbscr" , RESULT)
      CALL ChDir(CWD)
	call tsout(RESULT, icount, NOUT)
	OPEN (IN1, FILE = FL1(1),STATUS = 'OLD')
	READ (IN1, 101) KEYV
	IF (KEYV.NE.0) GO TO 99
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set .ILS file for PGC
      OPEN (IN8, FILE = 'PGC.REF', STATUS = 'OLD')
	NRF=2
	DO J=1,NRF
		READ (IN8, 100) BFFR
	END DO
	OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
	OPEN (IN9, FILE = '../pgc/PGCA.ILS', STATUS = 'REPLACE')
	WRITE (IN9, 100) BFFR
	CLOSE (IN9, STATUS = 'KEEP')
	CLOSE (IN8, STATUS = 'KEEP')
	CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 13
C     Run PGC
      CALL ChDir("../pgc/")
      CALL system("./pgcscr" , RESULT)
      CALL ChDir(CWD)
	call tsout(RESULT, icount, NOUT)
	OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1, 101) KEYV
	IF (KEYV.NE.0) GO TO 99
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set .ILS file for PGD
      OPEN (IN10, FILE = 'PGD.REF', STATUS = 'OLD')
	NRF=2
	DO J=1, NRF
		READ (IN10, 100) BFFR
	END DO
	OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
	OPEN (IN11, FILE = '../pgd/PGDA.ILS', STATUS = 'REPLACE')
	WRITE (IN11, 100) BFFR
	CLOSE (IN11, STATUS = 'KEEP')
	CLOSE (IN10, STATUS = 'KEEP')
	CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 14
C     Run PGD
      CALL ChDir("../pgd/")
      CALL system("./pgdscr" , RESULT)
      CALL ChDir(CWD)
	call tsout(RESULT, icount, NOUT)
	OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1, 101) KEYV
	IF (KEYV.NE.0) GO TO 99
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     The runs are now complete for the higher potential, star I
C     Set up and run for the lower potential, star I
C
C     Set .ILS file for CALPT
      OPEN (IN2, FILE = 'CALPT.REF', STATUS = 'OLD')
	NRF=3
	DO J=1,NRF
		READ (IN2, 100) BFFR
	END DO
	OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
	OPEN (IN3,  FILE = '../calpt/CALPTA.ILS', 
     $STATUS = 'REPLACE')
      WRITE (IN3, 100) BFFR
	CLOSE (IN3, STATUS ='KEEP')
	CLOSE (IN2, STATUS = 'KEEP')
	CLOSE (IN16,STATUS = 'KEEP')
C
      icount = 15
C     RUN CALPT
      CALL ChDir("../calpt/")
      CALL system("./calptscr" , RESULT)
      CALL ChDir(CWD)
	call tsout(RESULT, icount, NOUT)
	OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1,101) KEYV
	IF (KEYV.NE.0) GO TO 99
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
C			      
C     Set .ILS file for TRNSF1
      OPEN (IN12, FILE = 'TRNSF1.REF', STATUS = 'OLD')
	NRF=3
	DO J=1,NRF
		READ (IN12, 100) BFFR
	END DO
	OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
	OPEN (IN13, FILE = '../trnsf1/TRANSF1A.ILS', 
     $STATUS = 'REPLACE')
	WRITE (IN13, 100) BFFR
	CLOSE (IN13, STATUS = 'KEEP')
	CLOSE (IN12, STATUS = 'KEEP')
	CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 16
C     Run TRNSF1
C     Output new PGAL1.IUP
      CALL ChDir("../trnsf1/")
      CALL system("./trnsf1scr" , RESULT)
      CALL ChDir(CWD)
	call tsout(RESULT,icount,NOUT)
	OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1, 101) KEYV
	IF (KEYV.NE.0) GO TO 99
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set the .ILS file for REPL4
      OPEN (IN14, FILE = 'REPL4.REF', STATUS = 'OLD')
	NFR=3
	DO J=1,NRF
		READ (IN14, 100) BFFR
	END DO
	OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
	OPEN (IN15, FILE = '../repl4/REPL4A.ILS', 
     $STATUS = 'REPLACE')
	WRITE (IN15, 100) BFFR
	CLOSE (IN15, STATUS = 'KEEP')
	CLOSE (IN14, STATUS = 'KEEP')
	CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 17
C     Run REPL4
C     Copy PGACR.IUP to PGACR.INF
      CALL ChDir("../repl4/")
      CALL system("./repl4scr" , RESULT)
      CALL ChDir(CWD)
	call tsout(RESULT, icount, NOUT)
	OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1, 101) KEYV
	IF (KEYV.NE.0) GO TO 99
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set .ILS file for PGA
      OPEN (IN4, FILE = 'PGA.REF', STATUS = 'OLD')
	NRF=3
	DO J=1,NRF
		READ (IN4, 100) BFFR
	END DO
	OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
	OPEN (IN5, FILE = '../pga/PGAA.ILS', STATUS = 'REPLACE')
	WRITE (IN5, 100) BFFR
	CLOSE (IN5, STATUS = 'KEEP')
	CLOSE (IN4, STATUS = 'KEEP')
	CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 18
C     Run PGA
      CALL ChDir("../pga/")
      CALL system("./pgascr" , RESULT)
      CALL ChDir(CWD)
	OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1, 101) KEYV
	IF (KEYV.NE.0) GO TO 99
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set .ILS file for PGB
      OPEN (IN6, FILE = 'PGB.REF')
	NRF=3
	DO J=1,NRF
		READ (IN6, 100) BFFR
	END DO
	OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
	OPEN (IN7, FILE = '../pgb/PGBA.ILS', STATUS = 'REPLACE')
	WRITE (IN7, 100) BFFR
	CLOSE (IN7, STATUS = 'KEEP')
	CLOSE (IN6,STATUS = 'KEEP')
	CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 19
C     Run PGB
      CALL ChDir("../pgb/")
      CALL system("./pgbscr" , RESULT)
      CALL ChDir(CWD)
	call tsout(RESULT, icount, NOUT)
	OPEN (IN1, FILE = FL1(1),STATUS = 'OLD')
	READ (IN1, 101) KEYV
	IF (KEYV.NE.0) GO TO 99
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set .ILS file for PGC
      OPEN (IN8, FILE = 'PGC.REF', STATUS = 'OLD')
	NRF=3
	DO J=1,NRF
		READ (IN8, 100) BFFR
	END DO
	OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
	OPEN (IN9, FILE = '../pgc/PGCA.ILS', STATUS = 'REPLACE')
	WRITE (IN9, 100) BFFR
	CLOSE (IN9, STATUS = 'KEEP')
	CLOSE (IN8, STATUS = 'KEEP')
	CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 20
C     Run PGC
      CALL ChDir("../pgc/")
      CALL system("./pgcscr" , RESULT)
      CALL ChDir(CWD)
	call tsout(RESULT, icount, NOUT)
	OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1, 101) KEYV
	IF (KEYV.NE.0) GO TO 99
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set .ILS file for PGD
      OPEN (IN10, FILE = 'PGD.REF', STATUS = 'OLD')
	NRF=3
	DO J=1, NRF
		READ (IN10, 100) BFFR
	END DO
	OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
	OPEN (IN11, FILE = '../pgd/PGDA.ILS', STATUS = 'REPLACE')
	WRITE (IN11, 100) BFFR
	CLOSE (IN11, STATUS = 'KEEP')
	CLOSE (IN10, STATUS = 'KEEP')
	CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 21
C     Run PGD
      CALL ChDir("../pgd/")
      CALL system("./pgdscr" , RESULT)
      CALL ChDir(CWD)
	call tsout(RESULT, icount, NOUT)
	OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1, 101) KEYV
	IF (KEYV.NE.0) GO TO 99
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
      icount = 22
      CALL ChDir("../pgh1/")
      CALL system("./pgh1scr" , RESULT)
      CALL ChDir(CWD)
	call tsout(RESULT, icount, NOUT)
	OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1, 101) KEYV
	IF (KEYV.NE.0) GO TO 99
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Restore default .ILS files
C
      OPEN (IN2, FILE = 'CALPT.REF', STATUS = 'OLD')
	NRF=4
	DO J=1,NRF
		READ (IN2, 100) BFFR
	END DO
	OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
	OPEN (IN3,  FILE = '../calpt/CALPTA.ILS', 
     $STATUS = 'REPLACE')
      WRITE (IN3, 100) BFFR
	CLOSE (IN3, STATUS ='KEEP')
	CLOSE (IN2, STATUS = 'KEEP')
	CLOSE (IN16,STATUS = 'KEEP')
C
      OPEN (IN12, FILE = 'TRNSF1.REF', STATUS = 'OLD')
	NRF=4
	DO J=1,NRF
		READ (IN12, 100) BFFR
	END DO
	OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
	OPEN (IN13, FILE = '../trnsf1/TRANSF1A.ILS', 
     $STATUS = 'REPLACE')
	WRITE (IN13, 100) BFFR
	CLOSE (IN13, STATUS = 'KEEP')
	CLOSE (IN12, STATUS = 'KEEP')
	CLOSE (IN16, STATUS = 'KEEP')
C
      OPEN (IN14, FILE = 'REPL4.REF', STATUS = 'OLD')
	NFR=4
	DO J=1,NRF
		READ (IN14, 100) BFFR
	END DO
	OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
	OPEN (IN15, FILE = '../repl4/REPL4A.ILS', 
     $STATUS = 'REPLACE')
	WRITE (IN15, 100) BFFR
	CLOSE (IN15, STATUS = 'KEEP')
	CLOSE (IN14, STATUS = 'KEEP')
	CLOSE (IN16, STATUS = 'KEEP')
C
      OPEN (IN4, FILE = 'PGA.REF', STATUS = 'OLD')
	NRF=4
	DO J=1,NRF
		READ (IN4, 100) BFFR
	END DO
	OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
	OPEN (IN5, FILE = '../pga/PGAA.ILS', STATUS = 'REPLACE')
	WRITE (IN5, 100) BFFR
	CLOSE (IN5, STATUS = 'KEEP')
	CLOSE (IN4, STATUS = 'KEEP')
	CLOSE (IN16, STATUS = 'KEEP')
C
      OPEN (IN6, FILE = 'PGB.REF')
	NRF=4
	DO J=1,NRF
		READ (IN6, 100) BFFR
	END DO
	OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
	OPEN (IN7, FILE = '../pgb/PGBA.ILS', STATUS = 'REPLACE')
	WRITE (IN7, 100) BFFR
	CLOSE (IN7, STATUS = 'KEEP')
	CLOSE (IN6,STATUS = 'KEEP')
	CLOSE (IN16, STATUS = 'KEEP')
C
      OPEN (IN8, FILE = 'PGC.REF', STATUS = 'OLD')
	NRF=4
	DO J=1,NRF
		READ (IN8, 100) BFFR
	END DO
	OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
	OPEN (IN9, FILE = '../pgc/PGCA.ILS', STATUS = 'REPLACE')
	WRITE (IN9, 100) BFFR
	CLOSE (IN9, STATUS = 'KEEP')
	CLOSE (IN8, STATUS = 'KEEP')
	CLOSE (IN16, STATUS = 'KEEP')
C
      OPEN (IN10, FILE = 'PGD.REF', STATUS = 'OLD')
	NRF=4
	DO J=1, NRF
		READ (IN10, 100) BFFR
	END DO
	OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
	OPEN (IN11, FILE = '../pgd/PGDA.ILS', STATUS = 'REPLACE')
	WRITE (IN11, 100) BFFR
	CLOSE (IN11, STATUS = 'KEEP')
	CLOSE (IN10, STATUS = 'KEEP')
	CLOSE (IN16, STATUS = 'KEEP')
C
	WRITE (NOUT, 103)
  103 FORMAT (1X,'End of Program GRPA')
   99 CLOSE (UNIT = IN1,   STATUS = 'KEEP')
  999 CLOSE (UNIT = NOUT,  STATUS = 'KEEP')
      WRITE (*,'(1X,A,I2)') 'Value of LINKV.DAT=',KEYV
	STOP
	END PROGRAM GRPA
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
	END SUBROUTINE TSOUT
