      PROGRAM GRPA
C**********************************************************************
C     Program to run the programs in GRPA
      DIMENSION FL1(3)
      INTEGER (4) RESULT
      CHARACTER*80 BFFR,BFFRA
      CHARACTER * 24 FL1
      CHARACTER * 256 CWD
      DATA IN1,IN2,IN3,IN4,IN5,IN6,IN7/1,2,3,4,55,66,7/
      DATA IN8,IN9,IN10,IN11,IN12,IN13,IN14,IN15/8,9,10,11,12,13,14,15/
      DATA IN16,NOUT,INF/16,17,18/
  100 FORMAT (A80)
  101 FORMAT (I2)
  105 FORMAT (5I5)
C**********************************************************************
C     Store the current working directory (CWD).  NOTE: relative
C     filenames may be re-opened during execution, so this is critical.
      CALL GetCWD(CWD, RESULT)
      OPEN (19, FILE = 'GRPA.ILS', STATUS = 'OLD')
      READ (19, 104) (FL1(J),J=1,3)
  104 FORMAT (A24)
      OPEN (UNIT = NOUT, FILE = FL1(2), STATUS = 'REPLACE')
      OPEN (UNIT = IN1,  FILE = FL1(1), STATUS = 'OLD')
      OPEN (UNIT = INF,  FILE = FL1(3), STATUS = 'OLD')
      READ (IN1,101) KEYV
      IF (KEYV.NE.0) THEN
            KEYV=0
            BACKSPACE IN1
            WRITE (IN1,101) KEYV
      END IF
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
      READ (INF, 100) BFFRA
      READ (INF, 100) BFFRA
      READ (INF, 105) ISW1
C
C     Set .ILS file for CALPT
      OPEN (IN2, FILE = 'CALPT.REF', STATUS = 'OLD')
      NRF=1
      DO J=1,NRF
            READ (IN2, 100) BFFR
      END DO
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN3,  FILE = '../calpt/CALPTA.ILS', 
     $STATUS = 'REPLACE')
      WRITE (IN3, 100) BFFR
      CLOSE (IN3, STATUS ='KEEP')
      CLOSE (IN2, STATUS = 'KEEP')
C      CLOSE (IN16,STATUS = 'KEEP')
C
      icount = 1
C     RUN CALPT
      CALL ChDir("../calpt/")
      CALL system("./calpt" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1,101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C     Set files for synthetic photometry
      CALL SYSTEM("cp ../oa/CALPTCR.DAT ../oa/CALPTO1.DAT", RESULT)
      CALL SYSTEM("cp ../ia/ACR.INF ../ia/CALPT.INF", RESULT)
C
C     Set .ILS file for TRANSF1
      OPEN (IN12, FILE = 'TRANSF1.REF', STATUS = 'OLD')
      NRF=1
      DO J=1,NRF
            READ (IN12, 100) BFFR
      END DO
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN13, FILE ='../transf1/TRANSF1A.ILS', 
     $STATUS = 'REPLACE')
      WRITE (IN13, 100) BFFR
      CLOSE (IN13, STATUS = 'KEEP')
      CLOSE (IN12, STATUS = 'KEEP')
C      CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 2
C     Run TRANSF1
C     Output new PGACR.IUP
      CALL ChDir("../transf1/")
      CALL system("./transf1" , RESULT)
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
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN15, FILE = '../repl4/REPL4A.ILS', 
     $STATUS = 'REPLACE')
      WRITE (IN15, 100) BFFR
      CLOSE (IN15, STATUS = 'KEEP')
      CLOSE (IN14, STATUS = 'KEEP')
C      CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 3
C     Run REPL4
C     Copy PGACR.IUP to PGACR.INF
      CALL ChDir("../repl4/")
      CALL system("./repl4" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set .ILS file for PGA
      OPEN (IN4, FILE = 'PGA.REF', STATUS = 'OLD')
      NRF=1
      DO J=1,NRF
            READ (IN4, 100) BFFR
      END DO
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN5, FILE = '../pga/PGAA.ILS', STATUS = 'REPLACE')
      WRITE (IN5, 100) BFFR
      CLOSE (IN5, STATUS = 'KEEP')
      CLOSE (IN4, STATUS = 'KEEP')
C      CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 4
C     Run PGA
      CALL ChDir("../pga/")
      CALL system("./pga" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C     Set for synthetic photometry
      CALL SYSTEM("cp ../oa/GRPAAC2.DAT ../oa/PGAX2.DAT", RESULT)
C
C     Set .ILS file for PGB
      OPEN (IN6, FILE = 'PGB.REF')
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
      CALL system("./pgb" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1),STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C     Set for synthetic photometry
      CALL SYSTEM("cp ../oa/GRPABC2.DAT ../oa/PGBX2.DAT", RESULT)
      CALL SYSTEM("cp ../oa/GRPABC3.DAT ../oa/PGBX3.DAT", RESULT)
      CALL SYSTEM("cp ../oa/GRPABC3.DAT ../oa/PGBX3SV.DAT", RESULT)
      CALL SYSTEM("cp ../oa/GRPABC5.DAT ../oa/PGBX5.DAT", RESULT)
      CALL SYSTEM("cp ../oa/GRPABC6.DAT ../oa/PGBX6.DAT", RESULT)       
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
      CALL system("./pgc" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C     Set for synthetic photometry
      CALL SYSTEM("cp ../oa/GRPACC2.DAT ../oa/PGCX2.DAT", RESULT)
      CALL SYSTEM("cp ../oa/GRPACC3.DAT ../oa/PGCX3.DAT", RESULT)
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
      CALL system("./pgd" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C     Set for synthetic photometry
      CALL SYSTEM("cp ../oa/GRPADC3.DAT ../oa/PGDX3.DAT", RESULT)
      CALL SYSTEM("cp ../oa/GRPADC4.DAT ../oa/PGDX4.DAT", RESULT) 
      CALL SYSTEM("cp ../oa/GRPADC7.DAT ../oa/PGDX7.DAT", RESULT) 
      CALL SYSTEM("cp ../oa/GRPADC8.DAT ../oa/PGDX8.DAT", RESULT)  
C
      IF (ISW1.NE.0) THEN
C     Section to do synthetic photometry
      icount = 8
C     Run PLTB
      CALL ChDir("../pltb/")
      CALL system("./pltbscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
      icount = 8
C     Run ACPGA
      CALL ChDir("../acpga/")
      CALL system("./acpgascr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
      icount = 9
C     Run ACPGB
      CALL ChDir("../acpgb/")
      CALL system("./acpgbscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
      icount = 10
C     Run ACPGC
      CALL ChDir("../acpgc/")


      CALL system("./acpgcscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
C      icount = 11
C     Run PGCAC
C      CALL ChDir("../pgcac/")
C      CALL system("./pgcacscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
      icount = 12
C     Run ACPGD
      CALL ChDir("../acpgd/")
      CALL system("./acpgdscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
      icount = 13
C
C     Run RDVEL2
      CALL ChDir("../rdvel2/")
      CALL system("./rdvel2scr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
      icount = 14
C     Run ACPGF6
      CALL ChDir("../acpgf6/")
      CALL system("./acpgf6scr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
      CALL system("cp ../oa/ACPGF61.DAT ../datconv/",RESULT)
      CALL system("cp ../oa/ACPGF61.DAT ../synacsp/",RESULT)
      CALL system("cp ../oa/PGBX3.DAT ../datconv/",RESULT)
      CALL system("cp ../oa/ACPGF61.DAT ../synacsp/",RESULT)
C
      icount = 15
C     Run SYNACSP
      CALL ChDir("../synacsp/")
      CALL system("./synacspscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
      icount = 16
C     Run SYNPHOT
      CALL ChDir("../synphot/")
      CALL system("./synphotscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
      icount = 17
C     Run REPLD
      CALL ChDir("../repld/")
      CALL system("./repldscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C     Copy PGDX3R.DAT to PGDX3.DAT
      CALL ChDir("../oa/")
      CALL system("cp PGDX3R.DAT PGDX3.DAT")
      CALL system("cp PGDX3R.DAT GRPADC3.DAT")
      CALL ChDir(CWD)
C
      END IF
C
C     End of synthetic photometry section
C
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
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN3,  FILE = '../calpt/CALPTA.ILS', 
     $STATUS = 'REPLACE')
      WRITE (IN3, 100) BFFR
      CLOSE (IN3, STATUS ='KEEP')
      CLOSE (IN2, STATUS = 'KEEP')
C      CLOSE (IN16,STATUS = 'KEEP')
C
      icount = 8
C     RUN CALPT
      CALL ChDir("../calpt/")
      CALL system("./calpt" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1,101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
      CALL SYSTEM("cp ../oa/CALPTH1.DAT ../oa/CALPTO1.DAT", RESULT)
      CALL SYSTEM("cp ../ia/AHP1.INF ../ia/CALPT.INF", RESULT)
C
C     Set .ILS file for TRANSF1
      OPEN (IN12, FILE = 'TRANSF1.REF', STATUS = 'OLD')
      NRF=2
      DO J=1,NRF
            READ (IN12, 100) BFFR
      END DO
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN13, FILE = '../transf1/TRANSF1A.ILS', 
     $STATUS = 'REPLACE')
      WRITE (IN13, 100) BFFR
      CLOSE (IN13, STATUS = 'KEEP')
      CLOSE (IN12, STATUS = 'KEEP')
C      CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 9
C     Run TRANSF1
C     Output new PGAH1.IUP
      CALL ChDir("../transf1/")
      CALL system("./transf1" , RESULT)
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
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN15, FILE = '../repl4/REPL4A.ILS', 
     $STATUS = 'REPLACE')
      WRITE (IN15, 100) BFFR
      CLOSE (IN15, STATUS = 'KEEP')
      CLOSE (IN14, STATUS = 'KEEP')
C      CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 10
C     Run REPL4
C     Copy PGAH1.IUP to PGAH1.INF
      CALL ChDir("../repl4/")
      CALL system("./repl4" , RESULT)
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
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN5, FILE = '../pga/PGAA.ILS', STATUS = 'REPLACE')
      WRITE (IN5, 100) BFFR
      CLOSE (IN5, STATUS = 'KEEP')
      CLOSE (IN4, STATUS = 'KEEP')
C      CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 11
C     Run PGA
      CALL ChDir("../pga/")
      CALL system("./pga" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C     Set for synthetic photometry
      CALL SYSTEM("cp ../oa/GRPAAD2.DAT ../oa/PGAX2.DAT", RESULT)
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
      CALL system("./pgb" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1),STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C     Set for synthetic photometry
      CALL SYSTEM("cp ../oa/GRPABD2.DAT ../oa/PGBX2.DAT", RESULT)
      CALL SYSTEM("cp ../oa/GRPABD3.DAT ../oa/PGBX3.DAT", RESULT)
      CALL SYSTEM("cp ../oa/GRPABD3.DAT ../oa/PGBX3SV.DAT", RESULT)
      CALL SYSTEM("cp ../oa/GRPABD5.DAT ../oa/PGBX5.DAT", RESULT)
      CALL SYSTEM("cp ../oa/GRPABD6.DAT ../oa/PGBX6.DAT", RESULT)       
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
      CALL system("./pgc" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C     Set for synthetic photometry
      CALL SYSTEM("cp ../oa/GRPACD2.DAT ../oa/PGCX2.DAT", RESULT)
      CALL SYSTEM("cp ../oa/GRPACD3.DAT ../oa/PGCX3.DAT", RESULT)
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
      CALL system("./pgd" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C     Set for synthetic photometry
      CALL SYSTEM("cp ../oa/GRPADD3.DAT ../oa/PGDX3.DAT", RESULT)
      CALL SYSTEM("cp ../oa/GRPADD4.DAT ../oa/PGDX4.DAT", RESULT) 
      CALL SYSTEM("cp ../oa/GRPADD7.DAT ../oa/PGDX7.DAT", RESULT) 
      CALL SYSTEM("cp ../oa/GRPADD8.DAT ../oa/PGDX8.DAT", RESULT)  
C
      IF (ISW1.NE.0) THEN
C     Section to do synthetic photometry
      icount = 8
C     Run PLTB
      CALL ChDir("../pltb/")
      CALL system("./pltbscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
      icount = 8
C     Run ACPGA
      CALL ChDir("../acpga/")
      CALL system("./acpgascr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
      icount = 9
C     Run ACPGB
      CALL ChDir("../acpgb/")
      CALL system("./acpgbscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
      icount = 10
C     Run ACPGC
      CALL ChDir("../acpgc/")
      CALL system("./acpgcscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
      icount = 11
C     Run PGCAC
C      CALL ChDir("../acpgc/")
C      CALL system("./acpgcscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
      icount = 12
C     Run ACPGD
      CALL ChDir("../acpgd/")
      CALL system("./acpgdscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
      icount = 13
C     Run RDVEL2
      CALL ChDir("../rdvel2/")
      CALL system("./rdvel2scr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
      icount = 14
C     Run ACPGF6
      CALL ChDir("../acpgf6/")
      CALL system("./acpgf6scr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
      CALL system("cp ../oa/ACPGF61.DAT ../datconv/",RESULT)
      CALL system("cp ../oa/ACPGF61.DAT ../synacsp/",RESULT)
      CALL system("cp ../oa/PGBX3.DAT ../datconv/",RESULT)
      CALL system("cp ../oa/ACPGF61.DAT ../synacsp/",RESULT)
C
      icount = 15
C     Run SYNACSP
      CALL ChDir("../synacsp/")
      CALL system("./synacspscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
      icount = 16
C     Run SYNPHOT
      CALL ChDir("../synphot/")
      CALL system("./synphotscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
      icount = 17
C     Run REPLD
      CALL ChDir("../repld/")
      CALL system("./repldscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
C     Copy PGDX3R.DAT to PGDX3.DAT
      CALL ChDir("../oa/")
      CALL system("cp PGDX3R.DAT PGDX3.DAT")
      CALL system("cp PGDX3R.DAT GRPADD3.DAT")
      CALL ChDir(CWD)
C
      END IF
C
C     End of synthetic photometry section
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
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN3,  FILE = '../calpt/CALPTA.ILS', 
     $STATUS = 'REPLACE')
      WRITE (IN3, 100) BFFR
      CLOSE (IN3, STATUS ='KEEP')
      CLOSE (IN2, STATUS = 'KEEP')
C      CLOSE (IN16,STATUS = 'KEEP')
C
      icount = 15
C     RUN CALPT
      CALL ChDir("../calpt/")
      CALL system("./calpt" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1,101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
      CALL SYSTEM("cp ../oa/CALPTL1.DAT ../oa/CALPTO1.DAT", RESULT)
      CALL SYSTEM("cp ../ia/ALP1.INF ../ia/CALPT.INF", RESULT)
C
C     Set .ILS file for TRANSF1
      OPEN (IN12, FILE = 'TRANSF1.REF', STATUS = 'OLD')
      NRF=3
      DO J=1,NRF
            READ (IN12, 100) BFFR
      END DO
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN13, FILE = '../transf1/TRANSF1A.ILS', 
     $STATUS = 'REPLACE')
      WRITE (IN13, 100) BFFR
      CLOSE (IN13, STATUS = 'KEEP')
      CLOSE (IN12, STATUS = 'KEEP')
C      CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 16
C     Run TRANSF1
C     Output new PGAL1.IUP
      CALL ChDir("../transf1/")
      CALL system("./transf1" , RESULT)
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
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN15, FILE = '../repl4/REPL4A.ILS', 
     $STATUS = 'REPLACE')
      WRITE (IN15, 100) BFFR
      CLOSE (IN15, STATUS = 'KEEP')
      CLOSE (IN14, STATUS = 'KEEP')
C      CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 17
C     Run REPL4
C     Copy PGACR.IUP to PGACR.INF
      CALL ChDir("../repl4/")
      CALL system("./repl4" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
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
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN5, FILE = '../pga/PGAA.ILS', STATUS = 'REPLACE')
      WRITE (IN5, 100) BFFR
      CLOSE (IN5, STATUS = 'KEEP')
      CLOSE (IN4, STATUS = 'KEEP')
C      CLOSE (IN16, STATUS = 'KEEP')
C
      icount = 18
C     Run PGA
      CALL ChDir("../pga/")
      CALL system("./pga" , RESULT)
      CALL ChDir(CWD)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C     Set for synthetic photometry
      CALL SYSTEM("cp ../oa/GRPAAE2.DAT ../oa/PGAX2.DAT", RESULT)
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
C      icount = 19
C     Run PGB
      CALL ChDir("../pgb/")
      CALL system("./pgb" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1),STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C     Set for synthetic photometry
      CALL SYSTEM("cp ../oa/GRPABE2.DAT ../oa/PGBX2.DAT", RESULT)
      CALL SYSTEM("cp ../oa/GRPABE3.DAT ../oa/PGBX3.DAT", RESULT)
      CALL SYSTEM("cp ../oa/GRPABE3.DAT ../oa/PGBX3SV.DAT", RESULT)
      CALL SYSTEM("cp ../oa/GRPABE5.DAT ../oa/PGBX5.DAT", RESULT)
      CALL SYSTEM("cp ../oa/GRPABE6.DAT ../oa/PGBX6.DAT", RESULT)       
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
      CALL system("./pgc" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C     Set for synthetic photometry
      CALL SYSTEM("cp ../oa/GRPACC2.DAT ../oa/PGCX2.DAT", RESULT)
      CALL SYSTEM("cp ../oa/GRPACC3.DAT ../oa/PGCX3.DAT", RESULT)
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
      CALL system("./pgd" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C     Set for synthetic photometry
      CALL SYSTEM("cp ../oa/GRPADE3.DAT ../oa/PGDX3.DAT", RESULT)
      CALL SYSTEM("cp ../oa/GRPADE4.DAT ../oa/PGDX4.DAT", RESULT) 
      CALL SYSTEM("cp ../oa/GRPADE7.DAT ../oa/PGDX7.DAT", RESULT) 
      CALL SYSTEM("cp ../oa/GRPADE8.DAT ../oa/PGDX8.DAT", RESULT)  
C
      IF (ISW1.NE.0) THEN
C     Section to do synthetic photometry
      icount = 8
C     Run PLTB
      CALL ChDir("../pltb/")
      CALL system("./pltbscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
      icount = 8
C     Run ACPGA
      CALL ChDir("../acpga/")
      CALL system("./acpgascr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
      icount = 9
C     Run ACPGB
      CALL ChDir("../acpgb/")
      CALL system("./acpgbscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
C      icount = 10
C     Run ACPGC
      CALL ChDir("../acpgc/")
      CALL system("./acpgcscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
      icount = 11
C     Run PGCAC
C      CALL ChDir("../acpgc/")
C      CALL system("./acpgcscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
      icount = 12
C     Run ACPGD
      CALL ChDir("../acpgd/")
      CALL system("./acpgdscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
      icount = 13
C     Run RDVEL2
      CALL ChDir("../rdvel2/")
      CALL system("./rdvel2scr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
      icount = 14
C     Run ACPGF6
      CALL ChDir("../acpgf6/")
      CALL system("./acpgf6scr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
      CALL system("cp ../oa/ACPGF61.DAT ../datconv/",RESULT)
      CALL system("cp ../oa/ACPGF61.DAT ../synacsp/",RESULT)
      CALL system("cp ../oa/PGBX3.DAT ../datconv/",RESULT)
      CALL system("cp ../oa/ACPGF61.DAT ../synacsp/",RESULT)
C
C      icount = 15
C     Run SYNACSP
      CALL ChDir("../synacsp/")
      CALL system("./synacspscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
      icount = 16
C     Run SYNPHOT
      CALL ChDir("../synphot/")
      CALL system("./synphotscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
C      icount = 17
C     Run REPLD
      CALL ChDir("../repld/")
      CALL system("./repldscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C     Copy PGDX3R.DAT to PGDX3.DAT
      CALL ChDir("../oa/")
      CALL system("cp PGDX3R.DAT PGDX3.DAT")
      CALL system("cp PGDX3R.DAT GRPADE3.DAT")
      CALL ChDir(CWD)
C
      END IF
C
C     End of synthetic photometry section
C
      icount = 22
      CALL ChDir("../pgh1/")
      CALL system("./pgh1" , RESULT)
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
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN3,  FILE = '../calpt/CALPTA.ILS', 
     $STATUS = 'REPLACE')
      WRITE (IN3, 100) BFFR
      CLOSE (IN3, STATUS ='KEEP')
      CLOSE (IN2, STATUS = 'KEEP')
C      CLOSE (IN16,STATUS = 'KEEP')
C
      OPEN (IN12, FILE = 'TRANSF1.REF', STATUS = 'OLD')
      NRF=4
      DO J=1,NRF
            READ (IN12, 100) BFFR
      END DO
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN13, FILE = '../transf1/TRANSF1A.ILS', 
     $STATUS = 'REPLACE')
      WRITE (IN13, 100) BFFR
      CLOSE (IN13, STATUS = 'KEEP')
      CLOSE (IN12, STATUS = 'KEEP')
C      CLOSE (IN16, STATUS = 'KEEP')
C
      OPEN (IN14, FILE = 'REPL4.REF', STATUS = 'OLD')
      NFR=4
      DO J=1,NRF
            READ (IN14, 100) BFFR
      END DO
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN15, FILE = '../repl4/REPL4A.ILS', 
     $STATUS = 'REPLACE')
      WRITE (IN15, 100) BFFR
      CLOSE (IN15, STATUS = 'KEEP')
      CLOSE (IN14, STATUS = 'KEEP')
C      CLOSE (IN16, STATUS = 'KEEP')
C
      OPEN (IN4, FILE = 'PGA.REF', STATUS = 'OLD')
      NRF=4
      DO J=1,NRF
            READ (IN4, 100) BFFR
      END DO
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN5, FILE = '../pga/PGAA.ILS', STATUS = 'REPLACE')
      WRITE (IN5, 100) BFFR
      CLOSE (IN5, STATUS = 'KEEP')
      CLOSE (IN4, STATUS = 'KEEP')
C      CLOSE (IN16, STATUS = 'KEEP')
C
      OPEN (IN6, FILE = 'PGB.REF')
      NRF=4
      DO J=1,NRF
            READ (IN6, 100) BFFR
      END DO
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN7, FILE = '../pgb/PGBA.ILS', STATUS = 'REPLACE')
      WRITE (IN7, 100) BFFR
      CLOSE (IN7, STATUS = 'KEEP')
      CLOSE (IN6,STATUS = 'KEEP')
C      CLOSE (IN16, STATUS = 'KEEP')
C
      OPEN (IN8, FILE = 'PGC.REF', STATUS = 'OLD')
      NRF=4
      DO J=1,NRF
            READ (IN8, 100) BFFR
      END DO
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN9, FILE = '../pgc/PGCA.ILS', STATUS = 'REPLACE')
      WRITE (IN9, 100) BFFR
      CLOSE (IN9, STATUS = 'KEEP')
      CLOSE (IN8, STATUS = 'KEEP')
C      CLOSE (IN16, STATUS = 'KEEP')
C
      OPEN (IN10, FILE = 'PGD.REF', STATUS = 'OLD')
      NRF=4
      DO J=1, NRF
            READ (IN10, 100) BFFR
      END DO
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
      OPEN (IN11, FILE = '../pgd/PGDA.ILS', STATUS = 'REPLACE')
      WRITE (IN11, 100) BFFR
      CLOSE (IN11, STATUS = 'KEEP')
      CLOSE (IN10, STATUS = 'KEEP')
C      CLOSE (IN16, STATUS = 'KEEP')
C
      WRITE (NOUT, 103)
  103 FORMAT (1X,'End of Program GRPA')
   99 CLOSE (UNIT = IN1,   STATUS = 'KEEP')
      CLOSE (UNIT = NOUT,  STATUS = 'KEEP')
      WRITE (*,'(1X,A,I2)') 'Value of LINKV.DAT=',KEYV
      STOP 'grpa: Completed successfully'
      END PROGRAM GRPA
C
      SUBROUTINE TSOUT(RESULT, icount, NOUT)
      INTEGER(4) RESULT
      IF (RESULT.NE.0) THEN
            WRITE (NOUT, 101) icount, RESULT
  101 FORMAT (1X,'Unable to run, icount=',I4,2X,'RESULT=',I4)
            CALL EXIT(-1)
      ELSE
            WRITE (NOUT, 102) icount
  102 FORMAT (1X,'Successfully completed icount=',I4)
            RETURN
      ENDIF
      END SUBROUTINE TSOUT
