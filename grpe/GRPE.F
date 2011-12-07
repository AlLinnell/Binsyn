      PROGRAM GRPE
C**********************************************************************
C     Program to run the programs in GRPE
      DIMENSION FL1(3)
      INTEGER (4) RESULT
      CHARACTER*80 BFFR
      CHARACTER * 24 FL1
      CHARACTER * 256 CWD
      DATA IN1/1/
      DATA IN8,IN9,IN10,IN11/8,9,10,11/
      DATA IN16,NOUT,INF/16,17,18/
  100 FORMAT (A80)
  101 FORMAT (I2)
  105 FORMAT (5I5)
C**********************************************************************
C     Store the current working directory (CWD).  NOTE: relative
C     filenames may be re-opened during execution, so this is critical.
      CALL GetCWD(CWD, RESULT)
      OPEN (19, FILE = 'GRPE.ILS', STATUS = 'OLD')
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
      READ (INF, 105) ISW1,ISW2
C
C     Set .ILS file for PGCT1L
      OPEN (IN8, FILE = 'PGC.REF', STATUS = 'OLD')
      NRF=1
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
      icount = 1
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
      CALL SYSTEM("cp ../oa/CT1L2.DAT ../oa/PGCX2.DAT", RESULT)
      CALL SYSTEM("cp ../oa/CT1L3.DAT ../oa/PGCX3.DAT", RESULT)
C
C     Set .ILS file for PGDT1L
      OPEN (IN10, FILE = 'PGD.REF', STATUS = 'OLD')
      NRF=1
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
      icount = 2
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
      CALL SYSTEM("cp ../oa/DT1L3.DAT ../oa/PGDX3.DAT", RESULT)
      CALL SYSTEM("cp ../oa/DT1L4.DAT ../oa/PGDX4.DAT", RESULT) 
      CALL SYSTEM("cp ../oa/DT1L7.DAT ../oa/PGDX7.DAT", RESULT) 
      CALL SYSTEM("cp ../oa/DT1L8.DAT ../oa/PGDX8.DAT", RESULT)  
C
      IF (ISW1.NE.0) THEN
C     Section to treat synthetic photometry
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
      CALL ChDir("../acpgc/")
      CALL system("./acpgcscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
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
      CALL system("cp PGDX3R.DAT DT1L3.DAT")
      CALL ChDir(CWD)
C
      END IF
C
C     End of synthetic photometry section
C
C     Set .ILS file for PGCT1S
      OPEN (IN8, FILE = 'PGC.REF', STATUS = 'OLD')
      NRF=2
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
      icount = 3
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
      CALL SYSTEM("cp ../oa/CT1S2.DAT ../oa/PGCX2.DAT", RESULT)
      CALL SYSTEM("cp ../oa/CT1S3.DAT ../oa/PGCX3.DAT", RESULT)
C
C     Set .ILS file for PGDT1S
      OPEN (IN10, FILE = 'PGD.REF', STATUS = 'OLD')
      NRF=2
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
      icount = 4
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
      CALL SYSTEM("cp ../oa/DT1S3.DAT ../oa/PGDX3.DAT", RESULT)
      CALL SYSTEM("cp ../oa/DT1S4.DAT ../oa/PGDX4.DAT", RESULT) 
      CALL SYSTEM("cp ../oa/DT1S7.DAT ../oa/PGDX7.DAT", RESULT) 
      CALL SYSTEM("cp ../oa/DT1S8.DAT ../oa/PGDX8.DAT", RESULT)  
C
      IF (ISW1.NE.0) THEN
C     Section to treat synthetic photometry
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
      CALL ChDir("../acpgc/")
      CALL system("./acpgcscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
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
      CALL system("cp PGDX3R.DAT DT1S3.DAT")
      CALL ChDir(CWD)
C
      END IF
C
C     End of synthetic photometry section
C
C     Set .ILS file for PGCT2L
      OPEN (IN8, FILE = 'PGC.REF', STATUS = 'OLD')
      NRF=3
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
      icount = 5
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
      CALL SYSTEM("cp ../oa/CT2L2.DAT ../oa/PGCX2.DAT", RESULT)
      CALL SYSTEM("cp ../oa/CT2L3.DAT ../oa/PGCX3.DAT", RESULT)
C
C     Set .ILS file for PGDT2L
      OPEN (IN10, FILE = 'PGD.REF', STATUS = 'OLD')
      NRF=3
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
      icount = 6
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
      CALL SYSTEM("cp ../oa/DT2L3.DAT ../oa/PGDX3.DAT", RESULT)
      CALL SYSTEM("cp ../oa/DT2L4.DAT ../oa/PGDX4.DAT", RESULT) 
      CALL SYSTEM("cp ../oa/DT2L7.DAT ../oa/PGDX7.DAT", RESULT) 
      CALL SYSTEM("cp ../oa/DT2L8.DAT ../oa/PGDX8.DAT", RESULT)  
C
      IF (ISW1.NE.0) THEN
C     Section to treat synthetic photometry
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
      CALL ChDir("../acpgc/")
      CALL system("./acpgcscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
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
      CALL system("cp PGDX3R.DAT DT2L3.DAT")
      CALL ChDir(CWD)
C
      END IF
C
C     End of synthetic photometry section
C
C     Set .ILS file for PGCT2S
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
      icount = 7
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
      CALL SYSTEM("cp ../oa/CT2S2.DAT ../oa/PGCX2.DAT", RESULT)
      CALL SYSTEM("cp ../oa/CT2S3.DAT ../oa/PGCX3.DAT", RESULT)
C
C     Set .ILS file for PGDT2S
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
      icount = 8
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
      CALL SYSTEM("cp ../oa/DT2S3.DAT ../oa/PGDX3.DAT", RESULT)
      CALL SYSTEM("cp ../oa/DT2S4.DAT ../oa/PGDX4.DAT", RESULT) 
      CALL SYSTEM("cp ../oa/DT2S7.DAT ../oa/PGDX7.DAT", RESULT) 
      CALL SYSTEM("cp ../oa/DT2S8.DAT ../oa/PGDX8.DAT", RESULT)  
C
      IF (ISW1.NE.0) THEN
C     Section to treat synthetic photometry
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
      CALL ChDir("../acpgc/")
      CALL system("./acpgcscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
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
      CALL system("cp PGDX3R.DAT DT2S3.DAT")
      CALL ChDir(CWD)
C
      END IF
C
C     End of synthetic photometry section
C     ****NOTE****
C     DERIVATIVES FOR BOLOMETRIC ALBEDO AND GRAVITY DARKENING HAVE BEEN
C     COMMENTED OUT IN THE FOLLOWING SECTION.
C     IF THEY ARE TO BE IMPLEMENTED SUBSTANTIAL PROGRAM ADDITIONS ARE NEEDED!
C
C     Set .ILS file for PGCA1L
C      OPEN (IN8, FILE = 'PGC.REF', STATUS = 'OLD')
C      NRF=5
C      DO J=1,NRF
C            READ (IN8, 100) BFFR
C      END DO
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
C      OPEN (IN9, FILE = '../pgc/PGCA.ILS', STATUS = 'REPLACE')
C      WRITE (IN9, 100) BFFR
C      CLOSE (IN9, STATUS = 'KEEP')
C      CLOSE (IN8, STATUS = 'KEEP')
C      CLOSE (IN16, STATUS = 'KEEP')
C
C      icount = 9
C     Run PGC
C      CALL ChDir("../pgc/")
C      CALL system("./pgc" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set .ILS file for PGDA1L
C      OPEN (IN10, FILE = 'PGD.REF', STATUS = 'OLD')
C      NRF=5
C      DO J=1, NRF
C            READ (IN10, 100) BFFR
C      END DO
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
C      OPEN (IN11, FILE = '../pgd/PGDA.ILS', STATUS = 'REPLACE')
C      WRITE (IN11, 100) BFFR
C      CLOSE (IN11, STATUS = 'KEEP')
C      CLOSE (IN10, STATUS = 'KEEP')
C      CLOSE (IN16, STATUS = 'KEEP')
C
C      icount = 10
C     Run PGD
C      CALL ChDir("../pgd/")
C      CALL system("./pgd" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C      IF (ISW1.NE.0) THEN
C     Section to treat synthetic photometry
C      icount = 8
C     Run ACPGA
C      CALL ChDir("../acpga/")
C      CALL system("./acpgascr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 9
C     Run ACPGB
C      CALL ChDir("../acpgb/")
C      CALL system("./acpgbscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 10
C     Run ACPGC
C      CALL ChDir("../acpgc/")
C      CALL system("./acpgcscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 11
C     Run PGCAC
C      CALL ChDir("../acpgc/")
C      CALL system("./acpgcscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 12
C     Run ACPGD
C      CALL ChDir("../acpgd/")
C      CALL system("./acpgdscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 13
C     Run RDVEL2
C      CALL ChDir("../rdvel2/")
C      CALL system("./rdvel2scr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 14
C     Run ACPGF6
C      CALL ChDir("../acpgf6/")
C      CALL system("./acpgf6scr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C      CALL system("cp ../oa/ACPGF61.DAT ../datconv/",RESULT)
C      CALL system("cp ../oa/ACPGF61.DAT ../synacsp/",RESULT)
C      CALL system("cp ../oa/PGBX3.DAT ../datconv/",RESULT)
C      CALL system("cp ../oa/ACPGF61.DAT ../synacsp/",RESULT)
C
C      icount = 15
C     Run SYNACSP
C      CALL ChDir("../synacsp/")
C      CALL system("./synacspscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 16
C     Run SYNPHOT
C      CALL ChDir("../synphot/")
C      CALL system("./synphotscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C
C      icount = 17
C     Run REPLD
C      CALL ChDir("../repld/")
C      CALL system("./repldscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C     Copy PGDX3R.DAT to PGDX3.DAT
C      CALL ChDir("../oa/")
C      CALL system("cp PGDX3R.DAT PGDX3.DAT")
C      CALL system("cp PGDX3R.DAT DA1L3.DAT")
C      CALL ChDir(CWD)
C
C      END IF
C
C     End of synthetic photometry section
C
C     Set .ILS file for PGCA1S
C      OPEN (IN8, FILE = 'PGC.REF', STATUS = 'OLD')
C      NRF=6
C      DO J=1,NRF
C            READ (IN8, 100) BFFR
C      END DO
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
C      OPEN (IN9, FILE = '../pgc/PGCA.ILS', STATUS = 'REPLACE')
C      WRITE (IN9, 100) BFFR
C      CLOSE (IN9, STATUS = 'KEEP')
C      CLOSE (IN8, STATUS = 'KEEP')
C      CLOSE (IN16, STATUS = 'KEEP')
C
C      icount = 11
C     Run PGC
C      CALL ChDir("../pgc/")
C      CALL system("./pgc" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set .ILS file for PGDA1S
C      OPEN (IN10, FILE = 'PGD.REF', STATUS = 'OLD')
C      NRF=6
C      DO J=1, NRF
C            READ (IN10, 100) BFFR
C      END DO
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
C      OPEN (IN11, FILE = '../pgd/PGDA.ILS', STATUS = 'REPLACE')
C      WRITE (IN11, 100) BFFR
C      CLOSE (IN11, STATUS = 'KEEP')
C      CLOSE (IN10, STATUS = 'KEEP')
C      CLOSE (IN16, STATUS = 'KEEP')
C
C      icount = 12
C     Run PGD
C      CALL ChDir("../pgd/")
C      CALL system("./pgd" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C      IF (ISW1.NE.0) THEN
C     Section to treat synthetic photometry
C      icount = 8
C     Run ACPGA
C      CALL ChDir("../acpga/")
C      CALL system("./acpgascr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 9
C     Run ACPGB
C      CALL ChDir("../acpgb/")
C      CALL system("./acpgbscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 10
C     Run ACPGC
C      CALL ChDir("../acpgc/")
C      CALL system("./acpgcscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 11
C     Run PGCAC
C      CALL ChDir("../acpgc/")
C      CALL system("./acpgcscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 12
C     Run ACPGD
C      CALL ChDir("../acpgd/")
C      CALL system("./acpgdscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 13
C     Run RDVEL2
C      CALL ChDir("../rdvel2/")
C      CALL system("./rdvel2scr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 14
C     Run ACPGF6
C      CALL ChDir("../acpgf6/")
C      CALL system("./acpgf6scr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C      CALL system("cp ../oa/ACPGF61.DAT ../datconv/",RESULT)
C      CALL system("cp ../oa/ACPGF61.DAT ../synacsp/",RESULT)
C      CALL system("cp ../oa/PGBX3.DAT ../datconv/",RESULT)
C      CALL system("cp ../oa/ACPGF61.DAT ../synacsp/",RESULT)
C
C      icount = 15
C     Run SYNACSP
C      CALL ChDir("../synacsp/")
C      CALL system("./synacspscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 16
C     Run SYNPHOT
C      CALL ChDir("../synphot/")
C      CALL system("./synphotscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C
C      icount = 17
C     Run REPLD
C      CALL ChDir("../repld/")
C      CALL system("./repldscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C     Copy PGDX3R.DAT to PGDX3.DAT
C      CALL ChDir("../oa/")
C      CALL system("cp PGDX3R.DAT PGDX3.DAT")
C      CALL system("cp PGDX3R.DAT DA1S3.DAT")
C      CALL ChDir(CWD)
C
C      END IF
C
C     End of synthetic photometry section
C
C     Set .ILS file for PGCA2L
C      OPEN (IN8, FILE = 'PGC.REF', STATUS = 'OLD')
C      NRF=7
C      DO J=1,NRF
C            READ (IN8, 100) BFFR
C      END DO
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
C      OPEN (IN9, FILE = '../pgc/PGCA.ILS', STATUS = 'REPLACE')
C      WRITE (IN9, 100) BFFR
C      CLOSE (IN9, STATUS = 'KEEP')
C      CLOSE (IN8, STATUS = 'KEEP')
C      CLOSE (IN16, STATUS = 'KEEP')
C
C      icount = 13
C     Run PGC
C      CALL ChDir("../pgc/")
C      CALL system("./pgc" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set .ILS file for PGDA2L
C      OPEN (IN10, FILE = 'PGD.REF', STATUS = 'OLD')
C      NRF=7
C      DO J=1, NRF
C            READ (IN10, 100) BFFR
C      END DO
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
C      OPEN (IN11, FILE = '../pgd/PGDA.ILS', STATUS = 'REPLACE')
C      WRITE (IN11, 100) BFFR
C      CLOSE (IN11, STATUS = 'KEEP')
C      CLOSE (IN10, STATUS = 'KEEP')
C      CLOSE (IN16, STATUS = 'KEEP')
C
C      icount = 14
C     Run PGD
C      CALL ChDir("../pgd/")
C      CALL system("./pgd" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C      IF (ISW1.NE.0) THEN
C     Section to treat synthetic photometry
C      icount = 8
C     Run ACPGA
C      CALL ChDir("../acpga/")
C      CALL system("./acpgascr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 9
C     Run ACPGB
C      CALL ChDir("../acpgb/")
C      CALL system("./acpgbscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 10
C     Run ACPGC
C      CALL ChDir("../acpgc/")
C      CALL system("./acpgcscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 11
C     Run PGCAC
C      CALL ChDir("../acpgc/")
C      CALL system("./acpgcscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 12
C     Run ACPGD
C      CALL ChDir("../acpgd/")
C      CALL system("./acpgdscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 13
C     Run RDVEL2
C      CALL ChDir("../rdvel2/")
C      CALL system("./rdvel2scr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 14
C     Run ACPGF6
C      CALL ChDir("../acpgf6/")
C      CALL system("./acpgf6scr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C      CALL system("cp ../oa/ACPGF61.DAT ../datconv/",RESULT)
C      CALL system("cp ../oa/ACPGF61.DAT ../synacsp/",RESULT)
C      CALL system("cp ../oa/PGBX3.DAT ../datconv/",RESULT)
C      CALL system("cp ../oa/ACPGF61.DAT ../synacsp/",RESULT)
C
C      icount = 15
C     Run SYNACSP
C      CALL ChDir("../synacsp/")
C      CALL system("./synacspscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 16
C     Run SYNPHOT
C      CALL ChDir("../synphot/")
C      CALL system("./synphotscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C
C      icount = 17
C     Run REPLD
C      CALL ChDir("../repld/")
C      CALL system("./repldscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C     Copy PGDX3R.DAT to PGDX3.DAT
C      CALL ChDir("../oa/")
C      CALL system("cp PGDX3R.DAT PGDX3.DAT")
C      CALL system("cp PGDX3R.DAT DA2L3.DAT")
C      CALL ChDir(CWD)
C
C      END IF
C
C     End of synthetic photometry section
C
C     Set .ILS file for PGCA2S
C      OPEN (IN8, FILE = 'PGC.REF', STATUS = 'OLD')
C      NRF=8
C      DO J=1,NRF
C            READ (IN8, 100) BFFR
C      END DO
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
C      OPEN (IN9, FILE = '../pgc/PGCA.ILS', STATUS = 'REPLACE')
C      WRITE (IN9, 100) BFFR
C      CLOSE (IN9, STATUS = 'KEEP')
C      CLOSE (IN8, STATUS = 'KEEP')
C      CLOSE (IN16, STATUS = 'KEEP')
C
C      icount = 15
C     Run PGC
C      CALL ChDir("../pgc/")
C      CALL system("./pgc" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set .ILS file for PGDA2S
C      OPEN (IN10, FILE = 'PGD.REF', STATUS = 'OLD')
C      NRF=8
C      DO J=1, NRF
C            READ (IN10, 100) BFFR
C      END DO
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
C      OPEN (IN11, FILE = '../pgd/PGDA.ILS', STATUS = 'REPLACE')
C      WRITE (IN11, 100) BFFR
C      CLOSE (IN11, STATUS = 'KEEP')
C      CLOSE (IN10, STATUS = 'KEEP')
C      CLOSE (IN16, STATUS = 'KEEP')
C
C      icount = 16
C     Run PGD
C      CALL ChDir("../pgd/")
C      CALL system("./pgd" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C      IF (ISW1.NE.0) THEN
C     Section to treat synthetic photometry
C      icount = 8
C     Run ACPGA
C      CALL ChDir("../acpga/")
C      CALL system("./acpgascr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 9
C     Run ACPGB
C      CALL ChDir("../acpgb/")
C      CALL system("./acpgbscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 10
C     Run ACPGC
C      CALL ChDir("../acpgc/")
C      CALL system("./acpgcscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 11
C     Run PGCAC
C      CALL ChDir("../acpgc/")
C      CALL system("./acpgcscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 12
C     Run ACPGD
C      CALL ChDir("../acpgd/")
C      CALL system("./acpgdscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 13
C     Run RDVEL2
C      CALL ChDir("../rdvel2/")
C      CALL system("./rdvel2scr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 14
C     Run ACPGF6
C      CALL ChDir("../acpgf6/")
C      CALL system("./acpgf6scr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C      CALL system("cp ../oa/ACPGF61.DAT ../datconv/",RESULT)
C      CALL system("cp ../oa/ACPGF61.DAT ../synacsp/",RESULT)
C      CALL system("cp ../oa/PGBX3.DAT ../datconv/",RESULT)
C      CALL system("cp ../oa/ACPGF61.DAT ../synacsp/",RESULT)
C
C      icount = 15
C     Run SYNACSP
C      CALL ChDir("../synacsp/")
C      CALL system("./synacspscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 16
C     Run SYNPHOT
C      CALL ChDir("../synphot/")
C      CALL system("./synphotscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C
C      icount = 17
C     Run REPLD
C      CALL ChDir("../repld/")
C      CALL system("./repldscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C     Copy PGDX3R.DAT to PGDX3.DAT
C      CALL ChDir("../oa/")
C      CALL system("cp PGDX3R.DAT PGDX3.DAT")
C      CALL system("cp PGDX3R.DAT DA2S3.DAT")
C      CALL ChDir(CWD)
C
C      END IF
C
C     End of synthetic photometry section
C
C     Set .ILS file for PGCG1L
C      OPEN (IN8, FILE = 'PGC.REF', STATUS = 'OLD')
C      NRF=9
C      DO J=1,NRF
C            READ (IN8, 100) BFFR
C      END DO
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
C      OPEN (IN9, FILE = '../pgc/PGCA.ILS', STATUS = 'REPLACE')
C      WRITE (IN9, 100) BFFR
C      CLOSE (IN9, STATUS = 'KEEP')
C      CLOSE (IN8, STATUS = 'KEEP')
C      CLOSE (IN16, STATUS = 'KEEP')
C
C      icount = 17
C     Run PGC
C      CALL ChDir("../pgc/")
C      CALL system("./pgc" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set .ILS file for PGDG1L
C      OPEN (IN10, FILE = 'PGD.REF', STATUS = 'OLD')
C      NRF=9
C      DO J=1, NRF
C            READ (IN10, 100) BFFR
C      END DO
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
C      OPEN (IN11, FILE = '../pgd/PGDA.ILS', STATUS = 'REPLACE')
C      WRITE (IN11, 100) BFFR
C      CLOSE (IN11, STATUS = 'KEEP')
C      CLOSE (IN10, STATUS = 'KEEP')
C      CLOSE (IN16, STATUS = 'KEEP')
C
C      icount = 18
C     Run PGD
C      CALL ChDir("../pgd/")
C      CALL system("./pgd" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C      IF (ISW1.NE.0) THEN
C     Section to treat synthetic photometry
C      icount = 8
C     Run ACPGA
C      CALL ChDir("../acpga/")
C      CALL system("./acpgascr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 9
C     Run ACPGB
C      CALL ChDir("../acpgb/")
C      CALL system("./acpgbscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 10
C     Run ACPGC
C      CALL ChDir("../acpgc/")
C      CALL system("./acpgcscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 11
C     Run PGCAC
C      CALL ChDir("../acpgc/")
C      CALL system("./acpgcscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 12
C     Run ACPGD
C      CALL ChDir("../acpgd/")
C      CALL system("./acpgdscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 13
C     Run RDVEL2
C      CALL ChDir("../rdvel2/")
C      CALL system("./rdvel2scr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 14
C     Run ACPGF6
C      CALL ChDir("../acpgf6/")
C      CALL system("./acpgf6scr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C      CALL system("cp ../oa/ACPGF61.DAT ../datconv/",RESULT)
C      CALL system("cp ../oa/ACPGF61.DAT ../synacsp/",RESULT)
C      CALL system("cp ../oa/PGBX3.DAT ../datconv/",RESULT)
C      CALL system("cp ../oa/ACPGF61.DAT ../synacsp/",RESULT)
C
C      icount = 15
C     Run SYNACSP
C      CALL ChDir("../synacsp/")
C      CALL system("./synacspscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 16
C     Run SYNPHOT
C      CALL ChDir("../synphot/")
C      CALL system("./synphotscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C
C      icount = 17
C     Run REPLD
C      CALL ChDir("../repld/")
C      CALL system("./repldscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C     Copy PGDX3R.DAT to PGDX3.DAT
C      CALL ChDir("../oa/")
C      CALL system("cp PGDX3R.DAT PGDX3.DAT")
C      CALL system("cp PGDX3R.DAT DG1L3.DAT")
C      CALL ChDir(CWD)
C
C      END IF
C
C     End of synthetic photometry section
C
C     Set .ILS file for PGCG1S
C      OPEN (IN8, FILE = 'PGC.REF', STATUS = 'OLD')
C      NRF=10
C      DO J=1,NRF
C            READ (IN8, 100) BFFR
C      END DO
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
C      OPEN (IN9, FILE = '../pgc/PGCA.ILS', STATUS = 'REPLACE')
C      WRITE (IN9, 100) BFFR
C      CLOSE (IN9, STATUS = 'KEEP')
C      CLOSE (IN8, STATUS = 'KEEP')
C      CLOSE (IN16, STATUS = 'KEEP')
C
C      icount = 19
C     Run PGC
C      CALL ChDir("../pgc/")
C      CALL system("./pgc" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set .ILS file for PGDG1S
C      OPEN (IN10, FILE = 'PGD.REF', STATUS = 'OLD')
C      NRF=10
C      DO J=1, NRF
C            READ (IN10, 100) BFFR
C      END DO
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
C      OPEN (IN11, FILE = '../pgd/PGDA.ILS', STATUS = 'REPLACE')
C      WRITE (IN11, 100) BFFR
C      CLOSE (IN11, STATUS = 'KEEP')
C      CLOSE (IN10, STATUS = 'KEEP')
C      CLOSE (IN16, STATUS = 'KEEP')
C
C      icount = 20
C     Run PGD
C      CALL ChDir("../pgd/")
C      CALL system("./pgd" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C      IF (ISW1.NE.0) THEN
C     Section to treat synthetic photometry
C      icount = 8
C     Run ACPGA
C      CALL ChDir("../acpga/")
C      CALL system("./acpgascr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 9
C     Run ACPGB
C      CALL ChDir("../acpgb/")
C      CALL system("./acpgbscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 10
C     Run ACPGC
C      CALL ChDir("../acpgc/")
C      CALL system("./acpgcscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 11
C     Run PGCAC
C      CALL ChDir("../acpgc/")
C      CALL system("./acpgcscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 12
C     Run ACPGD
C      CALL ChDir("../acpgd/")
C      CALL system("./acpgdscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 13
C     Run RDVEL2
C      CALL ChDir("../rdvel2/")
C      CALL system("./rdvel2scr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 14
C     Run ACPGF6
C      CALL ChDir("../acpgf6/")
C      CALL system("./acpgf6scr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C      CALL system("cp ../oa/ACPGF61.DAT ../datconv/",RESULT)
C      CALL system("cp ../oa/ACPGF61.DAT ../synacsp/",RESULT)
C      CALL system("cp ../oa/PGBX3.DAT ../datconv/",RESULT)
C      CALL system("cp ../oa/ACPGF61.DAT ../synacsp/",RESULT)
C
C      icount = 15
C     Run SYNACSP
C      CALL ChDir("../synacsp/")
C      CALL system("./synacspscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 16
C     Run SYNPHOT
C      CALL ChDir("../synphot/")
C      CALL system("./synphotscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C
C      icount = 17
C     Run REPLD
C      CALL ChDir("../repld/")
C      CALL system("./repldscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C     Copy PGDX3R.DAT to PGDX3.DAT
C      CALL ChDir("../oa/")
C      CALL system("cp PGDX3R.DAT PGDX3.DAT")
C      CALL system("cp PGDX3R.DAT DG1S3.DAT")
C      CALL ChDir(CWD)
C
C      END IF
C
C     End of synthetic photometry section
C
C     Set .ILS file for PGCG2L
C      OPEN (IN8, FILE = 'PGC.REF', STATUS = 'OLD')
C      NRF=11
C      DO J=1,NRF
C            READ (IN8, 100) BFFR
C      END DO
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
C      OPEN (IN9, FILE = '../pgc/PGCA.ILS', STATUS = 'REPLACE')
C      WRITE (IN9, 100) BFFR
C      CLOSE (IN9, STATUS = 'KEEP')
C      CLOSE (IN8, STATUS = 'KEEP')
C      CLOSE (IN16, STATUS = 'KEEP')
C
C      icount = 21
C     Run PGC
C      CALL ChDir("../pgc/")
C      CALL system("./pgc" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set .ILS file for PGDG2L
C      OPEN (IN10, FILE = 'PGD.REF', STATUS = 'OLD')
C      NRF=11
C      DO J=1, NRF
C            READ (IN10, 100) BFFR
C      END DO
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
C      OPEN (IN11, FILE = '../pgd/PGDA.ILS', STATUS = 'REPLACE')
C      WRITE (IN11, 100) BFFR
C      CLOSE (IN11, STATUS = 'KEEP')
C      CLOSE (IN10, STATUS = 'KEEP')
C      CLOSE (IN16, STATUS = 'KEEP')
C
C      icount = 22
C     Run PGD
C      CALL ChDir("../pgd/")
C      CALL system("./pgd" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C      IF (ISW1.NE.0) THEN
C     Section to treat synthetic photometry
C      icount = 8
C     Run ACPGA
C      CALL ChDir("../acpga/")
C      CALL system("./acpgascr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 9
C     Run ACPGB
C      CALL ChDir("../acpgb/")
C      CALL system("./acpgbscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 10
C     Run ACPGC
C      CALL ChDir("../acpgc/")
C      CALL system("./acpgcscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 11
C     Run PGCAC
C      CALL ChDir("../acpgc/")
C      CALL system("./acpgcscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 12
C     Run ACPGD
C      CALL ChDir("../acpgd/")
C      CALL system("./acpgdscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 13
C     Run RDVEL2
C      CALL ChDir("../rdvel2/")
C      CALL system("./rdvel2scr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 14
C     Run ACPGF6
C      CALL ChDir("../acpgf6/")
C      CALL system("./acpgf6scr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C      CALL system("cp ../oa/ACPGF61.DAT ../datconv/",RESULT)
C      CALL system("cp ../oa/ACPGF61.DAT ../synacsp/",RESULT)
C      CALL system("cp ../oa/PGBX3.DAT ../datconv/",RESULT)
C      CALL system("cp ../oa/ACPGF61.DAT ../synacsp/",RESULT)
C
C      icount = 15
C     Run SYNACSP
C      CALL ChDir("../synacsp/")
C      CALL system("./synacspscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 16
C     Run SYNPHOT
C      CALL ChDir("../synphot/")
C      CALL system("./synphotscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C
C      icount = 17
C     Run REPLD
C      CALL ChDir("../repld/")
C      CALL system("./repldscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C     Copy PGDX3R.DAT to PGDX3.DAT
C      CALL ChDir("../oa/")
C      CALL system("cp PGDX3R.DAT PGDX3.DAT")
C      CALL system("cp PGDX3R.DAT DG2L3.DAT")
C      CALL ChDir(CWD)
C
C      END IF
C
C     End of synthetic photometry section
C
C     Set .ILS file for PGCG2S
C      OPEN (IN8, FILE = 'PGC.REF', STATUS = 'OLD')
C      NRF=12
C      DO J=1,NRF
C            READ (IN8, 100) BFFR
C      END DO
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
C      OPEN (IN9, FILE = '../pgc/PGCA.ILS', STATUS = 'REPLACE')
C      WRITE (IN9, 100) BFFR
C      CLOSE (IN9, STATUS = 'KEEP')
C      CLOSE (IN8, STATUS = 'KEEP')
C      CLOSE (IN16, STATUS = 'KEEP')
C
C      icount = 23
C     Run PGC
C      CALL ChDir("../pgc/")
C      CALL system("./pgc" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Set .ILS file for PGDG2S
C      OPEN (IN10, FILE = 'PGD.REF', STATUS = 'OLD')
C      NRF=12
C      DO J=1, NRF
C            READ (IN10, 100) BFFR
C      END DO
C      OPEN (IN16, FILE = BFFR, STATUS = 'OLD')
C      OPEN (IN11, FILE = '../pgd/PGDA.ILS', STATUS = 'REPLACE')
C      WRITE (IN11, 100) BFFR
C      CLOSE (IN11, STATUS = 'KEEP')
C      CLOSE (IN10, STATUS = 'KEEP')
C      CLOSE (IN16, STATUS = 'KEEP')
C
C      icount = 24
C     Run PGD
C      CALL ChDir("../pgd/")
C      CALL system("./pgd" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C      IF (ISW1.NE.0) THEN
C     Section to treat synthetic photometry
C      icount = 8
C     Run ACPGA
C      CALL ChDir("../acpga/")
C      CALL system("./acpgascr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 9
C     Run ACPGB
C      CALL ChDir("../acpgb/")
C      CALL system("./acpgbscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 10
C     Run ACPGC
C      CALL ChDir("../acpgc/")
C      CALL system("./acpgcscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 11
C     Run PGCAC
C      CALL ChDir("../acpgc/")
C      CALL system("./acpgcscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 12
C     Run ACPGD
C      CALL ChDir("../acpgd/")
C      CALL system("./acpgdscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 13
C     Run RDVEL2
C      CALL ChDir("../rdvel2/")
C      CALL system("./rdvel2scr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 14
C     Run ACPGF6
C      CALL ChDir("../acpgf6/")
C      CALL system("./acpgf6scr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C      CALL system("cp ../oa/ACPGF61.DAT ../datconv/",RESULT)
C      CALL system("cp ../oa/ACPGF61.DAT ../synacsp/",RESULT)
C      CALL system("cp ../oa/PGBX3.DAT ../datconv/",RESULT)
C      CALL system("cp ../oa/ACPGF61.DAT ../synacsp/",RESULT)
C
C      icount = 15
C     Run SYNACSP
C      CALL ChDir("../synacsp/")
C      CALL system("./synacspscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C      icount = 16
C     Run SYNPHOT
C      CALL ChDir("../synphot/")
C      CALL system("./synphotscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
C
C      icount = 17
C     Run REPLD
C      CALL ChDir("../repld/")
C      CALL system("./repldscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C     Copy PGDX3R.DAT to PGDX3.DAT
C      CALL ChDir("../oa/")
C      CALL system("cp PGDX3R.DAT PGDX3.DAT")
C      CALL system("cp PGDX3R.DAT DG2S3.DAT")
C      CALL ChDir(CWD)
C
C      END IF
C
C     End of synthetic photometry section
C
C
      icount = 25
C     Run PGH8
      CALL ChDir("../pgh8/")
      CALL system("./pgh8" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C
      icount = 26
C     Run PGH9
      CALL ChDir("../pgh9/")
      CALL system("./pgh9" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Copy GRPE092.DAT to GRPE102.DAT,GRPE112.DAT,GRPE122.DAT,
C     GRPE132.DAT and bypass pgh10,pgh11,pgh12,and pgh13.
      CALL SYSTEM("cp ../oa/GRPE092.DAT ../oa/GRPE102.DAT", RESULT)
      CALL SYSTEM("cp ../oa/GRPE092.DAT ../oa/GRPE112.DAT", RESULT)
      CALL SYSTEM("cp ../oa/GRPE092.DAT ../oa/GRPE122.DAT", RESULT)
      CALL SYSTEM("cp ../oa/GRPE092.DAT ../oa/GRPE132.DAT", RESULT)
C
C
C      icount = 27
C     Run PGH10
C      CALL ChDir("../pgh10/")
C      CALL system("./pgh10" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C      icount = 28
C     Run PGH11
C      CALL ChDir("../pgh11/")
C      CALL system("./pgh11" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C      icount = 29
C     Run PGH12
C      CALL ChDir("../pgh12/")
C      CALL system("./pgh12" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C      icount = 30
C     Run PGH13
C      CALL ChDir("../pgh13/")
C      CALL system("./pgh13" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
      icount = 31
C     Run PGH14
      CALL ChDir("../pgh14/")
      CALL system("./pgh14" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
      OPEN (IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
C     Restore default .ILS files
C
      OPEN (IN8, FILE = 'PGC.REF', STATUS = 'OLD')
      NRF=13
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
      NRF=13
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
  103 FORMAT (1X,'End of Program GRPE')
   99 CLOSE (UNIT = IN1,   STATUS = 'KEEP')
      CLOSE (UNIT = NOUT,  STATUS = 'KEEP')
      WRITE (*,'(1X,A,I2)') 'Value of LINKV.DAT=',KEYV
      STOP 'grpe: Completed successfully'
      END PROGRAM GRPE
C
      SUBROUTINE TSOUT(RESULT, icount, NOUT)
      INTEGER(4) RESULT
      IF (RESULT.LT.0) THEN
            WRITE (NOUT, 101) icount, RESULT
  101 FORMAT (1X,'Unable to run, icount=',I4,2X,'RESULT=',I4)
            CALL EXIT(-1)
      ELSE
            WRITE (NOUT, 102) icount
  102 FORMAT (1X,'Successfully completed icount=',I4)
            RETURN
      ENDIF
      END SUBROUTINE TSOUT
