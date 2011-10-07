      PROGRAM LGTSYN3
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C*********************************************************************
C
C     Program to run the light synthesis programs and iterate for a 
C     solution
C     This program includes the procedure for synthetic photometry
C     NOTE!!!!
C     Before starting an iterative solution make sure the following
C     files exist and are initialized correctly:
C          ../oa/NOIT.DAT
C          ../oa/SUM.DAT
C          the input files for program SPTSHELL
C     Alternatively, can set ITCOUNT=0. In this case, the program will
C     run the single orbital longitude for PGB, in
C     PGB.INF, and store the output synthetic spectrum, from
C     ../oa/SYSSM.DAT in directory ../oa/SYN/ with name contained
C     in file ../LGTSYN4/LGTSYN4.NAM.
      DIMENSION FL1(14),PHS(300),VLPSI(300),NAMAR(300)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      character*80 progname,ADA
      CHARACTER * 31 TXT
      CHARACTER * 43 NAME1
      character * 36  FL1
      CHARACTER * 3 PHASE
      CHARACTER * 40 ADB
      CHARACTER * 7 LABEL
      CHARACTER * 9 AD1
      CHARACTER * 80 NAMAR
      CHARACTER * 10 DATE,TIME,ZONE
      CHARACTER * 256 CWD
      INTEGER * 2 IYR,IMON,IDAY,IHR,IMIN,ISEC,I100TH
      DIMENSION TIMEVALUES(8)
      INTEGER :: CLOCK1,CLOCK2,COUNT_RATE,COUNT_MAX,TIMEVALUES
      INTEGER(4) RESULT
      DATA IN1,IN2,IN3,IN4,IN5,IN6,IN7/1,2,3,4,5,66,7/
      DATA NOUT/8/
      DATA IN9,IN10,IN11,IN12,IN13/9,10,11,12,13/
      DATA NPSIZ/1/
      LABEL='LGTSYN3'
C     ****************************************************************
  100 FORMAT (A36)
  101 FORMAT (I2)
  102 FORMAT (A9)
  104 FORMAT (2I5)
  106 FORMAT (A80)
  107 FORMAT (5D16.13)
  108 FORMAT (5D22.13)
  109 FORMAT (A80)
  110 FORMAT (10I5)
  112 FORMAT (A64,I4)
  113 FORMAT (A3)
  114 FORMAT (A31)
  205 FORMAT (1X,'*',12X,'***PGB',/,1X,'*',8X,
     $'Information file for program PGB',/,1X,'*',8X,
     $'File name= PGB.INF',/,1X,'*',/,1X,'*')
  206 FORMAT (1X,'Control integer to decide on the form of input'
     $,9X,'KCTRL',2X,':',I3,/,1X,'*',8X,
     $'If KCTRL=0, use N1,N2,N3 to calculate phases',/,1X,'*',8X,
     $'If KCTRL=1,input individual phase values from',/,1X,'*',
     $19X,'this file.(VLPSI values)',/,1X,'*',8X,
     $'If KCTRL= 2,read N1,N2,N3 and phase values from',/,1X,'*',19X,
     $'unit 13')
  209 FORMAT (1X,'Number of orbital phase values input,if KCTRL=1'
     $,8X,'NPSI',3X,':',I3,/,1X,'*')
  211 FORMAT (1X,'Orbital inclination in degrees',25X,'VLVI',
     $3X,':',F11.7,/,1X,'*')
  212 FORMAT (1X,'Orbital eccentricity',35X,'ECC',
     $4X,':',F8.5,/,1X,'*')
  213 FORMAT (1X,'Number of orbital values between phase 0.0 and int.con
     $t.N1',4X,':',I3,/,1X,'*',6X,'(Primary minimum)')
  214 FORMAT (1X,'Number of orbital values,int.contact-ext. contact'
     $,7X,'N2',4X,':',I3,/,1X,'*',6X,'(Primary minimum)')
  215 FORMAT (1X,'Number of orbital values,ext.contact-max. elong.',
     $8X,'N3',4X,':',I3,/,1X,'*',6X,'(Primary minimum)')
  220 FORMAT (1X,'Number of orbital values betweeen mid-sec. and int.con
     $t.N4',4X,':',I3,/,1X,'*',6X,'(Secondary minimum)')
  216 FORMAT (1X,'Ordinal value of starting phase value',18X,
     $'LST',4X,':',I3,/,1X,'*',8X,'For calculated phases,full range,set
     $LST=0')
  217 FORMAT (1X,'Ordinal value of final phase value',21X,
     $'LND',4X,':',I3,/,1X,'*',8X,
     $'For calculated phases,full range,set LND=0',/,1X,'*')
  218 FORMAT (1X,'Numerical values of orbital phase,if input directly'
     $,3X,'VLPSI(1):',F11.8)
  219 FORMAT (1X,'*',53X,'VLPSI(2):',F11.8)
  221 FORMAT (1X,'Longitude of periastron (degrees)',22X,'OMD',4X,':',
     $F9.4,/,1X,'*')
  222 FORMAT (1X,'Number of orbital values,int. contact-ext. contact',
     $6X,'N5',4X,':',I3,/,1X,'*',6X,'(Secondary minimum)')
  223 FORMAT (1X,'Number of orbital values,ext.contact-max. elong.',8X,
     $'N6',4X,':'I3,/,1X,'*')
  230 FORMAT ('*            SYNACSP')
  231 FORMAT ('*         Information file for program SYNACSP')
  232 FORMAT ('*         File name=SYNACSP.INF')
  233 FORMAT ('*')
  234 FORMAT ('*')
  235 FORMAT ('Starting ordinal number of orbital longitude             
     $     :',I4)
  236 FORMAT ('*')
  237 FORMAT ('Ending ordinal number of orbital longitude               
     $     :',I4)
  238 FORMAT ('*')
  239 FORMAT ('Number of spectral interval references to input          
     $     :',I4)
  240 FORMAT ('*')
  241 FORMAT ('Text file ADB, part of storage address')
  242 FORMAT ('cp ../oa/SYSSM.DAT ../synspadd/')
  243 FORMAT ('*')
  244 FORMAT ('Text sequence, next part of storage address')
  245 FORMAT ('SMad0830.')
C     The following section contains the lines to construct file DATCONV.INF
  250 FORMAT ('*            DATCONV')
  251 FORMAT ('*         Information file for program DATCONV')
  252 FORMAT ('*         File name=DATCONV.INF')
  253 FORMAT ('*')
  254 FORMAT ('*')
  255 FORMAT ('Ordinal value of orbital longitude to select           IO
     $RD   :',I4)
  256 FORMAT ('*')
  257 FORMAT ('Key to decide on application of scaling conversion     KE
     $Y    :',I4)
  258 FORMAT ('*   If KEY=1,apply conversion; otherwise no conversion')
  259 FORMAT ('Switch for output of flux rather than intensities      IS
     $W    :',I4)
  260 FORMAT ('*   If ISW=0, ACPGF6 used intensity input, otherwise')
  261 FORMAT ('*   input was flux.')
  262 FORMAT ('If the ACPGF6 control file used flux (xxx.7) files, set')
  263 FORMAT ('KEY = 0, ISW = 1 to produce flux output in SYSSM.DAT.')
  264 FORMAT ('If the ACPGF6 control file used intensity (xxx.10) files,
     $ set')
  265 FORMAT ('KEY = 0, ISW = 0 to produce flux output in SYSSM.DAT.')
  266 FORMAT ('If the ACPGF6 control file used intensity (xxx.10) files,
     $ set')
  267 FORMAT ('KEY = 1, ISW = 1 to produce intensity output in SYSSM.DAT
     $.')
C    Get Current time for elapse time calculations later
      CALL SYSTEM_CLOCK(CLOCK1)
      CALL DATE_AND_TIME(DATE,TIME,ZONE,TIMEVALUES)
      IYR=TIMEVALUES(1)
      IMON=TIMEVALUES(2)
      IDAY=TIMEVALUES(3)
      IHR=TIMEVALUES(5)
      IMIN=TIMEVALUES(6)
C     Read control information for LGTSYN3
C
      OPEN (30, FILE = 'LGTSYN3.ILS', STATUS = 'OLD')
      READ (30, 100) (FL1(J),J=1,13)
C
      OPEN (UNIT = IN4, FILE = FL1(4), STATUS = 'OLD')       !NINCR.DAT
      OPEN (UNIT = IN5, FILE = FL1(5), STATUS = 'OLD')       !NOIT.DAT
      OPEN (UNIT = IN6, FILE = FL1(6), STATUS = 'OLD')        !LGTSYN3.INF
      OPEN (UNIT = NOUT, FILE = FL1(8), STATUS = 'OLD')  !LGTSYN3.DAT
C      OPEN (UNIT = IN9, FILE = FL1(9), STATUS = 'OLD')       !SCRATCH
      OPEN (UNIT = IN10, FILE = FL1(10), STATUS = 'OLD')       !SYNACSP.INF
      OPEN (UNIT = IN11, FILE = FL1(11), STATUS = 'OLD')       !PGBX3.DAT
      OPEN (UNIT = IN12, FILE = FL1(12), STATUS = 'OLD')       !SYNACSP.ADR
      OPEN (UNIT = IN13, FILE = FL1(13), STATUS = 'REPLACE')   !LGTSYN3TM.DAT
      NU=IN6
      CALL GTLBL(LABEL,LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
      CALL IREAD (KCTRLA)
      CALL DUMMY (2)
      CALL IREAD (MODE)
      CALL DUMMY (2)
      CALL DREAD (EBRK,'F5.3')
      CALL DUMMY(8)
      CALL IREAD (LACD)
      CALL DUMMY(1)
      CALL IREAD (KMODE)
C     Read number of orbital longitudes to process.
      READ (IN11,104) KCTRL,NPSI
C     Read the orbital longitude values
      READ (IN11,107) (PHS(K),K=1,NPSI)
      REWIND 12
C     Read vector of orbital phases if MODE.NE.0
      IF (MODE.NE.0) THEN
        OPEN (UNIT=21, FILE = '../oa/PGBX3SV.DAT', STATUS = 'OLD')
        READ (21, 110) KCTRLZ, NPSI
        READ (21,107) (PHS(K),K=1,NPSI)
        CLOSE (21, STATUS = 'KEEP')
      END IF
C     Set up accumulation file for PGD output if MODE.NE.0
      IF (MODE.NE.0) THEN
        CALL system("cp ../oa/PGDX3.DAT ../oa/PGDX3A.DAT", RESULT)
        OPEN (UNIT = 22, FILE = '../oa/PGDX3A.DAT', STATUS='OLD')
        REWIND 22
      END IF
C     Read current values in PGB.INF
      OPEN (UNIT = IN7, FILE = FL1(7), STATUS = 'OLD')
      LABEL = ' ***PGB'
      LBLERR=0
      NU=IN7
      CALL GTLBL (LABEL, LBLERR)

      IF ( LBLERR.EQ.(-1)) CALL PMDSTOP
      CALL IREAD (KCTRL)
      CALL DUMMY(5)
      CALL IREAD (NPSIX)
      CALL DUMMY (1)
      CALL DREAD (VLVI, 'F12.8')
      CALL DUMMY (1)
      CALL DREAD (ECC, 'F11.8')
      CALL DUMMY (1)
      IF (ECC.NE.0.D0) THEN
        CALL DREAD (OMD, 'F11.8')
      ELSE
        CALL DUMMY (1)
      END IF
      CALL DUMMY (1)
      CALL IREAD (N1)
      CALL DUMMY (1)
      CALL IREAD (N2)
      CALL DUMMY (1)
      CALL IREAD (N3)
      CALL DUMMY (1)
      CALL IREAD (N4)
      CALL DUMMY (1)
      CALL IREAD (N5)
      CALL DUMMY (1)
      CALL IREAD (N6)
      CALL DUMMY (1)
      CALL IREAD (LST)
      CALL DUMMY (1)
      CALL IREAD (LND)
      CALL DUMMY(2)
      IF (KCTRL.NE.1) GO TO 1
      DO I=1,1
      CALL DREAD (VLPSI(I), 'F16.13')
      END DO
C     Open communication key for exiting this control program when a
C     program bombs
C
    1 CONTINUE
      ITKEY=0
      READ (IN4, 101) ITCOUNT    !Number of aditional iterations to do
      IF (ITCOUNT.EQ.0) THEN
        ITKEY=1
        ITCOUNT=1
      END IF
      READ (IN5, 101) ICNT       !Existing cumulative iteration count
C*********************************************************************
C     Do ITCOUNT additional iterations
C*********************************************************************
      DO 10 J=1,ITCOUNT
C     ******************
C     Initialize RFMG.DAT
      CALL system("cp ../ia/RFMG5.DAT ../ia/RFMG.DAT",RESULT)
      icount=1
C     Run CALPT
      CALL ChDir("../calpt/")
      CALL system("./calptscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
      icount = 2
C     Run TRANSF1
C     Use CALPT output and current PGA.INF to produce updated PGA.IUP
      CALL ChDir("../transf1/")
      CALL system("./transf1scr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
      icount = 3
C     Run REPL4
C     Copy D:\IA2\PGA.IUP to D:\IA2\PGA.INF, preserve D:\IA2\PGA.IUP
      CALL ChDir("../repl4/")
      CALL system("./repl4scr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
      icount = 4
C     Run PGA
      CALL ChDir("../pga/")
      CALL system("./pgascr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
      KLOOP=0
C     Test MODE
    7 IF (MODE.NE.0) THEN
        KLOOP=KLOOP+1
        IF (KLOOP.GT.NPSI) GO TO 121
        OPEN (UNIT = 10, FILE='../ia/PGBSET.INF', STATUS = 'REPLACE')
        WRITE (10,205)
        WRITE (10,206) KCTRLA
        WRITE (10,209) NPSIZ
        WRITE (10,211) VLVI
        WRITE (10,212) ECC
        WRITE (10,221) OMD
        WRITE (10,213) N1
        WRITE (10,214) N2
        WRITE (10,215) N3
        WRITE (10,220) N4
        WRITE (10,222) N5
        WRITE (10,223) N6
        WRITE (10,216) LST
        WRITE (10,217) LND
        WRITE (10,218) PHS(KLOOP)
        CLOSE (10, STATUS = 'KEEP')
C       GO TO 8
C     Copy PGBSET.INF to PGB.INF
      icount=1
      CALL system("cp ../ia/PGBSET.INF ../ia/PGB.INF", RESULT)
      call tsout(RESULT, icount, NOUT)
      ELSE
        GO TO 81
      END IF
   81 icount = 5
C     Run PGB
      CALL ChDir("../pgb/")
      CALL system("./pgbscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
      icount=6
      CALL system( "cp ../oa/PGBX3.DAT ../oa/PGBX3SV.DAT" , RESULT )
      CALL system( "cp ../oa/PGBX6.DAT ../oa/PGBX6SV.DAT" , RESULT )
      call tsout(RESULT, icount, NOUT)
C
      icount=7
C     Run PLTB
      CALL ChDir("../pltb/")
      CALL system("./pltbscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
      icount=8
C     Run ACPGA
      CALL ChDir("../acpga/")
      CALL system("./acpgascr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
      icount=9
C     Run ACPGB
      CALL ChDir("../acpgb/")
      CALL system("./acpgbscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
      icount=10
C     Run ACPGC
      CALL ChDir("../acpgc/")
      CALL system("./acpgcscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
      icount = 11
C     Run PGC
      CALL ChDir("../pgc/")
      CALL system("./pgcscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
      icount=12
C     Run PGCAC
C      CALL ChDir("../pgcac/")
C      CALL system("./pgcacscr" , RESULT)
C      CALL ChDir(CWD)
C      call tsout(RESULT, icount, NOUT)
C
      icount=13
C     Copy PGCAC output to PGC output
      IF (LACD.NE.0) THEN
      CALL system( "cp ../oa/PGCAC2.DAT ../oa/PGCX2.DAT", RESULT )
      CALL system( "cp ../oa/PGCAC3.DAT ../oa/PGCX3.DAT", RESULT )
      END IF
      call tsout(RESULT, icount, NOUT)      
C
      icount = 14
C     Run PGD
      CALL ChDir("../pgd/")
      CALL system("./pgdscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
      icount=15
C     Run ACPGD
      CALL ChDir("../acpgd/")
      CALL system("./acpgdscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)      
C
      icount=16
C     Run RDVEL2
      CALL ChDir("../rdvel2/")
      CALL system("./rdvel2scr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C     Test for operation in black body mode.
      IF (KMODE.EQ.0) GO TO 11
C
      icount=17
C     Run ACPGF6
      CALL ChDir("../acpgf6/")
      CALL system("./acpgf6scr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
       icount = 18
C     Run SYNACSP
      CALL ChDir("../synacsp/")
      CALL system("./synacspscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
      icount = 19
C     Run SYNPHOT
      CALL ChDir("../synphot/")
      CALL system("./synphotscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
      icount = 20
C     Run REPLD. Prepare output with SYNPHOT data substituted
C     Input is ../oa/PGDX3.DAT
C     Output goes in file ../oa/PGDX3R.DAT
      CALL ChDir("../repld/")
      CALL system("./repldscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
      icount = 21
C     Copy ../oa/PGDX3R.DAT to ../oa/PGDX3.DAT
      CALL system( "cp ../oa/PGDX3R.DAT ../oa/PGDX3.DAT" , RESULT )
      call tsout(RESULT, icount, NOUT)
C
   11 IF (MODE.NE.0) THEN
C     Accumulate PGD output for individual orbital longitudes
        OPEN (UNIT = 13, FILE ='../oa/PGDX3.DAT',STATUS='OLD')
        DO JK=1,NWL
          READ (13,106) K1,K2,K3
          READ (13,108) ZA,ZB,ZC,ZD,ZE,ZF,ZG,ZH,ZI,ZJ,ZK,ZL,ZM,ZN,
     $    ZO,ZP,ZQ,ZR,ZS,ZT
          WRITE (12,106) K1,K2,K3

          WRITE (12,108) ZA,ZB,ZC,ZD,ZE,ZF,ZG,ZH,ZI,ZJ,ZK,ZL,ZM,ZN,
     $    ZO,ZP,ZQ,ZR,ZS,ZT
        END DO
        CLOSE (UNIT = 13, STATUS = 'DELETE')
        GO TO 7
      ELSE
        GO TO 12
      END IF
C
C     Reassemble output file from PGD in correct order
  121 CLOSE (UNIT = 12, STATUS = 'KEEP')
      OPEN (UNIT = 12, FILE = '../oa/PGDX3A.DAT', STATUS = 'OLD')
      OPEN (UNIT = 13, FILE = '../oa/PGDX3.DAT', STATUS = 'REPLACE')
      DO KWL=0,NWL-1
        DO LCT=1,NPSI
          DO JWL=1,KWL
            READ (12,106) K1,K2,K3
            READ (12,108) ZA,ZB,ZC,ZD,ZE,ZF,ZG,ZH,ZI,ZJ,ZK,ZL,ZM,
     $      ZN,ZO,ZP,ZQ,ZR,ZS,ZT
          END DO
          READ (12,106) K1,K2,K3
          READ (12,108) ZA,ZB,ZC,ZD,ZE,ZF,ZG,ZH,ZI,ZJ,ZK,ZL,ZM,ZN,
     $    ZO,ZP,ZQ,ZR,ZS,ZT
          WRITE (13,106) K1,K2,K3
          WRITE (13,108) ZA,ZB,ZC,ZD,ZE,ZF,ZG,ZH,ZI,ZJ,ZK,ZL,ZM,ZN,
     $    ZO,ZP,ZQ,ZR,ZS,ZT
          DO JWL=KWL+1,NWL-1
            READ (12,106) K1,K2,K3
            READ (12,108) ZA,ZB,ZC,ZD,ZE,ZF,ZG,ZH,ZI,ZJ,ZK,ZL,ZM,
     $      ZN,ZO,ZP,ZQ,ZR,ZS,ZT
          END DO
        END DO
      REWIND 12
      END DO
      CLOSE (UNIT = 12, STATUS = 'KEEP')
      CLOSE (UNIT = 13, STATUS = 'KEEP')
   12 CONTINUE
C
C     Copy ../oa/PGBX6SV.DAT ../oa/PGBX6.DAT
C     Copy ../oa/PGBX3SV.DAT ../oa/PGBX3.DAT
C      IF (MODE.NE.0) THEN
C        CALL system( "cp ../oa/PGBX6SV.DAT ../oa/PGBX6.DAT", RESULT )
C        CALL system( "cp ../oa/PGBX3SV.DAT ../oa/PGBX3.DAT", RESULT )
C        call tsout(RESULT, icount, NOUT)
C      END IF
C
      icount = 22
C     Run SPT
      CALL ChDir("../spt/")
      CALL system("./sptscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C
      icount = 23
C     Run SET00
C     Update value of E:\IA2\RFMGA.DAT with
C     output of SPT and store in E:\IA2\RFMGA.DAT
      CALL ChDir("../set00/")
      CALL system("./set00scr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)   
C
      icount = 24
C     Copy file E:\IA2\RFMGA.DAT to E:\IA2\RFMG.DAT, delete RFMGA.DAT
C     RUN REPL5
      CALL ChDir("../repl5/")
      CALL system("./repl5scr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)   
C      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
      icount = 25
C     Run PGG. Produce file of light derivatives w/resp to geometric
C     parameters at fiducial orbital longitudes.
      CALL ChDir("../pgg/")
      CALL system("./pggscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
      icount = 26
C     Run PEDCS. Produce light derivatives at the orbital
C     phases of observation
      CALL ChDir("../pedcs/")
      CALL system("./pedcsscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
      icount = 27
C     Produce plot files of light derivatives via DVFLCR
      CALL ChDir("../dvflcr/")
      CALL system("./dvflcrscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
      icount = 28
C     Run program DIFCORR
      CALL ChDir("../difcorr/")
      CALL system("./difcorrscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
      icount = 29
C     Input E:\IA2\RFMG.DAT, update with data from DIFCORR, and 
C     write to E:\IA2\RFMGA.DAT
      CALL ChDir("../set0/")
      CALL system("./set0scr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
      icount = 30
C     Copy file E:\IA2\RFMGA.DAT to E:\RFMG.DAT, delete RFMGA.DAT
      CALL ChDir("../repl5/")
      CALL system("./repl5scr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
      icount = 31
C     Copy file ../oa/NOITA.DAT to new file ../oa/NOIT.DAT and
C     delete file ../oa/NOITA.DAT
      CALL ChDir("../noitct/")
      CALL system("./noitct" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
      icount = 32
C     Run program INCREM. Produce files for incrementing the 
C     system parameters by the calculated corrections.
      CALL ChDir("../increm/")
      CALL system("./incremscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
      icount = 33
C     Combine existing E:\IA2\CALPT.INF file with update information
C     to produce file E:\IA2\CLUP.INF
      CALL ChDir("../set1/")
      CALL system("./set1scr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
      icount = 34
C     Copy data from ../oa/CLUP.INF to ../oa/CALPT.INF, and
C     delete E:\IA2\CLUP.INF
      CALL ChDir("../repl1/")
      CALL system("./repl1scr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
      icount = 35
C     Combine file E:\IA2\PGB.INF with update data and produce new
C     file E:\IA2\PGBUP.INF
      CALL ChDir("../set3/")
      CALL system("./set3scr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
      icount = 36
C     Copy file E:\IA2\PGBUP.INF to E:\IA2\PGB.INF, 
C     delete E:\IA2\PGBUP.INF
      CALL ChDir("../repl2/")
      CALL system("./repl2scr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
      icount = 37
C     Combine data from file E:\IA2\PGC.INF with updata data and
C     produce new file E:\IA2\PGCUP.INF
      CALL ChDir("../set4/")
      CALL system("./set4scr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
      icount = 38
C     Copy data from file E:\IA2\PGCUP.INF to new file E:\IA2\PGC.INF
C     Delete E:\IA2\PGCUP.INF
      CALL ChDir("../repl3/")
      CALL system("./repl3scr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
      icount = 39
C     Run program SUMMARY
C     Update log of system parameters as function of iteration number
      CALL ChDir("../summary/")
      CALL system("./summaryscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
      icount = 40
C     Run program TRANSF3
C     Transfer output of SUMMARY to new input files
      CALL ChDir("../transf3/")
      CALL system("./transf3scr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
      ICNT = ICNT+1
   10 CONTINUE
      icount=41
C     Run LTCVA
      CALL ChDir("../ltcva/")
      CALL system("./ltcvascr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
      icount=42
C     Run LTRES
      CALL ChDir("../ltres/")
      CALL system("./ltresscr" , RESULT)
      CALL ChDir(CWD)
      call tsout(RESULT, icount, NOUT)
C      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
C      READ (IN1, 101) KEYV
C      IF (KEYV.NE.0) GO TO 99
C      CLOSE (UNIT = IN1, STATUS = 'KEEP')

C
C     Get current time, calcuate running time
      CALL SYSTEM_CLOCK(CLOCK2,COUNT_RATE,COUNT_MAX)
      AAA=CLOCK1
      BBB=CLOCK2
      CCC=COUNT_RATE
      TM=(BBB-AAA)/CCC
  105 WRITE (NOUT, 103) ICNT
  103 FORMAT (1X, 'End of Program LGTSYN3',/,1X, 
     $'Cumulative iteration number = ',I4)
      WRITE (IN13, 204) TM
  204 FORMAT (1X, 'Running time =',F12.4,2X,'seconds')
      CLOSE (UNIT = IN1,  STATUS = 'KEEP')
      CLOSE (UNIT = IN5,  STATUS = 'KEEP')
      CLOSE (UNIT = IN6,  STATUS = 'KEEP')
      CLOSE (UNIT = NOUT, STATUS = 'KEEP')
      CLOSE (UNIT = 8,    STATUS = 'KEEP')
      WRITE (*,'(1X,A,I2)') 'Value of LINKV.DAT=',KEYV
      STOP 'LGTSYN3 Completed successfully'
   99 CALL PMDSTOP
      END
C
C
      SUBROUTINE TSOUT( RESULT, icount, NOUT)
      integer(4) RESULT
      if (RESULT .LT. 0) then
       write (NOUT, 101) icount, RESULT
  101 FORMAT (1X, 'Unable to run, icount=',I4,2X,'RESULT=',I4)
       stop
      else
       write (NOUT, 102) icount
  102 FORMAT (1X,'Successfully completed icount=',I4)
      return
      endif
      end
