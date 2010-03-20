      PROGRAM LGTSYN2
	USE MSFLIB
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C*********************************************************************
C
C     Program to run the light synthesis programs and iterate for a 
C     solution
C     NOTE!!!!
C     Before starting an iterative solution make sure the following
C     files exist and are initialized correctly:
C          E:\OA2\NOIT.DAT
C          E:\OA2\SUM.DAT
C          the input files for program SPT
C		 Be sure PGB has been run separately to set the output file
C		 E:\OA2\PGBX3.DAT containing the full set of orbital
C		 longitudes for the LGTSYN2 run, and that the file
C		 E:\OA2\PGBX3.DAT has been copied to E:\OA2\PGBX3SV.DAT.
      DIMENSION FL1(9)
	DIMENSION PHS(300),VLPSI(300)
	COMMON /NUNIT/ NU, NUOUT, IFOUT
	CHARACTER * 7 LABEL
      character*80 progname
      character * 24  FL1
      integer*2 IMIN,ISEC,I100TH,IHR,IDAY,IMON,IYR
	INTEGER(4) RESULT
	DATA NPSIZ/1/
      DATA IN1,IN2,IN3,IN5,IN6/1,2,3,5,6/
      DATA NOUT/7/
C     ****************************************************************
      RESULT=SETEXITQQ(QWIN$EXITNOPERSIST)
      OPEN (10, FILE = 'C:\LGTSYN\LGTSYN2\LGTSYN2.ILS', STATUS = 'OLD')
      READ ( 10, 100) (FL1(J),J=1,9)
  100 FORMAT (A24)
  101 FORMAT (I2)
  106 FORMAT (10I5)
  107 FORMAT (5D16.13)
  108 FORMAT (5D22.13)
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
      CALL GETDAT(IYR,IMON,IDAY)
      CALL GETTIM(IHR,IMIN,ISEC,I100TH)
      TM=3600*IHR+60*IMIN+ISEC
      AD=I100TH
      TM=(100.D0*TM+AD)/100.D0
      AD =IDAY
      DA=AD+TM/86400.D0
      CALL CVJD(DA,IMON,IYR,F1)
C
C     Read control information for LGTSYN2
C
      OPEN (UNIT = IN2, FILE = FL1(7), STATUS = 'OLD')
	LABEL = 'LGTSYN2'
	NU=IN2
	CALL GTLBL(LABEL,LBLERR)
	IF (LBLERR.EQ.(-1)) CALL PMDSTOP
	CALL IREAD (KCTRLA)
	CALL DUMMY (2)
	CALL IREAD (MODE)
	CALL DUMMY (2)
	CALL DREAD (EBRK,'F5.3')
C     Read vector of orbital phases if MODE.NE.0
      IF (MODE.NE.0) THEN
		OPEN (UNIT=11, FILE = 'C:\OA2\PGBX3SV.DAT', STATUS = 'OLD')
		READ (11, 106) KCTRLZ, NPSI
		READ (11,107) (PHS(K),K=1,NPSI)
		CLOSE (11, STATUS = 'KEEP')
	END IF
C     Set up accumulation file for PGD output if MODE.NE.0
      IF (MODE.NE.0) THEN
		progname = 'copy C:\OA2\PGDX3.DAT C:\OA2\PGDX3A.DAT'
		RESULT = SYSTEMQQ(progname)
		OPEN (UNIT = 12, FILE = 'C:\OA2\PGDX3A.DAT', STATUS='OLD')
		REWIND 12
	END IF
C     Read number of wavelengths in output files
      OPEN (UNIT = 15, FILE = 'C:\IA2\PGC.INF', STATUS = 'OLD')
	LABEL = ' ***PGC'
	LBLERR=0
	NU=15
	CALL GTLBL (LABEL,LBLERR)
	IF (LBLERR.EQ.(-1)) CALL PMDSTOP
	CALL IREAD (NWL)
	CLOSE (15, STATUS = 'KEEP')
C     Read current values in PGB.INF
      OPEN (UNIT = IN3, FILE = FL1(8), STATUS = 'OLD')
      LABEL = ' ***PGB'
      LBLERR=0
      NU=IN3
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
    1 OPEN (UNIT = NOUT, FILE = FL1(9), STATUS = 'REPLACE')  !LGTSYN2.DAT
    4 OPEN (UNIT = IN5, FILE = FL1(5), STATUS = 'OLD')       !NINCR.DAT
      OPEN (UNIT = IN6, FILE = FL1(6), STATUS = 'OLD')       !NOIT.DAT
      READ (IN5, 101) ITCOUNT    !Number of aditional iterations to do
      READ (IN6, 101) ICNT       !Existing cumulative iteration count
C*********************************************************************
C     Do ITCOUNT additional iterations
C*********************************************************************
      DO 10 J=1,ITCOUNT
      icount = 1
	OPEN (UNIT = 12, FILE = 'C:\OA2\PGDX3A.DAT', 
     $		STATUS = 'REPLACE')
C     Run CALPT
      progname = 'C:\LGTSYN\CALPT\DEBUG\CALPT.EXE' 
      RESULT=SYSTEMQQ(PROGNAME)
      call tsout(RESULT, icount, NOUT)
      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')

      icount = 2
C     Use CALPT output and current PGA.INF to produce updated PGA.IUP
      progname = 'C:\LGTSYN\TRNSF1\DEBUG\TRNSF1.EXE' 
      RESULT=SYSTEMQQ(PROGNAME)
      call tsout(RESULT, icount, NOUT)
      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')


      icount = 3
C     Copy D:\IA2\PGA.IUP to D:\IA2\PGA.INF, preserve D:\IA2\PGA.IUP
      progname = 'C:\LGTSYN\REPL4\DEBUG\REPL4.EXE' 
      RESULT=SYSTEMQQ(PROGNAME)
      call tsout(RESULT, icount, NOUT)
      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')




      icount = 4
C     Run PGA
      progname = 'C:\LGTSYN\PGA\DEBUG\PGA.EXE' 
      RESULT=SYSTEMQQ(PROGNAME)
      call tsout(RESULT, icount, NOUT)
      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')

	KLOOP=0
C     Test MODE
    7 IF (MODE.NE.0) THEN
		KLOOP=KLOOP+1
		IF (KLOOP.GT.NPSI) GO TO 121
		OPEN (UNIT = 10, FILE='C:\IA2\PGBSET.INF', STATUS = 'REPLACE')
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
		GO TO 8
	ELSE
		GO TO 81
      END IF
C     Copy PGBSET.INF to PGB.INF
    8 progname = 'copy C:\IA2\PGBSET.INF C:\IA2\PGB.INF'
      RESULT = SYSTEMQQ(progname) 

   81 icount = 5
C     Run PGB
      progname = 'C:\LGTSYN\PGB\DEBUG\PGB.EXE' 
      RESULT=SYSTEMQQ(PROGNAME)
      call tsout(RESULT, icount, NOUT)
      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')


      icount = 6
C     Run PGC
      progname = 'C:\LGTSYN\PGC\DEBUG\PGC.EXE'
      RESULT=SYSTEMQQ(PROGNAME)
      call tsout(RESULT, icount, NOUT)	 
      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')


      icount = 7
C     Run PGD
      progname = 'C:\LGTSYN\PGD\DEBUG\PGD.EXE'
      RESULT=SYSTEMQQ(PROGNAME)
      call tsout(RESULT, icount, NOUT)	 
      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
   11	IF (MODE.NE.0) THEN
C     Accumulate PGD output for individual orbital longitudes
		OPEN (UNIT = 13, FILE ='C:\OA2\PGDX3.DAT',STATUS='OLD')
      DO JK=1,NWL
		READ (13,106) K1,K2,K3
		READ (13,108) ZA,ZB,ZC,ZD,ZE,ZF,ZG,ZH,ZI,ZJ,ZK,ZL,ZM,ZN,
     $	ZO,ZP,ZQ,ZR,ZS,ZT
		WRITE (12,106) K1,K2,K3
		WRITE (12,108) ZA,ZB,ZC,ZD,ZE,ZF,ZG,ZH,ZI,ZJ,ZK,ZL,ZM,ZN,
     $	ZO,ZP,ZQ,ZR,ZS,ZT
	END DO
     		CLOSE (UNIT = 13, STATUS = 'DELETE')
		GO TO 7
	ELSE
		GO TO 12
	END IF
C
C     Reassemble output file from PGD in correct order
  121 CLOSE (UNIT = 12, STATUS = 'KEEP')
      OPEN (UNIT = 12, FILE = 'C:\OA2\PGDX3A.DAT', STATUS = 'OLD')
	OPEN (UNIT = 13, FILE = 'C:\OA2\PGDX3.DAT', STATUS = 'REPLACE')
	DO KWL=0,NWL-1
		DO LCT=1,NPSI
			DO JWL=1,KWL
				READ (12,106) K1,K2,K3
				READ (12,108) ZA,ZB,ZC,ZD,ZE,ZF,ZG,ZH,ZI,ZJ,ZK,ZL,ZM,
     $			ZN,ZO,ZP,ZQ,ZR,ZS,ZT
			END DO
			READ (12,106) K1,K2,K3
			READ (12,108) ZA,ZB,ZC,ZD,ZE,ZF,ZG,ZH,ZI,ZJ,ZK,ZL,ZM,ZN,
     $		ZO,ZP,ZQ,ZR,ZS,ZT
			WRITE (13,106) K1,K2,K3
			WRITE (13,108) ZA,ZB,ZC,ZD,ZE,ZF,ZG,ZH,ZI,ZJ,ZK,ZL,ZM,ZN,
     $		ZO,ZP,ZQ,ZR,ZS,ZT
			DO JWL=KWL+1,NWL-1
				READ (12,106) K1,K2,K3
				READ (12,108) ZA,ZB,ZC,ZD,ZE,ZF,ZG,ZH,ZI,ZJ,ZK,ZL,ZM,
     $			ZN,ZO,ZP,ZQ,ZR,ZS,ZT
			END DO
		END DO
		REWIND 12
	END DO
	CLOSE (UNIT = 12, STATUS = 'KEEP')
	CLOSE (UNIT = 13, STATUS = 'KEEP')
C     Prepare to run program SPT
   12 icount = 8
      IF (MODE.NE.0) THEN
		progname='copy C:\OA2\PGBX6SV.DAT C:\OA2\PGBX6.DAT'
		RESULT = SYSTEMQQ(progname)
		progname='copy C:\OA2\PGBX3SV.DAT C:\OA2\PGBX3.DAT'
		RESULT = SYSTEMQQ(progname)
	END IF
C     Run SPT
      progname = 'C:\LGTSYN\SPT\DEBUG\SPT.EXE' 
      RESULT=SYSTEMQQ(PROGNAME)
      call tsout(RESULT, icount, NOUT)
      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')

      icount = 9
C     Update value of E:\IA2\RFMGA.DAT with
C     output of SPT and store in E:\IA2\RFMGA.DAT
      progname = 'C:\LGTSYN\SET00\DEBUG\SET00.EXE'
      RESULT=SYSTEMQQ(PROGNAME)
      call tsout(RESULT, icount, NOUT)	 
      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')

      icount = 10
C     Copy file E:\IA2\RFMGA.DAT to E:\RFMG.DAT, delete RFMGA.DAT
      progname = 'C:\LGTSYN\REPL5\DEBUG\REPL5.EXE'
      RESULT=SYSTEMQQ(PROGNAME)
      call tsout(RESULT, icount, NOUT)	 
      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')


      icount = 11
C     Run PGG. Produce file of light derivatives w/resp to geometric
C     parameters at fiducial orbital longitudes.
      progname = 'C:\LGTSYN\PGG\DEBUG\PGG.EXE' 
      RESULT=SYSTEMQQ(PROGNAME)
      call tsout(RESULT, icount, NOUT)
      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')


      icount = 12
C     Run PEDCS. Produce light derivatives at the orbital
C     phases of observation
      progname = 'C:\LGTSYN\PEDCS\DEBUG\PEDCS.EXE' 
      RESULT=SYSTEMQQ(PROGNAME)
      call tsout(RESULT, icount, NOUT)
      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')

      icount = 13
C     Produce plot files of light derivatives via DVFLCR
      progname = 'C:\LGTSYN\DVFLCR\DEBUG\DVFLCR.EXE' 
      RESULT=SYSTEMQQ(PROGNAME)
      call tsout(RESULT, icount, NOUT)
      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')


      icount = 14
C     Run program DIFCORR
      progname = 'C:\LGTSYN\DIFCORR\DEBUG\DIFCORR.EXE' 
      RESULT=SYSTEMQQ(PROGNAME)
      call tsout(RESULT, icount, NOUT)
      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')

      icount = 15
C     Input E:\IA2\RFMG.DAT, update with data from DIFCORR, and 
C     write to E:\IA2\RFMGA.DAT
      progname = 'C:\LGTSYN\SET0\DEBUG\SET0.EXE' 
      RESULT=SYSTEMQQ(PROGNAME)
      call tsout(RESULT, icount, NOUT)
      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')

      icount = 16
C     Copy file E:\IA2\RFMGA.DAT to E:\RFMG.DAT, delete RFMGA.DAT
      progname = 'C:\LGTSYN\REPL5\DEBUG\REPL5.EXE' 
      RESULT=SYSTEMQQ(PROGNAME)
      call tsout(RESULT, icount, NOUT)
      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')

      icount = 17
C     Copy file E:\OA2\NOITA.DAT to new file E:\OA2\NOIT.DAT and
C     delete file E:\OA2\NOITA.DAT
      progname = 'C:\LGTSYN\NOITCT\DEBUG\NOITCT.EXE' 
      RESULT=SYSTEMQQ(PROGNAME)
      call tsout(RESULT, icount, NOUT)
      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')

      icount = 18
C     Run program INCREM. Produce files for incrementing the 
C     system parameters by the calculated corrections.
      progname = 'C:\LGTSYN\INCREM\DEBUG\INCREM.EXE' 
      RESULT=SYSTEMQQ(PROGNAME)
      call tsout(RESULT, icount, NOUT)
      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')


      icount = 19
C     Combine existing E:\IA2\CALPT.INF file with update information
C     to produce file E:\IA2\CLUP.INF
      progname = 'C:\LGTSYN\SET1\DEBUG\SET1.EXE' 
      RESULT=SYSTEMQQ(PROGNAME)
      call tsout(RESULT, icount, NOUT)
      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')



      icount = 20
C     Copy data from E:\OA2\CLUP.INF to E:\OA2\CALPT.INF, and
C     delete E:\IA2\CLUP.INF
      progname = 'C:\LGTSYN\REPL1\DEBUG\REPL1.EXE' 
      RESULT=SYSTEMQQ(PROGNAME)
      call tsout(RESULT, icount, NOUT)
      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')


      icount = 21
C     Combine file E:\IA2\PGB.INF with update data and produce new
C     file E:\IA2\PGBUP.INF
      progname = 'C:\LGTSYN\SET3\DEBUG\SET3.EXE' 
      RESULT=SYSTEMQQ(PROGNAME)
      call tsout(RESULT, icount, NOUT)
      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')


      icount = 22
C     Copy file E:\IA2\PGBUP.INF to E:\IA2\PGB.INF, 
C     delete E:\IA2\PGBUP.INF
      progname = 'C:\LGTSYN\REPL2\DEBUG\REPL2.EXE' 
      RESULT=SYSTEMQQ(PROGNAME)
      call tsout(RESULT, icount, NOUT)
      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')


      icount = 23
C     Combine data from file E:\IA2\PGC.INF with updata data and
C     produce new file E:\IA2\PGCUP.INF
      progname = 'C:\LGTSYN\SET4\DEBUG\SET4.EXE' 
      RESULT=SYSTEMQQ(PROGNAME)
      call tsout(RESULT, icount, NOUT)
      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')


      icount = 24
C     Copy data from file E:\IA2\PGCUP.INF to new file E:\IA2\PGC.INF
C     Delete E:\IA2\PGCUP.INF
      progname = 'C:\LGTSYN\REPL3\DEBUG\REPL3.EXE' 
      RESULT=SYSTEMQQ(PROGNAME)
      call tsout(RESULT, icount, NOUT)
      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')


      icount = 25
C     Run program SUMMARY
C     Update log of system parameters as function of iteration number
      progname = 'C:\LGTSYN\SUMMARY\DEBUG\SUMMARY.EXE' 
      RESULT=SYSTEMQQ(PROGNAME)
      call tsout(RESULT, icount, NOUT)
      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')


      icount = 26
C     Run program TRANSF3
C     Transfer output of SUMMARY to new input files
      progname = 'C:\LGTSYN\TRANSF3\DEBUG\TRANSF3.EXE' 
      RESULT=SYSTEMQQ(PROGNAME)
      call tsout(RESULT, icount, NOUT)
      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
      ICNT = ICNT+1
   10 CONTINUE
C     Run LTCVA
      progname = 'C:\LGTSYN\LTCVA\DEBUG\LTCVA.EXE' 
      RESULT = SYSTEMQQ(progname)
      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C     Run LTRES
      progname = 'C:\LGTSYN\LTRES\DEBUG\LTRES.EXE'
	RESULT=SYSTEMQQ(progname)
      OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
      READ (IN1, 101) KEYV
      IF (KEYV.NE.0) GO TO 99
      CLOSE (UNIT = IN1, STATUS = 'KEEP')
C
      CALL GETDAT(IYR,IMON,IDAY)
      CALL GETTIM(IHR,IMIN,ISEC,I100TH)
      TM=3600*IHR+60*IMIN+ISEC
      AD=I100TH
      TM=(100.D0*TM+AD)/100.D0
      AD=IDAY
      DA=AD+TM/86400.D0
      CALL CVJD(DA,IMON,IYR,F2)
      TM=(F2-F1)*1440.D0
  105 WRITE (NOUT, 103) ICNT
  103 FORMAT (1X, 'End of Program LGTSYN2',/,1X, 
     $'Cumulative iteration number = ',I4)
      WRITE (NOUT, 204) TM
  204 FORMAT (1X, 'Running time =',F12.4,2X,'minutes')
   99 CLOSE (UNIT = IN1,  STATUS = 'KEEP')
      CLOSE (UNIT = IN5,  STATUS = 'KEEP')
      CLOSE (UNIT = IN6,  STATUS = 'KEEP')
      CLOSE (UNIT = NOUT, STATUS = 'KEEP')
      CLOSE (UNIT = 8,    STATUS = 'KEEP')
      WRITE (*,'(1X,A,I2)') 'Value of LINKV.DAT=',KEYV
      STOP
      END


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