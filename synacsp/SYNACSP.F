      PROGRAM SYNACSP
C     Program to produce a series of synthetic spectra of an accretion 
C     disk system and store the spectra.
C     The program calculates spectra for the full set of spectral 
C     intervals corresponding to the standard Hubeny line lists.
C     NOTE!!! PGB must have been run prior to running this program
C     and the output file PGBX3.DAT copied to PGBX3SV.DAT. Be sure
C     the full correct set of orbital longitudes is present in the
C     file PGBX3SV.DAT.
C     Note that this program runs the full set of programs, starting
C     with CALPT.
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	DIMENSION FL1(6)
	DIMENSION PHS(200),SPTANG(200)
	COMMON /NUNIT/ NU, NUOUT, IFOUT
	character * 80 progname,NAME1,ADA
	CHARACTER * 36 TXT
	CHARACTER * 36 FL1
	CHARACTER * 3 PHASE
	CHARACTER * 45 ADB
	CHARACTER * 7 LABEL
	CHARACTER * 9 AD1,AD2,AD3,AD4,AD5,AD6
      CHARACTER * 10 DATE,TIME,ZONE
      CHARACTER * 256 CWD
	INTEGER(4) RESULT
	INTEGER*2 IMIN,ISEC,I100TH,IHR,IDAY,IMON,IYR
      DIMENSION TIMEVALUES(8)
      INTEGER :: CLOCK1,CLOCK2,COUNT_RATE,COUNT_MAX,TIMEVALUES
	INTEGER * 2 KEYGOOD,KEYBAD,KEYERR
	DATA IN1,IN2,IN3,IN4,IN5,IN6/2,3,4,5,66,7/
	DATA KCTRLX,NPSIX,VLVI,ECC,OMD/1,1,86.D0,0.D0,90.D0/
	DATA N1,N2,N3,N4,N5,N6,LST,LND/4,8,8,4,8,8,0,0/
	DATA KEYGOOD/0/
	DATA KEYBAD/-1/
C
C     ***************************************************
C     Store the current working directory (CWD).  NOTE: relative
C     filenames may be re-opend during execution, so this is critical.
      CALL GetCWD(CWD, RESULT)
      CALL SYSTEM_CLOCK(CLOCK1)
      CALL DATE_AND_TIME(DATE,TIME,ZONE,TIMEVALUES)
      IYR=TIMEVALUES(1)
      IMON=TIMEVALUES(2)
      IDAY=TIMEVALUES(3)
      IHR=TIMEVALUES(5)
      IMIN=TIMEVALUES(6)
      OPEN (7, FILE = 'SYNACSP.ILS', STATUS = 'OLD')
	READ (7, 100) (FL1(J),J=1,6)
	OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
	OPEN (UNIT = IN2, FILE = FL1(2), STATUS = 'OLD')
	OPEN (UNIT = IN3, FILE = FL1(3), STATUS = 'OLD')
	OPEN (UNIT = IN4, FILE = FL1(4), STATUS = 'OLD')
	OPEN (UNIT = IN5, FILE = FL1(5), STATUS = 'OLD')
C	OPEN (UNIT = IN6, FILE = FL1(6), STATUS = 'OLD')
C     ******************************************************     
  100 FORMAT (A36)
  101 FORMAT (A36)
  102 FORMAT (A9)
  103 FORMAT (5D16.13)
  105 FORMAT (A80)
  106 FORMAT (10I5)
  107 FORMAT (A3)
  108 FORMAT (A14)
  109 FORMAT (A8)
  110 FORMAT (I2)
  111 FORMAT (10F8.4)
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
  224 FORMAT('Angular radii of spots, in degrees.First component.',/,
     $'* There are as many sets of radii as nimber of observation wavele
     $ngths')
  225 FORMAT (F7.1,2X,F7.1)
  303 FORMAT (1X,'Program running time=',F14.4,2X,'minutes')
C     ********************************************************
      WRITE (IN1,110) KEYGOOD
	CLOSE (UNIT = IN1, STATUS = 'KEEP') 
      LBLERR=0
	LABEL = 'SYNACSP'
	NU=IN2
	CALL GTLBL(LABEL,LBLERR)
	IF (LBLERR.EQ.(-1)) CALL PMDSTOP
	CALL IREAD (NSTART)
	CALL DUMMY (1)
	CALL IREAD (NEND)
	CALL DUMMY (1)
	CALL IREAD (NINT)
	CALL DUMMY(2)
	READ (NU,101) TXT
	CALL DUMMY(2)
	READ (NU,102) AD1
	IF (NINT.EQ.1) GO TO 1
	READ (NU,102) AD2
	IF (NINT.EQ.2) GO TO 1
	READ (NU,102) AD3
	IF (NINT.EQ.3) GO TO 1
	READ (NU,102) AD4
	IF (NINT.EQ.4) GO TO 1
	READ (NU,102) AD5
	IF (NINT.EQ.5) GO TO 1
	READ (NU,102) AD6
    1 CONTINUE
C	                    
C     Read number of orbital longitudes to process.
      READ (IN3,106) KCTRL,NPSI
C     Read the orbital longitude values
      READ (IN3,103) (PHS(K),K=1,NPSI)
	READ (IN3,103) VLVI
C     Convert to degrees
      VLVI=VLVI*57.2957795131D0
C     NOTE!!! PGB must have been run with the parameters KCTRL=0,
C     NPSI=300 prior to starting this program, if a full set of
C     orbital longitudes will be calculated.
C     Loop through all orbital longitudes
C     Read values of spot angular radii. Must have same number as
C     number of orbital longitudes.
C      READ (IN6,111) (SPTANG(K),K=1,NPSI)
	IF (NEND.EQ.0) NSTOP=NPSI
	IF (NEND.NE.0) NSTOP=NEND
      DO 3 KLOOP=1,NSTOP
C     Read orbital phase value for setting output storage.
	READ (IN5,107) PHASE
	IF (KLOOP.LT.NSTART) GO TO 3
C     Prepare control file to run PGB for single value of
C     orbital longitude.
      OPEN (UNIT = 10, FILE='../ia/PGBSET.INF', 
     $STATUS='REPLACE')
	WRITE (10,205)
	WRITE (10,206) KCTRLX
	WRITE (10,209) NPSIX
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
C
C     Prepare control file to set radius of spot(s) for use by PGC.
C
C      OPEN (UNIT = 10, FILE='D:\IA2\PGCSET.INF',
C     $STATUS='REPLACE')
C	WRITE (10,224)
C	WRITE (10,225) SPTANG(KLOOP),SPTANG(KLOOP)
C	CLOSE (10, STATUS = 'KEEP')
C     Set the PGB control file for the current value of KLOOP
      CALL system("cp ../ia/PGBSET.INF ../ia/PGB.INF")
C     Set the PGC control file for the current value of KLOOP
C      progname = 'copy D:\IA2\PGCSET.INF D:\IA2\VGMDA.DAT'
C	RESULT = SYSTEMQQ(Progname)	
C
C     Run the full sequence of programs, beginning with CALPT
C
C     Run program CALPT
C      progname = 'C:\LGTSYN\CALPT\DEBUG\CALPT.EXE'
C	RESULT = SYSTEMQQ(Progname)
C	OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
C	READ (IN1,110) KEYERR
C	CLOSE (UNIT = IN1, STATUS = 'KEEP')
C	IF (KEYERR.EQ.KEYBAD) GO TO 99
C     Run program TRNSF1
C      progname = 'C:\LGTSYN\TRNSF1\DEBUG\TRNSF1.EXE'
C	RESULT = SYSTEMQQ(Progname)
C	OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
C	READ (IN1,110) KEYERR
C	CLOSE (UNIT = IN1, STATUS = 'KEEP')
C	IF (KEYERR.EQ.KEYBAD) GO TO 99
C     Run program REPL4
C      progname = 'C:\LGTSYN\REPL4\DEBUG\REPL4.EXE'
C	RESULT = SYSTEMQQ(Progname)
C	OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
C	READ (IN1,110) KEYERR
C	CLOSE (UNIT = IN1, STATUS = 'KEEP')
C	IF (KEYERR.EQ.KEYBAD) GO TO 99
C     Run program PGA
C      progname = 'C:\LGTSYN\PGA\DEBUG\PGA.EXE'
C	RESULT = SYSTEMQQ(Progname)
C	OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
C	READ (IN1,110) KEYERR
C	CLOSE (UNIT = IN1, STATUS = 'KEEP')
C	IF (KEYERR.EQ.KEYBAD) GO TO 99
C     Run program PGB
      CALL ChDir("../pgb/")
      CALL system("./pgbscr" , RESULT)
      CALL ChDir(CWD)
		OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1,110) KEYERR
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
	IF (KEYERR.EQ.KEYBAD) GO TO 99
C     Run program PLTB
      CALL ChDir("../pltb/")
      CALL system("./pltbscr" , RESULT)
      CALL ChDir(CWD)
	OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1,110) KEYERR
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
	IF (KEYERR.EQ.KEYBAD) GO TO 99
C     Run program ACPGA
C      progname = 'C:\LGTSYN\ACPGA\DEBUG\ACPGA.EXE'
C	RESULT = SYSTEMQQ(Progname)
C	OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
C	READ (IN1,110) KEYERR
C	CLOSE (UNIT = IN1, STATUS = 'KEEP')
C	IF (KEYERR.EQ.KEYBAD) GO TO 99
C     Run program ACPGB
      CALL ChDir("../acpgb/")
      CALL system("./acpgbscr" , RESULT)
      CALL ChDir(CWD)
	OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1,110) KEYERR
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
	IF (KEYERR.EQ.KEYBAD) GO TO 99
C     Run program ACPGC
C      progname = 'C:\LGTSYN\ACPGC\DEBUG\ACPGC.EXE'
C	RESULT = SYSTEMQQ(Progname)
C	OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
C	READ (IN1,110) KEYERR
C	CLOSE (UNIT = IN1, STATUS = 'KEEP')
C	IF (KEYERR.EQ.KEYBAD) GO TO 99
C     Run program PGC
C      progname = 'C:\LGTSYN\PGC\DEBUG\PGC.EXE'
C	RESULT = SYSTEMQQ(Progname)
C	OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
C	READ (IN1,110) KEYERR
C	CLOSE (UNIT = IN1, STATUS = 'KEEP')
C	IF (KEYERR.EQ.KEYBAD) GO TO 99
C     Run program PGCAC
C      progname = 'C:\LGTSYN\PGCAC\DEBUG\PGCAC.EXE'
C	RESULT = SYSTEMQQ(Progname)
C	OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
C	READ (IN1,110) KEYERR
C	CLOSE (UNIT = IN1, STATUS = 'KEEP')
C	IF (KEYERR.EQ.KEYBAD) GO TO 99
C
C     Copy D:\OA2\PGCAC2.DAT to D:\OA2\PGCX2.DAT
C	OPEN (UNIT=11,FILE='D:\IA2\ACPGA.INF',STATUS='OLD')
C	CALL DUMMY (11)
C	CALL IREAD (NRING)
C	CLOSE (UNIT=11, STATUS='KEEP')
C	IF (NRING.GT.0) THEN
C      progname='copy d:\oa2\pgcac2.dat d:\oa2\PGCX2.dat'
C	RESULT = SYSTEMQQ(Progname)
C	END IF
C
C     Copy D:\OA2\PGCAC3.DAT to D:\OA2\PGCX3.DAT
C      progname='copy d:\oa2\pgcac3.dat d:\oa2\PGCX3.dat'
C	RESULT = SYSTEMQQ(Progname)
C 
C     Run program PGD
      CALL ChDir("../pgd/")
      CALL system("./pgdscr" , RESULT)
      CALL ChDir(CWD)
	OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1,110) KEYERR
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
	IF (KEYERR.EQ.KEYBAD) GO TO 99
C
C     Run program ACPGD
      CALL ChDir("../acpgd/")
      CALL system("./acpgdscr" , RESULT)
      CALL ChDir(CWD)
	OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1,110) KEYERR
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
	IF (KEYERR.EQ.KEYBAD) GO TO 99
C     Run program RDVEL2
      CALL ChDir("../rdvel2/")
      CALL system("./rdvel2scr" , RESULT)
      CALL ChDir(CWD)
	OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1,110) KEYERR
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
	IF (KEYERR.EQ.KEYBAD) GO TO 99
C     Run program TRUNCATE
C      progname = 'C:\LGTSYN\TRUNCATE\DEBUG\TRUNCATE.EXE'
C	RESULT = SYSTEMQQ(Progname)
C	OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
C	READ (IN1,110) KEYERR
C	CLOSE (UNIT = IN1, STATUS = 'KEEP')
C	IF (KEYERR.EQ.KEYBAD) GO TO 99
C     Loop through all wavelength ranges
      DO JLOOP=1,NINT
C     Prepare the control file for program ACPGF6. 
C     This program segment sets the wavelength range for ACPGF6.
C	READ (IN4,105) ADA
C	progname = ADA
C	RESULT = SYSTEMQQ(Progname)
C     Run program ACPGF6.
      CALL ChDir("../acpgf6/")
      CALL system("./acpgf6scr" , RESULT)
      CALL ChDir(CWD)
	OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1,110) KEYERR
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
	IF (KEYERR.EQ.KEYBAD) GO TO 99
C     Run program DATCONV.
      CALL ChDir("../datconv/")
      CALL system("./datconvscr" , RESULT)
      CALL ChDir(CWD)
	OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1,110) KEYERR
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
	IF (KEYERR.EQ.KEYBAD) GO TO 99
C     Store output.
      IF (JLOOP.EQ.1) THEN
		ADB=TXT//AD1
	END IF
	IF (JLOOP.EQ.2) THEN
		ADB=TXT//AD2 
	END IF
	IF (JLOOP.EQ.3) THEN
		ADB=TXT//AD3
	END IF
	IF (JLOOP.EQ.4) THEN
		ADB=TXT//AD4
	END IF
	IF (JLOOP.EQ.5) THEN
		ADB=TXT//AD5
	END IF
	IF (JLOOP.EQ.6) THEN
		ADB=TXT//AD6
	END IF
	NAME1=ADB//PHASE
C      copy ../oa/SYSSM.DAT to ../synthsp/lc/SMAD0830.001
      CALL system("cp ../oa/SYSSM.DAT ../synthsp/lc/SMAD0830.001",
     $ RESULT)
    	END DO		   !DO JLOOP=1,NINT
	REWIND IN4
    3	END DO		   !DO KLOOP=1,NPSI
    4 CONTINUE
C      REWIND IN5
C	IF (NPSI.EQ.1) GO TO 10
C
   10 CONTINUE
      CALL SYSTEM_CLOCK(CLOCK2,COUNT_RATE,COUNT_MAX)
      AAA=CLOCK1
      BBB=CLOCK2
      CCC=COUNT_RATE
      TM=(BBB-AAA)/CCC
      WRITE (IN2,303) TM
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
	CLOSE (UNIT = IN2, STATUS = 'KEEP')
	CLOSE (UNIT = IN3, STATUS = 'KEEP')
	CLOSE (UNIT = IN4, STATUS = 'KEEP')
	CLOSE (UNIT = IN5, STATUS = 'KEEP')
	STOP
   99 CALL PMDSTOP      
	END PROGRAM SYNACSP
