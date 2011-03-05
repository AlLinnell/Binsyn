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
C     Correct data must have been set in the control files in this
C     directory before starting a run.
C     ***************************************************************
C     NOTE: If a full set of orbital longitudes is to be run, set
C     the starting and ending orbital longitudes in SYNACSP.INF to 0.
C     ***************************************************************
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	DIMENSION FL1(6)
	DIMENSION PHS(300)
	COMMON /NUNIT/ NU, NUOUT, IFOUT
	character * 80 progname,ADA
	CHARACTER * 31 TXT
        CHARACTER * 43 NAME1
	CHARACTER * 36 FL1
	CHARACTER * 3 PHASE
	CHARACTER * 40 ADB
	CHARACTER * 7 LABEL
	CHARACTER * 9 AD1,AD2,AD3,AD4,AD5,AD6
      CHARACTER * 10 DATE,TIME,ZONE
      CHARACTER * 256 CWD
      CHARACTER * 64 LNA,LNB
	INTEGER(4) RESULT,NSTART
	INTEGER*2 IMIN,ISEC,I100TH,IHR,IDAY,IMON,IYR
      DIMENSION TIMEVALUES(8)
      INTEGER :: CLOCK1,CLOCK2,COUNT_RATE,COUNT_MAX,TIMEVALUES
	INTEGER * 2 KEYGOOD,KEYBAD,KEYERR
	DATA IN1,IN2,IN3,IN4,IN5,IN6/2,3,4,5,66,7/
	DATA KCTRLX,NPSIX,VLVI,ECC,OMD/1,1,86.D0,0.D0,90.D0/
	DATA N1,N2,N3,N4,N5,N6,LST,LND/4,8,8,4,8,8,0,0/
	DATA KEYGOOD/0/
	DATA KEYBAD/-1/
      LNA='Ordinal value of orbital longitude to select           IORD  
     $  :'
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
      OPEN (9, FILE = 'SYNACSP.ILS', STATUS = 'OLD')
	READ (9, 100) (FL1(J),J=1,6)
	OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
	OPEN (UNIT = IN2, FILE = FL1(2), STATUS = 'OLD')
	OPEN (UNIT = IN3, FILE = FL1(3), STATUS = 'OLD')
	OPEN (UNIT = IN4, FILE = FL1(4), STATUS = 'OLD')
	OPEN (UNIT = IN5, FILE = FL1(5), STATUS = 'OLD')
C     ******************************************************     
  100 FORMAT (A36)
  101 FORMAT (A31)
  102 FORMAT (A9)
  103 FORMAT (5D16.13)
  105 FORMAT (A80)
  106 FORMAT (10I5)
  107 FORMAT (A3)
  108 FORMAT (A14)
  109 FORMAT (A8)
  110 FORMAT (I2)
  111 FORMAT (10F8.4)
  112 FORMAT (A64,I4)
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
C     The following section contains lines to construct SYNACSP.INF
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
  303 FORMAT (1X,'Program running time=',F14.4,2X,'seconds')
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
    1 CONTINUE
C     Force program to start at the first orbital longitude
      NSTART=1
C     Run the full sequence of programs, beginning with CALPT
C
C     Run program CALPT
      CALL ChDir("../calpt/")
      CALL system("./calptscr" , RESULT)
      CALL ChDir(CWD)
		OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1,110) KEYERR
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
	IF (KEYERR.EQ.KEYBAD) GO TO 99
C     Run program TRNSF1
      CALL ChDir("../transf1/")
      CALL system("./transf1scr" , RESULT)
      CALL ChDir(CWD)
		OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1,110) KEYERR
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
	IF (KEYERR.EQ.KEYBAD) GO TO 99
C     Run program REPL4
      CALL ChDir("../repl4/")
      CALL system("./repl4scr" , RESULT)
      CALL ChDir(CWD)
		OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1,110) KEYERR
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
	IF (KEYERR.EQ.KEYBAD) GO TO 99
C     Run program PGA
      CALL ChDir("../pga/")
      CALL system("./pgascr" , RESULT)
      CALL ChDir(CWD)
		OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1,110) KEYERR
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
	IF (KEYERR.EQ.KEYBAD) GO TO 99
C     Run program PGB
C     Need to run PGB if individual orbital longitude runs are specified.
      CALL ChDir("../pgb/")
      CALL system("./pgbscr" , RESULT)
      CALL ChDir(CWD)
	OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1,110) KEYERR
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
	IF (KEYERR.EQ.KEYBAD) GO TO 99
      CALL system("cp ../oa/PGBX3.DAT ../oa/PGBX3SV.DAT", RESULT)
      CALL system("cp ../oa/PGBX3.DAT ./", RESULT)
C     Run program PLTB
      CALL ChDir("../pltb/")
      CALL system("./pltbscr" , RESULT)
      CALL ChDir(CWD)
	OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1,110) KEYERR
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
	IF (KEYERR.EQ.KEYBAD) GO TO 99
C     Run program ACPGA
      CALL ChDir("../acpga/")
      CALL system("./acpgascr" , RESULT)
      CALL ChDir(CWD)
		OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1,110) KEYERR
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
	IF (KEYERR.EQ.KEYBAD) GO TO 99
C     Run program ACPGB
      CALL ChDir("../acpgb/")
      CALL system("./acpgbscr" , RESULT)
      CALL ChDir(CWD)
	OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1,110) KEYERR
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
	IF (KEYERR.EQ.KEYBAD) GO TO 99
C     Run program ACPGC
      CALL ChDir("../acpgc/")
      CALL system("./acpgcscr" , RESULT)
      CALL ChDir(CWD)
		OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1,110) KEYERR
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
	IF (KEYERR.EQ.KEYBAD) GO TO 99
C     Run program PGC
      CALL ChDir("../pgc/")
      CALL system("./pgcscr" , RESULT)
      CALL ChDir(CWD)
		OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1,110) KEYERR
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
	IF (KEYERR.EQ.KEYBAD) GO TO 99
C     Run program PGCAC
      CALL ChDir("../pgcac/")
      CALL system("./pgcacscr" , RESULT)
      CALL ChDir(CWD)
		OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1,110) KEYERR
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
	IF (KEYERR.EQ.KEYBAD) GO TO 99
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
C     NOTE! This program only applies to systems where the accretion
C     disc is being considered by separate annuli. 
C      progname = 'C:\LGTSYN\TRUNCATE\DEBUG\TRUNCATE.EXE'
C	RESULT = SYSTEMQQ(Progname)
C	OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
C	READ (IN1,110) KEYERR
C	CLOSE (UNIT = IN1, STATUS = 'KEEP')
C	IF (KEYERR.EQ.KEYBAD) GO TO 99
C     Run program ACPGF6.
      CALL ChDir("../acpgf6/")
      CALL system("./acpgf6scr" , RESULT)
      CALL ChDir(CWD)
	OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1,110) KEYERR
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
	IF (KEYERR.EQ.KEYBAD) GO TO 99
C     Read number of orbital longitudes to process.
      READ (IN3,106) KCTRL,NPSI
C     Read the orbital longitude values
      READ (IN3,103) (PHS(K),K=1,NPSI)
	READ (IN3,103) VLVI
C     Convert to degrees
      VLVI=VLVI*57.2957795131D0
C     Read DATCONV.INF file
      CALL system("cp ../ia/DATCONV.INF ./SCRATCH", RESULT)
      OPEN (UNIT = IN6, FILE = FL1(6), STATUS = 'OLD')
      READ (IN6,105) ADA
      READ (IN6,105) ADA
      READ (IN6,105) ADA
      READ (IN6,105) ADA
      READ (IN6,105) ADA
      READ (IN6,112) LNB,IORD
      READ (IN6,105) ADA
      READ (IN6,112) LNB,KEY
      READ (IN6,105) ADA
      READ (IN6,112) LNB,ISW
      REWIND IN6
C     Force start at first orbital longitude
      IORD=1
C     Loop through all wavelength ranges
      DO JLOOP=1,NPSI
      READ (IN5,107) PHASE
C     Prepare the direct control file for datconv
      WRITE (IN6,250)
      WRITE (IN6,251)
      WRITE (IN6,252)
      WRITE (IN6,253)
      WRITE (IN6,254)
      WRITE (IN6,255),IORD
      WRITE (IN6,256)
      WRITE (IN6,257),KEY
      WRITE (IN6,258)
      WRITE (IN6,259),ISW
      WRITE (IN6,260)
      WRITE (IN6,261)
      WRITE (IN6,262)
      WRITE (IN6,263)
      WRITE (IN6,264)
      WRITE (IN6,265)
      WRITE (IN6,266)
      WRITE (IN6,267)
      REWIND IN6
C      CALL system("cp SCRATCH SYNACSP.INF", RESULT)
      CALL system("cp SCRATCH ../ia/DATCONV.INF", RESULT)
C     Run program DATCONV.
      CALL ChDir("../datconv/")
      CALL system("./datconvscr" , RESULT)
      CALL ChDir(CWD)
	OPEN (UNIT = IN1, FILE = FL1(1), STATUS = 'OLD')
	READ (IN1,110) KEYERR
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
	IF (KEYERR.EQ.KEYBAD) GO TO 99
C     Store output.
		ADB=TXT//AD1
	NAME1=ADB//PHASE
C      copy ../oa/SYSSM.DAT to ../binsyn/synspadd/SMAD0830.001 or subsequent
      CALL system(NAME1,
     $ RESULT)
       WRITE (*,106) JLOOP
      NSTART=NSTART+1
      IORD=IORD+1
    	END DO		   !DO JLOOP=1,NPSI
   10 CONTINUE
      CALL SYSTEM_CLOCK(CLOCK2,COUNT_RATE,COUNT_MAX)
      AAA=CLOCK1
      BBB=CLOCK2
      CCC=COUNT_RATE
      TM=(BBB-AAA)/CCC
      WRITE (*,303) TM
	CLOSE (UNIT = IN1, STATUS = 'KEEP')
	CLOSE (UNIT = IN2, STATUS = 'KEEP')
	CLOSE (UNIT = IN3, STATUS = 'KEEP')
	CLOSE (UNIT = IN4, STATUS = 'KEEP')
	CLOSE (UNIT = IN5, STATUS = 'KEEP')
	CLOSE (UNIT = IN6, STATUS = 'KEEP')
	STOP 'SYNACSP Successsfully completed'
   99 CALL PMDSTOP      
	END PROGRAM SYNACSP
