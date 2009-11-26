      
      PROGRAM REPL3
	USE MSFLIB
C     Program to replace input file for program PGC by PGCUP.INF
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION WL(5),C1(2),
     $C2(2),C3(2),C4(2),UC(2),TE(2),A(2),BCF(2),UA(2),
     $FL1(3)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 7 LABEL
      CHARACTER * 36 FL1
	INTEGER(4) RESULT
      DATA IN1,IN2,NOUT1/1,2,3/
      DATA KEYBAD/-1/
      DATA KEYGOOD/0/
  100 FORMAT (A36)
  101 FORMAT (I2)
  105 FORMAT (1X,'*',12X,'***PGC',/,1X,'*',9X,
     $'Information file for Program PGC',/,1X,'*',9X,
     $'File name=PGC.INF',/,1X,'*',/,1X,'*')
  106 FORMAT (1X,'Number of wavelengths of observation',18X,
     $'NWL',5X,':',I3)
  107 FORMAT (1X,'Emergent radiation approximation',22X,
     $'MDAT',4X,':',I3,/,1X,'*',8X,'Black body,MDAT=0',/,
     $1X,'*',8X,'Model atmosphere,MDAT=1')
  104 FORMAT (1X,'Control for use of irradiated model atmospheres',
     $7X,'IRCT',4X,':',I3,/,1X,'*',8X,'If IRCT=0, do not use irradiated
     $ case',/,1X,'*',8X,'If IRCT=1, use irradiated case')
  108 FORMAT (1X,'Number of reference wavelengths for interpolation',
     $5X,'NRF',5X,':',I3)
  109 FORMAT (1X,'Number of surface gravity values for interpolation',
     $4X,'NGR',5X,':',I3)
  110 FORMAT (1X,'Number of effective temperature values for interpol.'
     $,2X,'NTMP',4X,':',I3)
  111 FORMAT (1X,'Control for polar temperature of secondary component'
     $,2X,'KY',6X,':',I3,/,1X,'*',8X,
     $'If KY=0,calculate polar temp. of secondary',/,1X,'*',8X,
     $'from ratio of polar gravity values, unless'
     $,/,1X,'*',8X,'filling factor.LE.0.0.In latter case use',/,1X,'*'
     $,8X,'input value of secondary polar temp.whatever',/,1X,'*',8X,
     $'the value of KY.',/,1X,'*',8X,
     $'If KY=1,use input value of second polar temp.')
  112 FORMAT (1X,'Wavelengths of observations',27X,'WL(1)',3X,
     $':',F8.4)
  113 FORMAT (1X,'*',53X,'WL(',I2,')',2X,':',F8.4)
  114 FORMAT (1X,'*',8X,'Note that there must be as many entries as',
     $/,1X,'*',8X,'separate observation wavelengths')
  121 FORMAT (1X,'*',/,1X,
     $'Coefficients for empirical temperature profile',8X,'C1(I)',
     $3X,':',F5.1)
  122 FORMAT (1X,'*',53X,'C2(I)',3X,':',F5.1)
  123 FORMAT (1X,'*',53X,'C3(I)',3X,':',F5.1)
 1230 FORMAT (1X,'*',53X,'C4(I)',3X,':',F5.1)
  124 FORMAT (1X,'*',53X,'C1(3-I) :',F5.1)
  125 FORMAT (1X,'*',53X,'C2(3-I) :',F5.1)
  126 FORMAT (1X,'*',53X,'C3(3-I) :',F5.1)
 1260 FORMAT (1X,'*',53X,'C4(3-I) :',F5.1)
  127 FORMAT (1X,'*',/,1X,'Limb darkening scaling coefficient',
     $20X,'UC(I)',3X,':',F7.4)
  128 FORMAT (1X,'Limb darkening scaling coefficient',20X,
     $'UC(3-I) :',F7.4)
  129 FORMAT (1X,'*',/,1X,'Polar effective temp.,kilokelvins',
     $21X,'TE(I)',3X,':',1X,F9.5)
  130 FORMAT (1X,'Polar effective temp.,kilokelvins',21X,
     $'TE(3-I) :',1X,F9.5)
  131 FORMAT (1X,'*',/,1X,'Bolometric albedo',37X,'A(I)    :',
     $F9.5)
  132 FORMAT (1X,'Bolometric albedo',37X,'A(3-I)  :',F9.5)
  133 FORMAT (1X,'*',/,1X,'Gravity brightening coefficient',
     $23X,'BCF(I)',2X,':',F9.5)
  134 FORMAT (1X,'Gravity brightening coefficient',23X,'BCF(3-I):'
     $,F9.5)
  135 FORMAT (1X,'*',/,1X,
     $'Limb darkening,irradiating star,reflection effect',5X,'UA(I)'
     $,3X,':',F5.1)
  136 FORMAT (1X,
     $'Limb darkening,irradiating star,reflection effect',5X,
     $'UA(3-I) :',F5.1,/,1X,'*')
  137 FORMAT (1X,
     $'Number of iterations for reflection effect',12X,'NITER',
     $3X,':',I4)
	RESULT=SETEXITQQ(QWIN$EXITNOPERSIST)
C***********************************************************
C 
      OPEN (UNIT=4,   FILE='C:\LGTSYN\REPL3\REPL3.ILS',STATUS='OLD')
      READ (4,100)      (FL1(J),J=1,3)
      OPEN (UNIT=  IN1, FILE= FL1(1), STATUS='OLD')
      WRITE ( IN1, 101) KEYBAD
      REWIND IN1
      OPEN (UNIT=  IN2, FILE= FL1(2), STATUS='OLD')
      OPEN (UNIT=NOUT1, FILE= FL1(3), STATUS='REPLACE')
C***********************************************************
C     Input old data
      LABEL = ' ***PGC'
      LBLERR=0
      NU=IN2
C     Find label and point to first data file
      CALL GTLBL( LABEL, LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
      CALL IREAD (NWL)
      CALL IREAD (MDAT)
      CALL DUMMY(2)
	CALL IREAD (IRCT)
	CALL DUMMY (2)
      CALL IREAD (NRF)
      CALL IREAD (NGR)
      CALL IREAD (NTMP)
      CALL IREAD (KY)
      CALL DUMMY(6)
      CALL DREAD (WL(1), 'F9.4')
      IF (NWL.LE.1) GO TO 10
      DO K=2,NWL
      CALL DREAD (WL(K), 'F9.4')
      END DO
   10 CALL DUMMY(3)
      CALL DREAD (C1(1), 'F6.1')
      CALL DREAD (C2(1), 'F6.1')
      CALL DREAD (C3(1), 'F6.1')
      CALL DREAD (C4(1), 'F6.1')
      CALL DREAD (C1(2), 'F6.1')
      CALL DREAD (C2(2), 'F6.1')
      CALL DREAD (C3(2), 'F6.1')
      CALL DREAD (C4(2), 'F6.1')
      CALL DUMMY (1)
      CALL DREAD (UC(1), 'F9.5')
      CALL DREAD (UC(2), 'F9.5')
      CALL DUMMY (1)
      CALL DREAD (TE(1), 'F9.5')
      CALL DREAD (TE(2), 'F9.5')
      CALL DUMMY (1)
      CALL DREAD (A(1), 'F9.5')
      CALL DREAD (A(2), 'F9.5')
      CALL DUMMY (1)
      CALL DREAD (BCF(1), 'F9.5')
      CALL DREAD (BCF(2), 'F9.5')
      CALL DUMMY (1)
      CALL DREAD (UA(1), 'F6.1')
      CALL DREAD (UA(2), 'F6.1')
	CALL DUMMY (1)
	CALL IREAD (NITER)
C*********************************************************
C     Write updated input file for PGC
      WRITE (NOUT1,105)
      WRITE (NOUT1,106) NWL
      WRITE (NOUT1,107) MDAT
	WRITE (NOUT1,104) IRCT
      WRITE (NOUT1,108) NRF
      WRITE (NOUT1,109) NGR
      WRITE (NOUT1,110) NTMP
      WRITE (NOUT1,111) KY
      WRITE (NOUT1,112) WL(1)
      IF (NWL.LE.1) GO TO 30
      DO K=2,NWL
      WRITE (NOUT1,113) K,WL(K)
      END DO
   30 WRITE (NOUT1,114)
      WRITE (NOUT1,121) C1(1)
      WRITE (NOUT1,122) C2(1)
      WRITE (NOUT1,123) C3(1)
      WRITE (NOUT1,1230)C4(1)
      WRITE (NOUT1,124) C1(2)
      WRITE (NOUT1,125) C2(2)
      WRITE (NOUT1,126) C3(2)
      WRITE (NOUT1,1260)C4(2)
      WRITE (NOUT1,127) UC(1)
      WRITE (NOUT1,128) UC(2)
      WRITE (NOUT1,129) TE(1)
      WRITE (NOUT1,130) TE(2)
      WRITE (NOUT1,131) A(1)
      WRITE (NOUT1,132) A(2)
      WRITE (NOUT1,133) BCF(1)
      WRITE (NOUT1,134) BCF(2)
      WRITE (NOUT1,135) UA(1)
      WRITE (NOUT1,136) UA(2)
	WRITE (NOUT1,137) NITER
C*****************************************************
      CLOSE (UNIT=  IN2, STATUS='KEEP')
      CLOSE (UNIT=NOUT1, STATUS='KEEP')
      WRITE (IN1, 101) KEYGOOD
      CLOSE (UNIT=  IN1, STATUS = 'KEEP')
      STOP
      END PROGRAM REPL3
