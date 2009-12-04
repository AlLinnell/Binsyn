      PROGRAM SET3
C	USE MSFLIB
C     Program to set input file for program PGB
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION VLPSI(300),FL1(4)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 7 LABEL
      CHARACTER * 36 FL1
	INTEGER(4) RESULT
      DATA KIN/1/
      DATA IN1,IN2,NOUT1/2,3,4/
      DATA KEYBAD/-1/
      DATA KEYGOOD/0/
  100 FORMAT (A36)
  101 FORMAT (I2)
  105 FORMAT (1X,'*',12X,'***PGB',/,1X,'*',8X,
     $'Information file for program PGB',/,1X,'*',8X,
     $'File name= PGB.INF',/,1X,'*',/,1X,'*')
  106 FORMAT (1X,'Control integer to decide on the form of input'
     $,9X,'KCTRL',2X,':',I3,/,1X,'*',8X,
     $'If KCTRL=0, use N1,N2,N3 to calculate phases',/,1X,'*',8X,
     $'If KCTRL=1,input individual phase values from',/,1X,'*',
     $19X,'this file.(VLPSI values)',/,1X,'*',8X,
     $'If KCTRL= 2,read N1,N2,N3 and phase values from',/,1X,'*',19X,
     $'unit 13')
  109 FORMAT (1X,'Number of orbital phase values input,if KCTRL=1'
     $,8X,'NPSI',3X,':',I3,/,1X,'*')
  111 FORMAT (1X,'Orbital inclination in degrees',25X,'VLVI',
     $3X,':',F11.7,/,1X,'*')
  112 FORMAT (1X,'Orbital eccentricity',35X,'ECC',
     $4X,':',F8.5,/,1X,'*')
  113 FORMAT (1X,'Number of orbital values between phase 0.0 and int.con
     $t.N1',4X,':',I3,/,1X,'*',6X,'(Primary minimum)')
  114 FORMAT (1X,'Number of orbital values,int.contact-ext. contact'
     $,7X,'N2',4X,':',I3,/,1X,'*',6X,'(Primary minimum)')
  115 FORMAT (1X,'Number of orbital values,ext.contact-max. elong.',
     $8X,'N3',4X,':',I3,/,1X,'*',6X,'(Primary minimum)')
  120 FORMAT (1X,'Number of orbital values betweeen mid-sec. and int.con
     $t.N4',4X,':',I3,/,1X,'*',6X,'(Secondary minimum)')
  116 FORMAT (1X,'Ordinal value of starting phase value',18X,
     $'LST',4X,':',I3,/,1X,'*',8X,'For calculated phases,full range,set
     $LST=0')
  117 FORMAT (1X,'Ordinal value of final phase value',21X,
     $'LND',4X,':',I3,/,1X,'*',8X,
     $'For calculated phases,full range,set LND=0',/,1X,'*')
  118 FORMAT (1X,'Numerical values of orbital phase,if input directly'
     $,3X,'VLPSI(1):',F11.8)
  119 FORMAT (1X,'*',53X,'VLPSI(2):',F11.8)
  121 FORMAT (1X,'Longitude of periastron (degrees)',22X,'OMD',4X,':',
     $F9.4,/,1X,'*')
  122 FORMAT (1X,'Number of orbital values,int. contact-ext. contact',
     $6X,'N5',4X,':',I3,/,1X,'*',6X,'(Secondary minimum)')
  123 FORMAT (1X,'Number of orbital values,ext.contact-max. elong.',8X,
     $'N6',4X,':'I3,/,1X,'*')
C	RESULT=SETEXITQQ(QWIN$EXITNOPERSIST) 
C***********************************************************
C
      OPEN (UNIT=5,     FILE='C:\LGTSYN\SET3\SET3.ILS',STATUS='OLD')
      READ (5,100)      (FL1(J),J=1,4)
      OPEN (UNIT=  KIN, FILE= FL1(1), STATUS='OLD')
      WRITE (KIN,101) KEYBAD
      REWIND KIN
      OPEN (UNIT=  IN1, FILE= FL1(2), STATUS='OLD')
      OPEN (UNIT=  IN2, FILE= FL1(3), STATUS='OLD')
      OPEN (UNIT=NOUT1, FILE= FL1(4), STATUS='REPLACE')
C***********************************************************
C     Input old data
      LABEL = ' ***PGB'
      LBLERR=0
      NU=IN1
C     Find label and point to first data line
      CALL GTLBL ( LABEL, LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
      CALL IREAD (KCTRL)
      CALL DUMMY (5)
      CALL IREAD (NPSI)
      CALL DUMMY (1)
      CALL DREAD (VLVI, 'F9.5')
      CALL DUMMY (1)
      CALL DREAD (ECC, 'F8.5')
      CALL DUMMY (1)
      CALL DREAD (OMD, 'F9.4')
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
      IF (KCTRL.NE.1) GO TO 10
      DO K=1,NPSI
      CALL DREAD (VLPSI(K), 'F11.8')
      END DO
C*********************************************************
C     Read update data
   10 LABEL = 'UPDATEB'
      NU=IN2
      CALL GTLBL ( LABEL, LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
      CALL DREAD ( VLVIUP, 'F15.11')
      CALL DUMMY (1)
      CALL DREAD ( ECCUP, 'F15.11')
      CALL DUMMY (1)
      CALL DREAD ( OMDUP, 'F15.11')
C********************************************************
      WRITE (NOUT1,105)
      WRITE (NOUT1,106) KCTRL
      WRITE (NOUT1,109) NPSI
      WRITE (NOUT1,111) VLVIUP
      WRITE (NOUT1,112) ECCUP
      WRITE (NOUT1,121) OMDUP
      WRITE (NOUT1,113) N1
      WRITE (NOUT1,114) N2
      WRITE (NOUT1,115) N3
      WRITE (NOUT1,120) N4
      WRITE (NOUT1,122) N5
      WRITE (NOUT1,123) N6
      WRITE (NOUT1,116) LST
      WRITE (NOUT1,117) LND
      WRITE (NOUT1,118) VLPSI(1)
      IF (KCTRL.NE.1) GO TO 20
      DO K=2,NPSI
      WRITE (NOUT1,119) VLPSI(K)
      END DO
   20 CLOSE (UNIT=  IN1, STATUS='KEEP')
      CLOSE (UNIT=  IN2, STATUS='KEEP')
      CLOSE (UNIT=NOUT1, STATUS='KEEP')
      WRITE (KIN, 101) KEYGOOD
      CLOSE (UNIT=  KIN, STATUS='KEEP')
      STOP
      END PROGRAM SET3
