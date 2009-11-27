      PROGRAM REPL4
C	USE MSFLIB
C     Program to replace PGA input filE by PGA.IUP
C
      INCLUDE 'COMA.FOR'
      DIMENSION FVR(MTW)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 7 LABEL
      CHARACTER * 36 FL1,BUF
	INTEGER(4) RESULT
      DIMENSION FL1(3)
      DATA IN1,IN2,NOUT1/1,2,3/
      DATA KEYBAD/-1/
      DATA KEYGOOD/0/
  100 FORMAT (A36)
  101 FORMAT (I2)
  105 FORMAT ('*',13X,'***PGA',/,'*',9X,'Information file for program PG
     $A',/,'*',9X,'File name=PGA.INF',/,'*',/,'*')
  106 FORMAT ('Number of latitude values per hemisphere',
     $15X,'NTH',5X,':',I3)
  108 FORMAT ('Number of points on each theta half circle',13X,'NPTH',4X
     $,':',I3)
  208 FORMAT ('Number of radial increments,volume calculation',9X,'NR',6
     $X,':',I3)
  109 FORMAT ('Key for debug printout.Default to noprint.',13X,'KDB',5X,
     $':',I3,/,'*',4X,'Noprint=0',/,'*',4X,'Print=1')
  110 FORMAT ('Key for type stellar model.Default to Roche',12X,'LRT',5X
     $,':',I3,/,'*',4X,'Roche=1',/,'*',4X,'Spherical=2',/,'*',/,
     $'*',/,'*',/,'*',/,'*',/,'*')
  111 FORMAT ('Ratio of asynchronous rotation,component I',14X,'FV(I)',2
     $X,':',F11.5,/,'Ratio of asynchronous rotation,component 3-I',12X,'
     $FV(3-I):',F11.5)
  112 FORMAT ('Fractional radius,component I,spherical case',12X,'RA',5X
     $,':',F11.8)
  113 FORMAT ('Fractional radius,component 3-I,spherical case',10X,'RB',
     $5X,':',F11.8,/,'*')
  114 FORMAT ('Mass of component I,in gm.',30X,'VMI',4X,':',E18.11)
  115 FORMAT ('Separation of centers,in cm.',28X,'RI',5X,':',E20.13,/,'*
     $')
  116 FORMAT ('Mass ratio,in sense (eclipsed/eclipsing),phase 0.0',6X,'Q
     $S',5X,':',F13.8,/,'*')
  117 FORMAT ('Photospheric Roche potential,comp.I',17X,'OM(I)',2X,'
     $:',F18.13)
  118 FORMAT ('Photospheric Roche potential,comp.3-I',15X,'OM(3-I)',4X,
     $':',F18.13,/,'*')
	RESULT=SETEXITQQ(QWIN$EXITNOPERSIST)
C***********************************************************
      OPEN (UNIT=5,FILE='C:\LGTSYN\REPL4\REPL4A.ILS',   STATUS='OLD')
      READ ( 5, 100) BUF
      OPEN (UNIT=4,FILE= BUF,   STATUS='OLD')
      READ ( 4,  100)   (FL1(J),J=1,3)
      OPEN (UNIT=IN1,   FILE= FL1(1),  STATUS='OLD')
      WRITE (IN1, 101) KEYBAD
      REWIND IN1
      OPEN (UNIT=IN2,   FILE= FL1(2),  STATUS='OLD')
      OPEN (UNIT=NOUT1, FILE= FL1(3),  STATUS='REPLACE')
C***********************************************************
C     Now read data from old PGA input file
      LABEL = ' ***PGA'
      NU=IN2
      CALL GTLBL ( LABEL, LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
      CALL IREAD (NTH)
      CALL IREAD (NPTH)
      CALL IREAD (NR)
      CALL IREAD (KDB)
      CALL DUMMY (2)
      CALL IREAD (LRT)
      CALL DUMMY (8)
      CALL DREAD (FVR(1), 'F11.8')
      CALL DREAD (FVR(2), 'F11.8')
      CALL DREAD (RA, 'F11.8')
      CALL DREAD (RB, 'F11.8')
      CALL DUMMY (1)
      CALL DREAD (VMI, 'D18.11')
      CALL DREAD (RI, 'D20.13')
      CALL DUMMY (1)
      CALL DREAD (QS, 'F13.8')
      CALL DUMMY (1)
      CALL DREAD (OM(1), 'D18.13')
      CALL DREAD (OM(2), 'D18.13')
C
C     Write new PGA input file
C
      WRITE (NOUT1,105)
      WRITE (NOUT1,106) NTH
      WRITE (NOUT1,108) NPTH
      WRITE (NOUT1,208) NR
      WRITE (NOUT1,109) KDB
      WRITE (NOUT1,110) LRT
      WRITE (NOUT1,111) FVR(1),FVR(2)
      WRITE (NOUT1,112) RA
      WRITE (NOUT1,113) RB
      WRITE (NOUT1,114) VMI
      WRITE (NOUT1,115) RI
      WRITE (NOUT1,116) QS
      WRITE (NOUT1,117) OM(1)
      WRITE (NOUT1,118) OM(2)
      CLOSE (UNIT=IN2,   STATUS='KEEP')
      CLOSE (UNIT=NOUT1, STATUS='KEEP')
      WRITE (IN1, 101) KEYGOOD
      CLOSE (UNIT=IN1,   STATUS='KEEP')
      STOP
      END PROGRAM REPL4
