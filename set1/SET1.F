      PROGRAM SET1
C     Program to set input file for program CALPT
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION OM(2),OMNU(2),FV(2),FL1(4)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 7 LABEL
      CHARACTER * 20 OBJ
      CHARACTER * 36 FL1
      DATA KEYBAD/-1/
      DATA KEYGOOD/0/
  100 FORMAT (A36)
  101 FORMAT (I2)
  105 FORMAT (1X,'*',11X,'**CALPT',/,1X,'*',9X,
     $ 'Information file for program CALPT',/,1X,'*',9X,
     $'File name= CALPT.INF',/,1X,'*',/,1X,'*')
  106 FORMAT (1X,'Name of object',41X,'OBJ',4X,':',A20)
  107 FORMAT (1X,'Mass of primary component in solar mass units',
     $10X,'VMI',4X,':',F9.6)
  108 FORMAT (1X,'Orbital period in days',33X,'PER',4X,':',F15.12)
  109 FORMAT (1X,'Mass ratio,eclipsing star/star eclipsed at phase 0.0'
     $,3X,'Q',6X,':',F11.8)
  110 FORMAT (1X,'Roche potential of primary component photosphere',
     $7X,'OM(I)',2X,':',F11.8)
  111 FORMAT (1X,'Roche potential of secondary component photosphere',
     $5X,'OM(3-I):',F11.8)
  112 FORMAT (1X,'Ratio of asynchronous rotation, first component',8X,
     $'FV(I)  :',F6.3)
  113 FORMAT (1X,'Ratio of asynchronous rotation, second component',7X,
     $'FV(3-I):',F6.3)
C**************************************************************
C
      OPEN (UNIT=5,  FILE='SET1.ILS',  STATUS= 'OLD')
      READ (5,100)   (FL1(J),J=1,4)
      OPEN ( UNIT=1, FILE= FL1(1), STATUS= 'OLD')
      WRITE ( 1, 101) KEYBAD
      REWIND 1
      OPEN ( UNIT=2, FILE= FL1(2), STATUS= 'OLD')
      OPEN ( UNIT=3, FILE= FL1(3), STATUS= 'OLD')
      OPEN ( UNIT=4, FILE= FL1(4), STATUS= 'REPLACE')
C*************************************************************
C     Input old data
      LABEL = '**CALPT'
      LBLERR=0
      NU=2
C     Find label and point to first data line
      CALL GTLBL( LABEL, LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
      CALL CREAD ( OBJ)
      CALL DREAD (VMI, 'F9.6')
      CALL DREAD (PER, 'F15.12')
      CALL DREAD (Q, 'F11.8')
      CALL DREAD (OM(1), 'F11.8')
      CALL DREAD (OM(2), 'F11.8')
      CALL DREAD (FV(1), 'F9.3')
      CALL DREAD (FV(2), 'F9.3')
C*************************************************************
C     Now read data to update input file
      LABEL = 'UPDATEA'
      NU=3
      CALL GTLBL ( LABEL, LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
      CALL DREAD (QNU, 'F14.11')
      CALL DREAD (OMNU(1), 'F14.11')
      CALL DREAD (OMNU(2), 'F14.11')
C*************************************************************
C     Output new file for CALPT input
      WRITE (4,105)
      WRITE (4,106) OBJ
      WRITE (4,107) VMI
      WRITE (4,108) PER
      WRITE (4,109) QNU
      WRITE (4,110) OMNU(1)
      WRITE (4,111) OMNU(2)
      WRITE (4,112) FV(1)
      WRITE (4,113) FV(2)
      CLOSE ( UNIT= 2, STATUS= 'KEEP')
      CLOSE ( UNIT= 3, STATUS= 'KEEP')
      CLOSE ( UNIT= 4, STATUS= 'KEEP')
      WRITE ( 1, 101) KEYGOOD
      CLOSE ( UNIT= 1, STATUS= 'KEEP')
      STOP 'SET1: Completed successfully.'
      END PROGRAM SET1
