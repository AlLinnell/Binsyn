      PROGRAM HORZPLTA
C	USE MSFLIB
C    Program to produce data file to plot horizons of accretion disk
C    components.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	INCLUDE 'ACOMA.FOR'
      DIMENSION FL1(22)
      CHARACTER * 28 FL1
	INTEGER(4) RESULT
	INTEGER * 2 KEYGOOD, KEYBAD
      DATA IN1,IN2,NOUT1,NOUT2,NOUT3,NOUT4,NOUT5,NOUT6/1,2,3,4,5,6,7,8/
	DATA NOUT7,NOUT8,NOUT9,NOUT10/9,10,11,12/
	DATA NOUT11,NOUT12,NOUT13,NOUT14/13,14,15,16/
	DATA NOUT15,NOUT16,NOUT17,NOUT18/18,19,20,21/
	DATA IN3/17/
      DATA ANG,SP1,SP2,SP3,SP4,SP5,SP6/6.29D0,7.0D0,7.2D0,7.4D0,7.6D0,
     $7.8D0,8.D0/
      DATA ANGA,SP7,SP8,SP9,SP10,SP11,SP12/6.39D0,8.2D0,8.4D0,8.6D0,
     $8.8D0,9.0D0,9.2D0/
	DATA SP13,SP14,SP15,SP16/9.4D0,9.6D0,9.8D0,10.8D0/
	DATA ICT1,ICT2,ICT3,ICT4,ICT5,ICT6,ICT7/0,0,0,0,0,0,0/
	DATA ICT8,ICT9,ICT10,ICT11,ICT12,ICT13,ICT14/0,0,0,0,0,0,0/
	DATA ICT17,ICT18,ICT19,ICT20/0,0,0,0/
	DATA KEYGOOD/0/
	DATA KEYBAD/-1/
  100 FORMAT (A28)
  101 FORMAT (2F12.5)
  102 FORMAT (' ')
  103 FORMAT (I2)
C**********************************************************************
C      RESULT=SETEXITQQ(QWIN$EXITNOPERSIST)
      OPEN (UNIT=23,   FILE='C:\LGTSYN\HORZPLTA\HORZPLTA.ILS',
     $STATUS= 'OLD')
      READ (23, 100)     (FL1(J),J=1,21)
      OPEN (UNIT= IN1,  FILE=FL1(1),  STATUS= 'OLD')
      OPEN (UNIT= IN2,  FILE=FL1(2),  STATUS= 'OLD')
      OPEN (UNIT=NOUT1, FILE=FL1(3),  STATUS= 'REPLACE')	!eclipsing star, large theta
	OPEN (UNIT=NOUT2, FILE=FL1(4),  STATUS= 'REPLACE')	!eclipsed star, large theta
	OPEN (UNIT=NOUT3, FILE=FL1(5),  STATUS= 'REPLACE')	!one vertical rim horizon
	OPEN (UNIT=NOUT4, FILE=FL1(6),  STATUS= 'REPLACE')	!other vertical rim horizon
	OPEN (UNIT=NOUT5, FILE=FL1(7),  STATUS= 'REPLACE')	!eclipsed star, large theta, 2nd section
	OPEN (UNIT=NOUT6, FILE=FL1(8),  STATUS= 'REPLACE') 	!bottom rim, 3rd section
      OPEN (UNIT=NOUT7, FILE=FL1(9),  STATUS= 'REPLACE')	!eclipsing star, small theta, initial branch
	OPEN (UNIT=NOUT8, FILE=FL1(10), STATUS= 'REPLACE')	!eclipsing star, small theta, 2nd branch
      OPEN (UNIT=NOUT9, FILE=FL1(11), STATUS= 'REPLACE')	!eclipsed star, small theta, initial branch
	OPEN (UNIT=NOUT10,FILE=FL1(12), STATUS= 'REPLACE')	!eclipsed star, small theta, 2nd branch
      OPEN (UNIT=NOUT11,FILE=FL1(13), STATUS= 'REPLACE')	!top rim, second section
	OPEN (UNIT=NOUT12,FILE=FL1(14), STATUS= 'REPLACE')	!bottom rim, second section
      OPEN (UNIT=NOUT13,FILE=FL1(15), STATUS= 'REPLACE')	!top rim, first section
      OPEN (UNIT=NOUT14,FILE=FL1(16), STATUS= 'REPLACE')	!bottom rim, first section
      OPEN (UNIT=NOUT15,FILE=FL1(17), STATUS= 'REPLACE')	!first vertical rim, 2nd section
	OPEN (UNIT=NOUT16,FILE=FL1(18), STATUS= 'REPLACE')	!other vertical rim, 2nd section
	OPEN (UNIT=NOUT17,FILE=FL1(19), STATUS= 'REPLACE')	!eclipsing star, large theta, 2nd section
	OPEN (UNIT=NOUT18,FILE=FL1(20), STATUS= 'REPLACE')  !bottom rim, third section
	OPEN (UNIT= IN3,  FILE=FL1(21), STATUS= 'OLD')
C**********************************************************************
      WRITE (IN3,103) KEYBAD
	CLOSE (UNIT=IN3, STATUS = 'KEEP')
    1 READ (IN1, 101, ERR=98, END=10) X,Y
      IF ((X.GT.2.D0).AND.(Y.GT.2.D0)) THEN
		BF1=X
		BF2=Y
		GO TO 1
	ELSE
C     ANGA indicates the eclipsing star.
C     This file always associates with the large theta branch.
		IF ((BF1.EQ.ANGA).AND.(BF2.EQ.ANGA)) THEN
			WRITE (NOUT1,101) X,Y
			ICT1=ICT1+1
			GO TO 1
		END IF
C     ANG indicates the eclipsed star.
C     This file always associates with the large theta branch.
		IF ((BF1.EQ.ANG).AND.(BF2.EQ.ANG)) THEN
			WRITE (NOUT2,101) X,Y
			ICT2=ICT2+1
			GO TO 1
		END IF
C     SP2 delimits one "vertical" rim horizon
		IF ((BF1.EQ.SP2).AND.(BF2.EQ.SP2)) THEN
			WRITE (NOUT3,101) X,Y
			ICT3=ICT3+1
			GO TO 1
		END IF
C     SP11 delimits second section of first "vertical" rim
C     horizon, if needed
		IF ((BF1.EQ.SP11).AND.(BF2.EQ.SP11)) THEN
			WRITE (NOUT15,101) X,Y
			ICT13=ICT13+1
			GO TO 1
		END IF
C     SP3 delimits the other "vertical" rim horizon
		IF ((BF1.EQ.SP3).AND.(BF2.EQ.SP3)) THEN
			WRITE (NOUT4,101) X,Y
			ICT4=ICT4+1
			GO TO 1
		END IF
C     SP12 delimits second section of other "vertical" rim
C     horizon, if needed
		IF ((BF1.EQ.SP12).AND.(BF2.EQ.SP12)) THEN
			WRITE (NOUT16,101) X,Y
			ICT14=ICT14+1
			GO TO 1
		END IF
C     SP4 delimits the top rim, if all one section
		IF ((BF1.EQ.SP4).AND.(BF2.EQ.SP4)) THEN
			WRITE (NOUT13,101) X,Y
			ICT5=ICT5+1
			GO TO 1
		END IF
C     SP5 delimits the lower rim, if all one section
		IF ((BF1.EQ.SP5).AND.(BF2.EQ.SP5)) THEN
			WRITE (NOUT14,101) X,Y
			ICT6=ICT6+1
			GO TO 1
		END IF
C     SP1 delimits small theta branch of eclipsing
C     star, initial branch
		IF ((BF1.EQ.SP1).AND.(BF2.EQ.SP1)) THEN
			WRITE (NOUT7,101) X,Y
			ICT7=ICT7+1
			GO TO 1
		END IF
C     SP6 delimits small theta branch of
C     eclipsing star, second branch, if needed. 
		IF ((BF1.EQ.SP6).AND.(BF2.EQ.SP6)) THEN
			WRITE (NOUT8,101) X,Y
			ICT8=ICT8+1
			GO TO 1
		END IF
C     SP7 delimits small theta branch of
C     eclipsed star, first segment.
		IF ((BF1.EQ.SP7).AND.(BF2.EQ.SP7)) THEN
			WRITE (NOUT9,101) X,Y
			ICT9=ICT9+1
			GO TO 1
		END IF
C     SP8 delimits small theta branch of
C     eclipsed star, second branch, if needed.
		IF ((BF1.EQ.SP8).AND.(BF2.EQ.SP8)) THEN
			WRITE (NOUT10,101) X,Y
			ICT10=ICT10+1
			GO TO 1
		END IF
C     SP9 delimits second section of top rim, if needed
		IF ((BF1.EQ.SP9).AND.(BF2.EQ.SP9)) THEN
			WRITE (NOUT11,101) X,Y
			ICT11=ICT11+1
			GO TO 1
		END IF
C     SP10 delimits second section of bottom rim, if needed.
		IF ((BF1.EQ.SP10).AND.(BF2.EQ.SP10)) THEN
			WRITE (NOUT12,101) X,Y
			ICT12=ICT12+1
			GO TO 1
		END IF
C     SP15 delimits third section of bottom rim, if needed
		IF ((BF1.EQ.SP15).AND.(BF2.EQ.SP15)) THEN
			WRITE (NOUT6,101) X,Y
			ICT19=ICT19+1
			GO TO 1
		END IF
C     SP13 delimits second segment of large theta branch, eclipsing star
		IF ((BF1.EQ.SP13).AND.(BF2.EQ.SP13)) THEN
			WRITE (NOUT17,101) X,Y
			ICT17=ICT17+1
			GO TO 1
		END IF
C     SP14 delimits second segment of large theta branch, eclipsed star
		IF ((BF1.EQ.SP14).AND.(BF2.EQ.SP14)) THEN
			WRITE (NOUT5,101) X,Y
			ICT18=ICT18+1
			GO TO 1
		END IF
C     SP16 delimits third segment of bottom rim
		IF ((BF1.EQ.SP16).AND.(BF2.EQ.SP16)) THEN
			WRITE (NOUT18,101) X,Y
			ICT20=ICT20+1
			GO TO 1
		END IF
	END IF
	GO TO 1
   10 CONTINUE
C**********************************************************************
      CLOSE (UNIT= IN1, STATUS='KEEP')
	CLOSE (UNIT= IN2, STATUS='KEEP')
	CLOSE (UNIT= NOUT1, STATUS='KEEP')
      CLOSE (UNIT= NOUT2, STATUS='KEEP')
	CLOSE (UNIT= NOUT3, STATUS='KEEP')
	CLOSE (UNIT= NOUT4, STATUS='KEEP')
	CLOSE (UNIT= NOUT5, STATUS='KEEP')
	CLOSE (UNIT= NOUT6, STATUS='KEEP')
	CLOSE (UNIT= NOUT7, STATUS='KEEP')
	CLOSE (UNIT= NOUT8, STATUS='KEEP')
	CLOSE (UNIT= NOUT9, STATUS='KEEP')
	CLOSE (UNIT= NOUT10, STATUS='KEEP')
	CLOSE (UNIT= NOUT11, STATUS='KEEP')
	CLOSE (UNIT= NOUT12, STATUS='KEEP')
	CLOSE (UNIT= NOUT13, STATUS='KEEP')
	CLOSE (UNIT= NOUT14, STATUS='KEEP')
	CLOSE (UNIT= NOUT15, STATUS='KEEP')
	CLOSE (UNIT= NOUT16, STATUS='KEEP')
	CLOSE (UNIT= NOUT17, STATUS='KEEP')
	CLOSE (UNIT= NOUT18, STATUS='KEEP')
	OPEN (UNIT=IN3, FILE=FL1(21), STATUS = 'OLD')
	WRITE (IN3,103) KEYGOOD
	CLOSE (UNIT=IN3, STATUS= 'KEEP')
C**********************************************************************
      STOP
   98 CALL PMDSTOP
      END PROGRAM HORZPLTA
