      SUBROUTINE LNTER1(X1,Y1,X2,Y2,XA,YA)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)    
C     SRT FOR LINEAR INTERPOLATION BETWEEN TWO POINTS      
      IF (Y1.EQ.Y2) GO TO 10   
      IF (XA.EQ.X1) GO TO 10   
      IF (XA.EQ.X2) GO TO 11   
      YA=(Y2-Y1)/(X2-X1)*(XA-X1)+Y1   
   20 RETURN     
   10 YA=Y1      
      GO TO 20   
   11 YA=Y2      
      GO TO 20   
      END 
