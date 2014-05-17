      SUBROUTINE PD(Y0,H,F1,D1,D2,D3,D4,D5,D6,Y1)
C     Subroutine to do predicted value of ordinate in predictor-
C     corrector procedure.
C     The values of D1,D2,...are successively higher differences.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	DATA CF1/0.5D0/
	DATA CF2/4.1666666666666667D-1/
	DATA CF3/0.375D0/
	DATA CF4/3.4861111111111111D-1/
	DATA CF5/3.2986111111111111D-1/
	DATA CF6/3.15591931217D-1/
	Y1=Y0+H*(F1+CF1*D1+CF2*D2+CF3*D3+CF4*D4+CF5*D5+CF6*D6)
      RETURN	      
	END 
