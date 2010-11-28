      SUBROUTINE PROJ(TARY,X,Y,Z,XP,YP,ZP)
	INCLUDE 'COMA.FOR'   
      DIMENSION TARY(MTR,MTR)
      A1=TARY(1,1)
      A2=TARY(1,2)
      A3=TARY(1,3)
      B1=TARY(2,1)
      B2=TARY(2,2)
      B3=TARY(2,3)
      C1=TARY(3,1)
      C2=TARY(3,2)
      C3=TARY(3,3)      
      XP=TARY(1,1)*X+TARY(1,2)*Y+TARY(1,3)*Z 
      YP=TARY(2,1)*X+TARY(2,2)*Y+TARY(2,3)*Z 
      ZP=TARY(3,1)*X+TARY(3,2)*Y+TARY(3,3)*Z 
      RETURN     
      END 
