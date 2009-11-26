      SUBROUTINE PROJ(TARY,X,Y,Z,XP,YP,ZP)
	INCLUDE 'COMA.FOR'   
      DIMENSION TARY(MTR,MTR)      
      XP=TARY(1,1)*X+TARY(1,2)*Y+TARY(1,3)*Z 
      YP=TARY(2,1)*X+TARY(2,2)*Y+TARY(2,3)*Z 
      ZP=TARY(3,1)*X+TARY(3,2)*Y+TARY(3,3)*Z 
      RETURN     
      END 
