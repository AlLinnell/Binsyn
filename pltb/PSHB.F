      SUBROUTINE PSHB(ICOUNT,K2CT,LCT,NTH,TARY,XP,YP,ZP,CSPJ,SNPJ,
     $XCM,YCM,NOUT1,NOUT2)
      INCLUDE 'COMA.FOR'
      DIMENSION LCT(16),TARY(MTR,MTR)
      DATA ANG/6.29D0/
	DATA ERLT/1.D-13/
  193 FORMAT (3F12.5,I5)
C
C*********************************************************************
C     PLOT PROJECTIONS OF SHADOW BOUNDARIES  
C*********************************************************************
C  
      IF (ICOUNT.NE.2) GO TO 55
      IF (LCT(7).NE.1) GO TO 55
C
      DO 60 ITH=1,NTH   
      SNTV=SNT(I,ITH)   
      ANUV=ANU(I,ITH,1)   
   52 DO 59 J=1,3
      VGL=SHS(ITH,J)    
      IF (DABS(VGL-ANG).LE.ERLT) GO TO 59 
      CALL SGLTE(I,ITH,VGL,0.0000000001D0,SN,CS,RV)
      X=RV*SNTV*CS      
      Y=RV*SNTV*SN      
      Z=RV*ANUV  
      CALL PROJ(TARY,X,Y,Z,XP,YP,ZP)  
      X=XP
      Y=YP
      IF (LCT(15).EQ.1) GO TO 83      
      Y=XP*CSPJ-YP*SNPJ
      X=XP*SNPJ+YP*CSPJ
      GO TO 53   
   83 CONTINUE   
      X=XP
      Y=YP
   53 CONTINUE
      WRITE (NOUT2, 193) X-XCM, -Y+YCM, ZP, 0
	K2CT=K2CT+1
      VGL=SHN(ITH,J)    
      CALL SGLTE(I,ITH,VGL,0.0000000001D0,SN,CS,RV)
      X=RV*SNTV*CS      
      Y=RV*SNTV*SN      
      Z=RV*ANUV  
      CALL PROJ(TARY,X,Y,Z,XP,YP,ZP)  
      X=XP
      Y=YP
      IF (LCT(15).EQ.1) GO TO 84      
      Y=XP*CSPJ-YP*SNPJ
      X=XP*SNPJ+YP*CSPJ
      GO TO 54   
   84 CONTINUE   
      X=XP
      Y=YP
   54 CONTINUE
      WRITE (NOUT2, 193) X-XCM, -Y+YCM, ZP, 0
	K2CT=K2CT+1
   59 CONTINUE   
   60 CONTINUE
   55 CONTINUE
      RETURN
      END
