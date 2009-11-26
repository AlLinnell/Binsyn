      SUBROUTINE PNK(NKP,ICOUNT,K2CT,LCT,XP,YP,CSPJ,SNPJ,XCM,
     $YCM,NOUT1,NOUT2)
	INCLUDE 'COMA.FOR'
      DIMENSION LCT(16)
      DATA ANG/6.29D0/
	DATA ERLT/1.D-13/
  193 FORMAT (3F12.5,I5)
C  
C*********************************************************************
C     PLOT POINTS OUTSIDE HORIZON BOUNDARY,ON CONNECTING NECK   
C*********************************************************************
C
      DO 322 J=1,NKP     
      IF (DABS(XNK(J)-ANG).LE.ERLT) GO TO 322     
      IF (ICOUNT.EQ.2) GO TO 211      
C     ECLIPSING STAR    
      IF (LCT(12).NE.1) GO TO 322    
      YP=YNK(J)
      XP=XNK(J)  
      X=XP
      Y=YP
      IF (LCT(15).EQ.1) GO TO 79      
      Y=XP*CSPJ-YP*SNPJ
      X=XP*SNPJ+YP*CSPJ
      GO TO 303  
   79 CONTINUE   
      X=XP
      Y=YP
      GO TO 303  
C     ECLIPSED STAR     
  211 IF (LCT(8).NE.1) GO TO 322      
      YP=YNK(J)
      XP=XNK(J)  
      X=XP
      Y=YP
      IF (LCT(15).EQ.1) GO TO 80      
      Y=XP*CSPJ-YP*SNPJ
      X=XP*SNPJ+YP*CSPJ
      GO TO 303  
   80 X=XP
      Y=YP
  303 CONTINUE
      WRITE (NOUT2, 193) X-XCM, -Y+YCM, 3.D0, 0
	K2CT=K2CT+1
  322 CONTINUE
      RETURN
      END
