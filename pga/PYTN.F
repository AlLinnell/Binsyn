      SUBROUTINE PYTN(I,X,Y,Z,ER,LM)  
C     THIS SRT CALCULATES THE VALUE OF Y FOR ASSIGNED X AND Z,ON THE     
C     CONDITION THAT THE POINT LIES ON THE SURFACE OF POTENTIAL OM(I)    
C     THE SRT TRANSMITS THE X AND Z VALUES TO VARIOUS SRT,S VIA VX(I)    
C     AND VYZ(I) 
C     THE OUTPUT Y IS RETURNED IN THE CALLING SEQUENCE PARAMETER  
C     THE ERROR LIMIT TO SRT NWRP IS RECEIVED VIA COMMON IN EL    
C     THE MAXIMUM NUMBER OF ITERATIONS FOR NWRP IS RECEIVED VIA M(2)     
      INCLUDE 'COMA.FOR'
      EXTERNAL OMY,OMYP,OMYPP 
	DATA ERLT/1.D-13/ 
      VX(I)=X    
      VYZ(I)=Z   
      J=1 
      SY=0.01D0
      TY=0.05D0  
    4 CALL OMY(I,SY,E1)    
      IF (DABS(E1).LE.ERLT) GO TO 11  
      CALL OMY(I,TY,E2)    
      IF (DABS(E2).LE.ERLT) GO TO 12  
      IF (E1.GT.0.D0) GO TO 5   
C     IF E1 IS (-),SY IS TOO LARGE    
      TY=SY      
      SY=0.25D0*SY 
      J=J+1      
      IF (J.LT.10) GO TO 4     
      CALL PMDSTOP      
    5 IF (E2.LT.0.D0) GO TO 10  
C     IF E2 IS (+),TY IS TOO SMALL    
      SY=TY      
      TY=2.D0*TY  
      J=J+1      
      IF (J.LT.6) GO TO 4      
      CALL PMDSTOP      
   11 Y=SY
      GO TO 6    
   12 Y=TY
      GO TO 6    
   10 CALL NWRP(SY,TY,I,1,1,Y,OMY,OMYP,OMYPP,ER,LM) 
    6 RETURN     
      END 
