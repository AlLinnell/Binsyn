      SUBROUTINE FILL(I,NTH,ER,QN)
C     SRT TO STORE ALL THE GEOMETRIC QUANTITIES,INDEPENDENT OF THE LINE  
C     OF SIGHT,THAT COVER COMPONENT I ON AN ASSIGNED (THETA,PHI) GRID    
      INCLUDE 'COMA.FOR'
	INCLUDE 'COMM.FOR'
      ALM(I,1,1)=0.0D0
      AMU(I,1,1)=0.0D0    
      ALM(3-I,1,1)=0.0D0    
      AMU(3-I,1,1)=0.0D0  
      GO TO (1,2),M(2)
C     Spherical model
    2 XMN(I)=VR(I,1,1)
      XMN(3-I)=VR(3-I,1,1)
      YMN=0.0D0
      ZMN=0.0D0
      ALMX(I)=1.0D0
      ALMX(3-I)=1.0D0
      ALMY(I)=1.0D0
      ALMY(3-I)=1.0D0
C     RADII ARE PASSED IN ARRAY CSG,NOT YET USED
      OM(I)=1.0D0/VR(I,1,1)
      OM(3-I)=1.0D0/VR(3-I,1,1)
      G(I,1,1)=-1.0D0/VR(I,1,1)/VR(I,1,1)
      G(3-I,1,1)=-1.0D0/VR(3-I,1,1)/VR(3-I,1,1)
      POPL(I,1,1)=G(I,1,1)
      POPR(I,1,1)=G(I,1,1)
      POPN(I,1,1)=0.0D0
      CSB(I,1,1)=1.0D0
      ZL(I,1,1)=ALM(I,1,1)/G(I,1,1)*POPR(I,1,1)
      ZM(I,1,1)=AMU(I,1,1)/G(I,1,1)*POPR(I,1,1)
      ZN(I,1,1)=ANU(I,1,1)/G(I,1,1)*POPR(I,1,1)
      POPL(3-I,1,1)=G(3-I,1,1)
      POPR(3-I,1,1)=G(3-I,1,1)
      POPN(3-I,1,1)=0.0D0
      CSB(3-I,1,1)=1.0D0
      ZL(3-I,1,1)=ALM(3-I,1,1)/G(3-I,1,1)*POPR(3-I,1,1)
      ZM(3-I,1,1)=AMU(3-I,1,1)/G(3-I,1,1)*POPR(3-I,1,1)
      ZN(3-I,1,1)=ANU(3-I,1,1)/G(3-I,1,1)*POPR(3-I,1,1)
      GO TO 3
C     Roche model
C     DO DATA FOR UPPER POLE   
    1 CALL RCHER(I,1,1,ER)
      QSV=Q      
      Q=QN
      CALL RCHER(3-I,1,1,ER)
      Q=QSV      
      SNP=0.0D0
      CSP=1.0D0  
      CALL RCHERA(I,1,1)
      QSV=Q      
      Q=QN
      CALL RCHERA(3-I,1,1)     
      Q=QSV      
    3 DO 10 ITH=2,NTH
      CALL FILLA(I,ITH,ER,QN)     
   10 CONTINUE   
      RETURN     
      END 
