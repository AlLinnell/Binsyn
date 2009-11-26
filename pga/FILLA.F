      SUBROUTINE FILLA(I,ITH,ER,QN)      
C     SRT TO STORE ALL THE GEOMETRIC QUANTITIES,INDEPENDENT OF THE LINE  
C     OF SIGHT,THAT LIE ON A PARTICULAR THETA CIRCLE
      INCLUDE 'COMA.FOR'
	DATA ERLT/1.D-13/
C
      N=NPH(I,ITH)
      DO 1 K=1,N    
      SNP=DSIN(PH(I,ITH,K))  
      CSP=DCOS(PH(I,ITH,K))  
      IF (DABS(PH(I,ITH,K)).LE.ERLT) CSP=1.D0
      ALM(I,ITH,K)=SNT(I,ITH)*CSP  
      AMU(I,ITH,K)=SNT(I,ITH)*SNP  
      CALL RCHER(I,ITH,K,ER)      
      CALL RCHERA(I,ITH,K)     
    1 CONTINUE   
      QSV=Q      
      Q=QN
      DO 2 K=1,N    
      SNP=DSIN(PH(3-I,ITH,K))
      CSP=DCOS(PH(3-I,ITH,K))
      IF (DABS(PH(3-I,ITH,K)).LE.ERLT) CSP=1.D0
      ALM(3-I,ITH,K)=SNT(3-I,ITH)*CSP     
      AMU(3-I,ITH,K)=SNT(3-I,ITH)*SNP     
      CALL RCHER(3-I,ITH,K,ER)    
      CALL RCHERA(3-I,ITH,K)   
    2 CONTINUE   
      Q=QSV      
   12 RETURN     
      END 
