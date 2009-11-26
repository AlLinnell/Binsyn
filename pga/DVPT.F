      SUBROUTINE DVPT(I,NTH,NPTH)  
C     
C     NTH=NUMBER OF THETA MESH VALUES ON COMPONENT I,FROM UPPER POLE     
C     TO LOWER POLE, INCLUSIVE   
C     THIS = NUMBER OF THETA INTERVALS+1     
	INCLUDE 'COMA.FOR'
C
      DATA PI/3.141592653589793238463D0/
      DV=NTH-1   
      DO 8 ITH=1,NTH    
      PHV=(ITH-1)*PI/DV      
      TH(I,ITH)=PHV
      TH(3-I,ITH)=TH(I,ITH)    
      SNT(I,ITH)=DSIN(TH(I,ITH))
      SNT(3-I,ITH)=SNT(I,ITH)  
      ANU(I,ITH,1)=DCOS(TH(I,ITH))
      ANU(3-I,ITH,1)=ANU(I,ITH,1)
    8 CONTINUE   
      NPH(I,1)=1 
      NPH(3-I,1)=1      
      DO 15 ITH=2,NTH-1   
      NPH(I,ITH)=NPTH
      NPH(3-I,ITH)=NPH(I,ITH)  
   15 CONTINUE
      NPH(I,NTH)=1
	NPH(3-I,NTH)=NPH(I,NTH)   
      NTL(I,1)=NPH(I,1) 
      NTL(3-I,1)=NTL(I,1)      
      DO 2 K=2,NTH      
      NTL(I,K)=NTL(I,K-1)+NPH(I,K)    
      NTL(3-I,K)=NTL(I,K)      
    2 CONTINUE
      DO ITH=1,NTH   
	N=NPH(I,ITH)
	DO K=2,N
		ANU(I,ITH,K)=ANU(I,ITH,1)
		ANU(3-I,ITH,K)=ANU(3-I,ITH,1)
	END DO
	END DO  
      RETURN     
      END 
