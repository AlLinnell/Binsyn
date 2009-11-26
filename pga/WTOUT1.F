      SUBROUTINE WTOUT1(FL,HFER,I,KC,KL,NT,NTH,MN,QS,QN,RI,VMI,VVI,
     $VVJ,NO)
C     SRT TO WRITE SELECTED INTERMEDIATE DATA TO OUTPUT FILE
      INCLUDE 'COMA.FOR' 
      INCLUDE 'COMM.FOR'
  100 FORMAT (3D26.16)  
  101 FORMAT (10I5)
      WRITE (NO,101) NTH,NT,KL,I,KC,MN 
      WRITE (NO,101) (NPH(I,KK),KK=1,NTH)     
      WRITE (NO,101) (NTL(I,KK),KK=1,NTH)     
      WRITE (NO,101) (NPH(3-I,KK),KK=1,NTH)   
      WRITE (NO,101) (NTL(3-I,KK),KK=1,NTH)   
      WRITE (NO,100) QN,QS,FL,FV(I),FV(3-I),EL,HFER,VMI,RI,VVI,VVJ 
      WRITE (NO,100) (TH(I,KK),KK=1,NTH)      
      WRITE (NO,100) (TH(3-I,KK),KK=1,NTH)    
      WRITE (NO,100) (SNT(I,KK),KK=1,NTH)     
      WRITE (NO,100) (SNT(3-I,KK),KK=1,NTH)   
      WRITE (NO,100) VL(I,1),OM(I),OM(3-I),OMC(I,1),OMC(I,2),OMC(3-I,1),
     $OMC(3-I,2),VL(I,2),VL(3-I,1),VL(3-I,2)    
      WRITE (NO,100) XMN(I),XMN(3-I),ALMX(I),ALMX(3-I),ALMY(I),ALMY(3-I)
     $,YMN,ZMN    
	DO ITH=1,NTH
	N=NPH(I,ITH)      
      WRITE (NO,100) (PH(I,ITH,KK),KK=1,N)
      WRITE (NO,100) (ALM(I,ITH,KK),KK=1,N)
      WRITE (NO,100) (AMU(I,ITH,KK),KK=1,N)
      WRITE (NO,100) (ANU(I,ITH,KK),KK=1,N)
      WRITE (NO,100) (VR(I,ITH,KK),KK=1,N) 
      WRITE (NO,100) (POPL(I,ITH,KK),KK=1,N)      
      WRITE (NO,100) (POPN(I,ITH,KK),KK=1,N)      
      WRITE (NO,100) (POPR(I,ITH,KK),KK=1,N)      
      WRITE (NO,100) (G(I,ITH,KK),KK=1,N)  
      WRITE (NO,100) (ZL(I,ITH,KK),KK=1,N) 
      WRITE (NO,100) (ZM(I,ITH,KK),KK=1,N) 
      WRITE (NO,100) (ZN(I,ITH,KK),KK=1,N) 
      WRITE (NO,100) (CSB(I,ITH,KK),KK=1,N)
	END DO
	DO ITH=1,NTH
	N=NPH(3-I,ITH) 
      WRITE (NO,100) (PH(3-I,ITH,KK),KK=1,N)      
      WRITE (NO,100) (ALM(3-I,ITH,KK),KK=1,N)     
      WRITE (NO,100) (AMU(3-I,ITH,KK),KK=1,N)     
      WRITE (NO,100) (ANU(3-I,ITH,KK),KK=1,N)     
      WRITE (NO,100) (VR(3-I,ITH,KK),KK=1,N)      
      WRITE (NO,100) (POPL(3-I,ITH,KK),KK=1,N)    
      WRITE (NO,100) (POPN(3-I,ITH,KK),KK=1,N)    
      WRITE (NO,100) (POPR(3-I,ITH,KK),KK=1,N)    
      WRITE (NO,100) (G(3-I,ITH,KK),KK=1,N)
      WRITE (NO,100) (ZL(3-I,ITH,KK),KK=1,N)      
      WRITE (NO,100) (ZM(3-I,ITH,KK),KK=1,N)      
      WRITE (NO,100) (ZN(3-I,ITH,KK),KK=1,N)      
      WRITE (NO,100) (CSB(3-I,ITH,KK),KK=1,N)
	END DO
      WRITE (NO,101) (M(KK),KK=1,5)
   10 RETURN     
      END 
