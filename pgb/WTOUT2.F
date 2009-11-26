      SUBROUTINE WTOUT2(NT,NV,NTH,I,IEC,SP,OMRAD,ECC,VMNAN,VECAN,VTRUE,
     $VRD,PSIP,KCTRL,KFORM,KU)
      INCLUDE 'COMA.FOR'
  100 FORMAT (5D24.15)  
  101 FORMAT (10I5)
      WRITE (KU,101) NT,NV,NTH,I,IEC,KCTRL,KFORM
      WRITE (KU,100) VI,PSI,VL0,VM0,VN0,OMRAD,ECC,VMNAN,VECAN,VTRUE,
     $VRD,SP,SHUP,PHUP,SHDN,PHDN,PSIP
      WRITE (KU,100) THP(I),THP(3-I),PHP(I),PHP(3-I),THM(I),THM(3-I),     
     $PHM(I),PHM(3-I),TFP(I),TFP(3-I),PFP(I),PFP(3-I),TFM(I),TFM(3-I),   
     $PFM(I),PFM(3-I)   
      WRITE (KU,100) ((PHMXA(I,K,KK),K=1,NTH),KK=1,4) 
      WRITE (KU,100) ((PHMXA(3-I,K,KK),K=1,NTH),KK=1,4)      
      WRITE (KU,100) ((GMXA(I,K,KK),K=1,NTH),KK=1,4)  
      WRITE (KU,100) ((GMXA(3-I,K,KK),K=1,NTH),KK=1,4)
      WRITE (KU,100) ((PHMNA(I,K,KK),K=1,NTH),KK=1,4) 
      WRITE (KU,100) ((PHMNA(3-I,K,KK),K=1,NTH),KK=1,4)      
      WRITE (KU,100) ((GMNA(I,K,KK),K=1,NTH),KK=1,4)  
      WRITE (KU,100) ((GMNA(3-I,K,KK),K=1,NTH),KK=1,4)
      WRITE (KU,100) (VST(I,K,1),K=1,NTH)     
      WRITE (KU,100) (VST(3-I,K,1),K=1,NTH)   
      WRITE (KU,100) (VST(I,K,2),K=1,NTH)     
      WRITE (KU,100) (VST(3-I,K,2),K=1,NTH)   
      WRITE (KU,100) (VST(I,K,3),K=1,NTH)     
      WRITE (KU,100) (VST(3-I,K,3),K=1,NTH)   
      WRITE (KU,100) (VND(I,K,1),K=1,NTH)     
      WRITE (KU,100) (VND(3-I,K,1),K=1,NTH)   
      WRITE (KU,100) (VND(I,K,2),K=1,NTH)     
      WRITE (KU,100) (VND(3-I,K,2),K=1,NTH)   
      WRITE (KU,100) (VND(I,K,3),K=1,NTH)     
      WRITE (KU,100) (VND(3-I,K,3),K=1,NTH)   
      WRITE (KU,100) (SHS(K,1),K=1,NTH)
      WRITE (KU,100) (SHN(K,1),K=1,NTH)
      WRITE (KU,100) (SHS(K,2),K=1,NTH)
      WRITE (KU,100) (SHN(K,2),K=1,NTH)
      WRITE (KU,100) (SHS(K,3),K=1,NTH)
      WRITE (KU,100) (SHN(K,3),K=1,NTH)
	DO ITH=1,NTH
	N=NPH(I,ITH)
      WRITE (KU,100) (CSG(I,ITH,K),K=1,N)
	END DO
	DO ITH=1,NTH
	N=NPH(I,ITH)
      WRITE (KU,100) (CSG(3-I,ITH,K),K=1,N)
	END DO
      RETURN     
      END 
