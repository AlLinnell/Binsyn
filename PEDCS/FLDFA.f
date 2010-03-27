      SUBROUTINE FLDFA(NRY,KST,ARA,ARC,ARD,ARE,
     $ARF,ARG,ARH,ARI,ARJ,ARK,ARL,ARM,ARN,ARO,ARP,ARQ,ARR)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION PHR(300),DLDT1(300),DLDT2(300),DLDA1(300),DLDA2(300),
     $DLDB1(300),DLDB2(300),DLDUC1(300),DLDUC2(300),DLDPT1(300),
     $DLDPT2(300),DLDQ(300),DLDI(300),VCLC(300),DLDECC(300),DLDOM(300)
      DIMENSION ARO(11),ARP(11),ARQ(11),ARR(11)
      DIMENSION DLDTZP(300),DLDTZS(300)
      DIMENSION ARA(11),        ARC(11),ARD(11),ARE(11),ARF(11),ARG(11),
     $ARH(11),ARI(11),ARJ(11),ARK(11),ARL(11),ARM(11),ARN(11)
      COMMON /E/ PHR,DLDT1,DLDT2,DLDA1,DLDA2,DLDB1,DLDB2,DLDUC1,DLDUC2,         
     $DLDPT1,DLDPT2,DLDQ,DLDI,DLDTZP,DLDTZS,VCLC,DLDECC,DLDOM
      DO 1 K=1,NRY                                                              
      ARA(K)=PHR(K+KST)
      ARC(K)=DLDI(K+KST)
      ARQ(K)=DLDECC(K+KST)
      ARR(K)=DLDOM(K+KST)
      ARD(K)=DLDPT1(K+KST)                                                      
      ARE(K)=DLDPT2(K+KST)                                                      
      ARF(K)=DLDQ(K+KST)                                                        
      ARG(K)=DLDA1(K+KST)
      ARH(K)=DLDA2(K+KST)                                                       
      ARI(K)=DLDB1(K+KST)                                                       
      ARJ(K)=DLDB2(K+KST)                                                       
      ARK(K)=DLDT1(K+KST)                                                       
      ARL(K)=DLDT2(K+KST)                                                       
      ARM(K)=DLDUC1(K+KST)                                                      
      ARN(K)=DLDUC2(K+KST)                                                      
      ARO(K)=DLDTZP(K+KST)                                                      
      ARP(K)=DLDTZS(K+KST)                                                      
    1 CONTINUE
      RETURN                                                                    
      END                                                                       
