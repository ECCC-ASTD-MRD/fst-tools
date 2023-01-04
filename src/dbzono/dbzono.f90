      PROGRAM DBZONO
      use app
      IMPLICIT NONE
!                
!AUTEUR  G. PELLERIN - Janvier 93 - deballeur des diagnostics zonaux
!     
!REVISION
!         001 B.Dugas     (mar06) Declarations pour FSTD2000 (v3.3)
!         002 K.Winger    (nov06) Support de variables !4    (v3.4)      003 D.Bouhemhem (nov12) Chargement avec rmn_013    (v3.5)
!         004 M.Lepine    (mar14) Chargement avec rmn_014    (v3.6)
!         005 M.Lepine    (dec14) Chargement avec rmn_015.1  (v3.7)
!         006 M.Lepine    (fev15) Chargement avec rmn_015.2  (v3.8)
!
!FICHIERS        
!          - TAPE36  ENTREE FICHIER DIAGNOSTICS ZONAUX               
!          - TAPE37  SORTIE FICHIER PAR VARIABLES EXTRAITES           
!                                                                                                                                      
!MODULES                                                            
      EXTERNAL CCARD                                               
      EXTERNAL FCLOS                                             
      EXTERNAL INCTPHY                                         
      EXTERNAL LITZON                                         
      INTEGER  EXDB,EXFIN,FNOM                                
      EXTERNAL EXDB,EXFIN,FNOM                              
!                                                          
#include "ctescon.cdk"
#include "fst-tools_build_info.h"
!
      INTEGER STATZON
      CHARACTER *128, DEFO(6),LISTL(6),LFN(6) 
     
      LOGICAL ECHOZ 
      INTEGER IUNZON,ZONSTD                 
      INTEGER JUNK,IER
      SAVE LISTL,DEFO,LFN                  
      SAVE IUNZON, ZONSTD, STATZON    
      DATA IUNZON, ZONSTD, STATZON / 36 , 37 ,0 /
      DATA LISTL/'ISZONAL','OMZONAL','I','L','DATE','ECHOZ'/    
      DATA DEFO/'TAPE36','TAPE37','$IN','$OUT','OPRUN','OUI'/  
      DATA LFN/'TAPE36','TAPE37','$IN','$OUT','NON','NON'/
!                                                        
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!                                                                    
      CALL CCARD ( LISTL , DEFO , LFN , 6 , -1 )                    
      IER = FNOM ( IUNZON , LFN(1) , 'STD+RND' , 0 )               
      IER = FNOM ( ZONSTD , LFN(2) , 'STD+RND' , 0 )              
      IER = FNOM ( 5 , LFN(3) , 'SEQ' , 0 )                      
      IER = FNOM ( 6 , LFN(4) , 'SEQ' , 0 )                     
      ECHOZ = LFN(6).EQ.'OUI'                                  
!                                                             
      app_ptr=app_init(0,'DBZONO',VERSION,'',BUILD_TIMESTAMP)
      call app_start()
!                                                                                                           
!     INITIALISATION DU COMMON 'CTESCON'                       
!                                                            
      CALL INCTCON                               
!                                               
      PRINT *,' '                                    
      WRITE ( 6 , * ) '----DIAGNOSTICS ZONAUX----'  

      CALL LITZON (IUNZON,ZONSTD,STATZON,ECHOZ) 

      IF (statzon .lt.0 ) THEN       
!                                                                     
!    DETECTER CAS DE FICHIER ZONAL VIDE (PAS UNE ERREUR NECESSAIREMENT) 
!
            WRITE(app_msg,*) 'FICHIER ZONAL VIDE'                   
            call app_log(APP_ERROR,app_msg)
      ELSE                 
         CALL FCLOS(IUNZON)  
      ENDIF   

      app_status=app_end(-1)
      call qqexit(app_status)
      END   
      
