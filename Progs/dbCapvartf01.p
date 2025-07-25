session:DATA-ENTRY-RETURN = TRUE.
/* Program.................dbCapvartf01.p
   Notes:................. Capture Variable Tarrifs
   Author:.................S. Chisoro
*/

DEF VAR vaRAcc LIKE dbcmf.dbacc.
DEF VAR vaRAcc1 LIKE dbcmf.dbacc.
DEF VAR varTar LIKE dbtmf.Tarif.
DEF VAR wsTar LIKE dbtmf.Tarif.
DEF VAR wsTamt LIKE dbtmf.charge.
DEF VAR varAmount LIKE dbcmf.accbal.
DEF BUTTON   btn-ok LABEL "Save".
DEF BUTTON btn-Exit LABEL "Exit".
DEF VAR X AS INT.
DEF VAR j AS INT.
DEF VAR wsStatus AS CHAR.



DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 77 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 77 by 6.5.



DEF FRAME frm-main
    SKIP(1)
    varAcc       LABEL "Start Account" COLON 20  SKIP(0.2)
   varAcc1       LABEL "End Account" COLON 20 SKIP(0.2)
   varTar     LABEL "Tarif" COLON 20 SPACE(5)
    wsTamt NO-LABEL VIEW-AS TEXT SKIP(0.2)
    varAmount       LABEL "New Amount"  COLON 20 SKIP(0.2)
    btn-ok AT ROW 8.7 COL 10
    space(50) btn-exit SKIP(1)
    wsStatus    COLON 30 LABEL " Processing Account......." VIEW-AS TEXT
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 8 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 82 BY 12.5 KEEP-TAB-ORDER
    TITLE "VARIABLE TARIF CHARGE CAPTURE" VIEW-AS DIALOG-BOX.   



      
/* ******** Triggers  ***********/
ON 'enter':U OF varAcc IN FRAME frm-main
    OR 'tab':U OF varAcc IN FRAME frm-main 
    
DO:
   
       APPLY 'entry' TO varTar.
    RETURN.
 END.

ON 'enter':U OF varTar IN FRAME frm-main
    OR 'tab':U OF varTar IN FRAME frm-main 
    
DO:
    
  FIND FIRST dbtmf WHERE dbtmf.tarif  = int(varTar:SCREEN-VALUE IN FRAME frm-main) AND dbtmf.vcharge = YES NO-LOCK NO-ERROR.
            IF NOT AVAILABLE dbtmf THEN DO:
                 DISABLE varAmount WITH FRAME frm-main.
                MESSAGE "Tarif Does Not Accept Variable Charge."  VIEW-AS ALERT-BOX.
                ASSIGN
                  varTar:SCREEN-VALUE = "0".
                APPLY  'entry' TO varTar.
                RETURN NO-APPLY.
            END.
            ELSE IF AVAILABLE dbtmf THEN DO:
                /*FIND FIRST dbVtar WHERE dbVtar.tarif = dbtmf.tarif NO-LOCK NO-ERROR.
                IF AVAILABLE dbvTar THEN DO:
                    wsTamt:SCREEN-VALUE = string(dbvTar.charge).*/
                    ENABLE varAmount WITH FRAME frm-main.
                    APPLY  'entry' TO varAmount.
                /*END.*/
                
            END.
   RETURN.
END.

ON 'enter':U OF varAmount IN FRAME frm-main
    OR 'tab':U OF varAmount IN FRAME frm-main 
DO:
    wsTar = DEC(varTar:SCREEN-VALUE in FRAME frm-main).
    FIND FIRST dbtmf  WHERE dbtmf.tarif  = wsTar AND dbtmf.vcharge = YES NO-LOCK NO-ERROR.
    IF AVAILABLE dbtmf THEN DO:
       FOR EACH dbcmf WHERE dbAcc >= DEC(varAcc:SCREEN-VALUE in FRAME frm-main) AND dbAcc <= DEC(varAcc1:SCREEN-VALUE in FRAME frm-main) NO-LOCK:
                            wsStatus:SCREEN-VALUE in FRAME frm-main = string(dbcmf.dbacc).
                            FOR EACH dbvTar WHERE dbvTar.dbAcc =  dbcmf.dbAcc AND  dbvTar.Tarif = dec(varTar:SCREEN-VALUE in FRAME frm-main) 
                                AND  dbvTar.type  = dbtmf.TYPE /*AND dbvtar.charge = wstAmt*/ EXCLUSIVE-LOCK:
                                ASSIGN dbvTar.Charge = dec(varAmount:SCREEN-VALUE in FRAME frm-main).
                            END.
           
                            
          END.


           MESSAGE "Update successful" VIEW-AS ALERT-BOX.
    END.
    CLEAR FRAME frm-main ALL.
     DISABLE varAmount WITH FRAME frm-main.
    APPLY 'entry' TO varAcc.
    RETURN.
END.

ON 'choose':U OF btn-ok IN FRAME frm-main 
DO:
    APPLY 'enter' TO varAmount.
   RETURN.
END.

/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
ENABLE ALL EXCEPT varAmount WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.

 
