session:DATA-ENTRY-RETURN = TRUE.
/* Program.................dbCapvartf.p
   Notes:................. Capture Variable Tarrifs
   Author:.................S. Chisoro
*/

DEF VAR vaRAcc LIKE dbcmf.dbacc.
DEF VAR varTar LIKE dbtmf.Tarif.
DEF VAR wsTar LIKE dbtmf.Tarif.
DEF VAR varAmount LIKE dbcmf.accbal.
DEF VAR varDescrip AS CHAR FORM "X(15)".
DEF BUTTON   btn-ok LABEL "Save".
DEF BUTTON btn-Exit LABEL "Exit".
DEF VAR X AS INT.
DEF VAR j AS INT.



DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 77 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 77 by 6.5.



DEF FRAME frm-main
    SKIP(1)
    varAcc       LABEL "Consumer Account" COLON 20 SPACE (5)
    varDescrip NO-LABEL VIEW-AS TEXT SKIP(0.2)
    varTar     LABEL "Tarif" COLON 20 SKIP(0.2)
    varAmount       LABEL "Amount"  COLON 20 SKIP(0.2)
    btn-ok AT ROW 8.7 COL 10
    space(50) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 8 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 82 BY 11.5 KEEP-TAB-ORDER
    TITLE "VARIABLE TARIF CAPTURE" VIEW-AS DIALOG-BOX.   



      
/* ******** Triggers  ***********/
ON 'enter':U OF varAcc IN FRAME frm-main
    OR 'tab':U OF varAcc IN FRAME frm-main 
    
DO:
    FIND FIRST dbcmf WHERE dbAcc = dec(varAcc:SCREEN-VALUE IN FRAME frm-main) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE dbcmf THEN DO:
         DISABLE varAmount WITH FRAME frm-main.
        MESSAGE "Invalid account! Please enter valid Account." VIEW-AS ALERT-BOX.
        CLEAR FRAME frm-main ALL.
        APPLY 'entry' TO varAcc.
        RETURN NO-APPLY.
    END.
    ELSE IF AVAILABLE dbcmf THEN DO:
        varDescrip = dbcmf.NAME.
        DISPLAY varDescrip WITH FRAME frm-main.
       APPLY 'entry' TO varTar.
    END.
    RETURN.
 END.

ON 'enter':U OF varTar IN FRAME frm-main
    OR 'tab':U OF varTar IN FRAME frm-main 
    
DO:
   FIND FIRST dbcmf WHERE dbAcc = dec(varAcc:SCREEN-VALUE IN FRAME frm-main) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE dbcmf THEN DO:
         DISABLE varAmount WITH FRAME frm-main.
        MESSAGE "Invalid account! Please enter valid Account." VIEW-AS ALERT-BOX.
        CLEAR FRAME frm-main ALL.
        APPLY 'entry' TO varAcc.
        RETURN NO-APPLY.
    END.
    ELSE IF AVAILABLE dbcmf THEN DO:
        varDescrip = dbcmf.NAME.
        DISPLAY varDescrip WITH FRAME frm-main.
        wsTar = 0.
       DO X = 1 TO 14:
            IF tarif[X] = int(varTar:SCREEN-VALUE) THEN DO:
                    wsTar = tarif[X].
                    X = 14.
            END.
        END.
        IF wsTar <> 0 THEN DO:
            FIND FIRST dbtmf WHERE dbtmf.tarif  = wsTar AND dbtmf.vcharge = YES NO-LOCK NO-ERROR.
            IF NOT AVAILABLE dbtmf THEN DO:
                 DISABLE varAmount WITH FRAME frm-main.
                MESSAGE "Tarif Does Not Accept Variable Charge."  VIEW-AS ALERT-BOX.
                ASSIGN
                  varTar:SCREEN-VALUE = "0".
                APPLY  'entry' TO varTar.
                RETURN NO-APPLY.
            END.
            ELSE IF AVAILABLE dbtmf THEN DO:
                 FIND FIRST dbvTar WHERE dbvTar.dbAcc =  dec(varAcc:SCREEN-VALUE in FRAME frm-main) AND  dbvTar.Tarif = dec(varTar:SCREEN-VALUE in FRAME frm-main) AND  dbvTar.type  = dbtmf.TYPE NO-ERROR.
                 IF AVAILABLE dbvTar THEN DO:
                     ASSIGN varAmount:SCREEN-VALUE IN FRAME frm-Main = STRING(dbvTar.Charge).
                 END.
                 ELSE DO:
                     CREATE dbvTar.
                     ASSIGN
                         dbvTar.dbAcc = dec(varAcc:SCREEN-VALUE in FRAME frm-main)
                         dbvTar.Tarif = dec(varTar:SCREEN-VALUE in FRAME frm-main)
                         dbvTar.type  = dbtmf.TYPE
                         dbvTar.sgrp = dbtmf.sgrp.
                 END.
                ENABLE varAmount WITH FRAME frm-main.
                APPLY  'entry' TO varAmount.
            END.
        END.
        ELSE DO:
            DISABLE varAmount WITH FRAME frm-main.
            MESSAGE "Account not setup for this tarif." VIEW-AS ALERT-BOX.
             ASSIGN
                  varTar:SCREEN-VALUE = "0".
            APPLY  'entry' TO varTar.
            RETURN NO-APPLY.
        END.
    END.
    
RETURN.
END.

ON 'enter':U OF varAmount IN FRAME frm-main
    OR 'tab':U OF varAmount IN FRAME frm-main 
DO:
    FIND FIRST dbtmf  WHERE dbtmf.tarif  = wsTar AND dbtmf.vcharge = YES NO-LOCK NO-ERROR.
    IF AVAILABLE dbtmf THEN DO:
        FIND FIRST dbvTar WHERE dbvTar.dbAcc =  dec(varAcc:SCREEN-VALUE in FRAME frm-main) AND  dbvTar.Tarif = dec(varTar:SCREEN-VALUE in FRAME frm-main) AND  dbvTar.type  = dbtmf.TYPE EXCLUSIVE-LOCK.
        IF AVAILABLE dbvTar THEN DO:
            ASSIGN dbvTar.Charge = dec(varAmount:SCREEN-VALUE in FRAME frm-main).
        END.
        ELSE IF NOT AVAILABLE dbvTar THEN DO:
            CREATE dbvTar.
            ASSIGN
               dbvTar.Charge = dec(varAmount:SCREEN-VALUE in FRAME frm-main)
                dbvTar.dbAcc =  dec(varAcc:SCREEN-VALUE in FRAME frm-main)
                dbvTar.Sgrp = dbtmf.sgrp
                dbvTar.Tarif = dec(varTar:SCREEN-VALUE in FRAME frm-main)
                dbvTar.type  = dbtmf.TYPE.
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

 
