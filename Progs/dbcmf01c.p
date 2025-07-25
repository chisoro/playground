/* Program.................dbcmf01c.p.p
   Notes:................. Rename/transfer Account to another account code
   Author:.................S. Mawire
*/

session:DATA-ENTRY-RETURN = TRUE.

DEF VAR wsnew     LIKE dbcmf.dbacc.   
DEF VAR wsold     LIKE dbcmf.dbacc.   
DEF VAR wsStatus  LIKE dbcmf.dbacc.
DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-ok     LABEL "UPDATE".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 18.5.

DEF FRAME frm-main
    SKIP(1.5)
    wsnew     LABEL "Enter New Account" COLON 30 SKIP(0.5)
    wsold     LABEL "Enter Old Account" COLON 30 SKIP(1)
    dbcmf.Name LABEL "Account Name" COLON 30 SKIP(0.5)
    dbcmf.Add1 LABEL "Address"      COLON 30 SKIP
    dbcmf.add2 NO-LABEL  COLON 30 SKIP
    dbcmf.Add3 NO-LABEL  COLON 30 SKIP
    dbcmf.Bal    LABEL "Account Balance" COLON 30 SKIP(0.5)
    SKIP(1.0)
    wsStatus    COLON 30 LABEL " Processing Account......." VIEW-AS TEXT
    btn-ok AT ROW 20.7 COL 10 space(50) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 24 KEEP-TAB-ORDER
    TITLE "DEBTORS ACCOUNT MIGRATION" VIEW-AS DIALOG-BOX.


ON 'enter':U OF wsnew IN FRAME frm-Main 
DO:
    ASSIGN wsnew.
    FIND FIRST dbcmf WHERE dbcmf.dbacc = wsnew NO-LOCK NO-ERROR.
    IF AVAILABLE dbcmf THEN DO:
        MESSAGE "Account already exist ...pleas try again" VIEW-AS ALERT-BOX.
        wsnew:SCREEN-VALUE = "0" .
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'enter':U OF wsold IN FRAME frm-Main 
DO:
    ASSIGN wsold.
    FIND FIRST dbcmf WHERE dbcmf.dbacc = wsold NO-ERROR.
    IF NOT AVAILABLE dbcmf THEN DO:
        MESSAGE "Account does not exist ...pleas try again" VIEW-AS ALERT-BOX.
        wsold:SCREEN-VALUE = "0".
        RETURN NO-APPLY.
    END.
    ELSE DISPLAY dbcmf.NAME dbcmf.Add1 dbcmf.add2 dbcmf.Add3 dbcmf.bal
        WITH FRAME frm-main.
        RETURN.                             
END.

ON 'choose':U OF btn-ok IN FRAME frm-Main
DO:
   FOR EACH dbblf WHERE dbblf.dbacc = wsold:
       DISPLAY dbblf.dbacc @ wsStatus WITH FRAME frm-main.
       PAUSE 0.
       dbblf.dbacc = wsnew.
   END.
   FOR EACH dbmtf WHERE dbmtf.dbacc = wsold:
       DISPLAY dbmtf.dbacc @ wsStatus WITH FRAME frm-main.
       PAUSE 0.
       dbmtf.dbacc = wsnew.
   END.
   FOR EACH dbmmf WHERE dbmmf.dbacc = wsold:
       DISPLAY dbmmf.dbacc @ wsStatus WITH FRAME frm-main.
       PAUSE 0.
       dbmmf.dbacc = wsnew.
   END.
   FOR EACH dbbatch WHERE dbbatch.dbacc = wsold:
       DISPLAY dbbatch.dbacc @ wsStatus WITH FRAME frm-main.
       PAUSE 0.
       dbbatch.dbacc = wsnew.
   END.
   FOR EACH dbhtf WHERE dbhtf.dbacc = wsold:
       DISPLAY dbhtf.dbacc @ wsStatus WITH FRAME frm-main.
       PAUSE 0.
       dbhtf.dbacc = wsnew.
   END.
   dbcmf.dbacc = wsnew.
   MESSAGE "Account migration completed....." VIEW-AS ALERT-BOX.
    APPLY 'entry' TO btn-exit IN FRAME frm-main.
    RETURN.
END.

/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
ENABLE wsnew wsOld btn-ok btn-exit WITH FRAME frm-main.
ASSIGN wsOld:SCREEN-VALUE = "0"
       wsNew:SCREEN-VALUE = "0".
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.
