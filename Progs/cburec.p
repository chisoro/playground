/* Program.................CBuRec.p
   Notes:................. Undo Bank Reconciliation
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
DEF BUFFER bfrCBTrans FOR CBTrans.
DEF BUFFER bfrCBkmf FOR CBkmf.

DEF STREAM a.
DEF  VAR w-orientation AS CHAR      INITIAL "Portrait"    NO-UNDO.
DEF  VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF  VAR w-fontsize    AS INT       INITIAL 8.0          NO-UNDO.
DEFINE VARIABLE method-return AS LOGICAL NO-UNDO.
DEF VAR wsBal LIKE cbkmf.StatBal.
DEF  VAR wsper AS INT.
DEF VAR wsTitle AS CHAR FORM "x(65)".
DEF VAR wsTitle1 AS CHAR FORM "x(65)".
DEFINE BUTTON btn-ok LABEL "UN-RECONCILE".
DEFINE BUTTON btn-exit LABEL "CLOSE".
DEF BUTTON btnBank  LABEL "Bank".

DEF RECT rect-1 SIZE 93 BY 3.8.
DEF RECT rect-2 SIZE 93 BY 2.0.

DEF    QUERY qry-Pick FOR bfrcbkmf SCROLLING.
DEF BROWSE brw-Pick QUERY qry-Pick
    DISPLAY bfrcbkmf.bank bfrcbkmf.descrip WIDTH 40 WITH 20 DOWN SEPARATORS.

 DEFINE FRAME frm-Pick 
    brw-Pick AT ROW 1 COL 1.5
    skip(0.5)
    btn-Ok COLON 8 SPACE (20)
    btn-exit 
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Bank Selection".

DEFINE FRAME f1
     SKIP(1.5)
    btnBank COLON 20 NO-TAB-STOP bfrcbkmf.bank VIEW-AS FILL-IN NO-LABEL
    bfrcbkmf.descrip VIEW-AS TEXT NO-LABEL SKIP(0.5)
    bfrcbkmf.StatNo  COLON 30 VIEW-AS TEXT LABEL "Last Statement Number"
    bfrcbkmf.StatBal COLON 70 VIEW-AS TEXT LABEL "Statement Balance" 
    SKIP(1.5)
    btn-ok COLON 20 SPACE(40) btn-exit
    rect-1 AT ROW 2 COLUMN 3
    rect-2 AT ROW 6 COLUMN 3
    WITH SIZE 99 BY 8.5 SIDE-LABEL
     TITLE "UNDO BANK RECONCILIATION" VIEW-AS DIALOG-BOX.

ON CHOOSE OF btnBank IN FRAME f1
DO:
  VIEW FRAME frm-pick.
  OPEN QUERY qry-pick FOR EACH bfrcbkmf WHERE bfrcbkmf.bank <> 0 NO-LOCK.
  ENABLE ALL WITH FRAME frm-pick.
  WAIT-FOR CHOOSE OF btn-exit IN FRAME frm-pick 
          OR close of THIS-PROCEDURE IN FRAME frm-pick
          OR CHOOSE OF btn-ok IN FRAME frm-pick 
          OR 'enter':u OF brw-pick
          OR 'mouse-select-dblclick' OF brw-pick.
  CLOSE QUERY qry-pick.
  HIDE FRAME frm-pick.
  APPLY 'tab' TO btnBank.
  APPLY 'tab' TO bfrcbkmf.bank.
  RETURN. 
END. 

ON CHOOSE OF btn-ok IN FRAME frm-pick 
    OR 'enter':u OF brw-pick
    OR 'mouse-select-dblclick' OF brw-pick
DO: 
   GET CURRENT qry-pick EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY bfrcbkmf.bank bfrcbkmf.DESCRIP  WITH FRAME f1.
   APPLY 'TAB' TO bfrcbkmf.bank IN FRAME f1.
END.

ON 'enter':U OF bfrcbkmf.bank IN FRAME f1
    OR 'TAB' OF bfrcbkmf.bank IN FRAME f1
DO:
    FIND FIRST bfrcbkmf WHERE bfrcbkmf.bank = INT(bfrcbkmf.bank:SCREEN-VALUE)
         EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE bfrcbkmf THEN DO:
        IF LOCKED  bfrcbkmf THEN MESSAGE "Record LOCKED" VIEW-AS ALERT-BOX.
        DISPLAY bfrcbkmf.descrip bfrcbkmf.StatBal bfrcbkmf.StatNo  WITH FRAME f1.
        IF bfrcbkmf.StatNo = 0 THEN
            DISABLE btn-ok WITH FRAME F1.
        APPLY 'tab' TO SELF.
    END.
    RETURN.
END.

ON CHOOSE OF btn-ok IN FRAME f1
DO:
 DO TRANSACTION ON ERROR UNDO, LEAVE:
    FOR EACH bfrCBTrans WHERE bfrCBTrans.bank = INT(bfrcbkmf.bank:SCREEN-VALUE)
                          AND bfrCBTrans.Recon = "U" AND bfrCBTrans.StatNo = bfrcbkmf.StatNo:
        ASSIGN bfrcbkmf.StatBal = bfrcbkmf.StatBal - bfrCBTrans.amount
               bfrCBTrans.Recon = "Y" 
               bfrCBTrans.StatNo = 0.
    END.
    FIND FIRST cbrec WHERE cbrec.bank = INT(bfrcbkmf.bank:SCREEN-VALUE) AND cbrec.StatNo = bfrcbkmf.StatNo NO-ERROR.
    IF AVAILABLE cbrec THEN
        DELETE cbrec.
    bfrcbkmf.StatNo = bfrcbkmf.StatNo - 1.
    APPLY 'CLOSE' TO THIS-PROCEDURE.
 END.
END.

/********** MAIN LOGIC **********/
ENABLE ALL WITH FRAME f1.
WAIT-FOR CHOOSE OF btn-exit OR CLOSE OF THIS-PROCEDURE.
RELEASE bfrCBKmf.
RELEASE bfrCBTrans.
HIDE FRAME f1.

