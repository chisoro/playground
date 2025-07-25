session:DATA-ENTRY-RETURN = TRUE.
/* Program.................bfr{&tmptable}.p
   Notes:......            Receipt Operator Maintenance
   Author:.................S. Mawire
   Modified:..................S. Chisoro (encryption and audit trail 7/7/24)
*/
&SCOPED-DEFINE tmptable             dbRecop
&SCOPED-DEFINE skey                 dbRecop.usercode

{varlibrary.i}
DEF VAR varCode LIKE bfr{&tmptable}.usercode.
DEFINE VARIABLE binary-key LIKE bfr{&tmptable}.bKey.
 DEFINE VARIABLE crypto-value LIKE bfr{&tmptable}.password.
 DEFINE VARIABLE clear-text AS CHARACTER NO-UNDO.

DEFINE VARIABLE cIPAddresses AS CHARACTER              NO-UNDO.
/*DEFINE SHARED VARIABLE varUser AS CHARACTER.
DEFINE  SHARED VARIABLE lc-address AS CHAR NO-UNDO.
DEFINE  SHARED VARIABLE lc-host AS CHAR NO-UNDO.*/

DEF BUTTON btn-User LABEL "UserName".
/*DEF BUTTON btn-Cancel  LABEL "CANCEL".*/
DEF BUTTON btn-Reset  LABEL "RESET".
/*DEF BUTTON btn-Ok  LABEL "OK".*/
   
DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 75 by 8.5.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 75 by 2.5.

DEF    QUERY qry-user FOR simusr SCROLLING.
DEF BROWSE brw-user QUERY qry-user
    DISPLAY simusr.usercode simusr.Name simusr.txtJob
     WITH 20 DOWN SEPARATORS.
DEFINE FRAME frm-user 
    brw-user AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-Cancel colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "User Code Selection".


DEFINE NEW SHARED FRAME frmMain
    SKIP(1.0)
     btn-User NO-LABEL COLON 8 AUTO-RETURN
     simusr.NAME VIEW-AS TEXT NO-LABEL NO-TAB-STOP SKIP(0.5)
    bfr{&tmptable}.prn     COLON 20 LABEL "Receipt Printer" SKIP(0.5)
    bfr{&tmptable}.RecNo   COLON 20 LABEL "Receipt Number" SKIP(0.5)
    bfr{&tmptable}.EOD     COLON 20 LABEL "End-of-Day" SKIP(0.5)
    bfr{&tmptable}.txtPass COLON 20 LABEL "Password" PASSWORD-FIELD SKIP(1)  
    btn-ok       COLON 10 SPACE(20) btn-Reset SPACE(20) btn-Cancel
    rect-1 AT ROW 1 COL 5
    rect-2 AT ROW 9.7 COL 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         VIEW-AS DIALOG-BOX 
         SIZE 83 BY 12.9
         BGCOLOR 3 FGCOLOR 10 
         CANCEL-BUTTON Btn-cancel TITLE "RECEIPT OPERATOR FILE MAINTENANCE".

/**** Triggers ****/
ON CHOOSE OF btn-user IN FRAME frmMain
DO:
  varCode = "".
  VIEW FRAME frm-User.
  OPEN QUERY qry-User FOR EACH Simusr NO-LOCK.
  ENABLE ALL WITH FRAME frm-User.
  WAIT-FOR CHOOSE OF btn-cancel IN FRAME frm-User 
          OR close of THIS-PROCEDURE IN FRAME frm-User 
          OR CHOOSE OF btn-ok IN FRAME frm-User 
          OR 'enter':u OF brw-User
          OR 'mouse-select-dblclick' OF brw-User.
  CLOSE QUERY qry-User.
  HIDE FRAME frm-User.
  IF varCode <> "" THEN
      APPLY "tab" TO btn-user.
 RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-User 
    OR 'enter':u OF brw-User 
    OR 'mouse-select-dblclick' OF brw-User 
DO: 
   GET CURRENT qry-User  EXCLUSIVE-LOCK NO-WAIT.
    varCode = simusr.usercode.
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.usercode = varCode NO-ERROR.
    IF AVAILABLE bfr{&tmptable} THEN DO:
        DISPLAY simusr.NAME bfr{&tmptable}.prn bfr{&tmptable}.EOD bfr{&tmptable}.RecNo bfr{&tmptable}.txtPass
            WITH FRAME frmMain.
         btn-ok:LABEL = "Update".
         DISABLE bfr{&tmptable}.txtPass WITH FRAME frmMain.
    END.
    DISPLAY simusr.Name WITH FRAME frmMain.
    RETURN.
END.

ON CHOOSE OF btn-ok IN FRAME frmMain
DO:
   RUN simip.p.
   IF btn-ok:LABEL = "Update" THEN DO:
       wsBtn = "EDIT".
        clear-text = bfr{&tmptable}.txtPass:SCREEN-VALUE.
         binary-key = GENERATE-RANDOM-KEY.
       SECURITY-POLICY:SYMMETRIC-ENCRYPTION-KEY =binary-key.
       SECURITY-POLICY:SYMMETRIC-ENCRYPTION-IV = ?.
        crypto-value =ENCRYPT(clear-text).
       /*use siliot's program for update trail audit*/
       ASSIGN bfr{&tmptable}.EOD = LOGICAL(bfr{&tmptable}.EOD:SCREEN-VALUE)
              bfr{&tmptable}.prn = bfr{&tmptable}.prn:SCREEN-VALUE 
              bfr{&tmptable}.RecNo = INT( bfr{&tmptable}.RecNo:SCREEN-VALUE)
             /* bfr{&tmptable}.txtPass = bfr{&tmptable}.txtPass:SCREEN-VALUE*/
              bfr{&tmptable}.bKey=binary-key
              bfr{&tmptable}.password    =  crypto-value.
       FIND FIRST dbrecctr WHERE dbRecCtr.usercode = bfr{&tmptable}.usercode AND dbRecCtr.RecDate = bfr{&tmptable}.RecDate NO-ERROR.
       IF AVAILABLE dbrecctr THEN
            dbRecCtr.EOD = bfr{&tmptable}.EOD.
       RELEASE bfr{&tmptable}.
       HIDE FRAME frmMain.
       APPLY 'close' TO THIS-PROCEDURE.
   END.
   ELSE DO:
       wsBtn = "ADD".
       CREATE bfr{&tmptable}.
       clear-text = bfr{&tmptable}.txtPass:SCREEN-VALUE.
        binary-key = GENERATE-RANDOM-KEY.
       SECURITY-POLICY:SYMMETRIC-ENCRYPTION-KEY =binary-key.
       SECURITY-POLICY:SYMMETRIC-ENCRYPTION-IV = ?.
       crypto-value =ENCRYPT(clear-text).
       ASSIGN bfr{&tmptable}.usercode = varCode
              bfr{&tmptable}.EOD = LOGICAL(bfr{&tmptable}.EOD:SCREEN-VALUE)
              bfr{&tmptable}.prn = bfr{&tmptable}.prn:SCREEN-VALUE 
              bfr{&tmptable}.RecNo = INT( bfr{&tmptable}.RecNo:SCREEN-VALUE)
              bfr{&tmptable}.bKey=binary-key
              bfr{&tmptable}.password    =  crypto-value
              bfr{&tmptable}.DeviceName = lc-host
              bfr{&tmptable}.UID = varUser
              bfr{&tmptable}.creDate =NOW.   
       RELEASE bfr{&tmptable}.
       HIDE FRAME frmMain.
       APPLY 'close' TO THIS-PROCEDURE.
   END.
END.

ON CHOOSE OF btn-reset IN FRAME frmMain
DO: 
       ENABLE bfr{&tmptable}.txtPass WITH FRAME frmMain.
       APPLY 'entry' TO bfr{&tmptable}.txtPass.
END.

{audit.i}
/********** MAIN LOGIC **********/
VIEW FRAME frmMain. 
ENABLE ALL  WITH FRAME frmMain.
CLEAR FRAME frmMain ALL.
WAIT-FOR CHOOSE OF btn-cancel OR close of THIS-PROCEDURE IN FRAME frmMain.
HIDE FRAME frmMain.
RETURN.
