session:DATA-ENTRY-RETURN = TRUE.
/* Program.................dbsub.p
   Notes:...... usercode file maintenance
   Author:.................S. Mawire
   Edited:..................S, Chisoro (Audit 06/07/24)
*/
&SCOPED-DEFINE wsMsg             "User already exist"
&SCOPED-DEFINE wsTitle           "User Maintenance"
&SCOPED-DEFINE tmptable            simusr
&SCOPED-DEFINE skey                 simusr.usercode
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.Name ~
                                        COLUMN-LABEL ' Name ':C
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.usercode ~
                                        COLUMN-LABEL 'User Code ':C ~
                              bfr{&tmptable}.Name ~
                                        COLUMN-LABEL ' Name ':C ~
                              bfr{&tmptable}.txtJob ~
                                        COLUMN-LABEL ' Designation ':C ~
                              bfr{&tmptable}.txtDept ~
                                        COLUMN-LABEL ' Department ':C ~
                               bfr{&tmptable}.txtStatus ~
                                        COLUMN-LABEL 'Status ':C

 DEFINE VARIABLE binary-key LIKE simusr.bKey.
 DEFINE VARIABLE crypto-value LIKE simusr.password.
 DEFINE VARIABLE clear-text AS CHARACTER NO-UNDO.

DEFINE VARIABLE cIPAddresses AS CHARACTER              NO-UNDO.
/*DEFINE SHARED VARIABLE varUser AS CHARACTER.
DEFINE  SHARED VARIABLE lc-address AS CHAR NO-UNDO.
DEFINE  SHARED VARIABLE lc-host AS CHAR NO-UNDO.*/

{varlib.i}

DEFINE RECTANGLE RECT-u
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 2.67.

DEFINE RECTANGLE RECT-l
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 8.67.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 2.67.

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 18.5.

define frame frm-input
    SKIP(2)
     {&skey}                AT ROW 2.62 COL 20 COLON-ALIGNED label "User Code" WIDGET-ID 2
     bfr{&tmptable}.NAME    AT ROW 5.2 COL 20 COLON-ALIGNED  label "Name" WIDGET-ID 30
     bfr{&tmptable}.txtJob  AT ROW 6.4 COL 20 COLON-ALIGNED  label "Designation"
     bfr{&tmptable}.txtDept AT ROW 7.6 COL 20 COLON-ALIGNED  label "Department"
     bfr{&tmptable}.iPhone  AT ROW 8.8 COL 20 COLON-ALIGNED  label "Phone/Cell"
     bfr{&tmptable}.txtStatus AT ROW 10.0 COL 20 COLON-ALIGNED  label "STATUS" AUTO-RETURN
    VIEW-AS COMBO-BOX LIST-ITEM-PAIRS "A - ACTIVE Account","A","L - LOCKED Account","L"
    bfr{&tmptable}.Paysys  AT ROW 11.2 COL 25 COLON-ALIGNED LABEL "Access Which Payroll" AUTO-RETURN
    bfr{&tmptable}.supervisor LABEL "Receipting Supervisor?" AUTO-RETURN
     btn-Ok AT ROW 13.71 COL 10 WIDGET-ID 48
     btnReset AT ROW 13.71 COL 30 WIDGET-ID 48
     btn-Close AT ROW 13.71 COL 60 WIDGET-ID 46
     "Login Details" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 1.48 COL 8 WIDGET-ID 40
          FONT 6
    "User Details" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 4.38 COL 9 WIDGET-ID 42
          FONT 6
     RECT-u AT ROW 1.81 COL 6 WIDGET-ID 34
     RECT-l AT ROW 4.62 COL 6 WIDGET-ID 38
     Rect-3 AT ROW 13.0 COL 6
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED
         /*AT COL 2.2 ROW 1.19 */
         SIZE 80.8 BY 16.1
         BGCOLOR 11 FGCOLOR 1  WIDGET-ID 100
         TITLE "User Maintenance".
 
DEF NEW SHARED FRAME frm-main
    brw-{&tmptable} AT ROW 1.8 COL 5
    btn-add AT ROW 20.7 COL 5
    Space(20) btn-edit
    space(20) btn-del
    space(20) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     /*BGCOLOR 8 FGCOLOR 1 */ SIZE 100 BY 24
    TITLE {&wsTitle} VIEW-AS DIALOG-BOX.
/* end of Common Variables */
/* ***** Triggers for the main frame **** */
ON LEAVE OF {&skey} IN FRAME frm-input
DO: 
     wsid = {&skey}:SCREEN-VALUE IN FRAME frm-input.
    RUN validate-ip.
END.

ON 'start-search':u OF BROWSE brw-{&tmptable}
    run SEARCH.ip.

ON CHOOSE OF btn-Add IN FRAME frm-main DO:
     btn-ok:LABEL IN  FRAME frm-input = "Save".
    RUN proc-input.
    RETURN.
END.

ON CHOOSE OF btn-edit
    OR 'enter':u OF brw-{&tmptable}
    OR 'mouse-select-dblclick' OF brw-{&tmptable}
DO:
    btn-ok:LABEL IN  FRAME frm-input = "Update".
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    DISABLE {&skey} WITH FRAME frm-input.
    run proc-edit.
    RETURN.
END.

ON CHOOSE OF btn-Del DO:
    /*i think user cannot be deleted, we can only make the use ianactive for audit purposes*/
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.usercode.
    DELETE bfr{&tmptable}.
     Method-Status = brw-{&tmptable}:DELETE-SELECTED-ROWS().
    APPLY 'entry' TO btn-exit.
END.

ON CHOOSE OF btnReset DO:
    MESSAGE "Are you sure you want to Reset password?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
             TITLE "Password Reset?" UPDATE w_response AS LOGICAL.
    IF w_response THEN DO:
        wsBtn = "EDIT".
        
        binary-key = GENERATE-RANDOM-KEY.
       SECURITY-POLICY:SYMMETRIC-ENCRYPTION-KEY =binary-key.
       SECURITY-POLICY:SYMMETRIC-ENCRYPTION-IV = ?.
       clear-text = "sim".
       crypto-value =ENCRYPT(clear-text).
       bfr{&tmptable}.bKey=binary-key.
       bfr{&tmptable}.password    =  crypto-value.
          APPLY "CLOSE":U TO THIS-PROCEDURE.
    END.   
    ELSE
      RETURN NO-APPLY.
END.

ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
    IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN DO:
         RUN simip.p.
         binary-key = GENERATE-RANDOM-KEY.
       SECURITY-POLICY:SYMMETRIC-ENCRYPTION-KEY =binary-key.
       SECURITY-POLICY:SYMMETRIC-ENCRYPTION-IV = ?.
       clear-text = "sim".
       crypto-value =ENCRYPT(clear-text).
       wsBtn = "ADD".
        CREATE bfr{&tmptable}. 
          ASSIGN bfr{&tmptable}.usercode = wsid
                 bfr{&tmptable}.NAME = bfr{&tmptable}.NAME:SCREEN-VALUE
                 bfr{&tmptable}.txtJob = bfr{&tmptable}.txtJob:SCREEN-VALUE
                 bfr{&tmptable}.txtDept = bfr{&tmptable}.txtDept:SCREEN-VALUE
                 bfr{&tmptable}.iPhone = INT64(BFR{&tmptable}.iphone:SCREEN-VALUE)
                 bfr{&tmptable}.txtStatus = bfr{&tmptable}.txtStatus:SCREEN-VALUE
                 bfr{&tmptable}.Paysys    = INT(bfr{&tmptable}.Paysys:SCREEN-VALUE)
                 bfr{&tmptable}.bKey=binary-key
                 bfr{&tmptable}.password    =  crypto-value
                 bfr{&tmptable}.DeviceName = lc-host
                 bfr{&tmptable}.UID = varUser
                 bfr{&tmptable}.creDate =NOW
                 bfr{&tmptable}.supervisor = LOGICAL(bfr{&tmptable}.supervisor:SCREEN-VALUE).
          RELEASE {&tmptable}.
          brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
          CLEAR FRAME frm-input ALL.
          APPLY 'entry' TO {&skey} IN FRAME frm-Input.
    END.
    ELSE DO:
        /*need to use Siliot's program*/
        wsBtn = "EDIT".
        ASSIGN bfr{&tmptable}.name = bfr{&tmptable}.name:SCREEN-VALUE 
               bfr{&tmptable}.txtJob = bfr{&tmptable}.txtJob:SCREEN-VALUE
               bfr{&tmptable}.txtDept = bfr{&tmptable}.txtDept:SCREEN-VALUE
               bfr{&tmptable}.iPhone =  INT64(BFR{&tmptable}.iphone:SCREEN-VALUE)
               bfr{&tmptable}.txtStatus = bfr{&tmptable}.txtStatus:SCREEN-VALUE 
               bfr{&tmptable}.Paysys    = INT(bfr{&tmptable}.Paysys:SCREEN-VALUE)
               bfr{&tmptable}.supervisor = LOGICAL(bfr{&tmptable}.supervisor:SCREEN-VALUE).
        RELEASE {&tmptable}.
        APPLY 'close' TO SELF.
        HIDE FRAME frm-input.
        brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
    END. 
END.
{audit.i}
/********** MAIN LOGIC **********/
OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK .
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
CLOSE QUERY qry-{&tmptable}.
HIDE FRAME frm-main.

PROCEDURE proc-input:
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   CLEAR FRAME frm-input ALL.
   ENABLE ALL EXCEPT btnReset WITH FRAME frm-input.
   ASSIGN {&skey}:SCREEN-VALUE = "0"
    bfr{&tmptable}.txtStatus:SCREEN-VALUE ="A".
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
         CLOSE QUERY qry-{&tmptable}.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   HIDE FRAME frm-input.
END.

PROCEDURE Validate-ip:
    IF wsid = "0" THEN DO:
        MESSAGE "No value entered" VIEW-AS ALERT-BOX.
        CLEAR FRAME frm-input ALL.
        APPLY 'CLOSE'  TO THIS-PROCEDURE IN FRAME frm-input.
    END.
    ELSE DO:
       FIND FIRST {&tmptable} WHERE  {&skey} = wsid NO-ERROR. 
       IF AVAILABLE {&tmptable} THEN
       DO:
          MESSAGE  {&wsMsg} VIEW-AS ALERT-BOX.
          CLEAR FRAME frm-input ALL.
          APPLY 'entry' TO {&skey} IN FRAME frm-Input.
       END.
    END.
END.

PROCEDURE proc-Edit:
   ASSIGN wsid = bfr{&tmptable}.usercode.
   FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.usercode = wsid EXCLUSIVE-LOCK NO-ERROR.
   VIEW FRAME frm-input.
   ENABLE ALL EXCEPT {&skey}  WITH FRAME frm-input.
   DISPLAY wsid @ {&skey} bfr{&tmptable}.NAME bfr{&tmptable}.txtJob bfr{&tmptable}.txtDept
        bfr{&tmptable}.iPhone bfr{&tmptable}.txtStatus bfr{&tmptable}.paysys 
       bfr{&tmptable}.supervisor WITH FRAME frm-input.
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input
          OR CHOOSE OF btn-ok IN FRAME frm-input.
   HIDE FRAME frm-input.
END.

procedure Search.ip.
    hCol = browse brw-{&tmptable}:current-column.
       assign  frame Search-opt:title = hCol:label + " Column".
       IF trim(hCol:label)<> "Name" THEN
           RETURN NO-APPLY.
        view frame Search-opt.
        enable all with frame Search-opt.
        WAIT-FOR 'choose' of btnSearch OR 'tab' of btnSearch OR 'enter' of btnSearch 
            OR 'window-close' of frame Search-opt or close of this-procedure or
                'esc','f4' of frame Search-opt.
        hide frame Search-opt.
        /*if ll-sort = ? then return.*/
    case trim(hCol:label):
      when "Name" then
         do:
          OPEN query qry-{&tmptable} FOR EACH bfr{&tmptable} no-lock 
                            where bfr{&tmptable}.Name >= wsSearch:SCREEN-VALUE
                               BY bfr{&tmptable}.NAME.

            END.
        OTHERWISE DO:
           RETURN NO-APPLY.
        END.     
     END.
    RETURN.
END.
