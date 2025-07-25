session:DATA-ENTRY-RETURN = TRUE.
/* Program.................paypost.p
   Notes:...... Payroll Posts
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsMsg           "Payroll Post already exist"
&SCOPED-DEFINE wsTitle           "Payroll Post File Maintenance"
&SCOPED-DEFINE tmptable           paypost
&SCOPED-DEFINE skey               Paypost.Post
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.Descrip ~
                              bfr{&tmptable}.Proj~
                              bfr{&tmptable}.Fund~
                              bfr{&tmptable}.Dept~
                              bfr{&tmptable}.Grade~ 
                              
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.post ~
                                   COLUMN-LABEL 'Post ':C ~
                              bfr{&tmptable}.Descrip ~
                                   COLUMN-LABEL ' Description ' WIDTH 60~
                              bfr{&tmptable}.Dept~
                                  COLUMN-LABEL 'Department '~
                              bfr{&tmptable}.Proj~
                                  COLUMN-LABEL 'Programme '~
                              bfr{&tmptable}.Fund~
                                  COLUMN-LABEL 'Segment '~
                              bfr{&tmptable}.Grade~
                                  COLUMN-LABEL 'Grade '~
                              bfr{&tmptable}.Vacant~
                                  COLUMN-LABEL 'Vacant '~
                              
{varlib.i}
DEF BUTTON btnDept LABEL "DEPARTMENT".
DEF BUTTON btnProj LABEL "PROGRAMME".

DEFINE RECTANGLE rect1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 118 by 2.3.

DEFINE RECTANGLE rect2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 118 by 18.5.

DEF QUERY qry{&tmptable} FOR bfr{&tmptable}  SCROLLING.
DEF BROWSE brw{&tmptable} QUERY qry{&tmptable}
    DISPLAY {&tmpFields} 
    WITH 22 DOWN SEPARATORS  NO-BOX LABEL-FGCOLOR 9 LABEL-BGCOLOR 3 
    LABEL-FONT 6.

DEF NEW SHARED FRAME frmMain
    brw{&tmptable} AT ROW 1.8 COL 5
    btn-add AT ROW 20.7 COL 10
    Space(20) btn-edit
    space(20) btn-del
    space(20) btn-exit SKIP(1)
    rect2 AT ROW 1.4 COL 3
    rect1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     /*BGCOLOR 8 FGCOLOR 1 */ SIZE 122 BY 23.3
    TITLE {&wsTitle} VIEW-AS DIALOG-BOX.

define frame frm-input SKIP(1)
    {&skey}                 COLON 20  LABEL "Post" SKIP(0.5)
    bfr{&tmptable}.descrip  COLON 20  LABEL "Description"FORM "x(60)" SKIP(0.5)
    bfr{&tmptable}.Grade     COLON 20 LABEL "Grade" 
                    HELP "Grades should be the defined Salary Scales" SKIP(0.5)
    btnDept                  COLON 4  AUTO-RETURN
    bfr{&tmptable}.dept     NO-LABEL 
    paydept.descrip NO-LABEL VIEW-AS TEXT 
    SKIP(0.5)
    btnProj                  COLON 5  AUTO-RETURN
    bfr{&tmptable}.Proj     NO-LABEL 
    glProj.descrip NO-LABEL VIEW-AS TEXT 
    SKIP(0.5)
    btnFund                  COLON 8  AUTO-RETURN
    bfr{&tmptable}.fund     NO-LABEL 
    glfund.descrip NO-LABEL VIEW-AS TEXT
    SKIP(2.5)
    
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "PAYROLL POST MAINTENANCE".

DEF    QUERY qry-PickProj FOR glProj SCROLLING.
DEF BROWSE brw-PickProj QUERY qry-PickProj
    DISPLAY GLProj.Proj GLProj.DESCRIP COLUMN-LABEL "DESCRIPTION" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

DEF    QUERY qry-Fund FOR GlFund SCROLLING.
DEF BROWSE brw-Fund  QUERY qry-Fund 
    DISPLAY GLFUND.FUND COLUMN-LABEL "FUND  !SEGMENT" GLFUND.DESCRIP COLUMN-LABEL "DESCRIPTION" 
    WIDTH 50 WITH 20 DOWN SEPARATORS.

DEF    QUERY qry-dept FOR paydept SCROLLING.
DEF BROWSE brw-dept QUERY qry-dept
    DISPLAY Paydept.Dept Paydept.Descrip  COLUMN-LABEL "Decsription" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-PickProj 
    brw-PickProj AT ROW 2 COL 5
    skip(0.5)
    btn-Ok colon 5
    btn-Close colon 60 
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "PROGRAMME SELECTION".

DEFINE FRAME frm-pickFund 
    brw-Fund AT ROW 2 COL 5
    skip(0.5)
    btn-Ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "FUND/SEGMENT SELECTION".

DEFINE FRAME frm-pickdept 
    brw-dept AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Department Selection".

/* ***** Triggers for the main frame **** */
ON CHOOSE OF btnProj IN FRAME frm-Input
DO:
  VIEW FRAME frm-pickProj.
  OPEN QUERY qry-PickProj FOR EACH glProj NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickProj.
  WAIT-FOR CHOOSE OF btn-Close IN FRAME frm-pickProj 
          OR close of THIS-PROCEDURE IN FRAME frm-pickProj
          OR CHOOSE OF btn-Ok IN FRAME frm-pickProj 
          OR 'enter':u OF brw-PickProj
          OR 'mouse-select-dblclick' OF brw-PickProj.
  CLOSE QUERY qry-PickProj.
  HIDE FRAME frm-pickProj.
  APPLY 'tab' TO btnProj.
  APPLY 'tab' TO bfr{&tmptable}.proj.
  RETURN. 
END.

ON CHOOSE OF btn-Ok IN FRAME frm-pickProj 
    OR 'enter':u OF brw-PickProj
    OR 'mouse-select-dblclick' OF brw-PickProj
DO: 
   GET CURRENT qry-PickProj EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY glProj.Proj @ bfr{&tmptable}.proj glproj.DESCRIPTION  WITH FRAME frm-Input.
   RETURN.
END.

ON 'enter':U OF bfr{&tmptable}.Proj IN FRAME frm-Input
    OR 'tab':U OF bfr{&tmptable}.Proj IN FRAME frm-Input
DO:
    FIND FIRST glproj WHERE glproj.proj = INT(bfr{&tmptable}.Proj:SCREEN-VALUE) NO-ERROR.
    IF NOT AVAILABLE glproj OR INT(bfr{&tmptable}.Proj:SCREEN-VALUE) = 0 THEN DO:
        MESSAGE "Invalid Project entered" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        DISPLAY glProj.descrip WITH FRAME frm-Input.
    END.
    RETURN.
END.

ON CHOOSE OF btnFund IN FRAME frm-Input
DO:
  VIEW FRAME frm-PickFund.
  OPEN QUERY qry-Fund FOR EACH glFund NO-LOCK.
  ENABLE ALL WITH FRAME frm-PickFund.
  WAIT-FOR CHOOSE OF btn-Close IN FRAME frm-PickFund 
          OR close of THIS-PROCEDURE IN FRAME frm-PickFund
          OR CHOOSE OF btn-Ok IN FRAME frm-PickFund 
          OR 'enter':u OF brw-Fund
          OR 'mouse-select-dblclick' OF brw-Fund.
  CLOSE QUERY qry-Fund.
  HIDE FRAME frm-PickFund.
  APPLY 'tab' TO btnFund.
   APPLY 'tab' TO bfr{&tmptable}.Fund.
  RETURN. 
END.

ON CHOOSE OF btn-Ok IN FRAME frm-PickFund 
    OR 'enter':u OF brw-Fund
    OR 'mouse-select-dblclick' OF brw-Fund
DO: 
   GET CURRENT qry-Fund EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY glFund.Fund @ bfr{&tmptable}.Fund glFund.DESCRIP  WITH FRAME frm-Input.
   RETURN.
END.

ON 'enter':U OF bfr{&tmptable}.Fund IN FRAME frm-Input
    OR 'tab':U OF bfr{&tmptable}.Fund IN FRAME frm-Input
DO:
    FIND FIRST glFund WHERE glFund.Fund = INT(bfr{&tmptable}.Fund:SCREEN-VALUE) NO-ERROR.
    IF NOT AVAILABLE glFund OR INT(bfr{&tmptable}.Fund:SCREEN-VALUE) = 0  THEN DO:
        MESSAGE "Invalid Segment entered" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        DISPLAY glFund.descrip WITH FRAME frm-Input.
    END.
    RETURN.
END.

ON CHOOSE OF btnDept IN FRAME frm-input
DO:
  VIEW FRAME frm-pickDept.
  OPEN QUERY qry-dept FOR EACH paydept NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickdept.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pickdept 
          OR close of THIS-PROCEDURE IN FRAME frm-pickdept
          OR CHOOSE OF btn-ok IN FRAME frm-pickdept 
          OR 'enter':u OF brw-dept
          OR 'mouse-select-dblclick' OF brw-dept.
  CLOSE QUERY qry-dept.
  HIDE FRAME frm-pickdept.
  IF bfr{&tmptable}.dept:SCREEN-VALUE = "" THEN
       RETURN NO-APPLY.
  APPLY 'tab' TO SELF IN FRAME frm-input.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pickdept 
    OR 'enter':u OF brw-dept
    OR 'mouse-select-dblclick' OF brw-dept
DO: 
   GET CURRENT qry-dept EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY paydept.dept @ bfr{&tmptable}.dept paydept.descrip WITH FRAME frm-input.
   APPLY 'tab' TO bfr{&tmptable}.dept.
   
END.

ON CHOOSE OF btn-Add IN FRAME frmMain DO:
         btn-ok:LABEL IN  FRAME frm-input = "Save".
         wsbtn = "ADD".
        RUN proc-input.
        RETURN.
    END.

ON CHOOSE OF btn-edit
        OR 'enter':u OF brw{&tmptable}
        OR 'mouse-select-dblclick' OF brw{&tmptable}
    DO:
        btn-ok:LABEL IN  FRAME frm-input = "Update".
        wsbtn = "EDIT".
        GET CURRENT qry{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
        DISABLE {&skey} WITH FRAME frm-input.
        run proc-edit.
        RETURN.
END.

ON 'tab':U OF  bfr{&tmptable}.dept IN FRAME frm-input
    OR 'enter':U OF  bfr{&tmptable}.dept IN FRAME frm-input
DO:
    FIND FIRST paydept WHERE paydept.dept = INT(bfr{&tmptable}.dept:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE paydept THEN
        DISPLAY paydept.descrip WITH FRAME frm-input.
    ELSE DO:
        MESSAGE "Invalid department entered, please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'leave':U OF  bfr{&tmptable}.grade IN FRAME frm-input
DO:
    FIND FIRST payscale WHERE payscale.scale = INT(bfr{&tmptable}.grade:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE payscale THEN DO:
        MESSAGE "Grade not within Salary Scales, please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON LEAVE OF {&skey} IN FRAME frm-input
DO: 
     wsid = int({&skey}:SCREEN-VALUE IN FRAME frm-input).
   IF wsid = 0 THEN DO:
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
          RETURN NO-APPLY.
       END.
    END.
END.

ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
  IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN DO:
     FIND FIRST Paypost WHERE Paypost.post  = wsid NO-ERROR.
        IF AVAILABLE Paypost THEN DO:
           MESSAGE  "Post already exist" VIEW-AS ALERT-BOX.
           RETURN NO-APPLY.
        END.
        ELSE DO:
            CREATE bfr{&tmptable}. 
            ASSIGN bfr{&tmptable}.post    = wsid
                   bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input
                   bfr{&tmptable}.proj    = INT(bfr{&tmptable}.proj:SCREEN-VALUE)
                   bfr{&tmptable}.fund    = INT(bfr{&tmptable}.fund:SCREEN-VALUE)
                   bfr{&tmptable}.dept    = INT(bfr{&tmptable}.dept:SCREEN-VALUE)
                   bfr{&tmptable}.grade   = INT(bfr{&tmptable}.grade:SCREEN-VALUE)
                   bfr{&tmptable}.vacant  = YES
                   bfr{&tmptable}.DeviceName = lc-host
                   bfr{&tmptable}.CreDate  = today
                   bfr{&tmptable}.UID      = varuser.
            CLEAR FRAME frm-input ALL.
            APPLY 'entry' TO {&skey} IN FRAME frm-Input.
        END.
     END.
     ELSE DO:
         ASSIGN bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input
                bfr{&tmptable}.proj    = INT(bfr{&tmptable}.proj:SCREEN-VALUE)
                bfr{&tmptable}.fund    = INT(bfr{&tmptable}.fund:SCREEN-VALUE)
                bfr{&tmptable}.dept    = INT(bfr{&tmptable}.dept:SCREEN-VALUE)
                bfr{&tmptable}.grade   = INT(bfr{&tmptable}.grade:SCREEN-VALUE).
           FIND FIRST payemf WHERE payemf.Post = bfr{&tmptable}.post no-error.
               IF AVAILABLE payemf AND payemf.Dept <> bfr{&tmptable}.dept THEN
                  payemf.Dept = bfr{&tmptable}.dept.
          RELEASE {&tmptable}.
          APPLY 'close' TO SELF.
          HIDE FRAME frm-input.
          brw{&tmptable}:REFRESH() IN FRAME frmMain.
     END.
END.

ON CHOOSE OF btn-Del DO:
    GET CURRENT qry{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.post.
    wsbtn = "DELETE".
    FIND FIRST payemf WHERE Payemf.post  = bfr{&tmptable}.post NO-ERROR.
    IF AVAILABLE payemf THEN DO:
        MESSAGE "Payroll post has related records - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE payemf THEN DO:
     DELETE bfr{&tmptable}.
     Method-Status = brw{&tmptable}:DELETE-SELECTED-ROWS().
    END.
    APPLY 'entry' TO btn-exit.
END.

on 'start-search':u of BROWSE brw{&tmptable}
    run Search.ip.

/********** MAIN LOGIC **********/
browse brw{&tmptable}:allow-column-searching = true.
OPEN QUERY qry{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK .
ENABLE ALL WITH FRAME frmMain.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frmMain.
CLOSE QUERY qry{&tmptable}.
HIDE FRAME frmMain.

PROCEDURE proc-input:
   OPEN QUERY qry{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   CLEAR FRAME frm-input ALL.
   ENABLE ALL WITH FRAME frm-input.
   {&skey}:SCREEN-VALUE = "0".
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
   CLOSE QUERY qry{&tmptable}.
   OPEN QUERY qry{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   HIDE FRAME frm-input.
END.

PROCEDURE proc-edit:
    GET CURRENT qry{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = int(bfr{&tmptable}.post).
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.post  = wsid EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST paydept WHERE bfr{&tmptable}.dept = paydept.dept NO-LOCK NO-ERROR.
    DISPLAY wsid @ {&skey} {&updFields} paydept.descrip WITH FRAME frm-input.
    DISABLE ALL WITH FRAME frm-input.
    ENABLE {&updfields} btn-ok btn-close btnDept btnProj btnFund WITH FRAME frm-input.
    WAIT-FOR CHOOSE OF btn-close 
          OR CHOOSE OF btn-ok OR close of THIS-PROCEDURE IN FRAME frm-input.
    CLOSE QUERY qry{&tmptable}.
   OPEN QUERY qry{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   HIDE FRAME frm-input.
 END.

procedure Search.ip.
hCol = browse brw{&tmptable}:current-column.
    /*assign  frame Search-opt:title = hCol:label + " Column"
            frame Search-opt:x     = hCol:x
            frame Search-opt:y     = browse b-{&t_zoom_table}:y. */
    view frame Search-opt.
    enable all with frame Search-opt.
    WAIT-FOR 'choose' of btnSearch OR 'tab' of btnSearch OR 'enter' of btnSearch 
        OR 'window-close' of frame Search-opt or close of this-procedure or
            'esc','f4' of frame Search-opt.
    hide frame Search-opt.
    /*if ll-sort = ? then return.*/
    case trim(hCol:label):
        when "DESCRIPTION" then
        do:
           open query qry{&tmptable}
             FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.descrip CONTAINS wsSearch:SCREEN-VALUE no-lock.
        END.
        when "POST" then
        do:
           open QUERY qry{&tmptable}
               FOR EACH bfr{&tmptable}  WHERE  bfr{&tmptable}.post >= INT(wsSearch:SCREEN-VALUE) no-lock.
        END.
        when "DEPARTMENT" then
        do:
           open QUERY qry{&tmptable}
               FOR EACH bfr{&tmptable} WHERE  bfr{&tmptable}.dept >= INT(wsSearch:SCREEN-VALUE) no-lock.
        END.
        END.
    RETURN.
END.
