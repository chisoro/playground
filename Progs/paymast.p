session:DATA-ENTRY-RETURN = TRUE.
/* Program.................paymast.p
   Notes:...... Employee capture
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsMsg           "Employee record already exist"
&SCOPED-DEFINE wsTitle           "Employee Master File Maintenance"
&SCOPED-DEFINE tmptable           payemf
&SCOPED-DEFINE skey               payemf.empcode
&SCOPED-DEFINE UpdFields    bfr{&tmptable}.SName ~
                              bfr{&tmptable}.FName~
                              bfr{&tmptable}.Account~
                              bfr{&tmptable}.Add1~
                              bfr{&tmptable}.Add2~
                              bfr{&tmptable}.Add3~
                              bfr{&tmptable}.Bank FORM "99999"~
                              bfr{&tmptable}.BirthPlace~
                              bfr{&tmptable}.Cell~
                              bfr{&tmptable}.DOB~
                              bfr{&tmptable}.IDNo~
                              bfr{&tmptable}.email~
                              bfr{&tmptable}.KinCell~
                              bfr{&tmptable}.Marital~
                              bfr{&tmptable}.NextKin~
                              bfr{&tmptable}.Notch~
                              bfr{&tmptable}.Post~
                              bfr{&tmptable}.Relation~
                              bfr{&tmptable}.sex~
                               bfr{&tmptable}.leaveMax~
                              bfr{&tmptable}.Grade~
                              bfr{&tmptable}.StartDate~
                              bfr{&tmptable}.stdhours~

&SCOPED-DEFINE tmpFields      bfr{&tmptable}.empcode ~
                                   COLUMN-LABEL 'EMPLOYEE ':C ~
                              bfr{&tmptable}.SName ~
                                   COLUMN-LABEL ' SURNAME ' WIDTH 40~
                              bfr{&tmptable}.FName~
                                  COLUMN-LABEL 'FIRST NAME '~
                              bfr{&tmptable}.Dept~
                                  COLUMN-LABEL 'DEPARTMENT '~
                              bfr{&tmptable}.Designation ~
                                  COLUMN-LABEL 'OCCUPATION ' WIDTH 50~
 /**/                            
{varlib.i}
DEF VAR X AS INT.
DEF SHARED VAR varUser LIKE simusr.usercode.
DEF VAR wsNotch AS DEC.
DEF VAR wsSys  LIKE Paysys.Paysys.
DEF VAR wsDate AS DATE.
DEF VAR st-per LIKE gltdf.period.
DEF VAR wsstatus AS CHAR FORM "x(7)".


DEF FRAME Search-opt
    wsSearch  COLON 20 LABEL "Search Value"
    btnSearch
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED.

DEF TEMP-TABLE tmpNotch
    FIELD vrNotch AS INT LABEL "NOTCH"
    FIELD vrAmt AS DEC FORM "zzz,zzz,zz9.99-" LABEL "AMOUNT".

DEF BUTTON btnJob LABEL "OCCUPATION".
DEF BUTTON btn-Sys   LABEL "Payroll System".
DEF BUTTON btn-notch LABEL "Salary Notch".

DEFINE RECTANGLE rect1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 155 by 2.3.
DEFINE RECTANGLE Job
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 90 by 5.3.
DEFINE RECTANGLE Personal
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 90 by 13.3.
DEFINE RECTANGLE Employ
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 60 by 7.3.
DEFINE RECTANGLE Bank
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 60 by 11.3.
DEFINE RECTANGLE rect2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 155 by 18.5.

DEF QUERY qry{&tmptable} FOR bfr{&tmptable}  SCROLLING.
DEF BROWSE brw{&tmptable} QUERY qry{&tmptable}
    DISPLAY {&tmpFields} wsstatus FORM "x(10)"
    WITH 22 DOWN SEPARATORS  NO-BOX LABEL-FGCOLOR 9 LABEL-BGCOLOR 3 
    LABEL-FONT 6.

DEF    QUERY qry-Sys FOR Paysys SCROLLING.
DEF BROWSE brw-Sys QUERY qry-Sys
        DISPLAY Paysys.Paysys Paysys.Descrip COLUMN-LABEL "Decsription" 
        WIDTH 60 WITH 20 DOWN SEPARATORS.

DEF QUERY qry-Notch FOR tmpNotch SCROLLING.
DEF BROWSE brw-Notch QUERY qry-Notch
    DISPLAY  vrNotch vrAmt 
    WITH 12 DOWN SEPARATORS  NO-BOX LABEL-FGCOLOR 9 LABEL-BGCOLOR 3 
    LABEL-FONT 6.

DEFINE FRAME frm-pick 
    brw-Sys AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Payroll System Selection".

DEFINE FRAME frm-Paysys
    SKIP(1.5)
    btn-Sys   COLON 10 NO-LABEL NO-TAB-STOP
    wsSys     NO-LABEL AUTO-RETURN 
    Paysys.Descrip NO-LABEL FORM "x(30)" VIEW-AS TEXT SKIP(2.5)
    btn-ok colon 10
    btn-close colon 50
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 70 BY 8 KEEP-TAB-ORDER
    TITLE "EMPLOYEE MASTER RECORD MAINTENANCE" VIEW-AS DIALOG-BOX.

DEF NEW SHARED FRAME frmMain
    brw{&tmptable} AT ROW 1.8 COL 5
    btn-add AT ROW 20.7 COL 20
    Space(40) btn-edit
    /*space(25) btn-del */
    space(40) btn-exit SKIP(1)
    rect2 AT ROW 1.4 COL 3
    rect1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     /*BGCOLOR 8 FGCOLOR 1 */ SIZE 160 BY 23.3
    TITLE {&wsTitle} VIEW-AS DIALOG-BOX.

define frame frm-input SKIP(1)
       job      AT ROW 1 COL 5
        {&skey}  AT ROW 2 COL 6 LABEL "Employee Number" SKIP(0.5)
        btnJob               COLON 4  AUTO-RETURN NO-TAB-STOP
        bfr{&tmptable}.post     NO-LABEL AUTO-RETURN
        PayPost.Descrip NO-LABEL VIEW-AS TEXT SKIP(0.5)
        paydept.descrip COLON 20 LABEL "Department" VIEW-AS TEXT
        employ   AT ROW 1 COL 95
        bfr{&tmptable}.Grade     AT ROW 2   COL 113 LABEL "Grade" VIEW-AS TEXT
        btn-Notch                AT ROW 3.0   COL 110  AUTO-RETURN 
        bfr{&tmptable}.Notch     NO-TAB-STOP   NO-LABEL VIEW-AS TEXT
        bfr{&tmptable}.StartDate AT ROW 4.5 COL 110 LABEL "Start Date"  
        bfr{&tmptable}.stdhours  AT ROW 6.0   COL 98 LABEL "Standard working Hours" SKIP(0.5)
        bfr{&tmptable}.leaveMax AT ROW 7.0   COL 98 LABEL "Leave Days Max" SKIP(0.5)
        personal AT ROW 6.5 COL 5
        bfr{&tmptable}.SName  AT ROW 7 COL 13  LABEL "Surname" FORM "x(40)" SKIP(0.5)
        bfr{&tmptable}.FName      COLON 20 LABEL "First Name" FORM "x(40)" SKIP(0.5)
        bfr{&tmptable}.sex        COLON 20 LABEL "Sex" 
        bfr{&tmptable}.DOB                 LABEL "Date of Birth"
        bfr{&tmptable}.IDNo                LABEL "ID Number"      SKIP(0.5) 
        bfr{&tmptable}.BirthPlace COLON 20 LABEL "Place of Birth" SKIP(0.5)
        bfr{&tmptable}.Add1       COLON 20 FORM "X(40)" LABEL "Address" SKIP(0.5)
        bfr{&tmptable}.Add2       COLON 20 FORM "X(40)" NO-LABEL  SKIP(0.5)
        bfr{&tmptable}.Add3       COLON 20 FORM "X(40)" NO-LABEL  SKIP(0.5)
        bfr{&tmptable}.email      COLON 20 FORM "X(40)" LABEL "eMail" 
        bfr{&tmptable}.Cell                LABEL "Cell"
        bank     AT ROW 8.5 COL 95
        bfr{&tmptable}.Marital AT ROW 9 COL 97 LABEL "Marital Status" skip(0.5)
        bfr{&tmptable}.NextKin  COLON 110 LABEL "Next of Kin"skip(0.5)
        bfr{&tmptable}.Relation COLON 110 LABEL "Relation" SKIP(0.5)
        bfr{&tmptable}.kinCell  COLON 110 LABEL "Next of Kin Cell" SKIP(0.5)
        "BANKING DETAILS"       COLON 100 SKIP(0.5)
        btnBank                 COLON 100 AUTO-RETURN NO-TAB-STOP
        bfr{&tmptable}.Bank     NO-LABEL FORM "99999"  AUTO-RETURN  BankName NO-LABEL VIEW-AS TEXT SKIP(0.5)
        bfr{&tmptable}.Account  COLON 110 LABEL "Account" HELP "Separate Branch code from Bank Account with a -"
    SKIP(1.5)
    btn-ok colon 30
    btn-close colon 90 SKIP(1)
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "EMPLOYEE MASTER RECORD MAINTENANCE".

DEF    QUERY qry-Job FOR paypost SCROLLING.
DEF BROWSE brw-Job QUERY qry-Job
    DISPLAY PayPost.Dept PayPost.Post PayPost.Descrip WIDTH 60
    PayPost.Grade PayPost.Vacant WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pickJob 
    brw-Job AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 20
    btn-close colon 50
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Job Selection".

DEF    QUERY qry-Pbank FOR paybnk SCROLLING.
DEF BROWSE brw-pbank QUERY qry-Pbank
    DISPLAY paybnk.BankName paybnk.Bank WIDTH 60 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pickPBank 
    brw-pbank AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 20
    btn-close colon 50
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Payroll Bank Selection".

DEFINE FRAME frm-pickNotch 
    brw-Notch AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-close colon 15
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Salary Notch Selection".

/* Triggers for the Paysys frame */

ON CHOOSE OF btn-Notch IN FRAME frm-Input
DO:
   FIND FIRST payscale WHERE payscale.Scale = paypost.Grade NO-ERROR.
   FOR EACH tmpNotch.
       DELETE tmpNotch.
   END.
   DO X = 1 TO 12:
       IF notch[X] <> 0 THEN DO:
           CREATE tmpNotch.
           ASSIGN vrNotch = X
                  vrAmt = Notch[X].
       END.
   END.
  VIEW FRAME frm-pickNotch.
  OPEN QUERY qry-Notch FOR EACH tmpNotch NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickNotch.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pickNotch 
          OR close of THIS-PROCEDURE IN FRAME frm-pickNotch
          OR CHOOSE OF btn-ok IN FRAME frm-pickNotch 
          OR 'enter':u OF brw-Notch
          OR 'mouse-select-dblclick' OF brw-Notch.
  CLOSE QUERY qry-Notch.
  HIDE FRAME frm-pickNotch.
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pickNotch 
    OR 'enter':u OF brw-Notch
    OR 'mouse-select-dblclick' OF brw-notch
DO: 
   GET CURRENT qry-Notch NO-LOCK NO-WAIT.
   DISPLAY  tmpNotch.vrAmt @ bfr{&tmptable}.Notch WITH FRAME frm-input.
   wsNotch = tmpNotch.vrAmt.
   FOR EACH tmpNotch:
       DELETE tmpNotch.
   END.
   APPLY 'tab' TO SELF.
   RETURN. 
END.

ON row-display OF brw{&tmptable} DO:
    ASSIGN wsStatus = "ACTIVE" WHEN bfr{&tmptable}.eStatus = 1
           wsStatus = "LEFT" WHEN bfr{&tmptable}.eStatus = 4
           wsStatus = "SUSPEND" WHEN bfr{&tmptable}.eStatus = 3 
                                  OR bfr{&tmptable}.eStatus = 2.
END.

ON CHOOSE OF btn-sys IN FRAME frm-Paysys
  /*  OR CHOOSE OF btn-sys IN FRAME frm-Paysys */
DO:
  VIEW FRAME frm-pick.
  OPEN QUERY qry-sys FOR EACH paysys NO-LOCK.
  ENABLE ALL WITH FRAME frm-pick.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pick 
          OR close of THIS-PROCEDURE IN FRAME frm-pick
          OR CHOOSE OF btn-ok IN FRAME frm-pick 
          OR 'enter':u OF brw-sys
          OR 'mouse-select-dblclick' OF brw-sys.
  CLOSE QUERY qry-sys.
  HIDE FRAME frm-pick.
 APPLY 'tab' TO SELF.
 APPLY 'tab' TO wsSys.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pick 
    OR 'enter':u OF brw-sys
    OR 'mouse-select-dblclick' OF brw-sys
DO: 
   GET CURRENT qry-sys NO-LOCK NO-WAIT.
   DISPLAY Paysys.Paysys @ wsSys Paysys.Descrip WITH FRAME frm-Paysys.
   RETURN. 
END.

ON  'Enter':U OF wsSys IN FRAME frm-Paysys
     OR 'tab':U OF wsSys IN FRAME frm-Paysys
 DO:
    /*MESSAGE INT(wsSys:SCREEN-VALUE) varUser VIEW-AS ALERT-BOX. */
    ASSIGN wsSys.
     FIND FIRST paysys WHERE Paysys.Paysys = INT(wsSys:SCREEN-VALUE) NO-LOCK NO-ERROR.
     IF AVAILABLE paysys THEN DO:
         FIND FIRST simusr WHERE simusr.usercode = varUser NO-LOCK NO-ERROR.
         IF AVAILABLE simusr AND (simusr.Paysys = 99 OR simusr.Paysys = INT(wsSys:SCREEN-VALUE)) THEN
         DO:
              DISPLAY Paysys.Paysys @ wsSys Paysys.Descrip  WITH FRAME frm-Paysys.
              ENABLE btn-ok WITH FRAME frm-paysys.
              APPLY 'entry' TO btn-ok IN FRAME frm-paysys.
         END.
         ELSE DO:
             MESSAGE "You are not allowed to access this Payroll...Please try again" VIEW-AS ALERT-BOX.
             DISABLE btn-ok WITH FRAME frm-paysys.
             RETURN NO-APPLY.
         END.
     END.  
     ELSE DO:
         MESSAGE "Invalid Payroll System...Please try again" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
     END.
     RETURN.
 END.

ON 'choose':U OF btn-ok IN FRAME frm-Paysys 
DO:
    FIND FIRST paysys WHERE Paysys.Paysys = INT(wsSys:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE paysys THEN DO:
        HIDE FRAME frm-Paysys.
        FIND FIRST PAYCTR NO-LOCK NO-ERROR.
        OPEN QUERY qry{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.paysys = wsSys NO-LOCK .
        ENABLE ALL WITH FRAME frmMain.
        WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frmMain.
        CLOSE QUERY qry{&tmptable}.
        HIDE FRAME frmMain.
        APPLY 'close' TO THIS-PROCEDURE IN FRAME frm-Paysys.
       /* RETURN.*/
    END.
    ELSE IF NOT AVAILABLE paysys THEN DO:
        MESSAGE "Invalid Payroll System...Please try again" VIEW-AS ALERT-BOX.
        APPLY 'close' TO THIS-PROCEDURE IN FRAME frm-Paysys.
    END.
END.
/* ***** Triggers for the main frame **** */
ON CHOOSE OF btnJob IN FRAME frm-input
DO:
  VIEW FRAME frm-pickJob.
  OPEN QUERY qry-Job FOR EACH paypost WHERE PayPost.Vacant <> NO NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickJob.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pickJob 
          OR close of THIS-PROCEDURE IN FRAME frm-pickJob
          OR CHOOSE OF btn-ok IN FRAME frm-pickJob 
          OR 'enter':u OF brw-Job
          OR 'mouse-select-dblclick' OF brw-Job.
  CLOSE QUERY qry-Job.
  HIDE FRAME frm-pickJob.
  IF bfr{&tmptable}.post:SCREEN-VALUE = "" THEN
       RETURN NO-APPLY.
  APPLY 'tab' TO SELF IN FRAME frm-input.
  APPLY 'tab' TO bfr{&tmptable}.post.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pickJob 
    OR 'enter':u OF brw-Job
    OR 'mouse-select-dblclick' OF brw-Job
DO: 
   GET CURRENT qry-Job EXCLUSIVE-LOCK NO-WAIT.
   FIND FIRST paydept WHERE paydept.dept = paypost.dept NO-ERROR.
   DISPLAY PayPost.Post @ bfr{&tmptable}.Post PayPost.Descrip 
       paypost.grade @ bfr{&tmptable}.Grade paydept.descrip WITH FRAME frm-input.
   APPLY 'tab' TO bfr{&tmptable}.post.
   
END.

ON 'tab':U OF  bfr{&tmptable}.post IN FRAME frm-input
    OR 'enter':U OF  bfr{&tmptable}.post IN FRAME frm-input
DO:
    FIND FIRST paypost WHERE paypost.post = INT(bfr{&tmptable}.post:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE paypost THEN
        DISPLAY paypost.descrip WITH FRAME frm-input.
    ELSE DO:
        MESSAGE "Invalid Post entered, please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON CHOOSE OF btnBank IN FRAME frm-input
DO:
  VIEW FRAME frm-pickPBank.
  OPEN QUERY qry-Pbank FOR EACH paybnk NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickPbank.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pickPBank 
          OR close of THIS-PROCEDURE IN FRAME frm-pickPBank
          OR CHOOSE OF btn-ok IN FRAME frm-pickPBank 
          OR 'enter':u OF brw-pbank
          OR 'mouse-select-dblclick' OF brw-pbank.
  CLOSE QUERY qry-Pbank.
  HIDE FRAME frm-pickPbank.
  IF bfr{&tmptable}.bank:SCREEN-VALUE = "" THEN
       RETURN NO-APPLY.
  APPLY 'tab' TO SELF IN FRAME frm-input.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pickPBank 
    OR 'enter':u OF brw-pbank
    OR 'mouse-select-dblclick' OF brw-pbank
DO: 
   GET CURRENT qry-Pbank EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY paybnk.bank @ bfr{&tmptable}.bank paybnk.BankName  WITH FRAME frm-input.
   APPLY 'tab' TO bfr{&tmptable}.bank.
END.

ON 'tab':U OF  bfr{&tmptable}.bank IN FRAME frm-input
    OR 'enter':U OF  bfr{&tmptable}.bank IN FRAME frm-input
DO:
    FIND FIRST paybnk WHERE paybnk.bank = INT(bfr{&tmptable}.bank:SCREEN-VALUE IN FRAME frm-input) NO-LOCK NO-ERROR.
    IF AVAILABLE paybnk THEN
           DISPLAY paybnk.BankName @ BankName WITH FRAME frm-input.
    ELSE IF NOT AVAILABLE paybnk THEN DO:
        MESSAGE "Invalid Bank entered, please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON CHOOSE OF btn-Add IN FRAME frmMain 
DO:
   btn-ok:LABEL IN  FRAME frm-input = "Save".
   RUN proc-input.
   RETURN.
END.

ON CHOOSE OF btn-edit
    OR 'enter':u OF brw{&tmptable}
    OR 'mouse-select-dblclick' OF brw{&tmptable}
DO:
    btn-ok:LABEL IN  FRAME frm-input = "Update".
    GET CURRENT qry{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    DISABLE {&skey} btnJob bfr{&tmptable}.post WITH FRAME frm-input.
    run proc-edit.
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
  IF INT(bfr{&tmptable}.post:SCREEN-VALUE) = 0 
      OR INT(bfr{&tmptable}.bank:SCREEN-VALUE) = 0 THEN DO:
      MESSAGE "Invalid record, .....provide missing data" 
          VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
  END.
  IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN DO:
     FIND FIRST Payemf WHERE payemf.empcode  = wsid NO-ERROR.
        IF AVAILABLE Payemf THEN DO:
           MESSAGE {&wsMsg} VIEW-AS ALERT-BOX.
           RETURN NO-APPLY.
        END.
        ELSE DO:
            FIND FIRST paypost WHERE paypost.post = INT(bfr{&tmptable}.post:SCREEN-VALUE) NO-ERROR.
            CREATE bfr{&tmptable}. 
            ASSIGN bfr{&tmptable}.empcode  = wsid
                   bfr{&tmptable}.Sname    = bfr{&tmptable}.Sname:SCREEN-VALUE IN FRAME frm-input
                   bfr{&tmptable}.post     = INT(bfr{&tmptable}.post:SCREEN-VALUE)
                   bfr{&tmptable}.fName    = bfr{&tmptable}.Fname:SCREEN-VALUE
                   bfr{&tmptable}.designation = paypost.Descrip:SCREEN-VALUE
                   bfr{&tmptable}.dept     = PayPost.Dept
                   bfr{&tmptable}.grade    = PayPost.Grade
                   PayPost.Vacant          = No
                   bfr{&tmptable}.Account  = bfr{&tmptable}.Account:SCREEN-VALUE 
                   bfr{&tmptable}.Add1     = bfr{&tmptable}.Add1:SCREEN-VALUE
                   bfr{&tmptable}.Add2     = bfr{&tmptable}.Add2:SCREEN-VALUE 
                   bfr{&tmptable}.Add3     = bfr{&tmptable}.Add3:SCREEN-VALUE 
                   bfr{&tmptable}.Bank     = INT(bfr{&tmptable}.Bank:SCREEN-VALUE)
                   bfr{&tmptable}.BirthPlace =  bfr{&tmptable}.BirthPlace:SCREEN-VALUE 
                   bfr{&tmptable}.Cell      = INT(bfr{&tmptable}.cell:SCREEN-VALUE)
                   bfr{&tmptable}.DOB       = DATE(bfr{&tmptable}.DOB:SCREEN-VALUE)
                   bfr{&tmptable}.IDNo      = bfr{&tmptable}.IDNo:SCREEN-VALUE
                   bfr{&tmptable}.email     = bfr{&tmptable}.email:SCREEN-VALUE
                   bfr{&tmptable}.KinCell   = dec(bfr{&tmptable}.KinCell:SCREEN-VALUE)
                   bfr{&tmptable}.Marital   = bfr{&tmptable}.Marital:SCREEN-VALUE 
                   bfr{&tmptable}.NextKin   = bfr{&tmptable}.NextKin:SCREEN-VALUE 
                   bfr{&tmptable}.Notch     = dec(bfr{&tmptable}.Notch:SCREEN-VALUE)
                   bfr{&tmptable}.Relation  = bfr{&tmptable}.Relation:SCREEN-VALUE 
                   bfr{&tmptable}.scale     =  PayPost.Grade
                   bfr{&tmptable}.sex       =  bfr{&tmptable}.Sex:SCREEN-VALUE 
                   bfr{&tmptable}.StartDate = DATE( bfr{&tmptable}.StartDate:SCREEN-VALUE)
                   bfr{&tmptable}.stdhours  = INT(bfr{&tmptable}.stdHours:SCREEN-VALUE)
                   bfr{&tmptable}.leaveMax  = DEC( bfr{&tmptable}.leaveMax:SCREEN-VALUE)
                   bfr{&tmptable}.Paysys    = wsSys
                   bfr{&tmptable}.eStat     = 1
                   bfr{&tmptable}.Statdate  = DATE( bfr{&tmptable}.StartDate:SCREEN-VALUE)
                   bfr{&tmptable}.leaver     = NO
            bfr{&tmptable}.startper = Paysys.CurPer . /*INT(STRING(YEAR(bfr{&tmptable}.StartDate)) 
                                                + STRING(MONTH(bfr{&tmptable}.StartDate),"99")). */
            wsNotch = dec(bfr{&tmptable}.Notch:SCREEN-VALUE).
           DO X = 1 TO  3: /* Basic and NSSA items */
               CREATE paymtf.
                ASSIGN paymtf.empcode = wsid
                       Paymtf.ACTIVE   = 1
                       Paymtf.StdAmt   = wsNotch  WHEN X = 1
                       Paymtf.Itemcode = Payctr.Basic WHEN X = 1
                       Paymtf.Itemcode = Payctr.NSSA[1] WHEN X = 2
                       Paymtf.Itemcode = Payctr.NSSA[2] WHEN X = 3.
           END.
           DO X = 1 TO 3: /*Overtime items */
               IF Payctr.overtime[X] <> 0 THEN DO:
                   CREATE paymtf.
                   ASSIGN paymtf.empcode = wsid
                          Paymtf.ACTIVE   = 1
                          Paymtf.Itemcode = Payctr.overtime[X]
                          Paymtf.StdAmt   = (wsNotch / INT(bfr{&tmptable}.stdhours:SCREEN-VALUE IN FRAME frm-input))
                                            * Payctr.ORate[X].
               END. 
           END.                               
            CLEAR FRAME frm-input ALL.
            APPLY 'entry' TO {&skey} IN FRAME frm-Input.
        END.
     END.
     ELSE DO:
         IF  bfr{&tmptable}.Notch <> dec(bfr{&tmptable}.Notch:SCREEN-VALUE)
             OR bfr{&tmptable}.stdhours  <> INT(bfr{&tmptable}.stdHours:SCREEN-VALUE) THEN DO:
             ASSIGN wsNotch = dec(bfr{&tmptable}.Notch:SCREEN-VALUE). 
             FIND FIRST paymtf WHERE paymtf.empcode = wsid AND paymtf.itemcode = Payctr.Basic NO-ERROR.
             IF AVAILABLE paymtf THEN
                 ASSIGN Paymtf.StdAmt   = wsNotch.
             DO X = 1 TO 3: /*Overtime items */
                 IF Payctr.overtime[X] <> 0 THEN DO:
                    FIND FIRST paymtf WHERE paymtf.empcode = wsid AND paymtf.itemcode = Payctr.overtime[X] NO-ERROR.
                    IF NOT AVAILABLE paymtf THEN DO:
                       CREATE paymtf.
                       ASSIGN paymtf.empcode = wsid
                              Paymtf.ACTIVE   = 1
                              Paymtf.Itemcode = Payctr.overtime[X].
                    END.
                    Paymtf.StdAmt   = (wsNotch / INT(bfr{&tmptable}.stdhours:SCREEN-VALUE IN FRAME frm-input))
                                        * Payctr.ORate[X].
                 END.
             END. 
         END.
         IF bfr{&tmptable}.post <> INT(bfr{&tmptable}.post:SCREEN-VALUE) THEN DO:
            FIND FIRST paypost WHERE paypost.post = bfr{&tmptable}.post NO-ERROR.
            PayPost.Vacant          = YES.
         END.
         FIND FIRST paypost WHERE paypost.post = INT(bfr{&tmptable}.post:SCREEN-VALUE) NO-ERROR.
         ASSIGN bfr{&tmptable}.Sname    = bfr{&tmptable}.Sname:SCREEN-VALUE IN FRAME frm-input
                bfr{&tmptable}.post     = INT(bfr{&tmptable}.post:SCREEN-VALUE)
                bfr{&tmptable}.fName    = bfr{&tmptable}.Fname:SCREEN-VALUE
                bfr{&tmptable}.designation = paypost.Descrip:SCREEN-VALUE
                bfr{&tmptable}.dept     = PayPost.Dept
                bfr{&tmptable}.grade    = PayPost.Grade
                PayPost.Vacant          = No
                bfr{&tmptable}.Account  = bfr{&tmptable}.Account:SCREEN-VALUE 
                bfr{&tmptable}.Add1     = bfr{&tmptable}.Add1:SCREEN-VALUE
                bfr{&tmptable}.Add2     = bfr{&tmptable}.Add2:SCREEN-VALUE 
                bfr{&tmptable}.Add3     = bfr{&tmptable}.Add3:SCREEN-VALUE 
                bfr{&tmptable}.Bank     = INT(bfr{&tmptable}.Bank:SCREEN-VALUE)
                bfr{&tmptable}.BirthPlace =  bfr{&tmptable}.BirthPlace:SCREEN-VALUE 
                bfr{&tmptable}.Cell      = INT(bfr{&tmptable}.cell:SCREEN-VALUE)
                bfr{&tmptable}.DOB       = DATE(bfr{&tmptable}.DOB:SCREEN-VALUE)
                bfr{&tmptable}.IDNo      = bfr{&tmptable}.IDNo:SCREEN-VALUE
                bfr{&tmptable}.email     = bfr{&tmptable}.email:SCREEN-VALUE
                bfr{&tmptable}.KinCell   = dec(bfr{&tmptable}.KinCell:SCREEN-VALUE)
                bfr{&tmptable}.Marital   = bfr{&tmptable}.Marital:SCREEN-VALUE 
                bfr{&tmptable}.NextKin   = bfr{&tmptable}.NextKin:SCREEN-VALUE 
                bfr{&tmptable}.Notch     = dec(bfr{&tmptable}.Notch:SCREEN-VALUE)
                bfr{&tmptable}.Relation  = bfr{&tmptable}.Relation:SCREEN-VALUE 
                bfr{&tmptable}.scale     =  PayPost.Grade
                bfr{&tmptable}.sex       =  bfr{&tmptable}.Sex:SCREEN-VALUE 
                bfr{&tmptable}.stdhours  = INT(bfr{&tmptable}.stdHours:SCREEN-VALUE)
                bfr{&tmptable}.leaveMax  = DEC( bfr{&tmptable}.leaveMax:SCREEN-VALUE)
                bfr{&tmptable}.Paysys    = wsSys.
          FIND FIRST Payhtf WHERE payhtf.empcode = wsid NO-LOCK NO-ERROR.
          IF NOT AVAILABLE payhtf THEN
              ASSIGN bfr{&tmptable}.StartDate = DATE( bfr{&tmptable}.StartDate:SCREEN-VALUE).
          ELSE IF AVAILABLE payhtf AND payhtf.trDate >= DATE( bfr{&tmptable}.StartDate:SCREEN-VALUE) THEN
             ASSIGN bfr{&tmptable}.StartDate = DATE( bfr{&tmptable}.StartDate:SCREEN-VALUE).
                  ELSE MESSAGE "Start Date cannot be changed" VIEW-AS ALERT-BOX.
          RELEASE {&tmptable}.
          APPLY 'close' TO SELF.
          HIDE FRAME frm-input.
          brw{&tmptable}:REFRESH() IN FRAME frmMain.
     END.
END.

ON 'leave':U OF bfr{&tmptable}.stdhours IN FRAME frm-Input
   OR 'enter':U OF bfr{&tmptable}.stdhours
DO:
    IF INT(bfr{&tmptable}.stdhours:SCREEN-VALUE) = 0 THEN DO:
        MESSAGE "Invalid working hours, please enter correct data"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'leave':U OF bfr{&tmptable}.StartDate IN FRAME frm-Input
    OR 'enter':U OF bfr{&tmptable}.StartDate
DO:
    IF DATE(bfr{&tmptable}.StartDate:SCREEN-VALUE) = ? THEN DO:
        MESSAGE "Invalid start date, please enter correct date"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'leave':U OF bfr{&tmptable}.DOB IN FRAME frm-Input
    OR 'enter':U OF bfr{&tmptable}.DOB
DO:
    IF DATE(bfr{&tmptable}.DOB:SCREEN-VALUE) = ? THEN DO:
        MESSAGE "Invalid start date, please enter correct date"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'leave':U OF bfr{&tmptable}.IDNO IN FRAME frm-Input
    OR 'enter':U OF bfr{&tmptable}.IDNO
DO:
    IF bfr{&tmptable}.IDNO:SCREEN-VALUE = "" THEN DO:
        MESSAGE "Invalid start date, please enter correct date"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.


on 'start-search':u of BROWSE brw{&tmptable}
    run Search.ip.

/*ON 'choose':U OF btnSearch IN FRAME Search-opt 
    OR 'enter':U OF btnSearch IN FRAME Search-opt
    OR 'tab':U OF btnSearch IN FRAME Search-opt
DO:
    RUN SEARCH.ip.
END. */


/********** MAIN LOGIC **********/
browse brw{&tmptable}:allow-column-searching = true.
VIEW FRAME frm-Paysys.
ENABLE ALL WITH FRAME frm-Paysys.
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-Paysys.
HIDE FRAME frm-Paysys.

PROCEDURE proc-input:
   OPEN QUERY qry{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.Paysys = wsSys NO-LOCK.
   CLEAR FRAME frm-input ALL.
   ENABLE ALL WITH FRAME frm-input.
   {&skey}:SCREEN-VALUE = "0".
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
   CLOSE QUERY qry{&tmptable}.
   OPEN QUERY qry{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.Paysys = wsSys NO-LOCK.
   HIDE FRAME frm-input.
END.

PROCEDURE proc-edit:
    GET CURRENT qry{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = int(bfr{&tmptable}.empcode).
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.empcode  = wsid EXCLUSIVE-LOCK NO-ERROR.
    IF  bfr{&tmptable}.eStatus = 4 THEN DO:
        MESSAGE "Employee has left, cannot be editted."
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    CLEAR FRAME frm-input.
    FIND FIRST paypost WHERE bfr{&tmptable}.post = paypost.post NO-LOCK NO-ERROR.
    FIND FIRST paydept WHERE paydept.dept = paypost.dept NO-LOCK NO-ERROR.
    FIND FIRST paybnk WHERE paybnk.bank = bfr{&tmptable}.bank NO-LOCK NO-ERROR.
    DISPLAY wsid @ {&skey} {&updFields} paypost.descrip paydept.descrip paybnk.BankName WITH FRAME frm-input.
    DISABLE ALL WITH FRAME frm-input.
    ENABLE  ALL EXCEPT {&skey} bfr{&tmptable}.post btnJob WITH FRAME frm-input.
    WAIT-FOR CHOOSE OF btn-close 
          OR CHOOSE OF btn-ok OR close of THIS-PROCEDURE IN FRAME frm-input.
    CLOSE QUERY qry{&tmptable}.
   OPEN QUERY qry{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.Paysys = wsSys NO-LOCK.
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
        when "SURNAME" then
        do:
           open query qry{&tmptable}
                    FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.Paysys = wsSys  
                        AND bfr{&tmptable}.sname >= wsSearch:SCREEN-VALUE no-lock
                          BY bfr{&tmptable}.sname.

        END.
        when "EMPLOYEE" then
        do:
           open query qry{&tmptable}
                    FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.Paysys = wsSys  
                        AND bfr{&tmptable}.empcode >= INT(wsSearch:SCREEN-VALUE) USE-INDEX empcode no-lock.
                           

                    END.
        END.
    RETURN.
END.
     
