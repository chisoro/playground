session:DATA-ENTRY-RETURN = TRUE.
/* Program.................dbproj.p
   Notes:...... Project file maintenance
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsMsg           "Project already exist"
&SCOPED-DEFINE wsTitle           "Project file Maintenance"
&SCOPED-DEFINE tmptable           LProj
&SCOPED-DEFINE skey               LProj.scheme
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' Description ':C ~
                              bfr{&tmptable}.BillSep ~
                                        COLUMN-LABEL ' BillSep ':C ~
                              bfr{&tmptable}.iLedger[1] ~
                                        COLUMN-LABEL ' Income Ledger ':C ~
                              bfr{&tmptable}.iLedger[2] ~
                                        COLUMN-LABEL ' Survey Ledger ':C ~
                              bfr{&tmptable}.iLedger[3] ~
                                        COLUMN-LABEL ' Admin Ledger ':C ~
                              bfr{&tmptable}.iLedger[4] ~
                                        COLUMN-LABEL ' Capital Debtor ':C ~
                              bfr{&tmptable}.Svgrp[1] ~
                                        COLUMN-LABEL ' Service Group ':C ~
                              bfr{&tmptable}.Svgrp[2] ~
                                        COLUMN-LABEL ' Service Group ':C ~
                              bfr{&tmptable}.Svgrp[3] ~
                                        COLUMN-LABEL ' Service Group ':C ~
                              bfr{&tmptable}.ls ~
                              bfr{&tmptable}.vat%[1] ~
                              bfr{&tmptable}.vat%[2] ~
                              bfr{&tmptable}.vat%[3] ~
                              bfr{&tmptable}.freq[1] ~
                              bfr{&tmptable}.freq[2] ~
                              bfr{&tmptable}.freq[3] ~
                              bfr{&tmptable}.INT
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.scheme ~
                                        COLUMN-LABEL ' Code ':C ~
                              bfr{&tmptable}.DESCRIP ~
                                        FORM "x(40)" COLUMN-LABEL ' Description ':C ~
                               bfr{&tmptable}.ls ~
                                        COLUMN-LABEL ' Type ':C ~
                               bfr{&tmptable}.iLedger[4] ~
                                        COLUMN-LABEL ' Capital Debtor ':C ~
                               bfr{&tmptable}.iLedger[1] ~
                                        COLUMN-LABEL ' Sales Ledger ':C ~
                               bfr{&tmptable}.iLedger[2] ~
                                        COLUMN-LABEL ' Survey Ledger ':C ~
                               bfr{&tmptable}.iLedger[3] ~
                                        COLUMN-LABEL ' Admin Ledger ':C
{varlibrary.i}
DEF VAR wsDes LIKE glmf.DESCRIPTION EXTENT 4.
DEF VAR wsServ LIKE dbsgr.DESCRIP EXTENT 4.
DEF VAR X AS INT.

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 116 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 116 by 18.5.
DEF BUTTON btnSvgrp LABEL "Service Group".
DEF BUTTON btnSvgrp1 LABEL "Service Group".
DEF BUTTON btnSvgrp2 LABEL "Service Group".
DEF BUTTON btnSvgrp3 LABEL "Service Group".
DEF BUTTON btnLedger1.  
DEF BUTTON btnLedger2. 
DEF BUTTON btnLedger3. 

DEF NEW SHARED FRAME frm-main
    brw-{&tmptable} AT ROW 1.8 COL 10
    btn-add AT ROW 20.7 COL 7
    Space(25) btn-edit
    space(25) btn-del
    space(25) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 5
    rect-1 AT ROW 20 COL 5
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D  
     BGCOLOR 8 FGCOLOR 1 SIZE 125 BY 24
    TITLE {&wsTitle} VIEW-AS DIALOG-BOX.

define frame frm-input
    {&skey}                  COLON 30 LABEL "Project" SKIP(.5)
    bfr{&tmptable}.descrip   colon 30 LABEL "Description" FORM "x(40)" SKIP(0.5)
    bfr{&tmptable}.ls        COLON 30 LABEL "Project Type" AUTO-RETURN SPACE(20)
    bfr{&tmptable}.BillSep            LABEL "Billing Separately (Y/N)"
    SKIP(0.5)
    btnLedger               COLON 20 LABEL " Sales Ledger " NO-TAB-STOP
    bfr{&tmptable}.iLedger[1] NO-LABEl
    wsDes[1]         NO-LABEL VIEW-AS TEXT
    btnSvgrp                  COLON 20  NO-LABEL NO-TAB-STOP
    bfr{&tmptable}.Svgrp[1]    NO-LABEl
    wsServ[1] NO-LABEL VIEW-AS TEXT
    bfr{&tmptable}.Freq[1]    LABEL "Frequency"
    bfr{&tmptable}.VAT[1]   LABEL "VAT %" SKIP(0.5)
    btnLedger2                COLON 20 LABEL "Administration Cost Ledger" NO-TAB-STOP 
    bfr{&tmptable}.iLedger[3] NO-LABEl
    wsDes[3]         NO-LABEL VIEW-AS TEXT
    btnSvgrp2                  COLON 20  NO-LABEL NO-TAB-STOP
    bfr{&tmptable}.Svgrp[3]    NO-LABEl
    wsServ[3] NO-LABEL VIEW-AS TEXT
    bfr{&tmptable}.Freq[3]    LABEL "Frequency"
    bfr{&tmptable}.VAT[3]   LABEL "VAT %" SKIP(0.5)
    btnLedger1                COLON 20 LABEL "Survey Cost Ledger" NO-TAB-STOP
    bfr{&tmptable}.iLedger[2] NO-LABEl
    wsDes[2]         NO-LABEL VIEW-AS TEXT
    btnSvgrp1                  COLON 20  NO-LABEL NO-TAB-STOP
    bfr{&tmptable}.Svgrp[2]    NO-LABEl
    wsServ[2] NO-LABEL VIEW-AS TEXT
    bfr{&tmptable}.Freq[2]    LABEL "Frequency"
    bfr{&tmptable}.VAT[2]   LABEL "VAT %" SKIP(0.5)
    btnLedger3                COLON 20 LABEL "Capital Debtors Ledger" NO-TAB-STOP
    bfr{&tmptable}.iLedger[4] NO-LABEl
    wsDes[4]         NO-LABEL VIEW-AS TEXT
    skip(0.5)
    bfr{&tmptable}.INT COLON 30 LABEL "Interest Rate"
    SKIP(1.5)
    btn-ok colon 20 SPACE(50)
    btn-close 
    SKIP(0.5)
    with view-as dialog-box keep-tab-order no-validate
          side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "DATA CAPTURE".
    

/* ***** Triggers **** */
ON 'enter':U OF bfr{&tmptable}.Freq[1]
    OR 'enter':U OF bfr{&tmptable}.Freq[2]
    OR 'enter':U OF bfr{&tmptable}.Freq[3]
DO:
    APPLY 'tab' TO SELF.
    RETURN.
END.

ON ERROR ANYWHERE 
DO:
    RETURN NO-APPLY.
END.

ON 'enter':U OF bfr{&tmptable}.BillSep IN FRAME frm-input
DO:
    IF  INT(bfr{&tmptable}.ls:SCREEN-VALUE) = 2 THEN DO: /*Land Sales */
        ENABLE ALL WITH FRAME frm-input.
        IF LOGICAL(bfr{&tmptable}.BillSep:SCREEN-VALUE) = NO THEN 
            DISABLE btnLedger1 bfr{&tmptable}.iLedger[2] wsDes[2]btnSvgrp1 bfr{&tmptable}.Svgrp[2]
            wsServ[2] bfr{&tmptable}.Freq[2] bfr{&tmptable}.VAT[2] btnLedger2 bfr{&tmptable}.iLedger[3]
             wsDes[3] btnSvgrp2 bfr{&tmptable}.Svgrp[3]
            wsServ[3] bfr{&tmptable}.Freq[3] bfr{&tmptable}.VAT[3] WITH FRAME frm-input.
        ELSE 
        ENABLE btnLedger1 bfr{&tmptable}.iLedger[2] wsDes[2]btnSvgrp1 bfr{&tmptable}.Svgrp[2]
            wsServ[2] bfr{&tmptable}.Freq[2] bfr{&tmptable}.VAT[2] btnLedger2 bfr{&tmptable}.iLedger[3]
             wsDes[3] btnSvgrp2 bfr{&tmptable}.Svgrp[3]
            wsServ[3] bfr{&tmptable}.Freq[3] bfr{&tmptable}.VAT[3] WITH FRAME frm-input.

    END.
    ELSE IF  INT(bfr{&tmptable}.ls:SCREEN-VALUE) = 1 THEN DO:
        HIDE btnLedger1 bfr{&tmptable}.iLedger[2] wsDes[2]btnSvgrp1 bfr{&tmptable}.Svgrp[2]
             wsServ[2] bfr{&tmptable}.Freq[2] bfr{&tmptable}.VAT[2] bfr{&tmptable}.INT
             btnLedger3 bfr{&tmptable}.iLedger[4] wsDes[4] IN FRAME frm-input.
        IF LOGICAL(bfr{&tmptable}.BillSep:SCREEN-VALUE) = NO THEN DO:
            DISABLE btnLedger2 bfr{&tmptable}.iLedger[3] wsDes[3] btnSvgrp2 bfr{&tmptable}.Svgrp[3]
            wsServ[3] bfr{&tmptable}.Freq[3] bfr{&tmptable}.VAT[3] WITH FRAME frm-input.
        END.
        ELSE
            ENABLE btnLedger2 bfr{&tmptable}.iLedger[3] wsDes[3] btnSvgrp2 bfr{&tmptable}.Svgrp[3]
            wsServ[3] bfr{&tmptable}.Freq[3] bfr{&tmptable}.VAT[3] WITH FRAME frm-input.
    END.   
    RETURN.
END.

ON 'tab':U OF bfr{&tmptable}.ls IN FRAME frm-input
    OR 'enter':U OF bfr{&tmptable}.ls IN FRAME frm-input
DO:
    APPLY 'tab' TO bfr{&tmptable}.ls.
    RETURN.
END.

ON CHOOSE OF btnLedger  IN FRAME frm-input
DO:
   ASSIGN X = 1.
  RUN btn-t.
   APPLY 'tab' TO SELF.
   APPLY 'tab' TO bfr{&tmptable}.iLedger[1].
END.
ON CHOOSE OF btnLedger1  IN FRAME frm-input 
DO:
   ASSIGN X = 2.
  RUN btn-t.
   APPLY 'tab' TO SELF.
   APPLY 'tab' TO bfr{&tmptable}.iLedger[2].
END.
ON CHOOSE OF btnLedger2  IN FRAME frm-input
DO:
   ASSIGN X = 3.
  RUN btn-t.
   APPLY 'tab' TO SELF.
   APPLY 'tab' TO bfr{&tmptable}.iLedger[3].
END.
ON CHOOSE OF btnLedger3  IN FRAME frm-input
DO:
   ASSIGN X = 4.
  RUN btn-t.
   APPLY 'tab' TO SELF.
   APPLY 'tab' TO bfr{&tmptable}.iLedger[4].
END.

ON CHOOSE OF btn-ok IN FRAME frm-pick 
    OR 'enter':u OF brw-ledger
    OR 'mouse-select-dblclick' OF brw-ledger
DO: 
   GET CURRENT qry-ledger NO-LOCK NO-WAIT.
   DISPLAY glmf.acct @ bfr{&tmptable}.iLedger[X] glmf.DESCRIPTION @ wsDes[X] WITH FRAME frm-input.
   RETURN. 
END.

ON  'enter':U OF bfr{&tmptable}.iLedger[1] IN FRAME frm-input
    OR 'tab':U OF bfr{&tmptable}.iLedger[1] IN FRAME frm-input
DO:
    FIND FIRST glmf WHERE glmf.acct = DEC(bfr{&tmptable}.iLedger[1]:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE glmf THEN DO:
        wsdes[1] = glmf.DESCRIPTION.
         DISPLAY wsDes[1]  WITH FRAM frm-input.
    END.
    ELSE DO:
        MESSAGE "Invalid Ledger...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.
ON  'enter':U OF bfr{&tmptable}.iLedger[2] IN FRAME frm-input
    OR 'tab':U OF bfr{&tmptable}.iLedger[2] IN FRAME frm-input
DO:
    FIND FIRST glmf WHERE glmf.acct = DEC(bfr{&tmptable}.iLedger[2]:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE glmf THEN DO:
        wsdes[2] = glmf.DESCRIPTION.
         DISPLAY wsdes[2]  WITH FRAM frm-input.
    END.
    ELSE DO:
        MESSAGE "Invalid Ledger...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.
ON  'enter':U OF bfr{&tmptable}.iLedger[3] IN FRAME frm-input
    OR 'tab':U OF bfr{&tmptable}.iLedger[3] IN FRAME frm-input
DO:
    FIND FIRST glmf WHERE glmf.acct = DEC(bfr{&tmptable}.iLedger[3]:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE glmf THEN DO:
        wsdes[3] = glmf.DESCRIPTION.
         DISPLAY wsdes[3]  WITH FRAM frm-input.
    END.
    ELSE DO:
        MESSAGE "Invalid Ledger...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.
ON  'enter':U OF bfr{&tmptable}.iLedger[4] IN FRAME frm-input
    OR 'tab':U OF bfr{&tmptable}.iLedger[4] IN FRAME frm-input
DO:
    FIND FIRST glmf WHERE glmf.acct = DEC(bfr{&tmptable}.iLedger[4]:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE glmf THEN DO:
        wsdes[4] = glmf.DESCRIPTION.
         DISPLAY wsdes[4]  WITH FRAM frm-input.
    END.
    ELSE DO:
        MESSAGE "Invalid Ledger...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON CHOOSE OF btnSvgrp  IN FRAME frm-input
DO:
   ASSIGN X = 1.
  RUN btn-S.
   APPLY 'tab' TO SELF.
   APPLY 'tab' TO bfr{&tmptable}.Svgrp[1].
END.
ON CHOOSE OF btnSvgrp1  IN FRAME frm-input
DO:
   ASSIGN X = 2.
  RUN btn-S.
   APPLY 'tab' TO SELF.
   APPLY 'tab' TO bfr{&tmptable}.Svgrp[2].
END.
ON CHOOSE OF btnSvgrp2  IN FRAME frm-input
DO:
   ASSIGN X = 3.
  RUN btn-S.
   APPLY 'tab' TO SELF.
    APPLY 'tab' TO bfr{&tmptable}.Svgrp[3].
END.


ON CHOOSE OF btn-ok IN FRAME frm-PickService 
    OR 'enter':u OF brw-Service
    OR 'mouse-select-dblclick' OF brw-Service
DO: 
   GET CURRENT qry-Service NO-LOCK NO-WAIT.
   DISPLAY dbsgr.Sgrp  @ bfr{&tmptable}.Svgrp[X] dbsgr.Descrip @ wsServ[X] WITH FRAME frm-input.
   RETURN. 
END.

ON CHOOSE OF btn-Add IN FRAME frm-main DO:
     btn-ok:LABEL IN  FRAME frm-input = "Save".
    RUN proc-input.
    RELEASE bfr{&tmptable}.
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
    RELEASE bfr{&tmptable}.
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
         FIND FIRST glmf WHERE glmf.acct = DEC(bfr{&tmptable}.iLedger[1]:SCREEN-VALUE IN FRAME frm-input) NO-ERROR.
     IF NOT AVAILABLE glmf THEN DO:
        MESSAGE "Invalid Ledger supplied...no record saved" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
     END.
     FIND FIRST LProj WHERE LProj.Scheme  = wsid NO-ERROR.
      IF AVAILABLE LProj THEN DO:
          MESSAGE  "Project already exist" VIEW-AS ALERT-BOX.
          RETURN NO-APPLY.
      END.
      ELSE DO:
          CREATE bfr{&tmptable}. 
          ASSIGN bfr{&tmptable}.Scheme = wsid
                 bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input
                 bfr{&tmptable}.iLedger[1]  = DEC(bfr{&tmptable}.iLedger[1]:SCREEN-VALUE)
                 bfr{&tmptable}.iLedger[2]  = DEC(bfr{&tmptable}.iLedger[2]:SCREEN-VALUE)
                 bfr{&tmptable}.iLedger[3]  = DEC(bfr{&tmptable}.iLedger[3]:SCREEN-VALUE)
                 bfr{&tmptable}.iLedger[4]  = DEC(bfr{&tmptable}.iLedger[4]:SCREEN-VALUE)
                 bfr{&tmptable}.Svgrp[1]  = INT(bfr{&tmptable}.Svgrp[1]:SCREEN-VALUE)
                 bfr{&tmptable}.Svgrp[2]  = INT(bfr{&tmptable}.Svgrp[2]:SCREEN-VALUE)
                 bfr{&tmptable}.Svgrp[3]  = INT(bfr{&tmptable}.Svgrp[3]:SCREEN-VALUE)
                 bfr{&tmptable}.Freq[1]  = INT(bfr{&tmptable}.Freq[1]:SCREEN-VALUE)
                 bfr{&tmptable}.Freq[2]  = INT(bfr{&tmptable}.Freq[2]:SCREEN-VALUE)
                 bfr{&tmptable}.Freq[3]  = INT(bfr{&tmptable}.Freq[3]:SCREEN-VALUE)
                 bfr{&tmptable}.Freq[4]  = INT(bfr{&tmptable}.Freq[1]:SCREEN-VALUE)
                 bfr{&tmptable}.BillSep  = LOGICAL(bfr{&tmptable}.BillSep:SCREEN-VALUE)
                 bfr{&tmptable}.INT  = DEC(bfr{&tmptable}.INT:SCREEN-VALUE)
                 bfr{&tmptable}.vat%[1]  = DEC(bfr{&tmptable}.Vat%[1]:SCREEN-VALUE)
                 bfr{&tmptable}.vat%[2]  = DEC(bfr{&tmptable}.Vat%[2]:SCREEN-VALUE)
                 bfr{&tmptable}.Vat%[3]  = DEC(bfr{&tmptable}.Vat%[3]:SCREEN-VALUE)
                 bfr{&tmptable}.ls  = INT(bfr{&tmptable}.ls:SCREEN-VALUE).
          RELEASE {&tmptable}.
          brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
          CLEAR FRAME frm-input ALL.
          APPLY 'entry' TO {&skey} IN FRAME frm-Input.
      END.
    END.
    ELSE DO:
        FIND FIRST glmf WHERE glmf.acct = DEC(bfr{&tmptable}.iLedger[1]:SCREEN-VALUE IN FRAME frm-input) NO-ERROR.
        IF NOT AVAILABLE glmf THEN DO:
           MESSAGE "Invalid Ledger supplied...no record saved" VIEW-AS ALERT-BOX.
           RETURN NO-APPLY.
        END.
        ASSIGN bfr{&tmptable}.Descrip = bfr{&tmptable}.Descrip:SCREEN-VALUE IN FRAME frm-input
               bfr{&tmptable}.iLedger[1]  = DEC(bfr{&tmptable}.iLedger[1]:SCREEN-VALUE)
                 bfr{&tmptable}.iLedger[2]  = DEC(bfr{&tmptable}.iLedger[2]:SCREEN-VALUE)
                 bfr{&tmptable}.iLedger[3]  = DEC(bfr{&tmptable}.iLedger[3]:SCREEN-VALUE)
                 bfr{&tmptable}.iLedger[4]  = DEC(bfr{&tmptable}.iLedger[4]:SCREEN-VALUE)
                 bfr{&tmptable}.Svgrp[1]  = INT(bfr{&tmptable}.Svgrp[1]:SCREEN-VALUE)
                 bfr{&tmptable}.Svgrp[2]  = INT(bfr{&tmptable}.Svgrp[2]:SCREEN-VALUE)
                 bfr{&tmptable}.Svgrp[3]  = INT(bfr{&tmptable}.Svgrp[3]:SCREEN-VALUE)
                 bfr{&tmptable}.Freq[1]  = INT(bfr{&tmptable}.Freq[1]:SCREEN-VALUE)
                 bfr{&tmptable}.Freq[2]  = INT(bfr{&tmptable}.Freq[2]:SCREEN-VALUE)
                 bfr{&tmptable}.Freq[3]  = INT(bfr{&tmptable}.Freq[3]:SCREEN-VALUE)
                 bfr{&tmptable}.Freq[4]  = INT(bfr{&tmptable}.Freq[1]:SCREEN-VALUE)
                 bfr{&tmptable}.BillSep  = LOGICAL(bfr{&tmptable}.BillSep:SCREEN-VALUE)
                 bfr{&tmptable}.INT  = DEC(bfr{&tmptable}.INT:SCREEN-VALUE)
                 bfr{&tmptable}.vat%[1]  = DEC(bfr{&tmptable}.Vat%[1]:SCREEN-VALUE)
                 bfr{&tmptable}.vat%[2]  = DEC(bfr{&tmptable}.Vat%[2]:SCREEN-VALUE)
                 bfr{&tmptable}.Vat%[3]  = DEC(bfr{&tmptable}.Vat%[3]:SCREEN-VALUE)
                 bfr{&tmptable}.ls  = INT(bfr{&tmptable}.ls:SCREEN-VALUE).
        RELEASE {&tmptable}.
    END.
END.

ON CHOOSE OF btn-Del DO:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.Scheme.
    FIND FIRST LandSale WHERE LandSale.Scheme  = bfr{&tmptable}.Scheme NO-ERROR.
    IF AVAILABLE LandSale THEN DO:
        MESSAGE "Project has related records - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE LandSale THEN DO:
     DELETE bfr{&tmptable}.
     Method-Status = brw-{&tmptable}:DELETE-SELECTED-ROWS().
    END.
    APPLY 'entry' TO btn-exit.
END.

/********** MAIN LOGIC **********/
OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.Scheme <> 0  NO-LOCK .
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
CLOSE QUERY qry-{&tmptable}.
HIDE FRAME frm-main.

PROCEDURE proc-input:
   CLEAR FRAME frm-input ALL.
   ENABLE ALL WITH FRAME frm-input.
   {&skey}:SCREEN-VALUE = "0".
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
         CLOSE QUERY qry-{&tmptable}.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.Scheme <> 0  NO-LOCK.
   HIDE FRAME frm-input.
END.

PROCEDURE proc-edit:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = int(bfr{&tmptable}.Scheme).
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.Scheme  = wsid NO-ERROR.
    /*FIND FIRST glmf WHERE glmf.acct = bfr{&tmptable}.iLedger[1] NO-ERROR.
    IF AVAILABLE glmf THEN
        wsdes = glmf.DESCRIPTION.
    ELSE wsDes = "Invalid Ledger". */
    DO X = 1 TO 4:
        FIND FIRST glmf WHERE glmf.acct = bfr{&tmptable}.iLedger[X] NO-LOCK NO-ERROR.
        IF AVAILABLE glmf THEN
            wsDes[X] = glmf.DESCRIPTION.
        ELSE wsDes[X] = "Invalid Ledger".
        FIND FIRST dbsgr WHERE dbsgr.Sgrp = bfr{&tmptable}.svgrp[X] NO-LOCK NO-ERROR.
        IF AVAILABLE dbsgr THEN
            wsServ[X] = dbsgr.DESCRIP.
        ELSE wsServ[X] = "Invalid Service".
    END.
    DISPLAY wsid @ {&skey} {&updfields} wsDes wsServ[1] wsServ[2] wsServ[3]  WITH FRAME frm-input.
    ENABLE ALL EXCEPT {&skey} WITH FRAME frm-input.
    IF  bfr{&tmptable}.ls = 2 THEN DO:
        VIEW btnLedger1 bfr{&tmptable}.iLedger[2] wsDes[2]btnSvgrp1 bfr{&tmptable}.Svgrp[2]
            wsServ[2] bfr{&tmptable}.Freq[2] bfr{&tmptable}.VAT[2] bfr{&tmptable}.INT
            btnLedger3 bfr{&tmptable}.iLedger[4] wsDes[4] IN FRAME frm-input.
    END.
    ELSE DO:
        HIDE btnLedger1 bfr{&tmptable}.iLedger[2] wsDes[2]btnSvgrp1 bfr{&tmptable}.Svgrp[2]
            wsServ[2] bfr{&tmptable}.Freq[2] bfr{&tmptable}.VAT[2] bfr{&tmptable}.INT
            btnLedger3 bfr{&tmptable}.iLedger[4] wsDes[4] IN FRAME frm-input.
    END.
    WAIT-FOR CHOOSE OF btn-close 
          OR CHOOSE OF btn-ok OR close of THIS-PROCEDURE IN FRAME frm-input.
    CLOSE QUERY qry-{&tmptable}.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.Scheme <> 0  NO-LOCK.
   HIDE FRAME frm-input.
 END.

PROCEDURE btn-t:
  VIEW FRAME frm-pick.
  OPEN QUERY qry-ledger FOR EACH glmf NO-LOCK.
  ENABLE ALL WITH FRAME frm-pick.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pick 
          OR close of THIS-PROCEDURE IN FRAME frm-pick
          OR CHOOSE OF btn-ok IN FRAME frm-pick 
          OR 'enter':u OF brw-ledger
          OR 'mouse-select-dblclick' OF brw-ledger.
  CLOSE QUERY qry-ledger.
  HIDE FRAME frm-pick.
  RETURN.
END.

PROCEDURE btn-S:
  VIEW FRAME frm-PickService.
  OPEN QUERY qry-Service FOR EACH dbsgr NO-LOCK.
  ENABLE ALL WITH FRAME frm-PickService.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-PickService 
          OR close of THIS-PROCEDURE IN FRAME frm-PickService
          OR CHOOSE OF btn-ok IN FRAME frm-PickService 
          OR 'enter':u OF brw-Service
          OR 'mouse-select-dblclick' OF brw-Service.
  CLOSE QUERY qry-Service.
  HIDE FRAME frm-PickService.
  APPLY 'tab' TO SELF.
  RETURN.
END.
