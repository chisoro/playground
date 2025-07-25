/* Program.................dbjnl01.p
   Notes:................. Debtors Journal Capture
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
&SCOPED-DEFINE tmptable        bfrBatch
&SCOPED-DEFINE tmpFields       bfr{&tmptable}.trDATE ~
                                        COLUMN-LABEL ' Transaction Date ':C ~
                               bfr{&tmptable}.REF ~
                                        COLUMN-LABEL ' Reference ':C ~
                               bfr{&tmptable}.dbAcc ~
                                        COLUMN-LABEL ' Account ':C ~
                               bfr{&tmptable}.AMT ~
                                        COLUMN-LABEL ' Amount ':C ~
                              bfr{&tmptable}.DESCRIP ~
                                        COLUMN-LABEL ' Description ':C

DEF STREAM a.
DEF STREAM b.
DEF var w-orientation AS CHAR      INITIAL "Landscape"    NO-UNDO.
DEF var w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF var w-fontsize    AS INT       INITIAL 10.0          NO-UNDO.
DEF VAR wsSearch   LIKE dbcmf.NAME.
DEF VAR hCol        AS WIDGET-HANDLE.
DEF VAR hBr         AS WIDGET-HANDLE.
DEF SHARED VAR varUser LIKE simusr.usercode.
DEF VAR wsid LIKE  dbBCtr.intBatch.
DEF VAR Current-Record AS ROWID.
DEF VAR method-status  AS LOGICAL.
DEF VAR wsdel          AS LOGICAL INITIAL NO.
DEF VAR wsbatch     LIKE  dbBCtr.intBatch.
DEF VAR wsrec       LIKE  dbBatch.seq.
DEF VAR wscurr      LIKE SIMCTR.CURPER.
DEF VAR wsclosed    LIKE  SIMCTR.CLOSEPER.
DEF VAR wsper       LIKE simctr.closeper.
DEF VAR wsDate      LIKE dbBCtr.BDate.
DEF VAR wsAmt       LIKE dbBCtr.Amt.
DEF VAR wsbtn       AS CHAR.
DEF VAR wsAge       LIKE dbBatch.Age.
DEF VAR wsFile      AS CHAR.

DEF BUTTON  btnSearch LABEL "Search".
DEF BUTTON btn-Add   LABEL "ADD".
DEF BUTTON btn-Add1   LABEL "ADD".
DEF BUTTON btn-del   LABEL "DEL".
DEF BUTTON btn-exit  LABEL "CLOSE".
DEF BUTTON btn-Edit  LABEL "EDIT".
DEF BUTTON btn-cancel LABEL "CANCEL".
DEF BUTTON btn-ok     LABEL "  OK  ".
DEF BUTTON btnAcc    LABEL "Account".
DEF BUTTON btnLedger LABEL "Ledger ".
DEF BUTTON btnService LABEL "Service ".
DEF BUTTON btnProj   LABEL "Project".
DEF BUTTON btnFund   LABEL "Segment/Fund".
DEF BUTTON btn-Prn  LABEL "PRINT".
DEF BUTTON btn-Exp  LABEL "EXPORT".

FIND FIRST SIMCTR NO-ERROR.
FIND FIRST simusr WHERE simusr.usercode = varUser NO-ERROR.

DEF BUFFER bfr{&tmptable} FOR dbBatch PRESELECT.
DEF BUFFER bfdbCtr        FOR dbBCtr PRESELECT.
DEF BUFFER bfrBatch       FOR dbBatch.

DEFINE QUERY qry-bfdbCtr FOR  bfdbCtr scrolling.
DEF BROWSE brw-bfdbCtr QUERY qry-bfdbCtr
        DISPLAY bfdbCtr.UID COLUMN-LABEL "User" 
                bfdbCtr.period WIDTH 10 COLUMN-LABEL "Period"
                bfdbCtr.BDate WIDTH 12 COLUMN-LABEL "Batch Date"
                bfdbCtr.intBatch COLUMN-LABEL "Batch Number" 
                bfdbCtr.AMT[1]COLUMN-LABEL "Dr Amount" FORM "zzz,zzz,zzz,zz9.99-" 
                bfdbCtr.AMT[2] COLUMN-LABEL "Cr Amount" FORM "zzz,zzz,zzz,zz9.99-"
        WITH 20 DOWN SEPARATORS.

DEFINE QUERY qry-bfr{&tmptable} FOR  bfr{&tmptable} scrolling.
DEF BROWSE brw-bfr{&tmptable} QUERY qry-bfr{&tmptable}
    DISPLAY bfr{&tmptable}.seq  
            bfr{&tmptable}.trDATE  
            bfr{&tmptable}.REF  COLUMN-LABEL "Reference"
            bfr{&tmptable}.dbAcc COLUMN-LABEL "Account" 
            bfr{&tmptable}.Sgrp
            bfr{&tmptable}.DESCRIP WIDTH 35
            bfr{&tmptable}.AMT FORM "zzz,zzz,zzz,zz9.99-"
            bfr{&tmptable}.Vat  /* */
    WITH 20 DOWN SEPARATORS.

DEF    QUERY qry-ledger FOR glmf SCROLLING.
DEF BROWSE brw-ledger QUERY qry-ledger
    DISPLAY glmf.acct glmf.DESCRIPTION COLUMN-LABEL "Description" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pick 
    brw-ledger AT ROW 2 COL 5
    skip(0.2)
    btn-ok colon 5
    btn-exit colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Ledger Account Selection".

DEF    QUERY qry-Service FOR dbsgr SCROLLING.
DEF BROWSE brw-Service QUERY qry-Service
    DISPLAY dbsgr.Sgrp dbsgr.Descrip  COLUMN-LABEL "Decsription" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pickService 
    brw-Service AT ROW 2 COL 5
    skip(0.2)
    btn-ok colon 5
    btn-exit colon 60 LABEL "Close"
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Service Group Selection".

DEF    QUERY qry-pickAcc FOR dbcmf SCROLLING.
DEF BROWSE brw-pickAcc QUERY qry-pickAcc
    DISPLAY dbcmf.dbAcc dbcmf.Name dbcmf.StandNo  
     WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pickAcc 
    brw-pickAcc AT ROW 2 COL 5
    skip(0.2)
    btn-ok colon 5
    btn-Exit colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Account Selection".

DEF    QUERY qry-Fund FOR GlFund SCROLLING.
DEF BROWSE brw-Fund  QUERY qry-Fund 
    DISPLAY GLFUND.FUND COLUMN-LABEL "Segment" GLFUND.DESCRIP COLUMN-LABEL "Decsription" 
    WIDTH 50 WITH 20 DOWN SEPARATORS.
DEFINE FRAME frm-pickFund 
    brw-Fund AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-exit colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Segment/Fund Selection".

DEF    QUERY qry-PickProj FOR glProj SCROLLING.
DEF BROWSE brw-PickProj QUERY qry-PickProj
    DISPLAY GLProj.Proj GLProj.DESCRIP COLUMN-LABEL "Description" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-PickProj 
    brw-PickProj AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btn-exit colon 60 
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Project Selection".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 graphic-edge  NO-FILL
     SIZE 80 BY 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 80 BY 17.5.

DEFINE RECTANGLE rect-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 130 BY 2.3.

DEFINE RECTANGLE rect-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 130 BY 18.5.

DEFINE FRAME frm-input
      bfr{&tmptable}.Seq    COLON 20  LABEL "Record " SKIP(0.2)
      bfr{&tmptable}.trDATE COLON 20  LABEL "Transaction Date " SKIP(0.2)
      bfr{&tmptable}.REF    COLON 20  LABEL  "Reference Number " SKIP(0.2)
      btnAcc                COLON 10  NO-TAB-STOP 
      bfr{&tmptable}.dbAcc       NO-LABEL 
      dbcmf.Name             view-as text NO-LABEL SKIP(0.2)
      btnService           COLON 10 NO-TAB-STOP 
      bfr{&tmptable}.Sgrp      NO-LABEL 
      dbsgr.Descrip           view-as text NO-LABEL SKIP(0.2)
      bfr{&tmptable}.DESCRIP COLON 20 LABEL "Narration " SKIP(0.2)
      bfr{&tmptable}.age     COLON 20 LABEL "AGE"        SKIP(0.2)
      bfr{&tmptable}.AMT     COLON 20  LABEL "Amount " FORM "zzz,zzz,zzz,zz9.99-" SKIP(0.2)
      bfr{&tmptable}.vat     COLON 20  LABEL "VAT "  SKIP(0.2)
      btnLedger              COLON 10  NO-TAB-STOP
      bfr{&tmptable}.ILedger     NO-LABEL
      glmf.DESCRIPTION       view-as text NO-LABEL SKIP(0.2)
      btnFund                 COLON 4  no-tab-stop
      bfr{&tmptable}.Fund              NO-LABEL
      glfund.descrip  view-as text NO-LABEL skip(0.2)
      btnProj                 COLON 11  no-tab-stop
      bfr{&tmptable}.Proj              NO-LABEL
      glProj.descrip  view-as text no-label skip(1)
      btn-ok colon 5 
      btn-cancel colon 60  SKIP(1)
    with view-as dialog-box NO-VALIDATE KEEP-TAB-ORDER
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "DATA CAPTURE".
    
DEF FRAME frm-main
    brw-bfdbCtr AT ROW 2 COL 3
    btn-add AT ROW 19.7 COL 5 SPACE(6)
    btn-edit SPACE(6)
    btn-Prn  SPACE(6)
    btn-Exp  SPACE(6) 
    btn-del SPACE(6)
    btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 19 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
    SIZE 85 BY 23
    TITLE "Journal Maintenance" view-as dialog-box.

DEF FRAME frm-bfrBatch
     wsBatch   LABEL "Batch Number" AT ROW 2 COL 5 SPACE(2)
     wsDate    LABEL "Batch Date"   SPACE(2) 
     wsper     LABEL "Accounting Period"  SPACE(2)
     wsAmt[1]   LABEL "DR"  NO-TAB-STOP space(2) 
     wsAmt[2]   LABEL "CR" NO-TAB-STOP SPACE(2) 
     brw-bfr{&tmptable} NO-TAB-STOP AT ROW 3.5 COL 5 
    btn-add1 AT ROW 20.7 COL 10
    space(25) btn-edit
    space(25) btn-del
    space(25) btn-cancel SKIP(1)
    rect-4 AT ROW 1.4 COL 3
    rect-3 AT ROW 20 COL 3
    WITH TITLE "Journal Data Capture"
    1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
    SIZE 135 BY 24 view-as DIALOG-BOX.


DEF FRAME Search-opt
    wsSearch  COLON 20 LABEL "Search Value"
    btnSearch
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED.
         
FORM bfrBatch.Seq     LABEL "SEQ"
     bfrBatch.trDate  LABEL "DATE"
     bfrBatch.dbacc   LABEL "ACCOUNT"
     bfrBatch.Ref     LABEL "REFERENCE"
     bfrBatch.Sgrp    LABEL "SERVICE"
     bfrBatch.Amt     LABEL "AMOUNT" FORM "zzz,zzz,zz9.99-"
     bfrBatch.Vat     LABEL "VAT"
     bfrBatch.Iledger LABEL "LEDGER"
     bfrBatch.DESCRIP LABEL "NARRATION"
     HEADER skip(1) "DEBTORS JOURNAL LISTING N REPORT FOR BATCH: " wsBatch 
    "DATE: " wsDate  "PERIOD  :" wsPer  "Page: " AT 90 PAGE-NUMBER SKIP(1) 
    "        Batch Captured by:" simusr.Name SKIP(2) 
    WITH NO-LABEL DOWN STREAM-IO FONT 10 WIDTH 164 CENTERED NO-BOX FRAME frmRpt.
                          
/* *** Triggers to input frame frm-input*** */
ON 'esc' ANYWHERE 
DO:
    RETURN NO-APPLY.
END. 

/*ON ERROR ANYWHERE 
DO:
    APPLY 'close' TO THIS-PROCEDURE.
    RETURN .
END.*/

ON 'start-search':u OF BROWSE brw-Ledger
    OR 'start-search':u OF BROWSE brw-PickAcc
    OR 'start-search':u OF BROWSE brw-bfr{&tmptable}
DO:
    run SEARCH.ip.
END.

ON 'LEAVE':U OF bfr{&tmptable}.age IN FRAME frm-Input 
DO:
    IF INT(bfr{&tmptable}.age:SCREEN-VALUE) < 1 OR INT(bfr{&tmptable}.age:SCREEN-VALUE) > 14 THEN
    DO:
        MESSAGE "Age can only be BETWEEN 1 AND 14...." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON CHOOSE OF btnAcc IN FRAME  frm-input
DO: 
  VIEW FRAME frm-pickAcc.
  OPEN QUERY qry-pickAcc FOR EACH dbcmf WHERE (dbcmf.accstat = 0 OR dbcmf.accstat = 95) NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickAcc.
  WAIT-FOR CHOOSE OF btn-exit IN FRAME frm-pickAcc 
          OR close of THIS-PROCEDURE IN FRAME frm-pickAcc 
          OR CHOOSE OF btn-ok IN FRAME frm-pickAcc 
          OR 'enter':u OF brw-pickAcc
          OR 'mouse-select-dblclick' OF brw-pickAcc.
  CLOSE QUERY qry-pickAcc.
  HIDE FRAME frm-pickAcc.
  APPLY 'entry' TO bfr{&tmptable}.dbAcc IN FRAME frm-input.
END.

ON CHOOSE OF btn-ok IN FRAME frm-pickAcc 
    OR 'enter':u OF brw-pickAcc 
    OR 'mouse-select-dblclick' OF brw-pickAcc 
DO: 
   GET CURRENT qry-pickAcc  NO-LOCK NO-WAIT.
   DISPLAY  dbcmf.dbacc  @ bfr{&tmptable}.dbAcc dbcmf.NAME WITH FRAME frm-input.
   RETURN.
END.

ON 'enter' OF  bfr{&tmptable}.dbAcc IN FRAME frm-input 
    OR 'tab' OF  bfr{&tmptable}.dbAcc IN FRAME frm-input
DO:
    IF   DEC(bfr{&tmptable}.dbAcc:SCREEN-VALUE) = 0 THEN
    DO:
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        FIND FIRST dbcmf WHERE dbcmf.dbAcc = DEC( bfr{&tmptable}.dbAcc:SCREEN-VALUE) AND (dbcmf.accstat = 0 OR dbcmf.accstat = 95) NO-LOCK NO-ERROR.
        IF NOT AVAILABLE dbcmf THEN DO:
              MESSAGE "Account does not exist or is not active" VIEW-AS ALERT-BOX.
              APPLY 'entry':u TO SELF.
              RETURN NO-APPLY.
        END.
        DISPLAY dbcmf.NAME WITH FRAME frm-input.
    END.
END.

ON LEAVE OF  bfr{&tmptable}.Sgrp IN FRAME frm-input 
DO:
    IF  int( bfr{&tmptable}.Sgrp:SCREEN-VALUE) = 0 THEN
    DO:
        MESSAGE "Invalid Service entered" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        FIND FIRST dbsgr WHERE dbsgr.Sgrp = int(bfr{&tmptable}.Sgrp:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF NOT AVAILABLE dbsgr THEN DO:
              MESSAGE "Service does not exist" VIEW-AS ALERT-BOX.
              APPLY 'entry':u TO SELF.
              RETURN NO-APPLY.
        END.
        DISPLAY dbsgr.Descrip WITH FRAME frm-input.
    END.
END.

ON CHOOSE OF btnService IN FRAME frm-input
DO:
  VIEW FRAME frm-pickService.
  OPEN QUERY qry-Service FOR EACH dbsgr NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickService.
  WAIT-FOR CHOOSE OF btn-exit IN FRAME frm-pickService 
          OR close of THIS-PROCEDURE IN FRAME frm-pickService
          OR CHOOSE OF btn-ok IN FRAME frm-pickService 
          OR 'enter':u OF brw-Service
          OR 'mouse-select-dblclick' OF brw-Service.
  CLOSE QUERY qry-Service.
  HIDE FRAME frm-pickService.
  APPLY 'entry' TO bfr{&tmptable}.Sgrp IN FRAME frm-Input.
END.

ON CHOOSE OF btn-ok IN FRAME frm-pickService 
    OR 'enter':u OF brw-Service
    OR 'mouse-select-dblclick' OF brw-Service
DO: 
   GET CURRENT qry-Service NO-LOCK NO-WAIT.
   DISPLAY dbsgr.Sgrp @ bfr{&tmptable}.Sgrp dbsgr.Descrip  WITH FRAME frm-input.
   RETURN.
END.

ON CHOOSE OF btnLedger IN FRAME frm-input
    OR CHOOSE OF btnLedger IN FRAME frm-input
DO:
  VIEW FRAME frm-pick.
  OPEN QUERY qry-ledger FOR EACH glmf NO-LOCK.
  ENABLE ALL WITH FRAME frm-pick.
  WAIT-FOR CHOOSE OF btn-exit IN FRAME frm-pick 
          OR close of THIS-PROCEDURE IN FRAME frm-pick
          OR CHOOSE OF btn-ok IN FRAME frm-pick 
          OR 'enter':u OF brw-ledger
          OR 'mouse-select-dblclick' OF brw-ledger.
  CLOSE QUERY qry-ledger.
  HIDE FRAME frm-pick.
  APPLY 'entry' TO bfr{&tmptable}.ILedger IN FRAME frm-Input.
END.

ON CHOOSE OF btn-ok IN FRAME frm-pick 
    OR 'enter':u OF brw-ledger
    OR 'mouse-select-dblclick' OF brw-ledger
DO: 
   GET CURRENT qry-ledger NO-LOCK NO-WAIT.
   DISPLAY glmf.acct @ bfr{&tmptable}.ILedger glmf.DESCRIPTION WITH FRAME frm-input.
   RETURN. 
END.

ON "enter" OF  bfr{&tmptable}.Iledger IN FRAME frm-input 
    OR "tab" OF  bfr{&tmptable}.Iledger IN FRAME frm-input
DO:
    FIND FIRST glmf WHERE glmf.acct = DEC(bfr{&tmptable}.Iledger:SCREEN-VALUE) NO-LOCK NO-ERROR.
     IF NOT AVAILABLE glmf THEN DO:
        MESSAGE "Ledger does not exist" VIEW-AS ALERT-BOX.
        APPLY 'entry':u TO SELF.
        RETURN NO-APPLY.
     END.
     ELSE DO:
            FIND FIRST glfund WHERE glfund.fund = glmf.fund NO-LOCK NO-ERROR.
            FIND FIRST glproj WHERE glproj.proj = glmf.proj NO-LOCK NO-ERROR.
            DISPLAY glmf.fund @ bfr{&tmptable}.fund
                    glmf.Proj @ bfr{&tmptable}.Proj 
                    glFund.descrip glProj.descrip glmf.descrip WITH FRAME frm-input.
           IF glmf.fund <> 0 THEN
                DISABLE btnFund bfr{&tmptable}.fund WITH FRAME frm-Input.
            ELSE DO:
                ENABLE btnFund bfr{&tmptable}.Fund WITH FRAME frm-Input.
                APPLY 'entry' TO  bfr{&tmptable}.Fund IN FRAME frm-Input.
            END.
            IF glm.proj <> 0 THEN
                DISABLE btnProj bfr{&tmptable}.Proj WITH FRAME frm-Input.
            ELSE DO:
                 ENABLE btnProj bfr{&tmptable}.Proj WITH FRAME frm-Input.
                APPLY 'entry' TO  bfr{&tmptable}.proj IN FRAME frm-Input.
            END.
     END.
     RETURN.
END.

ON CHOOSE OF btnProj IN FRAME frm-input
DO:
  VIEW FRAME frm-pickProj.
  OPEN QUERY qry-PickProj FOR EACH glProj NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickProj.
  WAIT-FOR CHOOSE OF btn-exit IN FRAME frm-pickProj 
          OR close of THIS-PROCEDURE IN FRAME frm-pickProj
          OR CHOOSE OF btn-ok IN FRAME frm-pickProj 
          OR 'enter':u OF brw-PickProj
          OR 'mouse-select-dblclick' OF brw-PickProj.
  CLOSE QUERY qry-PickProj.
  HIDE FRAME frm-pickProj.
  APPLY 'tab' TO btnProj IN FRAME frm-input.
  APPLY 'entry' TO bfr{&tmptable}.proj IN FRAME frm-Input.
  RETURN. 
END.

ON 'enter':U OF bfr{&tmptable}.proj 
DO:
    FIND FIRST glproj WHERE glproj.proj = INT(bfr{&tmptable}.proj:SCREEN-VALUE) NO-ERROR.
    IF NOT AVAILABLE glproj THEN DO:
        MESSAGE "Invalid Project entered" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        DISPLAY glproj.Descrip WITH FRAME frm-input.
    END.
    RETURN.
END.

ON CHOOSE OF btn-ok IN FRAME frm-PickProj 
    OR 'enter':u OF brw-PickProj
    OR 'mouse-select-dblclick' OF brw-PickProj
DO: 
   GET CURRENT qry-PickProj EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY glProj.Proj @ bfr{&tmptable}.Proj glProj.DESCRIPTION  WITH FRAME frm-input.
   RETURN.
END.


ON CHOOSE OF btnFund IN FRAME frm-input
DO:
  VIEW FRAME frm-pickFund.
  OPEN QUERY qry-Fund FOR EACH glFund NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickFund.
  WAIT-FOR CHOOSE OF btn-exit IN FRAME frm-pickFund 
          OR close of THIS-PROCEDURE IN FRAME frm-pickFund
          OR CHOOSE OF btn-ok IN FRAME frm-pickFund 
          OR 'enter':u OF brw-Fund
          OR 'mouse-select-dblclick' OF brw-Fund.
  CLOSE QUERY qry-Fund.
  HIDE FRAME frm-pickFund.
  APPLY 'entry' TO bfr{&tmptable}.Fund IN FRAME frm-Input.

END.

ON CHOOSE OF btn-ok IN FRAME frm-PickFund 
    OR 'enter':u OF brw-Fund
    OR 'mouse-select-dblclick' OF brw-Fund
DO: 
   GET CURRENT qry-Fund NO-LOCK NO-WAIT.
   DISPLAY glFund.Fund @ bfr{&tmptable}.Fund glFund.DESCRIP  WITH FRAME frm-input.
   RETURN.
END.

ON 'enter':U OF bfr{&tmptable}.Fund 
DO:
    FIND FIRST glFund WHERE glFund.Fund = INT(bfr{&tmptable}.Fund:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE glFund THEN DO:
        MESSAGE "Invalid Segment/Fund entered" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        DISPLAY glFund.Descrip WITH FRAME frm-input.
        APPLY 'tab' TO bfr{&tmptable}.Fund IN FRAME frm-input.
    END.
    RETURN.
END.

ON 'choose':U OF btn-ok IN FRAME frm-input  
DO:
   IF NOT CAN-FIND(dbcmf WHERE dbcmf.dbacc = DEC(bfr{&tmptable}.dbAcc:SCREEN-VALUE)) AND (dbcmf.accstat = 0 OR dbcmf.accstat = 95) THEN DO:
        MESSAGE "Invalid Account entered .. no record saved" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END. 
    ELSE IF NOT CAN-FIND(glmf WHERE glmf.acct = DEC(bfr{&tmptable}.ILedger:SCREEN-VALUE)) THEN DO:
            MESSAGE "Invalid Ledger entered .. no record saved" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
        ELSE IF  DEC( bfr{&tmptable}.dbAcc:SCREEN-VALUE) = 0 THEN
        DO:
             APPLY "CLOSE":U TO THIS-PROCEDURE.
             RETURN NO-APPLY.
        END. 
        ELSE IF wsbtn = "New" THEN DO:
            CREATE  bfr{&tmptable}.
             ASSIGN  bfr{&tmptable}.ILedger  = DEC(bfr{&tmptable}.ILedger:SCREEN-VALUE)
                     bfr{&tmptable}.Dept     = glmf.dept
                     bfr{&tmptable}.fund     = INT(bfr{&tmptable}.fund:SCREEN-VALUE)
                     bfr{&tmptable}.Proj     = INT(bfr{&tmptable}.Proj:SCREEN-VALUE)
                     bfr{&tmptable}.intBatch = wsbatch
                     bfr{&tmptable}.seq      = INT( bfr{&tmptable}.seq:SCREEN-VALUE)
                     bfr{&tmptable}.dbAcc    = DEC( bfr{&tmptable}.dbAcc:SCREEN-VALUE)
                     bfr{&tmptable}.Sgrp    = INT( bfr{&tmptable}.Sgrp:SCREEN-VALUE)
                     bfr{&tmptable}.AMT      = DEC( bfr{&tmptable}.AMT:SCREEN-VALUE)
                     bfr{&tmptable}.Vat      = DEC( bfr{&tmptable}.Vat:SCREEN-VALUE)
                     bfr{&tmptable}.age      = DEC( bfr{&tmptable}.age:SCREEN-VALUE)
                     bfr{&tmptable}.REF      = bfr{&tmptable}.REF:SCREEN-VALUE IN FRAME frm-input
                     bfr{&tmptable}.trDATE   = DATE( bfr{&tmptable}.trDATE:SCREEN-VALUE)
                     bfr{&tmptable}.Descrip  =  bfr{&tmptable}.Descrip:SCREEN-VALUE.
              FIND FIRST bfdbCtr WHERE bfdbCtr.intbatch = wsbatch EXCLUSIVE-LOCK NO-ERROR. 
              ASSIGN bfdbCtr.amt[1] =  bfdbCtr.amt[1] + DEC( bfr{&tmptable}.AMT:SCREEN-VALUE) 
                            WHEN DEC(bfr{&tmptable}.AMT:SCREEN-VALUE) > 0 
                     bfdbCtr.amt[2] =  bfdbCtr.amt[2] + DEC( bfr{&tmptable}.AMT:SCREEN-VALUE) 
                           WHEN DEC( bfr{&tmptable}.AMT:SCREEN-VALUE) < 0. 
             FIND bfdbCtr WHERE bfdbCtr.intbatch = wsbatch NO-LOCK NO-ERROR. 
                   ASSIGN wsrec= wsrec + 1
                          wsAmt[1] = bfdbCtr.amt[1]
                          wsAmt[2] = bfdbCtr.amt[2].
                   DISPLAY wsrec @ bfr{&tmptable}.seq  WITH FRAME frm-input.
                   DISPLAY  wsAmt[1] wsAmt[2] WITH FRAME frm-bfrBatch.
                   OPEN QUERY qry-bfr{&tmptable} FOR EACH  bfr{&tmptable}  
                        WHERE  bfr{&tmptable}.intbatch = wsbatch NO-LOCK.
             APPLY 'entry' TO  bfr{&tmptable}.trDATE.
            RETURN.
        END.
        ELSE IF wsbtn <> "New" THEN DO:
            FIND FIRST bfdbCtr WHERE bfdbCtr.intbatch = wsbatch EXCLUSIVE-LOCK NO-ERROR.
            ASSIGN  bfdbCtr.amt[1] =  bfdbCtr.amt[1] -  bfr{&tmptable}.AMT WHEN  bfr{&tmptable}.AMT > 0
                    bfdbCtr.amt[2] =  bfdbCtr.amt[2] -  bfr{&tmptable}.AMT WHEN  bfr{&tmptable}.amt < 0.
            FIND bfdbCtr WHERE bfdbCtr.intbatch = wsbatch NO-LOCK NO-ERROR. 
            ASSIGN  bfr{&tmptable}.ILedger  = DEC(bfr{&tmptable}.ILedger:SCREEN-VALUE)
                     bfr{&tmptable}.Dept    = glmf.dept
                     bfr{&tmptable}.fund    = INT(bfr{&tmptable}.fund:SCREEN-VALUE)
                     bfr{&tmptable}.Proj    = INT(bfr{&tmptable}.Proj:SCREEN-VALUE)
                     bfr{&tmptable}.age     = DEC( bfr{&tmptable}.age:SCREEN-VALUE)
                     bfr{&tmptable}.dbAcc   = DEC( bfr{&tmptable}.dbAcc:SCREEN-VALUE)
                     bfr{&tmptable}.Sgrp    = INT( bfr{&tmptable}.Sgrp:SCREEN-VALUE)
                     bfr{&tmptable}.AMT     = DEC( bfr{&tmptable}.AMT:SCREEN-VALUE)
                     bfr{&tmptable}.Vat     = DEC( bfr{&tmptable}.Vat:SCREEN-VALUE)
                     bfr{&tmptable}.REF     = bfr{&tmptable}.REF:SCREEN-VALUE IN FRAME frm-input
                     bfr{&tmptable}.trDATE  = DATE( bfr{&tmptable}.trDATE:SCREEN-VALUE)
                     bfr{&tmptable}.Descrip =  bfr{&tmptable}.Descrip:SCREEN-VALUE.
            FIND FIRST bfdbCtr WHERE bfdbCtr.intbatch = wsbatch EXCLUSIVE-LOCK NO-ERROR.
            ASSIGN bfdbCtr.amt[1] =  bfdbCtr.amt[1] + DEC( bfr{&tmptable}.AMT:SCREEN-VALUE) 
                            WHEN DEC( bfr{&tmptable}.AMT:SCREEN-VALUE) > 0 
                     bfdbCtr.amt[2] =  bfdbCtr.amt[2] + DEC( bfr{&tmptable}.AMT:SCREEN-VALUE) 
                           WHEN DEC( bfr{&tmptable}.AMT:SCREEN-VALUE) < 0.
             FIND bfdbCtr WHERE bfdbCtr.intbatch = wsbatch NO-LOCK NO-ERROR. 
             ASSIGN wsAmt[1] = bfdbCtr.amt[1]
                    wsAmt[2] = bfdbCtr.amt[2].
             CLEAR FRAME frm-input ALL.
             HIDE FRAME frm-input.
             /* VIEW FRAME frm-bfrBatch. */
             DISPLAY  wsAmt[1] wsAmt[2] WITH FRAME frm-bfrBatch.
             OPEN QUERY qry-bfr{&tmptable} FOR EACH  bfr{&tmptable}  
                        WHERE  bfr{&tmptable}.intbatch = wsbatch NO-LOCK.
             APPLY "CLOSE":U TO THIS-PROCEDURE.
             RETURN.
        END.
END.

/* *** Triggers to frame frm-bfrBatch*** */
ON CHOOSE OF btn-Add1 IN FRAME frm-bfrBatch
 DO:
   FIND LAST  bfr{&tmptable} WHERE  bfr{&tmptable}.intbatch = wsbatch NO-LOCK NO-ERROR.
   IF AVAILABLE  bfr{&tmptable} THEN
       wsrec =  bfr{&tmptable}.seq.
    ELSE wsrec = 0.
    wsrec = wsrec + 1.
    wsbtn = "New".
    OPEN QUERY qry-bfr{&tmptable} FOR EACH  bfr{&tmptable}  
               WHERE  bfr{&tmptable}.intbatch = wsbatch NO-LOCK. 
    RUN proc-input.
    RETURN.
END.

ON 'mouse-select-dblclick' OF brw-bfr{&tmptable} IN FRAME frm-bfrBatch
    OR CHOOSE OF btn-edit IN FRAME frm-bfrBatch
DO: 
    wsbtn = "Upd".
    GET CURRENT qry-bfr{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsrec = int(bfr{&tmptable}.seq)
           wsid = int(bfr{&tmptable}.intbatch).
    FIND FIRST bfdbCtr WHERE bfdbCtr.intbatch  = wsid NO-LOCK NO-ERROR.
    FIND FIRST dbcmf WHERE dbcmf.dbacc = bfr{&tmptable}.dbAcc NO-LOCK NO-ERROR.
    FIND FIRST glmf WHERE glmf.acct = bfr{&tmptable}.ILedger NO-LOCK NO-ERROR.
    FIND FIRST dbsgr WHERE dbsgr.sgrp = bfr{&tmptable}.Sgrp NO-LOCK NO-ERROR.
    FIND FIRST glfund WHERE glfund.fund = bfr{&tmptable}.fund NO-LOCK NO-ERROR.
    FIND FIRST glproj WHERE glproj.proj = bfr{&tmptable}.proj NO-LOCK NO-ERROR.
    VIEW FRAME frm-input.
    DISPLAY bfr{&tmptable}.Seq
            bfr{&tmptable}.trDATE 
            bfr{&tmptable}.dbAcc dbcmf.Name
            bfr{&tmptable}.Sgrp dbsgr.descrip
            bfr{&tmptable}.DESCRIP 
            bfr{&tmptable}.ref
            bfr{&tmptable}.AMT
            bfr{&tmptable}.age
            bfr{&tmptable}.vat
            bfr{&tmptable}.ILedger glmf.descrip
            bfr{&tmptable}.Fund glfund.descrip
            bfr{&tmptable}.Proj glProj.descrip WITH FRAME frm-input.
    ENABLE ALL EXCEPT bfr{&tmptable}.Seq WITH FRAME frm-input. 
    WAIT-FOR CHOOSE OF btn-cancel OR close of THIS-PROCEDURE IN FRAME frm-input.
    HIDE FRAME frm-input.
    RETURN.
END.
 
ON CHOOSE OF btn-Del IN FRAME frm-bfrBatch DO:
    GET CURRENT qry-bfr{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid =  bfr{&tmptable}.intbatch.
    MESSAGE "Are you sure you want to delete this transaction?"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK-CANCEL UPDATE wsdel.
     IF wsdel THEN DO:
        FIND FIRST  bfdbCtr WHERE  bfdbCtr.intbatch = wsid EXCLUSIVE-LOCK NO-ERROR.
        ASSIGN  bfdbCtr.amt[1] =  bfdbCtr.amt[1] -  bfr{&tmptable}.AMT WHEN  bfr{&tmptable}.AMT > 0
               bfdbCtr.amt[2] =  bfdbCtr.amt[2] -  bfr{&tmptable}.AMT WHEN  bfr{&tmptable}.amt < 0.
        DELETE  bfr{&tmptable}.
        Method-Status = brw-bfr{&tmptable}:DELETE-SELECTED-ROWS().
        ASSIGN wsAmt[1] = bfdbCtr.amt[1]
                wsAmt[2] = bfdbCtr.amt[2].
        DISPLAY  wsAmt[1] wsAmt[2] WITH FRAME frm-bfrBatch.
     END.
    APPLY 'entry':u TO btn-cancel.
END.


ON 'TAB' OF  wsBatch IN FRAME frm-bfrBatch
    OR 'enter' OF  wsBatch IN FRAME frm-bfrBatch
DO:
    IF int( wsBatch:SCREEN-VALUE) = 0 THEN DO:
        HIDE FRAME frm-bfrBatch.
        APPLY 'Close' TO THIS-PROCEDURE.
    END.   
    ELSE DO:
      FIND FIRST  bfdbCtr WHERE  bfdbCtr.intbatch = int( wsBatch:SCREEN-VALUE) NO-LOCK NO-ERROR.
      IF AVAILABLE  bfdbCtr THEN DO:
          MESSAGE "Batch number already exist" VIEW-AS ALERT-BOX.
          APPLY 'Close' TO THIS-PROCEDURE.
          RETURN NO-APPLY.
       END.
        ELSE
           APPLY 'TAB' TO wsDate IN FRAME frm-bfrBatch.
    END.
END.

ON 'TAB' OF  wsper IN FRAME frm-bfrBatch
    OR 'enter' OF  wsPer IN FRAME frm-bfrBatch
DO:
    IF INT(wsper:SCREEN-VALUE) <= wscurr
        AND INT(wsper:SCREEN-VALUE) > wsClosed  THEN
    DO:
        CREATE  bfdbCtr.
        ASSIGN  bfdbCtr.UID      = varUser
                wsbatch         = INT( wsBatch:SCREEN-VALUE)
                bfdbCtr.intbatch = INT( wsBatch:SCREEN-VALUE)
                bfdbCtr.bdate    = DATE( wsdate:SCREEN-VALUE)
                bfdbCtr.period   = INT( wsper:SCREEN-VALUE).
        CURRENT-VALUE (intBatch,db003) = CURRENT-VALUE (intBatch,db003) + 1.        
        apply 'entry' to btn-Add1 IN FRAME frm-bfrBatch.
    END.
    ELSE DO:
         MESSAGE "The period " wsper " is outside Accounting Periods. No batch will be created"
              VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
    END.
    
END.

ON CHOOSE OF btn-Prn IN FRAME frm-main
DO:
    GET CURRENT qry-bfdbCtr NO-LOCK.
   
    ASSIGN wsDate = bfdbCtr.BDate 
          wsPer  = bfdbCtr.period
          wsBatch = bfdbCtr.intBatch.
   FIND FIRST simusr WHERE simusr.usercode = bfdbCtr.UID NO-LOCK NO-ERROR.
    /* Select print */
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="Report.ip"
                    &paged} 
    RETURN.
END.

ON CHOOSE OF btn-Exp IN FRAME frm-main 
DO:  
   GET CURRENT qry-bfdbCtr NO-LOCK.
    MESSAGE bfdbCtr.BDate VIEW-AS ALERT-BOX.
   wsFile = TRIM(SIMCTR.repDir) + "dbjnl" + STRING(bfdbCtr.intBatch) + ".csv".
   OUTPUT STREAM b TO VALUE(wsFile).
   ASSIGN wsDate = bfdbCtr.BDate 
          wsPer  = bfdbCtr.period
          wsBatch = bfdbCtr.intBatch.
   FIND FIRST simusr WHERE simusr.usercode = bfdbCtr.UID NO-LOCK NO-ERROR.
   RUN EXP.ip. 
   OUTPUT STREAM b CLOSE.
   OS-COMMAND NO-WAIT VALUE(wsFile).
   RETURN.
END.

/* ***** Triggers for the main frame **** */

ON CHOOSE OF btn-Add IN FRAME frm-main
DO:  
   wsdate = TODAY.
   VIEW FRAME frm-bfrBatch.
   ENABLE ALL  EXCEPT  wsAmt[1] wsAmt[2] brw-bfr{&tmptable} WITH FRAME frm-bfrBatch.
   wsbatch:SCREEN-VALUE IN FRAME frm-bfrBatch = STRING(CURRENT-VALUE (intBatch,db003 )).
   DISABLE wsbatch WITH FRAME frm-bfrBatch.
   WAIT-FOR CHOOSE OF btn-cancel OR CLOSE OF THIS-PROCEDURE IN FRAME frm-bfrBatch.
   CLEAR FRAME frm-bfrBatch ALL.
   DISABLE ALL WITH FRAME frm-bfrBatch.
   HIDE FRAME frm-bfrBatch.
   VIEW FRAME frm-main.
   IF simusr.supervisor = YES OR simusr.usercode = "1" THEN
      OPEN QUERY qry-bfdbCtr FOR EACH  bfdbCtr NO-LOCK.
   ELSE  
      OPEN QUERY qry-bfdbCtr FOR EACH  bfdbCtr WHERE  bfdbCtr.UID = varuser NO-LOCK.
END.
 
ON CHOOSE OF btn-Edit IN FRAME frm-main 
    OR 'mouse-select-dblclick' OF brw-bfdbCtr
DO:
    GET CURRENT qry-bfdbCtr NO-LOCK.
    ASSIGN wsbatch =  bfdbCtr.intbatch.
    FIND FIRST bfdbCtr WHERE  bfdbCtr.intbatch = wsBatch EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
     IF bfdbCtr.UID  <> varuser THEN DO:
         MESSAGE "Batch created by another user, you are not allowed to edit" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
     END.
    IF NOT AVAILABLE bfdbCtr THEN DO:
        MESSAGE "Someone is working on this Batch please try later" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        CLEAR FRAME frm-bfrBatch ALL.
        VIEW FRAME frm-bfrBatch.
        ENABLE ALL EXCEPT  wsbatch wsDate wsPer wsAmt[1] wsAmt[2] WITH FRAME frm-bfrBatch.
        DISPLAY  bfdbCtr.intbatch @ wsBatch bfdbCtr.bdate @ wsDate   bfdbCtr.period @ wsPer
             bfdbCtr.AMT[1]  @ wsAmt[1] bfdbCtr.AMT[2] @ wsAmt[2] WITH FRAME frm-bfrBatch.
        FIND LAST  bfr{&tmptable} WHERE  bfr{&tmptable}.intbatch = wsbatch NO-ERROR.
        IF AVAILABLE  bfr{&tmptable} THEN
            wsrec =  bfr{&tmptable}.seq.
        ELSE wsrec = 0.
        OPEN QUERY qry-bfr{&tmptable} FOR EACH  bfr{&tmptable}  
            WHERE  bfr{&tmptable}.intbatch = wsbatch NO-LOCK BY  bfr{&tmptable}.seq.
        WAIT-FOR CHOOSE OF btn-cancel OR close of THIS-PROCEDURE IN FRAME frm-bfrBatch.
        CLOSE QUERY qry-bfr{&tmptable}.
        CLEAR FRAME frm-bfrBatch ALL.
        DISABLE ALL WITH FRAME frm-bfrBatch.
        HIDE FRAME frm-bfrBatch.
        VIEW FRAME frm-main.
    END.
    
    RETURN.
END.


ON CHOOSE OF btn-Del IN FRAME frm-main DO:
    GET CURRENT qry-bfdbCtr EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid =  bfdbCtr.intbatch.
    MESSAGE "Are you sure you want to delete this batch?"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK-CANCEL UPDATE wsdel.
     IF wsdel THEN DO:
        IF bfdbCtr.UID <> varUser AND varUser <> "1" THEN DO:
            MESSAGE "You are not an Administrator, you can only delete your batches"
                VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
        ELSE DO:
             FOR EACH bfrBatch WHERE bfrBatch.intbatch = bfdbCtr.intbatch.
                 DELETE bfrBatch.
             END.
             DELETE  bfdbCtr.
             Method-Status = brw-bfdbCtr:DELETE-SELECTED-ROWS().
        END.
     END. 
    APPLY 'entry' TO btn-exit.
END.


/********** MAIN LOGIC **********/
browse brw-ledger:allow-column-searching = true. 
browse brw-PickAcc:allow-column-searching = true.
BROWSE brw-bfr{&tmptable}:allow-column-searching = true.
FIND FIRST SIMCTR NO-LOCK NO-ERROR.
FIND FIRST simusr WHERE simusr.usercode = varUser NO-LOCK NO-ERROR.
ASSIGN wscurr = SIMCTR.CURPER
       wsclosed = SIMCTR.CLOSEPER.
IF simusr.supervisor = YES OR simusr.usercode = "1" THEN
    OPEN QUERY qry-bfdbCtr FOR EACH  bfdbCtr NO-LOCK.
ELSE  
   OPEN QUERY qry-bfdbCtr FOR EACH  bfdbCtr WHERE  bfdbCtr.UID = varuser NO-LOCK.
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
CLOSE QUERY qry-bfdbCtr.
HIDE FRAME frm-main.

PROCEDURE proc-input:
  VIEW FRAME frm-input.
  CLEAR FRAME frm-input ALL.
  ENABLE ALL EXCEPT  bfr{&tmptable}.seq WITH FRAME frm-input.
  wsage = 1.
  DISPLAY wsAge @ bfr{&tmptable}.Age wsRec @ bfr{&tmptable}.seq WITH FRAME frm-input.
  WAIT-FOR CHOOSE OF btn-cancel OR close of THIS-PROCEDURE IN FRAME frm-input.
  IF simusr.supervisor = YES OR simusr.usercode = "1" THEN
     OPEN QUERY qry-bfdbCtr FOR EACH  bfdbCtr NO-LOCK.
  ELSE  
     OPEN QUERY qry-bfdbCtr FOR EACH  bfdbCtr WHERE  bfdbCtr.UID = varuser NO-LOCK.
  HIDE FRAME frm-input.
END.


procedure Search.ip.
    IF SELF:NAME = "brw-ledger" THEN
        hCol = browse brw-ledger:current-column.
    ELSE IF SELF:NAME = "brw-PickAcc" THEN
        hCol = browse brw-PickAcc:current-column.
    ELSE IF SELF:NAME = "brw-bfr{&tmptable}" THEN
        hCol = browse brw-bfr{&tmptable}:current-column.
       assign  frame Search-opt:title = hCol:label + " Column".
       IF trim(hCol:label) <> "Description" AND trim(hCol:label) <> "Name" 
           AND trim(hCol:label) <> "Account" AND trim(hCol:label) <> "Reference" THEN
           RETURN NO-APPLY.
        view frame Search-opt.
        enable all with frame Search-opt.
        WAIT-FOR 'choose' of btnSearch OR 'tab' of btnSearch OR 'enter' of btnSearch 
            OR 'window-close' of frame Search-opt or close of this-procedure or
                'esc','f4' of frame Search-opt.
        hide frame Search-opt.
        /*if ll-sort = ? then return.*/
   IF SELF:NAME = "brw-ledger" THEN DO:
       case trim(hCol:label):
         when "Description" THEN
         do:
            OPEN query qry-ledger FOR EACH glmf no-lock 
                            where glmf.Descrip >= wsSearch:SCREEN-VALUE USE-INDEX NAME.
          END.
       OTHERWISE
           RETURN NO-APPLY.     
     END.
   END.
   IF SELF:NAME = "brw-PickAcc" THEN DO:
       case trim(hCol:label):
       when "Name"  THEN
          do:
          OPEN query qry-PickAcc FOR EACH dbcmf no-lock 
                            where dbcmf.sortname >= wsSearch:SCREEN-VALUE AND (dbcmf.accstat = 0 OR dbcmf.accstat = 95)
               USE-INDEX NAME.
            END.
            when "Account"  THEN
          do:
          OPEN query qry-PickAcc FOR EACH dbcmf no-lock 
                            where dbcmf.dbAcc >= DEC(wsSearch:SCREEN-VALUE) AND (dbcmf.accstat = 0 OR dbcmf.accstat = 95)
              USE-INDEX dbacc.
            END.
        OTHERWISE
           RETURN NO-APPLY.   
     END.
   END. 
   IF SELF:NAME = "brw-bfr{&tmptable}" THEN DO:
      case trim(hCol:label):
          WHEN "Account"  THEN DO: 
               OPEN query qry-bfr{&tmptable} FOR EACH bfr{&tmptable} no-lock 
                            where bfr{&tmptable}.dbAcc = DEC(wsSearch:SCREEN-VALUE)
                             AND bfr{&tmptable}.intBatch = wsBatch.
           END.
           WHEN "Reference"  THEN DO:
               OPEN query qry-bfr{&tmptable} FOR EACH bfr{&tmptable} no-lock 
                            where bfr{&tmptable}.Ref >= wsSearch:SCREEN-VALUE 
                             AND bfr{&tmptable}.intBatch = wsBatch.
           END.
           OTHERWISE
             RETURN NO-APPLY.   
     END.
   END. 
   RETURN.
END.

PROCEDURE Report.ip:
    FOR EACH bfrBatch WHERE bfrBatch.intBatch = bfdBCtr.intBatch:
         DISPLAY STREAM a bfrBatch.Seq bfrBatch.trDate bfrBatch.dbacc bfrBatch.Ref bfrBatch.Sgrp bfrBatch.Amt 
                          bfrBatch.Vat bfrBatch.DESCRIP bfrBatch.Iledger WITH FRAME frmRpt.
         DOWN STREAM a WITH FRAME frmRpt.
    END.
END PROCEDURE.

PROCEDURE EXP.ip:
IF CAN-FIND (FIRST bfrBatch WHERE bfrBatch.intBatch = bfdBCtr.intBatch) THEN DO:
    EXPORT STREAM b DELIMITER ',' "BATCH: " + STRING(bfdBCtr.intBatch).
    EXPORT STREAM b DELIMITER ',' "BATCH" "PERIOD" "SEQ" "DATE" "ACCOUNT" "REF" "SERVIRCE" "AMOUNT" "VAT" "LEDGER" "NARRATION" "PROJECT" "FUND" "DEPT".
    FOR EACH bfrBatch WHERE bfrBatch.intBatch = bfdBCtr.intBatch:
         EXPORT STREAM b DELIMITER ','  bfdBCtr.intBatch bfdBCtr.period bfrBatch.Seq bfrBatch.trDate bfrBatch.dbacc bfrBatch.Ref bfrBatch.Sgrp bfrBatch.Amt 
                          bfrBatch.Vat bfrBatch.Iledger bfrBatch.DESCRIP bfrBatch.Proj bfrBatch.Fund bfrBatch.Dept.
 
    END.
END.
END PROCEDURE.
