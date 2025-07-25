/* Program.................payctr.p
   Notes:................. Parameter File Maintenance
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF VAR wsid LIKE simctr.cocode.
DEF VAR Current-Record AS ROWID.
DEF VAR method-status  AS LOGICAL.
DEF VAR ws             LIKE Payitem.Descrip  EXTENT 7.
DEF VAR wsdes          LIKE Payitem.Descrip EXTENT 6.
DEF VAR wsledger       LIKE glmf.DESCRIPTION.
DEF VAR X AS INT.

DEF BUTTON btn-close LABEL "CLOSE".
DEF BUTTON btn-ok    LABEL "OK".
DEF  BUTTON btnLedger LABEL "Control Ledger".
DEF  BUTTON btnBasic LABEL "Basic".

define rectangle rect-1
     edge-pixels 2 graphic-edge  no-fill
     size 116 by 2.3.

define rectangle rect-2
     edge-pixels 2 graphic-edge  NO-FILL
     size 116 by 18.5.

define frame frm-input
         SKIP(1)
    btnLedger colon 10 Payctr.Ledger   NO-LABEL 
    wsLedger NO-LABEL VIEW-AS TEXT  skip(0.2)
    Payctr.Meth COLON 26 LABEL "Tax Method" skip(0.2)
    "Payroll Items for:" COLON 10  SKIP(0.2)
    Payctr.Basic  COLON 26 LABEL "Basic Salary" ws[1] NO-LABEL VIEW-AS TEXT skip(0.2)
    Payctr.NSSA[1] COLON 26 LABEL "NSSA: Employee" ws[2] NO-LABEL VIEW-AS TEXT skip(0.2)
    Payctr.NSSA[2] COLON 26 LABEL "    : Employer" ws[3] NO-LABEL VIEW-AS TEXT skip(0.2)
    Payctr.Overtime[1]  colon 26 LABEL "Overtime" ws[4] NO-LABEL VIEW-AS TEXT
    Payctr.ORate[1] LABEL "Rate" skip(0.2)
    Payctr.Overtime[2]  colon 26 LABEL "Overtime" ws[5] NO-LABEL VIEW-AS TEXT
    Payctr.ORate[2] LABEL "Rate" skip(0.2)
    Payctr.Overtime[3]  COLON 26 LABEL "Overtime" ws[6] NO-LABEL VIEW-AS TEXT
    Payctr.ORate[3] LABEL "Rate" skip(1)
    "-----STATUTORY DEDUCTIONS -------" COLON 26 SKIP
    Payctr.AidsLevy    COLON 26 LABEL "AIDS Levy" ws[7] NO-LABEL VIEW-AS TEXT SKIP(0.2)
    Payctr.itemcod     COLON 26 LABEL "Leave Day Item"  SKIP(1.2)
    btn-ok colon 20
    btn-close colon 60 SKIP(0.5)
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE TITLE "PAYROLL CONTROL FILE".
    
DEF    QUERY qry-ledger FOR glmf SCROLLING.
DEF BROWSE brw-ledger QUERY qry-ledger
    DISPLAY glmf.acct glmf.DESCRIPTION COLUMN-LABEL "Decsription" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-pick 
    brw-ledger AT ROW 2 COL 5
    skip(0.2)
    btn-ok colon 5
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Ledger Account Selection".

/* *** Triggers to input frame frm-input*** */
ON CHOOSE OF btnLedger IN FRAME frm-input
    OR CHOOSE OF btnLedger IN FRAME frm-input
DO:
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
  APPLY 'tab' TO SELF.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pick 
    OR 'enter':u OF brw-ledger
    OR 'mouse-select-dblclick' OF brw-ledger
DO: 
   GET CURRENT qry-ledger NO-LOCK NO-WAIT.
   DISPLAY glmf.acct @ Payctr.Ledger glmf.DESCRIPTION @ wsLedger WITH FRAME frm-input.
   RETURN. 
END.

ON  'enter':U OF Payctr.Ledger IN FRAME frm-input
    OR 'tab':U OF Payctr.Ledger IN FRAME frm-input
DO:
    FIND FIRST glmf WHERE glmf.acct = DEC(Payctr.Ledger:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE glmf THEN
        DISPLAY glmf.DESCRIPTION @ wsLedger WITH FRAM frm-input.
    ELSE DO:
        MESSAGE "Invalid Ledger...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
     ASSIGN Payctr.Ledger      = DEC(Payctr.Ledger:SCREEN-VALUE)
            Payctr.Basic       = INT(Payctr.Basic:SCREEN-VALUE)
            Payctr.NSSA[1]     = INT(Payctr.NSSA[1]:SCREEN-VALUE)
            Payctr.NSSA[2]     = INT(Payctr.NSSA[2]:SCREEN-VALUE)
            Payctr.Overtime[1] = INT(Payctr.Overtime[1]:SCREEN-VALUE)
            Payctr.Overtime[2] = INT(Payctr.Overtime[2]:SCREEN-VALUE)   
            Payctr.Overtime[3] = INT(Payctr.Overtime[3]:SCREEN-VALUE)
            Payctr.ORate[1]    = DEC(Payctr.ORate[1]:SCREEN-VALUE)
            Payctr.ORate[2]    = DEC(Payctr.ORate[2]:SCREEN-VALUE)
            Payctr.ORate[3]    = DEC(Payctr.ORate[3]:SCREEN-VALUE)
            Payctr.AidsLevy    = INT(Payctr.AidsLevy:SCREEN-VALUE)
            Payctr.Itemcode    = INT(Payctr.itemcode:SCREEN-VALUE)
            Payctr.Meth        = INT(Payctr.Meth:SCREEN-VALUE).
           CLEAR FRAME frm-input ALL.
           APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/********** MAIN LOGIC **********/
VIEW FRAME frm-input.
ENABLE ALL WITH FRAME frm-input.
RUN display-ip.
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
HIDE FRAME frm-input.
RETURN.

PROCEDURE display-ip:
    FIND FIRST Payctr NO-ERROR.
    IF AVAILABLE Payctr THEN DO:
        FIND FIRST GLMF WHERE GLMF.ACCT = payctr.ledger NO-LOCK NO-ERROR.
        IF AVAILABLE glmf THEN wsLedger = glmf.DESCRIPTION.
        FIND FIRST payitem WHERE Payitem.Itemcode = Payctr.Basic NO-LOCK NO-ERROR.
             IF AVAILABLE payitem THEN ws[1] = Payitem.Descrip.
        FIND FIRST payitem WHERE Payitem.Itemcode = Payctr.NSSA[1] NO-LOCK NO-ERROR.
             IF AVAILABLE payitem THEN ws[2] = Payitem.Descrip.
        FIND FIRST payitem WHERE Payitem.Itemcode = Payctr.NSSA[2] NO-LOCK NO-ERROR.
             IF AVAILABLE payitem THEN ws[3] = Payitem.Descrip.
        FIND FIRST payitem WHERE Payitem.Itemcode = Payctr.Overtime[1] NO-LOCK NO-ERROR.
             IF AVAILABLE payitem THEN ws[4] = Payitem.Descrip.
        FIND FIRST payitem WHERE Payitem.Itemcode = Payctr.Overtime[2] NO-LOCK NO-ERROR.
             IF AVAILABLE payitem THEN ws[5] = Payitem.Descrip.
        FIND FIRST payitem WHERE Payitem.Itemcode = Payctr.Overtime[3] NO-LOCK NO-ERROR.
             IF AVAILABLE payitem THEN ws[6] = Payitem.Descrip.
        FIND FIRST payitem WHERE Payitem.Itemcode = Payctr.AidsLevy NO-LOCK NO-ERROR.
             IF AVAILABLE payitem THEN ws[7] = Payitem.Descrip.
       DISPLAY Payctr.Ledger wsLedger Payctr.Basic Payctr.AidsLevy Payctr.NSSA 
             Payctr.Overtime Payctr.ORate ws payctr.itemcode payctr.meth WITH FRAME frm-input.
    END.    
    ELSE IF NOT AVAILABLE Payctr THEN
        CREATE Payctr.
END.
