SESSION:DATA-ENTRY-RETURN = TRUE.
DEF BUFFER bfrCBTrans FOR CBTrans.
DEF BUFFER bfrCBkmf FOR CBkmf.

DEF STREAM a.
DEF  VAR w-orientation AS CHAR      INITIAL "Portrait"    NO-UNDO.
DEF  VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF  VAR w-fontsize    AS INT       INITIAL 10.0          NO-UNDO.
DEFINE VARIABLE method-return AS LOGICAL NO-UNDO.
DEF VAR wsBal LIKE cbkmf.StatBal.
DEF VAR wsAmt LIKE cbkmf.StatBal.
DEF VAR wsOp  LIKE cbkmf.StatBal.
DEF VAR wsCBtotal LIKE cbkmf.StatBal.
DEF VAR wsUnrec LIKE cbkmf.StatBal.
DEF  VAR wsper AS INT.
DEF VAR wsTitle AS CHAR FORM "x(65)".
DEF VAR wsTitle1 AS CHAR FORM "x(65)".

DEFINE BUTTON btn-ok LABEL "PRINT".
DEFINE BUTTON btn-exit LABEL "CLOSE".
DEFINE BUTTON btn-bank LABEL "BANK".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 75 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 75 by 19.

FORM SPACE(5) bfrCBTrans.Accper 
     bfrCBTrans.trDate 
     bfrCBTrans.Ref
     bfrCBTrans.Descrip 
     bfrCBTrans.amount
     bfrCBTrans.Recon
    HEADER skip(2) "BANK RECONCILIATION STATEMENT:" AT 5 SPACE(2) "Page: " AT 61 TRIM(STRING(PAGE-NUMBER(a)))
     SKIP(1)
     "BANK:- " AT 5 wsTitle SKIP
     wsTitle1   AT 5 SKIP(1) "RECONCILED ITEMS" AT 5 "PERIOD TR-DATE  REFERENCE" AT 65 SKIP
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-rpt.

DEF QUERY qry-recon FOR cbrec SCROLLING.
DEF BROWSE brw-recon QUERY qry-recon
    DISPLAY cbrec.StatNo LABEL "STAT-NO" cbrec.per LABEL "PERIOD" cbrec.OpenBal LABEL "OPENING BALANCE"
     cbrec.CloseBal LABEL "CLOSING BALANCE" WITH 20 DOWN SEPARATORS NO-LABEL.

DEFINE FRAME frm-main
    SKIP(1)
    btn-bank COLON 10 cbrec.bank NO-LABEL bfrCBkmf.descrip NO-LABEL VIEW-AS TEXT SKIP(.5)
    brw-recon AT 10
    skip(1.3)
    btn-ok colon 15
    btn-exit colon 50
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20.5 COL 3
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "BANK STATEMENT RECONCILIATION REPORT".

ON 'enter':U OF cbrec.bank IN FRAME frm-main 
DO:
    FIND FIRST bfrCBkmf WHERE bfrCBkmf.bank = INT(cbrec.bank:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF NOT AVAILABLE bfrCBkmf THEN DO:
            MESSAGE "Invalid bank entered...Please try again".
                RETURN NO-APPLY.
        END.
        ELSE DO:
            DISPLAY bfrCBkmf.descrip WITH FRAME frm-main.
            OPEN QUERY qry-recon FOR EACH cbrec WHERE cbrec.bank = INT(cbrec.bank:SCREEN-VALUE)
                NO-LOCK BY cbrec.statno DESCENDING.
                APPLY 'tab' TO SELF.
        END.              
    RETURN.
END.

ON 'choose':U OF btn-ok IN FRAME frm-main 
DO:
    GET CURRENT qry-recon NO-LOCK NO-WAIT.
    {PrintOpt.i &stream-name="stream a"
                    &print-prog="Report-ip"
                    &paged}
END.

/********** MAIN LOGIC **********/
ENABLE ALL  WITH FRAME frm-main.
APPLY 'ENTRY' TO CBREC.bank IN FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.

PROCEDURE Report-ip:
wsTitle = bfrCBkmf.descrip.
wsTitle1 = "STATEMENT NUMBER: " + STRING(cbrec.StatNo) + 
           "                       ENDING PERIOD: " + STRING(cbrec.per).
    FOR EACH bfrCBTrans WHERE bfrCBTrans.StatNo = cbrec.StatNo
                      AND bfrCBTrans.Accper <= cbrec.per AND bfrCBTRans.Recon = "U" AND bfrCBTrans.seq = 0 AND bfrCBTrans.bank = cbrec.bank NO-LOCK:
        DISPLAY STREAM a bfrCBTrans.Descrip  bfrCBTrans.amount  "Y" @ bfrCBTrans.Recon bfrCBTrans.Accper bfrCBTrans.trDate
             bfrCBTrans.Ref WITH FRAME frm-rpt.
        DOWN STREAM a WITH FRAME frm-rpt.
        
    END.
    APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

