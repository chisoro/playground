/* Program.................cbrpt02.p
   Notes:................. Cash Resource Availability
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
&SCOPED-DEFINE tmptable        cbkmf
&SCOPED-DEFINE tmpBank         bfr{&tmptable}.bank ~
                                        COLUMN-LABEL ' CASHBOOK ':C ~
                               bfr{&tmptable}.descrip ~
                                        COLUMN-LABEL ' ACCOUNT NAME 'WIDTH 40  ~
                                bfr{&tmptable}.txtCur ~
                                        COLUMN-LABEL ' CURRENCY '~
                                bfr{&tmptable}.Bal ~
                                       COLUMN-LABEL ' AMOUNT ':C


DEF VAR Current-Record AS ROWID.
DEF VAR method-status  AS LOGICAL.
DEF VAR wscurr      LIKE SIMCTR.CURPER.
DEF VAR wsclosed    LIKE  SIMCTR.CLOSEPER.
DEF VAR wsper       LIKE simctr.closeper.
DEF VAR wsAmt       LIKE cbkmf.Bal FORM "zzzzzzzzzzz9.99-".
DEF VAR wsRate      LIKE tblForex.decRate.
DEF VAR wsAmt1      LIKE wsAmt FORM "zzzzzzzzzzz9.99-".
DEF VAR X AS INT.

DEF BUTTON btn-exit  LABEL "CLOSE".
DEF BUTTON btn-ok    LABEL "OK".

DEF BUFFER bfr{&tmptable} FOR cbkmf.

DEFINE QUERY qry-{&tmptable} FOR  bfr{&tmptable} scrolling.
DEF BROWSE brw-{&tmptable} QUERY qry-{&tmptable}
   DISPLAY {&tmpBank} wsRate COLUMN-LABEL "RATE" wsAmt COLUMN-LABEL "BALANCE "
     wsAmt1 COLUMN-LABEL "LEDGER !AMOUNT "
    WITH 19 DOWN SEPARATORS.

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 graphic-edge  NO-FILL
     SIZE 125 BY 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 125 BY 18.


DEF FRAME frm-main
    SKIP(0.5)
    "CASHBOOKS"         COLON 10 
    brw-{&tmptable} AT ROW 2.5 COL 4   
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 19.5 COL 3
    btn-exit AT ROW 20 COL 50
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
    SIZE 133 BY 24
    TITLE "CASH RESOURCES AVAILABILITY" view-as dialog-box.

ON row-display OF brw-{&tmptable} DO:
    FIND FIRST glbal WHERE glbal.YEAR = INT(SUBSTR(STRING(wsCurr),1,4)) AND glbal.acct = bfr{&tmptable}.Ledger NO-LOCK NO-ERROR.
    IF AVAILABLE glbal THEN DO:
        wsamt1 = glbal.bfbal.
        DO X = 1 TO INT(SUBSTR(STRING(wsCurr),5,2)):
            wsamt1 = wsamt1 + glbal.amt[X].
        END.
    END.
    ELSE IF NOT AVAILABLE glbal THEN
        wsAmt1 = 0.
    FIND FIRST tblForex WHERE tblForex.txtCur = bfr{&tmptable}.txtCur NO-LOCK NO-ERROR.
    ASSIGN wsAmt  = wsAmt + ( bfr{&tmptable}.Bal * tblForex.decRate)
           wsRate = tblForex.decRate. 
END.

/********** MAIN LOGIC **********/
FIND FIRST simctr NO-ERROR.
ASSIGN wscurr = SIMCTR.CURPER
       wsclosed = SIMCTR.CLOSEPER.
OPEN QUERY qry-{&tmptable} FOR EACH   bfr{&tmptable} NO-LOCK.
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
CLOSE QUERY qry-{&tmptable}.
HIDE FRAME frm-main.

