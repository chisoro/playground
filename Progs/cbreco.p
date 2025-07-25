/* Program.................CBReco.p
   Notes:................. Bank Reconciliation
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
DEF VAR wsAmt LIKE cbkmf.StatBal.
DEF VAR wsOp  LIKE cbkmf.StatBal.
DEF VAR wsCBtotal LIKE cbkmf.StatBal.
DEF VAR wsUnrec LIKE cbkmf.StatBal.
DEF  VAR wsper AS INT.
DEF VAR wsTitle AS CHAR FORM "x(65)".
DEF VAR wsTitle1 AS CHAR FORM "x(65)".
DEFINE BUTTON btn-ok LABEL "RECONCILE".
DEFINE BUTTON btn-exit LABEL "CLOSE".
DEF RECT rect-1 SIZE 96.5 BY 3.8.
DEF RECT rect-2 SIZE 96.5 BY 2.0.

DEFINE QUERY q1 FOR bfrCBTrans SCROLLING.
DEFINE BROWSE b1 QUERY q1 NO-LOCK 
    DISPLAY Accper LABEL "PERIOD"
            trDate LABEL "DATE"
            Ref     LABEL "REF"
            Descrip LABEL "NARRATION" FORM "X(40)"
            amount LABEL "AMOUNT" Recon LABEL "RECON"
    ENABLE Recon WITH 21 DOWN NO-ASSIGN SEPARATORS NO-LABEL LABEL-FGCOLOR 9.

FORM SPACE(5) bfrCBTrans.Descrip 
     bfrCBTrans.amount
     bfrCBTrans.Accper 
     bfrCBTrans.trDate 
     bfrCBTrans.Ref
    HEADER skip(2) "BANK RECONCILIATION STATEMENT:" AT 5 SPACE(2) "Page: " AT 65 TRIM(STRING(PAGE-NUMBER(a)))
     SKIP(1)
     "BANK:- " AT 5 wsTitle SKIP
     wsTitle1   AT 5 SKIP(1) "UNRECONCILED ITEMS" AT 5 "PERIOD TR-DATE  REFERENCE" AT 61 SKIP
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-rpt.

DEFINE FRAME f1
     SKIP(0.5)
    bfrcbkmf.bank COLON 22 VIEW-AS FILL-IN LABEL "Bank"
    bfrcbkmf.descrip VIEW-AS TEXT NO-LABEL SKIP(0.2)
    bfrcbkmf.StatNo  COLON 22 VIEW-AS TEXT LABEL "Statement Number"
    bfrcbkmf.StatBal COLON 70 VIEW-AS TEXT LABEL "Opening Balance" 
    wsPer            COLON 22 FORM "999999" LABEL "Reconcile to Period"
    wsBal            COLON 70  LABEL "Closing Balance"
    rect-1 AT ROW 1 COLUMN 4
    SKIP(0.5) SPACE(3) b1 SKIP(0.5)
    rect-2 AT ROW 22.5 COLUMN 4
    btn-ok AT ROW 23 COLUMN 20 SPACE(40) btn-exit
    WITH SIZE 104 BY 25.5 SIDE-LABEL
     TITLE "Bank Reconciliations" VIEW-AS DIALOG-BOX.

ON ROW-LEAVE OF b1 IN FRAME f1 /* No-Assign Browser */
DO:
    /* If record exists and was changed in browse, update it. */
    IF BROWSE b1:CURRENT-ROW-MODIFIED then DO:
        GET CURRENT q1 EXCLUSIVE-LOCK.
        IF CURRENT-CHANGED bfrCBTrans THEN DO:
        MESSAGE "This record has been changed by another user."
        SKIP "Please re-enter your changes.".
        DISPLAY Accper trDate Descrip Ref amount Recon WITH BROWSE b1.
        RETURN NO-APPLY.
    END.
    ELSE /* Record is the same, so update it with exclusive-lock */
        ASSIGN INPUT BROWSE b1 Recon.
        /* Downgrade the lock to a no-lock. */
        GET CURRENT q1 NO-LOCK.
    END.
END.

ON 'enter':U OF bfrcbkmf.bank IN FRAME f1
    OR 'TAB' OF bfrcbkmf.bank IN FRAME f1
DO:
    FIND FIRST bfrcbkmf WHERE bfrcbkmf.bank = INT(bfrcbkmf.bank:SCREEN-VALUE)
         EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE bfrcbkmf THEN DO:
        IF LOCKED  bfrcbkmf THEN MESSAGE "Record LOCKED" VIEW-AS ALERT-BOX.
        DISPLAY bfrcbkmf.descrip bfrcbkmf.StatBal bfrcbkmf.StatNo + 1  @ bfrcbkmf.StatNo WITH FRAME f1.
        wsAmt = bfrcbkmf.StatBal.
           APPLY 'tab' TO SELF.
    END.
    RETURN.
END.

ON 'enter':U OF wsPer IN FRAME f1
    OR 'TAB' OF wsPer IN FRAME f1
DO:
    OPEN QUERY q1 FOR EACH bfrCBTrans WHERE bfrCBTrans.bank = bfrcbkmf.bank
                               AND bfrCBTrans.StatNo = 0 AND  bfrCBTrans.seq = 0
        BY bfrCBTrans.Accper BY bfrCBTrans.trDate.

     APPLY 'tab' TO SELF.
    RETURN.
END.

         
ON CHOOSE OF btn-ok IN FRAME f1
DO:
 DO TRANSACTION ON ERROR UNDO, LEAVE:
    ASSIGN wsOp = bfrcbkmf.StatBal.
    wsAmt = wsOp.
    FOR EACH bfrCBTrans WHERE bfrCBTrans.bank = bfrcbkmf.bank AND bfrCBTrans.Recon = "Y"
                       AND bfrCBTrans.StatNo = 0: 
          wsAmt = wsAmt + bfrCBTrans.amount.
    END.
   IF wsAmt = DEC(wsBal:SCREEN-VALUE) THEN DO:
      /* MESSAGE wsAmt "/" bfrcbkmf.bank VIEW-AS ALERT-BOX. */
           MESSAGE "Statement has reconcilled " wsAmt VIEW-AS ALERT-BOX.
           FOR EACH bfrCBTrans WHERE bfrCBTrans.bank = bfrcbkmf.bank AND bfrCBTrans.Recon = "Y"
                                 AND bfrCBTrans.StatNo = 0:
                ASSIGN bfrCBTrans.Recon = "U"
                       bfrCBTrans.StatNo = bfrcbkmf.StatNo + 1.
           END.
           ASSIGN bfrcbkmf.StatBal = DEC(wsBal:SCREEN-VALUE)
                  bfrcbkmf.StatNo = bfrcbkmf.StatNo + 1.
           CREATE cbrec.
           ASSIGN cbrec.bank    = bfrcbkmf.bank
                  cbrec.Statno  = bfrcbkmf.StatNo
                  cbrec.closeBal = bfrcbkmf.StatBal
                  cbrec.OpenBal  = wsOp
                  cbrec.per      = INT(wsPer:SCREEN-VALUE).
           {PrintOpt.i &stream-name="stream a"
                    &print-prog="Report.ip"
                    &paged} 
   END.
   ELSE 
        MESSAGE "Statement does NOT reconcile " wsAmt " :" DEC(wsBal:SCREEN-VALUE)
              VIEW-AS ALERT-BOX. 
    APPLY 'CLOSE' TO THIS-PROCEDURE.
 END.
END.

/********** MAIN LOGIC **********/
wsbal:SCREEN-VALUE = "0.00".
ENABLE ALL WITH FRAME f1.
WAIT-FOR CHOOSE OF btn-exit OR CLOSE OF THIS-PROCEDURE.
RELEASE bfrCBKmf.
RELEASE bfrCBTrans.
HIDE FRAME f1.

PROCEDURE Report.ip: 
    wsTitle = bfrCBkmf.descrip.
    wsTitle1 = "STATEMENT NUMBER: " + STRING(cbrec.StatNo) 
             + "                       ENDING PERIOD: " + STRING(cbrec.per).
    FOR EACH bfrCBTrans WHERE bfrCBTrans.Recon <> "U" AND bfrCBTrans.StatNo = 0
                           AND bfrCBTrans.Accper <= cbrec.per AND bfrCBTrans.bank = cbrec.bank
                           AND bfrCBTrans.seq = 0:
        DISPLAY STREAM a bfrCBTrans.Accper bfrCBTrans.trDate bfrCBTrans.Ref bfrCBTrans.Descrip 
        bfrCBTrans.amount WITH FRAME frm-rpt.
        DOWN STREAM a WITH FRAME frm-rpt.
        wsUnrec = wsUnrec + bfrCBTrans.amount.
    END.
    UNDERLINE STREAM a bfrCBTrans.amount WITH FRAME frm-rpt.
    DISPLAY STREAM a wsUnRec @ bfrCBTrans.amount WITH FRAME frm-rpt.
    DOWN STREAM a 5  WITH FRAME frm-rpt.
    DISPLAY STREAM a "BALANCE RECONCILITAION" @ bfrCBTrans.Descrip WITH FRAME frm-rpt.
    UNDERLINE STREAM a bfrCBTrans.Descrip WITH FRAME frm-rpt.
    DOWN STREAM a WITH FRAME frm-rpt.
    wsCBTotal = cbrec.closebal + wsUnrec.
    DISPLAY STREAM a "BALANCE AS PER BANK STATEMENT" @ bfrCBTrans.Descrip cbrec.closebal @ bfrCBTrans.amount WITH FRAME frm-rpt.
    DOWN STREAM a WITH FRAME frm-rpt.
    DISPLAY STREAM a "UNRECONCILED ITEMS" @ bfrCBTrans.Descrip wsUnrec @ bfrCBTrans.amoun WITH FRAME frm-rpt.
    DOWN STREAM a WITH FRAME frm-rpt.
    DISPLAY STREAM a "BALANCE AS PER OUR CASHBOOK" @ bfrCBTrans.Descrip wsCBTotal @ bfrCBTrans.amoun WITH FRAME frm-rpt.
    DOWN STREAM a WITH FRAME frm-rpt.
    APPLY 'CLOSE' TO THIS-PROCEDURE.
END.
 
