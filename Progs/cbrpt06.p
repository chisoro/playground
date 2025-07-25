/* Program.................cbrpt06.p
   Notes:................. Cash Resource Availability by Date
   Author:.................S. Mawire
   Edited:.................S. Chisoro
   to add functionality to use date
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF TEMP-TABLE tmptable 
    FIELD bank LIKE  cbkmf.bank
    FIELD acb LIKE cbkmf.acb
    FIELD descrip LIKE cbkmf.descrip
    FIELD amt LIKE cbkmf.bal
    FIELD txtCur LIKE cbkmf.txtCur
    FIELD bal LIKE cbkmf.bal.

DEF VAR Current-Record AS ROWID.
DEF VAR method-status  AS LOGICAL.
DEF VAR wscurr      LIKE SIMCTR.CURPER.
DEF VAR wsclosed    LIKE  SIMCTR.CLOSEPER.
DEF VAR wsper       LIKE simctr.closeper.
DEF VAR wsAmt       LIKE cbtrans.amount.
DEF VAR wsAmt1      LIKE cbtrans.amount.
DEF VAR wsDate   LIKE cbTrans.trDate.
DEF VAR wsBBal LIKE cbtrans.amount.
DEF VAR wsABal LIKE cbtrans.amount.
DEF VAR wsextgl LIKE cbtrans.amount.
DEF VAR wsBank LIKE cbkmf.bank.
DEF VAR wsCB LIKE cbkmf.acb.
DEF VAR wsDescrip LIKE cbkmf.descrip.
DEF VAR wsCur LIKE cbkmf.txtCur.
DEF VAR wsTitle0  AS CHAR FORM "X(80)".
DEF VAR w-orientation AS CHAR      INITIAL "Potrait"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10.0          NO-UNDO.

DEF BUTTON btn-exit  LABEL "CLOSE".
DEF BUTTON btn-ok LABEL "PRINT".

DEFINE QUERY qry-bank FOR  tmptable scrolling.
DEF BROWSE brw-bank QUERY qry-bank
   DISPLAY tmptable.bank COLUMN-LABEL "BANK" tmptable.descrip COLUMN-LABEL "ACCOUNT NAME" WIDTH 40 
    tmptable.txtCur COLUMN-LABEL "CURRENCY" WIDTH 15 tmptable.amt COLUMN-LABEL "AMOUNT" WIDTH 15 
    /* tmptable.bal COLUMN-LABEL "TOTAL" WIDTH 15 */
       WITH 15 DOWN SEPARATORS.

DEFINE QUERY qry1-cashbk FOR tmptable scrolling.
DEF BROWSE brw1-cashbk QUERY qry1-cashbk
    DISPLAY tmptable.Acb COLUMN-LABEL "CASHBOOK" tmptable.descrip COLUMN-LABEL "CASHBOOK NAME" WIDTH 40
    tmptable.amt COLUMN-LABEL "AMOUNT" WIDTH 15
     /*tmptable.bal COLUMN-LABEL "TOTAL" WIDTH 15 */
       WITH 15 DOWN SEPARATORS.


DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 graphic-edge  NO-FILL
     SIZE 184 BY 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 184 BY 17.5.


DEF FRAME frm-main
    SKIP(1)
    wsDate LABEL "RESOURCES BY DATE" COLON 30  SPACE(20)
    SKIP(1)
    "BANKS"             COLON 10
    "CASHBOOKS"         COLON 100  SKIP(0.5)
    brw-bank AT ROW 5 COL 4 
    brw1-cashbk AT ROW 5 COL 100 SKIP(2) 
    btn-ok COLON 10 btn-exit COLON 174 SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 19 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
    SIZE 190 BY 23
    TITLE "CASH RESOURCES AVAILABILITY" view-as dialog-box.


FORM tmptable.DESCRIP    LABEL "DESCRIPTION" FORM "x(40)"
     tmptable.txtCur     LABEL "CURRENCY"           
     tmptable.AMT        COLUMN-LABEL "AMOUNT"      
     tmptable.bal        COLUMN-LABEL "TOTAL"  FORM "zzz,zzz,zz9.99-" 
    HEADER skip(3) wsTitle0 SKIP(2) "CASH RESOURCES AVAILABILITY AS AT: " wsDate 
    "Page: " AT 60 PAGE-NUMBER(a) SKIP(1) 
    "--------------------------------------------------------------------------------------"
     SKIP(2)
    WITH DOWN STREAM-IO FONT 6 WIDTH 92 CENTERED NO-BOX FRAME frm-rpt.

ON 'choose':U OF btn-ok  
DO:
   session:set-wait-state(""). 
   ASSIGN wsDate.
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="Report.ip"
                    &paged} 
  /*APPLY "close" TO THIS-PROCEDURE.*/
    APPLY 'entry' TO wsDate.
        RETURN.
END.

ON  'enter':u OF wsDate IN FRAME  frm-main
     
DO:
    FOR EACH tmptable:
        DELETE tmptable.
    END.
    /*add date validation before proceeding */
    
    FOR EACH cbkmf NO-LOCK:
      wsbbal = 0.
      wsabal = 0.
      wsBank = cbkmf.bank.
      wscb = cbkmf.acb.
      wsDescrip = cbkmf.descrip.
      wsCur = cbkmf.txtCur.
      FOR EACH cbtrans WHERE (cbtrans.bank = wsBank /*AND cbtrans.acb = wscb */ ) AND (cbtrans.trDate <= DATE(wsDate:SCREEN-VALUE)) 
          AND cbtrans.seq = 0 NO-LOCK:
        wsBBal = wsBBal + cbtrans.amount .
      END.
      FIND LAST tblForexH WHERE tblForexH.txtCur = wsCur AND tblForexH.dtRate <= date(wsDate:SCREEN-VALUE) NO-LOCK NO-ERROR.
                IF AVAILABLE tblForexH THEN DO:
                   wsAmt = wsBBal * tblForexH.decRate.
                END.
                IF NOT AVAILABLE tblForexH THEN DO:
                    wsAmt = wsBBal * 1.
                END.
      CREATE tmptable.
       ASSIGN tmptable.bank = wsbank
           tmptable.acb = wscb
           tmptable.descrip = wsDescrip
           tmptable.amt = wsBbal
           tmptable.txtCur = wsCur
           tmptable.bal = wsAmt. 
    END.
   OPEN QUERY qry1-cashbk FOR EACH  tmptable WHERE tmptable.acb <> 0 NO-LOCK.
   OPEN QUERY qry-bank FOR EACH   tmptable WHERE  tmptable.bank <> 0 NO-LOCK.
 RETURN.
END.




/********** MAIN LOGIC **********/
FIND FIRST simctr NO-ERROR.
ASSIGN wscurr = SIMCTR.CURPER
       wsclosed = SIMCTR.CLOSEPER.
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
CLOSE QUERY qry-bank.
CLOSE QUERY qry1-cashbk.
HIDE FRAME frm-main.


PROCEDURE Report.ip:  
          wsbbal = 0.
          wsabal = 0.
          wsextgl = 0.
           /* print transactions */
          
          DISPLAY STREAM a "Banks" @ tmptable.descrip WITH FRAME frm-rpt.
          DOWN STREAM a WITH FRAME frm-rpt.
           /*DISPLAY STREAM a tmptable.bank tmptable.descrip  tmptable.txtCur tmptable.amt tmptable.bal WITH FRAME frm-rpt.
          DOWN STREAM a WITH FRAME frm-rpt.*/
          FOR EACH tmptable WHERE tmptable.bank <> 0  AND tmptable.amt <> 0 NO-LOCK:
              wsbbal  = wsbbal + tmptable.bal.
              DISPLAY STREAM a "     " + STRING(tmptable.bank,"z9")  + "  " + tmptable.descrip @ tmptable.descrip 
                  tmptable.txtCur tmptable.amt wsbbal @ tmptable.bal WITH FRAME frm-rpt.
              DOWN STREAM a WITH FRAME frm-rpt.
          END.
          DOWN STREAM a WITH FRAME frm-rpt.
          DISPLAY STREAM a "---------------" @ tmptable.bal WITH FRAME frm-rpt.
          DOWN STREAM a WITH FRAME frm-rpt.
          DISPLAY STREAM a "BANK TOTALS" @ tmptable.descrip wsbbal @ tmptable.bal WITH FRAME frm-rpt.
          DOWN 2 STREAM a WITH FRAME frm-rpt.
          DISPLAY STREAM a "Cash Books" @ tmptable.descrip WITH FRAME frm-rpt.
          DOWN STREAM a WITH FRAME frm-rpt.
          FOR EACH tmptable WHERE tmptable.acb <> 0 AND tmptable.amt <> 0 NO-LOCK:
            wsabal  = wsabal + tmptable.bal.
            DISPLAY STREAM a "     " + STRING(tmptable.acb,"z9") + " " + tmptable.descrip @ tmptable.descrip 
                tmptable.amt wsabal @ tmptable.bal WITH FRAME frm-rpt.
              DOWN STREAM a WITH FRAME frm-rpt.
          END.
          DOWN STREAM a WITH FRAME frm-rpt.
          DISPLAY STREAM a "---------------" @ tmptable.bal WITH FRAME frm-rpt.
          DOWN STREAM a WITH FRAME frm-rpt.
          DISPLAY STREAM a "CASHBOOK TOTALS" @ tmptable.descrip wsabal @ tmptable.bal WITH FRAME frm-rpt.
          DOWN STREAM a WITH FRAME frm-rpt.
          wsextgl = wsabal - wsbbal.
          DOWN STREAM a WITH FRAME frm-rpt.
          DISPLAY STREAM a "---------------" @ tmptable.bal WITH FRAME frm-rpt.
          DOWN STREAM a WITH FRAME frm-rpt.
          DISPLAY STREAM a "EXCHANGE RATE GAIN/(LOSS)" @ tmptable.descrip wsextgl @ tmptable.bal WITH FRAME frm-rpt.
          DOWN STREAM a WITH FRAME frm-rpt.
END.
