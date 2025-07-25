/* Program.................dbint.p
   Notes:...... Interest raising module
   Author:.................S. Mawire
*/
DEF VAR a AS INT.
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "landscape"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10          NO-UNDO.
DEF VAR varUser    AS CHAR FORM "xxx" INITIAL "01".
DEF VAR wsTransId AS DEC FORM "99999999999999".
DEF VAR wsTime    AS CHAR FORM "x(8)".
DEF VAR wsPer     AS INT FORM "999999".
DEF VAR wsCharge  LIKE dbcmf.accbal.
DEF VAR wsInt  LIKE dbcmf.accbal.
DEF VAR wsNo      AS INT. /* Count of months with meter readings */
DEF VAR wsT       LIKE dbtmf.tear.
DEF VAR wsFreq    AS INT.
DEF VAR wsDate    AS DATE INITIAL TODAY.
DEF VAR wsSeq     AS INT.
DEF VAR vatLedger LIKE glmf.acct.
DEF VAR wsStart   LIKE dbcmf.dbacc.
DEF VAR wsEnd     LIKE dbcmf.dbacc.
DEF VAR wsstatus  AS CHAR FORM "X(40)".
DEF VAR  varDescrip LIKE dbhtf.descrip.
DEF  VAR wsDr     LIKE wsint.
DEF  VAR wsCr      LIKE wsint.
DEF VAR wsTDr     LIKE wsint.
DEF VAR wsTCr     LIKE wsint.
DEF  VAR wsTotal LIKE wsint.
DEF VAR wsRate   AS DEC.
DEF VAR  wsSource LIKE dbgl.SOURCE INITIAL "DB".
DEF VAR wsage AS INT.
DEF VAR X AS INT.
DEF VAR t AS INT.

DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "Process".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 10.5.

DEFINE FRAME frm-main
    SKIP(1)
    wsPer     COLON 30 LABEL "Enter Accounting Period" SKIP(.5)
    wsDate    COLON 30 LABEL "Enter Transaction Date" SKIP(.5)
    wsAge     COLON 30 LABEL "Enter Arrear Age" VIEW-AS COMBO-BOX
                            list-item-pairs
       "Total Arrears",1, "30 Days+",3,"60 Days+",4, "90 Days+",5,"120 Days+",6 SKIP(0.5)
    wsStart   COLON 30 LABEL "Enter Start Account" SKIP(.5)
    wsEnd     COLON 30 LABEL "Enter End Account" SKIP(2)
    wsStatus  COLON 20 view-as text no-label no-tab-stop
    skip(1)
    btn-ok colon 15
    btn-close colon 70
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 12 COL 3
    with SIZE 99.8 BY 15 view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "DEBTORS INTEREST  RAISING".

FORM 
     dbcmf.dbAcc     LABEL "ACCOUNT"
     dbcmf.NAME      LABEL "DESCRIPTION"
     dbsgr.Descrip   LABEL "SERVICE" FORM "x(15)"
     wsCharge        LABEL "AMOUNT"
     wsInt          LABEL "INTEREST"
    HEADER skip(1) "                         INTEREST RAISING REPORT FOR THE PERIOD:" AT 10 SPACE(2)
    STRING(wsPer)
    "Page: " AT 110 PAGE-NUMBER(a) SKIP(1)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-rpt.

FORM dbgl.Proj          LABEL "PROJECT"
     dbgl.Fund          LABEL "FUND"
     dbgl.dept          LABEL "DEPT"
     dbgl.acct          LABEL "ACCOUNT"
     glmf.DESCRIPTION   LABEL "DESCRIPTION"
     wsDr               LABEL "DEBIT" 
     wsCr               LABEL "CREDIT"
     HEADER skip(1) "                       INTEREST RAISING CONSOLIDATION REPORT FOR THE PERIOD:" AT 10 SPACE(2)
    STRING(wsPer)
    "Page: " AT 110 PAGE-NUMBER(a) SKIP(1)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-lrpt.



ON CHOOSE OF btn-oK IN FRAME frm-main DO:
     session:set-wait-state("").
     ASSIGN wsAge = INT(wsAge:SCREEN-VALUE)
            wsdate
            wsStart = DEC(wsStart:SCREEN-VALUE)
            wsEnd =   DEC(wsEnd:SCREEN-VALUE)
            wsPer = INT(wsPer:SCREEN-VALUE).
            wsTime    = STRING(TIME,"HH:MM:SS").
            wsTransID = DEC (string(YEAR(TODAY),"9999")
                          + string(MONTH(TODAY),"99" )
                          + string(DAY(TODAY),"99")
                          + SUBSTR(STRING(wsTIME),1,2) 
                          + SUBSTR(STRING(wsTIME),4,2) 
                          + SUBSTR(STRING(wsTIME),7,2) ).
    IF wsPer > SIMCTR.CURPER OR wsPer <= SIMCTR.CLOSEPER THEN DO:
        MESSAGE "Accounting Period entered is in valid please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        FIND FIRST tblForex WHERE tblForex.txtCur = simctr.dbCur AND date(wsDate:SCREEN-VALUE) >= tblForex.DtRate  NO-LOCK NO-ERROR.
         IF AVAILABLE tblForex THEN DO:
            ASSIGN wsRate = tblForex.decRate.
         END.
         IF NOT AVAILABLE tblForex  THEN DO:
             FIND LAST tblForexH WHERE tblForexH.txtCur = simctr.dbCur AND date(wsDate:SCREEN-VALUE) >= tblForexH.dtRate NO-LOCK NO-ERROR.
                IF AVAILABLE tblForexH THEN DO:
                    ASSIGN wsRate = tblForexH.decRate.
                END.
         END.
        {PrintOpt.i &stream-name="stream a"
                    &print-prog="Interest.ip"
                    &paged}
    END.
    RETURN.
END.

/********** MAIN LOGIC **********/

ENABLE ALL WITH FRAME frm-main.
FIND FIRST simctr NO-LOCK NO-ERROR.
ASSIGN wsPer:SCREEN-VALUE = STRING(SIMCTR.CURPER)
       wsDate:SCREEN-VALUE = STRING(TODAY)
       wsStart:SCREEN-VALUE = "0"
       wsEnd:SCREEN-VALUE = "9999999999998"
       wsAge:SCREEN-VALUE = "1".
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.


PROCEDURE Interest.ip:
/*DO TRANSACTION ON ERROR UNDO, LEAVE: */
FOR EACH dbcmf WHERE dbcmf.dbacc >= wsStart AND dbcmf.dbacc <= wsEnd AND dbcmf.AccStat = 0:
    wsStatus = "Process Rates and Services......" + STRING(dbcmf.dbacc).
    DISPLAY wsStatus WITH FRAME frm-Main.
    FOR EACH dbblf WHERE dbblf.dbAcc = dbcmf.dbacc:
        FIND FIRST dbInt WHERE dbInt.cons = dbcmf.cons AND dbInt.Sgrp = dbblf.Sgrp NO-LOCK NO-ERROR.
        IF AVAILABLE dbint THEN DO:
            FIND FIRST dbsgr WHERE dbsgr.Sgrp = dbblf.Sgrp NO-LOCK NO-ERROR.
            wscharge = dbblf.Int. /*include unpaid Interest */
            DO X = wsage TO 14:
                wscharge = wscharge + amt[X].
            END.
            IF wsCharge > 0 THEN DO:
               wsint = ROUND((wscharge *  (dbInt.decInt / 100 / 12)),2).
               RUN writ-ip.
            END.
        END. 
    END.
END.
{glcon.i}
END. /*.. EOP */

PROCEDURE Writ-ip:
   ASSIGN  dbcmf.AccBal = dbcmf.AccBal + wsint
           dbblf.INT = dbblf.INT + wsint
           dbblf.dbAmt[1] = dbblf.dbAmt[1] + wsInt 
           dbblf.dbAmt[2] = dbblf.dbAmt[2] + ROUND((wsInt * wsRate),2). 

            /* create monthly transaction */
    IF wsInt <> 0.00 THEN DO:
       CREATE dbmtf.
       ASSIGN dbmtf.Accper  = wsPer
              dbmtf.Amt     = wsInt
              dbmtf.Vat     = 0
              dbmtf.dbacc   = dbcmf.dbacc
              dbmtf.Sgrp    = dbsgr.Sgrp
              dbmtf.Tarif   = 0
              dbmtf.prog    = "dbint.p"
              dbmtf.ref     =  STRING(dbsgr.Sgrp)
              dbmtf.trDate  = wsDate
              dbmtf.DESCRIP = "Interest" + string(dbblf.Sgrp).
                /* History record */
       CREATE dbhtf.
       ASSIGN dbhtf.Accper  = wsPer
              dbhtf.dbacc   = dbcmf.dbacc
              dbhtf.Amt     = wsInt
              dbhtf.Vat     = 0
              dbhtf.Iledger = dbsgr.IntLedger
              dbhtf.proj    = dbsgr.proj
              dbhtf.dept    = dbsgr.dept
              dbhtf.ref     = STRING(dbsgr.Sgrp)
              dbhtf.Sgrp    = dbsgr.Sgrp
              dbhtf.Tarif   = 0
              dbhtf.trDate  = wsDate
              dbhtf.PoDate  = TODAY
              dbhtf.prog    = "dbint.p"
              dbhtf.TransID = wsTransId  
              dbhtf.uid     = varUser
              dbhtf.DESCRIP = "Interest" + string(dbblf.Sgrp). 
                /* create record for income Ledger transaction */
         FIND FIRST dbgl WHERE dbgl.acct = dbsgr.IntLedger AND dbgl.period = wsper
                           AND dbgl.TransID = wsTransId no-error.
        IF NOT AVAILABLE dbgl THEN DO:
               CREATE dbgl.
               ASSIGN dbgl.acct = dbsgr.IntLedger
                      dbgl.proj = dbsgr.proj
                      dbgl.dept = dbsgr.dept
                      dbgl.period = wsper
                      dbgl.TransID = wsTransId
                      dbgl.trDATE  = wsDate
                      dbgl.UID     = varUser
                      dbgl.REF     = "dbint.p"
                      dbgl.DESCRIP = "Online Interest Raising"
                      dbgl.CREDATE = TODAY
                      dbgl.SOURCE = wsSource.
        END.
        ASSIGN dbgl.AMT = dbgl.AMT + ROUND((wsInt * -1 * wsRate),2). 
                    /* Create record for the Control Ledger record */
        FIND FIRST dbgl WHERE dbgl.acct = dbsgr.ctrLedger AND dbgl.period = wsper
                          AND dbgl.TransID = wsTransId no-error.
        IF NOT AVAILABLE dbgl THEN DO:
               CREATE dbgl.
               ASSIGN dbgl.acct = dbsgr.ctrLedger
                      dbgl.proj = dbsgr.proj
                      dbgl.dept = dbsgr.dept
                      dbgl.period = wsper
                      dbgl.TransID = wsTransId
                      dbgl.trDATE  = wsDate
                      dbgl.UID     = varUser
                      dbgl.REF     = "dbint.p"
                      dbgl.DESCRIP = "Online Interest Raising"
                      dbgl.CREDATE = TODAY
                      dbgl.SOURCE = wsSource.
        END.
        ASSIGN dbgl.AMT = dbgl.AMT + ROUND((wsInt * wsRate),2).
        DISPLAY STREAM a dbcmf.dbAcc dbcmf.NAME dbsgr.Descrip wsCharge wsInt WITH FRAME frm-rpt.
        DOWN STREAM a WITH FRAME frm-rpt.
        ASSIGN wsTotal = wsTotal + ROUND((wsInt * wsRate),2).
    END.
END.
