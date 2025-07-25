/* Program.................dbsg07.p
   Notes:...... Debit raising module
   Author:.................S. Mawire
*/
DEF STREAM b.
DEF VAR a AS INT.
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "landscape"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10          NO-UNDO.
DEF VAR varUser    AS CHAR FORM "xxx".
DEF VAR wsTransId AS DEC FORM "99999999999999".
DEF VAR wsTime    AS CHAR FORM "x(8)".
DEF VAR wsPer     AS INT FORM "999999".
DEF VAR wsCharge  LIKE dbcmf.accbal FORM "zzz,zzz,zz9.99-".
DEF VAR wsRate    LIKE tblforex.decRate.
DEF VAR wsTear    LIKE dbtmf.Tear.
DEF VAR dbTear    LIKE dbtmf.Tear.
DEF VAR wsVat     LIKE wsCharge.
DEF VAR wsTotal   LIKE wsCharge.
DEF VAR wsPrAmt   LIKE wsCharge.
DEF VAR wsExAmt   LIKE wsCharge.
DEF VAR wsTVat    LIKE wsCharge.
DEF VAR wsDr      LIKE wsCharge FORM "zzz,zzz,zzz,zz9.99-".
DEF VAR wsCr      LIKE wsCharge FORM "zzz,zzz,zzz,zz9.99-".
DEF VAR wsTDr     LIKE wsCharge.
DEF VAR wsTCr     LIKE wsCharge.
DEF VAR wsRef     LIKE dbhtf.ref.
DEF VAR wsuCharge LIKE dbtmf.charge.
DEF VAR wsQty     AS DEC.
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
DEF VAR wsm AS INT.
DEF VAR wsSource LIKE dbgl.SOURCE INITIAL "BD".
DEF VAR wsLedger LIKE glmf.acct.
DEF VAR wsq       AS DEC EXTENT 5.
DEF  VAR wsUpper AS DEC.
DEF VAR j AS INT.
DEF VAR varDept LIKE  dbtmf.dept.
DEF VAR varProj LIKE dbtmf.Proj.
DEF VAR varFund LIKE gldept.fund.
DEF VAR X AS INT.
DEF VAR t AS INT.
DEF VAR wsTitle AS CHAR INITIAL "DEBIT RAISING CONSOLIDATION REPORT FOR THE PERIOD:".

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
    wsStart   COLON 30 LABEL "Enter Start Account" SKIP(.5)
    wsEnd     COLON 30 LABEL "Enter End Account" SKIP(2)
    wsStatus  COLON 20 view-as text no-label no-tab-stop
    skip(2.5)
    btn-ok colon 15
    btn-close colon 70
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 12 COL 3
    with SIZE 99.8 BY 15 view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "DEBTORS DEBIT RAISING".

FORM 
     dbcmf.dbAcc     LABEL "ACCOUNT"
     dbcmf.NAME      LABEL "DESCRIPTION"
     dbtmf.Descrip   LABEL "TARIFF" FORM "x(20)"
     wsCharge        LABEL "AMOUNT"
     wsVat           LABEL "VAT"
    HEADER skip(1) "                         RATEPAYER DEBIT RAISING REPORT FOR THE PERIOD:" AT 10 SPACE(2)
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
     HEADER skip(1) "     ZWL  DEBIT RAISING CONSOLIDATION REPORT FOR THE PERIOD:" AT 10 SPACE(2)
    STRING(wsPer)
    "Page: " AT 110 PAGE-NUMBER(a) SKIP(1)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-lrpt.

FORM dbgl.Proj          LABEL "PROJECT"
     dbgl.Fund          LABEL "FUND"
     dbgl.dept          LABEL "DEPT"
     dbgl.acct          LABEL "ACCOUNT"
     glmf.DESCRIPTION   LABEL "DESCRIPTION"
     wsDr               LABEL "DEBIT" 
     wsCr               LABEL "CREDIT"
     HEADER skip(1) "     USD  DEBIT RAISING CONSOLIDATION REPORT FOR THE PERIOD:" AT 10 SPACE(2)
    STRING(wsPer)
    "Page: " AT 110 PAGE-NUMBER(a) SKIP(1)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-rpt1.

ON CHOOSE OF btn-oK IN FRAME frm-main DO:
     session:set-wait-state("").
     ASSIGN vatLedger = simctr.vat[1] /* VAT Provision */
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
                    &print-prog="Bill.ip"
                    &paged}
    END.
    APPLY 'entry' TO btn-close IN FRAME frm-main.
    RETURN.
END.

/********** MAIN LOGIC **********/
wsSource = "DB".
ENABLE ALL WITH FRAME frm-main.
FIND FIRST simctr NO-LOCK NO-ERROR.
vatLedger = simctr.vat[1]. 
ASSIGN wsPer:SCREEN-VALUE = STRING(SIMCTR.CURPER)
       wsDate:SCREEN-VALUE = STRING(TODAY)
       wsStart:SCREEN-VALUE = "0"
       wsEnd:SCREEN-VALUE = "9999999999999".
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.


PROCEDURE Bill.ip:
/*DO TRANSACTION ON ERROR UNDO, LEAVE: */
FOR EACH dbcmf WHERE dbcmf.dbacc >= wsStart AND dbcmf.dbacc <= wsEnd AND dbcmf.AccStat = 0:
    wsStatus = "Process Rates and Services......" + STRING(dbcmf.dbacc).
    DISPLAY wsStatus WITH FRAME frm-Main.
    DO t = 1 TO 14:
        ASSIGN wscharge = 0
               wsVat    = 0
               wsq      = 0
               wsqty    = 0
               wsuCharge = 0.
       IF dbcmf.NextP[t] <= wsPer AND tarif[t] <> 0 THEN DO:
            FIND FIRST dbtmf WHERE dbtmf.Tarif = dbcmf.tarif[t] AND dbtmf.TYPE = 3 NO-LOCK NO-ERROR.
            IF NOT AVAILABLE dbtmf THEN DO:
                MESSAGE "No master tarif record found for " dbcmf.tarif[t]
                    VIEW-AS ALERT-BOX.
                NEXT.
            END. /* eof dbtmf not available */
            ELSE DO: 
                wsref = STRING(dbtmf.Sgrp) + "/" + STRING(dbtmf.Tarif).
                wsuCharge = dbtmf.Charge.
                IF dbtmf.vCharge = YES THEN DO:
                   FIND FIRST dbvtar WHERE dbvTar.dbAcc = dbcmf.dbacc AND dbvTar.Sgrp = dbtmf.sgrp
                                     AND dbvTar.Tarif  = dbtmf.tarif AND dbvTar.TYPE   = dbtmf.TYPE NO-ERROR.
                   IF AVAILABLE dbvTar THEN
                       wsuCharge = dbvTar.Charge.
                END. 
                CASE dbtmf.SOURCE:
                    WHEN 1 THEN /* Units */
                        ASSIGN wscharge = ROUND((dbcmf.Units[t] * wsUcharge),2) 
                               wsvat    = ROUND(( wscharge * (dbtmf.Vat% / 100 )),2).
                        WHEN 2 THEN /* Area */
                        ASSIGN wscharge = ROUND((dbcmf.Units[t] * wsUcharge * dbcmf.Size),2)
                               wsvat    = ROUND(( wscharge * (dbtmf.Vat% / 100 )),2).
                        WHEN 3 THEN DO: /* water consumption */
                            FIND FIRST dbmmf WHERE dbmmf.dbAcc = dbcmf.dbacc AND dbmmf.mStats <> 3 NO-LOCK NO-ERROR.
                            IF AVAILABLE dbmmf THEN DO:
                                IF dbmmf.RDate[13] <> ? THEN DO:
                                    IF dbmmf.READ[13] >= dbmmf.READ[12] THEN
                                         wsQty =  dbmmf.READ[13] - dbmmf.READ[12].
                                    ELSE IF dbmmf.READ[13] < dbmmf.READ[12] THEN DO: /* Meter turn */
                                        wsUpper = 1.
                                        DO j = 1 TO LENGTH(STRING(INT(dbmmf.READ[12]))): /* Set Maxmum Meter Reading */
                                            wsUpper = wsUpper * 10. 
                                        END.
                                        ASSIGN wsQty =  ( wsUpper - dbmmf.READ[12]) + dbmmf.READ[13].
                                    END.
                                END.
                                ELSE
                                IF dbmmf.RDate[13] = ? OR dbmmf.READ[13] = ? THEN DO:
                                        ASSIGN wsQty = 0.
                                               X = 13.
                                               wsNo = 0.
                                         DO WHILE X >=  2:
                                              ASSIGN wsQty = wsQty + dbmmf.consum[X] /*(dbmmf.READ[X] - dbmmf.READ[X - 1]) */ 
                                                              WHEN dbmmf.consum[X] > 0 /*(dbmmf.READ[X] - dbmmf.READ[X - 1]) > 0 */
                                                     wsNo = wsNo + 1 WHEN dbmmf.consum[X] > 0 /*(dbmmf.READ[X] - dbmmf.READ[X - 1]) > 0 */
                                                     X = X - 1.
                                         END.
                                         ASSIGN wsQty = wsQty / wsNo WHEN wsNo <> 0
                                                wsQty = 0            WHEN wsNo = 0.
                                END.
                                ASSIGN  wsTear = 0
                                        wsCharge = 0
                                        dbTear   = 0.
                                ASSIGN dbtear[5] = dbtmf.tear[5]
                                       dbtear[4] = dbtmf.tear[4] - dbtmf.tear[3]
                                       dbtear[3] = dbtmf.tear[3] - dbtmf.tear[2]
                                       dbtear[2] = dbtmf.tear[2] - dbtmf.tear[1]
                                       dbTear[1] = dbtmf.tear[1].
                                IF dbmmf.TYPE = 1 THEN DO: /* Gallon metered meters */
                                    wsQty = ROUND((wsQty / 220),2).
                                END.
                                DO X = 1 TO 5:
                                     IF wsQty > 0 THEN
                                        ASSIGN wsTear[X] = dbtear[X] WHEN wsQty >= dbtear[X]
                                               wsTear[X] = wsQty WHEN wsQty < dbtear[X]
                                               wsQty     = wsQty - wsTear[X]
                                               wsCharge  = wsCharge + (wsTear[X] * dbtmf.tCharge[X]).
                                END.
                                  wsCharge = wsCharge + dbtmf.Charge. /* add basic charge */
                                  wsvat    = ROUND(( wscharge * (dbtmf.Vat% / 100 )) ,2).
                            END.
                        END. /* end of case 3 */
                        WHEN 4 THEN DO: /*Quantity */
                            FIND FIRST dbQty WHERE dbQty.dbAcc = dbcmf.dbacc NO-ERROR.
                            IF AVAILABLE dbQty THEN DO:
                               wsQty = dbQty.Qty[13].
                               IF wsQty <= tear[1] THEN
                                   wsq[1] = dbQty.Qty[13].
                                ELSE IF wsQty > tear[1] AND wsQty <= tear[2]THEN DO:
                                    ASSIGN wsq[1] = tear[1]
                                           wsq[2] = wsqty - tear[1].
                                END.
                                ELSE IF wsQty > tear[2] AND wsQty <= tear[3]THEN DO:
                                    ASSIGN wsq[1] = tear[1]
                                           wsq[2] = tear[2] - tear[1]
                                           wsq[3] = wsqty - (wsq[1] + wsq[2]).
                                END.
                                ELSE IF wsQty > tear[3] AND wsQty <= tear[4]THEN DO:
                                    ASSIGN wsq[1] = tear[1]
                                           wsq[2] = tear[2] - tear[1]
                                           wsq[3] = tear[3] - tear[2]
                                           wsq[4] = wsqty - (wsq[1] + wsq[2]+ wsq[3]).
                                END.
                                ELSE IF wsQty > tear[4] THEN DO:
                                    ASSIGN wsq[1] = tear[1]
                                           wsq[2] = tear[2] - tear[1]
                                           wsq[3] = tear[3] - tear[2]
                                           wsq[4] = tear[4] - tear[3]
                                           wsq[5] = wsqty - (wsq[1] + wsq[2]+ wsq[3] + wsq[4]).
                                END.
                                DO X = 1 TO 5:
                                   ASSIGN wscharge = wscharge + (wsq[X] / factor[X] * tcharge[X]).
                                END.
                                ASSIGN wscharge = wscharge * dbtmf.Charge
                                        wsvat    = ROUND(( wscharge * (dbtmf.Vat% / 100 )),2).
                                 DO X = 1 TO 12:
                                     ASSIGN dbQty.Qty[X] = dbQty.Qty[X + 1]
                                            dbQty.Qdate[X] = dbQty.Qdate[X + 1].
                                 END.
                                 ASSIGN dbQty.Qty[13] = 0
                                            dbQty.Qdate[13] = ?.
                            END.
                        END.
                 END CASE.
                 varDescrip = dbtmf.Descrip.
                  IF wsCharge = ? THEN
                     wsCharge = 0.
                 IF wsCharge > 0 THEN
                    RUN writ-ip.
                 IF dbtmf.freq = "M" THEN
                    ASSIGN  dbcmf.NextP[t] = dbcmf.NextP[t] + 1 WHEN INT(SUBSTR(STRING(dbcmf.NextP[t]),5,2)) < 12
                            dbcmf.NextP[t] = INT(STRING(INT(SUBSTR(STRING(dbcmf.NextP[t]),1,4)) + 1)+ "01") 
                                                      WHEN INT(SUBSTR(STRING(dbcmf.NextP[t]),5,2)) = 12.
                 ELSE IF dbtmf.freq = "A" THEN
                     ASSIGN dbcmf.NextP[t] = INT(STRING(INT(SUBSTR(STRING(dbcmf.NextP[t]),1,4)) + 1) 
                                                  + SUBSTR(STRING(dbcmf.NextP[t]),5,2)).
                 ELSE IF dbtmf.freq = "Q" THEN DO:
                     IF (INT(SUBSTR(STRING(dbcmf.NextP[t]),5,2)) + 3) - 12 <= 0 THEN
                           ASSIGN wsm = (INT(SUBSTR(STRING(dbcmf.NextP[t]),5,2)) + 3)
                                 dbcmf.NextP[t] = dbcmf.NextP[t] + wsm.
                      ELSE
                         ASSIGN wsm = (INT(SUBSTR(STRING(dbcmf.NextP[t]),5,2)) + 3) - 12
                                dbcmf.NextP[t] = INT(STRING(INT(SUBSTR(STRING(dbcmf.NextP[t]),1,4)) + 1)  + STRING(wsm, "99")).
                 END.
                 ELSE IF dbtmf.freq = "H" THEN DO:
                     IF (INT(SUBSTR(STRING(dbcmf.NextP[t]),5,2)) + 6) - 12 <= 0 THEN
                           ASSIGN wsm = (INT(SUBSTR(STRING(dbcmf.NextP[t]),5,2)) + 6)
                                 dbcmf.NextP[t] = dbcmf.NextP[t] + wsm.
                      ELSE
                         ASSIGN wsm = (INT(SUBSTR(STRING(dbcmf.NextP[t]),5,2)) + 6) - 12
                                dbcmf.NextP[t] = INT(STRING(INT(SUBSTR(STRING(dbcmf.NextP[t]),1,4)) + 1)  + STRING(wsm, "99")).
                 END.
            END. /*eof ELSE AVAILABLE dbtmf */
       END. /*eof tarif <> 0 */
    END. /* eof do t = 1 to 14 */ 

        /* Rates Charges */
    IF dbcmf.rate-tarif <> 0 AND dbcmf.rNextP <= wsPer THEN DO:
        ASSIGN wscharge = 0
               wsVat    = 0
               wsq      = 0
               wsqty    = 0.
        FIND FIRST dbtmf WHERE dbtmf.tarif = dbcmf.rate-tarif AND dbtmf.TYPE = 2 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE dbtmf THEN DO:
           MESSAGE "No master tarif record found for " dbcmf.rate-tarif
                    VIEW-AS ALERT-BOX.
           NEXT.
        END.
        ELSE DO:
            wsref = STRING(dbtmf.Sgrp) + "/" + STRING(dbtmf.Tarif).
            /*IF dbtmf.freq = "A" THEN
               wsFreq = 1.
            ELSE IF dbtmf.freq = "M" THEN
                  wsFreq = 12.
            ELSE IF dbtmf.freq = "Q" THEN
                  wsFreq = 4.
            ELSE IF dbtmf.freq = "H" THEN
                  wsFreq = 6. */
            wsFreq = 1.
            /* put conditions for source */
            ASSIGN wscharge = ROUND((dbcmf.BldValue * dbtmf.BValue),2) 
                            + ROUND((dbcmf.SiteValue * dbtmf.LValue),2).
            IF wsCharge = ? THEN
                wsCharge = 0.
          IF wsCharge <> 0 THEN DO:
                ASSIGN wsCharge = ROUND((wsCharge / wsFreq),2) WHEN wsFreq <> 0
                       wsvat    = ROUND(( wscharge * (dbtmf.Vat% / 100 )),2).
                 varDescrip = dbtmf.Descrip.
                 IF wsCharge > 0 THEN
                    RUN writ-ip.
            END.
            IF dbtmf.freq = "M" THEN
                ASSIGN  dbcmf.rNextP = dbcmf.rNextP + 1 WHEN INT(SUBSTR(STRING(dbcmf.rNextP),5,2)) < 12
                        dbcmf.rNextP = INT(STRING(INT(SUBSTR(STRING(dbcmf.rNextP),1,4)) + 1)+ "01") 
                                              WHEN INT(SUBSTR(STRING(dbcmf.rNextP),5,2)) = 12.
            ELSE IF dbtmf.freq = "A" THEN
                  ASSIGN dbcmf.rNextP = INT(STRING(INT(SUBSTR(STRING(dbcmf.rNextP),1,4)) + 1) 
                                      + SUBSTR(STRING(dbcmf.rNextP),5,2)).
            ELSE IF dbtmf.freq = "Q" THEN DO:
                 IF (INT(SUBSTR(STRING(dbcmf.rNextP),5,2)) + 3) - 12 <= 0 THEN
                       ASSIGN wsm = (INT(SUBSTR(STRING(dbcmf.rNextP),5,2)) + 3)
                              dbcmf.rNextP = dbcmf.rNextP + wsm.
                 ELSE
                     ASSIGN wsm = (INT(SUBSTR(STRING(dbcmf.rNextP),5,2)) + 3) - 12
                            dbcmf.rNextP = INT(STRING(INT(SUBSTR(STRING(dbcmf.rNextP),1,4)) + 1)  + STRING(wsm, "99")).
            END.
            ELSE IF dbtmf.freq = "H" THEN DO:
                 IF (INT(SUBSTR(STRING(dbcmf.rNextP),5,2)) + 6) - 12 <= 0 THEN
                       ASSIGN wsm = (INT(SUBSTR(STRING(dbcmf.rNextP),5,2)) + 6)
                              dbcmf.rNextP = dbcmf.rNextP + wsm.
                 ELSE
                     ASSIGN wsm = (INT(SUBSTR(STRING(dbcmf.rNextP),5,2)) + 6) - 12
                            dbcmf.rNextP = INT(STRING(INT(SUBSTR(STRING(dbcmf.rNextP),1,4)) + 1)  + STRING(wsm, "99")).
            END.
        END.
    END.
END. /* eof dbcmf */

/* water charges */
FOR EACH dbmmf WHERE dbmmf.tarif <> 0 AND dbmmf.mStat <> 3 AND dbmmf.wNextP <= wsPer
                 AND dbmmf.dbacc >= wsStart AND dbmmf.dbacc <= wsEnd:
    wsref = dbmmf.serial.
    ASSIGN wscharge = 0
           wsVat    = 0
           wsq      = 0
           wsqty    = 0.
    wsStatus = "Process Water......" + STRING(dbmmf.dbacc).
    DISPLAY wsStatus WITH FRAME frm-Main.
   
   FIND FIRST dbcmf WHERE dbcmf.dbacc = dbmmf.dbacc NO-ERROR.
   IF AVAILABLE dbcmf AND dbcmf.AccStat = 0 THEN DO:
       FIND FIRST dbtmf WHERE dbtmf.tarif = dbmmf.tarif AND dbtmf.TYPE = 1 NO-LOCK NO-ERROR.
       ASSIGN wsQty = 0.
                   X = 13.
                   wsNo = 0.
                   
       IF dbmmf.READ[13] < dbmmf.READ[12] AND dbmmf.READ[13] > 0 THEN DO: /* meter turn */
              wsUpper = 1.
              DO j = 1 TO LENGTH(STRING(INT(dbmmf.READ[12]))): /* Set Maxmum Meter Reading */
                   wsUpper = wsUpper * 10. 
              END.
              ASSIGN wsQty = ( wsUpper - dbmmf.READ[12]) + dbmmf.READ[13]
                     dbmmf.com[13] = 99
                     varDescrip = dbtmf.descrip + "(" + string(wsQty) + ")".
       END.
       ELSE IF dbmmf.READ[13] = 0  THEN /* Take reading that was imported */
                ASSIGN wsQty = dbmmf.consum[13]
                       varDescrip = "Water " + "(" + string(wsQty) + ")".
        ELSE IF dbmmf.read[13] > dbmmf.READ[12] AND dbmmf.READ[13] <> ? AND dbmmf.READ[12] <> ? THEN DO:
            ASSIGN wsqty = dbmmf.READ[13] - dbmmf.READ[12]. /* Actual Consumption */
                   varDescrip = dbtmf.descrip  + "(" + STRING(dbmmf.READ[13]) + " - " + STRING(dbmmf.READ[12]) + " = " + string(wsQty) + ")".
        END.           
        ELSE IF dbmmf.RDate[13] = ? OR dbmmf.READ[13] = ? THEN DO: /* not read - average */
             ASSIGN wsQty = 0.
                    X = 13.
                    wsNo = 0.
             DO WHILE X >=  2:
                 ASSIGN wsQty = wsQty + dbmmf.consum[X] /*(dbmmf.READ[X] - dbmmf.READ[X - 1]) */ 
                                 WHEN dbmmf.consum[X] > 0 /*(dbmmf.READ[X] - dbmmf.READ[X - 1]) > 0 */
                         wsNo = wsNo + 1 WHEN dbmmf.consum[X] > 0 /*(dbmmf.READ[X] - dbmmf.READ[X - 1]) > 0 */
                          X = X - 1.
             END.
             ASSIGN dbmmf.comm[13]   = 96
                    wsQty = ROUND((wsQty / wsNo),2) WHEN wsNo <> 0
                    wsQty = 0                       WHEN wsNo = 0
                    varDescrip = dbtmf.descrip + "(Average)" + "(" + string(wsQty) + ")".
       END.
        dbmmf.consum[13] = wsqty.
        DO X = 1 TO 12:
           ASSIGN dbmmf.READ[X]   = dbmmf.READ[X + 1]
                  dbmmf.RDATE[X]  = dbmmf.RDATE[X + 1]
                   dbmmf.consum[x] = dbmmf.consum[X + 1].
                       /*dbmmf.comm[x]   = dbmmf.comm[X + 1]. */
        END.
        ASSIGN dbmmf.READ[13]   = ?
               dbmmf.RDate[13]  = ?
               dbmmf.consum[13] = 0.
        ASSIGN  wsTear = 0
                wsCharge = 0.
        ASSIGN dbtear[5] = dbtmf.tear[5]
                dbtear[4] = dbtmf.tear[4] - dbtmf.tear[3]
                dbtear[3] = dbtmf.tear[3] - dbtmf.tear[2]
                dbtear[2] = dbtmf.tear[2] - dbtmf.tear[1]
                dbTear[1] = dbtmf.tear[1].
        IF dbmmf.TYPE = 1 THEN DO: /* Gallon metered meters */
            wsQty = ROUND((wsQty / 220),2).
        END.
        /*modify for Gwanda
        DO X = 1 TO 5:
             IF wsQty > 0 THEN
                ASSIGN wsTear[X] = dbtear[X] WHEN wsQty >= dbtear[X]
                       wsTear[X] = wsQty WHEN wsQty < dbtear[X]
                       wsQty     = wsQty - wsTear[X]
                       wsCharge  = wsCharge + (wsTear[X] * dbtmf.tCharge[X]).
        END.*/
          wsCharge = wsCharge + dbtmf.Charge. /* add basic charge */
          wsvat    = ROUND(( wscharge * (dbtmf.Vat% / 100 )),2).
          varDescrip = "Water - Basic Charge".  /*for Gwanda*/
          IF wsCharge = ? THEN
                wsCharge = 0.
          IF wsCharge > 0 THEN
             RUN writ-ip.
          ASSIGN dbmmf.wNextP = dbmmf.wNextP + 1 WHEN INT(SUBSTR(STRING(dbmmf.wNextP),5,2)) < 12
                 dbmmf.wNextP = INT(STRING(INT(SUBSTR(STRING(dbmmf.wNextP),1,4)) + 1)+ "01") 
                                                      WHEN INT(SUBSTR(STRING(dbmmf.wNextP),5,2)) = 12.
         /* Land Sales & Lease here ??? */
          RELEASE dbcmf.
          RELEASE dbmmf.
          RELEASE dbgl.
   END.
END.
{glcon1.i}
{glcon.i}
END. /*.. EOP */

PROCEDURE qty.ip:
    IF dbmmf.RDate[13] <> ? THEN DO:
         wsQty =  dbmmf.READ[13] - dbmmf.READ[12].
    END. 
    ELSE IF dbmmf.RDate[13] = ? AND dbmmf.READ[13] = 0 THEN DO:
            ASSIGN wsQty = 0.
                   X = 13.
                   wsNo = 0.
             DO WHILE X >=  2:
                  ASSIGN wsQty = wsQty + dbmmf.consum[X] /*(dbmmf.READ[X] - dbmmf.READ[X - 1]) */ 
                                  WHEN dbmmf.consum[X] > 0 /*(dbmmf.READ[X] - dbmmf.READ[X - 1]) > 0 */
                         wsNo = wsNo + 1 WHEN dbmmf.consum[X] > 0 /*(dbmmf.READ[X] - dbmmf.READ[X - 1]) > 0 */
                         X = X - 1.
             END.
             ASSIGN wsQty = wsQty / wsNo WHEN wsno <> 0
                    wsQty = 0            WHEN wsno = 0.
             IF dbmmf.TYPE = 1 THEN DO: /* Gallon metered meters */
                wsQty = ROUND((wsQty / 220),2).
             END.
       END.
END PROCEDURE.

PROCEDURE Writ-ip:
    ASSIGN wsPrAmt = 0
           wsExAmt = 0.
    dbcmf.AccBal = dbcmf.AccBal + wscharge + wsvat.
            /* update Balance record */
    FIND FIRST  dbblf WHERE dbblf.dbacc = dbcmf.dbacc 
                        AND dbblf.Sgrp  = dbtmf.Sgrp NO-ERROR.
    IF NOT AVAILABLE dbblf THEN DO:
           CREATE dbblf.
           ASSIGN dbblf.dbacc =  dbcmf.dbacc
                  dbblf.Sgrp  =  dbtmf.Sgrp.
    END.
   /* ELSE IF AVAILABLE dbblf THEN */
    ASSIGN wsPrAmt = dbblf.dbamt[2] + ((wscharge + wsvat) * wsRate) WHEN wsCharge <> 0.00
           wsPrAmt = dbblf.dbamt[2]                                 WHEN wsCharge  = 0.00
           dbblf.amt[1] = dbblf.amt[1] + (wscharge + wsvat)         WHEN wsCharge <> 0.00
           dbblf.vat    = dbblf.vat + wsvat
           dbblf.dbamt[1] = dbblf.dbamt[1] + (wscharge + wsvat)     WHEN wsCharge <> 0.00
           dbblf.dbamt[2] = dbblf.dbamt[2] + ((wscharge + wsvat) * wsRate)     WHEN wsCharge <> 0.00 . 
    IF wsCharge <> 0.00 THEN DO:
       CREATE dbmtf.
       ASSIGN dbmtf.Accper  = wsPer
              dbmtf.Amt     = wsCharge + wsVat /*?*/
              dbmtf.Vat     = wsVat
              dbmtf.dbacc   = dbcmf.dbacc
              dbmtf.Sgrp    = dbtmf.Sgrp
              dbmtf.TYPE    = dbtmf.TYPE
              dbmtf.Tarif   = dbtmf.tarif
              dbmtf.prog    = "dbsg07.p"
              dbmtf.ref     = wsRef
              dbmtf.trDate  = wsDate
              dbmtf.DESCRIP = varDescrip
              dbmtf.txtCur  = simctr.dbCur
              dbmtf.decRate = wsRate.
                /* History record */
       IF simctr.LForm  = 2 THEN DO:
           FIND FIRST gldept WHERE gldept.sub-prog = dbtmf.dept NO-LOCK NO-ERROR.
           IF AVAILABLE gldept THEN
               ASSIGN varDept = dbtmf.dept
                      varProj = dbtmf.Proj
                      varFund = gldept.fund.
           ELSE IF NOT AVAILABLE gldept THEN
                ASSIGN varDept = 0
                       varProj = 0
                       varFund = 0.
       END.
       ELSE IF simctr.LForm  = 1 THEN DO:
            FIND FIRST glmf WHERE glmf.acct =  dbtmf.Iledger NO-LOCK NO-ERROR.
            IF AVAILABLE glmf THEN
                ASSIGN varDept = glmf.dept
                       varProj = glmf.proj
                       varFund = glmf.fund.
            ELSE IF NOT AVAILABLE glmf THEN
                ASSIGN varDept = 0
                       varProj = 0
                       varFund = 0.
        END.
        
       CREATE dbhtf.
       ASSIGN dbhtf.Accper  = wsPer
              dbhtf.dbacc   = dbcmf.dbacc
              dbhtf.Amt     = wsCharge + wsVat /*?*/
              dbhtf.Vat     = wsVat
              dbhtf.Iledger = dbtmf.Iledger
              dbhtf.TYPE    = dbtmf.TYPE
              dbhtf.proj    = varProj
              dbhtf.dept    = varDept
              dbhtf.fund    = varFund
              dbhtf.ref     = wsRef
              dbhtf.Sgrp    = dbtmf.Sgrp
              dbhtf.Tarif   = dbtmf.tarif
              dbhtf.trDate  = wsDate
              dbhtf.PoDate  = TODAY
              dbhtf.prog    = "dbsg07.p"
              dbhtf.TransID = wsTransId  
              dbhtf.uid     = varUser
              dbhtf.DESCRIP = varDescrip
              dbhtf.txtCur  = simctr.dbCur
              dbhtf.decRate = wsRate. 
                /* create record for rates income Ledger transactions */
       IF dbtmf.TYPE = 2 THEN DO:
           IF  dbtmf.sIledger <> 0 AND ROUND((ROUND((dbcmf.SiteValue * dbtmf.LValue),2) * -1 * wsRate),2) <> 0 THEN DO:
               FIND FIRST dbgl WHERE dbgl.acct = dbtmf.sIledger AND dbgl.period = wsper
                           AND dbgl.TransID = wsTransId AND dbgl.proj = varproj
                           AND dbgl.dept = vardept AND dbgl.fund =  varfund 
                           AND dbgl.SOURCE   = wsSource NO-ERROR.
                IF NOT AVAILABLE dbgl THEN DO:
                       CREATE dbgl.
                       ASSIGN dbgl.acct     = dbtmf.sIledger
                              dbgl.proj     = varproj
                              dbgl.dept     = vardept
                              dbgl.fund     = varfund
                              dbgl.period   = wsper
                              dbgl.TransID  = wsTransId
                              dbgl.trDATE   = wsDate
                              dbgl.UID      = varUser
                              dbgl.REF      = "dbsg07.p"
                              dbgl.DESCRIP  = "Online Debit Raising"
                              dbgl.CREDATE  = TODAY
                              dbgl.SOURCE   = wsSource.
                END.
                ASSIGN dbgl.AMT = dbgl.AMT + ROUND((ROUND((dbcmf.SiteValue * dbtmf.LValue),2) * -1 * wsRate),2).
           END.
           
           IF dbtmf.viledger <> 0 AND ROUND((ROUND((dbcmf.BldValue * dbtmf.BValue),2) * -1 * wsRate),2) <> 0 THEN DO:
              FIND FIRST dbgl WHERE dbgl.acct = dbtmf.viledger AND dbgl.period = wsper
                           AND dbgl.TransID = wsTransId AND dbgl.proj = varproj
                           AND dbgl.dept = vardept AND dbgl.fund =  varfund 
                           AND dbgl.SOURCE   = wsSource NO-ERROR.
                IF NOT AVAILABLE dbgl THEN DO:
                       CREATE dbgl.
                       ASSIGN dbgl.acct     = dbtmf.vIledger
                              dbgl.proj     = varproj
                              dbgl.dept     = vardept
                              dbgl.fund     = varfund
                              dbgl.period   = wsper
                              dbgl.TransID  = wsTransId
                              dbgl.trDATE   = wsDate
                              dbgl.UID      = varUser
                              dbgl.REF      = "dbsg07.p"
                              dbgl.DESCRIP  = "Online Debit Raising"
                              dbgl.CREDATE  = TODAY
                              dbgl.SOURCE   = wsSource.
                END.
                ASSIGN dbgl.AMT = dbgl.AMT + ROUND((ROUND((dbcmf.BldValue * dbtmf.BValue),2) * -1 * wsRate),2).
           END.
       END.
       /* create record for non-rates income Ledger transaction */
        ELSE DO:
            FIND FIRST dbgl WHERE dbgl.acct = dbtmf.Iledger AND dbgl.period = wsper
                           AND dbgl.TransID = wsTransId AND dbgl.proj = varproj
                           AND dbgl.dept = vardept AND dbgl.fund =  varfund 
                           AND dbgl.SOURCE   = wsSource NO-ERROR.
            IF NOT AVAILABLE dbgl THEN DO:
                   CREATE dbgl.
                   ASSIGN dbgl.acct     = dbtmf.Iledger
                          dbgl.proj     = varproj
                          dbgl.dept     = vardept
                          dbgl.fund     = varfund
                          dbgl.period   = wsper
                          dbgl.TransID  = wsTransId
                          dbgl.trDATE   = wsDate
                          dbgl.UID      = varUser
                          dbgl.REF      = "dbsg07.p"
                          dbgl.DESCRIP  = "Online Debit Raising"
                          dbgl.CREDATE  = TODAY
                          dbgl.SOURCE   = wsSource.
            END.
            ASSIGN dbgl.AMT = dbgl.AMT + ROUND((wscharge * -1 * wsRate),2).
        END.
                /* Create VAT Provision Ledger  Record */
        IF wsVat <> 0 THEN DO:
            FIND FIRST dbgl WHERE dbgl.period = wsper  AND dbgl.TransID = wsTransId
                             AND dbgl.acct = vatLedger AND dbgl.proj = varproj
                             AND dbgl.dept = vardept AND dbgl.fund =  varfund
                             AND dbgl.SOURCE   = wsSource NO-ERROR.
            IF NOT AVAILABLE dbgl THEN DO:
               CREATE dbgl.
               ASSIGN dbgl.acct     = vatLedger
                      dbgl.proj     = varproj
                      dbgl.dept     = vardept
                      dbgl.fund     = varfund
                      dbgl.period   = wsper
                      dbgl.TransID  = wsTransId
                      dbgl.trDATE   = wsDate
                      dbgl.UID      = varUser
                      dbgl.REF      = "dbsg07.p"
                      dbgl.DESCRIP  = "Online Debit Raising"
                      dbgl.CREDATE  = TODAY
                      dbgl.SOURCE   = wsSource.
            END.
            ASSIGN dbgl.AMT = dbgl.AMT + ROUND((wsVat * -1 * wsRate),2).
        END.
        
                    /* Create record for the Control Ledger record */
        IF simctr.LForm  = 2 THEN DO:
            FIND FIRST dbsgr WHERE dbsgr.Sgrp = dbtmf.Sgrp NO-LOCK NO-ERROR.
            FIND FIRST gldept WHERE gldept.sub-prog = dbsgr.dept NO-LOCK NO-ERROR.
            IF AVAILABLE gldept THEN
                ASSIGN varDept = dbsgr.dept
                       varProj = dbsgr.proj
                       varFund = gldept.fund.
            ELSE IF NOT AVAILABLE gldept THEN
                ASSIGN varDept = 0
                       varProj = 0
                       varFund = 0.
        END.
        ELSE IF simctr.LForm  = 1 THEN DO:
            FIND FIRST dbsgr WHERE dbsgr.Sgrp = dbtmf.Sgrp NO-LOCK NO-ERROR.
            FIND FIRST glmf WHERE glmf.acct =  dbsgr.ctrLedger NO-LOCK NO-ERROR.
            IF AVAILABLE glmf THEN
                ASSIGN varDept = glmf.dept
                       varProj = glmf.proj
                       varFund = glmf.fund.
            ELSE IF NOT AVAILABLE glmf THEN
                ASSIGN varDept = 0
                       varProj = 0
                       varFund = 0.
        END.
        
        FIND FIRST dbgl WHERE dbgl.acct = dbsgr.ctrLedger AND dbgl.period = wsper
                          AND dbgl.TransID = wsTransId AND dbgl.dept = vardept
                          AND dbgl.proj = varproj AND dbgl.fund = varfund
                          AND dbgl.SOURCE   = wsSource  NO-ERROR.
        IF NOT AVAILABLE dbgl THEN DO:
               CREATE dbgl.
               ASSIGN dbgl.acct     = dbsgr.ctrLedger
                      dbgl.proj     = varproj
                      dbgl.dept     = vardept
                      dbgl.fund     = varfund
                      dbgl.period   = wsper
                      dbgl.TransID  = wsTransId
                      dbgl.trDATE   = wsDate
                      dbgl.UID      = varUser
                      dbgl.REF      = "dbsg07.p"
                      dbgl.DESCRIP  = "Online Debit Raising"
                      dbgl.CREDATE  = TODAY
                      dbgl.SOURCE   = wsSource.
        END.
        ASSIGN dbgl.AMT = dbgl.AMT + ROUND((wscharge * wsRate),2) + ROUND(( wsVat * wsRate),2).
        ASSIGN wsTotal = wsTotal + wsCharge
               wsTVat  = wsTVat  + wsVat.
    END.
    RETURN.
END PROCEDURE
 /*MESSAGE ETIME VIEW-AS ALERT-BOX.*/

    /*   
       IF dbmmf.RDate[13] = ? OR dbmmf.READ[13] = ? THEN DO:
            ASSIGN wsQty = 0.
                   X = 13.
                   wsNo = 0.
             DO WHILE X >=  2:
                  ASSIGN wsQty = wsQty + dbmmf.consum[X] /*(dbmmf.READ[X] - dbmmf.READ[X - 1]) */ 
                                  WHEN dbmmf.consum[X] > 0 /*(dbmmf.READ[X] - dbmmf.READ[X - 1]) > 0 */
                         wsNo = wsNo + 1 WHEN dbmmf.consum[X] > 0 /*(dbmmf.READ[X] - dbmmf.READ[X - 1]) > 0 */
                         X = X - 1.
             END.
             wsQty = wsQty / wsNo.
             varDescrip = "Average Water " + "(" + string(wsQty) + ")".
             ASSIGN dbmmf.READ[13]   = dbmmf.READ[12] + wsQty
                    dbmmf.RDate[13]  = TODAY
                    dbmmf.consum[13] = wsQty
                    dbmmf.comm[13]   = 96. /* avg */
             DO X = 1 TO 12:
                ASSIGN dbmmf.READ[X]   = dbmmf.READ[X + 1]
                       dbmmf.RDATE[X]  = dbmmf.RDATE[X + 1]
                       dbmmf.consum[x] = dbmmf.consum[X + 1].
                       /*dbmmf.comm[x]   = dbmmf.comm[X + 1]. */
            END.
            ASSIGN dbmmf.READ[13]   = 0
                   dbmmf.RDate[13]  = ?
                   dbmmf.consum[13] = 0
                   dbmmf.comm[13]   = 0.
        END.
        ELSE IF dbmmf.Read[13]  <> ? THEN DO:
            ASSIGN wsQty = dbmmf.consum[13]
                   dbmmf.comm[13]   = 99  WHEN dbmmf.READ[13] < dbmmf.READ[12].
            IF wsqty = ? THEN
                wsqty = 0.
            varDescrip = "Water " + "(" + string(wsQty) + ")".
            DO X = 1 TO 12:
                assign dbmmf.READ[X]   = dbmmf.READ[X + 1]
                       dbmmf.RDATE[X]  = dbmmf.RDATE[X + 1]
                       dbmmf.consum[X] = dbmmf.consum[X + 1].
            END.
            ASSIGN dbmmf.READ[13]   = 0
                   dbmmf.RDate[13]  = ?
                   dbmmf.consum[13] = 0.
        END.  */
