/* Program.................dbsg67.p
   Notes:...... Debit raising module
   Author:.................S. Mawire
*/
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "landscape"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10          NO-UNDO.
DEF VAR varUser    AS CHAR FORM "xxx" INITIAL "01".
DEF VAR wsTransId AS DEC FORM "99999999999999".
DEF VAR wsTime    AS CHAR FORM "x(8)".
DEF VAR wsPer     AS INT FORM "999999".
DEF VAR wsCharge  LIKE dbcmf.accbal.
DEF VAR wsTear    LIKE dbtmf.Tear.
DEF VAR wsVat     LIKE wsCharge.
DEF VAR wsTotal   LIKE wsCharge.
DEF VAR wsTVat    LIKE wsCharge.
DEF VAR wsDr      LIKE wsCharge.
DEF VAR wsCr      LIKE wsCharge.
DEF VAR wsTDr     LIKE wsCharge.
DEF VAR wsTCr     LIKE wsCharge.
DEF VAR wsQty     AS INT.
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
DEF VAR X AS INT.

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
    with view-as dialog-box keep-tab-order no-validate
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

FORM 
     dbgl.acct           LABEL "ACCOUNT"
     glmf.DESCRIPTION   LABEL "DESCRIPTION"
     wsDr               LABEL "DEBIT"
     wsCr               LABEL "CREDIT"
     HEADER skip(1) "                       DEBIT RAISING CONSOLIDATION REPORT FOR THE PERIOD:" AT 10 SPACE(2)
    STRING(wsPer)
    "Page: " AT 110 PAGE-NUMBER(a) SKIP(1)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-lrpt.

ON CHOOSE OF btn-oK IN FRAME frm-main DO:
     session:set-wait-state("").
     ASSIGN vatLedger = simctr.vat[1]
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
        {PrintOpt.i &stream-name="stream a"
                    &print-prog="Bill.ip"
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
       wsEnd:SCREEN-VALUE = "9999999999999".
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.


PROCEDURE Bill.ip:
DO TRANSACTION ON ERROR UNDO, LEAVE:
FOR EACH dbcmf WHERE dbcmf.dbacc >= wsStart AND dbcmf.dbacc <= wsEnd AND dbcmf.AccStat = 0:
    wsStatus = "Process......" + STRING(dbcmf.dbacc).
    DISPLAY wsStatus WITH FRAME frm-Main.
    FOR EACH dbtat WHERE dbtat.dbacc = dbcmf.dbacc SHARE-LOCK:
         IF dbtat.NextP <> wsPer THEN NEXT.
        FIND FIRST dbtmf WHERE dbtmf.Tarif = dbtat.tarif AND dbtat.TYPE = dbtmf.TYPE NO-LOCK NO-ERROR.
        IF NOT AVAILABLE dbtmf THEN DO:
            MESSAGE "No master tarif record found for " dbtat.tarif
                VIEW-AS ALERT-BOX.
            NEXT.
        END.
        ELSE DO:
            CASE dbtmf.TYPE:
                WHEN 2 THEN DO:
                    /* Rates Charges */
                    IF dbtmf.freq = "A" THEN
                        wsFreq = 1.
                    ELSE IF dbtmf.freq = "M" THEN
                        wsFreq = 12.
                    ELSE IF dbtmf.freq = "Q" THEN
                        wsFreq = 4.
                    ASSIGN wscharge = ROUND((dbcmf.BldValue * dbtmf.BValue)/ wsFreq ,2) 
                                    + ROUND((dbcmf.SiteValue * dbtmf.LValue)/ wsFreq ,2)
                           wsvat    = ROUND(( wscharge * (dbtmf.Vat% / 100 )) ,2).
                END.
                WHEN 3 THEN DO:
                    ASSIGN wscharge = ROUND((dbtat.Units * dbtmf.Charge) ,2)
                           wsvat    = ROUND(( wscharge * (dbtmf.Vat% / 100 )) ,2).
                END.  
            END CASE.
            varDescrip = dbtmf.Descrip.
            RUN writ-ip.
            IF dbtmf.freq = "M" THEN
                ASSIGN  dbtat.NextP = dbtat.NextP + 1 WHEN INT(SUBSTR(STRING(dbtat.NextP),5,2)) < 12
                        dbtat.NextP = INT(STRING(INT(SUBSTR(STRING(dbtat.NextP),1,4)) + 1)+ "01") 
                                                     WHEN INT(SUBSTR(STRING(dbtat.NextP),5,2)) = 12.
            ELSE IF dbtmf.freq = "A" THEN
                    ASSIGN dbtat.NextP = INT(STRING(INT(SUBSTR(STRING(dbtat.NextP),1,4)) + 1) 
                                               + SUBSTR(STRING(dbtat.NextP),5,2)).
             ELSE IF dbtmf.freq = "Q" THEN DO:
                 IF (INT(SUBSTR(STRING(dbtat.NextP),5,2)) + 3) - 12 <= 0 THEN
                     ASSIGN wsm = (INT(SUBSTR(STRING(dbtat.NextP),5,2)) + 3)
                            dbtat.NextP = dbtat.NextP + wsm.
                 ELSE
                     ASSIGN wsm = (INT(SUBSTR(STRING(dbtat.NextP),5,2)) + 3) - 12
                            dbtat.NextP = INT(STRING(INT(SUBSTR(STRING(dbtat.NextP),1,4)) + 1)  + STRING(wsm, "99")).
             END.
    END. /* eof each dbtat */                              
END. /* eof dbcmf */
END. /* Transaction Block */
/* water charges */
FOR EACH dbmmf WHERE dbmmf.tarif <> 0 AND dbmmf.mStat = 1:
    FIND FIRST dbcmf WHERE dbcmf.dbacc = dbmmf.dbacc EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST dbtmf WHERE dbtmf.tarif = dbmmf.tarif AND dbtmf.TYPE = 1 NO-LOCK NO-ERROR.
    IF dbmmf.RDate[13] <> ? THEN DO:
        wsQty = dbmmf.consum[13] /* dbmmf.READ[13] - dbmmf.READ[12] */.
        varDescrip = "Water " + "(" + string(wsQty) + ")".
        DO X = 1 TO 12:
            assign dbmmf.READ[X]   = dbmmf.READ[X + 1]
                   dbmmf.RDATE[X]  = dbmmf.RDATE[X + 1]
                   dbmmf.consum[X] = dbmmf.consum[X + 1].
        END.
        ASSIGN dbmmf.READ[13]   = 0
               dbmmf.RDate[13]  = ?
               dbmmf.consum[13] = 0.
    END. 
    ELSE IF dbmmf.RDate[13] = ? THEN DO:
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
                dbmmf.comm[13]   = 99.
         DO X = 1 TO 12:
            ASSIGN dbmmf.READ[X]   = dbmmf.READ[X + 1]
                   dbmmf.RDATE[X]  = dbmmf.RDATE[X + 1]
                   dbmmf.consum[x] = dbmmf.consum[X + 1]
                   dbmmf.comm[x]   = dbmmf.comm[X + 1].
        END.
        ASSIGN dbmmf.READ[13]   = 0
               dbmmf.RDate[13]  = ?
               dbmmf.consum[13] = 0
               dbmmf.comm[13]   = 0.
    END.
    ASSIGN  wsTear = 0
            wsCharge = 0.
    DO X = 1 TO 5:
         IF wsQty > 0 THEN
            ASSIGN wsTear[X] = dbtmf.tear[X] WHEN wsQty >= dbtmf.tear[X]
                   wsTear[X] = wsQty WHEN wsQty < dbtmf.tear[X]
                   wsQty     = wsQty - wsTear[X]
                   wsCharge  = wsCharge + (wsTear[X] * dbtmf.tCharge[X]).
    END.
      wsCharge = wsCharge + dbtmf.Charge. /* add basic charge */
      wsvat    = ROUND(( wscharge * (dbtmf.Vat% / 100 )) ,2).
      RUN writ-ip.
      RELEASE dbcmf.
END.
END.
UNDERLIN STREAM a dbcmf.dbAcc dbcmf.NAME dbtmf.Descrip wsCharge wsVat WITH FRAME frm-rpt.
DOWN STREAM a WITH FRAME frm-rpt.
DISPLAY STREAM a "TOTAL" @ dbcmf.NAME wsTotal @ wsCharge wsTVat @ wsVat WITH FRAME frm-rpt.
DOWN STREAM a WITH FRAME frm-rpt.

/* Consolidate to GL and Report */
DO TRANSACTION ON ERROR UNDO, LEAVE:
    wsStatus = "Consolidating to General Ledger".
    DISPLAY wsStatus WITH FRAME frm-Main.
FOR EACH dbgl:
    CREATE gltdf.
    BUFFER-COPY dbgl TO gltdf.
    ASSIGN gltdf.SOURCE = "DB".
    FIND FIRST glbal WHERE GLBAL.acct = dbgl.acct 
                        AND GLBAL.YEAR = INT(SUBSTR(STRING(dbgl.period),1,4)) NO-ERROR.
    IF NOT AVAILABLE glbal THEN DO:
        CREATE glbal.
        ASSIGN glbal.acct = dbgl.acct
               glbal.YEAR = INT(SUBSTR(STRING(dbgl.period),1,4)).
    END.
    ASSIGN glbal.amt[INT(SUBSTR(STRING(dbgl.period),5,2))]=
                   glbal.amt[INT(SUBSTR(STRING(dbgl.period),5,2))] + dbgl.amt.
    IF  dbgl.amt < 0 THEN
        ASSIGN wsDr = 0
               wsCr = dbgl.amt
               wsTCr = wsTCr + dbgl.amt.
    ELSE
        ASSIGN wsCr = 0
               wsDr = dbgl.amt
               wsTDr = wsTDr + dbgl.amt.
    FIND FIRST glmf WHERE glmf.acct = dbgl.acct NO-LOCK NO-ERROR.
    DISPLAY  STREAM a dbgl.acct glmf.DESCRIPTION wsDr wsCr WITH FRAME frm-lrpt.
    DOWN STREAM a WITH FRAME frm-lrpt.
    DELETE dbgl.
END.
UNDERLINE STREAM a dbgl.acct glmf.DESCRIPTION wsDr wsCr WITH FRAME frm-lrpt.
DISPLAY  STREAM a "TOTAL " @ glmf.DESCRIPTION wsTDr @ wsDr wsTCr @ wsCr WITH FRAME frm-lrpt.
DOWN STREAM a WITH FRAME frm-lrpt.
END.
END. /*.. EOP */

PROCEDURE Writ-ip:
    dbcmf.AccBal = dbcmf.AccBal + wscharge + wsvat.
            /* update Balance record */
    FIND FIRST  dbblf WHERE dbblf.dbacc = INT(dbcmf.dbacc) 
                      AND dbblf.Sgrp = dbtmf.Sgrp NO-ERROR.
    IF NOT AVAILABLE dbblf THEN DO:
           CREATE dbblf.
           ASSIGN dbblf.dbacc =  dbcmf.dbacc
                  dbblf.Sgrp  =  dbtmf.Sgrp.
    END.
    ASSIGN dbblf.amt[1] = dbblf.amt[1] + wscharge + wsvat WHEN wsCharge <> 0.00.
            /* create monthly transaction */
    IF wsCharge <> 0.00 THEN DO:
       CREATE dbmtf.
       ASSIGN dbmtf.Accper  = wsPer
              dbmtf.Amt     = wsCharge + wsVat /*?*/
              dbmtf.Vat     = wsVat
              dbmtf.dbacc   = dbcmf.dbacc
              dbmtf.Sgrp    = dbtmf.Sgrp
              dbmtf.Tarif   = dbtmf.tarif
              dbmtf.trDate  = wsDate
              dbmtf.DESCRIP = varDescrip.
                /* History record */
       CREATE dbhtf.
       ASSIGN dbhtf.Accper  = wsPer
              dbhtf.dbacc   = dbcmf.dbacc
              dbhtf.Amt     = wsCharge + wsVat /*?*/
              dbhtf.Vat     = wsVat
              dbhtf.Iledger = dbtmf.Iledger
              dbhtf.ref     = "Debit" + string(wsPer)
              dbhtf.Sgrp    = dbtmf.Sgrp
              dbhtf.Tarif   = dbtmf.tarif
              dbhtf.trDate  = wsDate
              dbhtf.PoDate  = TODAY
              dbhtf.prog    = "dbsg67.p"
              dbhtf.TransID = wsTransId  
              dbhtf.uid     = varUser
              dbhtf.DESCRIP = varDescrip. 
                /* create record for income Ledger transaction */
         FIND FIRST dbgl WHERE dbgl.acct = dbtmf.Iledger AND dbgl.period = wsper
                           AND dbgl.TransID = wsTransId no-error.
        IF NOT AVAILABLE dbgl THEN DO:
               CREATE dbgl.
               ASSIGN dbgl.acct = dbtmf.Iledger
                      dbgl.period = wsper
                      dbgl.TransID = wsTransId
                      dbgl.trDATE  = wsDate
                      dbgl.UID     = varUser
                      dbgl.REF     = "dbsg67.p"
                      dbgl.DESCRIP = "Online Debit Raising"
                     dbgl.CREDATE = TODAY.
        END.
        ASSIGN dbgl.AMT = dbgl.AMT + (wscharge * -1). 
                /* Create VAT Provision Ledger  Record */
        FIND FIRST dbgl WHERE dbgl.period = wsper  AND dbgl.TransID = wsTransId AND 
                          dbgl.acct = INT(SUBSTR(STRING(dbtmf.Iledger),1,1) + "00" + STRING(vatLedger))    
                          no-error.
        IF NOT AVAILABLE dbgl THEN DO:
               CREATE dbgl.
               ASSIGN dbgl.acct = INT(SUBSTR(STRING(dbtmf.Iledger),1,1) + "00" + STRING(vatLedger))
                      dbgl.period = wsper
                      dbgl.TransID = wsTransId
                      dbgl.trDATE  = wsDate
                      dbgl.UID     = varUser
                      dbgl.REF     = "dbsg67.p"
                      dbgl.DESCRIP = "Online Debit Raising"
                      dbgl.CREDATE = TODAY.
        END.
        ASSIGN dbgl.AMT = dbgl.AMT + (wsVat * -1).
                    /* Create record for the Control Ledger record */
        FIND FIRST dbsgr WHERE dbsgr.Sgrp = dbtmf.Sgrp NO-LOCK NO-ERROR.
        FIND FIRST dbgl WHERE dbgl.acct = dbsgr.ctrLedger AND dbgl.period = wsper
                          AND dbgl.TransID = wsTransId no-error.
        IF NOT AVAILABLE dbgl THEN DO:
               CREATE dbgl.
               ASSIGN dbgl.acct = dbsgr.ctrLedger
                      dbgl.period = wsper
                      dbgl.TransID = wsTransId
                      dbgl.trDATE  = wsDate
                      dbgl.UID     = varUser
                      dbgl.REF     = "dbsg67.p"
                      dbgl.DESCRIP = "Online Debit Raising"
                     dbgl.CREDATE = TODAY.
        END.
        ASSIGN dbgl.AMT = dbgl.AMT + wscharge + wsVat.
        DISPLAY STREAM a dbcmf.dbAcc dbcmf.NAME dbtmf.Descrip
                     wsCharge wsVat WITH FRAME frm-rpt.
        DOWN STREAM a WITH FRAME frm-rpt.
        ASSIGN wsTotal = wsTotal + wsCharge
        wsTVat  = wsTVat  + wsVat.
    END.
END.
