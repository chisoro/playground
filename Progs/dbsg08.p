/* Program.................dbsg08.p
   Notes:...... Land Sale Instalment/Lease debit raising module
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
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
DEF VAR wsTear    LIKE dbtmf.Tear.
DEF VAR wsVat     LIKE wsCharge.
DEF VAR wsTotal   LIKE wsCharge.
DEF VAR wsTVat    LIKE wsCharge.
DEF VAR wsDr      LIKE wsCharge.
DEF VAR wsCr      LIKE wsCharge.
DEF VAR wsTDr     LIKE wsCharge.
DEF VAR wsTCr     LIKE wsCharge.
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
DEF VAR wsq       AS DEC EXTENT 5.
DEF VAR X AS INT.
DEF VAR t AS INT.
DEF VAR wsBt      LIKE dbsgr.sgrp.
DEF VAR wsLedger  LIKE glmf.acct.
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
    TITLE "LAND SALES/LEASE INSTALMENT DEBIT RAISING".

FORM 
     dbcmf.dbAcc     LABEL "ACCOUNT"
     dbcmf.NAME      LABEL "DESCRIPTION"
     dbhtf.ref       LABEL "REFERENCE"
     dbhtf.Descrip   LABEL "TARIFF" FORM "x(20)"
     wsCharge        LABEL "AMOUNT"
     wsVat           LABEL "VAT"
    HEADER skip(1) "                  LAND SALES/LEASE INSTALMENT DEBIT RAISING REPORT FOR THE PERIOD:" AT 10 SPACE(2)
    STRING(wsPer)
    "Page: " AT 110 PAGE-NUMBER(a) SKIP(1) 
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-rpt-II.

FORM 
     dbcmf.dbAcc        LABEL "ACCOUNT"
     dbcmf.NAME         LABEL "DESCRIPTION"
     dbhtf.ref          LABEL "REFERENCE"
     landsale.Instal[4] LABEL "INSTALMENT"
     landsale.BAmt[4]   LABEL "BALANCE"
     landsale.Per[4]    LABEL "PERIODS"
    HEADER skip(1) "                  LAND SALES INSTALMENT DEBIT RAISING REPORT FOR THE PERIOD:" AT 10 SPACE(2)
    STRING(wsPer)
    "Page: " AT 110 PAGE-NUMBER(a) SKIP(1) 
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-rpt.

FORM dbgl.Fund             LABEL "SEGMENT"
     dbgl.Proj          LABEL "PROJECT"
     dbgl.dept          LABEL "DEPT"
     dbgl.acct          LABEL "ACCOUNT"
     glmf.DESCRIPTION   LABEL "DESCRIPTION"
     wsDr               LABEL "DEBIT"
     wsCr               LABEL "CREDIT"
     HEADER skip(1) "                      LAND SALES/LEASE INSTALMENT DEBIT RAISING CONSOLIDATION REPORT FOR THE PERIOD:" AT 10 SPACE(2)
    STRING(wsPer)
    "Page: " AT 110 PAGE-NUMBER(a) SKIP(1)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-lrpt.

ON CHOOSE OF btn-oK IN FRAME frm-main DO:
     session:set-wait-state("").
     ASSIGN vatLedger = simctr.vat[1]
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
        a = ETIME(YES).
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
ASSIGN wsPer:SCREEN-VALUE = STRING(SIMCTR.CURPER)
       wsDate:SCREEN-VALUE = STRING(TODAY)
       wsStart:SCREEN-VALUE = "0"
       wsEnd:SCREEN-VALUE = "9999999999999".
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.

PROCEDURE Bill.ip:
FOR EACH LProj NO-LOCK BY LProj.scheme :
    FOR EACH landsale WHERE landsale.scheme = LProj.scheme AND landsale.stat = 0 
        AND (landsale.BAmt[4] <> 0 OR landsale.Amt[1] <> 0) :
        wsCharge = 0.
        wsVat    = 0.
        FIND FIRST dbcmf WHERE dbcmf.dbacc = landsale.dbAcc NO-ERROR.
        wsStatus = "Process Rates and Services......" + STRING(dbcmf.dbacc).
        DISPLAY wsStatus WITH FRAME frm-Main.
        IF landsale.Billsep = NO AND LandSale.ls = 2 THEN DO:
           IF  landsale.NextP[4] <= wsPer THEN
                ASSIGN wsCharge = landsale.instal[4]
                       wsbt     = LProj.svgrp[1]
                       wsLedger = LProj.iLedger[4]
                       varDescrip = "Instalment"
                       wsVat      = ROUND((landsale.instal[1] * LProj.Vat%[1]) / (LProj.Vat%[1] + 100)
                                  + (landsale.instal[2] * LProj.Vat%[2]) / (LProj.Vat%[2] + 100)
                                  + (landsale.instal[3] * LProj.Vat%[3]) / (LProj.Vat%[3] + 100),2).
           IF wsCharge > 0 THEN RUN writ.ip.
           ASSIGN BAmt[4]   = BAmt[1] + BAmt[2] + BAmt[3]
                  /*Instal[4] = Instal[1] + Instal[2]+ Instal[3] */.
           IF BAmt[4] = 0 THEN
                landsale.stat = 99. /* close instalment */
        END.
        ELSE IF landsale.Billsep = YES AND LandSale.ls = 2 THEN DO:
            DO X = 1 TO 3:
                IF  landsale.NextP[X] <= wsPer THEN
                    ASSIGN wsCharge = landsale.instal[X]
                           wsbt     = LProj.svgrp[X]
                           wsLedger = LProj.iLedger[X] WHEN X <> 1
                           wsLedger = LProj.iLedger[4] WHEN X = 1
                           varDescrip = "Land Instalment"  WHEN LandSale.ls = 2
                           varDescrip = "Lease/Rental"  WHEN LandSale.ls = 1
                           varDescrip = "Survey Cost"  WHEN X = 2
                           varDescrip = "Admin/Renewal Cost"  WHEN X = 3
                           wsVat      = ROUND((wsCharge * (LProj.Vat%[X]) / (LProj.Vat%[X] + 100)),2).
               IF wsCharge > 0 THEN 
                   RUN writ.ip.
            END. /* eof DO x */
            ASSIGN BAmt[4]   = BAmt[1] + BAmt[2] + BAmt[3]
                  /*Instal[4] = Instal[1] + Instal[2]+ Instal[3] */.
            IF BAmt[4] = 0 THEN
                landsale.stat = 99. /* close instalment */
        END. /* landsale.Billsep = YES */
        ELSE IF landsale.Billsep = NO AND LandSale.ls = 1 THEN DO:
            IF landsale.NextP[1] <= wsPer THEN
               ASSIGN wsCharge = landsale.Amt[1] /*Rent */
                      wsVat    = ROUND((landsale.Amt[1] * (LProj.Vat%[1]) / (LProj.Vat%[1] + 100)),2)
                      wsLedger = LProj.iLedger[1].
             IF landsale.NextP[3] <= wsPer THEN
               ASSIGN wsCharge = wsCharge + landsale.Amt[3] /* Admin cost */
                      wsVat    = wsVat + ROUND((landsale.Amt[3] * (LProj.Vat%[3]) / (LProj.Vat%[3] + 100)),2).
             RUN writ.ip.
        END.
        ELSE IF landsale.Billsep = YES AND LandSale.ls = 1 THEN DO:
            IF landsale.NextP[1] <= wsPer THEN DO:
                ASSIGN wsCharge = landsale.Amt[1] /*Rent */
                      wsVat    = ROUND((landsale.Amt[1] * (LProj.Vat%[1]) / (LProj.Vat%[1] + 100)),2)
                      wsLedger = LProj.iLedger[1].
                RUN writ.ip.
            END.  
             IF landsale.NextP[3] <= wsPer THEN DO:
                 ASSIGN wsCharge = landsale.Amt[3] /* Admin cost */
                        wsVat    = ROUND((landsale.Amt[3] * (LProj.Vat%[3]) / (LProj.Vat%[3] + 100)),2)
                        wsLedger = LProj.iLedger[3].
             RUN writ.ip.
             END.     
        END.
        DO X = 1 TO 4:
            IF landsale.NextP[X] <= wsPer AND (landsale.BAmt[X] <> 0 OR landsale.Amt[X] <> 0) THEN DO:
                ASSIGN landsale.BAmt[X] = (landsale.BAmt[X] + (landsale.BAmt[X] * (landsale.INT[X] / 1200))) - landsale.Instal[X]
                       landsale.Instal[X] = landsale.BAmt[X] WHEN landsale.Instal[X] > landsale.BAmt[X].
                IF LProj.freq[X] = 1 THEN /*Monthly */
                   ASSIGN  LandSale.NextP[X] = LandSale.NextP[X] + 1 WHEN INT(SUBSTR(STRING(LandSale.NextP[X]),5,2)) < 12
                           LandSale.NextP[X] = INT(STRING(INT(SUBSTR(STRING(LandSale.NextP[X]),1,4)) + 1)+ "01") 
                                                              WHEN INT(SUBSTR(STRING(LandSale.NextP[X]),5,2)) = 12.
                ELSE IF lProj.freq[X] = 3 THEN /*Annual */
                        ASSIGN LandSale.NextP[X] = INT(STRING(INT(SUBSTR(STRING(LandSale.NextP[X]),1,4)) + 1) 
                                                          + SUBSTR(STRING(LandSale.NextP[X]),5,2)).
                ELSE IF LProj.freq[X] = 2 THEN DO: /* Quarterly */
                     IF (INT(SUBSTR(STRING(LandSale.NextP[X]),5,2)) + 3) - 12 <= 0 THEN
                             ASSIGN wsm = (INT(SUBSTR(STRING(LandSale.NextP[X]),5,2)) + 3)
                                   LandSale.NextP[X] = LandSale.NextP[X] + wsm.
                         ELSE
                            ASSIGN wsm = (INT(SUBSTR(STRING(LandSale.NextP[X]),5,2)) + 3) - 12
                                  LandSale.NextP[X] = INT(STRING(INT(SUBSTR(STRING(LandSale.NextP[X]),1,4)) + 1)  + STRING(wsm, "99")).
                END.
            END.
        END.
        IF Lproj.ls = 2 THEN DO:
            DISPLAY STREAM a dbcmf.dbAcc dbcmf.NAME ("L:" + string(landsale.Scheme)) @ dbhtf.ref landsale.Instal[4]
                     landsale.BAmt[4] landsale.per[4] WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
        END.
    END.  /* eof Each Landsale */
END. /*eof Each LProj */
{glcon.i}
END. /*eof Procedure */


PROCEDURE Writ.ip:
    dbcmf.AccBal = dbcmf.AccBal + wscharge.
            /* update Balance record */
    FIND FIRST dbsgr WHERE dbsgr.sgrp = wsbt  NO-LOCK NO-ERROR.
    FIND FIRST glmf WHERE glmf.acct = wsLedger NO-LOCK NO-ERROR.
    FIND FIRST  dbblf WHERE dbblf.dbacc = DEC(dbcmf.dbacc) 
                      AND dbblf.Sgrp = dbsgr.sgrp NO-ERROR.
    IF NOT AVAILABLE dbblf THEN DO:
           CREATE dbblf.
           ASSIGN dbblf.dbacc =  dbcmf.dbacc
                  dbblf.Sgrp  =  dbsgr.sgrp.
    END.
    ASSIGN dbblf.amt[1] = dbblf.amt[1] + wscharge WHEN wsCharge <> 0.00.
            /* create monthly transaction */
    IF wsCharge <> 0.00 THEN DO:
       CREATE dbmtf.
       ASSIGN dbmtf.Accper  = wsPer
              dbmtf.Amt     = wsCharge
              dbmtf.Vat     = wsVat 
              dbmtf.dbacc   = dbcmf.dbacc
              dbmtf.Sgrp    = dbsgr.Sgrp
              dbmtf.Tarif   = 0
              dbmtf.trDate  = wsDate
              dbmtf.DESCRIP = varDescrip.
                /* History record */
       CREATE dbhtf.
       ASSIGN dbhtf.Accper  = wsPer
              dbhtf.dbacc   = dbcmf.dbacc
              dbhtf.Amt     = wsCharge 
              dbhtf.Vat     = wsVat
              dbhtf.Iledger = wsLedger
              dbhtf.proj    = glmf.proj
              dbhtf.dept    = glmf.dept
              dbhtf.fund    = glmf.fund
              dbhtf.ref     = "L:" + string(landsale.Scheme) WHEN LProj.ls = 2
              dbhtf.ref     = "R:" + string(landsale.Scheme) WHEN LProj.ls = 1
              dbhtf.Sgrp    = dbsgr.Sgrp
              dbhtf.Tarif   = 0
              dbhtf.trDate  = wsDate
              dbhtf.PoDate  = TODAY
              dbhtf.prog    = "dbsg08.p"
              dbhtf.TransID = wsTransId  
              dbhtf.uid     = varUser
              dbhtf.DESCRIP = varDescrip. 
                /* create record for income Ledger transaction */
        FIND FIRST dbgl WHERE dbgl.acct = wsLedger AND dbgl.period = wsper
                           AND dbgl.TransID = wsTransId  AND dbgl.proj = glmf.proj
                           AND dbgl.dept = glmf.dept AND dbgl.fund = glmf.fund NO-ERROR.
        IF NOT AVAILABLE dbgl THEN DO:
               CREATE dbgl.
               ASSIGN dbgl.acct     = wsLedger
                      dbgl.proj     = glmf.proj
                      dbgl.dept     = glmf.dept
                      dbgl.fund     = glmf.fund
                      dbgl.period   = wsper
                      dbgl.TransID  = wsTransId
                      dbgl.trDATE   = wsDate
                      dbgl.UID      = varUser
                      dbgl.REF      = "dbsg08.p"
                      dbgl.DESCRIP  = "Instalment Raising"
                      dbgl.CREDATE  = TODAY
                      dbgl.SOURCE   = wsSource.
        END.
        ASSIGN /* dbgl.AMT = dbgl.AMT + ((wscharge) * -1) WHEN LandSale.ls = 1 */
               dbgl.AMT = dbgl.AMT + ((wscharge - wsVat) * -1). 
                /* Create VAT Provision Ledger  Record */
        IF wsVat <> 0 THEN DO:
            FIND FIRST dbgl WHERE dbgl.period = wsper  AND dbgl.TransID = wsTransId 
                                   AND dbgl.acct = vatLedger AND dbgl.dept = glmf.dept 
                                   AND dbgl.proj = glmf.proj AND dbgl.fund = glmf.fund NO-ERROR.
            IF NOT AVAILABLE dbgl THEN DO:
                       CREATE dbgl.
                       ASSIGN dbgl.acct     = vatLedger
                              dbgl.proj     = glmf.proj
                              dbgl.dept     = glmf.dept
                              dbgl.fund     = glmf.fund
                              dbgl.period   = wsper
                              dbgl.TransID  = wsTransId
                              dbgl.trDATE   = wsDate
                              dbgl.UID      = varUser
                              dbgl.REF      = "dbsg08.p"
                              dbgl.DESCRIP  = "Instalment Raising"
                              dbgl.CREDATE  = TODAY
                              dbgl.SOURCE   = wsSource.
            END.
            ASSIGN dbgl.AMT = dbgl.AMT + (wsVat * -1).
        END.
                        /* Create record for the Control Ledger record */
        /*FIND FIRST glmf WHERE glmf.acct = dbsgr.ctrLedger NO-LOCK NO-ERROR. */
        FIND FIRST dbgl WHERE dbgl.acct = dbsgr.ctrLedger AND dbgl.period = wsper
                          AND dbgl.TransID = wsTransId AND dbgl.dept = glmf.dept 
                          AND dbgl.proj = glmf.proj AND dbgl.fund = glmf.fund NO-ERROR.
        IF NOT AVAILABLE dbgl THEN DO:
               CREATE dbgl.
               ASSIGN dbgl.acct     = dbsgr.ctrLedger
                      dbgl.proj     = glmf.proj
                      dbgl.dept     = glmf.dept
                      dbgl.fund     = glmf.fund
                      dbgl.period   = wsper
                      dbgl.TransID  = wsTransId
                      dbgl.trDATE   = wsDate
                      dbgl.UID      = varUser
                      dbgl.REF      = "dbsg08.p"
                      dbgl.DESCRIP  = "Instalment Raising"
                      dbgl.CREDATE  = TODAY
                      dbgl.SOURCE   = wsSource.
        END.
        ASSIGN dbgl.AMT = dbgl.AMT + wscharge.
        ASSIGN wsTotal = wsTotal + wsCharge - wsVat
        wsTVat  = wsTVat  + wsVat.
    END.
    RETURN.
END.
