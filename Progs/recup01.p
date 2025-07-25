/* Program.................recUp01.p
   Notes:......            Receipt update.
   Author:.................S. Mawire
*/

DEF NEW SHARED STREAM a.
DEF NEW SHARED STREAM bb.

DEF SHARED VAR w-orientation AS CHAR      INITIAL "Landscape"    NO-UNDO.
DEF  SHARED VAR varUser LIKE simusr.usercode.
DEF SHARED VAR wsDate     LIKE dbRecCtr.RecDate.
DEF SHARED VAR wsOp       LIKE dbRecCtr.usercode.
DEF VAR wsSource AS CHAR INITIAL "RE". 
DEF VAR wsAmt AS DEC FORM "zzzzzz9.99-".
DEF VAR wsTotal LIKE wsAmt.
DEF VAR wsVat LIKE wsAmt.
DEF VAR wsTVat LIKE wsAmt.
DEF VAR wsDr   LIKE wsAmt.
DEF VAR wsCr   LIKE wsAmt.
DEF VAR wsTDr     LIKE wsAmt.
DEF VAR wsTCr     LIKE wsAmt.
DEF VAR wsCbTotal LIKE wsAmt.
DEF VAR wsPrAmt LIKE wsAmt.
DEF VAR wsLedger LIKE glmf.acct.
DEF VAR vatLedger LIKE glmf.acct.
DEF VAR wsTransId      LIKE dbgl.TransID.
DEF VAR wsTime      AS CHAR FORM "x(8)".
DEF  VAR wsper LIKE simctr.curper.
DEF  VAR wsyear     AS INT.
DEF VAR wsmonth     AS INT.
DEF VAR wsdisc AS DEC.
DEF VAR wsdiscount AS DEC INITIAL  0.1.
DEF VAR X AS INT.
DEF VAR Y AS INT.
DEF SHARED VAR wsTitle AS CHAR FORM "x(80)".
DEF VAR wsDes LIKE dbRCod.Descrip.
DEF  VAR varDept    LIKE glmf.dept.
DEF VAR varProj     LIKE glmf.proj.
DEF VAR varFund     LIKE glmf.fund.
DEF VAR dbRate      LIKE dbrec.decRate.

DEF BUFFER bfdbrec FOR dbrec.
DEF BUFFER bfDbrecH FOR dbRecH.

FORM bfdbRec.RecNo      LABEL "RECEIPT"
     bfdbRec.Account    LABEL "ACCOUNT"
     bfdbRec.Descrip      LABEL "DESCRIPTION"
     bfdbRec.Paytype      LABEL "P/CODE"
     bfdbRec.rCode      LABEL "ICODE"
     bfdbRec.Amount     LABEL "AMOUNT" FORM "zzzzzzzzz9.99-"
     bfdbRec.txtCur     NO-LABEL
    HEADER wsTitle AT 20
      skip(1) "             RECEIPT UPDATE REPORT FOR CASHIER: " wsOp " RECEIPT DATE: " wsDate 
    "Page: " AT 90 PAGE-NUMBER SKIP(1) 
    "                :" wsdes  SKIP(2)
    WITH NO-LABEL DOWN STREAM-IO FONT 8 WIDTH 132 CENTERED NO-BOX FRAME frm-rpt.

FORM cbkmf.descrip      LABEL "BANK"
     wsTotal            LABEL "AMOUNT" FORM "zzzzzzzzzz9.99-"
     HEADER wstitle AT 20
     skip(1) "             RECEIPT BANKING REPORT FOR CASHIER: " wsOp "DATE: " wsDate 
    "Page: " AT 90 PAGE-NUMBER SKIP(2) 
    WITH NO-LABEL DOWN STREAM-IO FONT 8 WIDTH 132 CENTERED NO-BOX FRAME frm-rpt2.

FORM dbgl.fund          LABEL "SEGMENT"
     dbgl.Proj          LABEL "PROJECT"
     dbgl.dept          LABEL "DEPT"
     dbgl.acct          LABEL "ACCOUNT"
     glmf.DESCRIPTION   LABEL "DESCRIPTION"
     wsDr               LABEL "DEBIT" FORM "zzzzzzzzzz9.99-"
     wsCr               LABEL "CREDIT" FORM "zzzzzzzzzz9.99-"
     HEADER wsTitle AT 20
     skip(1) "             RECEIPT CONSOLIDATION REPORT FOR CASHIER: " wsOp "DATE: " wsDate 
    "Page: " AT 90 PAGE-NUMBER SKIP(2) 
    WITH NO-LABEL DOWN STREAM-IO FONT 8 CENTERED NO-BOX
    WIDTH 132 FRAME frm-lrpt.

FORM glmf.acct             LABEL "ACCOUNT"
     glmf.DESCRIPTION      LABEL "DESCRIPTION"
     wsDr                  LABEL "DEBITS"  FORM "zzzzzzzzzz9.99-"
     wsCr                  LABEL "CREDITS" FORM "zzzzzzzzzz9.99-"
     HEADER wsTitle AT 20
     skip(1) "             RECEIPT CONSOLIDATION REPORT FOR CASHIER: " wsOp "DATE: " wsDate 
    "Page: " AT 90 PAGE-NUMBER SKIP(2) 
    WITH NO-LABEL DOWN STREAM-IO FONT 8 WIDTH 132 CENTERED NO-BOX FRAME frm-rpt3.

/* *** Main logic **** */
FIND FIRST SIMCTR NO-ERROR.
ASSIGN wsPer = SIMCTR.CURPER
       wsMonth = INT(SUBSTR(string(wsPer),5,2))
       wsYear  = INT(SUBSTR(string(wsPer),1,4)).
wsTime    = STRING(TIME,"HH:MM:SS").
wsTransID = DEC (string(YEAR(TODAY),"9999")
              + string(MONTH(TODAY),"99" )
              + string(DAY(TODAY),"99")
              + SUBSTR(STRING(wsTIME),1,2) 
              + SUBSTR(STRING(wsTIME),4,2) 
             + SUBSTR(STRING(wsTIME),7,2) ).
FIND FIRST dbRecCtr WHERE dbRecCtr.usercode = wsOp AND dbRecCtr.RecDate = wsDate EXCLUSIVE-LOCK NO-ERROR.
IF AVAILABLE dbRecCtr AND dbRecCtr.EOD = YES AND VALIDATE = YES THEN DO:
        wsper = INT(STRING(YEAR(dbRecCtr.RecDate)) + STRING(MONTH(dbRecCtr.RecDate),"99")).
    FIND FIRST tblForex WHERE tblForex.txtCur = simctr.dbCur AND dbRecCtr.RecDate >= tblForex.DtRate  NO-LOCK NO-ERROR.
    IF AVAILABLE tblForex THEN DO:
       ASSIGN dbRate = tblForex.decRate.
    END.
    IF NOT AVAILABLE tblForex  THEN DO:
       FIND LAST tblForexH WHERE tblForexH.txtCur = simctr.dbCur AND tblForexH.dtRate <= dbRecCtr.RecDate NO-LOCK NO-ERROR.
            IF AVAILABLE tblForexH THEN DO:
                ASSIGN dbRate = tblForexH.decRate.
            END.
    END.
   /* MESSAGE "Would you like to offer 10% Payment discount(Y/N)?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
            UPDATE wsans AS LOGICAL. */
    {PrintOpt.i &stream-name="stream a"
                        &print-prog="Update-ip"
                        &paged}
END.

PROCEDURE UPDATE-ip: 
FOR EACH bfdbrec WHERE bfdbrec.recDate = wsDate AND bfdbRec.opCode = wsOp AND  bfdbrec.RecStat <> "C"
                   BREAK BY bfdbrec.rcode:
   IF FIRST-OF(bfdbrec.rcode) THEN DO:
      FIND FIRST dbRCod WHERE dbRCod.rCode =  bfdbrec.rcode NO-LOCK NO-ERROR.
      ASSIGN wsTotal   = 0
             wsAmt     = 0
             wsTVat    = 0
             wsCbTotal = 0.
       IF AVAILABLE dbRcod THEN DO:
            ASSIGN varDept = dbRcod.dept
                   varProj = dbRCod.proj.
           IF dbRcod.dept <> 0 THEN DO:              
               FIND FIRST gldept WHERE gldept.dept = dbrcod.dept NO-LOCK NO-ERROR.
               IF NOT AVAILAB gldept THEN
                    varFund = gldept.fund.
           END.         
       END.
      PAGE STREAM a.
    END.
    wsDes = dbRCod.Descrip.
    DISPLAY STREAM a bfdbRec.Recno bfdbRec.Account bfdbRec.Descrip bfdbRec.rCode bfdbRec.Paytype 
        bfdbRec.Amount bfdbRec.txtCur WITH FRAME frm-rpt.
    DOWN STREAM a WITH FRAME frm-rpt.
    ASSIGN wsAmt =  ROUND((bfdbRec.Amount * bfdbrec.decRate),2) WHEN  dbRCod.vat% = 0
           /*wsAmt = ROUND((bfdbRec.Amount * bfdbrec.decRate) * 100 / (dbRCod.vat% + 100),2) WHEN dbRCod.vat% <> 0 */
            wsAmt = ROUND((bfdbRec.Amount * bfdbrec.decRate) * 100 / (dbRCod.vat% + 100),2) WHEN dbRCod.vat% <> 0
           wsVat = ROUND(((bfdbRec.Amount * bfdbrec.decRate) - wsAmt),2).
    IF dbRCod.TARGET = "V" THEN DO: /* direct TO Vote receipts */
        /* income ledger*/
        FIND FIRST dbpay WHERE dbPay.Paytype = bfdbrec.Paytype NO-LOCK NO-ERROR.
        FIND FIRST cbkmf WHERE cbkmf.bank = dbpay.bank NO-LOCK NO-ERROR.
        FIND FIRST glmf WHERE glmf.acct = cbkmf.ledger NO-LOCK NO-ERROR.
        IF AVAILABLE glmf THEN
            ASSIGN varDept = glmf.dept WHEN varDept <> 0
                   varProj = glmf.proj WHEN varproj <> 0
                   varFund = glmf.fund WHEN varDept <> 0.
        ELSE ASSIGN varDept = 0
                   varProj = 0
                   varFund =0.
        
        CREATE dbgl.
        ASSIGN dbgl.acct    = dbrcod.ledger
               dbgl.proj    = varProj
               dbgl.dept    = vardept
               dbgl.fund    = varfund
               dbgl.AMT     = (wsAmt * -1)
               dbgl.CREDATE = TODAY
               dbgl.DESCRIP = bfdbRec.Descrip
               dbgl.period  = wsPer
               dbgl.REF     = "M" + bfdbRec.OpCode + "REC" + STRING(bfdbRec.RecNo)
               dbgl.source  = "RE"
               dbgl.TransID = wsTransId
               dbgl.trDATE  = bfdbRec.RecDate
               dbgl.UID     = bfdbRec.OpCode
               dbgl.UID2    = varUser.
        ASSIGN wsTotal = wsTotal + wsAmt
               wsTVat  = wsTVat + wsVat
               wsCBTotal = wsCbTotal + ROUND((bfdbRec.Amount * bfdbrec.decRate),2).
    END. /* eof target = V */ 
    ELSE IF dbRCod.TARGET = "C" THEN DO:
        ASSIGN wsAmt =  bfdbRec.Amount WHEN bfdbrec.txtCur = simctr.dbCur 
               wsAmt =  ROUND((bfdbRec.Amount * (bfdbrec.decRate / dbRate)),2) WHEN bfdbrec.txtCur <> simctr.dbCur . /*consumer receipts */
               wsdisc = wsAmt.
        FIND FIRST dbsgr WHERE dbsgr.Sgrp = dbRCod.Sgrp NO-LOCK NO-ERROR.
        FIND FIRST glmf WHERE glmf.acct = dbsgr.ctrLedger NO-LOCK NO-ERROR.
        FIND FIRST gldept WHERE gldept.dept = dbsgr.dept NO-LOCK NO-ERROR.
        IF AVAILABLE gldept THEN
            ASSIGN wsLedger = dbsgr.ctrLedger
                   varProj  = dbsgr.proj
                   varDept  = dbsgr.dept
                   varFund  = gldept.Fund.
         ELSE  ASSIGN wsLedger = dbsgr.ctrLedger
                   varProj  = 0
                   varDept  = 0
                   varFund  = 0.
        FIND FIRST dbcmf WHERE dbcmf.dbAcc = bfdbRec.Account EXCLUSIVE-LOCK NO-ERROR.
        FIND FIRST dbblf WHERE dbblf.dbacc = bfdbRec.Account 
                           AND  dbblf.Sgrp = dbRCod.Sgrp NO-LOCK NO-ERROR.
        CREATE dbhtf.
        ASSIGN dbhtf.Accper = wsPer
               dbhtf.proj   = varProj
               dbhtf.dept   = vardept
               dbhtf.fund   = varfund
               dbhtf.Amt    = wsAmt * - 1
               dbhtf.Vat    = ROUND((wsAmt * -1 * (dbblf.vat / dbblf.dbamt[1])),2) /* (dbRCod.vat% / (dbRCod.vat% + 100)) */
               dbhtf.dbacc  = bfdbRec.Account
               dbhtf.descrip = "Receipt(" + STRING(dbRCod.Sgrp) + ")"
               dbhtf.PoDate = today
               dbhtf.prog   = "recupd.p"
               dbhtf.Ref    =  "M" + bfdbRec.OpCode + "R" + STRING(bfdbRec.RecNo)
               dbhtf.Sgrp   =  dbRCod.Sgrp
               dbhtf.Tarif  = 0
               dbhtf.TransID = wsTransId
               dbhtf.trDate  = bfdbRec.RecDate
               dbhtf.UID     = bfdbRec.OpCode
               dbhtf.decRate =  bfdbRec.decRate
               dbhtf.txtCur  =  bfdbRec.txtCur
               dbhtf.ILedger = dbsgr.ctrLedger.
        CREATE dbmtf.
        ASSIGN dbmtf.Accper = wsPer
               dbmtf.proj   = varProj
               dbmtf.dept   = vardept
               dbmtf.Amt    = wsAmt * - 1
               dbmtf.Vat    = wsAmt * -1 * ROUND((wsAmt * -1 * (dbblf.vat / dbblf.dbamt[1])),2) /* (dbRCod.vat% / (dbRCod.vat% + 100)) */
               dbmtf.dbacc  = bfdbRec.Account
               dbmtf.Ref    = "M" + bfdbRec.OpCode + "R" + STRING(bfdbRec.RecNo)
               dbmtf.Sgrp   = dbRCod.Sgrp
               dbmtf.descrip = "Receipt(" + STRING(dbRCod.Sgrp) + ")"
               dbmtf.Tarif  = 0
               dbmtf.prog   = "recupd.p"
               dbmtf.decRate =  bfdbRec.decRate
               dbmtf.txtCur  =  bfdbRec.txtCur
               dbmtf.trDate  = bfdbRec.RecDate.
        ASSIGN wsTotal = wsTotal + ROUND((bfdbRec.Amount * bfdbrec.decRate),2) 
               wsTVat  = wsTVat + ROUND((bfdbRec.Amount * bfdbrec.decRate * (dbblf.vat / dbblf.dbamt[1])),2) WHEN dbblf.dbamt[1] <> 0
               wsCBTotal = wsCBTotal + ROUND((bfdbRec.Amount * bfdbrec.decRate),2).
        ASSIGN dbcmf.AccBal = dbcmf.AccBal - wsAmt
               dbcmf.lpdate = bfdbrec.recdate.
        FIND FIRST dbblf WHERE dbblf.dbacc = bfdbRec.Account 
                           AND  dbblf.Sgrp = dbRCod.Sgrp EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE dbblf THEN DO:
            CREATE dbblf.
            ASSIGN dbblf.dbacc = bfdbRec.Account
                   dbblf.Sgrp   = dbRCod.Sgrp.
        END.
        ASSIGN dbblf.dbamt[1] = dbblf.dbamt[1] - wsAmt
               dbblf.vat      = dbblf.vat - dbmtf.Vat
               dbblf.dbamt[2] = dbblf.dbamt[2] - ROUND((bfdbRec.Amount * bfdbRec.decRate),2). /* forex coversion */
        IF dbblf.INT > 0 AND dbblf.INT > wsAmt THEN /* update Interest first */
           ASSIGN dbblf.INT = dbblf.INT - wsAmt
                  wsAmt = 0.
        ELSE  IF dbblf.INT > 0 AND dbblf.INT <= wsAmt THEN
            ASSIGN wsAmt =  wsAmt - dbblf.INT 
                   dbblf.INT = 0.
        IF wsAmt > 0 THEN DO: /* Update the debtor age oldest first */
            X = 15.
            DO WHILE X >= 1: 
                IF dbblf.Amt[X] >= wsAmt  THEN
                    ASSIGN dbblf.Amt[X] = dbblf.Amt[X] - wsAmt
                           wsAmt = 0
                           X = 0.
                ELSE IF dbblf.Amt[X] < wsAmt THEN
                    ASSIGN wsamt = wsamt - dbblf.Amt[X] WHEN X <> 1
                           dbblf.Amt[X] = 0 WHEN X <> 1
                           dbblf.Amt[X] = dbblf.Amt[X] - wsAmt WHEN X = 1
                           wsAmt = 0 WHEN X = 1.
               IF wsAmt = 0 THEN
                   X = 0.
               X = X - 1.
            END. /* eo do while */
        END. /* IF wsAmt > 0 */
        /* Victoria Falls Discount */
        /*IF simctr.cocode = "CVF" AND wsAns = YES AND bfdbrec.txtCur = "USD" 
            AND (bfdbrec.Account < 51000000 OR bfdbrec.Account > 71999999) THEN
            RUN disc-ip. */
    END.
    ELSE IF dbRcod.TARGET = "L" THEN DO:
        ASSIGN wsAmt =  bfdbRec.Amount WHEN bfdbrec.txtCur = simctr.dbCur 
               wsAmt =  ROUND((bfdbRec.Amount * (bfdbrec.decRate / dbRate)),2) WHEN bfdbrec.txtCur <> simctr.dbCur. 
        FIND FIRST hsecmf WHERE hsecmf.dbacc = bfdbRec.Account NO-ERROR.
        FIND FIRST hsesch WHERE hsesch.scheme = hsecmf.scheme NO-LOCK NO-ERROR.
        FIND FIRST glmf  WHERE glmf.acct = hsesch.ctrLedger NO-LOCK NO-ERROR.
        IF AVAILABLE glmf THEN
            ASSIGN wsLedger = hsesch.ctrLedger
                   varProj  = glmf.proj
                   varDept  = glmf.dept
                   varFund  = glmf.Fund.
        ELSE  ASSIGN wsLedger = hsesch.ctrLedger
                   varProj  = 0
                   varDept  = 0
                   varFund  = 0.
        ASSIGN Hsecmf.AmtDue[1] = Hsecmf.AmtDue[1] - wsAmt
               Hsecmf.AmtDue[2] = Hsecmf.AmtDue[2] - ROUND((bfdbRec.Amount * bfdbRec.DecRate),2)
               Hsecmf.LTAmt     = wsAmt * -1
               Hsecmf.LTDate    = wsDate
               Hsecmf.LTRate    = bfdbRec.DecRate.
        CREATE hsemtf.   /* create monthly transaction */
        ASSIGN hsemtf.dbacc   = hsecmf.dbacc
               hsemtf.scheme  = hsesch.scheme
               hsemtf.Accper  = wsPer
               hsemtf.Amt     = wsAmt * -1
               hsemtf.Vat     = ROUND((wsAmt * (Hsecmf.Vat / (Hsecmf.Vat + 100)) * -1),2)
               hsemtf.CRate   = bfdbRec.DecRate
               hsemtf.ILedger = hsesch.CtrLedger
               hsemtf.proj    = varProj
               hsemtf.dept    = vardept
               hsemtf.fund    = varfund
               hsemtf.trDate  = wsDate
               hsemtf.ref     = "M" + bfdbRec.OpCode + "R" + STRING(bfdbRec.RecNo)
               hsemtf.DESCRIP = "Receipt".       
        CREATE hsehtf.   /* History record */
        ASSIGN hsehtf.Accper  = wsPer
               hsehtf.dbacc   = hsecmf.dbacc
               hsehtf.scheme  = hsesch.scheme
               hsehtf.Amt     = wsAmt * -1
               hsehtf.Vat     = ROUND((wsAmt * (Hsecmf.Vat / (Hsecmf.Vat + 100)) * -1),2)
               hsehtf.CRate   = bfdbRec.DecRate
               hsehtf.tRate   = bfdbRec.DecRate
               hsehtf.txtCur  = bfdbRec.txtCur
               hsehtf.Iledger = hsesch.CtrLedger
               hsehtf.proj    = varProj
               hsehtf.dept    = vardept
               hsehtf.fund    = varfund
               hsehtf.ref     = "M" + bfdbRec.OpCode + "R" + STRING(bfdbRec.RecNo)
               hsehtf.trDate  =  bfdbRec.RecDate
               hsehtf.PoDate  = TODAY
               hsehtf.prog    = "recupd.p"
               hsehtf.TransID = wsTransId  
               hsehtf.uid     = varUser
               hsehtf.DESCRIP = "Receipt".
        ASSIGN wsTotal = wsTotal + ROUND((bfdbRec.Amount * bfdbrec.decRate),2) 
               wsTVat  = wsTVat + ROUND(((bfdbRec.Amount * bfdbrec.decRate) * (Hsecmf.Vat / (Hsecmf.Vat + 100)) * -1),2)
               wsCBTotal = wsCBTotal + ROUND((bfdbRec.Amount * bfdbrec.decRate),2).
        FIND FIRST hseblf WHERE hseblf.scheme = hsecmf.scheme AND hseblf.dbacc = hsecmf.dbacc NO-ERROR. /* Age analysis data */
        IF NOT AVAILABLE hseblf THEN DO:
            CREATE hseblf.
            ASSIGN hseblf.scheme = hsecmf.scheme
                   hseblf.dbacc  = hsecmf.dbacc.
        END. 
        X = 15.
        DO WHILE X >= 1: /* Update the land debtor age oldest first */
            IF hseblf.Amt[X] >= wsAmt  THEN
                ASSIGN hseblf.Amt[X] = hseblf.Amt[X] - wsAmt
                       wsAmt = 0
                       X = 0.
            ELSE IF hseblf.Amt[X] < wsAmt THEN
                ASSIGN wsamt = wsamt - hseblf.Amt[X] WHEN X <> 1
                       hseblf.Amt[X] = 0 WHEN X <> 1
                       hseblf.Amt[X] = hseblf.Amt[X] - wsAmt WHEN X = 1
                       wsAmt = 0 WHEN X = 1.
           IF wsAmt = 0 THEN
               X = 0.
           X = X - 1.
        END.      
    END.  /* eof Target-type */
    IF LAST-OF(bfdbrec.rcode) THEN DO: 
        IF dbRCod.TARGET = "V" THEN DO:
            wsLedger = dbrcod.ledger.
            IF wsTvat <> 0  THEN DO:
                FIND FIRST glmf WHERE glmf.acct = wsLedger NO-LOCK NO-ERROR.
                vatLedger = simctr.vat[2].
                FIND FIRST dbgl WHERE dbgl.acct = vatLedger AND dbgl.proj = varProj AND
                       dbgl.dept = vardept AND dbgl.fund = varfund AND dbgl.REF  = "VAT" + STRING(dbRCod.rcode) NO-ERROR.
                IF NOT AVAILABLE dbgl THEN DO:
                    CREATE dbgl. /* tdf for VAT */
                    ASSIGN dbgl.acct    = vatLedger
                           dbgl.proj    = varProj
                           dbgl.dept    = vardept
                           dbgl.fund    = varfund
                           dbgl.CREDATE = TODAY
                           dbgl.DESCRIP = dbRCod.Descrip
                           dbgl.period  = wsPer
                           dbgl.REF     = "VAT" + STRING(dbRCod.rcode)
                           dbgl.source  = "RE"
                           dbgl.TransID = wsTransId
                           dbgl.trDATE  = bfdbRec.RecDate
                           dbgl.UID     = bfdbRec.OpCode
                           dbgl.UID2    = varUser.
                END.
                ASSIGN dbgl.AMT  = dbgl.AMT + (wsTVat * -1).
            END. /* eof wsTvat <> 0 */          
        END. /* eof Target-type V */
        ELSE IF dbRCod.TARGET = "C" THEN DO: /*update ledger for service */
            FIND FIRST dbsgr WHERE dbsgr.Sgrp = dbRCod.Sgrp NO-LOCK NO-ERROR.
                FIND FIRST dbgl WHERE dbgl.acct = wsLedger AND dbgl.proj = varProj AND
                       dbgl.dept = vardept AND dbgl.fund = varfund AND dbgl.REF = bfdbRec.OpCode + "BT" + STRING(dbsgr.sgrp)NO-ERROR.
                IF NOT AVAILABLE dbgl THEN DO:
                    CREATE dbgl.
                    ASSIGN dbgl.acct    = wsLedger
                           dbgl.proj    = varProj
                           dbgl.dept    = vardept
                           dbgl.fund    = varfund
                           dbgl.CREDATE = TODAY
                           dbgl.DESCRIP = "Rec:" + string(dbRCod.Descrip)
                           dbgl.period  = wsPer
                           dbgl.REF     = bfdbRec.OpCode + "BT" + STRING(dbsgr.sgrp)
                           dbgl.source  = "RE"
                           dbgl.TransID = wsTransId
                           dbgl.trDATE  = bfdbRec.RecDate
                           dbgl.UID     = bfdbRec.OpCode
                           dbgl.UID2    = varUser.
                END.
                ASSIGN dbgl.AMT  = dbgl.AMT + (wsTotal * -1).
        END.
        ELSE IF dbRCod.TARGET = "L" THEN DO: /*update ledger for service */
            FIND FIRST glmf WHERE glmf.acct = hsesch.CtrLedger NO-LOCK NO-ERROR.
            FIND FIRST dbgl WHERE dbgl.acct = hsesch.CtrLedger AND dbgl.period = wsper
                               AND dbgl.TransID = wsTransId AND dbgl.proj = varproj
                               AND dbgl.dept = vardept AND dbgl.fund = varfund NO-ERROR.
            IF NOT AVAILABLE dbgl THEN DO:
                   CREATE dbgl.
                   ASSIGN dbgl.acct     = hsesch.CtrLedger
                          dbgl.proj     = varproj
                          dbgl.dept     = vardept
                          dbgl.fund     = varfund
                          dbgl.period   = wsper
                          dbgl.TransID  = wsTransId
                          dbgl.trDATE   = wsDate
                          dbgl.UID      = varUser
                          dbgl.REF      = "recupd.p"
                          dbgl.DESCRIP  = "Receipt"
                          dbgl.CREDATE  = TODAY
                          dbgl.SOURCE   = wsSource.
            END.
            ASSIGN dbgl.AMT = dbgl.AMT - wsTotal.
        END. /* eof Target-type  C*/
        IF (dbRCod.TARGET = "C" OR dbRCod.TARGET = "L") THEN DO: /* VAT */
            IF wsTvat <> 0  THEN DO:
                FIND FIRST glmf WHERE glmf.acct = wsLedger NO-LOCK NO-ERROR.
                DO X = 1 TO 2:
                    vatLedger = simctr.vat[X].
                    FIND FIRST dbgl WHERE dbgl.acct = VatLedger AND dbgl.proj = varProj AND
                       dbgl.dept = vardept AND dbgl.fund = varfund AND dbgl.REF  = "VAT" + STRING(dbRCod.rcode) NO-ERROR.
                    IF NOT AVAILABLE dbgl THEN DO:
                        CREATE dbgl. /* tdf for VAT */
                        ASSIGN dbgl.acct    = vatLedger
                               dbgl.proj    = varProj
                               dbgl.dept    = vardept
                               dbgl.fund    = varfund
                               dbgl.CREDATE = TODAY
                               dbgl.DESCRIP = dbRCod.Descrip
                               dbgl.period  = wsPer
                               dbgl.REF  = "VAT" + STRING(dbRCod.rcode)
                               dbgl.source  = "RE"
                               dbgl.TransID = wsTransId
                               dbgl.trDATE  = bfdbRec.RecDate
                               dbgl.UID     = bfdbRec.OpCode
                               dbgl.UID2    = varUser.
                    END.
                    ASSIGN dbgl.AMT     = wsTVat       WHEN X = 1
                           dbgl.AMT     = wsTVat * -1  WHEN X = 2.
                END.    
            END. /* eof wsTvat <> 0 */
        END. 
        UNDERLINE STREAM a bfdbRec.Descrip bfdbRec.Amount WITH FRAME frm-rpt.
        DOWN STREAM a WITH FRAME frm-rpt.
        DISPLAY STREAM a "TOTAL---" @ bfdbRec.Descrip wsCBTotal @ bfdbRec.Amount WITH FRAME frm-rpt.
        DOWN STREAM a WITH FRAME frm-rpt.
        PAGE STREAM a.
    END. /* eof last-of */
END.
/*MESSAGE "going to Cshbook" VIEW-AS ALERT-BOX.*/
/* Create Cashbook Transactions */
FOR EACH bfdbrec WHERE bfdbrec.recDate = wsDate AND bfdbRec.opCode = wsOp 
        BREAK BY bfdbrec.RecNo:
        IF  bfdbrec.recstat = "C" THEN DO:
            CREATE bfdbrecH.
            BUFFER-COPY bfdbRec TO bfdbrecH.
            DELETE bfdbRec.
            NEXT.
        END.
        FIND FIRST dbrcod WHERE dbrcod.rcode = bfdbrec.rcode NO-LOCK NO-ERROR.
        IF dbRcod.TARGET = "V" THEN
            wsLedger = dbRcod.ledger.
        ELSE IF dbRcod.TARGET = "C" THEN DO:
            FIND FIRST dbsgr WHERE dbsgr.Sgrp = dbRCod.Sgrp NO-LOCK NO-ERROR.
             wsLedger = dbsgr.ctrLedger.
        END.
        FIND FIRST dbpay WHERE dbpay.paytype = bfdbrec.paytype NO-LOCK NO-ERROR.
        FIND FIRST cbkmf WHERE cbkmf.bank = dbPay.bank EXCLUSIVE-LOCK NO-ERROR.
        FIND FIRST glmf WHERE glmf.acct = cbkmf.ledger NO-LOCK NO-ERROR.
        IF dbrcod.dept <> 0 THEN
            varDept  = dbRCod.dept.
        ELSE varDept  = glmf.dept.
        IF dbrcod.proj <> 0 THEN
           varProj  = dbRCod.proj.
            ELSE varProj  = glmf.proj.
         FIND FIRST gldept WHERE gldept.dept = varDept NO-LOCK NO-ERROR. 
         IF AVAILABLE gldept THEN
              varFund = gldept.fund.
         ELSE varFund  = glmf.Fund.
        IF FIRST-OF(bfdbrec.RecNo) THEN
            ASSIGN wsCbTotal = 0
                   Y = 0.
        ASSIGN wsCbTotal = wsCbTotal + bfdbrec.Amount
               wsTotal   = wsTotal   + bfdbrec.Amount.
           
        Y = Y + 1.
        CREATE CBTrans.
        ASSIGN CBTrans.Bank     = cbkmf.bank
               CBTrans.Accper   = wsPer
               CBTrans.amount   = bfdbRec.Amount
               CBTrans.txtCur = bfdbRec.txtCur
               CBTrans.decRate = bfdbRec.decRate
               CBTrans.Descrip  = "Rec:" + string(dbRCod.Descrip)
               CBTrans.Ledger   = wsLedger
               CBTrans.dept     = VarDept
               CBTrans.fund     = VarFUND
               CBTrans.proj     = Varproj
               CBTrans.OpCode   = bfdbRec.OpCode
               CBTrans.TrDate   = bfdbRec.RecDate
               CBTrans.Ref      = "M" + bfdbRec.OpCode + "R" + STRING(bfdbRec.recNo)
               cbTrans.seq      = Y
               cbTrans.srcCode  = STRING(dbRCod.rcode)
               CBTrans.TransID  = wsTransId
               CBTrans.TranType = 2
               CBTrans.acb      = dbRCod.acb.
        FIND FIRST dbgl WHERE dbgl.acct = cbkmf.ledger AND dbgl.proj = glmf.proj AND
                           dbgl.dept = glmf.dept AND dbgl.fund = glmf.FUND NO-ERROR.
        IF NOT AVAILABLE dbgl THEN DO:
                CREATE dbgl. /* tdf cashbook ledger*/
                ASSIGN dbgl.acct = cbkmf.ledger
                       dbgl.proj = glmf.proj
                       dbgl.dept = glmf.dept
                       dbgl.fund = glmf.FUND
                       dbgl.CREDATE = TODAY
                       dbgl.DESCRIP = "Receipt update"
                       dbgl.period  = wsPer
                       dbgl.REF     = "MC#" + bfdbRec.OpCode
                       dbgl.source  = "RE"
                       dbgl.TransID = wsTransId
                       dbgl.trDATE  = bfdbRec.RecDate
                       dbgl.UID     = bfdbRec.OpCode
                       dbgl.UID2    = varUser.
        END.
        ASSIGN dbgl.AMT  = dbgl.AMT + ROUND((bfdbRec.Amount *  bfdbRec.decRate),2).
        IF LAST-OF (bfdbrec.RecNo) THEN DO:
            cbkmf.Bal = cbkmf.Bal + wsCbTotal.
            CREATE CBTrans.
            ASSIGN CBTrans.Bank = cbkmf.Bank 
                   CBTrans.Accper = wsPer
                   CBTrans.amount = wsCBTotal
                   CBTrans.Descrip = bfdbRec.Descrip
                   CBTrans.Ledger  = 99                                                                                         
                   CBTrans.OpCode  = bfdbRec.OpCode
                   CBTrans.TrDate = bfdbRec.RecDate
                   CBTrans.txtCur = bfdbRec.txtCur
                   CBTrans.decRate = bfdbRec.decRate
                   CBTrans.Ref     =  "M" + bfdbRec.OpCode + "R" + STRING(bfdbRec.recNo)
                   cbTrans.seq     = 0
                   cbTrans.SrCcode = "RE"
                   CBTrans.TransID = wsTransId
                   CBTrans.TranType = 2.         
                   wsCbTotal = 0.  
        END. /* eof last-of */
        CREATE bfdbrecH.
        BUFFER-COPY bfdbRec TO bfdbrecH.
        DELETE bfdbRec.
    END.  /*eof each bfdbRec */
DELETE dbRecCtr.
PAGE STREAM a.
{glcon.i}
END. /* of Procedure update.ip */

PROCEDURE disc-ip:
    /*wsdisc = ROUND((wsdisc * wsdiscount),2).*/
    wsdisc = (wsdisc * wsdiscount).
     /* Service Control Ledger */
    FIND FIRST dbgl WHERE dbgl.acct = wsLedger AND dbgl.proj = varProj 
                      AND dbgl.dept = vardept AND dbgl.fund = varfund NO-ERROR.
    IF NOT AVAILABLE dbgl THEN DO:
          CREATE dbgl.
          ASSIGN dbgl.acct    = wsLedger
                 dbgl.proj    = varProj
                 dbgl.dept    = vardept
                 dbgl.fund    = varfund
                 dbgl.CREDATE = TODAY
                 dbgl.DESCRIP = "Payment Discount"
                 dbgl.period  = wsPer
                 dbgl.REF     = "DISC" + bfdbRec.OpCode + "R" + STRING(bfdbRec.RecNo)
                 dbgl.source  = "RE"
                 dbgl.TransID = wsTransId
                 dbgl.trDATE  = bfdbRec.RecDate
                 dbgl.UID     = bfdbRec.OpCode
                 dbgl.UID2    = varUser.
    END.
    /*ASSIGN dbgl.AMT  = dbgl.AMT + ROUND((wsdisc * bfdbrec.decRate * -1),2).*/
    ASSIGN dbgl.AMT  = dbgl.AMT + ROUND((wsdisc * bfdbrec.decRate * -1),2).
                /* Discount Ledger */
    FIND FIRST dbgl WHERE dbgl.acct = 11301208 AND dbgl.proj = varProj AND
                       dbgl.dept = vardept AND dbgl.fund = varfund NO-ERROR.
    IF NOT AVAILABLE dbgl THEN DO:
          CREATE dbgl.
          ASSIGN dbgl.acct    = 11301208
                 dbgl.proj    = varProj
                 dbgl.dept    = vardept
                 dbgl.fund    = varfund
                 dbgl.CREDATE = TODAY
                 dbgl.DESCRIP = "Payment Discount"
                 dbgl.period  = wsPer
                 dbgl.REF     = "DISC" + bfdbRec.OpCode + "R" + STRING(bfdbRec.RecNo)
                 dbgl.source  = "RE"
                 dbgl.TransID = wsTransId
                 dbgl.trDATE  = bfdbRec.RecDate
                 dbgl.UID     = bfdbRec.OpCode
                 dbgl.UID2    = varUser.
    END.
    /*ASSIGN dbgl.AMT  = dbgl.AMT + ROUND((wsdisc * bfdbrec.decRate),2).*/
    ASSIGN dbgl.AMT  = dbgl.AMT + (wsdisc * bfdbrec.decRate).
    CREATE dbhtf.
    ASSIGN dbhtf.Accper = wsPer
           dbhtf.proj   = varProj
           dbhtf.dept   = vardept
           dbhtf.fund   = varfund
           dbhtf.Amt    = wsdisc * -1
           dbhtf.Vat    = 0 
           dbhtf.dbacc  = bfdbRec.Account
           dbhtf.descrip = "Payment Discount"
           dbhtf.PoDate = today
           dbhtf.prog   = "dbjnl02.p"
           dbhtf.Ref    =  "DISC" + bfdbRec.OpCode + "R" + STRING(bfdbRec.RecNo)
           dbhtf.Sgrp   =  dbRCod.Sgrp
           dbhtf.Tarif  = 0
           dbhtf.TransID = wsTransId
           dbhtf.trDate  = bfdbRec.RecDate
           dbhtf.UID     = varUser
           dbhtf.decRate =  bfdbRec.decRate
           dbhtf.txtCur  =  bfdbRec.txtCur
           dbhtf.ILedger = dbsgr.ctrLedger.
    CREATE dbmtf.
    ASSIGN dbmtf.Accper = wsPer
           dbmtf.Amt    =  wsdisc * -1
           dbmtf.Vat    =  0
           dbmtf.dbacc  = bfdbRec.Account
           dbmtf.DESCRIP = "Payment Discount"
           dbmtf.Ref    =  "DISC" + bfdbRec.OpCode + "R" + STRING(bfdbRec.RecNo)
           dbmtf.Sgrp   =  dbRCod.Sgrp
           dbmtf.Tarif  = 0
           dbmtf.prog   = "dbjnl02.p"
           dbmtf.trDate  = bfdbRec.RecDate
           dbmtf.decRate = bfdbRec.decRate
           dbmtf.txtCur  = bfdbRec.txtCur.
    ASSIGN dbcmf.AccBal = dbcmf.AccBal - wsdisc.
    FIND FIRST dbblf WHERE dbblf.dbacc = bfdbRec.Account 
                       AND  dbblf.Sgrp = dbRCod.Sgrp EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE dbblf THEN DO:
       CREATE dbblf.
       ASSIGN dbblf.dbacc = bfdbRec.Account
              dbblf.Sgrp   = dbRCod.Sgrp.
    END.
    ASSIGN dbblf.dbamt[1] = dbblf.dbamt[1] - wsdisc
            dbblf.dbamt[2] = dbblf.dbamt[2] - ROUND((wsdisc * bfdbRec.decRate),2).
           /*dbblf.dbamt[2] = dbblf.dbamt[2] - ROUND((wsdisc * bfdbRec.decRate),2).*/ /* forex coversion */    
    IF wsdisc > 0 THEN DO: /* Update the debtor age oldest first */
        X = 15.
        DO WHILE X >= 1: 
           IF dbblf.Amt[X] >= wsdisc THEN
              ASSIGN dbblf.Amt[X] = dbblf.Amt[X] - wsdisc 
                     wsdisc = 0
                     X = 0.
           ELSE IF dbblf.Amt[X] < wsdisc THEN
                   ASSIGN wsdisc = wsdisc - dbblf.Amt[X] WHEN X <> 1
                          dbblf.Amt[X] = 0 WHEN X <> 1
                          dbblf.Amt[X] = dbblf.Amt[X] - wsdisc WHEN X = 1
                          wsdisc = 0 WHEN X = 1.
           IF wsdisc = 0 THEN
              X = 0.
              X = X - 1.
        END. /* eo do while */
    END. /* IF wsdisc > 0 */
END PROCEDURE.

