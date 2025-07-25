/* Program.................Dbjnl02.p
   Notes:................. Debtors Journal Update
   Author:.................S. Mawire
*/
DEF STREAM a.
DEF STREAM b.
DEF var w-orientation AS CHAR      INITIAL "Landscape"    NO-UNDO.
DEF var w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF var w-fontsize    AS INT       INITIAL 10.0          NO-UNDO.
DEF  shared var varUser LIKE simusr.usercode.
DEF var wsTransId      LIKE gltdf.TransID.
DEF var wsTime      AS CHAR FORM "x(8)".
DEF var wsper LIKE simctr.curper.
DEF var wsyear     AS INT.
DEF var wsmonth     AS INT.
DEF var wsDate     LIKE dbBCtr.BDate.
DEF var wsOp       LIKE dbBCtr.UID.
DEF var wsBatch    LIKE dbBatch.intBatch.
DEF var X AS INT.
DEF var wsAmt AS DEC FORM "zzz,zzz,zz9.99-".
DEF var wsTotal LIKE wsAmt.
DEF var wsTCr   LIKE wsAmt.
DEF var wsTDr   LIKE wsAmt.
DEF var wsDr    LIKE wsAmt.
DEF var wsCr    LIKE wsAmt.
DEF var wsVat   LIKE wsAmt.
DEF var wsRate  LIKE tblForex.decRate.
DEF var wsLedger LIKE glmf.acct.
DEF var vatLedger LIKE glmf.acct.
DEF var wsSource LIKE gltdf.SOURCE INITIAL "DB".
DEF VAR wsFile AS CHAR.

DEF BUTTON Btn-Print   LABEL "Print".
DEF BUTTON btn-Update   LABEL "UPDATE".
DEF BUTTON btn-Exit    LABEL "CLOSE".
DEF BUTTON btn-Prn  LABEL "PRINT".
DEF BUTTON btn-Exp  LABEL "EXPORT".
DEF NEW SHARED BUFFER bfdbBatch FOR dbBatch.

FIND FIRST SIMCTR NO-ERROR.
FIND FIRST simusr WHERE simusr.usercode = varUser NO-ERROR.
wsTime    = STRING(TIME,"HH:MM:SS").
wsTransID = DEC (string(YEAR(TODAY),"9999")
              + string(MONTH(TODAY),"99" )
              + string(DAY(TODAY),"99")
              + SUBSTR(STRING(wsTIME),1,2) 
              + SUBSTR(STRING(wsTIME),4,2) 
              + SUBSTR(STRING(wsTIME),7,2) ).

FORM 
     dbgl.Fund             LABEL "SEGMENT"
     dbgl.Proj             LABEL "PROJECT"
     dbgl.dept             LABEL "DEPARTMENT"
     dbgl.acct             LABEL "ACCOUNT"
     glmf.DESCRIPTION      LABEL "DESCRIPTION"
     wsDr                  LABEL "DEBITS" 
     wsCr                  LABEL "CREDITS" 
     HEADER skip(1) "JOURNAL UPDATE CONSOLIDATION REPORT - DATE: " wsDate  "PERIOD  :" wsPer
       SKIP(1)  
     WITH NO-LABEL DOWN STREAM-IO FONT 10 WIDTH 132 CENTERED NO-BOX FRAME frm-lrpt.
     
FORM dbBatch.Seq     LABEL "SEQ"
     dbBatch.trDate  LABEL "DATE"
     dbBatch.dbacc   LABEL "ACCOUNT"
     dbBatch.Ref     LABEL "REFERENCE"
     dbBatch.Sgrp    LABEL "SERVICE"
     dbBatch.Amt     LABEL "AMOUNT" FORM "zzz,zzz,zz9.99-"
     dbBatch.Vat     LABEL "VAT"
     dbBatch.Iledger LABEL "LEDGER"
     dbBatch.DESCRIP LABEL "NARRATION"
     HEADER skip(1) "DEBTORS JOURNAL LISTING N REPORT FOR BATCH: " wsBatch 
    "DATE: " wsDate  "PERIOD  :" wsPer  "Page: " AT 90 PAGE-NUMBER SKIP(1) 
    "        Batch Captured by:" simusr.Name SKIP(2) 
    WITH NO-LABEL DOWN STREAM-IO FONT 10 WIDTH 164 CENTERED NO-BOX FRAME frmRpt.
 
DEFINE QUERY qry-dbBCtr FOR  dbBCtr scrolling.
DEF BROWSE brw-dbBCtr QUERY qry-dbBCtr
        DISPLAY intBatch COLUMN-LABEL "Batch Number"
                period WIDTH 10 COLUMN-LABEL "Period"
                BDate WIDTH 12 COLUMN-LABEL "Batch Date"
                AMT[1]COLUMN-LABEL "Dr Amt" 
                AMT[2] COLUMN-LABEL "Cr Amt"
                UID COLUMN-LABEL "User"
        WITH 20 DOWN SEPARATORS.

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 graphic-edge  NO-FILL
     SIZE 80 BY 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 80 BY 17.5.

FORM
     bfdbBatch.dbAcc    LABEL "ACCOUNT"
     bfdbBatch.TrDate     LABEL "DATE"
     bfdbBatch.Ref        LABEL "REFERENCE"
     bfdbBatch.Descrip    LABEL "DESCRIPTION"
     bfdbBatch.Sgrp      LABEL "SCODE"
     bfdbBatch.Amt     LABEL "AMOUNT" FORM "zzz,zzz,zz9.99-"
     bfdbBatch.Vat     LABEL "VAT"
    HEADER skip(1) "DEBTORS JOURNAL UPDATE REPORT FOR BATCH: " TRIM(STRING(wsBatch)) 
     "Page: " AT 90 PAGE-NUMBER(a) SKIP(1) 
    "DATE: " wsDate "PERIOD  :" wsPer  " Batch Captured by:" simusr.Name SKIP(2)
    WITH NO-LABEL DOWN STREAM-IO FONT 8 WIDTH 132 CENTERED NO-BOX FRAME frm-rpt.

FORM glmf.acct             LABEL "ACCOUNT"
     glmf.DESCRIPTION      LABEL "DESCRIPTION"
     wsDr                  LABEL "DEBITS" FORM "zzz,zzz,zz9.99"
     wsCr                  LABEL "CREDITS" FORM "zzz,zzz,zz9.99-"
     HEADER skip(1) "DEBTORS JOURNAL UPDATE CONSOLIDATION REPORT FOR BATCH: " wsBatch 
    "DATE: " wsDate  "PERIOD  :" wsPer  "Page: " AT 90 PAGE-NUMBER SKIP(1) 
    "        Batch Captured by:" simusr.Name SKIP(2) 
    WITH NO-LABEL DOWN STREAM-IO FONT 8 WIDTH 132 CENTERED NO-BOX FRAME frm-rpt3.

DEF FRAME frm-main
    brw-dbBCtr AT ROW 2 COL 8
    btn-Update AT ROW 19.7 COL 15 SPACE(10)
    btn-Prn  SPACE(10)
    btn-Exp  SPACE(10) 
    btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 19 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
    SIZE 90 BY 23
    TITLE "DEBTORS JOURNAL UPDATE" view-as dialog-box.

/******* Triggers ***** */
ON CHOOSE OF btn-Update IN FRAME frm-main 
    OR 'mouse-select-dblclick' OF brw-dbBCtr
DO:
    GET CURRENT qry-dbBCtr EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsDate = dbBCtr.BDate
           wsBatch = dbBCtr.intBatch
           wsPer = dbBCtr.period
           wsMonth = INT(SUBSTR(string(wsPer),5,2))
           wsYear  = INT(SUBSTR(string(wsPer),1,4)).
    /* Select print */
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="UPDATE-ip"
                    &paged} 
   
END.

ON CHOOSE OF btn-Prn IN FRAME frm-main
DO:
    GET CURRENT qry-dbBCtr NO-LOCK.
   
    ASSIGN wsDate = dbBCtr.BDate 
          wsPer  = dbBCtr.period
          wsBatch = dbBCtr.intBatch.
   FIND FIRST simusr WHERE simusr.usercode = dbBCtr.UID NO-LOCK NO-ERROR.
    /* Select print */
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="Report.ip"
                    &paged} 
    RETURN.
END.

ON CHOOSE OF btn-Exp IN FRAME frm-main 
DO:  
   GET CURRENT qry-dbBCtr NO-LOCK.
    MESSAGE dbBCtr.BDate VIEW-AS ALERT-BOX.
   wsFile = TRIM(SIMCTR.repDir) + "dbjnl" + STRING(dbBCtr.intBatch) + ".csv".
   OUTPUT STREAM b TO VALUE(wsFile).
   ASSIGN wsDate = dbBCtr.BDate 
          wsPer  = dbBCtr.period
          wsBatch = dbBCtr.intBatch.
   FIND FIRST simusr WHERE simusr.usercode = dbBCtr.UID NO-LOCK NO-ERROR.
   RUN EXP.ip. 
   OUTPUT STREAM b CLOSE.
   OS-COMMAND NO-WAIT VALUE(wsFile).
   RETURN.
END.
/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
OPEN QUERY qry-dbBCtr FOR EACH dbBCtr NO-LOCK.
ENABLE ALL  EXCEPT WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
CLOSE QUERY qry-dbBCtr.
HIDE FRAME frm-main.

PROCEDURE UPDATE-ip:
/*DO TRANSACTION ON ERROR UNDO, LEAVE: */
    FOR EACH bfdbBatch WHERE bfdbBatch.IntBatch = wsBatch EXCLUSIVE-LOCK:
        FIND FIRST tblForex WHERE tblForex.txtCur = simctr.dbCur AND bfdbBatch.TrDate >= tblForex.DtRate  NO-LOCK NO-ERROR.
         IF AVAILABLE tblForex THEN DO:
            ASSIGN wsRate = tblForex.decRate.
         END.
         IF NOT AVAILABLE tblForex  THEN DO:
             FIND LAST tblForexH WHERE tblForexH.txtCur = simctr.dbCur AND bfdbBatch.TrDate >= tblForexH.dtRate NO-LOCK NO-ERROR.
                IF AVAILABLE tblForexH THEN DO:
                    ASSIGN wsRate = tblForexH.decRate.
                END.
         END.
        RUN writ.ip.
        RUN gl.ip.
       DELETE bfdbBatch.
    END.
    FIND FIRST dbBctr WHERE dbBctr.intbatch = wsBatch EXCLUSIVE-LOCK.
    DELETE dbBCtr.
    PAGE STREAM a.
    ASSIGN wsTCr = 0
           wsTDr = 0.   
      {glcon.i} 
    CLOSE QUERY qry-dbBCtr.
    OPEN QUERY qry-dbBCtr FOR EACH dbBCtr NO-LOCK.
    APPLY 'ENTRY' TO btn-exit IN FRAME frm-main.
/*END. */
END. /* of Procedure update.ip */ 

PROCEDURE gl.ip:
/* Create GL consolidation data */
        FIND FIRST dbgl WHERE dbgl.acct = bfdbBatch.iledger AND dbgl.period = wsper
                               AND dbgl.TransID = wsTransId no-error.
            IF NOT AVAILABLE dbgl THEN DO:
                   CREATE dbgl.
                   ASSIGN dbgl.acct = bfdbBatch.iledger
                          dbgl.dept = bfdbBatch.dept
                          dbgl.fund = bfdbBatch.fund
                          dbgl.proj = bfdbBatch.proj
                          dbgl.period = wsper
                          dbgl.TransID = wsTransId
                          dbgl.trDATE  = wsDate
                          dbgl.UID     = dbBCtr.UID
                          dbgl.UID2    = varUser
                          dbgl.REF     = "db" + STRING(bfdbBatch.intBatch)
                          dbgl.DESCRIP = "Online journal consolidation"
                         dbgl.CREDATE = TODAY
                         dbgl.SOURCE  = "DB".
            END.
            ASSIGN dbgl.AMT = dbgl.AMT + ROUND(((bfdbBatch.Amt - bfdbBatch.Vat) * -1 * wsRate),2).
        IF  bfdbBatch.Vat <> 0 THEN DO:
            FIND FIRST glmf WHERE glmf.acct = SIMCTR.Vat[1] NO-LOCK NO-ERROR.
            FIND FIRST dbgl WHERE dbgl.acct = SIMCTR.Vat[1] AND dbgl.period = wsper
                               AND dbgl.TransID = wsTransId no-error.
            IF NOT AVAILABLE dbgl THEN DO:
                   CREATE dbgl.
                   ASSIGN dbgl.acct = SIMCTR.Vat[1]
                          dbgl.dept = glmf.dept
                          dbgl.fund = bfdbBatch.fund
                          dbgl.proj = bfdbBatch.proj
                          dbgl.period = wsper
                          dbgl.TransID = wsTransId
                          dbgl.trDATE  = wsDate
                          dbgl.UID     = dbBCtr.UID
                          dbgl.UID2     = varUser
                          dbgl.REF     = "db" + STRING(bfdbBatch.intBatch)
                          dbgl.DESCRIP = "Online journal consolidation"
                         dbgl.CREDATE = TODAY
                         dbgl.SOURCE  = "DB".
            END.
            ASSIGN dbgl.AMT = dbgl.AMT + ROUND((bfdbBatch.Vat * -1 * wsRate),2).
        END.
        FIND FIRST glmf WHERE glmf.acct = dbsgr.ctrLedger NO-LOCK NO-ERROR.
        IF NOT AVAILABLE glmf THEN
                 MESSAGE dbsgr.ctrLedger " Control not available" VIEW-AS ALERT-BOX.
        ELSE DO:
            FIND FIRST dbgl WHERE dbgl.acct = dbsgr.ctrLedger AND dbgl.period = wsper
                               AND dbgl.TransID = wsTransId no-error.
            IF NOT AVAILABLE dbgl THEN DO:
                   CREATE dbgl.
                   ASSIGN dbgl.acct = dbsgr.ctrLedger
                          dbgl.dept = glmf.dept
                          dbgl.fund = bfdbBatch.fund
                          dbgl.proj = bfdbBatch.proj
                          dbgl.period = wsper
                          dbgl.TransID = wsTransId
                          dbgl.trDATE  = wsDate
                          dbgl.UID     = dbBCtr.UID
                          dbgl.UID2     = varUser
                          dbgl.REF     = "db" + STRING(bfdbBatch.intBatch)
                          dbgl.DESCRIP = "Online journal consolidation"
                         dbgl.CREDATE = TODAY
                         dbgl.SOURCE  = "DB".
            END.
            ASSIGN dbgl.AMT = dbgl.AMT + ROUND((bfdbBatch.Amt * wsRate),2).
        END.
END PROCEDURE.

PROCEDURE writ.ip:
        FIND FIRST dbSgr WHERE dbSgr.Sgrp =  bfdbBatch.Sgrp NO-LOCK NO-ERROR.
        ASSIGN wsAmt = bfdbBatch.Amt - bfdbBatch.Vat
               wsVat = bfdbBatch.Vat.
        CREATE dbhtf.
        ASSIGN dbhtf.Accper = wsPer
               dbhtf.Amt    =  bfdbBatch.Amt
               dbhtf.Vat    =  wsVat
               dbhtf.dbacc  = bfdbBatch.dbacc
               dbhtf.DESCRIP = bfdbBatch.descrip
               dbhtf.PoDate = today
               dbhtf.prog   = "dbjnl02.p"
               dbhtf.Ref    =  bfdbBatch.Ref
               dbhtf.Sgrp   =  dbSgr.Sgrp
               dbhtf.Tarif  = 0
               dbhtf.TransID = wsTransId
               dbhtf.Iledger = bfdbBatch.iledger
               dbhtf.trDate  = bfdbBatch.TrDate
               dbhtf.UID     = dbBCtr.UID
               dbhtf.UID2    = varUser
               dbhtf.decRate = wsrate
               dbhtf.txtCur  = simctr.dbCur.
        CREATE dbmtf.
        ASSIGN dbmtf.Accper = wsPer
               dbmtf.Amt    =  bfdbBatch.Amt
               dbmtf.Vat    =  wsVat
               dbmtf.dbacc  = bfdbBatch.dbacc
               dbmtf.DESCRIP = bfdbBatch.descrip
               dbmtf.Ref    =  bfdbBatch.Ref
               dbmtf.Sgrp   =  dbSgr.Sgrp
               dbmtf.Tarif  = 0
               dbmtf.prog   = "dbjnl02.p"
               dbmtf.trDate  = bfdbBatch.TrDate
               dbmtf.decRate = wsrate
               dbmtf.txtCur  = simctr.dbCur.
        FIND FIRST dbcmf WHERE dbcmf.dbAcc = bfdbBatch.dbAcc EXCLUSIVE-LOCK NO-ERROR.       
        ASSIGN dbcmf.AccBal = dbcmf.AccBal + bfdbBatch.Amt.
        RELEASE dbcmf. 
        FIND FIRST dbblf WHERE dbblf.dbacc = bfdbBatch.dbAcc 
                           AND  dbblf.Sgrp = dbSgr.Sgrp EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE dbblf THEN DO:
                CREATE dbblf.
                ASSIGN dbblf.dbacc = bfdbBatch.dbacc
                       dbblf.Sgrp   = dbSgr.Sgrp.
        END.
        ASSIGN dbblf.amt[bfdbBatch.age] = dbblf.amt[bfdbBatch.age] + bfdbBatch.Amt
               dbblf.dbAmt[1] = dbblf.dbAmt[1] +  bfdbBatch.Amt 
               dbblf.vat = dbblf.vat + dbmtf.Vat
               dbblf.dbAmt[2] = dbblf.dbAmt[2] +  ROUND((bfdbBatch.Amt * wsRate),2).
        DISPLAY STREAM a bfdbBatch.dbacc  bfdbBatch.TrDate bfdbBatch.Ref
                         bfdbBatch.Descrip bfdbBatch.Sgrp bfdbBatch.Amt bfdbBatch.Vat WITH FRAME frm-rpt.
        DOWN STREAM a WITH FRAME frm-rpt.
        RELEASE dbblf.
END PROCEDURE.

PROCEDURE Report.ip:
    FOR EACH dbbatch WHERE dbBatch.intBatch = dbBCtr.intBatch:
         DISPLAY STREAM a dbBatch.Seq dbBatch.trDate dbBatch.dbacc dbBatch.Ref dbBatch.Sgrp dbBatch.Amt 
                          dbBatch.Vat dbBatch.DESCRIP dbBatch.Iledger WITH FRAME frmRpt.
         DOWN STREAM a WITH FRAME frmRpt.
    END.
END PROCEDURE.

PROCEDURE EXP.ip:
IF CAN-FIND (FIRST dbbatch WHERE dbBatch.intBatch = dbBCtr.intBatch) THEN DO:
    EXPORT STREAM b DELIMITER ',' "BATCH: " + STRING(dbBCtr.intBatch).
    EXPORT STREAM b DELIMITER ',' "BATCH" "PERIOD" "SEQ" "DATE" "ACCOUNT" "REF" "SERVIRCE" "AMOUNT" "VAT" "LEDGER" "NARRATION" "PROJECT" "FUND" "DEPT".
    FOR EACH dbbatch WHERE dbBatch.intBatch = dbBCtr.intBatch:
         EXPORT STREAM b DELIMITER ','  dbBCtr.intBatch dbBCtr.period dbBatch.Seq dbBatch.trDate dbBatch.dbacc dbBatch.Ref dbBatch.Sgrp dbBatch.Amt 
                          dbBatch.Vat dbBatch.Iledger dbBatch.DESCRIP dbBatch.Proj dbBatch.Fund dbBatch.Dept.
 
    END.
END.
END PROCEDURE.
