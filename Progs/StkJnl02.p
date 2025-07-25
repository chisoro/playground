/* Program.................Stkjnl02.p
   Notes:................. Stores Journal Update
   Author:.................S. Mawire
*/
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "Landscape"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10.0          NO-UNDO.
DEF SHARED VAR varUser LIKE simusr.usercode.
DEF VAR wsTransId      LIKE gltdf.TransID.
DEF VAR wsTime      AS CHAR FORM "x(8)".
DEF VAR wsper LIKE simctr.curper.
DEF VAR wsyear     AS INT.
DEF VAR wsmonth     AS INT.
DEF VAR wsDate     LIKE stkbctr.BDate.
DEF VAR wsOp       LIKE stkbctr.UID.
DEF VAR wsBatch    LIKE stkbtf.intBatch.
DEF VAR X AS INT.
DEF VAR wsQuant AS DEC FORM "zzzzzz9.99-".
DEF VAR wsTotal LIKE wsQuant.
DEF VAR wsTCr LIKE wsQuant.
DEF VAR wsTDr LIKE wsQuant.
DEF VAR wsDr   LIKE wsQuant.
DEF VAR wsCr   LIKE wsQuant.
DEF VAR wsCost LIKE wsQuant.
DEF VAR wsLedger LIKE glmf.acct.
DEF VAR CostLedger LIKE glmf.acct.
DEF VAR wsSource LIKE dbgl.SOURCE INITIAL "ST".

DEF BUTTON Btn-Print   LABEL "Print".
DEF BUTTON btn-Update   LABEL "Update".
DEF BUTTON btn-Exit    LABEL "Close".
DEF BUFFER bfstkbtf FOR stkbtf.

DEFINE QUERY qry-stkbctr FOR  stkbctr scrolling.
DEF BROWSE brw-stkbctr QUERY qry-stkbctr
        DISPLAY intBatch        COLUMN-LABEL "BATCH"
                period WIDTH 10 COLUMN-LABEL "PERIOD"
                BDate WIDTH 12  COLUMN-LABEL "BATCH DATE"
                QUANTITY        COLUMN-LABEL "QUANTITY" 
                COST            COLUMN-LABEL "COST"
                TranType        COLUMN-LABEL "TYPE"
        WITH 20 DOWN SEPARATORS.

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 graphic-edge  NO-FILL
     SIZE 80 BY 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 80 BY 17.5.
FIND FIRST SIMCTR NO-LOCK NO-ERROR.
FIND FIRST simusr WHERE simusr.usercode = varUser NO-LOCK NO-ERROR.
FORM
     bfstkbtf.Stkcode    LABEL "STOCK"
     bfstkbtf.TrDate     LABEL "DATE"
     bfstkbtf.Ref        LABEL "REFERENCE"
     bfstkbtf.Descrip    LABEL "DESCRIPTION"
     bfstkbtf.Quantity   LABEL "QUANTITY"
     bfstkbtf.Cost       LABEL "COST"
    HEADER skip(1) "STORES JOURNAL UPDATE REPORT FOR BATCH: " TRIM(STRING(wsBatch)) 
     "Page: " AT 90 PAGE-NUMBER(a) SKIP(1) 
    "DATE: " wsDate "PERIOD  :" wsPer  " Batch Captured by:" simusr.Name SKIP(2)
    WITH NO-LABEL DOWN STREAM-IO FONT 8 WIDTH 132 CENTERED NO-BOX FRAME frm-rpt.

FORM dbgl.Proj             LABEL "PROJECT"
     dbgl.dept             LABEL "DEPARTMENT" 
     dbgl.acct             LABEL "ACCOUNT"
     glmf.DESCRIPTION      LABEL "DESCRIPTION"
     wsDr                  LABEL "DEBITS" FORM "zz,zzz,zz9.99"
     wsCr                  LABEL "CREDITS" FORM "zz,zzz,zz9.99-"
     HEADER skip(1) "STORES  JOURNAL UPDATE CONSOLIDATION REPORT FOR BATCH: " wsBatch 
    "DATE: " wsDate  "PERIOD  :" wsPer  "Page: " AT 90 PAGE-NUMBER SKIP(1) 
    "        Batch Captured by:" simusr.Name SKIP(2) 
    WITH NO-LABEL DOWN STREAM-IO FONT 8 WIDTH 132 CENTERED NO-BOX FRAME frm-lrpt.

DEF FRAME frm-main
    brw-stkbctr AT ROW 2 COL 8
    btn-Update AT ROW 19.7 COL 15 SPACE(40) 
    btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 19 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
    SIZE 90 BY 23
    TITLE "STORES  JOURNAL UPDATE" view-as dialog-box.

/******* Triggers ***** */

ON CHOOSE OF btn-Update IN FRAME frm-main 
    OR 'mouse-select-dblclick' OF brw-stkbctr
DO:
    GET CURRENT qry-stkbctr EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsDate = stkbctr.BDate
           wsBatch = stkbctr.intBatch
           wsPer = stkbctr.period
           wsMonth = INT(SUBSTR(string(wsPer),5,2))
           wsYear  = INT(SUBSTR(string(wsPer),1,4)).
    /* Select print */
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="Update-ip"
                    &paged}
   CLOSE QUERY qry-stkbctr.
   OPEN QUERY qry-stkbctr FOR EACH stkbctr NO-LOCK.
   APPLY 'ENTRY' TO btn-exit IN FRAME frm-main.   
END.

/********** MAIN LOGIC **********/
wsTime    = STRING(TIME,"HH:MM:SS").
wsTransID = DEC (string(YEAR(TODAY),"9999")
          + string(MONTH(TODAY),"99" )
          + string(DAY(TODAY),"99")
          + SUBSTR(STRING(wsTIME),1,2) 
          + SUBSTR(STRING(wsTIME),4,2) 
          + SUBSTR(STRING(wsTIME),7,2) ).
OPEN QUERY qry-stkbctr FOR EACH stkbctr NO-LOCK.
ENABLE ALL  EXCEPT WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
CLOSE QUERY qry-stkbctr.
HIDE FRAME frm-main.

PROCEDURE UPDATE-ip:
DO TRANSACTION ON ERROR UNDO, LEAVE:
FOR EACH bfstkbtf WHERE bfstkbtf.TrDate = wsDate AND bfstkbtf.IntBatch = wsBatch:
    FIND FIRST Stkmf WHERE Stkmf.Stkcode = bfstkbtf.Stkcode EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST stkwhf WHERE stkwhf.WareHse = stkmf.WareHse NO-LOCK NO-ERROR.
    CREATE Stktrans.
    ASSIGN Stktrans.Period   = wsPer
           Stktrans.trDate   = bfstkbtf.TrDate
           Stktrans.Stkcode  = bfstkbtf.Stkcode
           Stktrans.DESCRIP  = bfstkbtf.descrip
           Stktrans.Quantity =  bfstkbtf.Quantity
           Stktrans.Cost     =  bfstkbtf.Cost
           stktrans.UnitPrice = ROUND((bfstkbtf.Cost / bfstkbtf.Quantity),2)
           Stktrans.PoDATE   = TODAY
           Stktrans.prog     = "Stkjnl02.p"
           Stktrans.Ref      =  bfstkbtf.Ref
           Stktrans.TranType = stkbctr.TranType
           Stktrans.TransID  = wsTransId
           Stktrans.Ledger   = bfstkbtf.Ledger
           Stktrans.dept     = bfstkbtf.dept
           Stktrans.proj     = bfstkbtf.proj
           Stktrans.UID      = stkbctr.UID
           Stktrans.UID2     = VarUser.
    ASSIGN Stkmf.Quantity[2] = bfstkbtf.Quantity
           Stkmf.cost[2]     = bfstkbtf.Cost
           stkmf.UnitPrice   =  ROUND(((Stkmf.cost[1] + Stkmf.cost[2]) / (Stkmf.Quantity[1] + Stkmf.Quantity[2])),2)
           stkmf.Cost[1]     = stkmf.Cost[1] + bfstkbtf.Cost
           Stkmf.Quantity[1] = Stkmf.Quantity[1]+ bfstkbtf.Quantity
           . 
    DISPLAY STREAM a bfstkbtf.Stkcode  bfstkbtf.TrDate bfstkbtf.Ref
                     bfstkbtf.Descrip bfstkbtf.Quantity bfstkbtf.Cost WITH FRAME frm-rpt.
    DOWN STREAM a WITH FRAME frm-rpt.
    /* Create GL consolidation data */
    IF stkbctr.TranType = 2 THEN DO:
        DO X = 1 TO 2:
            wsCost = 0.
            IF X = 1 THEN DO:
                FIND FIRST dbgl WHERE dbgl.acct = bfstkbtf.Ledger AND dbgl.period = wsper
                                 AND dbgl.fund = bfstkbtf.fund AND dbgl.proj = bfstkbtf.proj
                                 AND dbgl.dept =  bfstkbtf.Dept AND dbgl.TransID = wsTransId NO-ERROR.
                IF NOT AVAILABLE dbgl THEN DO:
                       CREATE dbgl.
                       ASSIGN dbgl.acct = bfstkbtf.Ledger
                              dbgl.dept = bfstkbtf.Dept
                              dbgl.proj = bfstkbtf.proj
                              dbgl.fund = bfstkbtf.fund
                              dbgl.period = wsper
                              dbgl.TransID = wsTransId
                              dbgl.trDATE  = wsDate
                              dbgl.UID2    = varUser
                              dbgl.UID     = stkbctr.UID
                              dbgl.REF     = "Stk" + STRING(bfstkbtf.intBatch)
                              dbgl.DESCRIP = "Online consolidation"
                              dbgl.CREDATE = TODAY
                              dbgl.SOURCE = wsSource.
                END.
                ASSIGN dbgl.AMT = dbgl.AMT + (bfstkbtf.Cost * -1).
            END.
                
            ELSE IF X = 2 THEN DO: /*warehouse control */
               wsCost    = bfstkbtf.Cost.
               FIND FIRST glmf WHERE glmf.acct = stkwhf.Ledger NO-LOCK NO-ERROR.
               FIND FIRST dbgl WHERE dbgl.acct = stkwhf.Ledger AND dbgl.period = wsper
                                 AND dbgl.fund = glmf.fund AND dbgl.proj = glmf.proj
                                 AND dbgl.dept =  glmf.dept AND dbgl.TransID = wsTransId NO-ERROR.
               IF NOT AVAILABLE dbgl THEN DO:
                       CREATE dbgl.
                       ASSIGN dbgl.acct = stkwhf.Ledger
                              dbgl.dept =glmf.Dept
                              dbgl.proj = glmf.proj
                              dbgl.fund = glmf.fund
                              dbgl.period = wsper
                              dbgl.TransID = wsTransId
                              dbgl.trDATE  = wsDate
                              dbgl.UID2    = varUser
                              dbgl.UID     = stkbctr.UID
                              dbgl.REF     = "Stk" + STRING(bfstkbtf.intBatch)
                              dbgl.DESCRIP = "Online consolidation"
                              dbgl.CREDATE = TODAY
                              dbgl.SOURCE = wsSource.
               END.
               ASSIGN dbgl.AMT = dbgl.AMT + bfstkbtf.Cost.
            END.
        END. /* DO 1 TO 2 */
    END. /* eof transType */
   DELETE bfstkbtf.  
END. /* eof FOR EACH */
DELETE stkbctr.
/* Consolidate to GL and Report */
{glcon.i}

END. /* eof Do Transaction.. */
RETURN.
END. /* of Procedure update.ip */
