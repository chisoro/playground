SESSION:DATA-ENTRY-RETURN = TRUE.
/* Program.................Recexp.p
    Notes:...... ..........Export Receipts to Promun
    Author:.................S. Mawire
*/
&SCOPED-DEFINE wsMsg           ""
&SCOPED-DEFINE wsTitle           "Unposted Receipts Maintenance"
&SCOPED-DEFINE tmptable           dbRecCtr
&SCOPED-DEFINE skey               dbRecCtr.usercode
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.usercode ~
                                     COLUMN-LABEL ' USER':C ~
                              bfr{&tmptable}.RecDate ~
                                     COLUMN-LABEL ' DATE ':C ~
                              bfr{&tmptable}.EOD ~
                                     COLUMN-LABEL ' EOD':C ~
                              bfr{&tmptable}.Validate ~
                                     COLUMN-LABEL 'VALIDATED ':C ~
                               bfr{&tmptable}.RecTotal ~
                                      FORM "zzz,zzz,zzz,zz9.99-" COLUMN-LABEL ' AMOUNT':C
DEF BUFFER bfr{&tmptable} FOR {&tmptable} PRESELECT .
DEF VAR wsDate AS DATE.
DEF VAR wsMcno AS INT.
DEF VAR wsRec  AS INT.
DEF VAR wsOp   AS CHAR.
DEF VAR wsName AS CHAR FORM "X(20)".
DEF VAR wsTotal AS DEC FORM "zz,zzz,zzz,zzz,zz9.99-".
DEF VAR wsuser  LIKE wsop.

DEF BUTTON btn-close LABEL "CLOSE".
DEF BUTTON btn-ok    LABEL "OK".
DEF BUTTON btn-exp    LABEL "EXPORT TO Promun".

DEFINE RECTANGLE rec-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 90 by 2.3.

DEFINE RECTANGLE rec-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 90 by 6.3.
     
DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 100 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 100 by 14.

DEF QUERY qry-{&tmptable} FOR bfr{&tmptable} SCROLLING.
DEF BROWSE brw-{&tmptable} QUERY qry-{&tmptable}
    DISPLAY wsNAME LABEL "CASHIER" {&tmpFields}  wsTotal FORM "zz,zzz,zzz,zzz,zz9.99-"
    WITH 16 DOWN SEPARATORS  NO-BOX LABEL-FGCOLOR 9 LABEL-BGCOLOR 3 
    LABEL-FONT 6.
    
/*define frame frmMain
    brw-{&tmptable} AT ROW 1.8 COL 8
    btn-Exp   AT ROW 16 COL 10 SPACE(60)
    btn-close  
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 15.5 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     /*BGCOLOR 8 FGCOLOR 1 */ SIZE 105 BY 18.5
    TITLE "UNPOSTED RECEIPTS EXPORT TO PROMUN" VIEW-AS DIALOG-BOX. */

DEFINE FRAME frm-main
     SKIP(1.0)
     wsUser     COLON 25 LABEL "SimAcc Cashier code" SKIP(.2)
     wsDate     COLON 25 LABEL "Receipting Date" SKIP(.2)
     wsOp     COLON 25 LABEL "Operator Number" SKIP(.2)
     wsMcno   COLON 25 LABEL "Machine Number" SKIP(.2)
     wsRec    COLON 30 LABEL "Processing receipt...." VIEW-AS TEXT SKIP(1.3)
     btn-ok   colon 10 LABEL "EXPORT"
     btn-close colon 60
     rec-2 AT ROW 1.5 COL 3
     rec-1 AT ROW 8.0 COL 3
     WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
      SIZE 95 BY 13
     TITLE "RECEIPT EXPORT - SIMACC TO PROMUN " VIEW-AS DIALOG-BOX.

/*ON CHOOSE OF btn-exp IN FRAME frmMain 
DO:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    FIND FIRST dbRecCtr WHERE dbRecCtr.RecDate  = bfr{&tmptable}.RecDate
                          AND dbRecCtr.usercode  = bfr{&tmptable}.usercode NO-ERROR.
    ASSIGN wsDate = dbRecCtr.RecDate
           WsOp = dbRecCtr.usercode. 
    VIEW FRAME frm-Main IN WINDOW CURRENT-WINDOW.
    ENABLE ALL WITH FRAME frm-Main.
    WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-Main.
    HIDE FRAME frm-Main.
    RETURN.
   brw-{&tmptable}:REFRESH().
END.
*/
ON CHOOSE OF btn-ok IN FRAME frm-main 
DO: 
    ASSIGN wsMcno wsOp wsDate wsUser.
    CREATE rec.munrctctr.
    ASSIGN munrctctr.cash    = 999999999
           munrctctr.mcno    = wsMcno
           munrctctr.eff-date = TODAY 
           munrctctr.rec-date  = wsDate
           munrctctr.rec-status = "U".              .
    RUN Rec-ip. 
    MESSAGE "EXPORT COMPLETED....." VIEW-AS ALERT-BOX.
    APPLY 'CLOSE' TO THIS-PROCEDURE IN FRAME frmMain.
    RETURN NO-APPLY. 
 END.
 
/*ON row-display OF brw-{&tmptable} IN FRAME frmMain
DO:
    wsTotal = wsTotal + bfr{&tmptable}.RecTotal.
    FIND FIRST simusr WHERE simusr.usercode = bfr{&tmptable}.usercode NO-LOCK NO-ERROR.
    IF AVAILABLE simusr THEN
        wsName = simusr.NAME.
    ELSE
        wsName = "Invalid Operator".
END. */
   
/********** MAIN LOGIC **********/
 VIEW FRAME frm-Main IN WINDOW CURRENT-WINDOW.
    ENABLE ALL WITH FRAME frm-Main.
    WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-Main.
    HIDE FRAME frm-Main.
 RETURN.

 PROCEDURE Rec-ip:
     FOR EACH DBRECh  WHERE dbrech.recDate = wsDate AND dbRech.opCode =  wsUser:
         DISPLAY db003.dbRech.RecNo @ wsrec WITH FRAME frm-main.
         PAUSE 0.
         IF db003.dbRech.rCode = "LS" THEN NEXT. /*chipinge */
        CREATE munrct.
        ASSIGN
            rec.munrct.del-flag = YES WHEN db003.dbRech.RecStat = "C" 
            rec.munrct.rec-name = db003.dbRech.Descrip
            rec.munrct.recno = db003.dbRech.RecNo
            rec.munrct.seq-no = db003.dbRech.SeqNo
            rec.munrct.eff-date = TODAY
            rec.munrct.rec-date = db003.dbRech.RecDate
            /*rec.munrct.acc      = 315006602  WHEN db003.dbRech.rCode = "LS" */ /* Redcliff on landsale only*/
            rec.munrct.acc      = db003.dbRech.Account 
            rec.munrct.amount   = (db003.dbRech.Amount * db003.dbRech.decRate)
            rec.munrct.code     = db003.dbRech.rCode
            rec.munrct.mcno     = wsMcno
            rec.munrct.opcode   = wsOp
            rec.munrct.rec-status = "U".    
        FIND FIRST dbpay WHERE dbpay.paytype = dbRech.Paytype NO-ERROR.
        IF SUBSTR(dbPay.descrip,1,4) = "CASH" THEN
            rec.munrct.paytype = "C".
        ELSE IF SUBSTR(dbPay.descrip,1,4) = "POS" THEN
            rec.munrct.paytype = "P".
        ELSE IF SUBSTR(dbPay.descrip,1,4) = "BTRF" THEN
            rec.munrct.paytype = "B".
        ELSE IF SUBSTR(dbPay.descrip,1,4) = "EcoC" THEN
            rec.munrct.paytype = "V".
        ELSE IF SUBSTR(dbPay.descrip,1,4) = "OneM" THEN
            rec.munrct.paytype = "Q".
        ELSE IF SUBSTR(dbPay.descrip,1,4) = "SET-" THEN
            rec.munrct.paytype = "Q".
        ELSE IF SUBSTR(dbPay.descrip,1,4) = "DISC" THEN
            rec.munrct.paytype = "Q".
        /*IF simctr.cocode = "CVF" AND db003.dbRech.txtCur = "USD" THEN
            RUN CreBatch.ip. */
    END.
 END PROCEDURE.
/*
 PROCEDURE creBatch.ip: /* City of Victoria Falls USD payment discount batch */
     wsBatch = INT(STRING(MONTH(munrct.rec-date)) + STRING(DAY(munrct.rec-date),"99") + STRING(wsMcno)).
     IF NOT CAN-FIND (FIRST muntcf WHERE  muntcf.batch-date = munrct.rec-date
                                      AND  muntcf.batch-no   = wsBatch 
                                      AND  muntcf.batch-status = "U"
                                      AND  muntcf.batch-type   = 1
                                      AND  muntcf.descrip      = "USD PAYMENT DISCOUNT") THEN DO:
         CREATE muntcf.
         ASSIGN muntcf.batch-date = munrct.rec-date
                muntcf.batch-no   = wsBatch 
                muntcf.batch-status = "U"
                muntcf.batch-type   = 2
                muntcf.descrip      = "USD PAYMENT DISCOUNT"
                muntcf.acc-pd       = DEC(STRING(YEAR(munrct.rec-date)) + STRING(MONTH(munrct.rec-date),"99")).
                muntcf.pass-code    = simusr.usercode.
     END.
     IF wsseq = 0 THEN DO:
         FIND LAST mundcf WHERE mundcf.tr-date    = munrct.rec-date
                            AND mundcf.batch-no   = wsBatch 
                            AND mundcf.Rec-status = "U"
                            AND mundcf.company    = 0
                            AND mundcf.trans-desc = "USD PAYMENT DISCOUNT" NO-LOCK NO-ERROR.
         IF AVAILABLE mundcf THEN
             wsSeq = mundcf.seq + 1.
         ELSE wsSeq = 1.
     END.
     FIND FIRST municf WHERE municf.CODE = db003.dbRech.rCode NO-LOCK NO-ERROR.
     CREATE mundcf.
     ASSIGN mundcf.tr-date    = munrct.rec-date
            mundcf.batch-no   = wsBatch 
            mundcf.Rec-status = "U"
            mundcf.company    = 0
            mundcf.trans-desc = "USD PAYMENT DISCOUNT"
            mundcf.seq        = wsSeq
            mundcf.acc        = db003.dbRech.Account
            mundcf.age        = 1
            mundcf.amt        = ROUND(((db003.dbRech.Amount * db003.dbRech.decRate) * 0.1),2) 
            mundcf.bmf-type   = municf.bmf-type
            mundcf.gl-alloc   = "1"
            mundcf.ref        = "USD-DISCOUNT"
            mundcf.vat-amt    = 0.
     wsSeq = wsSeq + 1.
 END PROCEDURE.
                */      
