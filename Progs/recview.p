session:DATA-ENTRY-RETURN = TRUE.
/* Program.................recview.p
   Notes:...... View and Amend unposted receipts
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsMsg           ""
&SCOPED-DEFINE wsTitle           "Unposted Receipts Maintenance"
&SCOPED-DEFINE tmptable           dbRecCtr
&SCOPED-DEFINE tmptable1          dbRec
&SCOPED-DEFINE skey               dbRecCtr.usercode

&SCOPED-DEFINE UpdFields      bfr{&tmptable1}.rCode ~
                                     COLUMN-LABEL ' INCOME ':C ~
                              bfr{&tmptable1}.Paytype ~
                                     COLUMN-LABEL ' PAY-TYPE ':C 

&SCOPED-DEFINE tmp1Field      bfr{&tmptable1}.RecNo  ~
                                     COLUMN-LABEL ' REC. NUMBER ':C ~
                              bfr{&tmptable1}.SeqNo ~
                                     COLUMN-LABEL ' SEQUENCE ':C ~
                              bfr{&tmptable1}.Account ~
                                     COLUMN-LABEL ' ACCOUNT ':C ~
                              bfr{&tmptable1}.rCode ~
                                     COLUMN-LABEL ' INCOME ':C ~
                              bfr{&tmptable1}.Paytype ~
                                     COLUMN-LABEL ' PAY-TYPE ':C ~
                              bfr{&tmptable1}.Amount ~
                                     FORM "zzzzzzzzz9.99-" ~
                                     COLUMN-LABEL ' AMOUNT ':C ~
                              bfr{&tmptable1}.Descrip ~
                                     COLUMN-LABEL ' DESCRIPTION ':C

&SCOPED-DEFINE tmpFields      bfr{&tmptable}.usercode ~
                                     COLUMN-LABEL 'USER ':C ~
                              bfr{&tmptable}.RecDate ~
                                     COLUMN-LABEL ' DATE ':C ~
                              bfr{&tmptable}.EOD ~
                                     COLUMN-LABEL ' EOD':C ~
                              bfr{&tmptable}.Validate ~
                                     COLUMN-LABEL 'VALIDATED ':C ~
                               bfr{&tmptable}.RecTotal ~
                                      FORM "zzz,zzz,zzz,zz9.99-" COLUMN-LABEL ' AMOUNT':C 
                                            

session:DATA-ENTRY-RETURN = TRUE.
DEF NEW SHARED STREAM a.
DEF NEW SHARED VAR wsDate      LIKE dbRec.RecDate.
DEF NEW SHARED VAR wsOp        LIKE dbRec.OpCode.
DEF NEW SHARED VAR wsValid     AS LOGICAL INITIAL YES.
DEF NEW SHARED VAR wsStatus   AS CHAR FORM "X(20)".
DEF NEW SHARED VAR wsVar       AS CHAR FORM "X(20)".
DEF NEW SHARED VAR wsamt      AS DEC  FORM "ZZZZZZZZZZ9.99-".
DEF NEW SHARED VAR wsVoid     AS DEC  FORM "ZZZZZZZZZZ9.99-".
DEF NEW SHARED VAR wsTotal   AS DEC  LABEL "TOTAL" FORM "ZZZZZZZZZZ9.99-".
DEF NEW SHARED VAR w-orientation AS CHAR      INITIAL "Landscape"    NO-UNDO.
DEF NEW SHARED VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10.0          NO-UNDO.
DEF SHARED VAR varUser LIKE simusr.usercode.
DEF VAR wsTransId      LIKE gltdf.TransID.
DEF VAR wsTime      AS CHAR FORM "x(8)".
DEF  VAR wsper LIKE simctr.curper.
DEF  VAR wsyear     AS INT.
DEF VAR wsmonth     AS INT.
DEF VAR rdbRate  LIKE  tblForex.decRate.
DEF VAR X AS INT.
DEF VAR Current-Record AS ROWID.
DEF VAR method-status AS LOGICAL.
DEF VAR wsid LIKE {&skey}.
DEF VAR wsCashier   AS CHAR FORM "x(80)".
DEF VAR wsName      AS CHAR FORM "x(20)".
DEF VAR varTotal     LIKE DbRec.Amount FORM "zzzzzzzzz9.99-".
DEF VAR wsTitle1    LIKE simctr.CONAME.
DEF NEW SHARED VAR wsTitle AS CHAR FORM "x(80)".
DEF BUTTON btn-Amend    LABEL "AMEND".
DEF BUTTON btn-del    LABEL "DELETE".
DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-Edit   LABEL "EDIT".
DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "OK".
DEF BUTTON btn-upd    LABEL "UPDATE".
DEF BUTTON btn-cancel LABEL "CANCEL".
DEF BUTTON btn-Valid    LABEL "VALIDATE".
DEF BUTTON btn-Exp    LABEL "EXPORT".
DEF BUTTON btn-void    LABEL "VOID".
DEF BUTTON btn-eod    LABEL "EOD".
DEF NEW SHARED BUFFER bfdbRec FOR dbRec.
DEF BUFFER bfr{&tmptable} FOR {&tmptable} PRESELECT .
DEF BUFFER bfr{&tmptable1} FOR {&tmptable1} PRESELECT.

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
DEF QUERY qry-{&tmptable1} FOR bfr{&tmptable1} SCROLLING.
DEF BROWSE brw-{&tmptable1} QUERY qry-{&tmptable1}
    DISPLAY {&tmp1Field} 
    WITH 16 DOWN SEPARATORS  NO-BOX LABEL-FGCOLOR 9 LABEL-BGCOLOR 3 
    LABEL-FONT 6.

define frame frmMain
    brw-{&tmptable} AT ROW 1.8 COL 10
    btn-valid AT ROW 16 COL 10 SPACE(5)
    btn-Upd   SPACE(5) 
    btn-Amend SPACE(6) 
    btn-Exp   SPACE(6) 
    btn-Eod   SPACE(6) 
    btn-close SPACE(6) 
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 15.5 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     /*BGCOLOR 8 FGCOLOR 1 */ SIZE 105 BY 18.5
    TITLE "UNPOSTED RECEIPTS LISTING AND AMENDMENT" VIEW-AS DIALOG-BOX.

define frame frmAmend
    SKIP(0.5)
    {&skey}    colon 10 label "Operator " VIEW-AS TEXT
    simusr.NAME  FORM "x(40)" VIEW-AS TEXT NO-LABEL 
     bfr{&tmptable}.RecDate LABEL "Receipting Date" VIEW-AS TEXT SKIP(1)
    brw-{&tmptable1} COLON 5
    skip(0.5)
    btn-Edit colon 20 SPACE(30) btn-void SPACE(30) btn-exit
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "UNPOSTED RECEIPTS LISTING AND AMENDMENT".

DEF FRAME frmEdit
     bfr{&tmptable1}.RecNo    COLON 20 SKIP(0.5)
     bfr{&tmptable1}.SeqNo    COLON 20 SKIP(0.5)
     bfr{&tmptable1}.contype  COLON 20 SKIP(0.5)
     bfr{&tmptable1}.Account  COLON 20 SKIP(0.5)
     bfr{&tmptable1}.rCode    COLON 20 SKIP(0.5)
     bfr{&tmptable1}.Paytype  COLON 20 SKIP(0.5)
     bfr{&tmptable1}.Amount   COLON 20 SKIP(0.5)
     bfr{&tmptable1}.Ref      COLON 20 SKIP(0.5)
     bfr{&tmptable1}.Descrip  COLON 20 SKIP(0.5)
     btn-ok COLON 10 SPACE(30) btn-cancel
     with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "RECEIPTS AMENDMENT".

FORM
    dbPay.descrip LABEL "DESCRIPTION" wsAmt LABEL "AMOUNT" 
     HEADER "EOD" SKIP
     wsTitle1  SKIP
     wsCashier SKIP
     wsTitle SKIP(1)
    
    "------COLLECTION SUMMARY------"SKIP(1)
    WITH NO-LABEL DOWN STREAM-IO FONT 10 CENTERED NO-BOX FRAME frmEod.
    
/* ***** Triggers for the main frame **** */
ON CHOOSE OF btn-Upd IN FRAME frmMain 
DO:
    /*CHECK SUPERVISOR */
    FIND FIRST simusr WHERE simusr.usercode = VarUser NO-LOCK NO-ERROR.
    IF simusr.supervisor <> YES THEN DO:
        MESSAGE "Youe are not allowed to Update receipts" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    FIND FIRST dbRecCtr WHERE dbRecCtr.RecDate  = bfr{&tmptable}.RecDate
                          AND dbRecCtr.usercode  = bfr{&tmptable}.usercode NO-ERROR.
    ASSIGN wsDate = dbRecCtr.RecDate
           WsOp = dbRecCtr.usercode.
    RUN Recup01.p.
    wsTotal = 0.
    brw-{&tmptable}:REFRESH().
    /*MESSAGE "This option is disabled" VIEW-AS ALERT-BOX. */
    RETURN NO-APPLY.
END.

ON CHOOSE OF btn-valid IN FRAME frmMain 
DO:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    FIND FIRST dbRecCtr WHERE dbRecCtr.RecDate  = bfr{&tmptable}.RecDate
                          AND dbRecCtr.usercode  = bfr{&tmptable}.usercode NO-ERROR.
    ASSIGN wsDate = dbRecCtr.RecDate
           WsOp = dbRecCtr.usercode. 
   RUN recva01.p.
   wsTotal = 0.
   brw-{&tmptable}:REFRESH().
END.

ON 'choose':U OF btn-eod IN FRAME frmMain  
DO:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    FIND FIRST dbRecCtr WHERE dbRecCtr.RecDate  = bfr{&tmptable}.RecDate
                          AND dbRecCtr.usercode  = bfr{&tmptable}.usercode NO-ERROR.
    ASSIGN dbRecCtr.eod = YES.
    FIND FIRST dbrecop WHERE dbRecop.RecDate  = bfr{&tmptable}.RecDate
                        AND  dbRecop.usercode = bfr{&tmptable}.usercode NO-ERROR.
    IF AVAILABLE dbrecop AND dbRecop.EOD = NO THEN
       dbRecop.EOD = YES.
     {PrintOpt.i &stream-name="stream a"
                    &print-prog="eod.ip"
                    &paged}               
    brw-{&tmptable}:REFRESH().
    RETURN.
END.

ON CHOOSE OF btn-exp IN FRAME frmMain 
DO:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    FIND FIRST dbRecCtr WHERE dbRecCtr.RecDate  = bfr{&tmptable}.RecDate
                          AND dbRecCtr.usercode  = bfr{&tmptable}.usercode NO-ERROR.
    ASSIGN wsDate = dbRecCtr.RecDate
           WsOp = dbRecCtr.usercode. 
   RUN recva01x.p.
   wsTotal = 0.
   brw-{&tmptable}:REFRESH().
END.

ON CHOOSE OF btn-Amend IN FRAME frmMain 
    OR 'mouse-select-dblclick' OF brw-{&tmptable}
DO:
    /*CHECK SUPERVISOR */
    FIND FIRST simusr WHERE simusr.usercode = VarUser NO-LOCK NO-ERROR.
    IF simusr.supervisor <> YES THEN DO:
        MESSAGE "Youe are not allowed to amend receipts" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    FIND FIRST dbRecCtr WHERE dbRecCtr.RecDate  = bfr{&tmptable}.RecDate
                          AND dbRecCtr.usercode  = bfr{&tmptable}.usercode NO-ERROR.
    ASSIGN wsDate = {&tmptable}.RecDate
           WsOp = {&tmptable}.usercode.
    IF dbRecCtr.Validate = YES THEN DO:
      /* MESSAGE "Validation done, Receipts cannot be amended" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY. */
         dbRecCtr.Validate = NO.
    END. 
    FIND FIRST simusr WHERE simusr.usercode = wsOp NO-LOCK NO-ERROR.
    /* Receipt Amendment */
    HIDE FRAME frmMain.
    VIEW FRAME frmAmend.
    DISPLAY {&skey} simusr.NAME bfr{&tmptable}.RecDate WITH FRAME frmAmend.
    OPEN QUERY qry-{&tmptable1} FOR EACH bfr{&tmptable1} 
        WHERE bfr{&tmptable1}.recDate = wsDate AND bfr{&tmptable1}.OpCode = wsOp
        BY bfr{&tmptable1}.RecNo BY bfr{&tmptable1}.SeqNo.
    ENABLE ALL WITH FRAME frmAmend.
    WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frmAmed.
    CLOSE QUERY qry-{&tmptable1}.
    HIDE FRAME frmAmend.
    VIEW FRAME frmMain.
    wsTotal = 0.
    brw-{&tmptable}:REFRESH().
END.

ON CHOOSE OF btn-Void IN FRAME frmAmend 
DO:
    GET CURRENT qry-{&tmptable1} EXCLUSIVE-LOCK NO-WAIT.
    IF   btn-Void:LABEL = "VOID" THEN DO:
        FOR EACH dbRec WHERE dbRec.RecDate  = bfr{&tmptable1}.RecDate
                     AND dbRec.OpCode  = wsOp
                      AND dbRec.RecNo   =  bfr{&tmptable1}.RecNo:
    
        ASSIGN dbRec.RecStat = "C".
        END.
        MESSAGE "Receipt cancelled...." VIEW-AS ALERT-BOX.
        btn-Void:LABEL = "UnDo VOID".
    END.
    ELSE IF btn-Void:LABEL = "UnDo VOID" THEN DO:
        FOR EACH dbRec WHERE dbRec.RecDate  = bfr{&tmptable1}.RecDate
                     AND dbRec.OpCode  = wsOp
                      AND dbRec.RecNo   =  bfr{&tmptable1}.RecNo:
        ASSIGN dbRec.RecStat = "".
        END.
        MESSAGE "Receipt uncancelled...." VIEW-AS ALERT-BOX.
        btn-Void:LABEL = "VOID" .
    END.
END.
ON 'choose':U OF btn-Cancel IN FRAME frmEdit
DO:
    APPLY 'close' TO THIS-PROCEDURE.
    RETURN.
END.
ON CHOOSE OF btn-Edit IN FRAME frmAmend 
DO:
   GET CURRENT qry-{&tmptable1} EXCLUSIVE-LOCK NO-WAIT.
   VIEW FRAME frmEdit.
   DISPLAY bfr{&tmptable1}.RecNo 
           bfr{&tmptable1}.SeqNo
           bfr{&tmptable1}.contype 
           bfr{&tmptable1}.Account 
           bfr{&tmptable1}.rCode 
           bfr{&tmptable1}.Paytype 
           bfr{&tmptable1}.Amount FORM "ZZZZZZZZZ9.99-"
           bfr{&tmptable1}.Ref 
           bfr{&tmptable1}.Descrip WITH FRAME frmEdit.
   ENABLE bfr{&tmptable1}.Account bfr{&tmptable1}.contype bfr{&tmptable1}.rCode bfr{&tmptable1}.Paytype  btn-Ok btn-Cancel WITH FRAME frmEdit.
   WAIT-FOR CHOOSE OF btn-Ok  OR CHOOSE OF btn-Cancel OR close of THIS-PROCEDURE IN FRAME frmEdit.
   HIDE FRAME frmEdit.
   brw-{&tmptable1}:REFRESH().
END.

ON 'tab':U OF bfr{&tmptable1}.account IN FRAME frmEdit
    OR 'enter':U OF bfr{&tmptable1}.account IN FRAME frmEdit
DO:
    IF  bfr{&tmptable1}.contype:SCREEN-VALUE = "C" THEN DO:
        FIND FIRST dbcmf WHERE dbcmf.dbAcc = DEC(bfr{&tmptable1}.account:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF NOT AVAILABLE dbcmf THEN DO:
            MESSAGE "Invalid Consumer Account for the transaction type" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
        ELSE DO:
            ASSIGN bfr{&tmptable1}.Account:SCREEN-VALUE = STRING(dbcmf.dbAcc)
                   bfr{&tmptable1}.DESCRIP:SCREEN-VALUE = dbcmf.Name.
             DISABLE bfr{&tmptable1}.DESCRIP WITH FRAME frmEdit.
            APPLY 'tab' TO SELF.
        END.
    END.
    ELSE IF  bfr{&tmptable1}.contype:SCREEN-VALUE = "L" THEN DO:
        FIND FIRST hsecmf WHERE hsecmf.dbAcc = DEC(bfr{&tmptable1}.account:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF NOT AVAILABLE hsecmf THEN DO:
            MESSAGE "Invalid Landsales Account for the transaction type" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
        ELSE DO:
            ASSIGN bfr{&tmptable1}.Account:SCREEN-VALUE = STRING(hsecmf.dbAcc)
                   bfr{&tmptable1}.DESCRIP:SCREEN-VALUE = Hsecmf.Name.
             DISABLE bfr{&tmptable1}.DESCRIP WITH FRAME frmEdit.
            APPLY 'tab' TO SELF.
        END.
    END.
    ELSE IF  bfr{&tmptable1}.contype:SCREEN-VALUE = "V" THEN DO:
        FIND FIRST glmf WHERE glmf.acct = DEC(bfr{&tmptable1}.account:SCREEN-VALUE) NO-LOCK NO-ERROR.
        IF NOT AVAILABLE glmf THEN DO:
            MESSAGE "Invalid Ledger for the transaction type" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
        ELSE DO:
            ASSIGN bfr{&tmptable1}.Account:SCREEN-VALUE = STRING(glmf.acct).
            ENABLE bfr{&tmptable1}.DESCRIP WITH FRAME frmEdit.
            APPLY 'tab' TO SELF.
        END.
    END.  
    RETURN.
END.

ON 'tab':U OF bfr{&tmptable1}.rCode IN FRAME frmEdit
    OR 'enter':U OF bfr{&tmptable1}.rCode IN FRAME frmEdit
    OR 'LEAVE':U OF bfr{&tmptable1}.rCode IN FRAME frmEdit
DO:
    FIND FIRST dbRCod WHERE dbRCod.rCode = bfr{&tmptable1}.rCode:SCREEN-VALUE
                        AND dbRCod.TARGET = bfr{&tmptable1}.contype:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF NOT AVAILABLE dbRcod THEN DO:
        MESSAGE "Invalid Income code for the transaction type" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        ASSIGN bfr{&tmptable1}.rCode:SCREEN-VALUE = STRING(dbRCod.rcode).
        APPLY 'tab' TO SELF.
    END.
    RETURN.
END.

ON 'tab':U OF  bfr{&tmptable1}.Paytype IN FRAME frmEdit
  OR  'enter':U OF  bfr{&tmptable1}.Paytype IN FRAME frmEdit
  OR  'LEAVE':U OF  bfr{&tmptable1}.Paytype IN FRAME frmEdit
DO:
    FIND FIRST dbPay WHERE dbPay.Paytype = INT(bfr{&tmptable1}.Paytype:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE dbPay THEN DO:
        MESSAGE "Invalid Payment method" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        FIND FIRST cbkmf WHERE cbkmf.bank = dbPay.bank NO-LOCK NO-ERROR.
        FIND FIRST tblForex WHERE tblForex.txtCur = cbkmf.txtCur AND wsDate >= tblForex.DtRate  NO-LOCK NO-ERROR.
        IF AVAILABLE tblForex THEN DO:
              ASSIGN rdbRate = tblForex.decRate.
        END.
        ELSE IF NOT AVAILABLE tblForex  THEN DO:
           FIND LAST tblForexH WHERE tblForexH.txtCur = cbkmf.txtCur AND tblForexH.dtRate <= wsDate NO-LOCK NO-ERROR.
           IF AVAILABLE tblForexH THEN DO:
               ASSIGN rdbRate = tblForexH.decRate.
            END.
            ELSE IF NOT AVAILABLE tblForexH  THEN DO:
                FIND FIRST tblForexH WHERE  tblForexH.txtCur = cbkmf.txtCur AND tblForexH.dtRate >= wsDate NO-LOCK NO-ERROR.
                IF AVAILABLE tblForexH THEN DO:
                 ASSIGN rdbRate = tblForexH.decRate.
               END.
            END.
        END.

        ASSIGN bfr{&tmptable1}.Paytype:SCREEN-VALUE = STRING(dbPay.Paytype).
        APPLY 'tab' TO SELF.
    END.
    RETURN.
END.

ON 'choose':U OF btn-Ok IN FRAME frmEdit
DO:
    ASSIGN bfr{&tmptable1}.Account = DEC(bfr{&tmptable1}.Account:SCREEN-VALUE)
           bfr{&tmptable1}.rCode   = bfr{&tmptable1}.rCode:SCREEN-VALUE
           bfr{&tmptable1}.DESCRIP = bfr{&tmptable1}.DESCRIP:SCREEN-VALUE
           bfr{&tmptable1}.contype = bfr{&tmptable1}.contype:SCREEN-VALUE
            bfr{&tmptable1}.RecStat = "A".
    IF bfr{&tmptable1}.Paytype <> INT(bfr{&tmptable1}.Paytype:SCREEN-VALUE) THEN
        FOR EACH bfr{&tmptable1} WHERE bfr{&tmptable1}.RecNo = INT(bfr{&tmptable1}.RecNo:SCREEN-VALUE)
                                   AND bfr{&tmptable1}.OpCode = WsOp
                                   AND bfr{&tmptable}.RecDate = wsDate:
           ASSIGN bfr{&tmptable1}.Paytype = INT(bfr{&tmptable1}.Paytype:SCREEN-VALUE)
                  bfr{&tmptable1}.txtCur = cbkmf.txtCur
                  bfr{&tmptable1}.decRate = rdbRate.
        END.
    RETURN.
END.

ON row-display OF brw-{&tmptable} IN FRAME frmMain
DO:
    wsTotal = wsTotal + bfr{&tmptable}.RecTotal.
    FIND FIRST simusr WHERE simusr.usercode = bfr{&tmptable}.usercode NO-LOCK NO-ERROR.
    IF AVAILABLE simusr THEN
        wsName = simusr.NAME.
    ELSE
        wsName = "Invalid Operator".
END.
   
ON 'mouse-select-click':U OF brw-{&tmptable1}  
DO:
    GET CURRENT qry-{&tmptable1} NO-WAIT.
    FIND FIRST dbRec WHERE dbRec.RecDate  = bfr{&tmptable1}.RecDate
                      AND dbRec.OpCode  = wsOp
                      AND dbRec.RecNo   =  bfr{&tmptable1}.RecNo NO-ERROR.  
    IF dbRec.RecStat = "C" THEN
        btn-Void:LABEL = "UnDo VOID".
    ELSE
        btn-Void:LABEL = "VOID" .
    RETURN.
END.


/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
wsTitle = simctr.CONAME + "   Report Date: " + STRING(TODAY).
OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK .
ENABLE ALL WITH FRAME frmMain.
WAIT-FOR CHOOSE OF btn-close IN FRAME frmMain OR close of THIS-PROCEDURE IN FRAME frmMain.
CLOSE QUERY qry-{&tmptable}.
HIDE FRAME frmMain.

PROCEDURE eod.ip.
   FIND FIRST simusr WHERE simusr.usercode = bfr{&tmptable}.usercode  NO-LOCK NO-ERROR.
    ASSIGN wsTitle  = "Operator: " + bfrDbRecCtr.usercode + " " + simusr.Name
           wsTitle1 = "Receipt DATE: " + STRING(bfrDbRecCtr.recDate).
    ASSIGN wsAmt = 0
           wsvoid = 0
           wsTotal = 0.
    FOR EACH dbPay:
        wsAmt = 0.
        FOR EACH bfrDbRec WHERE bfrDbRec.OpCode = bfrDbRecCtr.usercode AND bfrDbRec.RecDate = bfrDbRecCtr.RecDate
            AND bfrDbRec.Paytype = dbPay .Paytype:
            ASSIGN wsAmt  = wsAmt + bfrDbRec.Amount    WHEN bfrDbRec.RecStat <> "C"
                   wsVoid = wsVoid + bfrDbRec.Amount * bfrDbRec.decrate  WHEN bfrDbRec.RecStat = "C" 
                   wsTotal = wsTotal + bfrDbRec.Amount * bfrDbRec.decrate  WHEN bfrDbRec.RecStat <> "C".
        END.
        IF wsAmt <> 0 THEN DO:
            FIND FIRST cbkmf WHERE cbkmf.bank = dbPay.bank NO-LOCK NO-ERROR.
            DISPLAY STREAM a dbPay.descrip wsAmt /*cbkmf.descrip */ WITH FRAME frmEod.
            DOWN STREAM a WITH FRAME frmEod.
        END.
    END.
    UNDERLINE STREAM a dbPay.descrip wsAmt WITH FRAME frmEod.
    DOWN STREAM a WITH FRAME frmEod.
    DISPLAY STREAM a "Total Collections" @ dbPay.descrip wsTotal @ wsAmt WITH FRAME frmEod.
    DOWN STREAM a WITH FRAME frmEod.
    DISPLAY STREAM a "Voids" @ dbPay.descrip wsVoid @ wsAmt WITH FRAME frmEod.
    DOWN  2 STREAM a WITH FRAME frmEod.
    DISPLAY STREAM a "Collections by Currency"  @ dbPay.descrip WITH FRAME frmEod.
    DOWN STREAM a WITH FRAME frmEod.
    ASSIGN wsAmt = 0
           wsVoid = 0
           wsTotal = 0.
    FOR EACH tblforex:
        IF NOT CAN-FIND(FIRST bfrDbRec WHERE  bfrDbRec.txtCur = tblForex.txtCur) THEN NEXT.
        ASSIGN wsAmt = 0
               wsVoid = 0.
        FOR EACH bfrDbRec WHERE bfrDbRec.OpCode = bfrDbRecCtr.usercode AND bfrDbRec.RecDate = bfrDbRecCtr.RecDate
            AND bfrDbRec.txtCur = tblForex.txtCur:
            ASSIGN wsAmt  = wsAmt + bfrDbRec.Amount  WHEN bfrDbRec.RecStat <> "C"
                   wsVoid = wsVoid + bfrDbRec.Amount WHEN bfrDbRec.RecStat = "C" .
        END.
        IF wsAmt <> 0 THEN DO:
            DISPLAY STREAM a tblForex.txtCur @ dbPay.descrip wsAmt /*"Voids: " + STRING(wsVoid) @ cbkmf.descrip */
             WITH FRAME frmEod.
            DOWN STREAM a WITH FRAME frmEod.
        END.
    END.
END.
