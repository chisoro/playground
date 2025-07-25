SESSION:DATA-ENTRY-RETURN = TRUE.
/* Program.................payexp.p
    Notes:...... ..........Export Payroll Costing to Promun
    Author:.................S. Mawire
*/
DEF SHARED VAR varUser       LIKE simusr.usercode.
DEF VAR wsSys         LIKE db004.paysys.paysys.
DEF VAR wsAmt         AS DEC FORM "zzz,zzz,zz9.99-".
DEF VAR wsDr          LIKE wsAmt.
DEF VAR wsCr          LIKE wsAmt.
DEF VAR wsLedger      LIKE db004.paycost.Ledger.
DEF VAR wsdes         AS CHAR FORM "x(40)".
DEF VAR st-per        AS INT FORM "999999".
DEF VAR wsdate        AS DATE.
DEF VAR wsTime        AS CHAR FORM "x(8)".
DEF VAR wsRate    LIKE tblforex.decRate.
DEF VAR wsprefix AS INT.
DEF VAR X AS INT. 
DEF VAR wsbatch AS INT.
DEF VAR wsopt AS INT view-as combo-box size 20 by 1 list-item-pairs
      "Current",1,"Historical",2.
DEF VAR wsDay   AS INT FORM "99".
DEF VAR wsMonth AS INT FORM "99".
DEF VAR wsYear  AS INT FORM "9999".
DEF VAR wsdays   AS INT FORM 99 EXTENT 12
        INITIAL [31,28,31,30,31,30,31,31,30,31,30,31].

 DEF BUTTON btn-Sys   LABEL "Payroll System".
 DEF BUTTON btn-exit  LABEL "CLOSE".
 DEF BUTTON btn-close LABEL "CLOSE".
 DEF BUTTON btn-ok    LABEL "EXPORT".

 DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 90 by 7.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 90 by 2.5.
DEF RECT rec SIZE 116 BY 17.
DEF RECT rec1 SIZE 116 BY 2.5.

 DEF    QUERY qry-Sys FOR db004.paysys SCROLLING.
 DEF BROWSE brw-Sys QUERY qry-Sys
         DISPLAY db004.paysys.paysys db004.paysys.Descrip COLUMN-LABEL "Decsription" 
         WIDTH 60 WITH 20 DOWN SEPARATORS.

DEF    QUERY qryView FOR Paycost SCROLLING.
DEF BROWSE brwView QUERY qryView
        DISPLAY paycost.Proj paycost.Fund paycost.Dept paycost.Ledger paycost.empcode 
                paycost.Itemcode COLUMN-LABEL "Code" wsdes COLUMN-LABEL "Description"
                paycost.Amount COLUMN-LABEL "Amount" WITH 20 DOWN SEPARATORS.
 
 DEFINE FRAME frm-View 
     brwView AT ROW 2 COL 5
     skip(1)
     btn-ok colon 20 LABEL "EXPORT"
     btn-close colon 80
     rec AT ROW 1.5 COL 3
     rec1 AT ROW 18.7 COL 3
     with view-as dialog-box keep-tab-order no-validate
     SIZE 121 BY 22
          side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Payroll Costing Data".

 DEFINE FRAME frm-pick 
     brw-Sys AT ROW 2 COL 5
     skip(0.5)
     btn-ok colon 5 LABEL "OK"
     btn-close colon 60
     with view-as dialog-box keep-tab-order no-validate
          side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Payroll System Selection".

 DEFINE FRAME frm-paysys
     SKIP(1.5)
     btn-Sys   COLON 10 NO-LABEL NO-TAB-STOP
     wsSys              NO-LABEL AUTO-RETURN 
     db004.paysys.Descrip     NO-LABEL FORM "x(30)" VIEW-AS TEXT SKIP(0.5)
     wsOpt     COLON 26 LABEL "SELECTED PERIOD" SKIP(0.5)
     st-per    COLON 26 LABEL "PAYROLL PERIOD"  SKIP(0.5)
     wsBatch   COLON 25 LABEL "BATCH NUMBER" SKIP(1.5)
     btn-ok colon 20 LABEL "PROCESS"
     btn-close colon 60
     rect-1 AT ROW 1.5 COL 3
     rect-2 AT ROW 9 COL 3
     WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D  KEEP-TAB-ORDER
      SIZE 95 BY 12
     TITLE "PAYROLL COSTING PROMUN EXPORT " VIEW-AS DIALOG-BOX.


 /* Triggers for the db004.paysys frame */
 ON CHOOSE OF btn-sys IN FRAME frm-paysys
     OR CHOOSE OF btn-sys IN FRAME frm-paysys
 DO:
   VIEW FRAME frm-pick.
   OPEN QUERY qry-sys FOR EACH db004.paysys NO-LOCK.
   ENABLE ALL WITH FRAME frm-pick.
   WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pick 
           OR close of THIS-PROCEDURE IN FRAME frm-pick
           OR CHOOSE OF btn-ok IN FRAME frm-pick 
           OR 'enter':u OF brw-sys
           OR 'mouse-select-dblclick' OF brw-sys.
   CLOSE QUERY qry-sys.
   HIDE FRAME frm-pick.
   APPLY 'tab' TO SELF.
   APPLY 'tab' TO wsSys.
   RETURN. 
 END.

 ON CHOOSE OF btn-ok IN FRAME frm-pick 
     OR 'enter':u OF brw-sys
     OR 'mouse-select-dblclick' OF brw-sys
 DO: 
    GET CURRENT qry-sys NO-LOCK NO-WAIT.
    DISPLAY db004.paysys.paysys @ wsSys WITH FRAME frm-paysys.
    RETURN. 
 END.

ON  'enter':U OF wsSys IN FRAME frm-paysys
     OR 'tab':U OF wsSys IN FRAME frm-paysys
 DO:
    ASSIGN wsBatch.
     FIND FIRST db004.paysys WHERE db004.paysys.paysys = INT(wsSys:SCREEN-VALUE) NO-LOCK NO-ERROR.
     IF AVAILABLE db004.paysys THEN DO:
         st-per = INT(STRING(YEAR(db004.paysys.Curdate)) + STRING(MONTH(db004.paysys.Curdate),"99")).
         FIND FIRST tblforex WHERE tblforex.txtcur = db004.paysys.txtcur NO-LOCK NO-ERROR.
         IF AVAILABLE tblforex AND simctr.LdCur <> db004.paysys.txtcur THEN
             wsRate = tblForex.decRate.
         ELSE wsRate = 1.
         FIND FIRST simusr WHERE simusr.usercode = varUser NO-LOCK NO-ERROR.
         IF AVAILABLE simusr AND (simusr.paysys = 99 OR simusr.paysys = INT(wsSys:SCREEN-VALUE)) THEN
         DO:
           DISPLAY  db004.paysys.paysys @ wsSys db004.paysys.Descrip st-per WITH FRAME frm-paysys.
         END.
         ELSE DO:
             MESSAGE "You are not allowed to access this Payroll...Please try again" VIEW-AS ALERT-BOX.
             RETURN NO-APPLY.
         END.
     END.  
     ELSE DO:
         MESSAGE "Invalid Payroll System...Please try again" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
     END.
     APPLY 'entry' TO wsopt IN FRAME frm-paysys.
     RETURN.
 END.

 ON 'enter':U OF wsopt IN FRAME frm-paysys
     OR 'leave':U OF wsopt IN FRAME frm-paysys
 DO:
     ASSIGN wsopt.
     IF wsopt = 1 THEN DO:
         DISABLE st-per WITH FRAME frm-paysys.
         APPLY 'entry' TO wsBatch IN FRAME frm-paysys.
     END.
     ELSE DO:
           ENABLE st-per WITH FRAME frm-paysys.
           APPLY 'entry' TO st-per IN FRAME frm-paysys.
     END.
     RETURN.
 END.

 ON 'enter':U OF st-per IN FRAME frm-paysys 
     OR 'leave':U OF st-per IN FRAME frm-paysys 
 DO:
     ASSIGN st-per.
     APPLY 'entry' TO wsBatch IN FRAME frm-paysys.
     RETURN.
 END.

 ON 'choose':U OF btn-ok IN FRAME frm-paysys 
 DO:
     ASSIGN wsSys wsbatch st-per wsopt.
     FIND FIRST db004.paysys WHERE db004.paysys.paysys = INT(wsSys:SCREEN-VALUE) NO-LOCK NO-ERROR.
     IF AVAILABLE db004.paysys THEN DO:
         IF wsopt = 1 THEN
           ASSIGN  st-per = INT(STRING(YEAR(db004.paysys.Curdate)) + STRING(MONTH(db004.paysys.Curdate),"99"))
                   wsdate = db004.paysys.curdate.
         IF AVAILABLE simusr AND (simusr.paysys = 99 OR simusr.paysys = INT(wsSys:SCREEN-VALUE)) THEN
         DO:
           DISPLAY db004.paysys.paysys @ wsSys db004.paysys.Descrip st-per WITH FRAME frm-paysys.
           IF wsopt = 2 THEN
               RUN Recost.ip.
           RUN ProView.ip.
         END.
         ELSE DO:
             MESSAGE "You are not allowed to access this Payroll...Please try again" VIEW-AS ALERT-BOX.
             RETURN NO-APPLY.
         END.
     END.  
     ELSE DO:
         MESSAGE "Invalid Payroll System...Please try again" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
     END.
     RETURN.
 END.

 ON row-display OF brwView DO:
     FIND FIRST payitem WHERE payitem.itemcode = paycost.itemcode NO-LOCK NO-ERROR.
     wsDes = Payitem.Descrip.
 END.

 ON 'choose':U OF btn-ok IN FRAME frm-View 
 DO:
     X = 0.
     IF wsopt = 1 then
         wsdate = db004.paysys.CurDate.
     MESSAGE wsdate VIEW-AS ALERT-BOX.
         FIND LAST tblforexh WHERE tblforexh.txtcur =  db004.paysys.txtcur AND tblforexh.DtRate <= wsdate NO-LOCK NO-ERROR.
         IF AVAILABLE tblforexh AND simctr.LdCur <> db004.paysys.txtcur THEN
             wsRate = tblForexh.decRate.
         ELSE wsRate = 1.
              MESSAGE wsrate VIEW-AS ALERT-BOX.
         FIND FIRST simusr WHERE simusr.usercode = varUser NO-LOCK NO-ERROR.
    RUN report.ip.
    CREATE igltcf.
    ASSIGN igltcf.acc-per-mm = INT(SUBSTR(string(st-per),5,2))
            igltcf.acc-per-yy = INT(SUBSTR(string(st-per),1,4))
            igltcf.batch-no = wsbatch 
            igltcf.batch-status = "b"
            igltcf.batch-total = wscr * -1
            igltcf.source = "HR"
            igltcf.trn-date = wsdate
            igltcf.total-cr = wscr * -1
            igltcf.total-dr = wscr.
    HIDE FRAME frm-View.
    APPLY 'close' TO THIS-PROCEDURE.
 END.

 /********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
VIEW FRAME frm-paysys IN WINDOW CURRENT-WINDOW.
wsopt:SCREEN-VALUE = "1".
ENABLE ALL WITH FRAME frm-paysys.
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-paysys.
HIDE FRAME frm-paysys.
RETURN.


PROCEDURE ProView.ip: 
    FOR EACH paycost:
        ASSIGN wskey = STRING(paycost.proj) + STRING(paycost.fund) + STRING(paycost.dept) + STRING(paycost.Ledger).
    END.
    VIEW FRAME frm-View.
    ENABLE ALL WITH FRAME frm-View.
    OPEN QUERY qryView FOR EACH paycost WHERE paycost.paysys = wsSys NO-LOCK.
    WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-View.
    HIDE FRAME frm-View.
END PROCEDURE.

PROCEDURE Report.ip:
    FOR EACH paycost WHERE paycost.paysys = wsSys BREAK BY wskey:
        ACCUMULATE paycost.Amount(TOTAL BY wskey).
        IF LAST-OF(wskey) THEN DO:
            ASSIGN wsAmt = (ACCUM TOTAL BY wskey  paycost.Amount).
                   wsLedger = paycost.Ledger.
            IF wsAmt = 0 THEN NEXT.
            /* CREATE A COSTING LINE */
            CREATE igldcf.
            X = X + 1.
            ASSIGN igldcf.batch-no   = wsBatch
                   igldcf.acct-no    = STRING(wsLedger)
                   igldcf.rec-status = "b"
                   igldcf.seq-no     = X
                   igldcf.source     = "HR"
                   igldcf.REF        = "HR" + string(wsSys) + "/" + string(st-per)
                   igldcf.DESCRIP    = "HR CONSOLIDATION"
                   igldcf.amount     =  ROUND((wsAmt * wsRate),2)
                   igldcf.trn-DATE   = wsdate
                   wscr              = wscr + ROUND((wsAmt * wsRate),2).
        END.
        DELETE db004.paycost.
    END.
    /* CREATE CONTROL LINE */
    FIND FIRST Payctr NO-LOCK NO-ERROR.
    FIND FIRST glmf WHERE glmf.acct = Payctr.Ledger NO-LOCK NO-ERROR.
    ASSIGN  wsLedger = Payctr.Ledger.
    CREATE igldcf.
    X = X + 1.
    ASSIGN igldcf.batch-no    = wsBatch
           igldcf.acct-no     = STRING(wsLedger)
           igldcf.rec-status  = "b"
           igldcf.seq-no      = X
           igldcf.source      = "HR"
           igldcf.REF         = "HR" + string(wsSys) + "/" + string(st-per)
           igldcf.DESCRIP     = "HR CONSOLIDATION"
           igldcf.amount      = wscr * -1
           igldcf.trn-DATE    = wsDate.

    RETURN.
END PROCEDURE.

PROCEDURE Recost.ip:
    ASSIGN wsMonth = INT(SUBSTR(STRING(st-per),5,2))
           wsYear = INT(SUBSTR(STRING(st-per),1,4)).
    wsDay   = wsDays[wsMonth].
    IF wsMonth = 2 AND wsYear MOD 4 = 0 THEN
       wsDay = 29.
     ELSE IF wsMonth = 2 AND wsYear MOD 4 <> 0 THEN
            wsDay = 28.
     wsDate = DATE(wsMonth,wsDay,wsYear).
    /* Delete Costing data */
    FOR EACH paycost WHERE paycost.paysys = wsSys: /*clean costing data */
        DELETE paycost.
    END.
    /* Payroll costing data */
    FOR EACH payemf WHERE payemf.paysys = wsSys NO-LOCK:
       IF simctr.LForm = 1 THEN DO:
            FIND FIRST db004.paypost WHERE db004.PayPost.Post = db004.payemf.Post NO-LOCK NO-ERROR.
            FIND FIRST db004.paydept WHERE db004.paydept.dept = payemf.dept NO-LOCK NO-ERROR.
            IF AVAILABLE db004.paydept THEN
               wsprefix = INT(STRING(db004.Paypost.proj) + STRING(db004.Paypost.fund) + STRING(db004.paydept.prefix)).
        END.
        ELSE IF simctr.LForm = 2 THEN DO:
            FIND FIRST db004.paypost WHERE db004.PayPost.Post = db004.payemf.Post NO-LOCK NO-ERROR.
            wsprefix = 0.
        END. 
        FOR EACH db004.payhtf WHERE db004.payhtf.empcode = db004.payemf.empcode AND db004.Payhtf.trDate = wsdate NO-LOCK:
            IF db004.payhtf.CurAmt = 0 THEN NEXT.
            ELSE DO:
                FIND FIRST payitem WHERE payitem.itemcode = payhtf.itemcode NO-LOCK NO-ERROR.
                IF db004.Payitem.cat = 1 OR db004.Payitem.cat = 3 THEN /*Income and Acrual costing */
                    wsLedger = DEC(TRIM(STRING(wsprefix)) + TRIM(STRING(db004.Payitem.Suffix,"9999"))).
                ELSE wsLedger = 0.
                IF wsLedger <> 0 THEN DO: /* Payroll costing data */
                CREATE db004.paycost.
                ASSIGN db004.paycost.empcode  = db004.payemf.empcode
                       db004.paycost.itemcode = db004.payitem.itemcode
                       db004.paycost.ledger   = wsLedger
                       db004.paycost.Proj     = db004.paypost.proj
                       db004.paycost.Fund     = db004.paypost.fund
                       db004.paycost.dept     = db004.payemf.dept
                       db004.paycost.Amount   = db004.payhtf.CurAmt
                       db004.paycost.paysys   = wsSys.
                END.
            END.
        END.
    END.
END PROCEDURE.
