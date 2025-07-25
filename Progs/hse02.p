/* Program.................hse02.p
   Notes:...... Land sale Master file maintenance
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.

&SCOPED-DEFINE tmptable           hsecmf
&SCOPED-DEFINE skey               hsecmf.dbacc
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.dbacc ~
                                        COLUMN-LABEL ' ACCOUNT ':C ~
                              bfr{&tmptable}.NAME ~
                                            WIDTH 40 COLUMN-LABEL ' NAME ':C ~
                              bfr{&tmptable}.Regid ~
                                        COLUMN-LABEL 'ID/CO !REGISTRATION ':C ~
                               bfr{&tmptable}.scheme ~
                                        COLUMN-LABEL ' SCHEME ':C ~
                              bfr{&tmptable}.Suburb ~
                                        COLUMN-LABEL 'SURBUB ':C ~
                              bfr{&tmptable}.PPrice ~
                                        COLUMN-LABEL ' PURCHASE PRICE ':C ~
                               bfr{&tmptable}.Bal ~
                                        COLUMN-LABEL ' CAPITAL BALANCE ':C ~
                              bfr{&tmptable}.AmtDue[1] ~
                                        FORM "ZZZZ,ZZZ,ZZZ,ZZ9.99-" COLUMN-LABEL ' AMOUNT DUE':C ~

{varlibrary.i}
DEF VAR a AS INT.
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "landscape"    NO-UNDO.
DEF VAR  wsTitle AS CHAR FORM "x(80)".
DEF VAR wsSource  AS CHAR INITIAL "HS".
DEF VAR wsTransId AS DEC FORM "99999999999999".
DEF VAR wsdate    AS DATE.
DEF VAR wsTime    AS CHAR FORM "x(8)".
DEF VAR st-per     AS INT FORM "999999".
DEF VAR wsAmt     LIKE hsecmf.bal.
DEF VAR wsVat     LIKE hsecmf.bal.
DEF VAR wsTotal   LIKE wsAmt.
DEF VAR wsTVat    LIKE wsAmt.
DEF VAR wsDr      LIKE wsAmt.
DEF VAR wsCr      LIKE wsAmt.
DEF VAR wsTDr     LIKE wsAmt.
DEF VAR wsTCr     LIKE wsAmt.
DEF VAR wsP       LIKE wsAmt.
DEF VAR wsn       AS INT.
DEF VAR wsr       AS   DEC FORM "zzz9.999999999-".
DEF VAR wsInst    LIKE wsAmt.
DEF VAR wsRate    AS DEC EXTENT 2.
DEF VAR wsdes     AS CHAR FORM "x(12)".
DEF  VAR wsLedger LIKE hsesch.CLedger.
DEF VAR X AS INT.

/*
DEF BUTTON  btnSearch LABEL "Search".
DEF BUTTON btn-Add    LABEL "ADD".
DEF BUTTON btn-del    LABEL "DELETE".
DEF BUTTON btn-exit   LABEL "CLOSE".
DEF BUTTON btn-Edit   LABEL "EDIT".
DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "SAVE".


DEF BUTTON btn-acc    LABEL "Account". */
DEF BUTTON btn-Scheme LABEL "Scheme".
DEF BUTTON btn-StandNo LABEL "STAND NUMBER".
DEF BUTTON btn-Sub    LABEL "Suburb".
DEF BUTTON btn-Ward   LABEL "Ward".

DEFINE QUERY qry-PickWard FOR dbwrd scrolling.
DEF BROWSE brw-PickWard QUERY qry-PickWard
    DISPLAY  dbwrd.Ward dbwrd.Descrip
    WITH 8 DOWN SEPARATORS.


DEFINE QUERY qry-PickStand FOR bfr{&tmptable} scrolling.
DEF BROWSE brw-PickStand QUERY qry-PickStand
    DISPLAY  bfr{&tmptable}.scheme bfr{&tmptable}.standno
    WITH 8 DOWN SEPARATORS.

DEFINE QUERY qry-PickSuburb FOR dbsmf scrolling.
DEF BROWSE brw-PickSuburb QUERY qry-PickSuburb
    DISPLAY  dbsmf.suburb dbsmf.Descrip
    WITH 8 DOWN SEPARATORS.

DEF    QUERY qry-pickScheme FOR hsesch SCROLLING.
DEF BROWSE brw-pickScheme QUERY qry-pickScheme
        DISPLAY hsesch.scheme hsesch.Descrip 
         WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-PickStand 
    brw-PickStand AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5 LABEL "OK"
    btn-close colon 20
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Stand Selection".

DEFINE FRAME frm-pickScheme 
        brw-pickScheme AT ROW 2 COL 5
        skip(0.5)
        btn-ok colon 5
        btn-close colon 60
        with view-as dialog-box keep-tab-order no-validate
             side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Scheme Selection".

define RECTANGLE rec-1
     edge-pixels 2 graphic-edge  no-fill
     size 150 by 2.3.

define RECTANGLE rec-2
     edge-pixels 2 graphic-edge  NO-FILL
     size 150 by 19.8.

define RECTANGLE rec-3
     edge-pixels 2 graphic-edge  NO-FILL
     size 99 by 19.5.
define RECTANGLE rec-4
     edge-pixels 2 graphic-edge  NO-FILL
     size 75 by 19.5.

define RECTANGLE rec-5
     edge-pixels 2 graphic-edge  NO-FILL
     size 60 by 8.

define RECTANGLE rec-6
     edge-pixels 2 graphic-edge  NO-FILL
     size 174 by 2.3.

FORM dbgl.Fund             LABEL "SEGMENT"
     dbgl.Proj             LABEL "PROJECT"
     dbgl.dept             LABEL "DEPARTMENT"
     dbgl.acct             LABEL "ACCOUNT"
     glmf.DESCRIPTION      LABEL "DESCRIPTION" FORM "x(50)"
     wsDr                  LABEL "DEBITS" 
     wsCr                  LABEL "CREDITS" 
     HEADER skip(1) "ONLINE JOURNAL CAPTURE AND UPDATE CONSOLIDATION REPORT - DATE: " wsDate  "PERIOD  :" st-per
       SKIP(1)  
     WITH NO-LABEL DOWN STREAM-IO FONT 8 WIDTH 132 CENTERED NO-BOX FRAME frm-lrpt.

DEF FRAME Search-opt
    wsSearch  COLON 20 LABEL "Search Value"
    btnSearch
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED.


define frame frm-input
    SKIP (0.5)
    "-------------PROPERTY DETAILS ---------------" COLON 20 SKIP
    btn-Scheme          COLON 10 bfr{&tmptable}.Scheme NO-LABEL hsesch.Descrip NO-LABEL VIEW-AS TEXT SKIP(.2)
    btn-StandNo   COLON 10  bfr{&tmptable}.standNo  NO-LABEL   
    bfr{&tmptable}.DeedNo       COLON 77.5    LABEL "Deed Number" SKIP(.2)
    bfr{&tmptable}.SIZE         COLON 20 LABEL "Stand Size"
    bfr{&tmptable}.sitevalue     LABEL "    Value Site" 
    bfr{&tmptable}.bldvalue      LABEL "Building" SKIP(.2)
    btn-Sub            COLON 11 NO-TAB-STOP 
    bfr{&tmptable}.Suburb       NO-LABEL
    dbsmf.Descrip      NO-LABEL VIEW-AS TEXT SKIP(.2)
    btn-Ward           COLON 12.5 NO-TAB-STOP 
    bfr{&tmptable}.Ward         NO-LABEL 
    dbwrd.Descrip      NO-LABEL VIEW-AS TEXT SKIP(1)

     "------------------ACCOUNT DETAILS ------------------" COLON 10 SKIP
    bfr{&tmptable}.dbacc        colon 20 LABEL "Account" 
    bfr{&tmptable}.Ref          COLON 77.5  LABEL "Reference" SKIP(.2)
    bfr{&tmptable}.NAME         COLON 20 LABEL "Account Name"
    bfr{&tmptable}.Sortname     COLON 77.5 LABEL "Sort Name"SKIP(.2)
    bfr{&tmptable}.RegID        COLON 20 LABEL "ID/Co. Reg no"
    bfr{&tmptable}.RegID1       COLON 40 NO-LABEL
    bfr{&tmptable}.VatNo        COLON 77.5 LABEL "VAT Registration" SKIP(.2)
    bfr{&tmptable}.Add1         colon 20 label "Address"
    bfr{&tmptable}.Add2         colon 20 NO-LABEL
    bfr{&tmptable}.Add3         colon 20 NO-LABEL SKIP(.2)
    bfr{&tmptable}.cell[1]      COLON 20    LABEL "Cell Number" SPACE(2)  bfr{&tmptable}.cell[2] NO-LABEL SKIP(.2)
    bfr{&tmptable}.emailAdd     COLON 20    LABEL "Email Address" SKIP(1)

    

    "-------------   PURCHASE DETAILS   ---------------" AT ROW 1.5 COL 110 SKIP(0.5)
    hsesch.txtCur COLON 120 LABEL "Currency"  FORM "!!!!!!!!"  VIEW-AS TEXT
    bfr{&tmptable}.PRate            LABEL "Rate" VIEW-AS TEXT  SKIP(0.5)
    bfr{&tmptable}.PDate  COLON 120 LABEL "Purchase Date" SKIP(.2)
    bfr{&tmptable}.PPrice COLON 120 LABEL "Purchase Price" SKIP(.2)
    bfr{&tmptable}.Vat    COLON 120 LABEL "VAT %"  skip(.2)
    bfr{&tmptable}.DAmt   COLON 120 LABEL "Required Deposit" skip(.2)
    bfr{&tmptable}.Period COLON 120 LABEL "Period  "
    wsdes                   NO-LABEL VIEW-AS TEXT
    hsesch.IntRate          LABEL "Interest Rate" VIEW-AS TEXT skip(.2)
    bfr{&tmptable}.Bal    COLON 120 LABEL "Capital Balance"  VIEW-AS TEXT SKIP(.2)
    bfr{&tmptable}.InsAmt COLON 120 LABEL "Instalment" VIEW-AS TEXT SKIP(.5)
    bfr{&tmptable}.AmtDue[1] COLON 130  LABEL "Trading Amount Due" VIEW-AS TEXT hsesch.txtCur NO-LABEL VIEW-AS TEXT SKIP(.2)
    bfr{&tmptable}.AmtDue[2] COLON 130  LABEL "Accounting  Amount Due" VIEW-AS TEXT  FGCOLOR 12 SKIP(1)
    bfr{&tmptable}.LTDate  COLON 130 LABEL "Last Transaction Date" VIEW-AS TEXT SKIP(.2)
    bfr{&tmptable}.LTAmt   COLON 130 LABEL "Last Transaction Amount" VIEW-AS TEXT SKIP(.2)
    bfr{&tmptable}.LTRate  COLON 130 LABEL "Last Transaction Rate" VIEW-AS TEXT SKIP(.5)

    "-------------   ACCOUNTING DATES   ---------------" COLON 110
    st-per            COLON 130  LABEL "Accounting Period" SKIP(.2)
    wsDate           COLON 130  LABEL "Transaction Date" SKIP(0.5)
    bfr{&tmptable}.comnt   AT ROW 21 COL 5  LABEL "COMMENTS" VIEW-AS EDITOR SIZE 132 BY 2 
    btn-ok  AT ROW 23.6  COL 30
    btn-close AT ROW 23.6  COL 120
    rec-3 AT ROW 1.4 COL 3
    rec-4 AT ROW 1.4 COL 102
    rec-6 AT ROW 23  COL 3
    with view-as dialog-box keep-tab-order no-validate
       side-labels no-underline three-d SCROLLABLE SIZE 180 BY 26
     TITLE "LAND SALES ACCOUNT MAINTENANCE".
    
DEF FRAME frm-Main
    brw-{&tmptable} AT ROW 2 COL 6
    btn-add AT ROW 21.7 COL 20
    Space(25) btn-edit
    space(25) btn-del
    space(25) btn-exit SKIP(1)
    rec-2 AT ROW 1.4 COL 3
    rec-1 AT ROW 21.3 COL 3
    with view-as dialog-box keep-tab-order no-validate
     NO-LABEL no-underline three-d SCROLLABLE SIZE 155 BY 25 CENTERED
    TITLE "LAND SALES ACCOUNT MAINTENANCE".

DEF    QUERY qry-Ward FOR dbwrd SCROLLING.
DEF BROWSE brw-Ward QUERY qry-Ward
    DISPLAY dbwrd.Ward dbwrd.Descrip WIDTH 60 WITH 20 DOWN SEPARATORS.
DEFINE FRAME frm-Ward 
    brw-Ward AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5 LABEL "OK"
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Ward Selection".

DEF    QUERY qry-Sub FOR dbsmf SCROLLING.
DEF BROWSE brw-Sub QUERY qry-Sub
    DISPLAY dbsmf.suburb dbsmf.Descrip WIDTH 60 WITH 20 DOWN SEPARATORS.
DEFINE FRAME frm-Sub 
    brw-Sub AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5 LABEL "OK"
    btn-close colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Suburb Selection".

ON ERROR ANYWHERE 
DO:
    APPLY 'entry' TO bfr{&tmptable}.dbacc IN FRAME frm-input.
    RETURN NO-APPLY.
END.

ON CHOOSE OF btn-Scheme IN FRAME frm-Input
DO:
  VIEW FRAME frm-pickScheme.
  OPEN QUERY qry-pickScheme FOR EACH hsesch NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickScheme.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pickScheme 
          OR close of THIS-PROCEDURE IN FRAME frm-pickScheme 
          OR CHOOSE OF btn-ok IN FRAME frm-pickScheme 
          OR 'enter':u OF brw-pickScheme
          OR 'mouse-select-dblclick' OF brw-pickScheme.
  CLOSE QUERY qry-pickScheme.
  HIDE FRAME frm-pickScheme.
  APPLY 'tab' TO  bfr{&tmptable}.Scheme.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-PickScheme 
    OR 'enter':u OF brw-PickScheme
    OR 'mouse-select-dblclick' OF brw-PickScheme
DO: 
   GET CURRENT qry-PickScheme NO-LOCK NO-WAIT.
   DISPLAY hsesch.Scheme @ bfr{&tmptable}.Scheme WITH FRAME frm-input.
   CLOSE QUERY qry-pickScheme.
  HIDE FRAME frm-pickScheme.
  APPLY 'tab' TO bfr{&tmptable}.Scheme.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-PickStand 
    OR 'enter':u OF brw-PickStand
    OR 'mouse-select-dblclick' OF brw-PickStand
DO: 
   GET CURRENT qry-PickStand NO-LOCK NO-WAIT.
   DISPLAY bfr{&tmptable}.StandNo WITH FRAME frm-input.
   CLOSE QUERY qry-pickStand.
  HIDE FRAME frm-pickStand.
  APPLY 'tab' TO bfr{&tmptable}.StandNo.
  RETURN. 
END.

ON  'enter':u OF bfr{&tmptable}.Scheme IN FRAME  frm-Input
     OR 'tab':u OF bfr{&tmptable}.Scheme IN FRAME  frm-Input
DO:
   FIND FIRST hsesch WHERE hsesch.Scheme = INT(bfr{&tmptable}.Scheme:SCREEN-VALUE) NO-LOCK NO-ERROR.
   IF NOT AVAILABLE hsesch THEN DO:
      MESSAGE "Project does not exist, please try again" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   ELSE  DO:
       FIND FIRST tblForex WHERE tblForex.txtCur = hsesch.txtCur NO-LOCK NO-ERROR.
       wsRate[1] = tblForex.DecRate.
       IF hsesch.Freq = "M"  THEN
          wsdes = "Monthly".
       IF hsesch.Freq = "Q"  THEN
          wsdes = "Quarterly".
       IF hsesch.Freq = "H"  THEN
          wsdes = "Half-Yearly".
       IF hsesch.Freq = "Y"  THEN
          wsdes = "Yearly".
       DISPLAY hsesch.Scheme @ bfr{&tmptable}.Scheme hsesch.Descrip hsesch.txtcur tblForex.DecRate @ bfr{&tmptable}.PRate 
               wsdes hsesch.IntRate WITH FRAME frm-input.
   END.   
   RETURN.    
END.

ON  CHOOSE OF btn-StandNo IN FRAME  frm-Input
DO:
  VIEW FRAME frm-pickStand.
  OPEN QUERY qry-pickStand FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.accStat <> 0 AND bfr{&tmptable}.scheme = int(bfr{&tmptable}.scheme:SCREEN-VALUE) NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickStand.
   WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pickStand 
          OR close of THIS-PROCEDURE IN FRAME frm-pickStand 
          OR CHOOSE OF btn-ok IN FRAME frm-pickStand 
          OR 'enter':u OF brw-pickStand
          OR 'mouse-select-dblclick' OF brw-pickStand.
  CLOSE QUERY qry-pickStand.
  HIDE FRAME frm-pickStand.
  APPLY 'tab' TO  bfr{&tmptable}.StandNo.
  RETURN. 
END.

ON 'enter':U OF bfr{&tmptable}.StandNo  
    OR 'tab':U OF bfr{&tmptable}.StandNo 
    OR 'leave':U OF bfr{&tmptable}.StandNo
DO:
    FIND FIRST bfr{&tmptable} WHERE  bfr{&tmptable}.Scheme = INT(bfr{&tmptable}.Scheme:SCREEN-VALUE)
         AND bfr{&tmptable}.StandNo = bfr{&tmptable}.StandNo:SCREEN-VALUE
         AND bfr{&tmptable}.Accstat   <> 0  NO-LOCK  NO-ERROR.
    IF AVAILABLE bfr{&tmptable} THEN DO:
        FIND FIRST dbsmf WHERE dbsmf.suburb = bfr{&tmptable}.Suburb NO-LOCK NO-ERROR.
        FIND FIRST dbwrd WHERE dbwrd.Ward = bfr{&tmptable}.Ward NO-LOCK NO-ERROR.
        ASSIGN bfr{&tmptable}.DeedNo:SCREEN-VALUE = STRING(bfr{&tmptable}.DeedNo)
                bfr{&tmptable}.SIZE:SCREEN-VALUE  =  string(bfr{&tmptable}.SIZE)     
                bfr{&tmptable}.sitevalue:SCREEN-VALUE = string(bfr{&tmptable}.sitevalue)  
                bfr{&tmptable}.bldvalue:SCREEN-VALUE  = string(bfr{&tmptable}.bldvalue)
                bfr{&tmptable}.Suburb:SCREEN-VALUE    = string(bfr{&tmptable}.Suburb)    
                dbsmf.Descrip:SCREEN-VALUE    =  dbsmf.Descrip  
                bfr{&tmptable}.Ward:SCREEN-VALUE    =     string(bfr{&tmptable}.Ward)   
                dbwrd.Descrip:SCREEN-VALUE =    dbwrd.Descrip
                bfr{&tmptable}.dbacc:SCREEN-VALUE   =   string(bfr{&tmptable}.dbacc)
                bfr{&tmptable}.pprice:SCREEN-VALUE  = STRING(bfr{&tmptable}.pprice)
                bfr{&tmptable}.vat:SCREEN-VALUE  = STRING(bfr{&tmptable}.vat).
        APPLY "tab" TO bfr{&tmptable}.Suburb IN FRAME frm-input.
        APPLY "enter" TO bfr{&tmptable}.Ward IN FRAME frm-input.
        
    END.
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.Scheme = INT(bfr{&tmptable}.Scheme:SCREEN-VALUE)
         AND bfr{&tmptable}.StandNo = bfr{&tmptable}.StandNo:SCREEN-VALUE
         AND bfr{&tmptable}.Accstat   = 0 NO-LOCK  NO-ERROR. 
       IF AVAILABLE bfr{&tmptable} THEN
       DO:
          MESSAGE "Stand Number " bfr{&tmptable}.StandNo:SCREEN-VALUE " OF Project  "
               hsesch.Descrip " is already allocated"
              VIEW-AS ALERT-BOX.
          bfr{&tmptable}.StandNo:SCREEN-VALUE IN FRAME frm-input = "".
          APPLY "entry" TO SELF IN FRAME  frm-Input.
          RETURN NO-APPLY.
       END.
     RETURN.
END.

ON 'enter':U OF bfr{&tmptable}.PDate 
    OR 'leave':U OF bfr{&tmptable}.PDate 
DO:
    FIND first tblForex WHERE tblForex.txtCur = hsesch.txtCur AND DATE(bfr{&tmptable}.PDate:SCREEN-VALUE) >= tblForex.DtRate  NO-LOCK NO-ERROR.
    IF AVAILABLE tblForex THEN DO:
        wsRate[1] = tblForex.decRate.
    END.    
    ELSE IF NOT AVAILABLE tblForex THEN DO:
       FIND LAST tblForexH WHERE tblForexH.txtCur = hsesch.txtCur AND tblForexH.dtRate <= DATE(bfr{&tmptable}.PDate:SCREEN-VALUE) NO-LOCK NO-ERROR.
             wsRate[1] = tblForexH.decRate.
    END.
    ELSE IF NOT AVAILABLE tblforexh THEN DO:
        MESSAGE "No Currency rate for that date, system may not have been in place..." VIEW-AS ALERT-BOX.
        LEAVE.
    END.
    bfr{&tmptable}.PRate:SCREEN-VALUE = STRING(wsRate[1]).
    wsDate = DATE(bfr{&tmptable}.PDate:SCREEN-VALUE).
    st-per = DEC(STRING(YEAR(wsDate)) + STRING(MONTH(wsDate),"99")).
    DISPLAY st-per wsDate WITH FRAME frm-Input.
    RETURN.
END.

ON 'enter':U OF st-per
    OR 'tab':U OF st-per
DO:
   IF INT(st-per:SCREEN-VALUE) <= SIMCTR.CLOSEPER
      OR INT(st-per:SCREEN-VALUE) > SIMCTR.CURPER THEN DO:
      MESSAGE "Invalid Accounting period entered" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
  END.
    RETURN.
END.

ON 'TAB':U OF wsDate IN FRAME frm-Input
   OR 'enter':U OF wsDate IN FRAME frm-Input
     OR 'leave':U OF wsDate IN FRAME frm-Input
DO:
    IF INT( STRING(YEAR(DATE(wsDate:SCREEN-VALUE))) + STRING(MONTH(DATE(wsDate:SCREEN-VALUE)),"99") ) <= SIMCTR.CLOSEPER
        OR INT( STRING(YEAR(DATE(wsDate:SCREEN-VALUE))) + STRING(MONTH(DATE(wsDate:SCREEN-VALUE)),"99") ) > SIMCTR.CURPER
         THEN DO:
        MESSAGE "Your date is outside the Accounting Periods...Please try again." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'choose':U OF btn-ok IN FRAME frm-input 
DO: 
    wsDate = DATE(bfr{&tmptable}.PDate:SCREEN-VALUE).
    st-per = DEC(STRING(YEAR(wsDate)) + STRING(MONTH(wsDate),"99")).
    IF INT(bfr{&tmptable}.scheme:SCREEN-VALUE) = 0  THEN DO:
        MESSAGE "Scheme cannot be zero...." VIEW-AS ALERT-BOX.
        APPLY 'entry' TO bfr{&tmptable}.scheme IN FRAME frm-Input.
        RETURN NO-APPLY.
    END.
    IF INT( bfr{&tmptable}.StandNo:SCREEN-VALUE) = 0  THEN DO:
        MESSAGE "Invalid Stand Number...." VIEW-AS ALERT-BOX.
        APPLY 'entry' TO  bfr{&tmptable}.StandNo IN FRAME frm-Input.
        RETURN NO-APPLY.
    END.
     IF DEC(bfr{&tmptable}.dbacc:SCREEN-VALUE) = 0  THEN DO:
        MESSAGE "Invalid Account number...." VIEW-AS ALERT-BOX.
        APPLY 'entry' TO  bfr{&tmptable}.dbAcc IN FRAME frm-Input.
        RETURN NO-APPLY.
    END.
    IF bfr{&tmptable}.NAME:SCREEN-VALUE = ""  THEN DO:
        MESSAGE "Account Name cannot be blank...." VIEW-AS ALERT-BOX.
        APPLY 'entry' TO  bfr{&tmptable}.NAME IN FRAME frm-Input.
        RETURN NO-APPLY.
    END.
     IF bfr{&tmptable}.sortname:SCREEN-VALUE = ""  THEN DO:
        MESSAGE "Invalid SortName...." VIEW-AS ALERT-BOX.
        APPLY 'entry' TO  bfr{&tmptable}.Sortname IN FRAME frm-Input.
        RETURN NO-APPLY.
    END.
    IF bfr{&tmptable}.RegId:SCREEN-VALUE = ""  THEN DO:
        MESSAGE "At least one registration number required..." VIEW-AS ALERT-BOX.
        APPLY 'entry' TO  bfr{&tmptable}.RegId IN FRAME frm-Input.
        RETURN NO-APPLY.
    END.
     IF bfr{&tmptable}.Add1:SCREEN-VALUE = ""  THEN DO:
        MESSAGE "At least one address line required..." VIEW-AS ALERT-BOX.
        APPLY 'entry' TO  bfr{&tmptable}.Add1 IN FRAME frm-Input.
        RETURN NO-APPLY.
    END.
    IF  bfr{&tmptable}.Ref:SCREEN-VALUE = ""  THEN DO:
        MESSAGE "Referance number cannot be blank..." VIEW-AS ALERT-BOX.
        APPLY 'entry' TO  bfr{&tmptable}.ref IN FRAME frm-Input.
        RETURN NO-APPLY.
    END.
    IF  DEC(bfr{&tmptable}.PPrice:SCREEN-VALUE) <= 0  THEN DO:
        MESSAGE "Purchase Price must be greater than zero..." VIEW-AS ALERT-BOX.
        APPLY 'entry' TO   bfr{&tmptable}.PPrice IN FRAME frm-Input.
        RETURN NO-APPLY.
    END.
    IF  INT(bfr{&tmptable}.Period:SCREEN-VALUE) <= 0  THEN DO:
        MESSAGE "Period should be at least 1..." VIEW-AS ALERT-BOX.
        APPLY 'entry' TO  bfr{&tmptable}.period IN FRAME frm-Input.
        RETURN NO-APPLY.
    END.
    IF (btn-ok:LABEL IN  FRAME frm-input) = "Update" THEN DO:
        /*DO TRANSACTION: */
           /* FIND CURRENT bfr{&tmptable} EXCLUSIVE-LOCK. */
           wsbtn = "EDIT".
            IF CURRENT-CHANGED bfr{&tmptable} THEN DO:
                MESSAGE "This record has been changed by another user"
                SKIP  "Please re-enter your changes." VIEW-AS ALERT-BOX.
                RETURN NO-APPLY.
            END.
            ELSE DO:
              FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.dbacc =  DEC(bfr{&tmptable}.dbacc:SCREEN-VALUE) EXCLUSIVE-LOCK NO-ERROR.
              ASSIGN  bfr{&tmptable}.NAME      = bfr{&tmptable}.NAME:SCREEN-VALUE
                    bfr{&tmptable}.scheme    = INT(bfr{&tmptable}.scheme:SCREEN-VALUE)
                    bfr{&tmptable}.Sortname  = bfr{&tmptable}.Sortname:SCREEN-VALUE
                    bfr{&tmptable}.RegId     = bfr{&tmptable}.RegId:SCREEN-VALUE
                    bfr{&tmptable}.RegId1     = bfr{&tmptable}.RegId1:SCREEN-VALUE
                    bfr{&tmptable}.VatNo     = DEC(bfr{&tmptable}.VatNo:SCREEN-VALUE)
                    bfr{&tmptable}.StandNo   = bfr{&tmptable}.StandNo:SCREEN-VALUE
                    bfr{&tmptable}.Add1      = bfr{&tmptable}.Add1:SCREEN-VALUE
                    bfr{&tmptable}.Add2      = bfr{&tmptable}.Add2:SCREEN-VALUE
                    bfr{&tmptable}.Add3      = bfr{&tmptable}.Add3:SCREEN-VALUE
                    bfr{&tmptable}.suburb    = int(bfr{&tmptable}.suburb:SCREEN-VALUE)
                    bfr{&tmptable}.ward      = int(bfr{&tmptable}.ward:SCREEN-VALUE)
                    bfr{&tmptable}.SIZE      = DEC(bfr{&tmptable}.SIZE:SCREEN-VALUE)
                    bfr{&tmptable}.SiteValue = DEC(bfr{&tmptable}.SiteValue:SCREEN-VALUE)
                    bfr{&tmptable}.BldValue  = DEC(bfr{&tmptable}.BldValue:SCREEN-VALUE)
                    bfr{&tmptable}.DeedNo    = bfr{&tmptable}.DeedNo:SCREEN-VALUE
                    bfr{&tmptable}.emailAdd  = bfr{&tmptable}.emailAdd:SCREEN-VALUE
                    bfr{&tmptable}.cell[1]      = DEC(bfr{&tmptable}.cell[1]:SCREEN-VALUE)
                    bfr{&tmptable}.cell[2]      = DEC(bfr{&tmptable}.cell[2]:SCREEN-VALUE)
                    bfr{&tmptable}.comnt      = bfr{&tmptable}.comnt:SCREEN-VALUE
                    bfr{&tmptable}.credate   = TODAY
                    bfr{&tmptable}.uid       = varuser.
               ASSIGN bfr{&tmptable}.InsAmt bfr{&tmptable}.Period bfr{&tmptable}.Ref.
            END.
            FIND CURRENT bfr{&tmptable} NO-LOCK.                    
       /* END. *//* Do Transaction */
        HIDE FRAME frm-input.
        APPLY 'close' TO THIS-PROCEDURE.
        brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
        RETURN.
    END.
    ELSE DO:
        IF bfr{&tmptable}.dbacc:SCREEN-VALUE = "" THEN DO:
            MESSAGE "BLANK not a valid Account...." VIEW-AS ALERT-BOX.
            APPLY 'entry' TO bfr{&tmptable}.dbacc IN FRAME frm-input.
            RETURN NO-APPLY.
        END.
        ELSE IF  DEC(bfr{&tmptable}.dbacc:SCREEN-VALUE) <> 0 THEN DO:
            FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.dbacc = DEC(bfr{&tmptable}.dbacc:SCREEN-VALUE) AND accStat = 0 NO-ERROR.
            IF AVAILABLE bfr{&tmptable} THEN DO:
                MESSAGE "Account alread exist.... no record saved" VIEW-AS ALERT-BOX.
                RETURN NO-APPLY.
            END.
            ELSE DO:
               FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.dbacc = DEC(bfr{&tmptable}.dbacc:SCREEN-VALUE) AND accStat <> 0 EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE bfr{&tmptable} THEN DO:
                    ASSIGN bfr{&tmptable}.dbacc     = DEC(bfr{&tmptable}.dbacc:SCREEN-VALUE)
                                bfr{&tmptable}.scheme    = INT(bfr{&tmptable}.scheme:SCREEN-VALUE)
                                bfr{&tmptable}.NAME      = bfr{&tmptable}.NAME:SCREEN-VALUE
                                bfr{&tmptable}.Sortname  = bfr{&tmptable}.Sortname:SCREEN-VALUE
                                bfr{&tmptable}.RegId     = bfr{&tmptable}.RegId:SCREEN-VALUE
                                bfr{&tmptable}.RegId1     = bfr{&tmptable}.RegId1:SCREEN-VALUE
                                bfr{&tmptable}.VatNo     = int(bfr{&tmptable}.VatNo:SCREEN-VALUE)
                                bfr{&tmptable}.StandNo   = bfr{&tmptable}.StandNo:SCREEN-VALUE
                                bfr{&tmptable}.Add1      = bfr{&tmptable}.Add1:SCREEN-VALUE
                                bfr{&tmptable}.Add2      = bfr{&tmptable}.Add2:SCREEN-VALUE
                                bfr{&tmptable}.Add3      = bfr{&tmptable}.Add3:SCREEN-VALUE
                                bfr{&tmptable}.suburb    = INT(bfr{&tmptable}.suburb:SCREEN-VALUE)
                                bfr{&tmptable}.ward      = INT(bfr{&tmptable}.ward:SCREEN-VALUE)
                                bfr{&tmptable}.SIZE      = DEC(bfr{&tmptable}.SIZE:SCREEN-VALUE)
                                bfr{&tmptable}.SiteValue = DEC(bfr{&tmptable}.SiteValue:SCREEN-VALUE)
                                bfr{&tmptable}.BldValue  = DEC(bfr{&tmptable}.BldValue:SCREEN-VALUE)
                                bfr{&tmptable}.DeedNo    = bfr{&tmptable}.DeedNo:SCREEN-VALUE
                                bfr{&tmptable}.emailAdd  = bfr{&tmptable}.emailAdd:SCREEN-VALUE
                                bfr{&tmptable}.cell[1]      = DEC(bfr{&tmptable}.cell[1]:SCREEN-VALUE)
                                bfr{&tmptable}.cell[2]      = DEC(bfr{&tmptable}.cell[2]:SCREEN-VALUE)
                                bfr{&tmptable}.comnt      = bfr{&tmptable}.comnt:SCREEN-VALUE
                                bfr{&tmptable}.AccStat   = 0
                                bfr{&tmptable}.LTDate    = wsDate
                                bfr{&tmptable}.scheme    = INT(bfr{&tmptable}.scheme:SCREEN-VALUE).
                                   
                         ASSIGN  bfr{&tmptable}.AmtDue[1] bfr{&tmptable}.AmtDue[2] bfr{&tmptable}.Bal bfr{&tmptable}.InsAmt
                                 bfr{&tmptable}.LTAmt bfr{&tmptable}.PPrice   bfr{&tmptable}.Ref bfr{&tmptable}.PRate 
                                 bfr{&tmptable}.LTRate  bfr{&tmptable}.Period bfr{&tmptable}.PDate bfr{&tmptable}.DAmt bfr{&tmptable}.Vat .
                         ASSIGN bfr{&tmptable}.LBal    = (bfr{&tmptable}.bal * wsRate[1])
                                bfr{&tmptable}.LAmtDue = bfr{&tmptable}.AmtDue[2].
                         IF DEC(bfr{&tmptable}.DAmt:SCREEN-VALUE) > 0 THEN
                             RUN CreTrans.ip.
                    END.
                    ELSE DO:
                         CREATE bfr{&tmptable}.
                         ASSIGN bfr{&tmptable}.dbacc     = DEC(bfr{&tmptable}.dbacc:SCREEN-VALUE)
                                bfr{&tmptable}.scheme    = INT(bfr{&tmptable}.scheme:SCREEN-VALUE)
                                bfr{&tmptable}.NAME      = bfr{&tmptable}.NAME:SCREEN-VALUE
                                bfr{&tmptable}.Sortname  = bfr{&tmptable}.Sortname:SCREEN-VALUE
                                bfr{&tmptable}.RegId     = bfr{&tmptable}.RegId:SCREEN-VALUE
                                bfr{&tmptable}.RegId1     = bfr{&tmptable}.RegId1:SCREEN-VALUE
                                bfr{&tmptable}.VatNo     = int(bfr{&tmptable}.VatNo:SCREEN-VALUE)
                                bfr{&tmptable}.StandNo   = bfr{&tmptable}.StandNo:SCREEN-VALUE
                                bfr{&tmptable}.Add1      = bfr{&tmptable}.Add1:SCREEN-VALUE
                                bfr{&tmptable}.Add2      = bfr{&tmptable}.Add2:SCREEN-VALUE
                                bfr{&tmptable}.Add3      = bfr{&tmptable}.Add3:SCREEN-VALUE
                                bfr{&tmptable}.suburb    = INT(bfr{&tmptable}.suburb:SCREEN-VALUE)
                                bfr{&tmptable}.ward      = INT(bfr{&tmptable}.ward:SCREEN-VALUE)
                                bfr{&tmptable}.SIZE      = DEC(bfr{&tmptable}.SIZE:SCREEN-VALUE)
                                bfr{&tmptable}.SiteValue = DEC(bfr{&tmptable}.SiteValue:SCREEN-VALUE)
                                bfr{&tmptable}.BldValue  = DEC(bfr{&tmptable}.BldValue:SCREEN-VALUE)
                                bfr{&tmptable}.DeedNo    = bfr{&tmptable}.DeedNo:SCREEN-VALUE
                                bfr{&tmptable}.emailAdd  = bfr{&tmptable}.emailAdd:SCREEN-VALUE
                                bfr{&tmptable}.cell[1]      = DEC(bfr{&tmptable}.cell[1]:SCREEN-VALUE)
                                bfr{&tmptable}.cell[2]      = DEC(bfr{&tmptable}.cell[2]:SCREEN-VALUE)
                                bfr{&tmptable}.comnt      = bfr{&tmptable}.comnt:SCREEN-VALUE
                                bfr{&tmptable}.credate   = TODAY
                                bfr{&tmptable}.AccStat   = 0
                                bfr{&tmptable}.LTDate    = wsDate
                                bfr{&tmptable}.scheme    = INT(bfr{&tmptable}.scheme:SCREEN-VALUE)
                                bfr{&tmptable}.DeviceName = lc-host
                                bfr{&tmptable}.Credate = TODAY 
                                bfr{&tmptable}.UID     = varuser.
                                   
                         ASSIGN  bfr{&tmptable}.AmtDue[1] bfr{&tmptable}.AmtDue[2] bfr{&tmptable}.Bal bfr{&tmptable}.InsAmt
                                 bfr{&tmptable}.LTAmt bfr{&tmptable}.PPrice   bfr{&tmptable}.Ref bfr{&tmptable}.PRate 
                                 bfr{&tmptable}.LTRate  bfr{&tmptable}.Period bfr{&tmptable}.PDate bfr{&tmptable}.DAmt bfr{&tmptable}.Vat .
                         ASSIGN bfr{&tmptable}.LBal    = (bfr{&tmptable}.bal * wsRate[1])
                                bfr{&tmptable}.LAmtDue = bfr{&tmptable}.AmtDue[2].
                         /* IF DEC(bfr{&tmptable}.DAmt:SCREEN-VALUE) > 0 THEN */
                             RUN CreTrans.ip.
                    END.
                    MESSAGE "Do you want a Service Account created?"
                        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
                        UPDATE wsAns AS LOGICAL.
                     IF wsAns = YES THEN DO:
                         CREATE dbcmf.
                         ASSIGN dbcmf.dbacc     = DEC(bfr{&tmptable}.dbacc:SCREEN-VALUE)
                                dbcmf.NAME      = bfr{&tmptable}.NAME:SCREEN-VALUE
                                dbcmf.Sortname  = bfr{&tmptable}.Sortname:SCREEN-VALUE
                                dbcmf.RegId     = bfr{&tmptable}.RegId:SCREEN-VALUE
                                dbcmf.VatNo     = int(bfr{&tmptable}.VatNo:SCREEN-VALUE)
                                dbcmf.StandNo   = bfr{&tmptable}.StandNo:SCREEN-VALUE
                                dbcmf.Add1      = bfr{&tmptable}.Add1:SCREEN-VALUE
                                dbcmf.Add2      = bfr{&tmptable}.Add2:SCREEN-VALUE
                                dbcmf.Add3      = bfr{&tmptable}.Add3:SCREEN-VALUE
                                dbcmf.suburb    = INT(bfr{&tmptable}.suburb:SCREEN-VALUE)
                                dbcmf.ward      = INT(bfr{&tmptable}.ward:SCREEN-VALUE)
                                dbcmf.SIZE      = DEC(bfr{&tmptable}.SIZE:SCREEN-VALUE)
                                dbcmf.SiteValue = DEC(bfr{&tmptable}.SiteValue:SCREEN-VALUE)
                                dbcmf.BldValue  = DEC(bfr{&tmptable}.BldValue:SCREEN-VALUE)
                                dbcmf.DeedNo    = bfr{&tmptable}.DeedNo:SCREEN-VALUE
                                dbcmf.emailAdd  = bfr{&tmptable}.emailAdd:SCREEN-VALUE
                                dbcmf.Cell      = DEC(bfr{&tmptable}.cell[1]:SCREEN-VALUE)
                                dbcmf.credate   = TODAY
                                dbcmf.AccStat   = 0
                                dbcmf.POwner    = YES
                                dbcmf.DeviceName = lc-host
                                dbcmf.Credate = TODAY 
                                dbcmf.UID     = varuser.
                     END.
                    RELEASE bfr{&tmptable}.
                   CLEAR FRAME frm-input ALL.
                   VIEW FRAME frm-input.
                   APPLY 'entry' TO bfr{&tmptable}.scheme.
                END.
                
        END. /* eof if dbacc <> 0 */
     END. /* eof ELSE */
END.

ON 'choose':U OF btn-close IN FRAME frm-input
DO:
    HIDE FRAME frm-input.
    APPLY 'close' TO THIS-PROCEDURE IN FRAME frm-input.
    RETURN.
END.

ON LEAVE OF bfr{&tmptable}.dbacc IN FRAME frm-input
DO:
   IF  DEC(bfr{&tmptable}.dbacc:SCREEN-VALUE) <> 0 THEN DO:
       FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.dbacc = DEC(bfr{&tmptable}.dbacc:SCREEN-VALUE) AND accStat = 0 NO-ERROR.
       IF AVAILABLE bfr{&tmptable} AND btn-ok:LABEL = "SAVE" THEN DO:
          MESSAGE "Record already exist" VIEW-AS ALERT-BOX.
          RETURN NO-APPLY.
       END.
   END.
   RETURN.    
END.

/*...... Triggers for button on-form buttons ....*/

/*------------------ Ward Button -------------*/
ON CHOOSE OF btn-Ward IN FRAME frm-input
DO:
  VIEW FRAME frm-Ward.
  OPEN QUERY qry-Ward FOR EACH dbwrd NO-LOCK.
  ENABLE ALL WITH FRAME frm-Ward.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-Ward 
          OR close of THIS-PROCEDURE IN FRAME frm-Ward
          OR CHOOSE OF btn-ok IN FRAME frm-Ward 
          OR 'enter':u OF brw-Ward
          OR 'mouse-select-dblclick' OF brw-Ward.
  CLOSE QUERY qry-Ward.
  HIDE FRAME frm-Ward.
  APPLY 'tab' TO SELF.
  APPLY 'tab' TO bfr{&tmptable}.ward.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-Ward
    OR 'enter':u OF brw-Ward
    OR 'mouse-select-dblclick' OF brw-Ward
DO: 
   GET CURRENT qry-Ward EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY dbwrd.Descrip dbwrd.Ward @ bfr{&tmptable}.ward  WITH FRAME frm-input.
   RETURN.
END.

ON 'enter':U OF bfr{&tmptable}.ward IN FRAME frm-input
    OR 'tab':U OF bfr{&tmptable}.ward IN FRAME frm-input
DO:
    FIND FIRST dbwrd WHERE dbwrd.Ward = INT(bfr{&tmptable}.ward:SCREEN-VALUE) NO-ERROR.
    IF AVAILABLE dbwrd THEN
        DISPLAY dbwrd.Descrip  WITH FRAM frm-input.
    ELSE DO:
        MESSAGE "Invalid Ward...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

/*------------------ Suburb Button -------------*/
ON CHOOSE OF btn-Sub IN FRAME frm-input
DO:
  VIEW FRAME frm-Sub.
  OPEN QUERY qry-Sub FOR EACH dbsmf NO-LOCK.
  ENABLE ALL WITH FRAME frm-Sub.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-Sub 
          OR close of THIS-PROCEDURE IN FRAME frm-Sub
          OR CHOOSE OF btn-ok IN FRAME frm-Sub 
          OR 'enter':u OF brw-Sub
          OR 'mouse-select-dblclick' OF brw-Sub.
  CLOSE QUERY qry-Sub.
  HIDE FRAME frm-Sub.
  APPLY 'tab' TO SELF.
   APPLY 'tab' TO bfr{&tmptable}.suburb.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-Sub
    OR 'enter':u OF brw-Sub
    OR 'mouse-select-dblclick' OF brw-Sub
DO: 
   GET CURRENT qry-Sub EXCLUSIVE-LOCK NO-WAIT.
        DISPLAY dbsmf.Descrip dbsmf.suburb @ bfr{&tmptable}.suburb  WITH FRAME frm-input.
        RETURN.
END.

ON 'enter':U OF bfr{&tmptable}.suburb IN FRAME frm-input
    OR 'tab':U OF bfr{&tmptable}.suburb IN FRAME frm-input
    OR 'leave':U OF bfr{&tmptable}.suburb IN FRAME frm-input
DO:
    FIND FIRST dbsmf WHERE dbsmf.suburb = INT(bfr{&tmptable}.suburb:SCREEN-VALUE) NO-ERROR.
    IF AVAILABLE dbsmf THEN
        DISPLAY dbsmf.Descrip  WITH FRAM frm-input.
    ELSE DO:
        MESSAGE "Invalid Suburb...Please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'enter':U OF bfr{&tmptable}.DAmt IN FRAME frm-Input
    OR 'leave':U OF bfr{&tmptable}.DAmt IN FRAME frm-Input 
DO:
    IF  DEC(bfr{&tmptable}.DAmt:SCREEN-VALUE) > DEC(bfr{&tmptable}.PPrice:SCREEN-VALUE) THEN DO:
        MESSAGE "Deposit may not be Greater than the Price......." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        ASSIGN  bfr{&tmptable}.Bal:SCREEN-VALUE = STRING(DEC(bfr{&tmptable}.PPrice:SCREEN-VALUE) - DEC(bfr{&tmptable}.DAmt:SCREEN-VALUE)).
       IF  DEC(bfr{&tmptable}.DAmt:SCREEN-VALUE) <> 0.00 THEN DO:
           ASSIGN bfr{&tmptable}.AmtDue[1]:SCREEN-VALUE =  STRING(DEC(bfr{&tmptable}.DAmt:SCREEN-VALUE))
                  bfr{&tmptable}.AmtDue[2]:SCREEN-VALUE = STRING(ROUND(DEC(bfr{&tmptable}.AmtDue[1]:SCREEN-VALUE) * DEC(bfr{&tmptable}.PRate:SCREEN-VALUE),2))
                  bfr{&tmptable}.LTDate:SCREEN-VALUE    = STRING(TODAY)
                  bfr{&tmptable}.LTRate:SCREEN-VALUE    = bfr{&tmptable}.PRate:SCREEN-VALUE
                  bfr{&tmptable}.LTAmt:SCREEN-VALUE     = bfr{&tmptable}.AmtDue[1]:SCREEN-VALUE.
                  
       END.
    END.   
    RETURN.
END.

ON 'enter':U OF bfr{&tmptable}.period
    OR 'leave':U OF bfr{&tmptable}.period
DO:
    IF DEC(bfr{&tmptable}.period:SCREEN-VALUE) <= 0  THEN DO:
        MESSAGE "Period should be at least 1 or more...." VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ASSIGN wsP = DEC(bfr{&tmptable}.PPrice:SCREEN-VALUE) - DEC(bfr{&tmptable}.DAmt:SCREEN-VALUE)
           wsn = DEC(bfr{&tmptable}.period:SCREEN-VALUE).
    IF hsesch.IntRate <> 0 THEN
           ASSIGN wsr = (hsesch.IntRate / 1200)
                  wsInst = ROUND(wsP / ((EXP(( 1 + wsr),wsn) - 1) / (wsr * (EXP((1 + wsr),wsn)))),2).
    ELSE wsInst = ROUND((wsP / wsn),2).
           MESSAGE "Installment = " wsInst VIEW-AS ALERT-BOX.
    bfr{&tmptable}.InsAmt:SCREEN-VALUE = STRING(wsInst).
    RETURN.
END.
/* ***** Triggers for the main frame **** */
 ON CHOOSE OF btn-Add IN FRAME frm-Main DO:
    btn-ok:LABEL IN  FRAME frm-input = "SAVE".
    CLEAR FRAME frm-input ALL.
    VIEW FRAME frm-input.
    st-per:SCREEN-VALUE = STRING(simctr.curper).
    ENABLE ALL  WITH FRAME frm-input.
    APPLY 'entry' TO bfr{&tmptable}.scheme.
    WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
    OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmpTable}.accStat = 0 NO-LOCK.
END.

ON CHOOSE OF btn-Edit IN FRAME frm-Main 
    OR 'mouse-select-dblclick' OF brw-{&tmptable} 
DO: 
    btn-ok:LABEL IN FRAME frm-input = "UPDATE".
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.dbacc.
    DISABLE bfr{&tmptable}.dbacc WITH FRAME frm-input.
    RUN proc-edit.
    RETURN.
END.

ON CHOOSE OF btn-Del IN FRAME frm-Main 
DO:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.dbacc.
    FIND FIRST hsehtf WHERE hsehtf.dbacc  = wsid NO-LOCK NO-ERROR.
    IF AVAILABLE hsehtf THEN DO:
        MESSAGE "Account has related records - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE hsehtf THEN DO:
       DELETE bfr{&tmptable}.
       Method-Status = brw-{&tmptable}:DELETE-SELECTED-ROWS().
    END.
    APPLY 'entry' TO btn-exit.
END.

{audit.i}

 on 'start-search':u of browse brw-{&tmptable}
    run SEARCH.ip.

/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
browse brw-{&tmptable}:allow-column-searching = true.
ASSIGN wsTime    = STRING(TIME,"HH:MM:SS").
       wsTransID = DEC (string(YEAR(TODAY),"9999")
                 + string(MONTH(TODAY),"99" )
                 + string(DAY(TODAY),"99")
                 + SUBSTR(STRING(wsTIME),1,2) 
                 + SUBSTR(STRING(wsTIME),4,2) 
                 + SUBSTR(STRING(wsTIME),7,2) ). 
OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.accStat = 0 NO-LOCK.
ENABLE ALL EXCEPT btn-Edit WITH FRAME frm-Main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-Main.
IF CAN-FIND (FIRST dbgl WHERE dbgl.TransID = wsTransID AND dbgl.SOURCE = wsSource NO-LOCK) THEN
DO:
    {PrintOpt.i &stream-name="stream a"
                    &print-prog="Consol.ip"
                    &paged}
END. 
CLOSE QUERY qry-{&tmptable}.
HIDE FRAME frm-Main.

/******* INTERNAL PROCEDURES *********/
PROCEDURE proc-input:
   CLEAR FRAME frm-input ALL.
   VIEW FRAME frm-input.
   ENABLE ALL WITH FRAME frm-input.
   APPLY 'entry' TO bfr{&tmptable}.scheme.
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmpTable}.accStat <> 0 NO-LOCK.
END PROCEDURE.

PROCEDURE proc-Edit:
   CLEAR FRAME frm-input.
   VIEW FRAME frm-input.
   ENABLE ALL WITH FRAME frm-input.
   FIND FIRST dbsmf WHERE dbsmf.suburb = bfr{&tmptable}.suburb NO-LOCK NO-ERROR.
   FIND FIRST dbwrd WHERE dbwrd.ward   = bfr{&tmptable}.ward NO-LOCK NO-ERROR.
   FIND FIRST hsesch WHERE hsesch.scheme = bfr{&tmptable}.scheme NO-LOCK NO-ERROR.
   IF hsesch.Freq = "M"  THEN
      wsdes = "Monthly".
   IF hsesch.Freq = "Q"  THEN
      wsdes = "Quarterly".
   IF hsesch.Freq = "H"  THEN
      wsdes = "Half-Yearly".
   IF hsesch.Freq = "Y"  THEN
      wsdes = "Yearly".
   DISPLAY bfr{&tmptable} EXCEPT bfr{&tmptable}.devicename bfr{&tmptable}.bfBal bfr{&tmptable}.AccStat bfr{&tmptable}.Credate bfr{&tmptable}.UID 
           bfr{&tmptable}.LAmtDue bfr{&tmptable}.lbal WITH FRAME frm-input.
   DISPLAY hsesch.Descrip wsdes hsesch.txtCur hsesch.IntRate 
       dbwrd.Descrip dbsmf.Descrip WITH FRAME frm-input.  
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input
          OR CHOOSE OF btn-ok IN FRAME frm-input.
END PROCEDURE.

PROCEDURE SEARCH.ip.
    hCol = BROWSE brw-{&tmptable}:CURRENT-COLUMN.
        VIEW FRAME Search-opt.
        ENABLE ALL WITH FRAME Search-opt.
        WAIT-FOR 'choose' OF btnSearch OR 'tab' of btnSearch OR 'enter' OF btnSearch 
            OR 'window-close' OF FRAME Search-opt OR CLOSE OF THIS-PROCEDURE OR
                'esc','f4' OF FRAME Search-opt.
        HIDE FRAME Search-opt.
        CASE trim(hCol:label):
            when "Name" then
            do:
               open query qry-{&tmptable} preselect 
                        EACH bfr{&tmptable} no-lock 
                            where bfr{&tmpTable}.accStat = 0 AND bfr{&tmptable}.NAME >= wsSearch:SCREEN-VALUE USE-INDEX sortname.

            END.
            when "Account" then
            do:
               open query qry-{&tmptable} preselect 
                        EACH bfr{&tmptable} no-lock 
                            where bfr{&tmpTable}.accStat = 0 AND bfr{&tmptable}.dbAcc >= DEC(wsSearch:SCREEN-VALUE) USE-INDEX dbacc.

            END.
            when "StandNo" then
            do:
               open query qry-{&tmptable} preselect 
                        EACH bfr{&tmptable} no-lock 
                            where bfr{&tmpTable}.accStat = 0 AND bfr{&tmptable}.StandNo >= wsSearch:SCREEN-VALUE
                               BY bfr{&tmptable}.StandNo.

            END.
            when "Scheme" then
            do:
               open query qry-{&tmptable} preselect 
                        EACH bfr{&tmptable} no-lock 
                            where bfr{&tmpTable}.accStat = 0 AND bfr{&tmptable}.scheme >= INT(wsSearch:SCREEN-VALUE)
                               BY bfr{&tmptable}.Scheme.

            END.
            when "Suburb" then
            do:
               open query qry-{&tmptable} preselect 
                        EACH bfr{&tmptable} no-lock 
                            where bfr{&tmpTable}.accStat = 0 AND bfr{&tmptable}.Suburb >= INT(wsSearch:SCREEN-VALUE)
                               BY bfr{&tmptable}.Suburb.

            END.
        END.
        RETURN.
END PROCEDURE.


PROCEDURE CreTrans.ip:
    /*Create Monthly Transaction  for deposit*/
    CREATE hsemtf.
    ASSIGN hsemtf.Accper  = st-per
           hsemtf.Amt     = bfr{&tmptable}.DAmt /* bfr{&tmptable}.PRate */
           hsemtf.CRate   = bfr{&tmptable}.PRate
           hsemtf.Vat     = ROUND((bfr{&tmptable}.DAmt * (bfr{&tmptable}.Vat / (100  + bfr{&tmptable}.Vat))) ,2)
           hsemtf.dbacc   = bfr{&tmptable}.dbacc
           hsemtf.Scheme  = hsesch.scheme
           hsemtf.trDate  = wsDate
           hsemtf.ref     = bfr{&tmptable}.ref
           hsemtf.descrip = "Land Sales Deposit".
     /*Create Historical Transaction for deposit*/
       FIND FIRST glmf WHERE glmf.acct = hsesch.CLedger NO-LOCK NO-ERROR.
       CREATE hsehtf.
       ASSIGN hsehtf.Accper  = st-per
              hsehtf.dbacc   = bfr{&tmptable}.dbacc
              hsehtf.Amt     = bfr{&tmptable}.DAmt
              hsehtf.CRate   = bfr{&tmptable}.PRate
              hsehtf.tRate   = bfr{&tmptable}.PRate
              hsehtf.txtCur  = hsesch.txtCur
              hsehtf.Vat     = ROUND((bfr{&tmptable}.DAmt * (bfr{&tmptable}.Vat / (100 + bfr{&tmptable}.Vat))) ,2)
              hsehtf.Iledger = hsesch.Cledger
              hsehtf.proj    = glmf.Proj
              hsehtf.dept    = glmf.dept
              hsehtf.fund    = glmf.fund
              hsehtf.ref     = bfr{&tmptable}.ref
              hsehtf.Scheme  = hsesch.scheme
              hsehtf.trDate  = wsDate
              hsehtf.PoDate  = TODAY
              hsehtf.prog    = "hse02.p"
              hsehtf.TransID = wsTransId  
              hsehtf.uid     = varUser
              hsehtf.descrip = "Land Sales Deposit".
    FIND FIRST hseblf WHERE hseblf.scheme = bfr{&tmptable}.scheme AND hseblf.dbacc = bfr{&tmptable}.dbacc NO-ERROR. /* Age analysis data */
    IF NOT AVAILABLE hseblf THEN DO:
        CREATE hseblf.
        ASSIGN hseblf.scheme = bfr{&tmptable}.scheme
               hseblf.dbacc  = bfr{&tmptable}.dbacc.
    END.
    hseblf.amt[1] = hseblf.amt[1] + bfr{&tmptable}.DAmt.
    RUN glcons-ip.

END PROCEDURE.      

PROCEDURE Consol.ip:
     {glcon.i}
END.
   
PROCEDURE glcons-ip:
    /* Create GL Consolidation Transactions */ 
     wsVat  = ROUND((bfr{&tmptable}.PPrice * wsrate[1]) * (bfr{&tmptable}.Vat / (100 + bfr{&tmptable}.Vat)),2).
    DO X = 1 TO 2: /*Capital Amount */     
         ASSIGN wsledger = hsesch.CLedger WHEN X = 1
                wsledger = hsesch.ILedger WHEN X = 2
                wsAmt  = ROUND(((bfr{&tmptable}.PPrice * wsRate[1]) - wsVat),2)        WHEN X = 1
                wsAmt  = ROUND(((bfr{&tmptable}.PPrice * wsRate[1]) - wsVat),2) * -1 WHEN X = 2.
         FIND FIRST glmf WHERE glmf.acct = wsLedger NO-LOCK NO-ERROR.
         FIND FIRST dbgl WHERE dbgl.acct = wsLedger AND dbgl.period = st-per
                           AND dbgl.TransID = wsTransId AND dbgl.proj = glmf.proj
                           AND dbgl.dept = glmf.dept AND dbgl.fund = glmf.fund AND dbgl.descrip  = "Online Land Sales " NO-ERROR.
         IF NOT AVAILABLE dbgl THEN DO:
               CREATE dbgl.
               ASSIGN dbgl.acct     = wsLedger
                      dbgl.proj     = glmf.proj
                      dbgl.dept     = glmf.dept
                      dbgl.fund     = glmf.fund
                      dbgl.period   = st-per
                      dbgl.TransID  = wsTransId
                      dbgl.trDATE   = wsDate
                      dbgl.UID      = varUser
                      dbgl.REF      = "HSE02" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99")
                      dbgl.descrip  = "Online Land Sales "
                      dbgl.CREDATE  = TODAY
                      dbgl.SOURCE   = wsSource.
         END.
         ASSIGN dbgl.AMT = dbgl.AMT + wsAmt + wsVat WHEN X = 1
                dbgl.AMT = dbgl.AMT + wsAmt WHEN X = 2.
    END.
    IF wsVat <> 0 THEN DO: /* Credit Vat leg */
         FIND FIRST glmf WHERE glmf.acct = simctr.vat[1] NO-LOCK NO-ERROR.
         FIND FIRST dbgl WHERE dbgl.acct = simctr.vat[1] AND dbgl.period = st-per
                           AND dbgl.TransID = wsTransId AND dbgl.proj = glmf.proj
                           AND dbgl.dept = glmf.dept AND dbgl.fund = glmf.fund
                           AND dbgl.descrip  = "Online Land Sales VAT" NO-ERROR.
         IF NOT AVAILABLE dbgl THEN DO:
               CREATE dbgl.
               ASSIGN dbgl.acct     = simctr.vat[1]
                      dbgl.proj     = glmf.proj
                      dbgl.dept     = glmf.dept
                      dbgl.fund     = glmf.fund
                      dbgl.period   = st-per
                      dbgl.TransID  = wsTransId
                      dbgl.trDATE   = wsDate
                      dbgl.UID      = varUser
                      dbgl.REF      = "HSE02" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99")
                      dbgl.descrip  = "Online Land Sales VAT"
                      dbgl.CREDATE  = TODAY
                      dbgl.SOURCE   = wsSource.
         END.
         dbgl.AMT = dbgl.AMT + wsVat * -1.
    END.
    IF  bfr{&tmptable}.DAmt  <> 0 THEN DO:
        DO X = 1 TO 2: /* Deposit Amount */       
             ASSIGN wsledger = hsesch.CtrLedger WHEN X = 1
                    wsledger = hsesch.CLedger WHEN X = 2
                    wsAmt  = ROUND((bfr{&tmptable}.DAmt * wsrate[1]),2)        WHEN X = 1
                    wsAmt  = ROUND(((bfr{&tmptable}.DAmt * wsrate[1]) * -1),2) WHEN X = 2.
             FIND FIRST glmf WHERE glmf.acct = wsLedger NO-LOCK NO-ERROR.
             FIND FIRST dbgl WHERE dbgl.acct = wsLedger AND dbgl.period = st-per
                               AND dbgl.TransID = wsTransId AND dbgl.proj = glmf.proj
                               AND dbgl.dept = glmf.dept AND dbgl.fund = glmf.fund  
                               AND dbgl.descrip = "Online Land Sales Deposit" NO-ERROR.
             IF NOT AVAILABLE dbgl THEN DO:
                   CREATE dbgl.
                   ASSIGN dbgl.acct     = wsLedger
                          dbgl.proj     = glmf.proj
                          dbgl.dept     = glmf.dept
                          dbgl.fund     = glmf.fund
                          dbgl.period   = st-per
                          dbgl.TransID  = wsTransId
                          dbgl.trDATE   = wsDate
                          dbgl.UID      = varUser
                          dbgl.REF      = "HSE02" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99")
                          dbgl.descrip  = "Online Land Sales Deposit"
                          dbgl.CREDATE  = TODAY
                          dbgl.SOURCE   = wsSource.
             END.
             dbgl.AMT = dbgl.AMT + wsAmt.
        END.
    END.
END PROCEDURE.
