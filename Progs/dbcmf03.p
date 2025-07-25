session:DATA-ENTRY-RETURN = TRUE.
/* Program.................dbcmf03.p
   Notes:...... ...........lAND SALES file maintenance
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsMsg          "Account already exist"
&SCOPED-DEFINE wsTitle        "LAND SALES ACCOUNT FILE MAINTENENACE"
&SCOPED-DEFINE tmptable       landSale
&SCOPED-DEFINE skey           {&tmptable}.dbAcc
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.ls ~
                              bfr{&tmptable}.scheme ~
                              bfr{&tmptable}.StandNo  ~
                              bfr{&tmptable}.PDate ~
                              bfr{&tmptable}.Amt[1] ~
                              bfr{&tmptable}.Amt[2] ~
                              bfr{&tmptable}.Amt[3] ~
                              bfr{&tmptable}.Amt[4] ~
                              bfr{&tmptable}.SAmt[1] ~
                              bfr{&tmptable}.SAmt[2] ~
                              bfr{&tmptable}.SAmt[3] ~
                              bfr{&tmptable}.SAmt[4] ~
                              bfr{&tmptable}.DAmt[1] ~
                              bfr{&tmptable}.DAmt[2] ~
                              bfr{&tmptable}.DAmt[3] ~
                              bfr{&tmptable}.DAmt[4] ~
                              bfr{&tmptable}.BAmt[1] ~
                              bfr{&tmptable}.BAmt[2] ~
                              bfr{&tmptable}.BAmt[3] ~
                              bfr{&tmptable}.BAmt[4] ~
                              bfr{&tmptable}.INT[1] ~
                              bfr{&tmptable}.INT[2] ~
                              bfr{&tmptable}.INT[3] ~
                              bfr{&tmptable}.INT[4] ~
                              bfr{&tmptable}.Per[1] ~
                              bfr{&tmptable}.Per[2] ~
                              bfr{&tmptable}.Per[3] ~
                              bfr{&tmptable}.Per[4] ~
                              bfr{&tmptable}.Instal[1] ~
                              bfr{&tmptable}.Instal[2] ~
                              bfr{&tmptable}.Instal[3] ~
                              bfr{&tmptable}.Instal[4] ~
                              bfr{&tmptable}.NextP[1] ~
                              bfr{&tmptable}.NextP[2] ~
                              bfr{&tmptable}.NextP[3] ~
                              bfr{&tmptable}.NextP[4] ~
                              bfr{&tmptable}.comm

&SCOPED-DEFINE tmpFields      bfr{&tmptable}.dbAcc ~
                                        COLUMN-LABEL ' ACCOUNT ':C ~
                              bfr{&tmptable}.Scheme ~
                                        COLUMN-LABEL ' SCHEME ':C ~
                               bfr{&tmptable}.StandNo ~
                                        COLUMN-LABEL ' STAND ':C ~
                               bfr{&tmptable}.PDate ~
                                        COLUMN-LABEL 'DATE ':C ~
                               bfr{&tmptable}.LS ~
                                        COLUMN-LABEL ' TYPE ':C ~
                               bfr{&tmptable}.Amt[4] ~
                                        COLUMN-LABEL ' PURCHASE COST ':C ~
                               bfr{&tmptable}.INT[4] ~
                                        COLUMN-LABEL 'INTEREST':C ~
                               bfr{&tmptable}.PER[4] ~
                                        COLUMN-LABEL 'PERIODS':C ~
                               bfr{&tmptable}.Instal[4] ~
                                        COLUMN-LABEL 'INSTALMENT':C ~
                               bfr{&tmptable}.BAmt[4] ~
                                        COLUMN-LABEL ' BALANCE ':C
                              
{varlibrary.i}
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
DEF VAR wsVat     LIKE wsCharge.
DEF VAR wsTotal   LIKE wsCharge.
DEF VAR wsTVat    LIKE wsCharge.
DEF VAR wsDr      LIKE wsCharge.
DEF VAR wsCr      LIKE wsCharge.
DEF VAR wsTDr     LIKE wsCharge.
DEF VAR wsTCr     LIKE wsCharge.
DEF VAR wsDate    AS DATE.
DEF VAR vatLedger LIKE glmf.acct.
DEF VAR wsLedger LIKE glmf.acct.
DEF VAR wsstatus  AS CHAR FORM "X(40)".
DEF VAR  varDescrip LIKE dbhtf.descrip.
DEF VAR wsBT      LIKE dbsgr.sgrp.
DEF VAR wsSource LIKE dbgl.SOURCE INITIAL "BD".
DEF VAR t AS INT.
DEF VAR wsDes LIKE lProj.Descrip EXTENT 2.
DEF VAR wsInt AS DEC FORM 99.9999.
DEF VAR  BilSep AS LOGICAL INITIAL YES.
DEF SHARED VAR wsTransType LIKE landsale.ls.
DEF VAR X AS INT.

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 116 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 116 by 18.5.

FORM dbgl.Proj          LABEL "PROJECT"
     dbgl.dept          LABEL "DEPT"
     dbgl.acct          LABEL "ACCOUNT"
     glmf.DESCRIPTION   LABEL "DESCRIPTION"
     wsDr               LABEL "DEBIT"
     wsCr               LABEL "CREDIT"
     HEADER skip(1) "                      LAND SALES CONSOLIDATION REPORT FOR THE PERIOD:" AT 10 SPACE(2)
    STRING(wsPer)
    "Page: " AT 110 PAGE-NUMBER(a) SKIP(1)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-lrpt.

DEF NEW SHARED FRAME frm-main
    brw-{&tmptable} AT ROW 1.8 COL 10
    btn-add AT ROW 20.7 COL 7
    Space(25) btn-edit
    space(25) btn-del
    space(25) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 5
    rect-1 AT ROW 20 COL 5
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D  
     BGCOLOR 8 FGCOLOR 1 SIZE 125 BY 24
    TITLE {&wsTitle} VIEW-AS DIALOG-BOX.

define frame frm-input
    SKIP(0.2) "---------------------------ACCOUNT DETAILS---------------------------" 
    COLON 10 SKIP(0.5)
    btnAcc NO-TAB-STOP COLON 20 {&skey}     NO-LABEL SPACE(5) 
    dbcmf.NAME LABEL "Accout Name" VIEW-AS TEXT SKIP(.2)
    dbcmf.Add1 COLON 30 LABEL "Postal Address" VIEW-AS TEXT
    dbcmf.add2 COLON 30 NO-LABEL VIEW-AS TEXT 
    dbcmf.Add3 COLON 30 NO-LABEL VIEW-AS text
    wsDes[1]      COLON 30 NO-LABEL VIEW-AS TEXT SKIP(0.2)
     "----------------------------------PROPERTY DETAILS------------------------------" COLON 10
    bfr{&tmptable}.LS COLON 30 LABEL "Type" NO-TAB-STOP SPACE(15)
    /*bfr{&tmptable}.billSep     LABEL "Bill Seperately(Y/N)?" SPACE(5) */
    btnScheme COLON 30 NO-TAB-STOP 
    bfr{&tmptable}.scheme NO-LABEL wsDes[2] VIEW-AS TEXT NO-LABEL SKIP(0.2)
    bfr{&tmptable}.StandNo COLON 30 LABEL "Stand Number" SPACE(5)
    bfr{&tmptable}.PDate  LABEL "Purchase Date" SKIP(0.2)
    "Land Costs            Survey Costs           Admin/Renewal Cost          Total Cost" COLON 35
    bfr{&tmptable}.Amt[1] COLON 30 LABEL "Original Cost" bfr{&tmptable}.Amt[2] NO-LABEL
    bfr{&tmptable}.Amt[3] NO-LABEL bfr{&tmptable}.Amt[4] NO-LABEL VIEW-AS TEXT SKIP(0.2)
    bfr{&tmptable}.DAmt[1] COLON 30 LABEL "Deposit" bfr{&tmptable}.DAmt[2] NO-LABEL
    bfr{&tmptable}.DAmt[3] NO-LABEL bfr{&tmptable}.DAmt[4] NO-LABEL  VIEW-AS TEXT SKIP(0.2) 
    bfr{&tmptable}.SAmt[1] COLON 30 LABEL "Sub-Total" VIEW-AS TEXT 
    SPACE(3) bfr{&tmptable}.SAmt[2] NO-LABEL VIEW-AS TEXT SPACE(3) bfr{&tmptable}.SAmt[3]NO-LABEL VIEW-AS TEXT
   SPACE(3) bfr{&tmptable}.SAmt[4] NO-LABEL VIEW-AS TEXT SKIP(0.2)
    bfr{&tmptable}.INT[1]  COLON 30 LABEL "Interest %" space(15) bfr{&tmptable}.INT[2] NO-LABEL
    space(10) bfr{&tmptable}.INT[3] NO-LABEL space(10) bfr{&tmptable}.INT[4] NO-LABEL VIEW-AS TEXT SKIP(0.2)
    bfr{&tmptable}.Per[1] COLON 30 LABEL "Periods" space(18) bfr{&tmptable}.Per[2] NO-LABEL
    space(12)bfr{&tmptable}.Per[3] NO-LABEL space(13) bfr{&tmptable}.Per[4] NO-LABEL VIEW-AS TEXT SKIP(0.2)  
    bfr{&tmptable}.BAmt[1] COLON 30 LABEL "Balance" bfr{&tmptable}.BAmt[2] NO-LABEL
    bfr{&tmptable}.BAmt[3] NO-LABEL bfr{&tmptable}.BAmt[4] NO-LABEL VIEW-AS TEXT SKIP(0.2)
    bfr{&tmptable}.Instal[1] COLON 30 LABEL "Instalment" VIEW-AS TEXT SPACE(3) 
    bfr{&tmptable}.Instal[2] NO-LABEL VIEW-AS TEXT SPACE(3) bfr{&tmptable}.Instal[3] NO-LABEL VIEW-AS TEXT 
    bfr{&tmptable}.Instal[4] NO-LABEL VIEW-AS TEXT SKIP(0.2) 
    bfr{&tmptable}.NextP[1] COLON 30 LABEL "Next Billing Period" space(14)
    bfr{&tmptable}.NextP[2] NO-LABEL space(13) bfr{&tmptable}.NextP[3] NO-LABEL 
    space(11) bfr{&tmptable}.NextP[4] NO-LABEL VIEW-AS TEXT SKIP(0.2)
    bfr{&tmptable}.comm COLON 30 VIEW-AS EDITOR SIZE 60 BY 2
    SKIP(1.0)
    btn-ok colon 30 SPACE(60)
    btn-close 
    SKIP(0.2)
    with view-as dialog-box keep-tab-order no-validate
          side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "LAND SALES ACCOUNT DATA CAPTURE".
    

/* ***** Triggers  **** */
ON CHOOSE OF btnAcc IN FRAME frm-Input
DO:
  VIEW FRAME frm-pickAcc.
  OPEN QUERY qry-pickAcc FOR EACH dbcmf NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickAcc.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pickAcc 
          OR close of THIS-PROCEDURE IN FRAME frm-pickAcc 
          OR CHOOSE OF btn-ok IN FRAME frm-pickAcc 
          OR 'enter':u OF brw-pickAcc
          OR 'mouse-select-dblclick' OF brw-pickAcc.
  CLOSE QUERY qry-pickAcc.
  HIDE FRAME frm-pickAcc.
  APPLY 'tab' TO {&sKey}.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pickAcc 
    OR 'enter':u OF brw-pickAcc
    OR 'mouse-select-dblclick' OF brw-pickAcc
DO: 
   GET CURRENT qry-pickAcc NO-LOCK NO-WAIT.
   DISPLAY dbcmf.dbAcc @ {&sKey} WITH FRAME frm-input.
   CLOSE QUERY qry-pickAcc.
  HIDE FRAME frm-pickAcc.
  APPLY 'tab' TO {&sKey} IN FRAME  frm-Input.
  RETURN. 
END.

ON  'enter':u OF {&sKey} IN FRAME  frm-Input
     OR 'tab':u OF {&sKey} IN FRAME  frm-Input
DO:
   FIND FIRST dbcmf WHERE dbcmf.dbacc = DEC({&sKey}:SCREEN-VALUE) NO-LOCK NO-ERROR.
   IF NOT AVAILABLE dbcmf THEN DO:
      MESSAGE "Account does not exist, please create Debtors Account first" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   ELSE DO:
       FIND FIRST {&tmptable} WHERE  {&skey} = DEC({&sKey}:SCREEN-VALUE) 
                                AND {&tmptable}.stat = 0  NO-ERROR. 
       IF AVAILABLE {&tmptable} THEN
       DO:
          MESSAGE  {&wsMsg} STRING({&sKey}:SCREEN-VALUE) SKIP 
              "You cannot have more than one active land sales on one account" VIEW-AS ALERT-BOX.
          CLEAR FRAME frm-input ALL.
          RETURN NO-APPLY.
       END.
       ELSE DO:
           FIND FIRST dbsmf WHERE dbsmf.suburb = dbcmf.suburb NO-LOCK NO-ERROR.
           IF AVAILABLE dbsmf THEN
               wsdes[1] = dbsmf.Descrip.
           ELSE wsDes[1] = "Invalid Suburb".
           DISPLAY dbcmf.Name dbcmf.Add1 dbcmf.add2 dbcmf.Add3 wsDes  WITH FRAME frm-Input.
           APPLY 'entry' TO bfr{&tmptable}.Scheme IN FRAME  frm-Input.
       END.
   END.
   RETURN.    
END.

ON CHOOSE OF btnScheme IN FRAME frm-Input
DO:
  VIEW FRAME frm-pickScheme.
  OPEN QUERY qry-pickScheme FOR EACH LProj WHERE LProj.ls = wsTransType NO-LOCK.
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
   DISPLAY LProj.Scheme @ bfr{&tmptable}.Scheme LProj.Descrip @ wsDes[2] WITH FRAME frm-input.
   CLOSE QUERY qry-pickScheme.
  HIDE FRAME frm-pickScheme.
  APPLY 'tab' TO bfr{&tmptable}.Scheme.
  RETURN. 
END.

ON  'enter':u OF bfr{&tmptable}.Scheme IN FRAME  frm-Input
     OR 'tab':u OF bfr{&tmptable}.Scheme IN FRAME  frm-Input
DO:
   FIND FIRST LProj WHERE LProj.Scheme = INT(bfr{&tmptable}.Scheme:SCREEN-VALUE)
       AND  LProj.ls = wsTransType NO-LOCK NO-ERROR.
   IF NOT AVAILABLE LProj THEN DO:
      MESSAGE "Project does not exist, please try again" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   ELSE  DO:
       IF Lproj.BillSep = No THEN
        DISABLE bfr{&tmpTable}.INT[2] bfr{&tmpTable}.INT[3] bfr{&tmpTable}.per[2] bfr{&tmpTable}.per[3]
        WITH FRAME frm-input.
    ELSE IF Lproj.BillSep = Yes THEN
        ENABLE bfr{&tmpTable}.INT[2] bfr{&tmpTable}.INT[3] bfr{&tmpTable}.per[2] bfr{&tmpTable}.per[3]
        WITH FRAME frm-input.
        ASSIGN bfr{&tmptable}.int[1]:SCREEN-VALUE = STRING(LProj.INT)
              bfr{&tmptable}.int[2]:SCREEN-VALUE = STRING(LProj.INT)
              bfr{&tmptable}.int[3]:SCREEN-VALUE = STRING(LProj.INT)
              bfr{&tmptable}.int[4]:SCREEN-VALUE = STRING(LProj.INT).
       DISPLAY LProj.Scheme @ bfr{&tmptable}.Scheme LProj.Descrip @ wsDes[2] WITH FRAME frm-input.
   END.   
   RETURN.    
END.

ON 'enter':U OF bfr{&tmptable}.StandNo  
DO:
     FIND FIRST {&tmptable} WHERE bfr{&tmptable}.Scheme = INT(bfr{&tmptable}.Scheme:SCREEN-VALUE)
         AND bfr{&tmptable}.StandNo = bfr{&tmptable}.StandNo:SCREEN-VALUE
         AND bfr{&tmptable}.ls     = wsTransType
         AND bfr{&tmptable}.stat   = 0 NO-LOCK  NO-ERROR. 
       IF AVAILABLE {&tmptable} THEN
       DO:
          MESSAGE "Stand Number " bfr{&tmptable}.StandNo:SCREEN-VALUE " OF Project  "
               LProj.Descrip " is already allocated"
              VIEW-AS ALERT-BOX.
          CLEAR FRAME frm-input ALL.
          APPLY 'entry' TO {&sKey} IN FRAME  frm-Input.
          RETURN NO-APPLY.
       END.
    RETURN.
END.
ON 'enter':U OF bfr{&tmptable}.Amt[1] 
    OR 'leave':U OF bfr{&tmptable}.Amt[1]
    OR 'enter':U OF bfr{&tmptable}.Amt[2] 
    OR 'leave':U OF bfr{&tmptable}.Amt[2]
    OR 'enter':U OF bfr{&tmptable}.Amt[3] 
    OR 'leave':U OF bfr{&tmptable}.Amt[3]
    OR 'enter':U OF bfr{&tmptable}.dAmt[1] 
    OR 'leave':U OF bfr{&tmptable}.dAmt[1]
    OR 'enter':U OF bfr{&tmptable}.dAmt[2] 
    OR 'leave':U OF bfr{&tmptable}.dAmt[2]
    OR 'enter':U OF bfr{&tmptable}.dAmt[3] 
    OR 'leave':U OF bfr{&tmptable}.dAmt[3]
DO:
    RUN sumit.ip.
    RETURN.
END.

ON 'enter':U OF bfr{&tmptable}.INT[1]
    OR 'leave':U OF bfr{&tmptable}.INT[1]
DO:
    IF  Lproj.BillSep = NO THEN
        ASSIGN bfr{&tmptable}.INT[2]:SCREEN-VALUE = STRING(bfr{&tmptable}.INT[1]:SCREEN-VALUE)
               bfr{&tmptable}.INT[3]:SCREEN-VALUE = STRING(bfr{&tmptable}.INT[1]:SCREEN-VALUE).
       bfr{&tmptable}.INT[4]:SCREEN-VALUE = STRING(bfr{&tmptable}.INT[1]:SCREEN-VALUE).
    RETURN.
END.

ON 'enter':U OF bfr{&tmptable}.Per[1]
    OR 'leave':U OF bfr{&tmptable}.Per[1]
DO:
    IF Lproj.BillSep = NO THEN
        ASSIGN bfr{&tmptable}.Per[2]:SCREEN-VALUE = STRING(bfr{&tmptable}.Per[1]:SCREEN-VALUE)
               bfr{&tmptable}.Per[3]:SCREEN-VALUE = STRING(bfr{&tmptable}.Per[1]:SCREEN-VALUE). 
        bfr{&tmptable}.Per[4]:SCREEN-VALUE = STRING(bfr{&tmptable}.Per[1]:SCREEN-VALUE).
    RETURN.
END.

ON 'enter':U OF bfr{&tmptable}.BAmt[1] IN FRAME frm-input
    OR 'leave':U OF bfr{&tmptable}.BAmt[1]
    OR 'enter':U OF bfr{&tmptable}.BAmt[2] 
    OR 'leave':U OF bfr{&tmptable}.BAmt[2]
    OR 'enter':U OF bfr{&tmptable}.BAmt[3] 
    OR 'leave':U OF bfr{&tmptable}.BAmt[3]
DO:
   ASSIGN bfr{&tmptable}.BAmt[4]:SCREEN-VALUE IN FRAME frm-input =  STRING(DEC(bfr{&tmptable}.BAmt[1]:SCREEN-VALUE)
                                                         + DEC(bfr{&tmptable}.BAmt[2]:SCREEN-VALUE)
                                                         + DEC(bfr{&tmptable}.BAmt[3]:SCREEN-VALUE)).
   IF  DEC(bfr{&tmptable}.INT[1]:SCREEN-VALUE) = 0 THEN
       bfr{&tmptable}.Instal[1]:SCREEN-VALUE  = STRING( ROUND(DEC(bfr{&tmptable}.BAmt[1]:SCREEN-VALUE) 
                                                                / DEC(bfr{&tmptable}.Per[1]:SCREEN-VALUE),2)).
   ELSE DO:
       wsInt = DEC(bfr{&tmptable}.INT[1]:SCREEN-VALUE) / 1200.
      bfr{&tmptable}.Instal[1]:screen-value = 
                string(round(dec(bfr{&tmptable}.BAmt[1]:screen-value) * wsInt
                    / (1 - (1 / exp((wsInt + 1), 
                        INT(bfr{&tmptable}.Per[1]:SCREEN-VALUE)))),2)).
   END.
   IF  DEC(bfr{&tmptable}.INT[2]:SCREEN-VALUE) = 0 THEN
       bfr{&tmptable}.Instal[2]:SCREEN-VALUE  = STRING( ROUND(DEC(bfr{&tmptable}.BAmt[2]:SCREEN-VALUE) 
                                                                / DEC(bfr{&tmptable}.Per[2]:SCREEN-VALUE),2)).
    ELSE DO:
       wsInt = DEC(bfr{&tmptable}.INT[2]:SCREEN-VALUE) / 1200.
      bfr{&tmptable}.Instal[2]:screen-value = 
                string(round(dec(bfr{&tmptable}.BAmt[2]:screen-value) * wsInt
                    / (1 - (1 / exp((wsInt + 1), 
                        INT(bfr{&tmptable}.Per[2]:SCREEN-VALUE)))),2)).
    END.
   IF  DEC(bfr{&tmptable}.INT[3]:SCREEN-VALUE) = 0 THEN
       bfr{&tmptable}.Instal[3]:SCREEN-VALUE  = STRING( ROUND(DEC(bfr{&tmptable}.BAmt[3]:SCREEN-VALUE) 
                                                                / DEC(bfr{&tmptable}.Per[3]:SCREEN-VALUE),2)).
    ELSE DO:
       wsInt = DEC(bfr{&tmptable}.INT[3]:SCREEN-VALUE) / 1200.
      bfr{&tmptable}.Instal[3]:screen-value = 
                string(round(dec(bfr{&tmptable}.BAmt[3]:screen-value) * wsInt
                    / (1 - (1 / exp((wsInt + 1), 
                        INT(bfr{&tmptable}.Per[3]:SCREEN-VALUE)))),2)).
    END.
    IF Lproj.BillSep = NO THEN 
        IF  DEC(bfr{&tmptable}.INT[4]:SCREEN-VALUE) = 0 THEN
           bfr{&tmptable}.Instal[4]:SCREEN-VALUE  = STRING( ROUND(DEC(bfr{&tmptable}.BAmt[4]:SCREEN-VALUE) 
                                                                    / DEC(bfr{&tmptable}.Per[4]:SCREEN-VALUE),2)).
        ELSE DO:
           wsInt = DEC(bfr{&tmptable}.INT[4]:SCREEN-VALUE) / 1200.
          bfr{&tmptable}.Instal[4]:screen-value = 
                    string(round(dec(bfr{&tmptable}.BAmt[4]:screen-value) * wsInt
                        / (1 - (1 / exp((wsInt + 1), 
                            INT(bfr{&tmptable}.Per[4]:SCREEN-VALUE)))),2)).
        END.
     ELSE DEC(bfr{&tmptable}.Instal[4]:SCREEN-VALUE) = DEC(bfr{&tmptable}.Instal[1]:SCREEN-VALUE).
    RETURN.
END.

ON 'enter':U OF bfr{&tmptable}.per[1] IN FRAME frm-input
    OR 'leave':U OF bfr{&tmptable}.Per[1] IN FRAME frm-input
DO:
   ASSIGN bfr{&tmptable}.per[4]:SCREEN-VALUE = bfr{&tmptable}.per[1]:SCREEN-VALUE.
   IF  LProj.BillSep = NO THEN
       ASSIGN bfr{&tmptable}.per[2]:SCREEN-VALUE = bfr{&tmptable}.per[1]:SCREEN-VALUE
              bfr{&tmptable}.per[3]:SCREEN-VALUE = bfr{&tmptable}.per[1]:SCREEN-VALUE.
    RETURN.
END.

ON 'enter':U OF bfr{&tmptable}.NextP[1] IN FRAME frm-input
    OR 'leave':U OF bfr{&tmptable}.NextP[1] IN FRAME frm-input
DO:
   IF  LProj.BillSep = NO THEN
        ASSIGN bfr{&tmptable}.NextP[2]:SCREEN-VALUE = STRING(bfr{&tmptable}.NextP[1]:SCREEN-VALUE)
               bfr{&tmptable}.NextP[3]:SCREEN-VALUE = STRING(bfr{&tmptable}.NextP[1]:SCREEN-VALUE). 
   ASSIGN bfr{&tmptable}.NextP[4]:SCREEN-VALUE = bfr{&tmptable}.NextP[1]:SCREEN-VALUE.
    RETURN.
END.

ON CHOOSE OF btn-Add IN FRAME frm-main DO:
     btn-ok:LABEL IN  FRAME frm-input = "Save".
    RUN proc-input.
    RELEASE bfr{&tmptable}.
    RETURN.
END.

ON CHOOSE OF btn-edit
    OR 'enter':u OF brw-{&tmptable}
    OR 'mouse-select-dblclick' OF brw-{&tmptable}
DO:
    btn-ok:LABEL IN  FRAME frm-input = "Update".
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    DISABLE {&skey} WITH FRAME frm-input.
    run proc-edit.
    RELEASE bfr{&tmptable}.
    RETURN.
END.


ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
    IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN DO:
         FIND FIRST dbcmf WHERE dbcmf.dbacc = DEC({&skey}:SCREEN-VALUE IN FRAME frm-input) NO-ERROR.
     IF NOT AVAILABLE dbcmf THEN DO:
        MESSAGE "Invalid Debtors Account supplied...no record saved" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
     END.
     FIND FIRST LProj WHERE LProj.Scheme  = INT(bfr{&tmptable}.Scheme:SCREEN-VALUE IN FRAME frm-input) NO-ERROR.
      IF NOT AVAILABLE LProj THEN DO:
          MESSAGE  "Invalid Project entered ... no record saved" VIEW-AS ALERT-BOX.
          RETURN NO-APPLY.
      END.
      ELSE DO:
          CREATE bfr{&tmptable}. 
          ASSIGN bfr{&tmptable}.dbacc = DEC({&skey}:SCREEN-VALUE)
                 bfr{&tmptable}.Scheme = INT(bfr{&tmptable}.Scheme:SCREEN-VALUE)
                 bfr{&tmptable}.StandNo = bfr{&tmptable}.StandNo:SCREEN-VALUE IN FRAME frm-input
                 bfr{&tmptable}.PDate = DATE(bfr{&tmptable}.PDate:SCREEN-VALUE)
                 bfr{&tmptable}.ls = wsTransType
                 /*bfr{&tmptable}.BillSep = LProj.BillSep */
                 bfr{&tmptable}.CreDate = TODAY
                 bfr{&tmptable}.Amt[1] = DEC(bfr{&tmptable}.Amt[1]:SCREEN-VALUE)
                 bfr{&tmptable}.Amt[2] = DEC(bfr{&tmptable}.Amt[2]:SCREEN-VALUE)
                 bfr{&tmptable}.Amt[3] = DEC(bfr{&tmptable}.Amt[3]:SCREEN-VALUE)
                 bfr{&tmptable}.Amt[4] = DEC(bfr{&tmptable}.Amt[4]:SCREEN-VALUE)
                 bfr{&tmptable}.DAmt[1] = DEC(bfr{&tmptable}.DAmt[1]:SCREEN-VALUE)
                 bfr{&tmptable}.DAmt[2] = DEC(bfr{&tmptable}.DAmt[2]:SCREEN-VALUE)
                 bfr{&tmptable}.DAmt[3] = DEC(bfr{&tmptable}.DAmt[3]:SCREEN-VALUE)
                 bfr{&tmptable}.DAmt[4] = DEC(bfr{&tmptable}.DAmt[4]:SCREEN-VALUE)
                 bfr{&tmptable}.SAmt[1] = DEC(bfr{&tmptable}.SAmt[1]:SCREEN-VALUE)
                 bfr{&tmptable}.SAmt[2] = DEC(bfr{&tmptable}.SAmt[2]:SCREEN-VALUE)
                 bfr{&tmptable}.SAmt[3] = DEC(bfr{&tmptable}.SAmt[3]:SCREEN-VALUE)
                 bfr{&tmptable}.SAmt[4] = DEC(bfr{&tmptable}.SAmt[4]:SCREEN-VALUE)
                 bfr{&tmptable}.BAmt[1] = DEC(bfr{&tmptable}.BAmt[1]:SCREEN-VALUE)
                 bfr{&tmptable}.BAmt[2] = DEC(bfr{&tmptable}.BAmt[2]:SCREEN-VALUE)
                 bfr{&tmptable}.BAmt[3] = DEC(bfr{&tmptable}.BAmt[3]:SCREEN-VALUE)
                 bfr{&tmptable}.BAmt[4] = DEC(bfr{&tmptable}.BAmt[4]:SCREEN-VALUE)
                 bfr{&tmptable}.Per[1] = INT(bfr{&tmptable}.Per[1]:SCREEN-VALUE)
                 bfr{&tmptable}.Per[2] = INT(bfr{&tmptable}.Per[2]:SCREEN-VALUE)
                 bfr{&tmptable}.Per[3] = INT(bfr{&tmptable}.Per[3]:SCREEN-VALUE)
                 bfr{&tmptable}.Per[4] = INT(bfr{&tmptable}.Per[4]:SCREEN-VALUE)
                 bfr{&tmptable}.INT[1] = DEC(bfr{&tmptable}.INT[1]:SCREEN-VALUE)
                 bfr{&tmptable}.INT[2] = DEC(bfr{&tmptable}.INT[2]:SCREEN-VALUE)
                 bfr{&tmptable}.INT[3] = DEC(bfr{&tmptable}.INT[3]:SCREEN-VALUE)
                 bfr{&tmptable}.INT[4] = DEC(bfr{&tmptable}.INT[4]:SCREEN-VALUE)
                 bfr{&tmptable}.Instal[1] = DEC(bfr{&tmptable}.Instal[1]:SCREEN-VALUE)
                 bfr{&tmptable}.Instal[2] = DEC(bfr{&tmptable}.Instal[2]:SCREEN-VALUE)
                 bfr{&tmptable}.Instal[3] = DEC(bfr{&tmptable}.Instal[3]:SCREEN-VALUE)
                 bfr{&tmptable}.Instal[4] = DEC(bfr{&tmptable}.Instal[4]:SCREEN-VALUE)
                 bfr{&tmptable}.NextP[1] = INT(bfr{&tmptable}.NextP[1]:SCREEN-VALUE)
                 bfr{&tmptable}.NextP[2] = INT(bfr{&tmptable}.NextP[2]:SCREEN-VALUE)
                 bfr{&tmptable}.NextP[3] = INT(bfr{&tmptable}.NextP[3]:SCREEN-VALUE)
                 bfr{&tmptable}.NextP[4] = INT(bfr{&tmptable}.NextP[4]:SCREEN-VALUE)
                 bfr{&tmptable}.stat     = 0
                 bfr{&tmptable}.comm     = bfr{&tmptable}.comm:SCREEN-VALUE IN FRAME frm-input.
          IF wsTransType = 2 THEN DO:
              DO X = 1 TO 3:
                  ASSIGN wsCharge = bfr{&tmptable}.DAmt[X]
                    wsTotal  = bfr{&tmptable}.Amt[X]
                     wsbt     = LProj.svgrp[X]
                     wsLedger = LProj.iLedger[X]
                     varDescrip = "Deposit Land Sales " 
                     varDescrip = "Deposit Survey Cost "  WHEN X = 2
                     varDescrip = "Deposit Admin/Renewal Cost "  WHEN X = 3
                     wsVat      = (wsTotal * LProj.Vat%[X]) / (LProj.Vat%[X] + 100).
                  IF wsTotal > 0 THEN RUN writ.ip.
              END. /* eof DO x */
          END.  
          RELEASE {&tmptable}.
          brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
          CLEAR FRAME frm-input ALL.
           APPLY 'entry' TO {&skey} IN FRAME frm-Input.
      END.
    END.
    ELSE DO:
        FIND FIRST dbcmf WHERE dbcmf.dbacc = DEC({&skey}:SCREEN-VALUE IN FRAME frm-input) NO-ERROR.
        IF NOT AVAILABLE dbcmf THEN DO:
            MESSAGE "Invalid Debtors Account supplied...no record saved" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
         END.
         FIND FIRST LProj WHERE LProj.Scheme  = INT(bfr{&tmptable}.Scheme:SCREEN-VALUE IN FRAME frm-input) NO-ERROR.
          IF NOT AVAILABLE LProj THEN DO:
              MESSAGE  "Invalid Project entered ... no record saved" VIEW-AS ALERT-BOX.
              RETURN NO-APPLY.
          END.
        ELSE
        ASSIGN bfr{&tmptable}.StandNo = bfr{&tmptable}.StandNo:SCREEN-VALUE IN FRAME frm-input
                 bfr{&tmptable}.PDate = DATE(bfr{&tmptable}.PDate:SCREEN-VALUE)
                 bfr{&tmptable}.BAmt[1] = DEC(bfr{&tmptable}.BAmt[1]:SCREEN-VALUE)
                 bfr{&tmptable}.BAmt[2] = DEC(bfr{&tmptable}.BAmt[2]:SCREEN-VALUE)
                 bfr{&tmptable}.BAmt[3] = DEC(bfr{&tmptable}.BAmt[3]:SCREEN-VALUE)
                 bfr{&tmptable}.BAmt[4] = DEC(bfr{&tmptable}.BAmt[4]:SCREEN-VALUE)
                 bfr{&tmptable}.Per[1] = INT(bfr{&tmptable}.Per[1]:SCREEN-VALUE)
                 bfr{&tmptable}.Per[2] = INT(bfr{&tmptable}.Per[2]:SCREEN-VALUE)
                 bfr{&tmptable}.Per[3] = INT(bfr{&tmptable}.Per[3]:SCREEN-VALUE)
                 bfr{&tmptable}.Per[4] = INT(bfr{&tmptable}.Per[4]:SCREEN-VALUE)
                 bfr{&tmptable}.INT[1] = DEC(bfr{&tmptable}.INT[1]:SCREEN-VALUE)
                 bfr{&tmptable}.INT[2] = DEC(bfr{&tmptable}.INT[2]:SCREEN-VALUE)
                 bfr{&tmptable}.INT[3] = DEC(bfr{&tmptable}.INT[3]:SCREEN-VALUE)
                 bfr{&tmptable}.INT[4] = DEC(bfr{&tmptable}.INT[4]:SCREEN-VALUE)
                 bfr{&tmptable}.Instal[1] = DEC(bfr{&tmptable}.Instal[1]:SCREEN-VALUE)
                 bfr{&tmptable}.Instal[2] = DEC(bfr{&tmptable}.Instal[2]:SCREEN-VALUE)
                 bfr{&tmptable}.Instal[3] = DEC(bfr{&tmptable}.Instal[3]:SCREEN-VALUE)
                 bfr{&tmptable}.Instal[4] = DEC(bfr{&tmptable}.Instal[4]:SCREEN-VALUE)
                 bfr{&tmptable}.NextP[1] = INT(bfr{&tmptable}.NextP[1]:SCREEN-VALUE)
                 bfr{&tmptable}.NextP[2] = INT(bfr{&tmptable}.NextP[2]:SCREEN-VALUE)
                 bfr{&tmptable}.NextP[3] = INT(bfr{&tmptable}.NextP[3]:SCREEN-VALUE)
                 bfr{&tmptable}.NextP[4] = INT(bfr{&tmptable}.NextP[4]:SCREEN-VALUE)
                 bfr{&tmptable}.stat     = 0
                 bfr{&tmptable}.comm      = bfr{&tmptable}.comm:SCREEN-VALUE IN FRAME frm-input.
        RELEASE {&tmptable}.
    END.
END.

ON CHOOSE OF btn-Del DO:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.dbAcc.
    FIND FIRST dbhtf WHERE dbhtf.dbAcc  = bfr{&tmptable}.dbAcc NO-ERROR.
    IF AVAILABLE dbhtf THEN DO:
        MESSAGE "Account has related records - cannot be delete"
            VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE IF NOT AVAILABLE dbhtf THEN DO:
     DELETE bfr{&tmptable}.
     Method-Status = brw-{&tmptable}:DELETE-SELECTED-ROWS().
    END.
    APPLY 'entry' TO btn-exit.
END.

/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
ASSIGN vatLedger = simctr.vat[1]
            wsdate = TODAY
            wsPer = SIMCTR.CURPER
            wsTime    = STRING(TIME,"HH:MM:SS").
            wsTransID = DEC (string(YEAR(TODAY),"9999")
                          + string(MONTH(TODAY),"99" )
                          + string(DAY(TODAY),"99")
                          + SUBSTR(STRING(wsTIME),1,2) 
                          + SUBSTR(STRING(wsTIME),4,2) 
                          + SUBSTR(STRING(wsTIME),7,2) ).
OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.ls = wsTransType NO-LOCK .
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
CLOSE QUERY qry-{&tmptable}.
HIDE FRAME frm-main.

PROCEDURE proc-input:
   CLEAR FRAME frm-input ALL.
   bfr{&tmptable}.LS:SCREEN-VALUE = STRING(wsTransType).
   ENABLE ALL WITH FRAME frm-input.
   {&skey}:SCREEN-VALUE = "0".
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
   CLOSE QUERY qry-{&tmptable}.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.ls = wsTransType NO-LOCK.
   HIDE FRAME frm-input.
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="prn.ip"
                    &paged}
END.

PROCEDURE proc-edit:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = int(bfr{&tmptable}.dbAcc).
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.dbAcc  = wsid NO-ERROR.
    FIND FIRST dbcmf WHERE dbcmf.dbAcc = wsId NO-LOCK NO-ERROR.
    FIND FIRST dbsmf WHERE dbsmf.suburb = dbcmf.suburb NO-LOCK NO-ERROR.
    IF AVAILABLE dbsmf THEN
        wsdes[1] = dbsmf.Descrip.
    ELSE wsDes[1] = "Invalid Suburb".
    DISPLAY dbcmf.Name dbcmf.Add1 dbcmf.add2 dbcmf.Add3 wsid @ {&skey} {&updfields} wsDes  WITH FRAME frm-Input.
    ENABLE bfr{&tmpTable}.INT bfr{&tmpTable}.per bfr{&tmpTable}.NextP bfr{&tmpTable}.BAmt bfr{&tmpTable}.standNo 
        bfr{&tmpTable}.PDate btn-close btn-ok bfr{&tmptable}.comm WITH FRAME frm-input.
    IF   LProj.BillSep = No THEN
        DISABLE bfr{&tmpTable}.INT[2] bfr{&tmpTable}.INT[3] bfr{&tmpTable}.per[2] bfr{&tmpTable}.per[3]
        bfr{&tmpTable}.NextP[2] bfr{&tmpTable}.NextP[3] WITH FRAME frm-input.
    ELSE
        ENABLE bfr{&tmpTable}.INT bfr{&tmpTable}.BAmt bfr{&tmpTable}.per bfr{&tmpTable}.NextP
        WITH FRAME frm-input.
    WAIT-FOR CHOOSE OF btn-close 
          OR CHOOSE OF btn-ok OR close of THIS-PROCEDURE IN FRAME frm-input.
   CLOSE QUERY qry-{&tmptable}.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.ls = wsTransType  NO-LOCK.
   HIDE FRAME frm-input.
 END.

PROCEDURE sumit.ip:
    ASSIGN bfr{&tmptable}.Amt[4]:SCREEN-VALUE IN FRAME frm-input =  STRING(DEC(bfr{&tmptable}.Amt[1]:SCREEN-VALUE)
                                                      + DEC(bfr{&tmptable}.Amt[2]:SCREEN-VALUE)
                                                      + DEC(bfr{&tmptable}.Amt[3]:SCREEN-VALUE))
           bfr{&tmptable}.DAmt[4]:SCREEN-VALUE IN FRAME frm-input =  STRING(DEC(bfr{&tmptable}.DAmt[1]:SCREEN-VALUE)
                                                      + DEC(bfr{&tmptable}.DAmt[2]:SCREEN-VALUE)
                                                      + DEC(bfr{&tmptable}.DAmt[3]:SCREEN-VALUE))
          bfr{&tmptable}.SAmt[1]:SCREEN-VALUE IN FRAME frm-input =  STRING(DEC(bfr{&tmptable}.Amt[1]:SCREEN-VALUE)
                                                      - DEC(bfr{&tmptable}.DAmt[1]:SCREEN-VALUE))
          bfr{&tmptable}.SAmt[2]:SCREEN-VALUE IN FRAME frm-input =  STRING(DEC(bfr{&tmptable}.Amt[2]:SCREEN-VALUE)
                                                      - DEC(bfr{&tmptable}.DAmt[2]:SCREEN-VALUE))
          bfr{&tmptable}.SAmt[3]:SCREEN-VALUE IN FRAME frm-input =  STRING(DEC(bfr{&tmptable}.Amt[3]:SCREEN-VALUE)
                                                      - DEC(bfr{&tmptable}.DAmt[3]:SCREEN-VALUE))
          bfr{&tmptable}.SAmt[4]:SCREEN-VALUE IN FRAME frm-input =  STRING(DEC(bfr{&tmptable}.SAmt[1]:SCREEN-VALUE)
                                                      + DEC(bfr{&tmptable}.SAmt[2]:SCREEN-VALUE)
                                                      + DEC(bfr{&tmptable}.SAmt[3]:SCREEN-VALUE))
          bfr{&tmptable}.BAmt[1]:SCREEN-VALUE IN FRAME frm-input =  STRING(DEC(bfr{&tmptable}.Amt[1]:SCREEN-VALUE)
                                                      - DEC(bfr{&tmptable}.DAmt[1]:SCREEN-VALUE))
          bfr{&tmptable}.BAmt[2]:SCREEN-VALUE IN FRAME frm-input =  STRING(DEC(bfr{&tmptable}.Amt[2]:SCREEN-VALUE)
                                                      - DEC(bfr{&tmptable}.DAmt[2]:SCREEN-VALUE))
          bfr{&tmptable}.BAmt[3]:SCREEN-VALUE IN FRAME frm-input =  STRING(DEC(bfr{&tmptable}.Amt[3]:SCREEN-VALUE)
                                                      - DEC(bfr{&tmptable}.DAmt[3]:SCREEN-VALUE)).
               
END.

PROCEDURE Writ.ip:
    FIND FIRST dbcmf WHERE dbcmf.dbacc = bfr{&tmptable}.dbAcc NO-ERROR.
    IF wsCharge <> 0.00 THEN DO: /* Deposit transactions */
        dbcmf.AccBal = dbcmf.AccBal + wsCharge.
                /* update Balance record */
        FIND FIRST dbsgr WHERE dbsgr.sgrp = wsBt  NO-LOCK NO-ERROR.
        FIND FIRST glmf WHERE glmf.acct = wsLedger NO-LOCK NO-ERROR.
        FIND FIRST  dbblf WHERE dbblf.dbacc = DEC(dbcmf.dbacc) 
                          AND dbblf.Sgrp = dbsgr.sgrp NO-ERROR.
        IF NOT AVAILABLE dbblf THEN DO:
               CREATE dbblf.
               ASSIGN dbblf.dbacc =  dbcmf.dbacc
                      dbblf.Sgrp  =  dbsgr.sgrp.
        END.
        ASSIGN dbblf.amt[1] = dbblf.amt[1] + wscharge.
                /* create monthly transaction */
       CREATE dbmtf.
       ASSIGN dbmtf.Accper  = wsPer
              dbmtf.Amt     = wsCharge
              dbmtf.Vat     = (wsCharge * LProj.Vat%[X]) / (LProj.Vat%[X] + 100)
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
              dbhtf.Vat     = (wsCharge * LProj.Vat%[X]) / (LProj.Vat%[X] + 100) 
              dbhtf.Iledger = wsLedger
              dbhtf.proj    = glmf.proj
              dbhtf.dept    = glmf.dept
              dbhtf.ref     = "Debit" + string(wsPer)
              dbhtf.Sgrp    = dbsgr.Sgrp
              dbhtf.Tarif   = 0
              dbhtf.trDate  = wsDate
              dbhtf.PoDate  = TODAY
              dbhtf.prog    = "dbsg08.p"
              dbhtf.TransID = wsTransId  
              dbhtf.uid     = varUser
              dbhtf.DESCRIP = varDescrip. 
    END. /* eof Charge > 0 */
    IF wsVat > 0 THEN DO:  /* Create VAT Provision Ledger  Record */
       FIND FIRST dbgl WHERE dbgl.period = wsper  AND dbgl.TransID = wsTransId AND 
                              dbgl.acct = vatLedger AND dbgl.dept = glmf.dept AND
                              dbgl.proj = glmf.proj NO-ERROR.
        IF NOT AVAILABLE dbgl THEN DO:
                   CREATE dbgl.
                   ASSIGN dbgl.acct = vatLedger
                          dbgl.proj = glmf.proj
                          dbgl.dept = glmf.dept
                          dbgl.period = wsper
                          dbgl.TransID = wsTransId
                          dbgl.trDATE  = wsDate
                          dbgl.UID     = varUser
                          dbgl.REF     = "dbsg08.p"
                          dbgl.DESCRIP = "Land Sales Deposit"
                          dbgl.CREDATE = TODAY
                          dbgl.SOURCE = wsSource.
        END.
        ASSIGN dbgl.AMT = dbgl.AMT + (wsVat * -1).
    END.
    /* create record for Ledger transaction */
    DO t = 1 TO 2:
       IF wsTotal > 0 THEN DO:
               ASSIGN  wsLedger = lProj.Iledger[4] WHEN t = 2.
               FIND FIRST glmf WHERE glmf.acct = wsLedger NO-LOCK NO-ERROR.
               FIND FIRST dbgl WHERE dbgl.acct = wsLedger AND dbgl.period = wsper
                               AND dbgl.TransID = wsTransId NO-ERROR.
                IF NOT AVAILABLE dbgl THEN DO:
                       CREATE dbgl.
                       ASSIGN dbgl.acct = wsLedger
                              dbgl.proj = glmf.proj
                              dbgl.dept = glmf.dept
                              dbgl.fund = glmf.fund
                              dbgl.period = wsper
                              dbgl.TransID = wsTransId
                              dbgl.trDATE  = wsDate
                              dbgl.UID     = varUser
                              dbgl.REF     = "dbsg08.p"
                              dbgl.DESCRIP = "Land Sales"
                              dbgl.CREDATE = TODAY
                              dbgl.SOURCE = wsSource.
                END.
                ASSIGN dbgl.AMT = dbgl.AMT + ((wsTotal - wsVat) * -1) WHEN t = 1
                       dbgl.AMT = dbgl.AMT + wsTotal  WHEN t = 2.
       END.
       IF wsCharge > 0 THEN DO:  /* Create record for the Control Ledger records */
              ASSIGN  wsLedger = lProj.Iledger[4] WHEN t = 2
                      wsLedger = dbsgr.ctrLedger  WHEN t = 1.
              FIND FIRST glmf WHERE glmf.acct = wsLedger NO-LOCK NO-ERROR.
              FIND FIRST dbgl WHERE dbgl.acct = wsLedger AND dbgl.period = wsper
                              AND dbgl.TransID = wsTransId AND dbgl.dept = dbsgr.dept AND
                                      dbgl.proj = dbsgr.proj NO-ERROR.
              IF NOT AVAILABLE dbgl THEN DO:
                      CREATE dbgl.
                      ASSIGN dbgl.acct = wsLedger
                             dbgl.proj = glmf.proj
                             dbgl.dept = glmf.dept
                             dbgl.fund = glmf.fund
                             dbgl.period = wsper
                             dbgl.TransID = wsTransId
                             dbgl.trDATE  = wsDate
                             dbgl.UID     = varUser
                             dbgl.REF     = "dbsg08.p"
                             dbgl.DESCRIP = "Land Sales Deposit"
                             dbgl.CREDATE = TODAY
                             dbgl.SOURCE = wsSource.
              END.
              ASSIGN dbgl.AMT = dbgl.AMT + wscharge  WHEN t = 1
              dbgl.AMT = dbgl.AMT + (wscharge * -1)  WHEN t = 2.
       END.
    END. /* eof do 1 to 2 */
    ASSIGN wsTotal = wsTotal + wsCharge - wsVat
    wsTVat  = wsTVat  + wsVat.
    RETURN.
END.

PROCEDURE prn.ip:
   {glcon.i}
END.
