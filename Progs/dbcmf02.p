
/* Program.................dbcmf02.p
   Notes:...... Lease/Rental file maintenance
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
&SCOPED-DEFINE wsMsg          "Account already exist"
&SCOPED-DEFINE wsTitle        "LEASE/RENTAL DATA ACCOUNT FILE MAINTENANCE"
&SCOPED-DEFINE tmptable       landSale
&SCOPED-DEFINE skey           {&tmptable}.dbAcc
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.ls ~
                              bfr{&tmptable}.scheme ~
                              bfr{&tmptable}.StandNo  ~
                              bfr{&tmptable}.PDate ~
                              bfr{&tmptable}.Amt[1] ~
                              bfr{&tmptable}.Amt[3] ~
                              bfr{&tmptable}.Amt[4] ~
                              bfr{&tmptable}.NextP[1] ~
                              bfr{&tmptable}.NextP[3] ~
                              bfr{&tmptable}.NextP[4] ~
                              bfr{&tmptable}.comm

&SCOPED-DEFINE tmpFields      bfr{&tmptable}.dbAcc ~
                                        COLUMN-LABEL ' ACCOUNT ':C ~
                              bfr{&tmptable}.Scheme ~
                                        COLUMN-LABEL ' SCHEME ':C ~
                               bfr{&tmptable}.StandNo ~
                                        COLUMN-LABEL 'PROPERTY ':C ~
                               bfr{&tmptable}.PDate ~
                                        COLUMN-LABEL 'DATE ':C ~
                               bfr{&tmptable}.LS ~
                                        COLUMN-LABEL ' TYPE ':C ~
                               bfr{&tmptable}.Amt[1] ~
                                        COLUMN-LABEL 'RENTAL/LEASE ':C ~
                               bfr{&tmptable}.AMT[3] ~
                                        COLUMN-LABEL 'ADMIN/RENEWAL':C ~
                               bfr{&tmptable}.NEXTP[1] ~
                                        COLUMN-LABEL 'NEXT BILLING':C
                              
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
     SIZE 110 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 110 by 18.5.

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
    bfr{&tmptable}.LS COLON 30 LABEL "Type" NO-TAB-STOP SPACE(5)
    btnScheme COLON 30 NO-TAB-STOP 
    bfr{&tmptable}.scheme NO-LABEL wsDes[2]NO-TAB-STOP NO-LABEL SKIP(0.2)
     "----------------------------------PROPERTY DETAILS------------------------------" COLON 10
    bfr{&tmptable}.StandNo COLON 30 LABEL "Property Number" SPACE(5)
    bfr{&tmptable}.PDate  LABEL "Start Date" SKIP(0.2)
    "Rental/Lease            Admin/Renewal Cost          Total Cost" COLON 35
    bfr{&tmptable}.Amt[1] COLON 30 LABEL "Amount" bfr{&tmptable}.Amt[3] NO-LABEL
    SPACE(10)  bfr{&tmptable}.Amt[4] NO-LABEL VIEW-AS TEXT SKIP(0.2)
    bfr{&tmptable}.NextP[1] COLON 30 LABEL "Next Billing Period" space(13) bfr{&tmptable}.NextP[3] NO-LABEL 
    space(14) bfr{&tmptable}.NextP[4] NO-LABEL VIEW-AS TEXT SKIP(0.2)
    bfr{&tmptable}.comm COLON 30 VIEW-AS EDITOR SIZE 60 BY 2
    SKIP(1.0)
    btn-ok colon 30 SPACE(60)
    btn-close 
    SKIP(0.2)
    with view-as dialog-box keep-tab-order no-validate
          side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "LEASE/RENTAL DATA CAPTURE".


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
  APPLY 'tab' TO {&sKey} IN FRAME  frm-Input .
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
           APPLY 'entry' TO  bfr{&tmptable}.Scheme.
       END.
   END.
   RETURN.    
END.

ON 'enter':U OF bfr{&tmptable}.Amt[1] 
    OR 'enter':U OF bfr{&tmptable}.Amt[3] 
DO:
     bfr{&tmptable}.Amt[4]:SCREEN-VALUE =  STRING(DEC(bfr{&tmptable}.Amt[1]:SCREEN-VALUE) 
                                                + dec(bfr{&tmptable}.Amt[3]:SCREEN-VALUE)). 
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
   ELSE 
       DISPLAY LProj.Scheme @ bfr{&tmptable}.Scheme LProj.Descrip @ wsDes[2] WITH FRAME frm-input.
   RETURN.    
END.

ON 'enter':U OF bfr{&tmptable}.StandNo IN FRAME Frm-input 
    OR 'tab':U OF bfr{&tmptable}.StandNo IN FRAME Frm-input 
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
          APPLY 'entry' TO {&sKey}.
          RETURN NO-APPLY.
       END.
    RETURN.
END.

ON 'enter':U OF bfr{&tmptable}.NextP[1] IN FRAME frm-input
    OR 'leave':U OF bfr{&tmptable}.NextP[1] IN FRAME frm-input
DO:
   ASSIGN bfr{&tmptable}.NextP[3]:SCREEN-VALUE = STRING(bfr{&tmptable}.NextP[1]:SCREEN-VALUE) 
          bfr{&tmptable}.NextP[4]:SCREEN-VALUE = STRING(bfr{&tmptable}.NextP[1]:SCREEN-VALUE).
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
    DISABLE ALL WITH FRAME frm-input.
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
                 /*bfr{&tmptable}.BillSep = NO */
                 bfr{&tmptable}.CreDate = TODAY
                 bfr{&tmptable}.Amt[1] = DEC(bfr{&tmptable}.Amt[1]:SCREEN-VALUE)
                 bfr{&tmptable}.Amt[3] = DEC(bfr{&tmptable}.Amt[3]:SCREEN-VALUE)
                 bfr{&tmptable}.Amt[4] = DEC(bfr{&tmptable}.Amt[4]:SCREEN-VALUE)
                 bfr{&tmptable}.NextP[1] = INT(bfr{&tmptable}.NextP[1]:SCREEN-VALUE)
                 bfr{&tmptable}.NextP[3] = INT(bfr{&tmptable}.NextP[3]:SCREEN-VALUE)
                 bfr{&tmptable}.NextP[4] = INT(bfr{&tmptable}.NextP[4]:SCREEN-VALUE)
                 bfr{&tmptable}.stat     = 0
                 bfr{&tmptable}.comm     = bfr{&tmptable}.comm:SCREEN-VALUE IN FRAME frm-input.
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
                 bfr{&tmptable}.Amt[1] = DEC(bfr{&tmptable}.Amt[1]:SCREEN-VALUE)
                 bfr{&tmptable}.Amt[3] = DEC(bfr{&tmptable}.Amt[3]:SCREEN-VALUE)
                 bfr{&tmptable}.Amt[4] = DEC(bfr{&tmptable}.Amt[4]:SCREEN-VALUE)
                 bfr{&tmptable}.NextP[1] = INT(bfr{&tmptable}.NextP[1]:SCREEN-VALUE)
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
            wsPer = SIMCTR.CURPER.
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
END.

PROCEDURE proc-edit:
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = int(bfr{&tmptable}.dbAcc).
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.dbAcc  = wsid NO-ERROR.
    FIND FIRST LProj WHERE LProj.scheme  = bfr{&tmptable}.scheme NO-LOCK NO-ERROR.
    FIND FIRST dbcmf WHERE dbcmf.dbAcc = wsId NO-LOCK NO-ERROR.
    FIND FIRST dbsmf WHERE dbsmf.suburb = dbcmf.suburb NO-LOCK NO-ERROR.
    IF AVAILABLE dbsmf THEN
        wsdes[1] = dbsmf.Descrip.
    ELSE wsDes[1] = "Invalid Suburb".
    DISPLAY dbcmf.Name dbcmf.Add1 dbcmf.add2 dbcmf.Add3 wsid @ {&skey} {&updfields} 
        LProj.Descrip @ wsDes[2] WITH FRAME frm-Input.
    ENABLE bfr{&tmpTable}.Amt[1] bfr{&tmpTable}.Amt[3] bfr{&tmpTable}.NextP[1] bfr{&tmpTable}.NextP[3] 
        bfr{&tmpTable}.PDate btn-close btn-ok bfr{&tmptable}.comm WITH FRAME frm-input.
    WAIT-FOR CHOOSE OF btn-close 
          OR CHOOSE OF btn-ok OR close of THIS-PROCEDURE IN FRAME frm-input.
   CLOSE QUERY qry-{&tmptable}.
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.ls = wsTransType  NO-LOCK.
   HIDE FRAME frm-input.
 END.
