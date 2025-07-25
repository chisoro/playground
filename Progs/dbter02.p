session:DATA-ENTRY-RETURN = TRUE.
/* Program.................dbter02.p
   Notes:...... ...........lAND SALES Withdrawal
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsMsg          "Account already exist"
&SCOPED-DEFINE wsTitle        "LAND SALES WITHDRAWAL/CLOSURE"
&SCOPED-DEFINE tmptable       Landsale
&SCOPED-DEFINE skey           {&tmptable}.dbAcc
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.Amt[1] ~
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
DEF VAR X AS INT.

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 116 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 116 by 22.8.

DEF    QUERY qry-acc FOR bfr{&tmptable} SCROLLING.
DEF BROWSE brw-acc QUERY qry-acc
    DISPLAY bfr{&tmptable}.scheme COLUMN-LABEL "SCHEME" wsDes[2] COLUMN-LABEL "DESCRIPTION" FORM "x(30)" 
            bfr{&tmptable}.StandNo COLUMN-LABEL "STAND NO" bfr{&tmptable}.PDate  COLUMN-LABEL "PDATE " 
            bfr{&tmptable}.Amt[4] COLUMN-LABEL "LAND COST" bfr{&tmptable}.BAmt[4] COLUMN-LABEL "BALANCE" 
            bfr{&tmptable}.stats COLUMN-LABEL "STATUS" WITH 3 DOWN SEPARATORS.

define frame frm-main
    SKIP(0.2) "------------------------------------------------------------ACCOUNT DETAILS------------------------------------------------------------" 
    COLON 10 SKIP(0.5)
    btnAcc NO-TAB-STOP COLON 20 {&skey}     NO-LABEL SPACE(5) 
    dbcmf.NAME LABEL "Accout Name" VIEW-AS TEXT SKIP(.2)
    dbcmf.Add1 COLON 30 LABEL "Postal Address" VIEW-AS TEXT
    dbcmf.add2 COLON 30 NO-LABEL VIEW-AS TEXT 
    dbcmf.Add3 COLON 30 NO-LABEL VIEW-AS text
    wsDes[1]      COLON 30 NO-LABEL VIEW-AS TEXT SKIP(0.2)
    brw-acc COLON 10 SKIP(0.2)
     "-------------------------------------------------PROPERTY DETAILS----------------------
---------------------------------------------------------" COLON 10
    "Land Costs            Survey Costs           Admin/Renewal Cost          Total Cost" COLON 35
    bfr{&tmptable}.Amt[1] COLON 30 LABEL "Original Cost" bfr{&tmptable}.Amt[2] NO-LABEL
    bfr{&tmptable}.Amt[3] NO-LABEL bfr{&tmptable}.Amt[4] NO-LABEL  SKIP(0.2)
    bfr{&tmptable}.DAmt[1] COLON 30 LABEL "Deposit" bfr{&tmptable}.DAmt[2] NO-LABEL
    bfr{&tmptable}.DAmt[3] NO-LABEL bfr{&tmptable}.DAmt[4] NO-LABEL  SKIP(0.2) 
    bfr{&tmptable}.SAmt[1] COLON 30 LABEL "Sub-Total"  
    bfr{&tmptable}.SAmt[2] NO-LABEL bfr{&tmptable}.SAmt[3]NO-LABEL
    bfr{&tmptable}.SAmt[4] NO-LABEL  SKIP(0.2)
    bfr{&tmptable}.INT[1]  COLON 30 LABEL "Interest %" space(15) bfr{&tmptable}.INT[2] NO-LABEL
    space(10) bfr{&tmptable}.INT[3] NO-LABEL space(10) bfr{&tmptable}.INT[4] NO-LABEL SKIP(0.2)
    bfr{&tmptable}.Per[1] COLON 30 LABEL "Periods" space(18) bfr{&tmptable}.Per[2] NO-LABEL
    space(12)bfr{&tmptable}.Per[3] NO-LABEL space(13) bfr{&tmptable}.Per[4] NO-LABEL SKIP(0.2)  
    bfr{&tmptable}.BAmt[1] COLON 30 LABEL "Balance" bfr{&tmptable}.BAmt[2] NO-LABEL
    bfr{&tmptable}.BAmt[3] NO-LABEL bfr{&tmptable}.BAmt[4] NO-LABEL  SKIP(0.2)
    bfr{&tmptable}.Instal[1] COLON 30 LABEL "Instalment" 
    bfr{&tmptable}.Instal[2] NO-LABEL  bfr{&tmptable}.Instal[3] NO-LABEL 
    bfr{&tmptable}.Instal[4] NO-LABEL SKIP(0.2) 
    bfr{&tmptable}.NextP[1] COLON 30 LABEL "Next Billing Period" space(14)
    bfr{&tmptable}.NextP[2] NO-LABEL space(9) bfr{&tmptable}.NextP[3] NO-LABEL 
    space(9) bfr{&tmptable}.NextP[4] NO-LABEL SKIP(0.2)
    bfr{&tmptable}.comm COLON 30 VIEW-AS EDITOR SIZE 70 BY 2
    btn-ok AT ROW 24.7 COL 20 LABEL "WITH DRAW" SPACE(60)
    btn-close 
    rect-2 AT ROW 1.0 COL 5
    rect-1 AT ROW 24 COL 5
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D  
     /*BGCOLOR 8 FGCOLOR  1*/ SIZE 125 BY 26.8
    TITLE "LAND ALLOCATION WITH DRAWAL DATA CAPTURE" VIEW-AS DIALOG-BOX.
    

/* ***** Triggers  **** */
ON CHOOSE OF btnAcc IN FRAME frm-main
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
   DISPLAY dbcmf.dbAcc @ {&sKey} WITH FRAME frm-main.
   CLOSE QUERY qry-pickAcc.
  HIDE FRAME frm-pickAcc.
  APPLY 'tab' TO {&sKey} IN FRAME  frm-main.
  RETURN. 
END.

ON  'enter':u OF {&sKey} IN FRAME  frm-main
     OR 'tab':u OF {&sKey} IN FRAME  frm-main
DO:
   FIND FIRST dbcmf WHERE dbcmf.dbacc = DEC({&sKey}:SCREEN-VALUE) NO-LOCK NO-ERROR.
   IF NOT AVAILABLE dbcmf THEN DO:
      MESSAGE "Account does not exist, please create Debtors Account first" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   ELSE DO:
       CLEAR FRAME frm-main. 
       FIND FIRST dbsmf WHERE dbsmf.suburb = dbcmf.suburb NO-LOCK NO-ERROR.
       IF AVAILABLE dbsmf THEN
           wsdes[1] = dbsmf.Descrip.
       ELSE wsDes[1] = "Invalid Suburb".
       DISPLAY dbcmf.dbacc @ {&sKey} dbcmf.Name dbcmf.Add1 dbcmf.add2 dbcmf.Add3 wsDes[1]  WITH FRAME frm-main.
       OPEN QUERY qry-Acc FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.dbacc = dbcmf.dbacc NO-LOCK .
       /*GET CURRENT qry-acc NO-LOCK  NO-WAIT .*/ 
       IF AVAILABLE bfr{&tmptable} THEN
            DISPLAY {&UpdFields} WITH FRAME frm-main. 
   END.
   RETURN.    
END.

ON 'mouse-select-click' OF brw-acc
DO: 
  GET CURRENT qry-acc NO-LOCK NO-WAIT.
   IF AVAILABLE bfr{&tmptable} THEN
        DISPLAY {&UpdFields} WITH FRAME frm-main. 
  RETURN. 
END.

ON row-display OF brw-aCC DO:
    FIND FIRST LProj WHERE lProj.scheme = bfr{&tmptable}.scheme NO-LOCK NO-ERROR.
    wsDes[2] = LProj.Descrip.
END. 

ON CHOOSE OF btn-ok IN FRAME frm-Main 
DO: 
   GET CURRENT qry-Acc EXCLUSIVE-LOCK.
   IF bfr{&tmptable}.stat = 99 THEN DO:
       MESSAGE "This Landsale is already closed/withdrawn" VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
   END.
   ELSE DO:
       ASSIGN bfr{&tmptable}.stat = 99
              bfr{&tmptable}.comm = bfr{&tmptable}.comm + ". Account closed/Withdrawn  " + STRING(TODAY)
              + " by user# " + STRING(varUser).
       MESSAGE "This Landsale has been closed/withdrawn" VIEW-AS ALERT-BOX.
      CLEAR FRAME frm-Main ALL.
      APPLY 'entry' TO {&skey}.
   END.
   RETURN. 
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
VIEW FRAME frm-main.
ENABLE  {&skey} btn-close btn-ok btnAcc   WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
