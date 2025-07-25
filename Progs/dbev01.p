session:DATA-ENTRY-RETURN = TRUE.
/* Program.................dbev01.p
   Notes:...... ...........Accounts Receivable event capture
   Author:.................S. Mawire
*/
&SCOPED-DEFINE wsMsg          "Account already exist"
&SCOPED-DEFINE wsTitle        "Accounts Receivable Event Maintenance"
&SCOPED-DEFINE tmptable       dbTrack
&SCOPED-DEFINE skey           {&tmptable}.Acc
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.EDate ~
                              bfr{&tmptable}.Ref ~
                              bfr{&tmptable}.txtEvent ~
                              bfr{&tmptable}.Alert ~
                              bfr{&tmptable}.txtComm
                              
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.EDate ~
                              bfr{&tmptable}.Ref ~
                              bfr{&tmptable}.txtEvent ~
                              bfr{&tmptable}.txtComm ~
                              bfr{&tmptable}.Alert
                              
{varlibrary.i}
DEF VAR varUser    AS CHAR FORM "xxx" INITIAL "01".
DEF VAR wsdes LIKE dbsmf.Descrip.
DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 100 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 100 by 20.8.


DEFINE QUERY qry-track FOR bfr{&tmptable} scrolling.
DEF BROWSE brw-track QUERY qry-track
    DISPLAY bfr{&tmptable}.txtSource bfr{&tmptable}.EDate COLUMN-LABEL "Event Date" bfr{&tmptable}.Ref 
            bfr{&tmptable}.txtEvent WIDTH 40 bfr{&tmptable}.alert WITH 8 DOWN SEPARATORS.

DEFINE FRAME frm-main
    SKIP(0.2) "------------------------------------------------------------ACCOUNT DETAILS------------------------------------------------------------" 
    COLON 10 SKIP(0.5)
    btnAcc NO-TAB-STOP COLON 20 {&skey}     NO-LABEL 
    dbcmf.NAME COLON 60 LABEL "Accout Name" VIEW-AS TEXT SKIP(.2)
    dbcmf.Add1 COLON 60 LABEL "Postal Address" VIEW-AS TEXT
    dbcmf.add2 COLON 60 NO-LABEL VIEW-AS TEXT 
    dbcmf.Add3 COLON 60 NO-LABEL VIEW-AS text
    wsDes      COLON 60 NO-LABEL VIEW-AS TEXT SKIP(0.2)
     "-------------------------------------------------EVENT DETAILS----------------------
---------------------------------------------------------" COLON 10
    bfr{&tmptable}.EDate     COLON 30 LABEL "Event Date"
    bfr{&tmptable}.Ref       COLON 30 LABEL "Refence"
    bfr{&tmptable}.txtEvent COLON 30 LABEL "Event Description"
    bfr{&tmptable}.Alert     COLON 30 LABEL "Is it an Alert" 
    bfr{&tmptable}.txtcomm   COLON 30 VIEW-AS EDITOR SIZE 60 BY 2 SKIP(0.2)
    brw-Track COLON 20 
    btn-ok AT ROW 22.7 COL 20 SPACE(50)
    btn-close 
    rect-2 AT ROW 1.0 COL 5
    rect-1 AT ROW 22 COL 5
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D  
     /*BGCOLOR 8 FGCOLOR  1*/ SIZE 110 BY 24.8
    TITLE "ACCOUNTS RECEIVABLE EVENT MAINTENANCE" VIEW-AS DIALOG-BOX.
    
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
           wsdes = dbsmf.Descrip.
       ELSE wsDes = "Invalid Suburb".
       DISPLAY dbcmf.dbacc @ {&sKey} dbcmf.Name dbcmf.Add1 dbcmf.add2 dbcmf.Add3 wsDes  WITH FRAME frm-main.
       OPEN QUERY qry-Track FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.acc = dbcmf.dbacc NO-LOCK .
       /*IF AVAILABLE bfr{&tmptable} THEN
            DISPLAY {&UpdFields} WITH FRAME frm-main. */
       APPLY 'tab' TO brw-track IN FRAME frm-main.
       APPLY 'entry' TO bfr{&tmptable}.EDate IN FRAME frm-main.
   END.
   RETURN.    
END.

ON 'mouse-select-dblclick' OF brw-track
DO: 
  GET CURRENT qry-track NO-LOCK NO-WAIT.
  IF AVAILABLE dbTrack THEN
      MESSAGE bfr{&tmptable}.txtComm VIEW-AS ALERT-BOX.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-Main 
DO: 
   IF CAN-FIND (FIRST bfr{&tmptable} WHERE bfr{&tmptable}.eDate = DATE(bfr{&tmptable}.eDate:SCREEN-VALUE)
                               AND bfr{&tmptable}.Ref   = bfr{&tmptable}.Ref:SCREEN-VALUE
                               AND bfr{&tmptable}.acc   = dbcmf.dbacc) 
                               AND bfr{&tmptable}.txtSource = "DB" THEN DO:
        MESSAGE "A similar event exist for the Account" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
   END.
   ELSE DO:
       CREATE bfr{&tmptable}.
       ASSIGN bfr{&tmptable}.eDate    = DATE(bfr{&tmptable}.eDate:SCREEN-VALUE)
              bfr{&tmptable}.Ref      = bfr{&tmptable}.Ref:SCREEN-VALUE
              bfr{&tmptable}.acc      = dbcmf.dbacc 
              bfr{&tmptable}.txtEvent = bfr{&tmptable}.txtEvent:SCREEN-VALUE
              bfr{&tmptable}.Alert    = LOGICAL(bfr{&tmptable}.alert:SCREEN-VALUE) 
              bfr{&tmptable}.txtComm  = bfr{&tmptable}.txtComm:SCREEN-VALUE
              bfr{&tmptable}.UID      = varUser
              bfr{&tmptable}.txtSource = "DB".
       APPLY 'tab' to {&skey}.
      OPEN QUERY qry-Track FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.acc = dbcmf.dbacc NO-LOCK .
      APPLY 'entry' TO {&skey}.
   END.
   RETURN. 
END.
/********** MAIN LOGIC **********/
VIEW FRAME frm-main.
ENABLE  btnAcc {&skey}  {&UpdFields} brw-track btn-ok btn-close    WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
