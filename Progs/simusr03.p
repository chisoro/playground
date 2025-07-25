session:DATA-ENTRY-RETURN = TRUE.
/* Program.................simusr03.p
   Notes:...... Online usercode file maintenance
   Author:.................S. Chisoro
*/
&SCOPED-DEFINE wsMsg             "User already exist"
&SCOPED-DEFINE wsMsg1             "User not created in Simusr, create the user first"
&SCOPED-DEFINE wsTitle           "Online User Maintenance"

&SCOPED-DEFINE tmptable            dbRecopOn
&SCOPED-DEFINE skey                dbRecopon.ucode
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.txtName ~
                                        COLUMN-LABEL ' Online Name ':C
&SCOPED-DEFINE tmpFields      bfr{&tmptable}.ucode ~
                                        COLUMN-LABEL 'User Code ':C ~
                              bfr{&tmptable}.txtName ~
                                        COLUMN-LABEL ' Online Name ':C ~
                              bfr{&tmptable}.txtCur ~
                                        COLUMN-LABEL ' Currency ':C ~
                              bfr{&tmptable}.Paytype ~
                                        COLUMN-LABEL ' Payment Type ':C 
                              
DEF STREAM a.

{varlib.i}
    DEF VAR w-orientation AS CHAR      INITIAL "Portrait"    NO-UNDO.
   DEF VAR varMeth     LIKE dbPay.Paytype.
   
    DEF BUTTON   btnclose LABEL "CLOSE".
    DEF BUTTON btn-Cur LABEL "Currency".
    DEF BUTTON btn-Pay LABEL "Payment Type".
    DEF VAR wsDescrip LIKE dbPay.descrip.
    DEFINE VARIABLE rSalt AS RAW NO-UNDO.
    DEFINE VARIABLE cSalt AS CHARACTER NO-UNDO.
    DEF VAR bName LIKE bfr{&tmptable}.txtName.
    DEF VAR bCur LIKE bfr{&tmptable}.txtCur.
    DEF VAR bCode LIKE bfr{&tmptable}.ucode.
    DEF VAR bKey LIKE bfr{&tmptable}.txtKey.
    
    DEFINE RECTANGLE RECT-u
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 2.67.

DEFINE RECTANGLE RECT-l
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 8.67.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 2.67.

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 55 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 55 by 18.5.

DEFINE RECTANGLE rect-2-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 70 by 5.5.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 2.67.


FUNCTION GetSalt RETURNS RAW (INPUT saltLengthLimit AS INTEGER):
        DEFINE VARIABLE i AS INTEGER INITIAL 0 NO-UNDO.

        saltLengthLimit = saltLengthLimit / 8.

        DO WHILE i < saltLengthLimit:
            PUT-BYTES(rSalt, LENGTH(rSalt) + 1) = GENERATE-PBE-SALT.
            i = i + 1.
        END.

        RETURN (rSalt).
    END FUNCTION.


    FORM 
        HEADER {&wsTitle} AT 20
           skip(1) "             Online User Token" SKIP(1)
         "Bank Name: "      bName  SKIP (1)
        "Currency: " bCur SKIP(1)
        "Code: "  bCode SKIP(1)
        "Key: "   bKey
        "Page: " AT 90 PAGE-NUMBER(a) SKIP(2) 
        WITH NO-LABEL DOWN STREAM-IO FONT 8 WIDTH 132 CENTERED NO-BOX FRAME frm-rpt.

    define frame frm-input
    SKIP(2)
     {&skey}                AT ROW 2.62 COL 20 COLON-ALIGNED label "User Code" WIDGET-ID 2
       btn-Pay AT ROW 5.2 COL 9.5 COLON-ALIGNED bfr{&tmptable}.Paytype no-label 
     wsDescrip VIEW-AS TEXT NO-LABEL NO-TAB-STOP WIDGET-ID 30
      bfr{&tmptable}.txtCur   AT ROW 6.6 COL 20 COLON-ALIGNED VIEW-AS TEXT LABEL "Currency" 
     
     bfr{&tmptable}.txtNAME    AT ROW 8 COL 20 COLON-ALIGNED  label "Bank Name" 
     btn-Ok AT ROW 10.71 COL 10 WIDGET-ID 48
     btnReset AT ROW 10.71 COL 30 LABEL "Generate Token" WIDGET-ID 48
     btn-Close AT ROW 10.71 COL 60 WIDGET-ID 46
     "Authentication Details" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 1.48 COL 8 WIDGET-ID 40
          FONT 6
    "Online User Details" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 4.38 COL 9 WIDGET-ID 42
          FONT 6
     RECT-u AT ROW 1.81 COL 6 WIDGET-ID 34
     RECT-2-1 AT ROW 4.62 COL 6 WIDGET-ID 38
     Rect-4 AT ROW 10.0 COL 6
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED
         /*AT COL 2.2 ROW 1.19 */
         SIZE 80.8 BY 14.1
         BGCOLOR 11 FGCOLOR 1  WIDGET-ID 100
         TITLE "Online User Maintenance".

  DEF NEW SHARED FRAME frm-main
    brw-{&tmptable} AT ROW 1.8 COL 5
    btn-add AT ROW 20.7 COL 5
    Space(7) btn-edit
    space(7) btn-del
    space(7) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     /*BGCOLOR 8 FGCOLOR 1 */ SIZE 60 BY 24
    TITLE {&wsTitle} VIEW-AS DIALOG-BOX.

  DEF FRAME Search-opt
    wsSearch  COLON 20 LABEL "Search Value"
    btnSearch
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED.

DEF    QUERY qry-pickMet FOR dbPay SCROLLING.
DEF BROWSE brw-pickMet QUERY qry-pickMet
    DISPLAY dbPay.Paytype dbPay.descrip 
     WITH 20 DOWN SEPARATORS.
DEFINE FRAME frm-pickMet 
    brw-pickMet AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5
    btnclose colon 60
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Payment Method Selection".
/*triggers*/

ON CHOOSE OF btn-pay IN FRAME frm-Input
DO:
  VIEW FRAME frm-pickMet.
  OPEN QUERY qry-pickMet FOR EACH dbPay NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickMet.
  WAIT-FOR CHOOSE OF btnclose IN FRAME frm-pickMet 
          OR close of THIS-PROCEDURE IN FRAME frm-pickMet 
          OR CHOOSE OF btn-ok IN FRAME frm-pickMet 
          OR 'enter':u OF brw-pickMet
          OR 'mouse-select-dblclick' OF brw-pickMet.
  CLOSE QUERY qry-pickMet.
  HIDE FRAME frm-pickMet.
  APPLY 'tab' TO SELF.
  APPLY 'tab' TO bfr{&tmptable}.Paytype IN FRAME frm-input.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pickMet 
    OR 'enter':u OF brw-pickMet 
    OR 'mouse-select-dblclick' OF brw-pickMet 
DO: 
   GET CURRENT qry-pickMet NO-LOCK NO-WAIT.
    ASSIGN varMeth = dbPay.Paytype.
   DISPLAY  varMeth @ bfr{&tmptable}.Paytype  dbPay.descrip @ wsDescrip  WITH FRAME frm-Input.
   APPLY 'enter' TO bfr{&tmptable}.Paytype IN FRAME frm-input.
   RETURN.
END.

ON  'enter':u OF bfr{&tmptable}.Paytype  IN FRAME frm-input
    
DO: 
            wsid = {&skey}:SCREEN-VALUE IN FRAME frm-input.
            RUN validate-ip.
     
END.

ON  'enter':u OF bfr{&tmptable}.txtNAME  IN FRAME frm-input
    
DO: 
            IF  bfr{&tmptable}.txtNAME:SCREEN-VALUE IN FRAME frm-input = "" THEN DO:
                MESSAGE "Bank Name cannot be blank" VIEW-AS ALERT-BOX.
                DISABLE  btn-ok WITH FRAME frm-input.
                APPLY 'entry' TO  bfr{&tmptable}.txtNAME IN FRAME frm-input.
            END.
            ELSE DO:
                 ENABLE  btn-ok WITH FRAME frm-input.
                APPLY 'entry' TO btn-ok IN FRAME frm-input.
            END.
     
END.

ON 'enter':U OF  {&skey} IN FRAME frm-input
    OR 'tab':U OF {&skey} IN FRAME frm-input
DO:
    wsid = {&skey}:SCREEN-VALUE IN FRAME frm-input.
    IF (wsid <> "0")  THEN DO:
       FIND FIRST simusr WHERE  simusr.usercode  = wsid NO-ERROR. 
       IF NOT AVAILABLE simusr THEN DO:
          MESSAGE  {&wsMsg1} VIEW-AS ALERT-BOX.
          DISABLE bfr{&tmptable}.txtNAME bfr{&tmptable}.Paytype btn-Pay bfr{&tmptable}.txtCur WITH FRAME frm-input.
          APPLY 'entry' TO {&skey} IN FRAME frm-Input.
          RETURN NO-APPLY.
       END.
       ELSE IF AVAILABLE simusr THEN DO:
            ENABLE bfr{&tmptable}.txtNAME bfr{&tmptable}.Paytype btn-Pay bfr{&tmptable}.txtCur WITH FRAME frm-input.
            RETURN.
       END.
    END.
    ELSE DO:
        MESSAGE "Usercode cannot be 0." VIEW-AS ALERT-BOX.
        DISABLE bfr{&tmptable}.txtNAME bfr{&tmptable}.Paytype btn-Pay bfr{&tmptable}.txtCur WITH FRAME frm-input.
          APPLY 'entry' TO {&skey} IN FRAME frm-Input.
          RETURN NO-APPLY.
    END.
    
END.

ON 'start-search':u OF BROWSE brw-{&tmptable}
    run SEARCH.ip.

ON CHOOSE OF btn-Add IN FRAME frm-main DO:
     btn-ok:LABEL IN  FRAME frm-input = "Save".
    RUN proc-input.
    RETURN.
END.

ON CHOOSE OF btn-edit
    OR 'enter':u OF  brw-{&tmptable}
    OR 'mouse-select-dblclick' OF  brw-{&tmptable}
DO:
   GET CURRENT qry-{&tmptable}  EXCLUSIVE-LOCK NO-WAIT.
   run proc-edit.
    RETURN.
END.




ON CHOOSE OF btn-Del DO:
    GET CURRENT  qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.ucode
        varMeth   =  bfr{&tmptable}.Paytype.
    FIND FIRST dbRec WHERE dbRec.Paytype = varMeth AND dbRec.OpCode = wsid NO-ERROR.
    IF AVAILABLE dbRec THEN DO:
        MESSAGE 'Operator has transactions. Cannot be deleted' VIEW-AS ALERT-BOX.
    END.
    ELSE IF NOT AVAILABLE dbRec THEN DO:
          FIND FIRST dbRecH WHERE dbRecH.Paytype = varMeth AND dbRecH.OpCode = wsid NO-ERROR.
          IF AVAILABLE dbRecH THEN DO:
                MESSAGE 'Operator has transactions. Cannot be deleted' VIEW-AS ALERT-BOX.
         END.
         ELSE IF NOT AVAILABLE dbRecH THEN DO:
                FOR EACH bfr{&tmptable}  WHERE  bfr{&tmptable}.ucode = wsid AND bfr{&tmptable}.Paytype = varMeth:
                    DELETE bfr{&tmptable}.
                END.
               FOR EACH bfr{&tmptable}  WHERE  bfr{&tmptable}.ucode = wsid AND bfr{&tmptable}.Paytype = varMeth:
                    DELETE bfr{&tmptable}. 
               END.
               FOR EACH dbRecop WHERE  dbRecop.usercode = wsid:
                   DELETE dbRecop.
               END.
                Method-Status = brw-{&tmptable}:DELETE-SELECTED-ROWS().
         END.
         
    END.
   
    APPLY 'entry' TO btn-exit.
END.

ON CHOOSE OF btnReset DO:

    MESSAGE "Are you sure you want to Generate New Token?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
             TITLE "Token Generation?" UPDATE w_response AS LOGICAL.
    IF w_response THEN DO:
       wsBtn = "EDIT".
         cSalt = STRING(HEX-ENCODE(GetSalt(16))).
         FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.ucode = {&skey}:SCREEN-VALUE IN FRAME frm-input  AND bfr{&tmptable}.Paytype = int(bfr{&tmptable}.Paytype:SCREEN-VALUE IN FRAME frm-input) NO-ERROR.
          IF AVAILABLE  bfr{&tmptable} THEN DO:
             bfr{&tmptable}.txtKey = cSalt.
          END.
             
         ASSIGN  bCur = bfr{&tmptable}.txtCur:SCREEN-VALUE IN FRAME frm-input 
                    bKey = cSalt
                   bNAME = bfr{&tmptable}.txtName:SCREEN-VALUE IN FRAME frm-input 
                   bCode = {&skey}:SCREEN-VALUE IN FRAME frm-input.
               {PrintOpt.i &stream-name="stream a"
                    &print-prog="Report.ip"
                    &paged} .
          APPLY "CLOSE":U TO THIS-PROCEDURE.
    END.   
    ELSE
      RETURN NO-APPLY.
END. 

ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
   
   
   IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN DO:
           cSalt = STRING(HEX-ENCODE(GetSalt(16))).
           FIND FIRST dbRecop WHERE dbRecop.usercode = wsid NO-ERROR.
           IF NOT AVAILABLE dbRecop THEN DO:
                CREATE dbRecop.
                wsBtn = "ADD".
                ASSIGN
                        dbRecop.usercode  = wsid
                        dbRecop.RecNo = 0
                        dbRecop.EOD = NO
                        dbRecop.DeviceName = lc-host
                        dbRecop.UID = varUser
                        dbRecop.creDate =NOW.
           END.

           CREATE bfr{&tmptable}.
           ASSIGN 
               bfr{&tmptable}.Paytype = int(bfr{&tmptable}.Paytype:SCREEN-VALUE  IN FRAME frm-input)
               bfr{&tmptable}.txtCur = bfr{&tmptable}.txtCur:SCREEN-VALUE  IN FRAME frm-input
               bfr{&tmptable}.txtKey = cSalt
               bfr{&tmptable}.txtName = bfr{&tmptable}.txtNAME:SCREEN-VALUE IN FRAME frm-input
               bfr{&tmptable}.ucode = wsid
               bfr{&tmptable}.DeviceName = lc-host
               bfr{&tmptable}.UID = varUser
               bfr{&tmptable}.creDate =NOW.
            /*create a file to share with bank*/
               ASSIGN  bCur = bfr{&tmptable}.txtCur  
                    bKey = bfr{&tmptable}.txtKey
                   bNAME = bfr{&tmptable}.txtName 
                   bCode = bfr{&tmptable}.ucode.
               {PrintOpt.i &stream-name="stream a"
                    &print-prog="Report.ip"
                    &paged} 
          CLEAR FRAME frm-input ALL.
          APPLY 'entry' TO {&skey} IN FRAME frm-Input.
    END.
 
END.

{audit.i}

/********** MAIN LOGIC **********/
OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK .
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
CLOSE QUERY qry-{&tmptable}.
HIDE FRAME frm-main.

PROCEDURE proc-input:
   OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   CLEAR FRAME frm-input ALL.
   ENABLE ALL EXCEPT btnReset btn-ok  bfr{&tmptable}.txtNAME WITH FRAME frm-input.
   ASSIGN {&skey}:SCREEN-VALUE = "0".
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
         CLOSE QUERY  qry-{&tmptable}.
   OPEN QUERY  qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK.
   HIDE FRAME frm-input.
END.


PROCEDURE Validate-ip:
 
    IF btn-ok:LABEL  IN FRAME frm-input = "Save" THEN DO:
        IF (wsid <> "0")  THEN DO:
           FIND FIRST  bfr{&tmptable} WHERE  bfr{&tmptable}.ucode  = wsid  AND  bfr{&tmptable}.Paytype =  int( bfr{&tmptable}.PayType:SCREEN-VALUE IN FRAME frm-input) NO-ERROR. 
           IF AVAILABLE bfr{&tmptable} THEN DO:
              MESSAGE  {&wsMsg} VIEW-AS ALERT-BOX.
               DISABLE   bfr{&tmptable}.txtNAME WITH FRAME frm-input.
              APPLY 'entry' TO {&skey} IN FRAME frm-Input.
           END.
           ELSE IF NOT AVAILABLE  bfr{&tmptable} THEN DO:
               FIND FIRST dbPay WHERE dbPay.paytype = int(bfr{&tmptable}.Paytype:SCREEN-VALUE) NO-LOCK NO-ERROR.
               IF AVAILABLE dbPay THEN DO:
                             wsDescrip:SCREEN-VALUE = dbPay.descrip.
                             FIND FIRST cbkmf WHERE cbkmf.bank = dbPay.bank NO-LOCK NO-ERROR.
                              bfr{&tmptable}.txtCur:SCREEN-VALUE = cbkmf.txtCur.
                            ENABLE  bfr{&tmptable}.txtNAME WITH FRAME frm-input.
                            APPLY 'entry' TO  bfr{&tmptable}.txtNAME IN FRAME frm-input.
                END.
                ELSE IF NOT AVAILABLE dbPay THEN DO:
                        MESSAGE "Payment Type Does not Exist" VIEW-AS ALERT-BOX.
                        APPLY 'entry' TO  bfr{&tmptable}.paytype IN FRAME frm-input.
                 END.
        
            END.
        END.
        ELSE DO:
            MESSAGE "No Usercode Entered" VIEW-AS ALERT-BOX.
           APPLY 'close' TO THIS-PROCEDURE.
        END.

    END.
       
END.

PROCEDURE Report.ip.
   DISPLAY  STREAM a  bCur bKey  bNAME  bCode WITH FRAME frm-rpt.
 END.

 PROCEDURE proc-Edit:
   ASSIGN wsid =  bfr{&tmptable}.ucode
            varMeth =  bfr{&tmptable}.paytype.
   
   VIEW FRAME frm-input.
   ENABLE ALL EXCEPT   {&skey}   bfr{&tmptable}.Paytype  wsDescrip   bfr{&tmptable}.txtCur  bfr{&tmptable}.txtNAME btn-ok btn-Pay WITH FRAME frm-input.
   btn-ok:LABEL = "Update".
   FIND FIRST dbPay WHERE dbPay.paytype = bfr{&tmptable}.Paytype NO-LOCK.
   IF AVAILABLE dbPay THEN DO:
       wsDescrip = dbPay.Descrip.
   END.
   DISPLAY wsid @  {&skey} 
       varMeth @  bfr{&tmptable}.Paytype  
        wsDescrip 
        bfr{&tmptable}.txtCur @  bfr{&tmptable}.txtCur  
        bfr{&tmptable}.txtName @  bfr{&tmptable}.txtNAME 
       WITH FRAME frm-input.
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME  frm-input.
   HIDE FRAME frm-input.
END.

procedure Search.ip.
    hCol = browse brw-{&tmptable}:current-column.
       assign  frame Search-opt:title = hCol:label + " Column".
       IF trim(hCol:label)<> "Bank Name" THEN
           RETURN NO-APPLY.
        view frame Search-opt.
        enable all with frame Search-opt.
        WAIT-FOR 'choose' of btnSearch OR 'tab' of btnSearch OR 'enter' of btnSearch 
            OR 'window-close' of frame Search-opt or close of this-procedure or
                'esc','f4' of frame Search-opt.
        hide frame Search-opt.
        /*if ll-sort = ? then return.*/
    case trim(hCol:label):
      when "Name" then
         do:
          OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} NO-LOCK
         
                            where bfr{&tmptable}.txtName >= wsSearch:SCREEN-VALUE
                               BY bfr{&tmptable}.txtNAME.

            END.
        OTHERWISE DO:
           RETURN NO-APPLY.
        END.     
     END.
    RETURN.
END.
