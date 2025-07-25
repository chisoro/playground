session:DATA-ENTRY-RETURN = TRUE.
/* Program.................simusr03.p
   Notes:...... Online usercode file maintenance
   Author:.................S. Chisoro
*/
&SCOPED-DEFINE wsMsg             "User already exist"
&SCOPED-DEFINE wsTitle           "Online User Maintenance"
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "Portrait"    NO-UNDO.
DEF VAR wsid LIKE dbRecopOn.ucode.
DEF VAR varMeth     LIKE dbPay.Paytype.
DEF BUTTON btn-Add    LABEL "ADD".
DEF BUTTON btn-del    LABEL "DELETE".
DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-Edit   LABEL "EDIT".
DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON   btnclose LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "OK".
DEF BUTTON btn-upd    LABEL "UPDATE".
DEF BUTTON btn-cancel LABEL "CANCEL".
DEF BUTTON btn-Cur LABEL "Currency".
DEF BUTTON btn-Pay LABEL "Payment Type".
DEF BUTTON btnReset  LABEL "Generate Token".
DEF VAR wsSearch   LIKE dbRecopOn.txtName.
DEF VAR hCol        AS WIDGET-HANDLE.
DEF BUTTON  btnSearch LABEL "Search".
DEF VAR wsDescrip LIKE dbPay.descrip.
DEFINE VARIABLE rSalt AS RAW NO-UNDO.
DEFINE VARIABLE cSalt AS CHARACTER NO-UNDO.
DEF VAR bName LIKE dbRecopOn.txtName.
DEF VAR bCur LIKE dbRecopOn.txtCur.
DEF VAR bCode LIKE dbRecopOn.ucode.
DEF VAR bKey LIKE dbRecopOn.txtKey.
DEF VAR method-status AS LOGICAL.


FUNCTION GetSalt RETURNS RAW (INPUT saltLengthLimit AS INTEGER):
    DEFINE VARIABLE i AS INTEGER INITIAL 0 NO-UNDO.
    
    saltLengthLimit = saltLengthLimit / 8.
    
    DO WHILE i < saltLengthLimit:
        PUT-BYTES(rSalt, LENGTH(rSalt) + 1) = GENERATE-PBE-SALT.
        i = i + 1.
    END.
    
    RETURN (rSalt).
END FUNCTION.

DEF TEMP-TABLE tempRecop
    FIELD txtName LIKE dbRecopOn.txtName
    FIELD ucode LIKE  dbRecopOn.ucode
    FIELD txtCur LIKE  dbRecopOn.txtCur
    FIELD paytype LIKE  dbRecopOn.paytype
    FIELD txtDescrip LIKE dbPay.descrip.

FORM 
    HEADER {&wsTitle} AT 20
       skip(1) "             Online User Token" SKIP(1)
     "Bank Name: "      bName  SKIP (1)
    "Currency: " bCur SKIP(1)
    "Code: "  bCode SKIP(1)
    "Key: "   bKey
    "Page: " AT 90 PAGE-NUMBER(a) SKIP(2) 
    WITH NO-LABEL DOWN STREAM-IO FONT 8 WIDTH 132 CENTERED NO-BOX FRAME frm-rpt.

DEFINE RECTANGLE RECT-u
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 2.67.

DEFINE RECTANGLE RECT-l
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 100 BY 24.9.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 100 BY 2.67.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 4.9.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 2.67.

DEF  QUERY qry-Recopon FOR tempRecop SCROLLING.
DEF BROWSE brw-tempRecOp QUERY qry-Recopon
    DISPLAY tempRecop.ucode  LABEL "User Code" FORM "X(3)" 
            tempRecop.txtName  COLUMN-LABEL "Bank Name" FORM "X(40)" 
            tempRecop.txtCur  LABEL "Currency" FORM "X(4)" 
            tempRecop.paytype  COLUMN-LABEL "Payment Type"  
            tempRecop.txtDescrip  COLUMN-LABEL "Payment Description" 
    WITH 30 DOWN SEPARATORS.

DEFINE FRAME frm-Main 
    brw-tempRecOp AT ROW 1.8 COL 5
    btn-add AT ROW 26.9 COL 5
    Space(20) btn-edit
    space(20) btn-del
    space(20) btn-exit SKIP(1)
    RECT-l AT ROW 1.4 COL 3
    rect-3 AT ROW 26.5 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     /*BGCOLOR 8 FGCOLOR 1 */ SIZE 105 BY 30
    TITLE "Online User Maintenance" VIEW-AS DIALOG-BOX.

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

define frame frm-input
    SKIP(2)
     dbRecopOn.ucode               AT ROW 2.62 COL 20 COLON-ALIGNED label "User Code" WIDGET-ID 2
       btn-Pay AT ROW 5.2 COL 9.5 COLON-ALIGNED dbRecopOn.Paytype no-label 
     wsDescrip VIEW-AS TEXT NO-LABEL NO-TAB-STOP WIDGET-ID 30
      dbRecopOn.txtCur   AT ROW 6.6 COL 20 COLON-ALIGNED VIEW-AS TEXT LABEL "Currency" 
     
     dbRecopOn.txtNAME    AT ROW 8 COL 20 COLON-ALIGNED  label "Bank Name" 
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
     RECT-2 AT ROW 4.62 COL 6 WIDGET-ID 38
     Rect-4 AT ROW 10.0 COL 6
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED
         /*AT COL 2.2 ROW 1.19 */
         SIZE 80.8 BY 14.1
         BGCOLOR 11 FGCOLOR 1  WIDGET-ID 100
         TITLE "Online User Maintenance".
    
/* ***** Triggers for the main frame **** 
ON LEAVE OF dbRecopOn.ucode  IN FRAME frm-input
    OR 'enter':u OF dbRecopOn.ucode  IN FRAME frm-input
DO: 
     wsid = dbRecopOn.ucode :SCREEN-VALUE IN FRAME frm-input.
     RUN validate-ip.
END.*/
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
  APPLY 'tab' TO dbRecopOn.Paytype IN FRAME frm-input.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pickMet 
    OR 'enter':u OF brw-pickMet 
    OR 'mouse-select-dblclick' OF brw-pickMet 
DO: 
   GET CURRENT qry-pickMet NO-LOCK NO-WAIT.
    ASSIGN varMeth = dbPay.Paytype.
   DISPLAY  varMeth @ dbRecopOn.Paytype  dbPay.descrip @ wsDescrip  WITH FRAME frm-Input.
   APPLY 'enter' TO dbRecopOn.Paytype IN FRAME frm-input.
   RETURN.
END.

ON  'enter':u OF dbRecopOn.Paytype  IN FRAME frm-input
    
DO: 
            wsid = dbRecopOn.ucode:SCREEN-VALUE IN FRAME frm-input.
            RUN validate-ip.
     
END.

ON  'enter':u OF dbRecopOn.txtNAME  IN FRAME frm-input
    
DO: 
            IF  dbRecopOn.txtNAME:SCREEN-VALUE IN FRAME frm-input = "" THEN DO:
                MESSAGE "Bank Name cannot be blank" VIEW-AS ALERT-BOX.
                DISABLE  btn-ok WITH FRAME frm-input.
                APPLY 'entry' TO  dbRecopOn.txtNAME IN FRAME frm-input.
            END.
            ELSE DO:
                 ENABLE  btn-ok WITH FRAME frm-input.
                APPLY 'entry' TO btn-ok IN FRAME frm-input.
            END.
     
END.

ON 'start-search':u OF BROWSE brw-tempRecOp
    run SEARCH.ip.

ON CHOOSE OF btn-Add IN FRAME frm-main DO:
     btn-ok:LABEL IN  FRAME frm-input = "Save".
    RUN proc-input.
    RETURN.
END.

ON CHOOSE OF btn-edit
    OR 'enter':u OF brw-tempRecOp
    OR 'mouse-select-dblclick' OF brw-tempRecOp
DO:
   GET CURRENT qry-Recopon EXCLUSIVE-LOCK NO-WAIT.
    /*DISABLE ALL EXCEPT btnReset btn-Close  WITH FRAME frm-input.*/
    run proc-edit.
    RETURN.
END.


ON CHOOSE OF btn-Del DO:
    GET CURRENT  qry-Recopon EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = tempRecop.ucode
        varMeth   =  tempRecop.Paytype.
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
                FOR EACH tempRecop  WHERE  tempRecop.ucode = wsid AND tempRecop.Paytype = varMeth:
                    DELETE tempRecop.
                END.
               FOR EACH dbRecopon  WHERE  dbRecopon.ucode = wsid AND dbRecopon.Paytype = varMeth:
                    DELETE dbRecopon. 
               END.
               FOR EACH dbRecop WHERE  dbRecop.usercode = wsid:
                   DELETE dbRecop.
               END.
                Method-Status = brw-tempRecOp:DELETE-SELECTED-ROWS().
         END.
         
    END.
   
    APPLY 'entry' TO btn-exit.
END.

ON CHOOSE OF btnReset DO:
    MESSAGE "Are you sure you want to Generate New Token?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
             TITLE "Token Generation?" UPDATE w_response AS LOGICAL.
    IF w_response THEN DO:
         cSalt = STRING(HEX-ENCODE(GetSalt(16))).
         FOR EACH dbRecopon WHERE dbRecopon.ucode = dbRecopon.ucode:SCREEN-VALUE IN FRAME frm-input  AND dbRecopon.Paytype = int(dbRecopon.Paytype:SCREEN-VALUE IN FRAME frm-input):
             ASSIGN 
                 dbRecopon.txtKey = cSalt.
         END.
         ASSIGN  bCur = dbRecopon.txtCur:SCREEN-VALUE IN FRAME frm-input 
                    bKey = cSalt
                   bNAME = dbRecopon.txtName:SCREEN-VALUE IN FRAME frm-input 
                   bCode = dbRecopon.ucode:SCREEN-VALUE IN FRAME frm-input.
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
   
    /*MESSAGE "Bytes: " LENGTH(cSalt)  SKIP(1)
    "Salt: " cSalt
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
   IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN DO:
           cSalt = STRING(HEX-ENCODE(GetSalt(16))).
           FIND FIRST simusr WHERE simusr.usercode = wsid NO-ERROR.
           IF AVAILABLE simusr THEN DO:
               ASSIGN 
                   simusr.NAME = dbRecopOn.txtNAME:SCREEN-VALUE IN FRAME frm-input.
                   FOR EACH dbRecopon WHERE dbRecopon.ucode = wsid:
                       ASSIGN
                                dbRecopon.txtName = dbRecopOn.txtNAME:SCREEN-VALUE IN FRAME frm-input.
                   END.
           END.
           ELSE IF NOT AVAILABLE simusr THEN DO:
               CREATE simusr.
               ASSIGN
                   simusr.NAME = dbRecopOn.txtNAME:SCREEN-VALUE IN FRAME frm-input
                   simusr.usercode = wsid
                   simusr.txtStatus = "A".
           END.
           FIND FIRST dbRecop WHERE dbRecop.usercode = wsid NO-ERROR.
           IF NOT AVAILABLE dbRecop THEN DO:
                CREATE dbRecop.
                ASSIGN
                        dbRecop.usercode  = wsid
                        dbRecop.RecNo = 0
                        dbRecop.EOD = NO.
           END.

           CREATE dbRecopon.
           ASSIGN 
               dbRecopon.Paytype = int(dbRecopOn.Paytype:SCREEN-VALUE  IN FRAME frm-input)
               dbRecopon.txtCur = dbRecopOn.txtCur:SCREEN-VALUE  IN FRAME frm-input
               dbRecopon.txtKey = cSalt
               dbRecopon.txtName = dbRecopOn.txtNAME:SCREEN-VALUE IN FRAME frm-input
               dbRecopon.ucode = wsid.
            /*create a file to share with bank*/
               ASSIGN  bCur = dbRecopon.txtCur  
                    bKey = dbRecopon.txtKey
                   bNAME = dbRecopon.txtName 
                   bCode = dbRecopon.ucode.
               {PrintOpt.i &stream-name="stream a"
                    &print-prog="Report.ip"
                    &paged} 
          CLEAR FRAME frm-input ALL.
          APPLY 'entry' TO dbRecopon.ucode IN FRAME frm-Input.
    END.
   
    FOR EACH tempRecop:
        DELETE tempRecop.
    END.
    FOR EACH dbRecopon:
        FIND FIRST dbPay WHERE dbpay.paytype = dbRecopon.Paytype NO-LOCK NO-ERROR .
        CREATE tempRecop.
        ASSIGN 
            tempRecop.txtName = dbRecopOn.txtName
            tempRecop.ucode =  dbRecopOn.ucode
            tempRecop.txtCur = dbRecopOn.txtCur
            tempRecop.paytype =  dbRecopOn.paytype
            tempRecop.txtDescrip = dbPay.descrip.
        END.
    OPEN QUERY qry-Recopon FOR EACH tempRecop NO-LOCK .

END.

/********** MAIN LOGIC **********/
FOR EACH dbRecopon:
    FIND FIRST dbPay WHERE dbpay.paytype = dbRecopon.Paytype NO-LOCK NO-ERROR .
    CREATE tempRecop.
    ASSIGN 
        tempRecop.txtName = dbRecopOn.txtName
        tempRecop.ucode =  dbRecopOn.ucode
        tempRecop.txtCur = dbRecopOn.txtCur
        tempRecop.paytype =  dbRecopOn.paytype
        tempRecop.txtDescrip = dbPay.descrip.
END.
OPEN QUERY qry-Recopon FOR EACH tempRecop NO-LOCK .
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
CLOSE QUERY qry-Recopon.
HIDE FRAME frm-main.

PROCEDURE proc-input:
   OPEN QUERY qry-Recopon FOR EACH tempRecop NO-LOCK.
   CLEAR FRAME frm-input ALL.
   ENABLE ALL EXCEPT btnReset btn-ok dbRecopOn.txtNAME WITH FRAME frm-input.
   ASSIGN dbRecopOn.ucode:SCREEN-VALUE = "0".
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input.
         CLOSE QUERY  qry-Recopon.
   OPEN QUERY qry-Recopon FOR EACH tempRecop NO-LOCK.
   HIDE FRAME frm-input.
END.

PROCEDURE Validate-ip:
    
    IF (wsid <> "0")  THEN DO:
       FIND FIRST tempRecop WHERE  tempRecop.ucode  = wsid  AND  tempRecop.Paytype =  int(dbRecopOn.Paytype:SCREEN-VALUE IN FRAME frm-input) NO-ERROR. 
       IF AVAILABLE tempRecop THEN DO:
          MESSAGE  {&wsMsg} VIEW-AS ALERT-BOX.
           /* CLEAR FRAME frm-input ALL. 
            dbRecopOn.Paytype:SCREEN-VALUE = ""*/
          DISABLE  dbRecopOn.txtNAME WITH FRAME frm-input.
          APPLY 'entry' TO dbRecopOn.ucode IN FRAME frm-Input.
       END.
       ELSE IF NOT AVAILABLE tempRecop THEN DO:
           FIND FIRST dbPay WHERE dbPay.paytype = int(dbRecopOn.Paytype:SCREEN-VALUE) NO-LOCK NO-ERROR.
           IF AVAILABLE dbPay THEN DO:
                         wsDescrip:SCREEN-VALUE = dbPay.descrip.
                         FIND FIRST cbkmf WHERE cbkmf.bank = dbPay.bank NO-LOCK NO-ERROR.
                         dbRecopOn.txtCur:SCREEN-VALUE = cbkmf.txtCur.
                        ENABLE dbRecopOn.txtNAME WITH FRAME frm-input.
                        APPLY 'entry' TO dbRecopOn.txtNAME IN FRAME frm-input.
            END.
            ELSE IF NOT AVAILABLE dbPay THEN DO:
                    MESSAGE "Payment Type Does not Exist" VIEW-AS ALERT-BOX.
                    APPLY 'entry' TO dbRecopOn.paytype IN FRAME frm-input.
             END.
             
        END.
    END.
    ELSE DO:
        MESSAGE "No Usercode Entered" VIEW-AS ALERT-BOX.
        /*CLEAR FRAME frm-input ALL.*/
        APPLY 'close' TO THIS-PROCEDURE.
    END.
END.

PROCEDURE proc-Edit:
   ASSIGN wsid = tempRecop.ucode
            varMeth = tempRecop.paytype.
   
   /*VIEW FRAME frm-input*/.
   ENABLE ALL EXCEPT   dbRecopOn.ucode   dbRecopOn.Paytype  wsDescrip  dbRecopOn.txtCur dbRecopOn.txtNAME btn-ok WITH FRAME frm-input.
   DISPLAY  wsid @ dbRecopOn.ucode  
       varMeth @ dbRecopOn.Paytype  
       tempRecop.txtDescrip  @ wsDescrip 
       tempRecop.txtCur @ dbRecopOn.txtCur  
       tempRecop.txtName @ dbRecopOn.txtNAME 
       WITH FRAME frm-input.
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME  frm-input.
   HIDE FRAME frm-input.
END.

procedure Search.ip.
    hCol = browse brw-tempRecOp:current-column.
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
          OPEN query qry-Recopon FOR EACH tempRecop no-lock 
                            where tempRecop.txtName >= wsSearch:SCREEN-VALUE
                               BY tempRecop.txtNAME.

            END.
        OTHERWISE DO:
           RETURN NO-APPLY.
        END.     
     END.
    RETURN.
END.


PROCEDURE Report.ip.
   DISPLAY  STREAM a  bCur bKey  bNAME  bCode WITH FRAME frm-rpt.
 END.


