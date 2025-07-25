USING System.Net.Http.*. 
USING System.Environment.

/*Program:...........eftTerm.p
   Notes:...............Create eft terminals
   Programmer:................S. Chisoro */
 session:DATA-ENTRY-RETURN = TRUE.

 &SCOPED-DEFINE wsTitle           "EFT Terminal File Maintenance"
 &SCOPED-DEFINE tmptable            eftTerminal
&SCOPED-DEFINE skey               eftTerminal.term_id
&SCOPED-DEFINE UpdFields      bfr{&tmptable}.usr ~
                                        COLUMN-LABEL ' USER CODE ':C
&SCOPED-DEFINE tmpFields   bfr{&tmptable}.term_id ~
                                        COLUMN-LABEL 'TERMINAL ' FORMAT 'X(15)':C~
                                         bfr{&tmptable}.usr ~
                                        COLUMN-LABEL ' USER CODE ' FORMAT 'X(15)':C~
                                         bfr{&tmptable}.Name~
                                         COLUMN-LABEL 'USER NAME ' FORMAT 'X(40)':C

DEF VAR i AS INTEGER.
 


define rectangle rect-1
     edge-pixels 2 graphic-edge  no-fill
     size 80 by 2.3.

define rectangle rect-11
     edge-pixels 2 graphic-edge  no-fill
     size 80 by 2.3.

define rectangle rect-12
     edge-pixels 2 graphic-edge  no-fill
     size 80 by 5.8.

define rectangle rect-2
     edge-pixels 2 graphic-edge  NO-FILL
     size 80 by 18.4.

define rectangle rect-3
     edge-pixels 2 graphic-edge  NO-FILL
     size 55 by 9.5.
define rectangle rect-4
     edge-pixels 2 graphic-edge  NO-FILL
     size 84 by 9.5.
DEF BUTTON btn-devAdd    LABEL 'ADD'.
DEF BUTTON btn-devALLOC LABEL 'ALLOCATE'.
DEF BUTTON btn-devINIT LABEL 'INITIALISE'.
DEF BUTTON btn-devCLOSE LABEL 'CLOSE '.
DEF BUTTON btn-user.


{varlib.i}

define frame frm-input
    SKIP(0.5)
    
    
    rect-11 AT ROW  1 COL 2
    {&skey}   AT ROW 1.8 COL 5 LABEL "TERMINAL ID"  SKIP(0.2)
    
    rect-1 AT ROW 3.5 COL 2
    btn-ok AT ROW 4.3 COL  20
    btn-close colon 50 SKIP(0.5)
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d scrollable
         TITLE "EFT TERMINAL MASTER CONTROL FILE".

DEFINE QUERY qry-user FOR simusr  scrolling.
DEF BROWSE brw-user QUERY qry-user
    DISPLAY simusr.usercode simusr.NAME 
    WITH 8 DOWN SEPARATORS.

define frame frm-input1
    SKIP(0.5)
    rect-12 AT ROW  1 COL 2
    {&skey}   AT ROW 1.8 COL 5 LABEL "TERMINAL ID"  SKIP(2)
     btn-user  COLON 5  LABEL "USER"  bfr{&tmptable}.usr   NO-LABEL SPACE (5)
      bfr{&tmptable}.NAME NO-LABEL VIEW-AS TEXT SKIP(0.2) 
    rect-1 AT ROW 7.0 COL 2
    btn-ok AT ROW 7.8 COL  20
    btn-close colon 50 SKIP(0.5)
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d scrollable
         TITLE "EFT TERMINAL MASTER CONTROL FILE".

DEFINE FRAME frm-user 
    brw-user AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 10 LABEL "OK" SPACE(30)  btn-close
   with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Available Operators".
    
DEF NEW SHARED FRAME frm-main
     SKIP(0.5)
    brw-{&tmptable}  COLON 5
    btn-devAdd AT ROW 20.7 COL 5
    Space(4) btn-devAlloc
    space(4) btn-del
    SPACE(4) btn-devInit
    SPACE(4) btn-devClose
    space(4) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     /*BGCOLOR 8 FGCOLOR 1 */ SIZE 85 BY 24
    TITLE {&wsTitle} VIEW-AS DIALOG-BOX.

/*API intergration*/
DEFINE VARIABLE HttpClient AS CLASS System.Net.WebClient. 
DEFINE VARIABLE webResponse AS LONGCHAR NO-UNDO. 
DEF VAR txtWebResponse AS CHAR.
DEF VAR vTemp AS CHAR.
DEF VAR txtUnWantedR AS CHAR EXTENT 6.
DEF VAR rspCode AS CHAR.
DEF VAR rspMessage AS CHAR.

/* *** Triggers to input frame frm-input*** */
ON CHOOSE OF btn-user IN FRAME frm-input1
DO:
        VIEW FRAME frm-user.
          OPEN QUERY qry-user FOR EACH simusr NO-LOCK.
          ENABLE ALL WITH FRAME frm-user.
          WAIT-FOR CHOOSE OF btn-close IN FRAME frm-user 
                  OR close of THIS-PROCEDURE IN FRAME frm-user
                 OR CHOOSE OF btn-ok IN FRAME frm-user
                OR 'enter':u OF brw-user
                OR 'mouse-select-dblclick' OF brw-user.
          CLOSE QUERY qry-user.
          HIDE FRAME frm-user.
          APPLY 'tab' TO SELF.
          RETURN.
END.

ON 'enter':U OF bfr{&tmptable}.usr  IN FRAME frm-input1 
DO:
    FIND FIRST simusr WHERE simusr.usercode = bfr{&tmptable}.usr:SCREEN-VALUE IN FRAME frm-input1 NO-LOCK NO-ERROR.
    IF AVAILABLE simusr THEN DO:
        FIND FIRST dbRecop WHERE dbRecop.usercode = bfr{&tmptable}.usr:SCREEN-VALUE IN FRAME frm-input1 NO-LOCK NO-ERROR.
        IF AVAILABLE dbRecop THEN DO:
             DISPLAY simusr.usercode @ bfr{&tmpTable}.usr simusr.NAME @ bfr{&tmpTable}.NAME WITH FRAME frm-input1.
              RETURN.
        END.
        ELSE IF NOT AVAILABLE dbRecop THEN DO:
             MESSAGE simusr.NAME + " is not a receipt operator" VIEW-AS ALERT-BOX.
             RETURN NO-APPLY.
        END.
    END.
    ELSE IF NOT AVAILABLE simusr THEN DO:
        MESSAGE "User does not exist" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
   
END.

ON 'choose':U OF btn-ok IN FRAME frm-input1 
DO:
   
    wsid = {&skey}:SCREEN-VALUE IN FRAME frm-input1.
     FIND FIRST simusr WHERE simusr.usercode = bfr{&tmptable}.usr:SCREEN-VALUE IN FRAME frm-input1  NO-LOCK NO-ERROR.
    IF AVAILABLE simusr THEN DO:
        FIND FIRST dbRecop WHERE dbRecop.usercode = bfr{&tmptable}.usr:SCREEN-VALUE IN FRAME frm-input1 NO-LOCK NO-ERROR.
        IF AVAILABLE dbRecop THEN DO:
           ASSIGN bfr{&tmptable}.term_id = wsid
                bfr{&tmptable}.usr = bfr{&tmptable}.usr:SCREEN-VALUE IN FRAME frm-input1
                bfr{&tmptable}.NAME = bfr{&tmptable}.NAME:SCREEN-VALUE IN FRAME frm-input1.
          RELEASE {&tmptable}.
          APPLY 'close' TO SELF.
        END.
        ELSE IF NOT AVAILABLE dbRecop THEN DO:
             MESSAGE simusr.NAME + " is not a receipt operator" VIEW-AS ALERT-BOX.
             RETURN NO-APPLY.
        END.
    END.
    ELSE IF NOT AVAILABLE simusr THEN DO:
        MESSAGE "User does not exist" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
          OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.dStat = "A"  NO-LOCK .
         HIDE FRAME frm-input.
         brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
     RETURN. 
END.

ON 'choose':U OF btn-ok IN FRAME frm-input 
DO:
   
    wsid = {&skey}:SCREEN-VALUE IN FRAME frm-input.
     IF (btn-ok:LABEL IN  FRAME frm-input) = "Save" THEN DO:
         RUN simip.p.
        wsBtn = "ADD".
        CREATE bfr{&tmptable}. 
          ASSIGN bfr{&tmptable}.term_id = wsid
                 bfr{&tmptable}.DeviceName = lc-host
                 bfr{&tmptable}.UID = varUser
                 bfr{&tmptable}.creDate =NOW.
          RELEASE {&tmptable}.
          APPLY 'close' TO SELF.
          OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.dStat = "A"  NO-LOCK .
         HIDE FRAME frm-input.
         brw-{&tmptable}:REFRESH() IN FRAME frm-Main.
    END.
      RETURN. 
END.


ON CHOOSE OF btn-Del DO:
   GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.TERM_id.
    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.TERM_id = wsid EXCLUSIVE-LOCK NO-ERROR.
    wsBtn = "EDIT".
      ASSIGN bfr{&tmptable}.dStat = "D".
       RELEASE {&tmptable}.
       OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.dStat = "A"  NO-LOCK .
     APPLY 'entry' TO btn-exit.
END.


ON CHOOSE OF btn-devAdd IN FRAME frm-main DO:
     btn-ok:LABEL IN  FRAME frm-input = "Save".
    RUN proc-input.
    RETURN.
END.

ON CHOOSE OF btn-devAlloc IN FRAME frm-main DO:
     btn-ok:LABEL IN  FRAME frm-input = "Allocate".
     wsBtn = "EDIT".
    RUN proc-input1.
    RETURN.
END.

ON CHOOSE OF btn-ok IN FRAME frm-user
    OR 'enter':u OF brw-user
    OR 'mouse-select-dblclick' OF brw-user
DO: 
   
        GET CURRENT qry-user EXCLUSIVE-LOCK NO-WAIT.
        FIND FIRST dbRecop WHERE dbRecop.usercode = simusr.userCODE NO-LOCK NO-ERROR.
        IF NOT AVAILABLE dbRecop THEN DO:
            MESSAGE simusr.NAME + " is not a receipt operator" VIEW-AS ALERT-BOX.
        END.
        ELSE IF AVAILABLE dbRecop THEN DO:
            DISPLAY simusr.usercode @ bfr{&tmpTable}.usr simusr.NAME @ bfr{&tmpTable}.NAME WITH FRAME frm-input1.
        END.
       APPLY 'tab' TO SELF.
        RETURN.
   
END.


ON CHOOSE OF btn-devcLOSE IN FRAME frm-main DO:
    webResponse ="".
    FIX-CODEPAGE (webResponse) = "UTF-8".
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.TERM_ID.
    RUN proc-CloseDevice.
    RETURN.
END.

ON CHOOSE OF btn-deviNIT IN FRAME frm-main DO:
    webResponse ="".
    FIX-CODEPAGE (webResponse) = "UTF-8".
    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsid = bfr{&tmptable}.TERM_ID.
    RUN proc-InitDevice.
    RETURN.
END.

{audit.i}
/********** MAIN LOGIC **********/

OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.dStat = "A"  NO-LOCK .
ENABLE ALL EXCEPT  WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
CLOSE QUERY qry-{&tmptable}.
HIDE FRAME frm-main.


PROCEDURE proc-input:
  CLEAR FRAME frm-input ALL.
   ENABLE ALL  WITH FRAME frm-input.
  ASSIGN {&skey}:SCREEN-VALUE = "".
  WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input
      OR CHOOSE OF btn-ok IN FRAME frm-input.
   HIDE FRAME frm-input.
END.

PROCEDURE proc-input1:
  ASSIGN wsid = bfr{&tmptable}.TERM_ID.
   FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.TERM_ID = wsid EXCLUSIVE-LOCK NO-ERROR.
   VIEW FRAME frm-input1.
   ENABLE ALL EXCEPT {&skey}  WITH FRAME frm-input1.
   DISPLAY wsid @ {&skey} 
       bfr{&tmptable}.Name bfr{&tmptable}.usr
       WITH FRAME frm-input1.
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-input1
          OR CHOOSE OF btn-ok IN FRAME frm-input1.
   HIDE FRAME frm-input1.
END.

PROCEDURE proc-InitDevice:
    ASSIGN wsid = bfr{&tmptable}.TERM_ID.
    HttpClient = NEW System.Net.WebClient(). 
    HttpClient:Proxy:Credentials = System.Net.CredentialCache:DefaultNetworkCredentials. 

   webResponse = httpClient:DownloadString("http://127.0.0.1:8086/init/" + wsid).
                                                                                                
    
    HttpClient:Dispose(). 
    DELETE OBJECT HttpClient.

    txtWebResponse = STRING(webResponse).
    txtUnWantedR[1] = "~{".
    txtUnWantedR[2] = "~}".
    txtUnWantedR[3] = "~[".
    txtUnWantedR[4] = "~]".
    txtUnWantedR[5] = "~"".
    txtUnWantedR[6] = "~n".

    DO j = 1 TO EXTENT(txtUnWantedR):
         IF txtUnWantedR[j] <> "" THEN DO:
             txtWebResponse = REPLACE(txtWebResponse, txtUnWantedR[j], "").
         END.
     END.
   txtWebResponse = REPLACE(txtWebResponse, " ","").
     j = 0.
      IF j = 0 THEN DO:
              txtWebResponse = TRIM(txtWebResponse).
               j = LOOKUP("MessageReasonCode:9791",txtWebResponse).
            IF j<>0 THEN DO:
                   rspCode = REPLACE(ENTRY(j,txtWebResponse),":",",").
                   rspCode= ENTRY(2,rspCode).
                   DO i = 1 TO NUM-ENTRIES(txtWebResponse):
                       vTemp = ENTRY(i,txtWebResponse).
                       IF SUBSTR(trim(vTemp),1,6) = "Action"  THEN DO:
                            rspMessage = replace(vTemp,":",",").
                       END.
                    END.
                    rspMessage=ENTRY(2,rspMessage).
                    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
                    ASSIGN wsid = bfr{&tmptable}.TERM_id.
                    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.TERM_id = wsid EXCLUSIVE-LOCK NO-ERROR.
                    ASSIGN bfr{&tmptable}.action = rspMessage
                        bfr{&tmptable}.actionCode = rspCode.
                    RELEASE {&tmptable}.
                    MESSAGE "Terminal initailised" VIEW-AS ALERT-BOX.
                    OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.dStat = "A"  NO-LOCK .
              END.
              ELSE 
                  MESSAGE "Terminal not initialised" VIEW-AS ALERT-BOX.

        END.
END PROCEDURE.

PROCEDURE proc-CloseDevice:
    ASSIGN wsid = bfr{&tmptable}.TERM_ID.
    HttpClient = NEW System.Net.WebClient(). 
    HttpClient:Proxy:Credentials = System.Net.CredentialCache:DefaultNetworkCredentials. 

   webResponse = httpClient:DownloadString("http://127.0.0.1:8086/close/" + wsid).

                                                                                                
    
    HttpClient:Dispose(). 
    DELETE OBJECT HttpClient.

    txtWebResponse = STRING(webResponse).
        txtUnWantedR[1] = "~{".
    txtUnWantedR[2] = "~}".
    txtUnWantedR[3] = "~[".
    txtUnWantedR[4] = "~]".
    txtUnWantedR[5] = "~"".
    txtUnWantedR[6] = "~n".

    DO j = 1 TO EXTENT(txtUnWantedR):
         IF txtUnWantedR[j] <> "" THEN DO:
             txtWebResponse = REPLACE(txtWebResponse, txtUnWantedR[j], "").
         END.
     END.
   txtWebResponse = REPLACE(txtWebResponse, " ","").
     j = 0.
      IF j = 0 THEN DO:
              txtWebResponse = TRIM(txtWebResponse).
               j = LOOKUP("MessageReasonCode:9791",txtWebResponse).
            IF j<>0 THEN DO:
                   rspCode = REPLACE(ENTRY(j,txtWebResponse),":",",").
                   rspCode= ENTRY(2,rspCode).
                   DO i = 1 TO NUM-ENTRIES(txtWebResponse):
                       vTemp = ENTRY(i,txtWebResponse).
                       IF SUBSTR(trim(vTemp),1,6) = "Action"  THEN DO:
                            rspMessage = replace(vTemp,":",",").
                       END.
                    END.
                    rspMessage=ENTRY(2,rspMessage).
                    GET CURRENT qry-{&tmptable} EXCLUSIVE-LOCK NO-WAIT.
                    ASSIGN wsid = bfr{&tmptable}.TERM_id.
                    FIND FIRST bfr{&tmptable} WHERE bfr{&tmptable}.TERM_id = wsid EXCLUSIVE-LOCK NO-ERROR.
                    ASSIGN bfr{&tmptable}.action = rspMessage
                        bfr{&tmptable}.actionCode = rspCode.
                    RELEASE {&tmptable}.
                    MESSAGE "Terminal Closed" VIEW-AS ALERT-BOX.
                    OPEN QUERY qry-{&tmptable} FOR EACH bfr{&tmptable} WHERE bfr{&tmptable}.dStat = "A"  NO-LOCK .
              END.
              ELSE 
                  MESSAGE "Error communicating with Terminal" VIEW-AS ALERT-BOX.

        END.

END PROCEDURE.
