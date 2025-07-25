/* Program.................Logon.p
   Notes:...... User Logon and verification
   Author:.................S. Mawire
   Edited:...................S. Chisoro
*/


/*CREATE WIDGET-POOL.*/
 SESSION:DATA-ENTRY-RETURN = YES.
 DEFINE VARIABLE binary-key LIKE simusr.bKey.
 DEFINE VARIABLE crypto-value LIKE simusr.password.
 DEFINE VARIABLE clear-text AS CHARACTER NO-UNDO.

/*DEFINE VARIABLE oIPHostEntry AS System.Net.IPHostEntry NO-UNDO.
DEFINE VARIABLE oIPAddress   AS System.Net.IPAddress   NO-UNDO.*/
DEFINE VARIABLE cIPAddresses AS CHARACTER              NO-UNDO.
DEFINE SHARED VARIABLE varUser AS CHARACTER.
DEFINE  SHARED VARIABLE lc-address AS CHAR NO-UNDO.
DEFINE  SHARED VARIABLE lc-host AS CHAR NO-UNDO.

DEFINE VARIABLE iCount       AS INTEGER                NO-UNDO INITIAL 1.

DEF SHARED VAR wsName LIKE simusr.Name.
DEF SHARED VAR wsCo   LIKE SIMCTR.CONAME.
DEFINE VAR X AS INT.

DEFINE VARIABLE w_password AS CHARACTER CASE-SENSITIVE FORMAT "X(256)" 
     LABEL "Password" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE NEW SHARED VARIABLE w_userno AS CHARACTER FORMAT "X(256)":U 
     LABEL "Logon Code" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.
DEF VAR w_response LIKE w_password CASE-SENSITIVE NO-UNDO.
DEF VAR w_response1 LIKE w_password CASE-SENSITIVE NO-UNDO.

DEFINE BUTTON btLogin 
     LABEL "Login" 
     SIZE 15 BY 1.14
     BGCOLOR 10 .

DEFINE BUTTON btnOk 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 10 .

DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Cancel" 
     SIZE 9 BY 1.14
     BGCOLOR 12 .

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 68 BY 8.81
     BGCOLOR 6 FGCOLOR 0 .


DEFINE VARIABLE cPassword AS CHARACTER CASE-SENSITIVE NO-UNDO.
DEFINE VARIABLE lValid AS LOGICAL NO-UNDO.
DEFINE VARIABLE cErrorMessage AS CHARACTER NO-UNDO.
DEFINE VARIABLE lHasUpper AS LOGICAL NO-UNDO.
DEFINE VARIABLE lHasLower AS LOGICAL NO-UNDO.
DEFINE VARIABLE lHasDigit AS LOGICAL NO-UNDO.
DEFINE VARIABLE lHasSpecial AS LOGICAL NO-UNDO.
DEFINE VARIABLE cSpecialChars AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER.
DEFINE VARIABLE dWarn AS INTEGER.

lHasUpper = FALSE.
lHasLower = FALSE.
lHasDigit =  FALSE.
lHasSpecial = FALSE.
cSpecialChars = "!@#$%^&*()-_=+[]{}~\|;:''',.<>/?`~~".


FUNCTION verifyPasswordComplexity RETURNS LOGICAL (cPassword AS CHARACTER):
    DEFINE VARIABLE lValid AS LOGICAL NO-UNDO.
    lValid = TRUE.
    DEFINE VARIABLE pass AS CHARACTER CASE-SENSITIVE NO-UNDO.
    pass = cPassword.
    cErrorMessage = "".
    IF LENGTH(cPassword) < 8 THEN
    DO:
        lValid = FALSE.
        cErrorMessage = "Password should be at least 8 characters long.".
    END.

    /* Digit check - manually scan characters */
    DO i = 1 TO LENGTH(cPassword):
        IF INDEX("0123456789", SUBSTRING(cPassword, i, 1)) > 0 THEN DO:
            lHasDigit = TRUE.
            LEAVE.
        END.
    END.
    IF NOT lHasDigit THEN DO:
        lValid = FALSE.
        IF cErrorMessage <> "" THEN cErrorMessage = cErrorMessage + "~n".
        cErrorMessage = cErrorMessage + "Password should contain at least one digit.".
    END.

    DO i = 1 TO LENGTH(cPassword):
        IF INDEX("ABCDEFGHIJKLMNOPQRSTUVWXYZ", SUBSTRING(pass, i, 1)) > 0 THEN DO:
                lHasUpper = TRUE.
                LEAVE.
            END.
     END.
     
        
     IF NOT lHasUpper THEN DO:
            lValid = FALSE.
            IF cErrorMessage <> "" THEN cErrorMessage = cErrorMessage + "~n".
            cErrorMessage = cErrorMessage + "Password should contain at least one uppercase letter.".
     END.
        
     DO i = 1 TO LENGTH(cPassword):
         IF INDEX("abcdefghijklmnopqrstuvwxyz", SUBSTRING(pass, i, 1)) > 0 THEN DO:
                lHasLower = TRUE.
                LEAVE.
         END.
     END.
        
    IF NOT lHasLower THEN DO:
            lValid = FALSE.
            IF cErrorMessage <> "" THEN cErrorMessage = cErrorMessage + "~n".
            cErrorMessage = cErrorMessage + "Password should contain at least one lowercase letter.".
    END.
        
   DO i = 1 TO LENGTH(cPassword):
        IF INDEX(cSpecialChars, SUBSTRING(cPassword, i, 1)) > 0 THEN DO:
            lHasSpecial = TRUE.
            LEAVE.
        END.
    END.
    IF NOT lHasSpecial THEN DO:
        lValid = FALSE.
        IF cErrorMessage <> "" THEN cErrorMessage = cErrorMessage + "~n".
        cErrorMessage = cErrorMessage + "Password should contain at least one special character.".
    END.

    RETURN lValid.
END FUNCTION.


/* ************************  Frame Definitions  *********************** */
DEFINE FRAME frm-newPass
     w_response AT ROW 1.71 COL 30 COLON-ALIGNED LABEL "NEW Password" PASSWORD-FIELD
     w_response1 AT ROW 2.71 COL 30 COLON-ALIGNED LABEL "Confirm Password" PASSWORD-FIELD
     btLogin LABEL "OK" AT ROW 8.38 COL 21 WIDGET-ID 8
     BtnCancel AT ROW 8.38 COL 48 WIDGET-ID 10
     WITH 1 DOWN NO-BOX KEEP-TAB-ORDER  
         SIDE-LABELS NO-UNDERLINE THREE-D 
         SIZE 75.8 BY 10.91
         BGCOLOR 3 FGCOLOR 0 
         CANCEL-BUTTON BtnCancel OVERLAY CENTERED VIEW-AS DIALOG-BOX.

DEFINE FRAME fMain
     w_userno AT ROW 4.81 COL 19 COLON-ALIGNED WIDGET-ID 4
     simusr.NAME VIEW-AS FILL-IN NO-LABEL NO-TAB-STOP
     w_password AT ROW 6.71 COL 19 COLON-ALIGNED WIDGET-ID 6 PASSWORD-FIELD 
     btLogin AT ROW 8.38 COL 21 WIDGET-ID 8
     BtnCancel AT ROW 8.38 COL 48 WIDGET-ID 10
     RECT-1 AT ROW 1.48 COL 3 WIDGET-ID 2
   WITH 1 DOWN NO-BOX KEEP-TAB-ORDER  
         SIDE-LABELS NO-UNDERLINE THREE-D 
         SIZE 75.8 BY 10.91
         BGCOLOR 3 FGCOLOR 0 
         CANCEL-BUTTON BtnCancel VIEW-AS DIALOG-BOX.
FRAME fMain:TITLE="SIMACC ACCOUNTING".

/* ************************  Control Triggers  ************************ */

ON 'enter' OF w_userno IN FRAME fMain
   OR 'tab' OF w_userno IN FRAME fMain
    OR 'leave' OF w_userno IN FRAME fMain
DO:
    FIND FIRST simusr WHERE simusr.usercode = w_userno:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF NOT AVAILABLE simusr THEN DO:
        X = X + 1.
        MESSAGE "Invalid User entered..please try again" VIEW-AS ALERT-BOX.
        RUN simip.p.
        CREATE tblLog.
         ASSIGN
             db005.tblLog.action = "login"
             db005.tblLog.COUNT = X
             db005.tblLog.DeviceName = lc-host
             db005.tblLog.ipaddress = lc-address
             db005.tblLog.trDate = NOW
             db005.tblLog.result = "Invalid Account"
             db005.tblLog.UID = varUser.
        IF X < 3 THEN
           RETURN NO-APPLY.
        ELSE QUIT.
    END.
    ELSE IF AVAILABLE simusr AND simusr.txtStatus = "L" THEN DO:
        MESSAGE "USER IS LOCKED... cannot LOG" VIEW-AS ALERT-BOX.
        RUN simip.p.
          CREATE tblLog.
         ASSIGN
             db005.tblLog.action = "login"
             db005.tblLog.COUNT = X
             db005.tblLog.DeviceName = lc-host
             db005.tblLog.ipaddress = lc-address
             db005.tblLog.trDate = NOW
             db005.tblLog.result = "Account Locked"
             db005.tblLog.UID = varUser.
        RETURN NO-APPLY.
    END.
    ELSE IF AVAILABLE simusr AND (TODAY > simusr.NextExpire OR simusr.nextExpire = ?) THEN DO:
              MESSAGE "USER PASSWORD EXPIRED. PLEASE RENEW" VIEW-AS ALERT-BOX.
              varUser = w_userno:SCREEN-VALUE.
              ENABLE ALL WITH FRAME frm-Newpass.
              APPLY 'entry' TO w_response.
              WAIT-FOR CLOSE OF THIS-PROCEDURE.
    END.
    ELSE DO:
        dWarn = simusr.NextExpire - TODAY.
        IF dWarn <= 10 THEN
            MESSAGE  STRING(dWarn) + " Days Left For Your Password To Expire. Please Change." VIEW-AS ALERT-BOX.
        DISPLAY simusr.NAME WITH FRAME fmain.
        ASSIGN w_userno.
        binary-key = simusr.bKey.
        crypto-value = simusr.password.
        SECURITY-POLICY:SYMMETRIC-ENCRYPTION-KEY = binary-key.
        SECURITY-POLICY:SYMMETRIC-ENCRYPTION-IV=?.
        clear-text =GET-STRING(DECRYPT(crypto-value),1).
        IF clear-text = "sim" THEN DO:
          /*VIEW FRAME frm-Newpass. */
          ENABLE ALL WITH FRAME frm-Newpass.
          APPLY 'entry' TO w_response.
          WAIT-FOR CLOSE OF THIS-PROCEDURE.
        END.
       APPLY 'tab' TO SELF.
    END.
    RETURN.
END.

ON CHOOSE OF btLogin IN FRAME fMain /* Login */
DO:
    ASSIGN varUser = w_userno:SCREEN-VALUE.
    IF simusr.usercode = w_userno:SCREEN-VALUE 
        AND COMPARE(clear-text, "=", w_password:SCREEN-VALUE, "CASE-SENSITIVE") THEN
    DO:
        ASSIGN varUser = w_userno:SCREEN-VALUE.
        FIND FIRST simusr WHERE simusr.usercode = w_userno:SCREEN-VALUE NO-LOCK NO-ERROR. 
            ASSIGN wsName  = simusr.NAME.
           RUN simip.p.
         CREATE tblLog.
         ASSIGN
            db005.tblLog.action = "login"
             db005.tblLog.COUNT = X
             db005.tblLog.DeviceName = lc-host
             db005.tblLog.ipaddress = lc-address
             db005.tblLog.trDate = NOW
             db005.tblLog.result = "Success"
             db005.tblLog.UID = varUser.
        HIDE FRAME fMain.
        RETURN.
    END.
    ELSE DO:
        IF COMPARE(clear-text, "<>", w_password:SCREEN-VALUE, "Case-sensitive") THEN DO:
            X = X + 1.
            MESSAGE "Invalid Password entered..please try again" VIEW-AS ALERT-BOX.
            RUN simip.p.
            CREATE tblLog.
         ASSIGN
             db005.tblLog.action = "login"
             db005.tblLog.COUNT = X
             db005.tblLog.DeviceName = lc-host
             db005.tblLog.ipaddress = lc-address
             db005.tblLog.trDate = NOW
             db005.tblLog.result = "Wrong Password"
             db005.tblLog.UID = varUser.
            IF X < 3 THEN DO:
                CLEAR  FRAME fMain.
                APPLY 'entry' TO w_userno.
                RETURN NO-APPLY.
            END.
                 
            ELSE DO: 
                FIND FIRST simusr WHERE simusr.usercode = w_userno:SCREEN-VALUE SHARE-LOCK NO-ERROR.
               simusr.txtStatus = "L".
               MESSAGE "Too many failures Account has been LOCKED" VIEW-AS ALERT-BOX.
               RUN simip.p.
               CREATE tblLog.
         ASSIGN
             db005.tblLog.action = "login"
             db005.tblLog.COUNT = X
             db005.tblLog.DeviceName = lc-host
             db005.tblLog.ipaddress = lc-address
             db005.tblLog.trDate = NOW
             db005.tblLog.result = "Locked"
             db005.tblLog.UID = varUser.
               QUIT.
            END.
        END.
    END.
END.

ON 'tab':U OF w_response IN FRAME frm-Newpass
DO:
    IF w_response:SCREEN-VALUE IN FRAME frm-newpass = "" THEN DO:
        MESSAGE "New Password cannot be blank. Try Again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE  DO:
        /*check password complexity*/
        
        lValid = verifyPasswordComplexity(INPUT w_response:SCREEN-VALUE IN FRAME frm-newpass).
        IF lValid THEN DO:
            APPLY 'ENTRY' TO w_response1.
           RETURN.
        END.
           
        ELSE DO:
            MESSAGE cErrorMessage VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
   END.
        
END.

ON 'tab':U OF w_response1 IN FRAME frm-Newpass 
DO:
    ASSIGN w_response w_response1.
    IF  w_response <> w_response1 THEN DO:
        MESSAGE "Password Creation Failed" VIEW-AS ALERT-BOX.
        RUN simip.p.
          CREATE tblLog.
         ASSIGN
             db005.tblLog.action = "Password Change"
             db005.tblLog.COUNT = X
             db005.tblLog.DeviceName = lc-host
             db005.tblLog.ipaddress = lc-address
             db005.tblLog.trDate = NOW
             db005.tblLog.result = "Passwords not Matching"
             db005.tblLog.UID = varUser.
         QUIT.
    END.
    ELSE DO:
       FIND FIRST simusr WHERE simusr.usercode = varUser SHARE-LOCK NO-ERROR.
       binary-key = GENERATE-RANDOM-KEY.
       SECURITY-POLICY:SYMMETRIC-ENCRYPTION-KEY =binary-key.
       SECURITY-POLICY:SYMMETRIC-ENCRYPTION-IV = ?.
       crypto-value =ENCRYPT(w_response).
       simusr.bKey=binary-key.
           simusr.Password = crypto-value.
           simusr.NextExpire = (TODAY + 90).
       
       
       MESSAGE "New password created please relaunch the application" VIEW-AS ALERT-BOX.
       RUN simip.p.
        CREATE tblLog.
         ASSIGN
             db005.tblLog.action = "login"
             db005.tblLog.COUNT = X
             db005.tblLog.DeviceName = lc-host
             db005.tblLog.ipaddress = lc-address
             db005.tblLog.trDate = NOW
             db005.tblLog.result = "Success"
             db005.tblLog.UID = varUser.
       QUIT.
    END.
    RETURN.
END.

ON CHOOSE OF btLogin IN FRAME frm-Newpass 
DO:
    HIDE FRAME frm-newPass.
    APPLY 'TAB' TO w_response1 IN FRAME frm-Newpass.
    RETURN.
END.

ON CHOOSE OF  btnCancel IN FRAME frm-newpass
DO:
    RUN simstop.p.
    HIDE FRAME frm-newPass.
    QUIT.
END.

ON CHOOSE OF BtnCancel IN FRAME fMain /* Cancel */
DO:
    RUN simstop.p.
    HIDE FRAME fmain.
    QUIT.
END.

/*................................ Main Block ....................*/
ENABLE ALL WITH FRAME fMain.
/*oIPHostEntry = System.Net.Dns:GetHostEntry(System.Net.Dns:GetHostName()).
DO WHILE TRUE:
  oIPAddress = CAST(oIPHostEntry:AddressList:GetValue(iCount),"System.Net.IPAddress") NO-ERROR.
  IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
    LEAVE.

  cIPAddresses = cIPAddresses + (IF (cIPAddresses GT "") EQ TRUE THEN CHR(10) ELSE "") + oIPAddress:ToString().
  iCount = iCount + 1.
END. */

DISPLAY w_userno w_password 
      WITH FRAME fMain.
  ENABLE RECT-1 w_userno w_password btLogin BtnCancel 
      WITH FRAME fMain.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE OR CHOOSE OF  btLogin IN FRAME fMain.
  HIDE FRAME fMain.

