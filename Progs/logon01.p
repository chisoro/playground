/* Program.................Logon01.p
   Notes:...... User password change
   Author:.................S. Mawire
   Modified:...............S. Chisoro 04/06/24
*/session:DATA-ENTRY-RETURN = TRUE.
DEFINE VARIABLE binary-key LIKE simusr.bKey.
 DEFINE VARIABLE crypto-value LIKE simusr.password.
 DEFINE VARIABLE clear-text AS CHARACTER NO-UNDO.

DEF SHARED VAR wsName LIKE simusr.Name.
DEFINE  SHARED VARIABLE lc-address AS CHAR NO-UNDO.
DEFINE  SHARED VARIABLE lc-host AS CHAR NO-UNDO.
DEFINE VARIABLE w_password AS CHARACTER CASE-SENSITIVE FORMAT "X(256)"
     LABEL "CURRENT Password" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.
DEFINE VARIABLE w_password1 LIKE w_password CASE-SENSITIVE.
DEFINE VARIABLE w_password2 LIKE w_password CASE-SENSITIVE.
DEFINE SHARED VARIABLE varUser AS CHARACTER FORMAT "X(3)".

DEFINE BUTTON btnok 
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

DEFINE FRAME frm-Main
    varUser AT ROW 2.81 COL 19 COLON-ALIGNED WIDGET-ID 4 LABEL "USER" VIEW-AS TEXT
     simusr.NAME VIEW-AS TEXT NO-LABEL NO-TAB-STOP
     w_password AT ROW 4.71 COL 30 COLON-ALIGNED WIDGET-ID 6 PASSWORD-FIELD
     w_password1 AT ROW 5.71 COL 30 COLON-ALIGNED LABEL "NEW Password" PASSWORD-FIELD
     w_password2 AT ROW 6.71 COL 30 COLON-ALIGNED LABEL "Confirm Password" PASSWORD-FIELD
     btnok AT ROW 8.38 COL 21 WIDGET-ID 8
     BtnCancel AT ROW 8.38 COL 48 WIDGET-ID 10
     RECT-1 AT ROW 1.48 COL 3 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 10.4 ROW 5.19
         SIZE 70.8 BY 10.91
         BGCOLOR 3 FGCOLOR 0 
         CANCEL-BUTTON BtnCancel WIDGET-ID 100.

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



ON 'enter':U OF  w_password
    OR 'TAB':U OF  w_password
DO:
   /*FIND FIRST simusr WHERE simusr.usercode = varuser. */
   binary-key = simusr.bKey.
   crypto-value = simusr.password.
   SECURITY-POLICY:SYMMETRIC-ENCRYPTION-KEY = binary-key.
   SECURITY-POLICY:SYMMETRIC-ENCRYPTION-IV=?.
  clear-text =GET-STRING(DECRYPT(crypto-value),1).
   IF  w_password:SCREEN-VALUE = clear-text THEN DO:
        APPLY 'ENTRY' TO w_password1.
    END.
    ELSE DO:
        MESSAGE "Invalid Password, please try again...." VIEW-AS ALERT-BOX.
        APPLY 'CLOSE' TO THIS-PROCEDURE IN FRAME frm-main.
    END.
    RETURN.
END.

ON 'tab':U OF w_password1
DO:
    IF w_password1:SCREEN-VALUE = "" THEN DO:
        MESSAGE "New Password cannot be blank. Try Again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    ELSE  DO:
        /*check password complexity*/
        lValid = verifyPasswordComplexity(INPUT w_password1:SCREEN-VALUE).
        IF lValid THEN DO:
            APPLY 'ENTRY' TO w_password2.
           RETURN.
        END.
           
        ELSE DO:
            MESSAGE cErrorMessage VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END.
   END.
        
END.


ON 'enter':U OF  w_password2
    OR 'TAB':U OF  w_password2
    OR CHOOSE OF btnOk
DO:
    IF  w_password1:SCREEN-VALUE =  w_password2:SCREEN-VALUE 
        AND w_password1:SCREEN-VALUE <> "" THEN DO:
        binary-key = GENERATE-RANDOM-KEY.
       SECURITY-POLICY:SYMMETRIC-ENCRYPTION-KEY =binary-key.
       SECURITY-POLICY:SYMMETRIC-ENCRYPTION-IV = ?.
       crypto-value =ENCRYPT(w_password1:SCREEN-VALUE).
       simusr.bKey=binary-key.
       simusr.Password = crypto-value.
       simusr.NextExpire = (TODAY + 90).
       CREATE tblLog.
         ASSIGN
             db005.tblLog.action = "Password Change"
             db005.tblLog.COUNT = 1
             db005.tblLog.DeviceName = lc-host
             db005.tblLog.ipaddress = lc-address
             db005.tblLog.trDate = NOW
             db005.tblLog.result = "Password Changed"
             db005.tblLog.UID = varUser.
        MESSAGE "Password has been changed successfully..." VIEW-AS ALERT-BOX.
        APPLY 'CLOSE' TO THIS-PROCEDURE IN FRAME frm-main.
    END.
    ELSE DO:
        MESSAGE "Password confirmation failed NO Password changed...." VIEW-AS ALERT-BOX.
        APPLY 'CLOSE' TO THIS-PROCEDURE IN FRAME frm-main.
    END.
    RETURN.
END.

/********** MAIN LOGIC **********/

ENABLE ALL WITH FRAME frm-main.
FIND FIRST simusr WHERE simusr.usercode = varUser NO-ERROR.
IF AVAILABLE simusr THEN DO:
    DISPLAY simusr.usercode @ varUser simusr.NAME WITH FRAME frm-main.
END.
WAIT-FOR CHOOSE OF btncancel  OR CHOOSE OF btnOk OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
/*MESSAGE varUser VIEW-AS ALERT-BOX.*/
VIEW FRAME frm-track.
ENABLE ALL WITH FRAME frm-track.
OPEN QUERY qry-track FOR EACH dbTrack WHERE dbTrack.UID = varUser NO-LOCK.
   
