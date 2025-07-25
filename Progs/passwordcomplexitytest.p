DEFINE VARIABLE cPassword AS CHARACTER NO-UNDO.
DEFINE VARIABLE lValid AS LOGICAL NO-UNDO.
DEFINE VARIABLE cErrorMessage AS CHARACTER NO-UNDO.
DEFINE VARIABLE lHasUpper AS LOGICAL NO-UNDO.
DEFINE VARIABLE lHasLower AS LOGICAL NO-UNDO.
DEFINE VARIABLE lHasDigit AS LOGICAL NO-UNDO.
DEFINE VARIABLE lHasSpecial AS LOGICAL NO-UNDO.
DEFINE VARIABLE cSpecialChars AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER.

lHasUpper = FALSE.
lHasLower = FALSE.
lHasDigit =  FALSE.
lHasSpecial = FALSE.
cSpecialChars = "!@#$%^&*()-_=+[]{}~\|;:''',.<>/?`~~".
   
FUNCTION verifyPasswordComplexity RETURNS LOGICAL (INPUT cPassword AS CHARACTER):
    DEFINE VARIABLE lValid AS LOGICAL NO-UNDO.
    lValid = TRUE.

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
        IF INDEX("ABCDEFGHIJKLMNOPQRSTUVWXYZ", SUBSTRING(cPassword, i, 1)) > 0 THEN DO:
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
         IF INDEX("abcdefghijklmnopqrstuvwxyz", SUBSTRING(cPassword, i, 1)) > 0 THEN DO:
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

cPassword = "Test@2025".

lValid = verifyPasswordComplexity(INPUT cPassword).

IF lValid THEN
    MESSAGE "Password meets complexity requirements." VIEW-AS ALERT-BOX.
ELSE
    MESSAGE cErrorMessage VIEW-AS ALERT-BOX.


