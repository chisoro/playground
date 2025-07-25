DEFINE VARIABLE binary-key AS RAW  NO-UNDO.
DEFINE VARIABLE clear-text AS CHARACTER NO-UNDO INITIAL '31/07/25'.
DEFINE VARIABLE crypto-value AS RAW  NO-UNDO.
DEFINE VARIABLE warning-days LIKE tblKey.dWarn.
DEFINE VARIABLE license-to LIKE tblKey.lTo.
binary-key   = GENERATE-RANDOM-KEY.
warning-days = 15.
license-to   = 'CTC'.
clear-text = license-to + clear-Text.
SECURITY-POLICY:SYMMETRIC-ENCRYPTION-KEY = binary-key.
SECURITY-POLICY:SYMMETRIC-ENCRYPTION-IV = ?.
crypto-value = Encrypt (clear-text).
IF NOT CAN-FIND(FIRST tblkey) THEN DO:
    CREATE tblkey.
    SET sKey  = crypto-value.
    SET bKey  = binary-key.
    SET dWarn = warning-days.
    /*SET lTo   = license-to. */
END.
ELSE DO:
    FIND FIRST tblKey NO-ERROR.
        SET sKey  = crypto-value.
        SET bKey  = binary-key.
        SET dWarn = warning-days.
         SET lTo   = license-to.
END.
MESSAGE "Your system licence has been successfuly renewed....THANK YOU"
    VIEW-AS ALERT-BOX.
