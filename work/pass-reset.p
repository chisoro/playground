DEFINE VARIABLE binary-key AS RAW  NO-UNDO.
DEFINE VARIABLE clear-text AS CHARACTER NO-UNDO INITIAL '31/12/24'.
DEFINE VARIABLE crypto-value AS RAW  NO-UNDO.
DEFINE VARIABLE warning-days LIKE tblKey.dWarn.
DEFINE VARIABLE license-to LIKE tblKey.lTo.
FIND FIRST simusr WHERE usercode = "1" NO-ERROR.
binary-key   = GENERATE-RANDOM-KEY.

       binary-key = GENERATE-RANDOM-KEY.
       SECURITY-POLICY:SYMMETRIC-ENCRYPTION-KEY = binary-key.
       SECURITY-POLICY:SYMMETRIC-ENCRYPTION-IV = ?.
       clear-text = "sim".
       crypto-value =ENCRYPT(clear-text).
       simusr.bKey=binary-key.
       simusr.password    =  crypto-value.
