
DEFINE VARIABLE binary-key AS RAW  NO-UNDO.
DEFINE VARIABLE pass AS CHARACTER NO-UNDO.
DEFINE VARIABLE crypto-value AS RAW  NO-UNDO.
DEFINE VARIABLE ucode LIKE simusr.usercode.
DEFINE VARIABLE ucode2 LIKE simusr.usercode.
DEFINE VARIABLE uname LIKE simusr.NAME.
DEFINE VARIABLE uStat LIKE simusr.txtStatus.
FOR EACH simusr WHERE usercode = "":
  DELETE simusr.
END.
FOR EACH simusr WHERE txtPass = "":
  txtPass = "sim".
END.
FOR EACH dbRecop WHERE usercode = "":
  DELETE dbRecop.
END.
FOR EACH dbRecop WHERE txtPass = "":
  txtPass = "sim".
END.

FOR EACH simusr:
    binary-key   = GENERATE-RANDOM-KEY.
    SECURITY-POLICY:SYMMETRIC-ENCRYPTION-KEY = binary-key.
    SECURITY-POLICY:SYMMETRIC-ENCRYPTION-IV = ?.
    pass = simusr.txtPass.
    crypto-value = Encrypt (pass).
    SET password = crypto-value.
    SET bKey = binary-key.
    DISPLAY usercode txtPass. PAUSE 0.
END.
FOR EACH dbRecop:
     binary-key   = GENERATE-RANDOM-KEY.
    SECURITY-POLICY:SYMMETRIC-ENCRYPTION-KEY = binary-key.
    SECURITY-POLICY:SYMMETRIC-ENCRYPTION-IV = ?.
    pass = dbRecop.txtPass.
    crypto-value = Encrypt (pass).
    SET dbrecop.password = crypto-value.
    SET dbrecop.bKey = binary-key.
    DISPLAY usercode txtPass. PAUSE 0.
END.
 
 MESSAGE "Convesrion Complete....THANK YOU"
    VIEW-AS ALERT-BOX.

