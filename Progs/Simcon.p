DEFINE VARIABLE binary-key LIKE tblKey.bKey.
DEFINE VARIABLE clear-text AS CHARACTER NO-UNDO.
DEFINE VARIABLE crypto-value LIKE tblKey.sKey.
DEFINE VARIABLE warn-days LIKE tblKey.dWarn.
DEFINE VARIABLE Lic-to LIKE tblKey.lTo.
DEF VAR wsd AS INT.
DEF VAR wsAns AS LOGICAL.
DEF VAR wsg AS LOGICAL INITIAL NO.
FIND FIRST tblKey WHERE NO-ERROR.
   IF AVAILABLE tblKey THEN DO:
      binary-key = tblKey.bKey.
      crypto-value=tblKey.sKey.
      warn-days = tblKey.dWarn.
      Lic-to =tblKey.lTo.
   END.
SECURITY-POLICY:SYMMETRIC-ENCRYPTION-KEY = binary-key.
SECURITY-POLICY:SYMMETRIC-ENCRYPTION-IV=?.
clear-text =GET-STRING(DECRYPT(crypto-value),1).
ASSIGN wsd = (DATE(clear-text) - TODAY).
DO WHILE wsg = NO:
    IF wsd < 0 THEN DO:
        wsg = NO.
        MESSAGE "Your Licence has expired...Would you like to renew it no"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "System Licence Renewal"
            UPDATE wsAns.
        IF wsAns = YES THEN DO:
            RUN eglic.p.
        END.
        ELSE DO:
            MESSAGE "License has expired....System will TERMINATE" VIEW-AS ALERT-BOX.
            QUIT.
        END.
    END.  
    ELSE IF wsd <= warn-days AND wsd >= 0 THEN DO:
         MESSAGE "License will expire in " wsd " days .Would you like to renew it now"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "System Licence Renewal"
            UPDATE wsAns.
         IF wsAns = YES THEN DO:
             wsg = NO.
             RUN eglic.p.
         END.
         ELSE DO:
             wsg = YES.
             RUN simacc.w.
         END.   
    END.  
    ELSE DO:
        wsg = YES.
         RUN simacc.w.
    END.
END.
