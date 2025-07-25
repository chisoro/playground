DEF  SHARED  VAR wsa LIKE db003.dbblf.dbAcc.
DEF  SHARED  VAR wsbal LIKE inc.munbmf.bal[1].
DEF  SHARED  VAR wsint LIKE inc.munbmf.interest[1].
DEF  SHARED  VAR wsbtotal LIKE inc.munbmf.bal[1].
DEF  SHARED  VAR wsctotal LIKE inc.munbmf.bal[1].
DEF  SHARED  VAR wst LIKE inc.munbmf.TYPE.
DEF  SHARED  VAR wsRef AS CHAR FORM "x(15)".
DEF  SHARED  VAR wstype AS INT FORM "99".
DEF  SHARED  VAR wsdif LIKE inc.munbmf.bal[1].
DEF  SHARED  VAR wsbt LIKE inc.mundcf.batch-no.
DEF  SHARED  VAR stAcc   LIKE db003.dbcmf.dbacc.
DEF  SHARED  VAR enAcc   LIKE db003.dbcmf.dbacc.
DEF  SHARED  VAR wsstatus  LIKE db003.dbcmf.dbacc.
DEF  SHARED  VAR wsdescrip LIKE  glmf.DESCRIPTION.
DEF  SHARED  VAR wsAcct LIKE  glmf.acct.
DEF  SHARED  VAR i AS INTEGER.
DEF  SHARED  VAR wsseq AS INTEGER.
DEF SHARED VAR wsrate LIKE tblforex.decRate.
wsseq = 0.
wsbtotal = 0.
wsctotal = 0.
FOR EACH inc.munbmf WHERE inc.munbmf.company = 0 AND inc.munbmf.acc >= stAcc AND inc.munbmf.acc <= enAcc NO-LOCK:
       pause 0 before-hide. 
       DISPLAY inc.munbmf.acc WITH FRAME x ROW 18 NO-LABEL OVERLAY.
        i = 1.
        wsbal = 0.
        wsint = 0.
       DO i = 1 TO 14:
            wsbal = wsbal + inc.munbmf.bal[i]  + inc.munbmf.interest[i].
        END.
        FIND FIRST db003.dbblf WHERE db003.dbblf.dbacc = inc.munbmf.Acc AND db003.dbblf.sgrp = inc.munbmf.TYPE NO-LOCK NO-ERROR.
        IF AVAILABLE db003.dbblf THEN DO:
        /*compare the ammounts and create a journal*/
            wsdif = ROUND((db003.dbblf.dbAmt[1] * wsRate),2) - wsbal.
            IF wsdif <> 0 THEN DO:
                wsseq = wsseq + 1.
                wsbtotal = wsbtotal + wsdif.
                wsctotal = wsctotal + wsdif.
                CREATE mundcf.
                 ASSIGN inc.mundcf.acc = inc.munbmf.acc
                     inc.mundcf.batch-no = wsbt
                     inc.mundcf.seq = wsseq
                     inc.mundcf.bmf-type = inc.munbmf.TYPE
                     inc.mundcf.tr-date = TODAY
                     inc.mundcf.ref = wsRef
                     inc.mundcf.amt = wsdif
                     inc.mundcf.vat-amt = ROUND((db003.dbblf.vat * wsRate),2) - inc.munbmf.vat-amt
                     inc.mundcf.age = 3
                     inc.mundcf.gl-alloc = STRING(wsAcct)
                     inc.mundcf.trans-desc = "Exchange Gain/Loss Adjustment".
            END.
        END.
    END.
