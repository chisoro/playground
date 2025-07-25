DEF VAR X AS INT.
DEF STREAM a.
DEF VAR wsDes AS CHAR FORM "x(40)".

OUTPUT STREAM a TO c:\simacc\test.csv.
DEF TEMP-TABLE dbtemp
    FIELD wsCons  LIKE dbctf.cons
    FIELD wssgrp LIKE dbsgr.sgrp
    FIELD wsAmt   LIKE dbcmf.bal FORM "zzz,zzz,zz9.99-".

FORM wsDes AT 10 LABEL "NARRATION"
     dbtemp.wsAmt   LABEL "AMOUNT"
     HEADER  skip(2) "COLLECTION DISTRIBUTION STATISTICS" AT 5 SKIP(2)
    WITH DOWN STREAM-IO FONT 10 CENTERED NO-LABEL NO-BOX FRAME B WIDTH 132.

FOR EACH dbhtf WHERE prog = "recupd.p" 
        AND accper >= 202201 AND accper <= 202207 NO-LOCK
                    BREAK BY sgrp:
        FIND FIRST dbcmf WHERE dbcmf.dbacc = dbhtf.dbacc NO-LOCK NO-ERROR.
        FIND FIRST dbtemp WHERE dbtemp.wsCons = dbcmf.cons AND dbtemp.wssgrp = dbhtf.sgrp NO-ERROR.
        IF NOT AVAILABLE dbtemp THEN DO:
            CREATE dbtemp.
            ASSIGN dbtemp.wscons = dbcmf.cons
                   dbtemp.wssgrp = dbhtf.sgrp.
        END.
        ASSIGN dbtemp.wsAmt = dbtemp.wsAmt + dbhtf.amt.   
END.
EXPORT STREAM a DELIMITER ','  "NARRATION" "AMOUNT".
FOR EACH dbtemp NO-LOCK BREAK BY wsCons BY wssgrp .
    IF FIRST-OF(wsCons) THEN DO:
        FIND FIRST dbctf WHERE dbctf.cons = wsCons NO-LOCK NO-ERROR.
        IF AVAILABLE dbctf THEN
            wsdes = dbctf.descrip.
        ELSE IF NOT AVAILABLE dbctf THEN
            wsdes = "N/A".
        EXPORT STREAM a DELIMITER ','  wsDes.
    END.
    FIND FIRST dbsgr WHERE dbsgr.sgrp = dbtemp.wssgrp NO-LOCK NO-ERROR.
    IF AVAILABLE dbsgr THEN
      wsDes = dbsgr.descrip.
    ELSE IF NOT AVAILABLE dbsgr THEN
        wsdes = "N/A".
     EXPORT  STREAM a DELIMITER ',' "  " + wsDes  wsAmt.
    IF LAST-OF(wscons) THEN
       EXPORT STREAM a DELIMITER ','   "" .
END.

