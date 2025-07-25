DEF VAR wsiRate AS DEC.
DEF VAR wsArr  AS DEC.
DEF VAR wsInAmt AS DEC.
DEF VAR X AS INT.
FOR EACH dbcmf:
    ASSIGN wsiRate = 0
           wsArr = 0.
    FIND FIRST dbctf WHERE dbctf.cons = dbcmf.cons NO-LOCK NO-ERROR.
    IF AVAILABLE dbctf THEN DO:
        wsiRate = (dbctf.interest / 1200).
        FOR EACH dbblf WHERE dbblf.dbacc = dbcmf.dbacc:
           ASSIGN wsInAmt =  ROUND((dbblf.dbAmt[1] * wsIRate),2).
           DISPLAY dbcmf.dbacc dbblf.sgrp dbblf.dbAmt[1] wsInAmt.
        END.
     END.
END.
