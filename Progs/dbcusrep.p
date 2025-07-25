SESSION:DATA-ENTRY-RETURN = TRUE.
DEF VAR wsCurr LIKE dbcmf.balbf.
DEF VAR wsTotal LIKE dbcmf.balbf.
DEF VAR wsFile AS CHAR.
DEF STREAM a.
FIND FIRST simctr NO-LOCK NO-ERROR.
wsFile = simctr.repDir + "customers.csv".
SESSION:SET-WAIT-STATE("").
OUTPUT STREAM a TO VALUE (wsFile).
EXPORT STREAM a DELIMITER ',' "ACCOUNT" "NAME" "ID" "ADDRESS" "ADDRESS" "ADDRESS" "CELL" "EMAIL" "CURRENT" "TOTAL".
FOR EACH dbcmf  NO-LOCK:
   wsTotal = 0.
   wsCurr = 0.
  wsTotal = 0.
    FOR EACH dbblf WHERE dbblf.dbacc = dbcmf.dbacc NO-LOCK.
         wsCurr = wsCurr + dbblf.amt[1]  + dbblf.amt[2].
         wsTotal = wsTotal + dbblf.amt[1]+ dbblf.amt[2]+ dbblf.amt[3]+ dbblf.amt[4]+ dbblf.amt[5]+ dbblf.amt[6]+ dbblf.amt[7]+ dbblf.amt[8]+ dbblf.amt[9]+ dbblf.amt[10]+ dbblf.amt[11]+ dbblf.amt[12]+ dbblf.amt[13]+ dbblf.amt[14]+ dbblf.amt[15].
    END.
    EXPORT STREAM a DELIMITER ',' dbcmf.dbacc dbcmf.NAME dbcmf.regid add1 add2 add3 cell emailadd wsCurr wsTotal.
END.
OS-COMMAND NO-WAIT VALUE(wsFile).
