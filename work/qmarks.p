DEF VAR X AS INTEGER.
OUTPUT TO C:\Users\ADMIN\OneDrive\Documents\Projects\Progress\Simacc\REPORTS\erroMeterRead.csv.
EXPORT DELIMITER "," "ACCOUNT" "SIMACC" "PROMUN" "CONSUMPTION".
FOR EACH munrmf WHERE company = 0 AND munrmf.tarif <> 0 AND ew = "w" NO-LOCK:
    FIND FIRST dbmmf WHERE dbmmf.dbacc = munrmf.acc NO-ERROR.
    IF AVAILABLE dbmmf THEN DO:
        DO X = 1 TO 13:
             EXPORT DELIMITER "," dbmmf.dbacc dbmmf.READ[X] munrmf.READ[X] dbmmf.consum[X].
         END.
    END.
    
   
END.
