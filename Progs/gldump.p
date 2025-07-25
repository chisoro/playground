OUTPUT TO c:\simacc\test.csv.
DEF VAR wst AS DEC.
DEF VAR wsamt AS DEC EXTENT 2.
DEF VAR X AS INT.
FOR EACH glbal WHERE YEAR = 2022 NO-LOCK:
    ASSIGN wst = 0
           wsamt = 0.
    DO X = 1 TO 12:
        wst = wst + amt[X].
    END.
    wst = wst + bfbal.
    IF wst >= 0 THEN
        ASSIGN wsAmt[1] = wst
               wsAmt[2] = 0.
    ELSE IF wst < 0 THEN
        ASSIGN wsAmt[2] = wst
               wsAmt[1] = 0.
    FIND FIRST glmf WHERE glmf.acct = glbal.acct NO-LOCK NO-ERROR.
    EXPORT DELIMITER ','  glbal.acct glmf.DESCRIPTION "" "" "" wsamt.
    FOR EACH gltdf WHERE gltdf.acct = glbal.acct 
        AND SUBSTR(STRING(gltdf.per),1,4) = "2022" NO-LOCK:
        EXPORT DELIMITER ',' gltdf.acct gltdf.descrip gltdf.trdate gltdf.ref gltdf.amt.
    END.
END.
