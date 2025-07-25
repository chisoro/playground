DEF VAR X AS INT.
DEF VAR wsp LIKE gltdf.period.
DEF VAR Y AS INT.
DEF VAR wsa AS DEC EXTENT 12.
DO Y = 2019 TO 2019:
   FOR EACH gltdf WHERE INT(substr(string(gltdf.period),1,4)) = Y:
        DISPLAY period gltdf.acc. PAUSE 0.
        FIND FIRST glbal WHERE GLBAL.acct = gltdf.acc 
            AND GLBAL.YEAR = int(SUBSTR(STRING(gltdf.period),1,4)) NO-ERRor.
        IF NOT AVAILABLE glbal THEN DO:
            CREATE glbal.
            ASSIGN glbal.YEAR = int(
            SUBSTR(STRING(gltdf.period),1,4))
                   glbal.acc = gltdf.acc.
        END. 
        ASSIGN X = INT(SUBSTR(STRING(gltdf.period),5,2)).
               glbal.amt[X] = glbal.amt[X] + gltdf.amt.
        /*department */
        FIND FIRST glDbal WHERE GLDBAL.acct = gltdf.acc AND GLDBAL.dept = gltdf.dept
            AND GLDBAL.YEAR = int(SUBSTR(STRING(gltdf.period),1,4)) NO-ERROR.
        IF NOT AVAILABLE glDbal THEN DO:
            CREATE glDbal.
            ASSIGN glDbal.YEAR = int(
                   SUBSTR(STRING(gltdf.period),1,4))
                   glDbal.acc = gltdf.acc
                   glDbal.dept = gltdf.dept.
        END. 
        ASSIGN X = INT(SUBSTR(STRING(gltdf.period),5,2)).
               glDbal.amt[X] = glDbal.amt[X] + gltdf.amt.
         /*Project */
        FIND FIRST glpbal WHERE GLpBAL.acct = gltdf.acc AND GLpBAL.proj = gltdf.proj
            AND GLpBAL.YEAR = int(SUBSTR(STRING(gltdf.period),1,4)) NO-ERROR.
        IF NOT AVAILABLE glpbal THEN DO:
            CREATE glPbal.
            ASSIGN glPbal.YEAR = int(
                   SUBSTR(STRING(gltdf.period),1,4))
                   glPbal.acc = gltdf.acc
                   glPbal.proj = gltdf.proj.
        END.
        ASSIGN X = INT(SUBSTR(STRING(gltdf.period),5,2)).
               glPbal.amt[X] = glPbal.amt[X] + gltdf.amt.
    END.
END.
