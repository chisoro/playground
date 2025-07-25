DEF VAR wsamt LIKE gltdf.amt.
DEF VAR X AS INT.
DEF VAR Y AS INT.
FIND FIRST glBctr WHERE intBatch = 65 NO-ERROR.
FOR EACH gldbal WHERE YEAR = 2018:
    FIND FIRST glmf WHERE glmf.acct = gldbal.acct NO-LOCK NO-ERROR.
    wsamt = gldbal.bfbal.
    Y = Y + 1.
    DO X = 1 TO 12:
        wsamt = wsamt + amt[X].
    END.
    DISPLAY gldbal.acct wsamt.
    PAUSE 0.
    IF wsAmt <> 0 THEN DO:
        CREATE glb.
        ASSIGN glb.acct = gldbal.acct
            glb.AMT    = wsamt * -1
            glb.Dept   = glmf.dept
            glb.DESCRIPTION = "Balance Reset"
            glb.intBatch = 65
            glb.Proj = glmf.proj
            glb.Recno = Y
            glb.REF   = "Reset"
            glb.trDATE = 12/31/18
            glb.UID = "1"
            GLBCTR.AMT[1] = GLBCTR.AMT[1] + wsAmt WHEN wsAmt > 0
            GLBCTR.AMT[2] = GLBCTR.AMT[2] + wsAmt WHEN wsAmt < 0.
    END.
    
END.

