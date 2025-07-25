DEF VAR wsa LIKE glmf.acct.
DEF VAR wsd LIKE glmf.dept.
DEF VAR wsdes LIKE glmf.descrip.
DEF VAR wsamt AS DEC.
DEF VAR Y AS INT INITIAL 0.
FIND FIRST GLBCTR WHERE GLBCTR.intBatch = 70 NO-ERROR.
FIND LAST glb NO-ERROR.
IF AVAILABLE glb THEN
   Y = glb.Recno.
ELSE Y = 0.
INPUT FROM g:\simacc\work\water2018.csv.
REPEAT :
    IMPORT DELIMITER ',' wsa wsd wsdes wsamt.
    DISPLAY wsa. PAUSE 0.
    /*FIND FIRST glmf WHERE glmf.acct = wsa NO-ERROR.
    IF AVAILABLE glmf THEN
        proj = wsp.
    ELSE 
        MESSAGE wsa " not available" VIEW-AS ALERT-BOX. */
    CREATE glb.
    Y = Y + 1.
    ASSIGN glb.acct = wsa
        glb.AMT    = wsamt WHEN wsamt <> 0
        glb.AMT    = wsamt WHEN wsamt <> 0
        glb.Dept   = wsd
        glb.DESCRIPTION = wsdes
        glb.intBatch = 70
        glb.Proj = 0
        glb.Recno = Y
        glb.REF   = "Take-on"
        glb.trDATE = 12/31/18
        glb.UID = "1"
        glbctr.amt[1] = glbctr.amt[1] + wsamt WHEN wsamt > 0
        glbctr.amt[2] = glbctr.amt[2] + wsamt WHEN wsamt < 0.
END.
