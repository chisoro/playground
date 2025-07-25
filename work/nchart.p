DEF VAR wsclass LIKE glclass.CLAS.
DEF VAR wscat LIKE glcat.cat.
DEF VAR wssubcat LIKE glsubcat.subcat.
DEF VAR wssubcat1 AS INT.
DEF VAR wsdes LIKE glcat.descrip.
DEF VAR wsl AS INT.
DEF VAR wsa LIKE glmf.acct.
INPUT FROM g:\simacc\work\nchart.csv.
REPEAT:
    IMPORT DELIMITER ',' wsdes wsa wsclass wscat wssubcat  wssubcat1 wsl.
    DISPLAY wsdes wsa  wsl.
    PAUSE 0.
    IF wsl = 1 THEN DO:
        CREATE glclass.
        ASSIGN glclass.CLAS = wsclass
               glclass.description = wsdes.
    END.
    ELSE IF wsl = 2 THEN DO:
        CREATE glcat.
        ASSIGN glcat.cat = wscat
               glcat.descrip = wsdes.
    END.
    ELSE IF wsl = 3 THEN DO:
        CREATE glsubcat.
        ASSIGN glsubcat.subcat = wssubcat
               glsubcat.descrip = wsdes.
    END.
   /* ELSE IF wsl = 4 THEN DO:
        CREATE glsubcat1.
        ASSIGN glsubcat1.subcat1 = wssubcat1
               glsubcat1.descrip = wsdes.
    END. */
    ELSE IF wsl = 10 THEN DO:
        CREATE glmf.
        ASSIGN glmf.CLAS   = wsclass
               glmf.cat     = wscat
               glmf.subcat  = wssubcat
               /*glmf.subcat1 = wssubcat1 */
               glmf.acct    = wsa
               glmf.DESCRIPTION = wsdes
               glmf.drcr = 1 WHEN wsclass = 1
               glmf.drcr = 2 WHEN wsclass <> 1.
    END.
END.
