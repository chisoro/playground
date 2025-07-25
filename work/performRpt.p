
DEF VAR wsGrand  AS DEC     EXTENT 2 FORM "zzzzzzzz9.99-".
DEF VAR wsAct    AS DEC     EXTENT 2 FORM "zzzzzzzz9.99-". /*Ledger total" */
DEF VAR wsCatAct LIKE wsAct EXTENT 2. /* Category TOTAL */
DEF VAR prAct    AS DEC     EXTENT 2 FORM "zzzzzzzz9.99-". /*Ledger total previous year" */
DEF VAR prCatAct LIKE wsAct EXTENT 2. /* Category TOTAL previous year */
DEF VAR wsBud    LIKE wsAct EXTENT 2.
DEF VAR wsCatBud LIKE wsAct EXTENT 2.
DEF VAR wsTitle  AS CHAR FORM "X(80)".
DEF VAR  wsc     LIKE glmf.subcat.
DEF VAR wsYear AS INT FORM "9999" INITIAL 2018.
DEF VAR wsMonth AS INT FORM "99" INITIAL 9.
DEF VAR wsper LIKE SIMCTR.CURPER.
DEF VAR wssurp  LIKE glmf.acct.
DEF VAR X AS INT.
DEF STREAM a.

FIND FIRST SIMCTR NO-LOCK NO-ERROR.
    ASSIGN wssurp = SIMCTR.SURACC
           wsPer  = SIMCTR.CURPER.

OUTPUT STREAM a TO g:\simacc\sfpe.txt.

FORM glmf.DESCRIPTION NO-LABEL  AT 5
     wsAct[1] 
     prAct[1]
    HEADER skip(1) "STATEMENT OF FINANCIAL PERFORMANCE FOR THE PERIOD ENDED:" AT 5 
    STRING(wsPer) "Page: " AT 80 TRIM(STRING(PAGE-NUMBER(a))) SKIP(1)
        wsYear AT 61 string(wsYear - 1) AT 75 SKIP
        "$" AT 63 "$" AT 77 SKIP
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX WIDTH 95 FRAME frmRpt.
    

FOR EACH glmf WHERE cat <= 9 BREAK BY cat BY subcat:
    IF FIRST-OF(glmf.cat) THEN DO:
        FIND FIRST glcat WHERE glcat.cat = glmf.cat NO-ERROR.
        ASSIGN wsCatAct[1] = 0
               wsAct[2] = 0
               prCatAct[1] = 0
               prAct[2] = 0.
        DISPLAY STREAM a glcat.DESCRIP @ glmf.DESCRIPTION 
            WITH FRAME frmRpt. 
        DOWN STREAM a WITH FRAME frmRpt.
    END.
    IF FIRST-OF(glmf.subcat) THEN DO:
        ASSIGN wsAct[1] = 0
               prAct[1] = 0.
    END.
    FIND FIRST glbal WHERE glbal.acct = glmf.acct 
                       AND glbal.YEAR = wsYear NO-LOCK NO-ERROR. /* Current year */
    IF AVAILABLE glbal THEN DO:
      DO X = 1 TO wsMonth:
            ASSIGN wsAct[1]     = wsAct[1] + glbal.amt[X]
                   wsCatAct[1]  = wsCatAct[1] + glbal.amt[X] 
                   wsAct[2]     = wsAct[2] + glbal.amt[X]
                   wsCatAct[2]  = wsCatAct[2] + glbal.amt[X]
                   wsGrand[1]   = wsGrand[1] + glbal.amt[X].
        END.
    END.
    FIND FIRST glbal WHERE glbal.acct = glmf.acct 
                       AND glbal.YEAR = (wsYear - 1) NO-LOCK NO-ERROR. /*previous year */
    IF AVAILABLE glbal THEN DO:
      DO X = 1 TO wsMonth:
            ASSIGN prAct[1]     = prAct[1] + glbal.amt[X]
                   prCatAct[1]  = prCatAct[1] + glbal.amt[X] 
                   prAct[2]     = prAct[2] + glbal.amt[X]
                   prCatAct[2]  = prCatAct[2] + glbal.amt[X]
                   wsGrand[2]   = wsGrand[2] + glbal.amt[X].
        END.
    END.
    
    IF last-OF(glmf.subcat) THEN DO:
        FIND FIRST glsubcat WHERE glsubcat.subcat = glmf.subcat NO-ERROR.
        IF NOT AVAILABLE glsubcat THEN
            MESSAGE glmf.subcat glmf.cat VIEW-AS ALERT-BOX.
        ELSE wsTitle = "     " + glsubcat.DESCRIP.
        DISPLAY STREAM a wsTitle @ glmf.DESCRIPTION  wsAct[1] prAct[1] WITH FRAME frmRpt.
        DOWN STREAM a WITH FRAME frmRpt.
    END.  
    IF last-OF(glmf.cat) THEN DO:
        UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
        DISPLAY STREAM a "TOTAL" @ glmf.DESCRIPTION wsAct[2] @ wsAct[1]
                          prAct[2] @ prAct[1] WITH FRAME frmRpt.
        DOWN 2 STREAM a WITH FRAME frmRpt.
    END. 
END.
UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
DISPLAY STREAM a "SURPLU/DEFICIT" @ glmf.DESCRIPTION wsCatAct[2] @ wsAct[1]
                          prCatAct[2] @ prAct[1]  WITH FRAME frmRpt.
DOWN  STREAM a WITH FRAME frmRpt.
ASSIGN wsAct[1] = 0
       prAct[1] = 0.
FOR EACH glmf WHERE INT(SUBSTR(STRING(glmf.Acct),4,6)) = wssurp:
    FIND FIRST glbal WHERE glbal.acct = glmf.acct 
                       AND glbal.YEAR = wsYear NO-LOCK NO-ERROR. /* Current year */
    IF AVAILABLE glbal THEN 
            ASSIGN wsAct[1]     = wsAct[1] + GLBAL.BFBAL
                   wsGrand[1]   = wsGrand[1] + GLBAL.BFBAL.
    FIND FIRST glbal WHERE glbal.acct = glmf.acct 
                       AND glbal.YEAR = (wsYear - 1) NO-LOCK NO-ERROR. /*previous year */
    IF AVAILABLE glbal THEN
            ASSIGN prAct[1]     = prAct[1] +  GLBAL.BFBAL
                   wsGrand[2]   = wsGrand[2] +  GLBAL.BFBAL.
END.
DISPLAY STREAM a "SURPLUS/DEFICIT B/F" @ glmf.DESCRIPTION  wsAct[1] prAct[1]
                           WITH FRAME frmRpt.
UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
DISPLAY STREAM a "SURPLUS/DEFICIT C/D" @ glmf.DESCRIPTION wsGrand[1] @ wsAct[1] wsGrand[2] @ prAct[1] 
                           WITH FRAME frmRpt.
UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
DOWN  STREAM a WITH FRAME frmRpt.



