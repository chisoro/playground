DISPLAY glmf.acct @ wsStatus WITH FRAME frm-main.
    IF FIRST-OF(glmf.cat) THEN DO:
       IF glmf.cat > 22 AND glmf.cat < 25 THEN DO:
           UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
           DISPLAY STREAM a "TOTAL ASSETS" @ glmf.DESCRIPTION wsGrand[1] @ wsAct[1] wsGrand[2] @ prAct[1]
                WITH FRAME frmRpt.
           DOWN 2 STREAM a WITH FRAME frmRpt.
           DISPLAY STREAM a "LIABILITIES" @ glmf.DESCRIPTION  WITH FRAME frmRpt.
           DOWN  STREAM a WITH FRAME frmRpt.
           ASSIGN wsGrand = 0.
       END.
       IF   glmf.cat > 24 THEN DO:
           UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
           DISPLAY STREAM a "TOTAL LIABILITIES" @ glmf.DESCRIPTION wsGrand[1] @ wsAct[1] wsGrand[2] @ prAct[1]
                WITH FRAME frmRpt.
           DOWN 2 STREAM a WITH FRAME frmRpt.
           DISPLAY STREAM a "NET ASSETS" @ glmf.DESCRIPTION wsGrandT[1] @ wsAct[1] wsGrandT[2] @ prAct[1]
                WITH FRAME frmRpt.
           DOWN 2 STREAM a WITH FRAME frmRpt.
           ASSIGN wsGrand = 0.
       END.
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
      ASSIGN wsAct[1]   = wsAct[1] + glbal.bfbal
             wsGrand[1] = wsGrand[1] + glbal.bfbal
             wsGrandt[1] = wsGrandt[1] + glbal.bfbal.
      DO X = 1 TO wsMonth:
            ASSIGN wsAct[1]     = wsAct[1] + glbal.amt[X]
                   wsCatAct[1]  = wsCatAct[1] + glbal.amt[X] 
                   wsAct[2]     = wsAct[2] + glbal.amt[X]
                   wsCatAct[2]  = wsCatAct[2] + glbal.amt[X]
                   wsGrand[1]   = wsGrand[1] + glbal.amt[X]
                   wsGrandt[1] = wsGrandt[1] +  glbal.amt[X].
        END.
    END.
    FIND FIRST glbal WHERE glbal.acct = glmf.acct 
                       AND glbal.YEAR = (wsYear - 1) NO-LOCK NO-ERROR. /*previous year */
    IF AVAILABLE glbal THEN DO:
      ASSIGN prAct[1]   = prAct[1] + glbal.bfbal
             wsGrand[2] = wsGrand[2] +  glbal.bfbal
              wsGrandt[2] = wsGrandt[2] +  glbal.bfbal.
      DO X = 1 TO wsMonth:
            ASSIGN prAct[1]     = prAct[1] + glbal.amt[X]
                   prCatAct[1]  = prCatAct[1] + glbal.amt[X] 
                   prAct[2]     = prAct[2] + glbal.amt[X]
                   prCatAct[2]  = prCatAct[2] + glbal.amt[X]
                   wsGrand[2]   = wsGrand[2] + glbal.amt[X]
                   wsGrandt[2]   = wsGrandt[2] + glbal.amt[X].
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
        DISPLAY STREAM a wsAct[2] @ wsAct[1] prAct[2] @ prAct[1] WITH FRAME frmRpt.
        DOWN STREAM a WITH FRAME frmRpt.
        /* IF glmf.cat <= 22 THEN DO:
           UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
           DISPLAY STREAM a "TOTAL ASSETS" @ glmf.DESCRIPTION wsGrand[1] @ wsAct[1] wsGrand[2] @ prAct[1]
                WITH FRAME frmRpt.
           DOWN  STREAM a WITH FRAME frmRpt.
        END.
        IF glmf.cat > 22 AND glmf.cat < 25 THEN DO:
           UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
           DISPLAY STREAM a "TOTAL LIABILITIES" @ glmf.DESCRIPTION wsGrand[1] @ wsAct[1] wsGrand[2] @ prAct[1]
                WITH FRAME frmRpt.
           DOWN  STREAM a WITH FRAME frmRpt.
        END.*/
    END.

