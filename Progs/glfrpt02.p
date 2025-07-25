/* Program.................glfrpt02.p
   Notes:...... Statement of Financial Postion Report
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
DEF BUFFER bfrglmf FOR glmf.

DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "PORTRAIT" /*"landscape" */   NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10          NO-UNDO.
DEF VAR wsStatus LIKE gltdf.acct.
DEF VAR wsGrand  AS DEC     EXTENT 2 FORM "zzzzzzzz9.99-".
DEF VAR wsAct    AS DEC     EXTENT 2 FORM "zzzzzzzz9.99-". /*Ledger total" */
DEF VAR wsclasAct LIKE wsAct EXTENT 2. /* clasegory TOTAL */
DEF VAR prAct    AS DEC     EXTENT 2 FORM "zzzzzzzz9.99-". /*Ledger total previous year" */
DEF VAR prclasAct LIKE wsAct EXTENT 2. /* clasegory TOTAL previous year */
DEF VAR wsBud    LIKE wsAct EXTENT 2.
DEF VAR wsclasBud LIKE wsAct EXTENT 2.
DEF VAR wsTitle  AS CHAR FORM "X(80)".
DEF VAR wsTitle0  AS CHAR FORM "X(80)".
DEF VAR  wsc     LIKE glmf.cat.
DEF VAR wsYear AS INT FORM "9999".
DEF VAR wsMonth AS INT FORM "99".
DEF VAR wsper LIKE SIMCTR.CURPER.
DEF VAR wssurp  LIKE glsubCat.SubCat.
DEF VAR wsEquity LIKE glmf.clas INITIAL 5.
DEF VAR st-proj LIKE glmf.proj.
DEF VAR end-proj LIKE glmf.proj.
DEF VAR st-fund LIKE glmf.fund.
DEF VAR end-fund LIKE glmf.fund.
DEF VAR wsDescrip AS CHAR FORM "X(80)".
DEF VAR wsOpt AS INT VIEW-AS RADIO-SET RADIO-BUTTONS 
    "Consolidated ", 1, "By Project", 2, "By Fund", 3.
DEF VAR X AS INT.
DEF VAR prnCat AS LOGICAL.
DEF VAR wsProg AS CHAR.
DEF VAR wsNotes AS LOGICAL INITIAL YES.
DEF VAR wsTitle1  AS CHAR FORM "X(70)" INITIAL "STATEMENT OF FINANCIAL PERFORMANCE FOR THE PERIOD ENDED:".

DEF BUTTON btnclose  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "Process".
DEF BUTTON btnSubCat  LABEL "Surplus/Deficit SubCat".
DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 16.5.

DEF    QUERY qry-Picksubcat FOR glsubcat SCROLLING.
DEF BROWSE brw-Picksubcat QUERY qry-Picksubcat
    DISPLAY GLsubCAT.subCAT GLsubCAT.DESCRIP COLUMN-LABEL "Decsription" 
    WIDTH 60 WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-Picksubcat 
    brw-Picksubcat AT ROW 2 COL 5
    skip(0.5)
    btn-ok colon 5 LABEL "OK"
    btnclose colon 60 
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED TITLE "Sub-Category Selection".

DEFINE FRAME frm-main
    SKIP(2)
    wsper     COLON 30 LABEL "Reporting Accounting Period" SKIP(.5)
    btnSubCat COLON 8 no-tab-stop wssurp NO-LABEL SKIP(0.5)
    wsOpt    LABEL  "Selection Report Option" COLON 30 SKIP(1)
    st-proj   COLON 30 LABEL  "FROM Project" SKIP(0.5)
    end-proj  COLON 30 LABEL  "TO Project" SKIP(0.5)
    st-fund   COLON 30 LABEL  "FROM Fund" SKIP(0.5)
    end-fund   COLON 30 LABEL  "TO Fund" SKIP(0.5)
    wsNotes   COLON 30 LABEL  "Print Report with Notes: Y/N?" 
    wsStatus  LABEL "        Processing......" view-as text no-tab-stop
    skip(2.2)
    btn-ok colon 15
    btnclose colon 70
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 18 COL 3
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "STATEMENT OF FINANCIAL PERFORMANCE  REPORT".

FORM glmf.DESCRIPTION NO-LABEL AT 10
     wsAct[1] 
     prAct[1]
    HEADER SKIP(1) wsTitle0 AT 20 skip(1) wsDescrip AT 5 SKIP
    wsTitle1 AT 5  "Page: " AT 80 TRIM(STRING(PAGE-NUMBER(a))) SKIP(1)
        wsYear AT 66 string(wsYear - 1) AT 79 SKIP
        "$" AT 67 "$" AT 80 SKIP
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX WIDTH 95 FRAME frmRpt.

ON CHOOSE OF btnSubCat IN FRAME frm-main
DO:
  VIEW FRAME frm-picksubCat.
  OPEN QUERY qry-picksubCat FOR EACH glSubCat NO-LOCK.
  ENABLE ALL WITH FRAME frm-picksubCat.
  WAIT-FOR CHOOSE OF btnclose IN FRAME frm-picksubCat 
          OR close of THIS-PROCEDURE IN FRAME frm-picksubCat
          OR CHOOSE OF btn-ok IN FRAME frm-picksubCat 
          OR 'enter':u OF brw-picksubCat
          OR 'mouse-select-dblclick' OF brw-picksubCat.
  CLOSE QUERY qry-picksubCat.
  HIDE FRAME frm-picksubCat.
  APPLY 'tab' TO SELF.
  APPLY 'tab' TO wsSurp IN FRAME frm-main.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-picksubCat 
    OR 'enter':u OF brw-picksubCat
    OR 'mouse-select-dblclick' OF brw-picksubCat
DO: 
   GET CURRENT qry-picksubCat EXCLUSIVE-LOCK NO-WAIT.
   DISPLAY glsubCat.SubCat @ wsSurp WITH FRAME frm-main.
   ASSIGN wsSurp.
   APPLY 'tab' TO btnSubCat IN FRAME frm-main.
   RETURN.
END.

ON 'enter':U OF  wsSurp IN FRAME frm-main 
    OR 'tab':U OF  wsSurp IN FRAME frm-main 
DO:
    FIND FIRST glsubCat WHERE glsubCat.SubCat = int( wsSurp:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE glsubCat THEN DO:
      APPLY 'tab' TO SELF.
      ASSIGN wsSurp.
    END.
    ELSE IF NOT AVAILABLE glsubCat THEN DO:
        MESSAGE  wsSurp " Invalid sub-Category, please try again" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
    END.
    RETURN.
END.

ON 'enter':U OF wsOpt IN FRAME frm-main 
   OR 'tab':U OF wsOpt IN FRAME frm-main
    OR 'mouse-select-click' OF FRAME frm-main
DO:
    ASSIGN wsopt.
    APPLY 'tab' TO SELF.
    RETURN.
END.

ON CHOOSE OF btn-oK IN FRAME frm-main DO:
     session:set-wait-state("").
     IF  INT(wsper:SCREEN-VALUE) > SIMCTR.CURPER THEN DO:
         MESSAGE "ENTERED period cannot be greater than CURRENT period" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
     END.
     ASSIGN wsper = INT(wsper:SCREEN-VALUE)
            wsYear = INT(SUBSTR(wsper:SCREEN-VALUE,1,4))
            wsMonth = INT(SUBSTR(wsper:SCREEN-VALUE,5,2))
            st-proj = INT(st-proj:SCREEN-VALUE)
            end-proj = INT(end-proj:SCREEN-VALUE)
            st-fund = INT(st-fund:SCREEN-VALUE)
            end-fund = INT(end-fund:SCREEN-VALUE)
            wsNotes
            wsOpt.
     IF wsOpt = 1 THEN DO:
        DISABLE st-proj end-proj st-fund end-fund WITH FRAME frm-main.
        wsProg = "Report.ip".
    END.
    IF wsOpt = 2 THEN DO:
        wsNotes:SCREEN-VALUE = "No".
        ENABLE st-proj end-proj WITH FRAME frm-main.
         DISABLE st-fund end-fund wsNotes WITH FRAME frm-main.
         wsProg = "Report1.ip".
    END.
    IF wsOpt = 3 THEN DO:
        wsNotes:SCREEN-VALUE = "No".
        DISABLE st-proj end-proj wsNotes WITH FRAME frm-main.
        ENABLE st-fund end-fund  WITH FRAME frm-main.
         wsProg = "Report2.ip".
    END.
    {PrintOpt.i &stream-name="stream a"
                    &print-prog=VALUE(wsProg)
                    &paged}
END.

/********** MAIN LOGIC **********/
ENABLE ALL WITH FRAME frm-main.
FIND FIRST simctr NO-LOCK NO-ERROR.
ASSIGN wsper:SCREEN-VALUE = STRING(SIMCTR.CURPER)
       end-proj:SCREEN-VALUE = "99"
       st-proj:SCREEN-VALUE = "01"
       end-fund:SCREEN-VALUE = "99"
       st-fund :SCREEN-VALUE = "01"
       wsNotes:SCREEN-VALUE = "YES"
       wsTitle0 = simctr.CONAME.    
WAIT-FOR CHOOSE OF btnclose OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.

PROCEDURE Report.ip: /* Consolidated Report */
    wsTitle1 = "STATEMENT OF FINANCIAL POSITION FOR THE PERIOD ENDED: " + STRING(wsPer).
    IF st-proj = end-proj THEN DO:
        FIND glproj WHERE glproj.proj = st-proj NO-LOCK NO-ERROR.
        wsDescrip = GLproj.DESCRIP.
    END.
    ELSE IF end-proj = 99 THEN
        wsDescrip = "CONSOLIDATED".
    ELSE 
        wsDescrip = "PROJECT: " + STRING(st-proj) + " TO " + STRING(end-proj).
    FOR EACH glmf WHERE glmf.acctype = "BS" BREAK BY clas BY cat BY subcat:
        DISPLAY glmf.acct @ wsStatus WITH FRAME frm-main.
        IF FIRST-OF(glmf.clas) AND glmf.clas = wsEquity THEN DO:
                UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
                DISPLAY STREAM a "NET ASSETS" @ glmf.DESCRIPTION
                                  wsGrand[1] @ wsAct[1]
                                  wsGrand[2] @ prAct[1] WITH FRAME frmRpt.
                UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
                DOWN 1 STREAM a WITH FRAME frmRpt.
                ASSIGN wsGrand[1] = 0
                       wsGrand[2] = 0.
        END.
        FIND FIRST glbal WHERE glbal.acct = glmf.acct 
                           AND glbal.YEAR = wsYear NO-LOCK NO-ERROR. /* Current year */
        IF AVAILABLE glbal THEN DO:
           ASSIGN wsAct[1]     = wsAct[1] + glbal.bfbal
                  wsclasAct[1] = wsclasAct[1] + glbal.bfbal
                  wsAct[2]     = wsAct[2] + glbal.bfbal
                  wsclasAct[2] = wsclasAct[2] + glbal.bfbal
                  wsGrand[1]   = wsGrand[1] + glbal.bfbal.
          DO X = 1 TO wsMonth:
                ASSIGN wsAct[1]     = wsAct[1] + glbal.amt[X]
                       wsclasAct[1]  = wsclasAct[1] + glbal.amt[X] 
                       wsAct[2]     = wsAct[2] + glbal.amt[X]
                       wsclasAct[2]  = wsclasAct[2] + glbal.amt[X]
                       wsGrand[1]   = wsGrand[1] + glbal.amt[X].
            END.
        END.
        FIND FIRST glbal WHERE glbal.acct = glmf.acct 
                           AND glbal.YEAR = (wsYear - 1) NO-LOCK NO-ERROR. /*previous year */
        IF AVAILABLE glbal THEN DO:
           ASSIGN prAct[1]     = prAct[1] + glbal.bfbal
                  prclasAct[1] = prclasAct[1] + glbal.bfbal
                  prAct[2]     = prAct[2] + glbal.bfbal
                  prclasAct[2] = prclasAct[2] + glbal.bfbal
                  wsGrand[2]   = wsGrand[2] + glbal.bfbal.
          DO X = 1 TO wsMonth:
                ASSIGN prAct[1]     = prAct[1] + glbal.amt[X]
                       prclasAct[1]  = prclasAct[1] + glbal.amt[X] 
                       prAct[2]     = prAct[2] + glbal.amt[X]
                       prclasAct[2]  = prclasAct[2] + glbal.amt[X]
                       wsGrand[2]   = wsGrand[2] + glbal.amt[X].
            END.
        END.  
        IF FIRST-OF(glmf.clas) AND glmf.clas > 2 THEN DO:
                FIND FIRST glclass WHERE glclas.clas = glmf.CLAS NO-LOCK NO-ERROR.
                DISPLAY STREAM a glclas.DESCRIP @ glmf.DESCRIPTION WITH FRAME frmRpt. 
                DOWN STREAM a WITH FRAME frmRpt.
        END. 
        IF  wsAct[2] <> 0 OR  prAct[2] <> 0 THEN DO:
            IF prncat = NO THEN DO:
                FIND FIRST glcat WHERE glcat.cat = glmf.cat NO-ERROR.
                DISPLAY STREAM a "   " + glcat.descrip @ glmf.DESCRIPTION WITH FRAME frmRpt.
                DOWN STREAM a WITH FRAME frmRpt.
                prncat = YES.
            END.
        END.
        IF last-OF(glmf.subcat) THEN DO:
            FIND FIRST glsubcat WHERE glsubcat.subcat = glmf.subcat NO-ERROR.
            IF NOT AVAILABLE glsubcat THEN
                MESSAGE glmf.subcat " sub-category does not exist for " glmf.acct VIEW-AS ALERT-BOX.
            ELSE wsTitle = "     " + glsubcat.DESCRIP.
            IF glsubcat.subcat = INT(wsSurp) THEN DO: /* extract current Surplus/deficit */
               FOR EACH bfrglmf WHERE bfrglmf.acctype = "I" OR bfrglmf.acctype = "E":
                   DISPLAY bfrglmf.acct @ wsStatus WITH FRAME frm-main.
                   FOR EACH glbal WHERE glbal.acct = bfrglmf.acct AND glbal.YEAR = wsYear NO-LOCK:
                       DO X = 1 TO wsMonth:
                        ASSIGN wsAct[1]     = wsAct[1] + glbal.amt[X]
                               wsclasAct[1]  = wsclasAct[1] + glbal.amt[X] 
                               wsAct[2]     = wsAct[2] + glbal.amt[X]
                               wsclasAct[2]  = wsclasAct[2] + glbal.amt[X]
                               wsGrand[1]   = wsGrand[1] + glbal.amt[X].
                       END.
                   END.
                   FOR EACH glbal WHERE glbal.acct = bfrglmf.acct AND glbal.YEAR = (wsYear - 1) NO-LOCK:
                       DO X = 1 TO wsMonth:
                        ASSIGN prAct[1]     = prAct[1] + glbal.amt[X]
                               prclasAct[1]  = prclasAct[1] + glbal.amt[X] 
                               prAct[2]     = prAct[2] + glbal.amt[X]
                               prclasAct[2]  = prclasAct[2] + glbal.amt[X]
                               wsGrand[2]   = wsGrand[2] + glbal.amt[X].
                       END.
                   END.
               END.
            END.
            IF  wsAct[1] <> 0 OR  prAct[1] <> 0 THEN DO: 
                 DISPLAY STREAM a wsTitle @ glmf.DESCRIPTION  wsAct[1] prAct[1] WITH FRAME frmRpt.
                 DOWN STREAM a WITH FRAME frmRpt.
            END.
            ASSIGN wsAct[1] = 0
                   prAct[1] = 0.
        END.  
        IF last-OF(glmf.cat) AND glmf.clas <> 5 THEN DO:
            FIND FIRST glcat WHERE glcat.cat = glmf.cat NO-ERROR.
            IF NOT AVAILABLE glcat THEN
                MESSAGE glmf.cat " category does not exist" VIEW-AS ALERT-BOX.
            ELSE wsTitle = "Total " + glcat.DESCRIP.
            IF  wsAct[2] <> 0 OR  prAct[2] <> 0 THEN DO:
                 UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
                 DISPLAY STREAM a wsTitle @ glmf.DESCRIPTION  wsAct[2] @ wsAct[1] 
                     prAct[2] @ prAct[1] WITH FRAME frmRpt.
                 DOWN 2 STREAM a WITH FRAME frmRpt.
                 ASSIGN wsAct[2] = 0
                        prAct[2] = 0
                        prncat = NO.
            END.
        END.  
        IF last-OF(glmf.clas) AND glmf.clas <> wsEquity AND 
                     (wsClasAct[1] <> 0 OR  prClasAct[1] <> 0) THEN DO:
            wsTitle = "TOTAL " + glclass.DESCRIP.
            UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
            DISPLAY STREAM a wsTitle @ glmf.DESCRIPTION wsClasAct[1] @ wsAct[1]
                              prClasAct[1] @ prAct[1] WITH FRAME frmRpt.
            DOWN 2 STREAM a WITH FRAME frmRpt.
            ASSIGN wsClasAct[1] = 0
                   prClasAct[1] = 0.
        END. 
        IF LAST-OF(glmf.clas) AND glmf.clas = wsEquity THEN DO:
                UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
                DISPLAY STREAM a "NET ASSETS/EQUITY" @ glmf.DESCRIPTION wsGrand[1] @ wsAct[1]
                                  wsGrand[2] @ prAct[1] WITH FRAME frmRpt.
                UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
                DOWN STREAM a WITH FRAME frmRpt.
        END.
    END.
    IF wsNotes = YES THEN DO:
        PAGE STREAM a.
        RUN Notes.ip.
    END.
    APPLY 'CLOSE' TO THIS-PROCEDURE.
END.


PROCEDURE Report1.ip:
    wsTitle1 = "STATEMENT OF FINANCIAL POSITION FOR THE PERIOD ENDED: " + STRING(wsPer).
    IF st-proj = end-proj THEN DO:
        FIND glproj WHERE glproj.proj = st-proj NO-LOCK NO-ERROR.
        wsDescrip = GLproj.DESCRIP.
    END.
    ELSE IF end-proj = 99 THEN
        wsDescrip = "CONSOLIDATED".
    ELSE 
        wsDescrip = "PROJECT: " + STRING(st-proj) + " TO " + STRING(end-proj).
    FOR EACH glProj WHERE glproj.proj >= st-proj AND glproj.proj <= end-proj:
        IF NOT CAN-FIND (FIRST glPbal WHERE glPBal.Proj = glProj.Proj
                         AND  glPBal.YEAR = wsYear) THEN NEXT.
        ELSE
           DISPLAY STREAM a glProj.DESCRIPTION @ glmf.DESCRIPTION WITH FRAME frmRpt.
           DOWN 2 STREAM a WITH FRAME frmRpt.
            ASSIGN prclasAct = 0
                   wsGrand   = 0
                   wsclasAct = 0
                   wsGrand   = 0
                   wsAct     = 0.
        FOR EACH glmf WHERE glmf.acctype = "BS" BREAK BY clas BY cat BY subcat:
            DISPLAY glmf.acct @ wsStatus WITH FRAME frm-main.
            IF FIRST-OF(glmf.clas) AND glmf.clas = wsEquity THEN DO:
                    UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
                    DISPLAY STREAM a "NET ASSETS" @ glmf.DESCRIPTION wsGrand[1] @ wsAct[1]
                                      wsGrand[2] @ prAct[1] WITH FRAME frmRpt.
                    UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
                    DOWN 1 STREAM a WITH FRAME frmRpt.
                    ASSIGN wsGrand[1] = 0
                           wsGrand[2] = 0.
            END.
            FIND FIRST glPBal WHERE glPBal.acct = glmf.acct AND glPBal.proj = glproj.proj 
                               AND glPBal.YEAR = wsYear NO-LOCK NO-ERROR. /* Current year */
            IF AVAILABLE glPBal THEN DO:
               ASSIGN wsAct[1]     = wsAct[1] + glPBal.bfbal
                      wsclasAct[1] = wsclasAct[1] + glPBal.bfbal
                      wsAct[2]     = wsAct[2] + glPBal.bfbal
                      wsclasAct[2] = wsclasAct[2] + glPBal.bfbal
                      wsGrand[1]   = wsGrand[1] + glPBal.bfbal.
              DO X = 1 TO wsMonth:
                    ASSIGN wsAct[1]     = wsAct[1] + glPBal.amt[X]
                           wsclasAct[1]  = wsclasAct[1] + glPBal.amt[X] 
                           wsAct[2]     = wsAct[2] + glPBal.amt[X]
                           wsclasAct[2]  = wsclasAct[2] + glPBal.amt[X]
                           wsGrand[1]   = wsGrand[1] + glPBal.amt[X].
                END.
            END.
            FIND FIRST glPBal WHERE glPBal.acct = glmf.acct AND glPBal.proj = glproj.proj
                               AND glPBal.YEAR = (wsYear - 1) NO-LOCK NO-ERROR. /*previous year */
            IF AVAILABLE glPBal THEN DO:
               ASSIGN prAct[1]     = prAct[1] + glPBal.bfbal
                      prclasAct[1] = prclasAct[1] + glPBal.bfbal
                      prAct[2]     = prAct[2] + glPBal.bfbal
                      prclasAct[2] = prclasAct[2] + glPBal.bfbal
                      wsGrand[2]   = wsGrand[2] + glPBal.bfbal.
              DO X = 1 TO wsMonth:
                    ASSIGN prAct[1]     = prAct[1] + glPBal.amt[X]
                           prclasAct[1]  = prclasAct[1] + glPBal.amt[X] 
                           prAct[2]     = prAct[2] + glPBal.amt[X]
                           prclasAct[2]  = prclasAct[2] + glPBal.amt[X]
                           wsGrand[2]   = wsGrand[2] + glPBal.amt[X].
                END.
            END.  
            IF FIRST-OF(glmf.clas) AND glmf.clas > 2 THEN DO:
                    FIND FIRST glclass WHERE glclas.clas = glmf.CLAS NO-LOCK NO-ERROR.
                    DISPLAY STREAM a glclas.DESCRIP @ glmf.DESCRIPTION WITH FRAME frmRpt. 
                    DOWN STREAM a WITH FRAME frmRpt.
            END. 
            IF  wsAct[2] <> 0 OR  prAct[2] <> 0 THEN DO:
                IF prncat = NO THEN DO:
                    FIND FIRST glcat WHERE glcat.cat = glmf.cat NO-ERROR.
                    DISPLAY STREAM a "   " + glcat.descrip @ glmf.DESCRIPTION WITH FRAME frmRpt.
                    DOWN STREAM a WITH FRAME frmRpt.
                    prncat = YES.
                END.
            END.
            IF last-OF(glmf.subcat) THEN DO:
                FIND FIRST glsubcat WHERE glsubcat.subcat = glmf.subcat NO-ERROR.
                IF NOT AVAILABLE glsubcat THEN
                    MESSAGE glmf.subcat " sub-category does not exist" VIEW-AS ALERT-BOX.
                ELSE wsTitle = "     " + glsubcat.DESCRIP.
                IF glsubcat.subcat = INT(wsSurp) THEN DO: /* extract current Surplus/deficit */
                   FOR EACH bfrglmf WHERE bfrglmf.acctype = "I" OR bfrglmf.acctype = "E":
                       DISPLAY bfrglmf.acct @ wsStatus WITH FRAME frm-main.
                       FOR EACH glPbal WHERE glPbal.acct = bfrglmf.acct AND glPbal.YEAR = wsYear NO-LOCK:
                           DO X = 1 TO wsMonth:
                            ASSIGN wsAct[1]     = wsAct[1] + glPbal.amt[X]
                                   wsclasAct[1]  = wsclasAct[1] + glPbal.amt[X] 
                                   wsAct[2]     = wsAct[2] + glPbal.amt[X]
                                   wsclasAct[2]  = wsclasAct[2] + glPbal.amt[X]
                                   wsGrand[1]   = wsGrand[1] + glPbal.amt[X].
                           END.
                       END.
                       FOR EACH glPbal WHERE glPbal.acct = bfrglmf.acct AND glPbal.YEAR = (wsYear - 1) NO-LOCK:
                           DO X = 1 TO wsMonth:
                            ASSIGN prAct[1]     = prAct[1] + glPbal.amt[X]
                                   prclasAct[1]  = prclasAct[1] + glPbal.amt[X] 
                                   prAct[2]     = prAct[2] + glPbal.amt[X]
                                   prclasAct[2]  = prclasAct[2] + glPbal.amt[X]
                                   wsGrand[2]   = wsGrand[2] + glPbal.amt[X].
                           END.
                       END.
                   END.
                END.
                IF  wsAct[1] <> 0 OR  prAct[1] <> 0 THEN DO: 
                     DISPLAY STREAM a wsTitle @ glmf.DESCRIPTION  wsAct[1] prAct[1] WITH FRAME frmRpt.
                     DOWN STREAM a WITH FRAME frmRpt.
                END.
                ASSIGN wsAct[1] = 0
                       prAct[1] = 0.
            END.  
            IF last-OF(glmf.cat) THEN DO:
                FIND FIRST glcat WHERE glcat.cat = glmf.cat NO-ERROR.
                IF NOT AVAILABLE glcat THEN
                    MESSAGE glmf.cat " category does not exist" VIEW-AS ALERT-BOX.
                ELSE wsTitle = "Total " + glcat.DESCRIP.
                IF  wsAct[2] <> 0 OR  prAct[2] <> 0 THEN DO:
                     UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
                     DISPLAY STREAM a wsTitle @ glmf.DESCRIPTION  wsAct[2] @ wsAct[1] 
                         prAct[2] @ prAct[1] WITH FRAME frmRpt.
                     DOWN STREAM a WITH FRAME frmRpt.
                     ASSIGN wsAct[2] = 0
                            prAct[2] = 0
                            prncat = NO.
                END.
            END.  
            IF last-OF(glmf.clas) AND glmf.clas <> wsEquity AND 
                         (wsClasAct[1] <> 0 OR  prClasAct[1] <> 0) THEN DO:
                wsTitle = "TOTAL " + glclass.DESCRIP.
                UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
                DISPLAY STREAM a wsTitle @ glmf.DESCRIPTION wsClasAct[1] @ wsAct[1]
                                  prClasAct[1] @ prAct[1] WITH FRAME frmRpt.
                DOWN 2 STREAM a WITH FRAME frmRpt.
                ASSIGN wsClasAct[1] = 0
                       prClasAct[1] = 0.
            END. 
            IF LAST-OF(glmf.clas) AND glmf.clas = wsEquity THEN DO:
                    UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
                    DISPLAY STREAM a "NET ASSETS/EQUITY" @ glmf.DESCRIPTION wsGrand[1] @ wsAct[1]
                                      wsGrand[2] @ prAct[1] WITH FRAME frmRpt.
                    UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
                    PAGE STREAM a.
            END.
        END.
    END. /*each glProj */
    APPLY 'CLOSE' TO THIS-PROCEDURE.
END.


PROCEDURE Report2.ip:
    wsTitle1 = "STATEMENT OF FINANCIAL PERFORMANCE FOR THE PERIOD ENDED: " + STRING(wsPer).
    wsDescrip = "Fund: " + STRING(st-fund) + " TO " + STRING(end-fund).
FOR EACH glFund WHERE glFund.fund >= st-fund AND glFund.fund <= end-fund:
    IF NOT CAN-FIND (FIRST glFBal WHERE glFBal.fund = glFund.fund
                     AND  glFBal.YEAR = wsYear) THEN NEXT.
    ELSE
       DISPLAY STREAM a glFund.DESCRIP @ glmf.DESCRIPTION WITH FRAME frmRpt.
    ASSIGN prclasAct[2] = 0
           wsGrand[2]   = 0
           wsclasAct[2] = 0
           wsGrand[1]   = 0
           wsAct        = 0.
    DOWN 2 STREAM a WITH FRAME frmRpt.
    
    FOR EACH glmf WHERE glmf.acctype = "BS" BREAK BY clas BY cat BY subcat:
                DISPLAY glmf.acct @ wsStatus WITH FRAME frm-main.
                IF FIRST-OF(glmf.clas) AND glmf.clas = wsEquity THEN DO:
                        UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
                        DISPLAY STREAM a "NET ASSETS" @ glmf.DESCRIPTION wsGrand[1] @ wsAct[1]
                                          wsGrand[2] @ prAct[1] WITH FRAME frmRpt.
                        UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
                        DOWN 1 STREAM a WITH FRAME frmRpt.
                        ASSIGN wsGrand[1] = 0
                               wsGrand[2] = 0.
                END.
                FIND FIRST glFBal WHERE glFBal.acct = glmf.acct AND glFBal.fund = glfund.fund 
                                   AND glFBal.YEAR = wsYear NO-LOCK NO-ERROR. /* Current year */
                IF AVAILABLE glFBal THEN DO:
                   ASSIGN wsAct[1]     = wsAct[1] + glFBal.bfbal
                          wsclasAct[1] = wsclasAct[1] + glFBal.bfbal
                          wsAct[2]     = wsAct[2] + glFBal.bfbal
                          wsclasAct[2] = wsclasAct[2] + glFBal.bfbal
                          wsGrand[1]   = wsGrand[1] + glFBal.bfbal.
                  DO X = 1 TO wsMonth:
                        ASSIGN wsAct[1]     = wsAct[1] + glFBal.amt[X]
                               wsclasAct[1]  = wsclasAct[1] + glFBal.amt[X] 
                               wsAct[2]     = wsAct[2] + glFBal.amt[X]
                               wsclasAct[2]  = wsclasAct[2] + glFBal.amt[X]
                               wsGrand[1]   = wsGrand[1] + glFBal.amt[X].
                    END.
                END.
                FIND FIRST glFBal WHERE glFBal.acct = glmf.acct AND glFBal.fund = glfund.fund
                                   AND glFBal.YEAR = (wsYear - 1) NO-LOCK NO-ERROR. /*previous year */
                IF AVAILABLE glFBal THEN DO:
                   ASSIGN prAct[1]     = prAct[1] + glFBal.bfbal
                          prclasAct[1] = prclasAct[1] + glFBal.bfbal
                          prAct[2]     = prAct[2] + glFBal.bfbal
                          prclasAct[2] = prclasAct[2] + glFBal.bfbal
                          wsGrand[2]   = wsGrand[2] + glFBal.bfbal.
                  DO X = 1 TO wsMonth:
                        ASSIGN prAct[1]     = prAct[1] + glFBal.amt[X]
                               prclasAct[1]  = prclasAct[1] + glFBal.amt[X] 
                               prAct[2]     = prAct[2] + glFBal.amt[X]
                               prclasAct[2]  = prclasAct[2] + glFBal.amt[X]
                               wsGrand[2]   = wsGrand[2] + glFBal.amt[X].
                    END.
                END.  
                IF FIRST-OF(glmf.clas) AND glmf.clas > 2 THEN DO:
                        FIND FIRST glclass WHERE glclas.clas = glmf.CLAS NO-LOCK NO-ERROR.
                        DISPLAY STREAM a glclas.DESCRIP @ glmf.DESCRIPTION WITH FRAME frmRpt. 
                        DOWN STREAM a WITH FRAME frmRpt.
                END. 
                IF  wsAct[2] <> 0 OR  prAct[2] <> 0 THEN DO:
                    IF prncat = NO THEN DO:
                        FIND FIRST glcat WHERE glcat.cat = glmf.cat NO-ERROR.
                        DISPLAY STREAM a "   " + glcat.descrip @ glmf.DESCRIPTION WITH FRAME frmRpt.
                        DOWN STREAM a WITH FRAME frmRpt.
                        prncat = YES.
                    END.
                END.
                IF last-OF(glmf.subcat) THEN DO:
                    FIND FIRST glsubcat WHERE glsubcat.subcat = glmf.subcat NO-ERROR.
                    IF NOT AVAILABLE glsubcat THEN
                        MESSAGE glmf.subcat "sub-category does not exist" VIEW-AS ALERT-BOX.
                    ELSE wsTitle = "     " + glsubcat.DESCRIP.
                    IF glsubcat.subcat = INT(wsSurp) THEN DO: /* extract current Surplus/deficit */
                       FOR EACH bfrglmf WHERE bfrglmf.acctype = "I" OR bfrglmf.acctype = "E":
                           DISPLAY bfrglmf.acct @ wsStatus WITH FRAME frm-main.
                           FOR EACH glFbal WHERE glFbal.acct = bfrglmf.acct AND glFbal.YEAR = wsYear NO-LOCK:
                               DO X = 1 TO wsMonth:
                                ASSIGN wsAct[1]     = wsAct[1] + glFbal.amt[X]
                                       wsclasAct[1]  = wsclasAct[1] + glFbal.amt[X] 
                                       wsAct[2]     = wsAct[2] + glFbal.amt[X]
                                       wsclasAct[2]  = wsclasAct[2] + glFbal.amt[X]
                                       wsGrand[1]   = wsGrand[1] + glFbal.amt[X].
                               END.
                           END.
                           FOR EACH glFbal WHERE glFbal.acct = bfrglmf.acct AND glFbal.YEAR = (wsYear - 1) NO-LOCK:
                               DO X = 1 TO wsMonth:
                                ASSIGN prAct[1]     = prAct[1] + glFbal.amt[X]
                                       prclasAct[1]  = prclasAct[1] + glFbal.amt[X] 
                                       prAct[2]     = prAct[2] + glFbal.amt[X]
                                       prclasAct[2]  = prclasAct[2] + glFbal.amt[X]
                                       wsGrand[2]   = wsGrand[2] + glFbal.amt[X].
                               END.
                           END.
                       END.
                    END.
                    IF  wsAct[1] <> 0 OR  prAct[1] <> 0 THEN DO: 
                         DISPLAY STREAM a wsTitle @ glmf.DESCRIPTION  wsAct[1] prAct[1] WITH FRAME frmRpt.
                         DOWN STREAM a WITH FRAME frmRpt.
                    END.
                    ASSIGN wsAct[1] = 0
                           prAct[1] = 0.
                END.  
                IF last-OF(glmf.cat) THEN DO:
                    FIND FIRST glcat WHERE glcat.cat = glmf.cat NO-ERROR.
                    IF NOT AVAILABLE glcat THEN
                        MESSAGE glmf.cat glmf.clas VIEW-AS ALERT-BOX.
                    ELSE wsTitle = "Total " + glcat.DESCRIP.
                    IF  wsAct[2] <> 0 OR  prAct[2] <> 0 THEN DO:
                         UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
                         DISPLAY STREAM a wsTitle @ glmf.DESCRIPTION  wsAct[2] @ wsAct[1] 
                             prAct[2] @ prAct[1] WITH FRAME frmRpt.
                         DOWN STREAM a WITH FRAME frmRpt.
                         ASSIGN wsAct[2] = 0
                                prAct[2] = 0
                                prncat = NO.
                    END.
                END.  
                IF last-OF(glmf.clas) AND glmf.clas <> wsEquity AND 
                             (wsClasAct[1] <> 0 OR  prClasAct[1] <> 0) THEN DO:
                    wsTitle = "TOTAL " + glclass.descrip.
                    UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
                    DISPLAY STREAM a wsTitle @ glmf.DESCRIPTION wsClasAct[1] @ wsAct[1]
                                      prClasAct[1] @ prAct[1] WITH FRAME frmRpt.
                    DOWN 2 STREAM a WITH FRAME frmRpt.
                    ASSIGN wsClasAct[1] = 0
                           prClasAct[1] = 0.
                END. 
                IF LAST-OF(glmf.clas) AND glmf.clas = wsEquity THEN DO:
                        UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
                        DISPLAY STREAM a "NET ASSETS/EQUITY" @ glmf.DESCRIPTION wsGrand[1] @ wsAct[1]
                                          wsGrand[2] @ prAct[1] WITH FRAME frmRpt.
                        UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
                        PAGE STREAM a.
                END.
            END.
        END. /*each glfund */
        APPLY 'CLOSE' TO THIS-PROCEDURE.
    END.

    PROCEDURE Notes.ip:
        wsTitle1 = "NOTES TO STATEMENT OF FINANCIAL POSITION FOR THE PERIOD ENDED: " + STRING(wsPer).
        IF st-proj = end-proj THEN DO:
            FIND glproj WHERE glproj.proj = st-proj NO-LOCK NO-ERROR.
            wsDescrip = GLproj.DESCRIP.
        END.
        ELSE IF end-proj = 99 THEN
            wsDescrip = "CONSOLIDATED".
        ELSE 
            wsDescrip = "proj: " + STRING(st-proj) + " TO " + STRING(end-proj).
    ASSIGN wsclasAct[1] = 0
           prclasAct[2] = 0.
    FOR EACH glmf WHERE glmf.acctype = "BS"
                     BREAK BY glmf.clas BY glmf.cat BY glmf.subcat BY glmf.acct:
        IF glmf.subcat = wsSurp THEN NEXT.
        ASSIGN wsAct[1] = 0
               prAct[1] = 0.
        DISPLAY glmf.acct @ wsStatus WITH FRAME frm-main.
        FOR EACH glbal WHERE glbal.acct = glmf.acct 
                           AND glbal.YEAR = wsYear NO-LOCK: /* Current year */
            ASSIGN wsAct[1]     = wsAct[1] + glbal.bfbal
                  wsclasAct[1] = wsclasAct[1] + glbal.bfbal
                  wsAct[2]     = wsAct[2] + glbal.bfbal
                  wsclasAct[2] = wsclasAct[2] + glbal.bfbal
                  wsGrand[1]   = wsGrand[1] + glbal.bfbal.
            DO X = 1 TO wsMonth:
                ASSIGN wsAct[1]     = wsAct[1] + glbal.amt[X]
                       wsclasAct[1]  = wsclasAct[1] + glbal.amt[X] 
                       wsAct[2]     = wsAct[2] + glbal.amt[X]
                       wsclasAct[2]  = wsclasAct[2] + glbal.amt[X]
                       wsGrand[1]   = wsGrand[1] + glbal.amt[X].
            END.
        END.
        FOR EACH glbal WHERE glbal.acct = glmf.acct 
                           AND glbal.YEAR = (wsYear - 1) NO-LOCK: /*previous year */
            ASSIGN prAct[1]     = prAct[1] + glbal.bfbal
                  prclasAct[1] = prclasAct[1] + glbal.bfbal
                  prAct[2]     = prAct[2] + glbal.bfbal
                  prclasAct[2] = prclasAct[2] + glbal.bfbal
                  wsGrand[2]   = wsGrand[2] + glbal.bfbal.
            DO X = 1 TO wsMonth:
                ASSIGN prAct[1]     = prAct[1] + glbal.amt[X]
                       prclasAct[1]  = prclasAct[1] + glbal.amt[X] 
                       prAct[2]     = prAct[2] + glbal.amt[X]
                       prclasAct[2]  = prclasAct[2] + glbal.amt[X]
                       wsGrand[2]   = wsGrand[2] + glbal.amt[X].
            END.
        END.
        IF (wsAct[1] <> 0 OR  prAct[1] <> 0) AND prnCat = NO THEN DO:
           FIND FIRST glsubcat WHERE glsubcat.subcat = glmf.subcat NO-ERROR.
           DISPLAY STREAM a glsubcat.DESCRIP @ glmf.DESCRIPTION 
                  WITH FRAME frmRpt. 
           DOWN STREAM a WITH FRAME frmRpt.
           prncat = YES.
        END.
        IF  wsAct[1] <> 0 OR  prAct[1] <> 0 THEN DO:
            DISPLAY STREAM a "     " + glmf.DESCRIPTION @ glmf.DESCRIPTION   wsAct[1] prAct[1] WITH FRAME frmRpt.
            DOWN STREAM a WITH FRAME frmRpt.
            ASSIGN wsAct[1] = 0
                   prAct[1] = 0.
        END.
        IF LAST-OF(glmf.subcat) THEN
           IF  wsclasAct[1] <> 0 OR  prclasAct[2] <> 0 THEN DO:
                UNDERLINE STREAM a glmf.DESCRIPTION  wsAct[1] prAct[1] WITH FRAME frmRpt.
                DISPLAY STREAM a "TOTAL " @ glmf.DESCRIPTION  wsclasAct[1] @ wsAct[1] prclasAct[2] @ prAct[1] WITH FRAME frmRpt.
                DOWN 2 STREAM a WITH FRAME frmRpt.
                ASSIGN wsclasAct[1] = 0
                       prclasAct[2] = 0
                       prncat = NO.
        END.
    END. /*each glmf */
    APPLY 'CLOSE' TO THIS-PROCEDURE.
END.
