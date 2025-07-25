/* Program.................glfrpt01.p
   Notes:...... Statement of Financial Performance Report
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
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
DEF VAR wsTitle0  AS CHAR FORM "X(60)".
DEF VAR  wsc     LIKE glmf.cat.
DEF VAR wsYear AS INT FORM "9999" INITIAL 2018.
DEF VAR wsMonth AS INT FORM "99" INITIAL 9.
DEF VAR wsper LIKE SIMCTR.CURPER.
DEF VAR wssurp  LIKE glmf.acct.
DEF VAR st-proj LIKE glmf.proj.
DEF VAR end-proj LIKE glmf.proj.
DEF VAR st-fund LIKE glmf.fund.
DEF VAR end-fund LIKE glmf.fund.
DEF VAR st-dept LIKE gldept.dept.
DEF VAR end-dept LIKE gldept.dept.
DEF VAR wsDescrip AS CHAR FORM "X(80)".
DEF VAR wsOpt AS INT VIEW-AS RADIO-SET RADIO-BUTTONS 
    "Consolidated ", 1, "By Project", 2, "By Fund", 3, "By Department", 4.
DEF VAR X AS INT.
DEF VAR prnTitle AS LOGICAL.
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

DEF BUFFER bfglpbal FOR glpbal.
DEF BUFFER bfglFbal FOR glFbal.
DEF BUFFER bfglDbal FOR glDbal.

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
   /* btnSubCat COLON 8 no-tab-stop wssurp NO-LABEL SKIP(0.5) */
    wsOpt    LABEL  "Selection Report Option" COLON 30 SKIP(1)
    st-proj   COLON 30 LABEL  "FROM Project" 
    end-proj  COLON 60 LABEL  "TO Project" SKIP(0.5)
    st-fund   COLON 30 LABEL  "FROM Fund" 
    end-fund  COLON 60 LABEL  "TO Fund" SKIP(0.5)
    st-dept   COLON 30 LABEL  "FROM Department" 
    end-dept  COLON 60 LABEL  "TO Department" SKIP(0.5)
    wsNotes   COLON 30 LABEL  "Print Report with Notes: Y/N?" SKIP(0.5)
    wsStatus  COLON 30 LABEL "        Processing......" view-as text no-tab-stop
    skip(1.8)
    btn-ok AT ROW 18.5 COL 15
    btnclose colon 70
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 18 COL 3
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "STATEMENT OF FINANCIAL PERFORMANCE  REPORT".

FORM glmf.DESCRIPTION NO-LABEL AT 10
     wsAct[1] 
     prAct[1]
    HEADER SKIP(1) wsTitle0 AT 20 SKIP(1) wsDescrip AT 5 SKIP
    wsTitle1 AT 5  "Page: " AT 80 TRIM(STRING(PAGE-NUMBER(a))) SKIP(1)
        wsYear AT 66 string(wsYear - 1) AT 79 SKIP
        "$" AT 67 "$" AT 80 SKIP
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX WIDTH 95 FRAME frmRpt.

ON 'enter':U OF wsOpt IN FRAME frm-main 
   OR 'tab':U OF wsOpt IN FRAME frm-main
    OR 'mouse-select-click' OF FRAME frm-main
DO:
    ASSIGN wsclasAct = 0
           wsAct      = 0
           prclasAct  = 0
           prAct      = 0
           wsGrand    = 0.
    ASSIGN wsopt.
    IF wsOpt = 1 THEN DO:
        DISABLE st-proj end-proj st-fund end-fund st-dept end-dept WITH FRAME frm-main.
        wsProg = "Report.ip".
    END.
    IF wsOpt = 2 THEN DO:
        wsNotes:SCREEN-VALUE = "No".
        ENABLE st-proj end-proj WITH FRAME frm-main.
         DISABLE st-fund end-fund st-dept end-dept wsNotes WITH FRAME frm-main.
         wsProg = "Report1.ip".
    END.
    IF wsOpt = 3 THEN DO:
        wsNotes:SCREEN-VALUE = "No".
        DISABLE st-proj end-proj st-dept end-dept wsNotes WITH FRAME frm-main.
        ENABLE st-fund end-fund  WITH FRAME frm-main.
         wsProg = "Report2.ip".
    END.
    IF wsOpt = 4 THEN DO:
        wsNotes:SCREEN-VALUE = "No".
       DISABLE st-proj end-proj st-fund end-fund wsNotes WITH FRAME frm-main.
       ENABLE st-dept end-dept WITH FRAME frm-main.
        wsProg = "Report3.ip".
    END.
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
            st-dept = INT(st-dept:SCREEN-VALUE)
            end-dept = INT(end-dept:SCREEN-VALUE)
            wsNotes.
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
       st-fund:SCREEN-VALUE = "01"
       end-dept:SCREEN-VALUE = "1900"
       st-dept:SCREEN-VALUE = "1100"
       wsNotes:SCREEN-VALUE = "YES"
        wsTitle0 = simctr.CONAME.    
WAIT-FOR CHOOSE OF btnclose OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.

PROCEDURE Report.ip: /* Consolidated Report */
    wsTitle1 = "STATEMENT OF FINANCIAL PERFORMANCE FOR THE PERIOD ENDED: " + STRING(wsPer).
    IF st-proj = end-proj THEN DO:
        FIND glproj WHERE glproj.proj = st-proj NO-LOCK NO-ERROR.
        wsDescrip = GLproj.DESCRIP.
    END.
    ELSE IF end-proj = 99 THEN
        wsDescrip = "CONSOLIDATED".
    ELSE 
        wsDescrip = "PROJECT: " + STRING(st-proj) + " TO " + STRING(end-proj).
FOR EACH glmf WHERE (glmf.acctype = "I" OR glmf.acctype = "E") BREAK BY clas BY cat:
    DISPLAY glmf.acct @ wsStatus WITH FRAME frm-main.
    IF FIRST-OF(glmf.clas) THEN DO:
        prnTitle = NO.
        FIND FIRST glclas WHERE glclas.clas = glmf.clas NO-ERROR.
        ASSIGN wsclasAct[1] = 0
               wsAct[2] = 0
               prclasAct[1] = 0
               prAct[2] = 0.
    END.
    IF FIRST-OF(glmf.cat) THEN DO:
        ASSIGN wsAct[1] = 0
               prAct[1] = 0.
    END.
    FIND FIRST glbal WHERE glbal.acct = glmf.acct 
                       AND glbal.YEAR = wsYear NO-LOCK NO-ERROR. /* Current year */
    IF AVAILABLE glbal THEN DO:
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
      DO X = 1 TO wsMonth:
            ASSIGN prAct[1]     = prAct[1] + glbal.amt[X]
                   prclasAct[1]  = prclasAct[1] + glbal.amt[X] 
                   prAct[2]     = prAct[2] + glbal.amt[X]
                   prclasAct[2]  = prclasAct[2] + glbal.amt[X]
                   wsGrand[2]   = wsGrand[2] + glbal.amt[X].
        END.
    END.
    IF  wsAct[1] <> 0 OR  prAct[1] <> 0 THEN DO:
        IF prnTitle = NO THEN DO:
            DISPLAY STREAM a glclas.DESCRIP @ glmf.DESCRIPTION WITH FRAME frmRpt. 
            DOWN STREAM a WITH FRAME frmRpt.
            prnTitle = YES.
        END.
    END.
    IF last-OF(glmf.cat) THEN DO:
        FIND FIRST glcat WHERE glcat.cat = glmf.cat NO-ERROR.
        IF NOT AVAILABLE glcat THEN
            MESSAGE glmf.cat " category does not exist" VIEW-AS ALERT-BOX.
        ELSE wsTitle = "     " + glcat.DESCRIP.
        IF  wsAct[1] <> 0 OR  prAct[1] <> 0 THEN DO: 
             DISPLAY STREAM a wsTitle @ glmf.DESCRIPTION  wsAct[1] prAct[1] WITH FRAME frmRpt.
             DOWN STREAM a WITH FRAME frmRpt.
        END.
    END.  
    IF last-OF(glmf.clas) AND (wsAct[2] <> 0 OR  prAct[2] <> 0) THEN DO:
        UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
        DISPLAY STREAM a "TOTAL" @ glmf.DESCRIPTION wsAct[2] @ wsAct[1]
                          prAct[2] @ prAct[1] WITH FRAME frmRpt.
        DOWN 2 STREAM a WITH FRAME frmRpt.
    END. 
END.
UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
DISPLAY STREAM a "SURPLU/DEFICIT" @ glmf.DESCRIPTION wsclasAct[2] @ wsAct[1]
                          prclasAct[2] @ prAct[1]  WITH FRAME frmRpt.
DOWN  STREAM a WITH FRAME frmRpt.
ASSIGN wsAct[1] = 0
       prAct[1] = 0.

IF simctr.CLOSEPER >= (wsPer - 100)  THEN
       wsGrand[1] = 0.
FOR EACH glmf WHERE glmf.acct = simctr.suracc: /* surplus/deficit account */
    FIND FIRST glbal WHERE glbal.acct = glmf.acct 
                       AND glbal.YEAR = wsYear NO-LOCK NO-ERROR. /* Current year */
    IF AVAILABLE glbal THEN  DO:
        ASSIGN wsAct[1]     = wsAct[1] + GLBAL.BFBAL
               wsGrand[1]   = wsGrand[1] + GLBAL.BFBAL.
        
        DO X = 1 TO wsMonth:
           ASSIGN wsAct[1]     = wsAct[1]   + GLBAL.amt[X]
                  wsGrand[1]   = wsGrand[1] + GLBAL.amt[X].
        END.
       
    END.
    FIND FIRST glbal WHERE glbal.acct = glmf.acct 
                       AND glbal.YEAR = (wsYear - 1) NO-LOCK NO-ERROR. /*previous year */
    
    IF AVAILABLE glbal THEN
       ASSIGN prAct[1]     = prAct[1] +  GLBAL.BFBAL
              wsGrand[2]   = wsGrand[2] +  GLBAL.BFBAL.
        
END.
IF CLOSEPER >= (wsPer - 100) THEN
    DISPLAY STREAM a "SURPLUS/DEFICIT B/F" @ glmf.DESCRIPTION wsAct[1] @ wsAct[1] prAct[1]
                               WITH FRAME frmRpt.
ELSE 
   DISPLAY STREAM a "SURPLUS/DEFICIT B/F" @ glmf.DESCRIPTION wsAct[1] prAct[1]
                               WITH FRAME frmRpt.
    UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
    DISPLAY STREAM a "SURPLUS/DEFICIT C/D" @ glmf.DESCRIPTION (wsclasAct[2] + wsAct[1]) @ wsAct[1] 
        wsGrand[2] @ prAct[1] WITH FRAME frmRpt.

/*DISPLAY STREAM a "SURPLUS/DEFICIT B/F" @ glmf.DESCRIPTION  wsAct[1] prAct[1]
                           WITH FRAME frmRpt.
UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
DISPLAY STREAM a "SURPLUS/DEFICIT C/D" @ glmf.DESCRIPTION wsGrand[1] @ wsAct[1] 
    wsGrand[2] @ prAct[1] WITH FRAME frmRpt.
UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt. */
DOWN  STREAM a WITH FRAME frmRpt.
IF wsNotes = YES THEN DO:
    PAGE STREAM a.
      RUN Notes.ip.
END.
APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

PROCEDURE Report1.ip:
    wsTitle1 = "STATEMENT OF FINANCIAL PERFORMANCE FOR THE PERIOD ENDED: " + STRING(wsPer).
    wsDescrip = "PROJECT: " + STRING(st-proj) + " TO " + STRING(end-proj).
FOR EACH glProj WHERE glproj.proj >= st-proj AND glproj.proj <= end-proj:
    IF NOT CAN-FIND (FIRST glPbal WHERE glPBal.Proj = glProj.Proj
                     AND  glPBal.YEAR = wsYear) THEN NEXT.
    ELSE
       DISPLAY STREAM a glProj.DESCRIPTION @ glmf.DESCRIPTION WITH FRAME frmRpt.
    ASSIGN prclasAct[2] = 0
           wsGrand[2]   = 0
           wsclasAct[2] = 0
           wsGrand[1]   = 0
           wsGrand[2]   = 0.
    DOWN STREAM a WITH FRAME frmRpt.
    FOR EACH glmf WHERE  (glmf.acctype = "I" OR glmf.acctype = "E") BREAK BY clas BY cat:
        DISPLAY glmf.acct @ wsStatus WITH FRAME frm-main.
        IF FIRST-OF(glmf.clas) THEN DO:
            FIND FIRST glclas WHERE glclas.clas = glmf.clas NO-ERROR.
            ASSIGN wsclasAct[1] = 0
                   wsAct[2]     = 0
                   prclasAct[1] = 0
                   prAct[2]      = 0.
                   prnTitle = NO.
        END.
        IF FIRST-OF(glmf.cat) THEN DO:
            ASSIGN wsAct[1] = 0
                   prAct[1] = 0.
        END.
        
       FOR EACH glPbal WHERE glPbal.acct = glmf.acct 
                         AND glPbal.YEAR = wsYear AND glPbal.Proj = glProj.Proj NO-LOCK: /* Current year */
        DO X = 1 TO wsMonth:
                ASSIGN wsAct[1]     = wsAct[1]     + glpbal.amt[X]
                       wsclasAct[1] = wsclasAct[1] + glpbal.amt[X] 
                       wsAct[2]     = wsAct[2]     + glpbal.amt[X]
                       wsclasAct[2] = wsclasAct[2] + glpbal.amt[X]
                       wsGrand[1]   = wsGrand[1]   + glpbal.amt[X].
            END.
        END.
        FOR EACH glpbal WHERE glpbal.acct = glmf.acct 
                           AND glpbal.YEAR = (wsYear - 1) AND glPbal.Proj = glProj.Proj NO-LOCK: /*previous year */
          DO X = 1 TO wsMonth:
                ASSIGN prAct[1]      = prAct[1]     + glPbal.amt[X]
                       prclasAct[1]  = prclasAct[1] + glPbal.amt[X] 
                       prAct[2]      = prAct[2]     + glPbal.amt[X]
                       prclasAct[2]  = prclasAct[2] + glPbal.amt[X]
                       wsGrand[2]    = wsGrand[2]   + glPbal.amt[X].
            END.
        END.
        IF  wsAct[1] <> 0 OR  prAct[1] <> 0 THEN DO:
        IF prnTitle = NO THEN DO:
            DISPLAY STREAM a glclas.DESCRIP @ glmf.DESCRIPTION WITH FRAME frmRpt. 
            DOWN STREAM a WITH FRAME frmRpt.
            prnTitle = YES.
        END.
    END.
        IF last-OF(glmf.cat) THEN DO:
            FIND FIRST glcat WHERE glcat.cat = glmf.cat NO-ERROR.
            IF NOT AVAILABLE glcat THEN
                MESSAGE glmf.cat " category does note exist" VIEW-AS ALERT-BOX.
            ELSE wsTitle = "     " + glcat.DESCRIP.
            IF  wsAct[1] <> 0 OR  prAct[1] <> 0 THEN DO: 
                 DISPLAY STREAM a wsTitle @ glmf.DESCRIPTION  wsAct[1] prAct[1] WITH FRAME frmRpt.
                 DOWN STREAM a WITH FRAME frmRpt.
            END.
        END.  
        IF last-OF(glmf.clas) AND (wsAct[2] <> 0 OR  prAct[2] <> 0) THEN DO:
                UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
                DISPLAY STREAM a "   TOTAL" @ glmf.DESCRIPTION wsAct[2] @ wsAct[1]
                                  prAct[2] @ prAct[1] WITH FRAME frmRpt.
                DOWN 2 STREAM a WITH FRAME frmRpt.
        END. 
    END.
    UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
    DISPLAY STREAM a "SURPLU/DEFICIT" @ glmf.DESCRIPTION wsclasAct[2] @ wsAct[1]
                              prclasAct[2] @ prAct[1]  WITH FRAME frmRpt.
    DOWN  STREAM a WITH FRAME frmRpt.
    FOR EACH glmf WHERE glmf.acct = simctr.suracc NO-LOCK:
        ASSIGN wsAct[1] = 0
               prAct[1] = 0
               wsGrand[1] = 0.
       FOR  EACH glPbal WHERE glPbal.YEAR = wsYear 
                      AND glPbal.Proj = glProj.Proj NO-LOCK: /* Current year */
                ASSIGN wsAct[1]     = wsAct[1] + GLPBAL.BFBAL
                       wsGrand[1]   = wsGrand[1] + GLPBAL.BFBAL.
                DO X = 1 TO wsMonth:
           ASSIGN wsAct[1]     = wsAct[1]   + GLpBAL.amt[X]
                  wsGrand[1]   = wsGrand[1] + GLpBAL.amt[X].
        END.
        FOR  EACH bfglPbal WHERE bfglPbal.acct = glmf.acct 
                           AND bfglPbal.YEAR = (wsYear - 1) 
                           AND bfglPbal.Proj = glProj.Proj NO-LOCK : /*previous year */
                ASSIGN prAct[1]     = prAct[1] +  bfGLPBAL.BFBAL
                       wsGrand[2]   = wsGrand[2] +  bfGLPBAL.BFBAL.
        END.
    END.
    END.
    IF CLOSEPER >= (wsPer - 100) THEN
            DISPLAY STREAM a "SURPLUS/DEFICIT B/F" @ glmf.DESCRIPTION wsAct[1] prAct[1]
                                WITH FRAME frmRpt.
        ELSE 
           DISPLAY STREAM a "SURPLUS/DEFICIT B/F" @ glmf.DESCRIPTION wsAct[1] prAct[1]
                                       WITH FRAME frmRpt.
        UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
        DISPLAY STREAM a "SURPLUS/DEFICIT C/D" @ glmf.DESCRIPTION (wsclasAct[2] + wsAct[1]) @ wsAct[1] 
        wsGrand[2] @ prAct[1] WITH FRAME frmRpt.
        UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
        PAGE STREAM a.
        APPLY 'CLOSE' TO THIS-PROCEDURE.
END. /*each glProj */
END.


PROCEDURE Report2.ip:
    wsTitle1 = "STATEMENT OF FINANCIAL PERFORMANCE FOR THE PERIOD ENDED: " + STRING(wsPer).
    wsDescrip = "Fund: " + STRING(st-fund) + " TO " + STRING(end-fund).
    FOR EACH glFund WHERE glFund.Fund >= st-Fund AND glFund.Fund <= end-Fund:
        IF NOT CAN-FIND (FIRST glFbal WHERE glFbal.Fund = glFund.Fund
                         AND  glFbal.YEAR = wsYear) THEN NEXT.
        ELSE
           DISPLAY STREAM a glFund.DESCRIP @ glmf.DESCRIPTION WITH FRAME frmRpt.
        ASSIGN prclasAct[2] = 0
               wsGrand[2]   = 0
               wsclasAct[2] = 0
               wsGrand[1]   = 0
               wsGrand[2]   = 0.
        DOWN STREAM a WITH FRAME frmRpt.
        FOR EACH glmf WHERE  (glmf.acctype = "I" OR glmf.acctype = "E") BREAK BY clas BY cat:
            DISPLAY glmf.acct @ wsStatus WITH FRAME frm-main.
            IF FIRST-OF(glmf.clas) THEN DO:
                FIND FIRST glclas WHERE glclas.clas = glmf.clas NO-ERROR.
                ASSIGN wsclasAct[1] = 0
                       wsAct[2]     = 0
                       prclasAct[1] = 0
                       prAct[2]      = 0.
                       prnTitle = NO.
            END.
            IF FIRST-OF(glmf.cat) THEN DO:
                ASSIGN wsAct[1] = 0
                       prAct[1] = 0.
            END.     
           FOR EACH glFbal WHERE glFbal.acct = glmf.acct 
                             AND glFbal.YEAR = wsYear AND glFbal.Fund = glFund.Fund NO-LOCK: /* Current year */
            DO X = 1 TO wsMonth:
                    ASSIGN wsAct[1]     = wsAct[1]     + glFbal.amt[X]
                           wsclasAct[1] = wsclasAct[1] + glFbal.amt[X] 
                           wsAct[2]     = wsAct[2]     + glFbal.amt[X]
                           wsclasAct[2] = wsclasAct[2] + glFbal.amt[X]
                           wsGrand[1]   = wsGrand[1]   + glFbal.amt[X].
                END.
            END.
            FOR EACH glFbal WHERE glFbal.acct = glmf.acct 
                               AND glFbal.YEAR = (wsYear - 1) AND glFbal.Fund = glFund.Fund NO-LOCK: /*previous year */
              DO X = 1 TO wsMonth:
                    ASSIGN prAct[1]      = prAct[1]     + glFbal.amt[X]
                           prclasAct[1]  = prclasAct[1] + glFbal.amt[X] 
                           prAct[2]      = prAct[2]     + glFbal.amt[X]
                           prclasAct[2]  = prclasAct[2] + glFbal.amt[X]
                           wsGrand[2]    = wsGrand[2]   + glFbal.amt[X].
                END.
            END.
            IF  wsAct[1] <> 0 OR  prAct[1] <> 0 THEN DO:
                IF prnTitle = NO THEN DO:
                    DISPLAY STREAM a glclas.DESCRIP @ glmf.DESCRIPTION WITH FRAME frmRpt. 
                    DOWN STREAM a WITH FRAME frmRpt.
                    prnTitle = YES.
                END.
            END.
            IF last-OF(glmf.cat) THEN DO:
                FIND FIRST glcat WHERE glcat.cat = glmf.cat NO-ERROR.
                IF NOT AVAILABLE glcat THEN
                    MESSAGE glmf.cat " category does note exist" VIEW-AS ALERT-BOX.
                ELSE wsTitle = "     " + glcat.DESCRIP.
                IF  wsAct[1] <> 0 OR  prAct[1] <> 0 THEN DO: 
                     DISPLAY STREAM a wsTitle @ glmf.DESCRIPTION  wsAct[1] prAct[1] WITH FRAME frmRpt.
                     DOWN STREAM a WITH FRAME frmRpt.
                END.
            END.  
            IF last-OF(glmf.clas) AND (wsAct[2] <> 0 OR  prAct[2] <> 0) THEN DO:
                    UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
                    DISPLAY STREAM a "   TOTAL" @ glmf.DESCRIPTION wsAct[2] @ wsAct[1]
                                      prAct[2] @ prAct[1] WITH FRAME frmRpt.
                    DOWN 2 STREAM a WITH FRAME frmRpt.
            END. 
        END. /* eof glm... I /E */
        UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
        DISPLAY STREAM a "SURPLU/DEFICIT" @ glmf.DESCRIPTION wsclasAct[2] @ wsAct[1]
                                  prclasAct[2] @ prAct[1]  WITH FRAME frmRpt.
        DOWN  STREAM a WITH FRAME frmRpt.     
        FOR EACH glmf WHERE glmf.acct = simctr.suracc NO-LOCK:
            ASSIGN wsAct[1] = 0
                   prAct[1] = 0
                   wsGrand[1] = 0.
            FOR EACH glFbal WHERE glFbal.YEAR = wsYear 
                          AND glFbal.Fund = glFund.Fund  AND glfbal.acct = glmf.acct NO-LOCK: /* Current year */
                    ASSIGN wsAct[1]     = wsAct[1] + glFbal.BFBAL
                           wsGrand[1]   = wsGrand[1] + glFbal.BFBAL.
                    DO X = 1 TO wsMonth:
                        ASSIGN wsAct[1]     = wsAct[1]   + GLfBAL.amt[X]
                              wsGrand[1]   = wsGrand[1] + GLfBAL.amt[X].
                    END.
            END.
            FOR  EACH bfglFbal WHERE bfglFbal.acct = glmf.acct 
                               AND bfglFbal.YEAR = (wsYear - 1) 
                               AND bfglFbal.Fund = glFund.Fund NO-LOCK : /*previous year */
                    ASSIGN prAct[1]     = prAct[1] +  bfglFbal.BFBAL
                           wsGrand[2]   = wsGrand[2] +  bfglFbal.BFBAL.
            END.
        END.
        IF CLOSEPER >= (wsPer - 100) THEN
            DISPLAY STREAM a "SURPLUS/DEFICIT B/F" @ glmf.DESCRIPTION wsAct[1] prAct[1]
                                WITH FRAME frmRpt.
        ELSE 
           DISPLAY STREAM a "SURPLUS/DEFICIT B/F" @ glmf.DESCRIPTION wsAct[1] prAct[1]
                                       WITH FRAME frmRpt.
        UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
        DISPLAY STREAM a "SURPLUS/DEFICIT C/D" @ glmf.DESCRIPTION (wsclasAct[2] + wsAct[1]) @ wsAct[1] 
        wsGrand[2] @ prAct[1] WITH FRAME frmRpt.
        UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
        PAGE STREAM a.
        APPLY 'CLOSE' TO THIS-PROCEDURE.
    END. /*each glFund */
END.


PROCEDURE Report3.ip: /* Departmental Report */
    wsTitle1 = "STATEMENT OF FINANCIAL PERFORMANCE FOR THE PERIOD ENDED: " + STRING(wsPer).
    wsDescrip = "DEPARTMENT: " + STRING(st-dept) + " TO " + STRING(end-dept).
    FOR EACH glDept WHERE glDept.Dept >= st-Dept AND glDept.Dept <= end-Dept:
        IF NOT CAN-FIND (FIRST glDbal WHERE glDbal.Dept = glDept.Dept
                         AND  glDbal.YEAR = wsYear) THEN NEXT.
        ELSE
           DISPLAY STREAM a glDept.DESCRIP @ glmf.DESCRIPTION WITH FRAME frmRpt.
        ASSIGN prclasAct[2] = 0
               wsGrand[2]   = 0
               wsclasAct[2] = 0
               wsGrand[1]   = 0
               wsGrand[2]   = 0.
        DOWN STREAM a WITH FRAME frmRpt.
        FOR EACH glmf WHERE  (glmf.acctype = "I" OR glmf.acctype = "E") BREAK BY clas BY cat:
            DISPLAY glmf.acct @ wsStatus WITH FRAME frm-main.
            IF FIRST-OF(glmf.clas) THEN DO:
                FIND FIRST glclas WHERE glclas.clas = glmf.clas NO-ERROR.
                ASSIGN wsclasAct[1] = 0
                       wsAct[2]     = 0
                       prclasAct[1] = 0
                       prAct[2]      = 0.
                       prnTitle = NO.
            END.
            IF FIRST-OF(glmf.cat) THEN DO:
                ASSIGN wsAct[1] = 0
                       prAct[1] = 0.
            END.
           FOR EACH glDbal WHERE glDbal.acct = glmf.acct 
                             AND glDbal.YEAR = wsYear AND glDbal.Dept = glDept.Dept NO-LOCK: /* Current year */
            DO X = 1 TO wsMonth:
                    ASSIGN wsAct[1]     = wsAct[1]     + glDbal.amt[X]
                           wsclasAct[1] = wsclasAct[1] + glDbal.amt[X] 
                           wsAct[2]     = wsAct[2]     + glDbal.amt[X]
                           wsclasAct[2] = wsclasAct[2] + glDbal.amt[X]
                           wsGrand[1]   = wsGrand[1]   + glDbal.amt[X].
                END.
            END.
            FOR EACH glDbal WHERE glDbal.acct = glmf.acct 
                               AND glDbal.YEAR = (wsYear - 1) AND glDbal.Dept = glDept.Dept NO-LOCK: /*previous year */
              DO X = 1 TO wsMonth:
                    ASSIGN prAct[1]      = prAct[1]     + glDbal.amt[X]
                           prclasAct[1]  = prclasAct[1] + glDbal.amt[X] 
                           prAct[2]      = prAct[2]     + glDbal.amt[X]
                           prclasAct[2]  = prclasAct[2] + glDbal.amt[X]
                           wsGrand[2]    = wsGrand[2]   + glDbal.amt[X].
                END.
            END.
            IF  wsAct[1] <> 0 OR  prAct[1] <> 0 THEN DO:
            IF prnTitle = NO THEN DO:
                DISPLAY STREAM a glclas.DESCRIP @ glmf.DESCRIPTION WITH FRAME frmRpt. 
                DOWN STREAM a WITH FRAME frmRpt.
                prnTitle = YES.
            END.
        END.
            IF last-OF(glmf.cat) THEN DO:
                FIND FIRST glcat WHERE glcat.cat = glmf.cat NO-ERROR.
                IF NOT AVAILABLE glcat THEN
                    MESSAGE glmf.cat " category does note exist" VIEW-AS ALERT-BOX.
                ELSE wsTitle = "     " + glcat.DESCRIP.
                IF  wsAct[1] <> 0 OR  prAct[1] <> 0 THEN DO: 
                     DISPLAY STREAM a wsTitle @ glmf.DESCRIPTION  wsAct[1] prAct[1] WITH FRAME frmRpt.
                     DOWN STREAM a WITH FRAME frmRpt.
                END.
            END.  
            IF last-OF(glmf.clas) AND (wsAct[2] <> 0 OR  prAct[2] <> 0) THEN DO:
                    UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
                    DISPLAY STREAM a "   TOTAL" @ glmf.DESCRIPTION wsAct[2] @ wsAct[1]
                                      prAct[2] @ prAct[1] WITH FRAME frmRpt.
                    DOWN 2 STREAM a WITH FRAME frmRpt.
            END. 
        END.
        UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
        DISPLAY STREAM a "SURPLU/DEFICIT" @ glmf.DESCRIPTION wsclasAct[2] @ wsAct[1]
                                  prclasAct[2] @ prAct[1]  WITH FRAME frmRpt.
        DOWN  STREAM a WITH FRAME frmRpt.           
        FOR EACH glmf WHERE glmf.acct = simctr.suracc NO-LOCK:
            ASSIGN wsAct[1] = 0
                   prAct[1] = 0
                   wsGrand[1] = 0.
            FOR EACH glDbal WHERE glDbal.YEAR = wsYear 
                          AND glDbal.Dept = glDept.Dept NO-LOCK: /* Current year */
                    ASSIGN wsAct[1]     = wsAct[1] + glDbal.BFBAL
                           wsGrand[1]   = wsGrand[1] + glDbal.BFBAL.
                    DO X = 1 TO wsMonth:
                       ASSIGN wsAct[1]     = wsAct[1]   + GLdBAL.amt[X]
                              wsGrand[1]   = wsGrand[1] + GLdBAL.amt[X].
                    END.
                FOR  EACH bfglDbal WHERE bfglDbal.acct = glmf.acct 
                                   AND bfglDbal.YEAR = (wsYear - 1) 
                                   AND bfglDbal.Dept = glDept.Dept NO-LOCK : /*previous year */
                        ASSIGN prAct[1]     = prAct[1] +  bfglDbal.BFBAL
                               wsGrand[2]   = wsGrand[2] +  bfglDbal.BFBAL.
                END.
            END.
        END.
        IF CLOSEPER >= (wsPer - 100) THEN
            DISPLAY STREAM a "SURPLUS/DEFICIT B/F" @ glmf.DESCRIPTION wsAct[1] prAct[1]
                                WITH FRAME frmRpt.
        ELSE 
           DISPLAY STREAM a "SURPLUS/DEFICIT B/F" @ glmf.DESCRIPTION wsAct[1] prAct[1]
                                       WITH FRAME frmRpt.
        UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
        DISPLAY STREAM a "SURPLUS/DEFICIT C/D" @ glmf.DESCRIPTION (wsclasAct[2] + wsAct[1]) @ wsAct[1] 
        wsGrand[2] @ prAct[1] WITH FRAME frmRpt.
        UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
        PAGE STREAM a.
        APPLY 'CLOSE' TO THIS-PROCEDURE.
    END. /*each glDept */
END.

PROCEDURE Notes.ip:
    wsTitle1 = "NOTES TO STATEMENT OF FINANCIAL PERFORMANCE FOR THE PERIOD ENDED: " + STRING(wsPer).
    IF st-proj = end-proj THEN DO:
        FIND glproj WHERE glproj.proj = st-proj NO-LOCK NO-ERROR.
        wsDescrip = GLproj.DESCRIP.
    END.
    ELSE IF end-proj = 99 THEN
        wsDescrip = "CONSOLIDATED".
    ELSE 
        wsDescrip = "proj: " + STRING(st-proj) + " TO " + STRING(end-proj).
FOR EACH glmf WHERE (glmf.acctype = "I" OR glmf.acctype = "E")
                 BREAK BY glmf.clas BY glmf.cat BY glmf.acct:
    ASSIGN wsAct[1] = 0
           prAct[1] = 0.
    DISPLAY glmf.acct @ wsStatus WITH FRAME frm-main.
    IF FIRST-OF(glmf.cat) THEN DO:
        ASSIGN wsclasAct[1] = 0
               wsAct[2] = 0
               prclasAct[1] = 0
               prAct[2] = 0.
        IF CAN-FIND (FIRST glbal WHERE SUBSTR(STRING(glbal.acct),1,2) = SUBSTR(STRING(glmf.acct),1,2))
            THEN DO:
            FIND FIRST glcat WHERE glcat.cat = glmf.cat NO-ERROR.
            DISPLAY STREAM a glcat.DESCRIP @ glmf.DESCRIPTION 
                WITH FRAME frmRpt. 
            DOWN STREAM a WITH FRAME frmRpt.
        END.
    END.
    FOR EACH glbal WHERE glbal.acct = glmf.acct 
                       AND glbal.YEAR = wsYear NO-LOCK: /* Current year */
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
      DO X = 1 TO wsMonth:
            ASSIGN prAct[1]     = prAct[1] + glbal.amt[X]
                   prclasAct[1]  = prclasAct[1] + glbal.amt[X] 
                   prAct[2]     = prAct[2] + glbal.amt[X]
                   prclasAct[2]  = prclasAct[2] + glbal.amt[X]
                   wsGrand[2]   = wsGrand[2] + glbal.amt[X].
        END.
    END.
    IF  wsAct[1] <> 0 OR  prAct[1] <> 0 THEN DO:
        DISPLAY STREAM a "     " + glmf.DESCRIPTION @ glmf.DESCRIPTION   wsAct[1] prAct[1] WITH FRAME frmRpt.
        DOWN STREAM a WITH FRAME frmRpt.
    END.
    IF LAST-OF(glmf.cat) THEN
          IF  wsclasAct[1] <> 0 OR  prclasAct[2] <> 0 THEN DO:
        UNDERLINE STREAM a glmf.DESCRIPTION  wsAct[1] prAct[1] WITH FRAME frmRpt.
        DISPLAY STREAM a "TOTAL " @ glmf.DESCRIPTION  wsclasAct[1] @ wsAct[1] prclasAct[1] @ prAct[1] WITH FRAME frmRpt.
        DOWN 2 STREAM a WITH FRAME frmRpt.
    END.
END. /*each glmf */
APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

PROCEDURE NatRep.ip: /* Consolidated Report */
    wsTitle1 = "STATEMENT OF FINANCIAL PERFORMANCE FOR THE PERIOD ENDED: " + STRING(wsPer).
    IF st-proj = end-proj THEN DO:
        FIND glproj WHERE glproj.proj = st-proj NO-LOCK NO-ERROR.
        wsDescrip = GLproj.DESCRIP.
    END.
    ELSE IF end-proj = 99 THEN
        wsDescrip = "CONSOLIDATED".
    ELSE 
        wsDescrip = "PROJECT: " + STRING(st-proj) + " TO " + STRING(end-proj).
        wsGrand = 0.
FOR EACH glmf WHERE (glmf.acctype = "I" OR glmf.acctype = "E") BREAK BY clas BY cat:
    DISPLAY glmf.acct @ wsStatus WITH FRAME frm-main.
    IF FIRST-OF(glmf.clas) THEN DO:
        prnTitle = NO.
        FIND FIRST glclas WHERE glclas.clas = glmf.clas NO-ERROR.
        ASSIGN wsclasAct[1] = 0
               wsAct[2] = 0
               prclasAct[1] = 0
               prAct[2] = 0.
    END.
    IF FIRST-OF(glmf.cat) THEN DO:
        ASSIGN wsAct[1] = 0
               prAct[1] = 0.
    END.
    FIND FIRST glbal WHERE glbal.acct = glmf.acct 
                       AND glbal.YEAR = wsYear NO-LOCK NO-ERROR. /* Current year */
    IF AVAILABLE glbal THEN DO:
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
      DO X = 1 TO wsMonth:
            ASSIGN prAct[1]     = prAct[1] + glbal.amt[X]
                   prclasAct[1]  = prclasAct[1] + glbal.amt[X] 
                   prAct[2]     = prAct[2] + glbal.amt[X]
                   prclasAct[2]  = prclasAct[2] + glbal.amt[X]
                   wsGrand[2]   = wsGrand[2] + glbal.amt[X].
        END.
    END.
    IF  wsAct[1] <> 0 OR  prAct[1] <> 0 THEN DO:
        IF prnTitle = NO THEN DO:
            DISPLAY STREAM a glclas.DESCRIP @ glmf.DESCRIPTION WITH FRAME frmRpt. 
            DOWN STREAM a WITH FRAME frmRpt.
            prnTitle = YES.
        END.
    END.
    IF last-OF(glmf.cat) THEN DO:
        FIND FIRST glcat WHERE glcat.cat = glmf.cat NO-ERROR.
        IF NOT AVAILABLE glcat THEN
            MESSAGE glmf.cat " category does not exist" VIEW-AS ALERT-BOX.
        ELSE wsTitle = "     " + glcat.DESCRIP.
        IF  wsAct[1] <> 0 OR  prAct[1] <> 0 THEN DO: 
             DISPLAY STREAM a wsTitle @ glmf.DESCRIPTION  wsAct[1] prAct[1] WITH FRAME frmRpt.
             DOWN STREAM a WITH FRAME frmRpt.
        END.
    END.  
    IF last-OF(glmf.clas) AND (wsAct[2] <> 0 OR  prAct[2] <> 0) THEN DO:
        UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
        DISPLAY STREAM a "TOTAL" @ glmf.DESCRIPTION wsAct[2] @ wsAct[1]
                          prAct[2] @ prAct[1] WITH FRAME frmRpt.
        DOWN 2 STREAM a WITH FRAME frmRpt.
    END. 
END.
UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
DISPLAY STREAM a "SURPLU/DEFICIT" @ glmf.DESCRIPTION wsclasAct[2] @ wsAct[1]
                          prclasAct[2] @ prAct[1]  WITH FRAME frmRpt.
DOWN  STREAM a WITH FRAME frmRpt.
ASSIGN wsAct[1] = 0
       prAct[1] = 0.
FOR EACH glmf WHERE glmf.acct = Simctr.SurAcc:
    FIND FIRST glbal WHERE glbal.acct = glmf.acct 
                       AND glbal.YEAR = wsYear NO-LOCK NO-ERROR. /* Current year */
    IF AVAILABLE glbal THEN  DO:
        ASSIGN wsAct[1]     = wsAct[1] + GLBAL.BFBAL
                   wsGrand[1]   = wsGrand[1] + GLBAL.BFBAL.
        DO X = 1 TO wsMonth:
           ASSIGN wsAct[1]     = wsAct[1]   + GLBAL.amt[wsMonth]
                  wsGrand[1]   = wsGrand[1] + GLBAL.amt[wsMonth].
        END.
    END.
    FIND FIRST glbal WHERE glbal.acct = glmf.acct 
                       AND glbal.YEAR = (wsYear - 1) NO-LOCK NO-ERROR. /*previous year */
    IF AVAILABLE glbal THEN
       ASSIGN prAct[1]     = prAct[1] +  GLBAL.BFBAL
              wsGrand[2]   = wsGrand[2] +  GLBAL.BFBAL.
        
END.
DISPLAY STREAM a "SURPLUS/DEFICIT B/F" @ glmf.DESCRIPTION wsGrand[2] @ wsAct[1] prAct[1]
                               WITH FRAME frmRpt.
UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
DISPLAY STREAM a "SURPLUS/DEFICIT C/D" @ glmf.DESCRIPTION (wsclasAct[2] + wsGrand[2]) @ wsAct[1] 
        wsGrand[2] @ prAct[1] WITH FRAME frmRpt.
UNDERLINE STREAM a wsAct[1] prAct[1] WITH FRAME frmRpt.
DOWN  STREAM a WITH FRAME frmRpt.
IF wsNotes = YES THEN DO:
    PAGE STREAM a.
      RUN Notes.ip.
END.
APPLY 'CLOSE' TO THIS-PROCEDURE.
END.
