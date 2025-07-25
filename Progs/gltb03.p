/* Program.................gltb01.p
   Notes:................. Trial Balance Report
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF STREAM b.
DEF VAR w-orientation AS CHAR      INITIAL "Landscape"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10.0          NO-UNDO.
DEF VAR wsUser AS CHAR FORM "xxx" INITIAL "01".
DEF VAR wsYear AS INT FORM "9999".
DEF VAR wsMonth AS INT FORM "99".
DEF VAR wsstart LIKE glmf.acct.
DEF VAR wsend LIKE glmf.acct.
DEF VAR wsOpt AS INT VIEW-AS RADIO-SET RADIO-BUTTONS 
    "Consolidated Trial Balance", 1,
    "Trial Balance by Project", 2,
    "Trial Balance by Segment", 3,
    "Trial Balance by Departments", 4.
DEF VAR wsFile AS CHAR FORM "x(40)".    
DEF VAR curAmt AS DEC EXTENT 4 FORM "ZZZZZZZZZZ9.99-".
DEF VAR ytdAmt AS DEC EXTENT 4 FORM "ZZZZZZZZZZ9.99-".
DEF VAR wsamt  AS DEC EXTENT 2 FORM "ZZZZZZZZZZ9.99-".
DEF VAR DTAmt AS DEC EXTENT 4 FORM "ZZZZZZZZZZ9.99-".
DEF VAR PTAmt AS DEC EXTENT 4 FORM "ZZZZZZZZZZ9.99-".
DEF VAR st-proj LIKE glproj.proj.
DEF VAR end-Proj LIKE glproj.proj.
DEF VAR st-dept LIKE gldept.dept.
DEF VAR end-dept LIKE gldept.dept.
DEF VAR st-fund  LIKE glfund.fund.
DEF VAR end-fund LIKE glfund.fund.
DEF VAR LAmnt LIKE gltdf.amt.
DEF VAR wsProg AS CHAR.
DEF VAR wsTitle AS CHAR FORM "x(80)".
DEF VAR X AS INT.

DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-ok     LABEL "Print".
DEF BUTTON btn-exp    LABEL "Export".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 18.5.

DEF FRAME frm-main
    SKIP(1)
    wsYear    LABEL "Accounting Year" COLON 30 
    wsMonth   LABEL "Accounting Month"  
     SKIP(0.5)
    wsstart   LABEL "Start Account"   COLON 30 SKIP(0.5)
    wsend    LABEL  "End Account"    COLON 30
        SKIP(0.5)
    wsOpt    LABEL  "Selection Report Option" COLON 30 SKIP(0.5)
    st-Proj   LABEL "Start Project"   COLON 30 SKIP(0.5)
    end-Proj  LABEL  "End Project"    COLON 30
        SKIP(0.5)
    st-Fund   LABEL "Start Segment"   COLON 30 SKIP(0.5)
    end-Fund  LABEL  "End Segment"    COLON 30
        SKIP(0.5)
    st-Dept   LABEL "Start Department"   COLON 30 SKIP(0.5)
    end-Dept  LABEL  "End Department"    COLON 30
        SKIP(1)
    /*btn-ok AT ROW 20.7 COL 20
    SPACE(25)*/
    btn-Exp AT ROW 20.7 COL 20 SPACE(50) btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 20 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 24
    TITLE "TRIAL BALANCE REPORT" VIEW-AS DIALOG-BOX KEEP-TAB-ORDER.

FORM 
     glmf.acct        LABEL "ACCOUNT"
     glmf.DESCRIPTION LABEL "DESCRIPTION"
     curAmt[1]        LABEL "DEBITS"
     curAmt[2]        LABEL "CREDITS"
     ytdAmt[1]        LABEL "DEBITS"
     ytdAmt[2]        LABEL "CREDITS"
    HEADER SKIP(1) wsTitle AT 20 skip(1) "                         TRIAL BALANCE REPORT FOR THE PERIOD:" AT 10 SPACE(2)
    STRING(wsYear) + STRING(wsMonth,"99")
    "Page: " AT 110 PAGE-NUMBER(a) SKIP(1)
    SPACE(65)" --------CURRENT PERIOD--------  --------TOTAL TO DATE--------" SKIP(1)
    WITH DOWN STREAM-IO FONT 6 CENTERED NO-BOX
    WIDTH 132 FRAME frm-rpt.

/* ******** Triggers  ***********/
ON 'enter':U OF wsOpt IN FRAME frm-main 
   OR 'tab':U OF wsOpt IN FRAME frm-main 
DO:
    ASSIGN wsopt.
    IF wsOpt = 1 THEN DO:
        DISABLE st-proj end-proj st-fund end-fund st-dept end-dept WITH FRAME frm-main.
        wsProg = "Report1.ip".
    END.
    IF wsOpt = 2 THEN DO:
        ENABLE st-proj end-proj WITH FRAME frm-main.
        DISABLE st-fund end-fund st-dept end-dept WITH FRAME frm-main.
         wsProg = "Report2.ip".
    END.
    IF wsOpt = 3 THEN DO:
        ENABLE st-fund end-fund  WITH FRAME frm-main.
        DISABLE st-proj end-proj st-dept end-dept WITH FRAME frm-main. 
         wsProg = "Report3.ip".
    END.
    IF wsOpt = 4 THEN DO:
        ENABLE st-dept end-dept  WITH FRAME frm-main.
        DISABLE st-proj end-proj st-fund end-fund WITH FRAME frm-main.
         wsProg = "Report4.ip".
    END.
    APPLY 'tab' TO SELF.
    RETURN.
END.

/*ON 'choose':U OF btn-ok  
DO:
   session:set-wait-state("").
   APPLY 'tab' TO wsOpt.
    ASSIGN curAmt = 0
           ytdAmt = 0
           wsamt  = 0
           DTAmt =  0
           PTAmt =  0.
   ASSIGN wsYear wsMonth wsStart wsEnd st-proj end-proj st-fund end-fund st-dept end-dept.
   {PrintOpt.i &stream-name="stream a"
                    &print-prog = VALUE(wsProg)
                    &paged}
                   
END. */

ON 'choose':U OF btn-Exp  
DO:
   session:set-wait-state("").
   APPLY 'tab' TO wsOpt.
   ASSIGN curAmt = 0
           ytdAmt = 0
           wsamt  = 0
           DTAmt =  0
           PTAmt =  0.
   wsFile = simctr.repDir + "TB" 
          + string(day(today),"99")    + "_"
          + string(month(today),"99")  + "_"
          + string(year(today),"9999") + "_"
          + substr(string(time,"HH:MM:SS"),1,2)
          + substr(string(time,"HH:MM:SS"),4,2)
          + substr(string(time,"HH:MM:SS"),7,2)
          + ".csv".    
   OUTPUT STREAM b TO VALUE(wsFile).
    curAmt = 0.
    ytdAmt = 0.
   ASSIGN wsYear wsMonth wsStart wsEnd st-proj end-proj st-fund end-fund st-dept end-dept.
   IF wsOpt = 1 THEN
      RUN Exc1.ip.
   ELSE IF wsOpt = 2 THEN
      RUN Exc2.ip.
   ELSE IF wsOpt = 3 THEN
      RUN Exc3.ip.
   ELSE IF wsOpt = 4 THEN
      RUN Exc4.ip.
   wsfile = "START " + wsFile.
   OUTPUT STREAM b CLOSE.
    OS-COMMAND NO-WAIT VALUE(wsFile).
END.

/********** MAIN LOGIC **********/
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
FIND FIRST simctr.
ASSIGN wsYear:SCREEN-VALUE = STRING(YEAR(TODAY))
       wsMonth:SCREEN-VALUE = STRING(MONTH(TODAY))
       wsStart:SCREEN-VALUE = "0"
       wsEnd:SCREEN-VALUE   = "9999999999"
       st-proj:SCREEN-VALUE = "00"
       end-Proj:SCREEN-VALUE = "99"
       st-Dept:SCREEN-VALUE = "0000"
       end-Dept:SCREEN-VALUE = "9999"
       st-Fund:SCREEN-VALUE = "0000"
       end-Fund:SCREEN-VALUE = "99"
       wsTitle = simctr.CONAME.
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.


PROCEDURE report1.ip:
    /* HIDE FRAME frm-main. */
    ASSIGN wsAmt = 0
           curAmt = 0
           ytdAmt = 0.
   FOR EACH glmf WHERE glmf.acct >= wsStart AND glmf.acct <= wsEnd NO-LOCK BY glmf.acct:
       ASSIGN wsAmt[1] = 0
              curAmt[1] = 0
              curAmt[2] = 0
              ytdAmt[1] = 0
              ytdAmt[2] = 0.
       FIND FIRST glbal WHERE glbal.acct = glmf.acct AND glbal.YEAR = wsYear NO-ERROR.
       IF NOT AVAILABLE glbal THEN  NEXT.
       ELSE IF AVAILABLE glbal THEN DO:
           ASSIGN wsAmt[1] = glbal.amt[wsMonth]
                  wsAmt[2] = bfbal.
           DO X = 1 TO wsMonth:
               ASSIGN wsAmt[2] = wsAmt[2] + glbal.amt[X].
           END.
           ASSIGN curAmt[1] = wsAmt[1] WHEN wsAmt[1] >= 0
                  curAmt[2] = wsAmt[1] WHEN wsAmt[1] < 0
                  curAmt[3] = curAmt[3] + wsAmt[1] WHEN wsAmt[1] >= 0
                  curAmt[4] = curAmt[4] + wsAmt[1] WHEN wsAmt[1] < 0
                  ytdAmt[1] = wsAmt[2] WHEN wsAmt[2] >= 0
                  ytdAmt[2] = wsAmt[2] WHEN wsAmt[2] < 0
                  ytdAmt[3] = ytdAmt[3] + wsAmt[2] WHEN wsAmt[2] >= 0
                  ytdAmt[4] = ytdAmt[4] + wsAmt[2] WHEN wsAmt[2] < 0.
          IF ytdAmt[1] <> 0 OR ytdAmt[2] <> 0 THEN DO:
             DISPLAY STREAM a glmf.acct UPPER(glmf.DESCRIPTION) @ glmf.DESCRIPTION curAmt[1]  curAmt[2]
                               ytdAmt[1] ytdAmt[2] WITH FRAME frm-rpt.
              DOWN STREAM a WITH FRAME frm-rpt.
          END.
       END.
   END.
   UNDERLINE STREAM a glmf.DESCRIPTION curAmt[1] curAmt[2]
                          ytdAmt[1] ytdAmt[2] WITH FRAME frm-rpt.
   DISPLAY STREAM a "TOTALS" @ glmf.DESCRIPTION
                        curAmt[3] @ curAmt[1] curAmt[4] @ curAmt[2]
                        ytdAmt[3] @ ytdAmt[1] ytdAmt[4] @ ytdAmt[2]  WITH FRAME frm-rpt.
   DOWN STREAM a WITH FRAME frm-rpt.
   DISPLAY STREAM a "DIFF" @  glmf.DESCRIPTION (curAmt[3] + curAmt[4]) @ curAmt[2]
                                               (ytdAmt[3] + ytdAmt[4]) @ ytdAmt[2] WITH FRAME frm-rpt.
   DOWN STREAM a WITH FRAME frm-rpt.
   RETURN.
END.

PROCEDURE report2.ip:
    ASSIGN ytdAmt = 0
           curAmt = 0.
    FOR EACH glproj WHERE glproj.proj >= st-proj AND glproj.proj <= end-proj BY proj:
        IF CAN-FIND (FIRST glPbal WHERE glPbal.proj = glproj.proj AND glPbal.YEAR = wsYear)  THEN DO:
            DISPLAY STREAM a UPPER(glproj.descrip) @ glmf.DESCRIPTION WITH FRAME frm-rpt.
            DOWN 1 STREAM a WITH FRAME frm-rpt.
            ASSIGN wsAmt = 0
                   pTAmt = 0.
           FOR EACH glPbal WHERE glPbal.proj = glproj.proj AND glPbal.YEAR = wsYear
                             AND glPbal.acct >= wsStart AND glPbal.acct <= wsEnd NO-LOCK 
                              BREAK BY glPbal.acct:
               IF FIRST-OF(glPbal.acct) THEN DO:
                   FIND FIRST glmf WHERE glmf.acct = glPbal.acct NO-LOCK NO-ERROR.
                   IF NOT AVAILABLE glmf THEN NEXT.
                   ELSE 
                       ASSIGN wsAmt[1] = 0
                              curAmt[1] = 0
                              curAmt[2] = 0
                              ytdAmt[1] = 0
                              ytdAmt[2] = 0.
               END.
               ASSIGN wsAmt[1] = glPbal.amt[wsMonth]
                          wsAmt[2] = glPbal.bfbal.
                   DO X = 1 TO wsMonth:
                       ASSIGN wsAmt[2] = wsAmt[2] + glPbal.amt[X].
                   END.
                   ASSIGN curAmt[1] = wsAmt[1] WHEN wsAmt[1] >= 0
                          curAmt[2] = wsAmt[1] WHEN wsAmt[1] < 0
                          curAmt[3] = curAmt[3] + wsAmt[1] WHEN wsAmt[1] >= 0
                          curAmt[4] = curAmt[4] + wsAmt[1] WHEN wsAmt[1] < 0
                          ytdAmt[1] = wsAmt[2] WHEN wsAmt[2] >= 0
                          ytdAmt[2] = wsAmt[2] WHEN wsAmt[2] < 0
                          ytdAmt[3] = ytdAmt[3] + wsAmt[2] WHEN wsAmt[2] >= 0
                          ytdAmt[4] = ytdAmt[4] + wsAmt[2] WHEN wsAmt[2] < 0
                          pTAmt[1] = pTAmt[1] + wsAmt[1] WHEN wsAmt[2] >= 0
                          pTAmt[2] = pTAmt[2] + wsAmt[1] WHEN wsAmt[2] < 0
                          pTAmt[3] = pTAmt[3] + wsAmt[2] WHEN wsAmt[2] >= 0
                          pTAmt[4] = pTAmt[4] + wsAmt[2] WHEN wsAmt[2] < 0.
                   IF LAST-OF(glPbal.acct) THEN DO:
                       IF ytdAmt[1] <> 0 OR ytdAmt[2] <> 0 THEN DO:
                         DISPLAY STREAM a glmf.acct UPPER(glmf.DESCRIPTION) @ glmf.DESCRIPTION curAmt[1]  curAmt[2]
                                           ytdAmt[1] ytdAmt[2] WITH FRAME frm-rpt.
                          DOWN STREAM a WITH FRAME frm-rpt.
                       END.
                   END.
           END. /* each glPbal */
           UNDERLINE STREAM a glmf.DESCRIPTION curAmt[1] curAmt[2] ytdAmt[1] ytdAmt[2]
                       WITH FRAME frm-rpt.
           DISPLAY  STREAM a "PROJECT TOTALS" @ glmf.DESCRIPTION pTAmt[1]  @ curAmt[1] 
                         pTAmt[2] @ curAmt[2] pTAmt[3]  @ ytdAmt[1] pTAmt[4]  @ ytdAmt[2] 
                 WITH FRAME frm-rpt.
           DOWN 2 STREAM a WITH FRAME frm-rpt.
        END.
    END. /* each glproj */
   UNDERLINE STREAM a glmf.DESCRIPTION curAmt[1] curAmt[2] ytdAmt[1] ytdAmt[2]
         WITH FRAME frm-rpt.
   DISPLAY STREAM a "GRAND TOTALS" @ glmf.DESCRIPTION
                        curAmt[3] @ curAmt[1] curAmt[4] @ curAmt[2]
                        ytdAmt[3] @ ytdAmt[1] ytdAmt[4] @ ytdAmt[2]  WITH FRAME frm-rpt.
   DOWN STREAM a WITH FRAME frm-rpt.
   DISPLAY STREAM a "DIFF" @  glmf.DESCRIPTION (curAmt[3] + curAmt[4]) @ curAmt[2]
                                               (ytdAmt[3] + ytdAmt[4]) @ ytdAmt[2] WITH FRAME frm-rpt.
   DOWN STREAM a WITH FRAME frm-rpt.
   RETURN.
END.

PROCEDURE report3.ip:
    ASSIGN ytdAmt = 0
           curAmt = 0.
    FOR EACH glfund WHERE glfund.fund >= st-fund AND glfund.fund <= end-fund BY glfund.fund:
        IF CAN-FIND (FIRST glFbal WHERE glFbal.fund = glfund.fund AND glFbal.YEAR = wsYear)  THEN DO:
            DISPLAY STREAM a UPPER(glfund.descrip) @ glmf.DESCRIPTION WITH FRAME frm-rpt.
            DOWN 1 STREAM a WITH FRAME frm-rpt.
            ASSIGN wsAmt = 0
                   DTAmt = 0.
           FOR EACH glFbal WHERE glFbal.fund = glfund.fund AND glFbal.YEAR = wsYear
                             AND glFbal.acct >= wsStart AND glFbal.acct <= wsEnd NO-LOCK 
                              BREAK BY glFbal.acct:
               IF FIRST-OF(glFbal.acct) THEN DO:
                  FIND FIRST glmf WHERE glmf.acct = glFbal.acct NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE glmf THEN NEXT.
                  ELSE
                       ASSIGN wsAmt[1] = 0
                              curAmt[1] = 0
                              curAmt[2] = 0
                              ytdAmt[1] = 0
                              ytdAmt[2] = 0.
               END.
               ASSIGN wsAmt[1] = glFbal.amt[wsMonth]
                      wsAmt[2] = glFbal.bfbal.
               DO X = 1 TO wsMonth:
                     ASSIGN wsAmt[2] = wsAmt[2] + glFbal.amt[X].
               END.
               ASSIGN curAmt[1] = wsAmt[1] WHEN wsAmt[1] >= 0
                          curAmt[2] = wsAmt[1] WHEN wsAmt[1] < 0
                          curAmt[3] = curAmt[3] + wsAmt[1] WHEN wsAmt[1] >= 0
                          curAmt[4] = curAmt[4] + wsAmt[1] WHEN wsAmt[1] < 0
                          ytdAmt[1] = wsAmt[2] WHEN wsAmt[2] >= 0
                          ytdAmt[2] = wsAmt[2] WHEN wsAmt[2] < 0
                          ytdAmt[3] = ytdAmt[3] + wsAmt[2] WHEN wsAmt[2] >= 0
                          ytdAmt[4] = ytdAmt[4] + wsAmt[2] WHEN wsAmt[2] < 0
                          DTAmt[1] = DTAmt[1] + wsAmt[1] WHEN wsAmt[2] >= 0
                          DTAmt[2] = DTAmt[2] + wsAmt[1] WHEN wsAmt[2] < 0
                          DTAmt[3] = DTAmt[3] + wsAmt[2] WHEN wsAmt[2] >= 0
                          DTAmt[4] = DTAmt[4] + wsAmt[2] WHEN wsAmt[2] < 0.
                   IF LAST-OF(glFbal.acct) THEN DO: 
                       IF ytdAmt[1] <> 0 OR ytdAmt[2] <> 0 THEN DO:
                            DISPLAY STREAM a glmf.acct UPPER(glmf.DESCRIPTION) @ glmf.DESCRIPTION curAmt[1]  curAmt[2]
                                       ytdAmt[1] ytdAmt[2] WITH FRAME frm-rpt.
                             DOWN STREAM a WITH FRAME frm-rpt.
                       END.
                   END.
                    
           END. /* each glFbal */
           UNDERLINE STREAM a glmf.DESCRIPTION curAmt[1] curAmt[2] ytdAmt[1] ytdAmt[2]
                       WITH FRAME frm-rpt.
           DISPLAY  STREAM a "SEGMENT TOTALS" @ glmf.DESCRIPTION DtAmt[1]  @ curAmt[1] 
                         DtAmt[2] @ curAmt[2] DtAmt[3]  @ ytdAmt[1] DtAmt[4]  @ ytdAmt[2] 
                       WITH FRAME frm-rpt.
           DOWN 2 STREAM a WITH FRAME frm-rpt.
        END.
    END. /* each glFund */
   UNDERLINE STREAM a glmf.DESCRIPTION curAmt[1] curAmt[2] ytdAmt[1] ytdAmt[2]
         WITH FRAME frm-rpt.
   DISPLAY STREAM a "GRAND TOTALS" @ glmf.DESCRIPTION
                        curAmt[3] @ curAmt[1] curAmt[4] @ curAmt[2]
                        ytdAmt[3] @ ytdAmt[1] ytdAmt[4] @ ytdAmt[2]  WITH FRAME frm-rpt.
   DOWN STREAM a WITH FRAME frm-rpt.
   DISPLAY STREAM a "DIFF" @  glmf.DESCRIPTION (curAmt[3] + curAmt[4]) @ curAmt[2]
                                               (ytdAmt[3] + ytdAmt[4]) @ ytdAmt[2] WITH FRAME frm-rpt.
   DOWN STREAM a WITH FRAME frm-rpt.
   RETURN.
END.

PROCEDURE report4.ip:
    ASSIGN ytdAmt = 0
           curAmt = 0.
    FOR EACH gldept WHERE gldept.dept >= st-dept AND gldept.dept <= end-dept BY dept:
        IF CAN-FIND (FIRST gldbal WHERE gldbal.dept = gldept.dept AND gldbal.YEAR = wsYear)  THEN DO:
            DISPLAY STREAM a UPPER(gldept.descrip) @ glmf.DESCRIPTION WITH FRAME frm-rpt.
            DOWN 1 STREAM a WITH FRAME frm-rpt.
            ASSIGN wsAmt = 0
                   DTAmt = 0.
           FOR EACH gldBal WHERE gldbal.dept = gldept.dept AND gldbal.YEAR = wsYear
                             AND gldBal.acct >= wsStart AND gldBal.acct <= wsEnd NO-LOCK 
                              BREAK BY gldBal.acct:
               IF FIRST-OF(gldBal.acct) THEN DO:
                  FIND FIRST glmf WHERE glmf.acct = gldbal.acct NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE glmf THEN NEXT.
                  ELSE
                       ASSIGN wsAmt[1] = 0
                              curAmt[1] = 0
                              curAmt[2] = 0
                              ytdAmt[1] = 0
                              ytdAmt[2] = 0.
               END.
               ASSIGN wsAmt[1] = gldbal.amt[wsMonth]
                      wsAmt[2] = gldbal.bfbal.
               DO X = 1 TO wsMonth:
                     ASSIGN wsAmt[2] = wsAmt[2] + gldbal.amt[X].
               END.
               ASSIGN curAmt[1] = wsAmt[1] WHEN wsAmt[1] >= 0
                          curAmt[2] = wsAmt[1] WHEN wsAmt[1] < 0
                          curAmt[3] = curAmt[3] + wsAmt[1] WHEN wsAmt[1] >= 0
                          curAmt[4] = curAmt[4] + wsAmt[1] WHEN wsAmt[1] < 0
                          ytdAmt[1] = wsAmt[2] WHEN wsAmt[2] >= 0
                          ytdAmt[2] = wsAmt[2] WHEN wsAmt[2] < 0
                          ytdAmt[3] = ytdAmt[3] + wsAmt[2] WHEN wsAmt[2] >= 0
                          ytdAmt[4] = ytdAmt[4] + wsAmt[2] WHEN wsAmt[2] < 0
                          DTAmt[1] = DTAmt[1] + wsAmt[1] WHEN wsAmt[2] >= 0
                          DTAmt[2] = DTAmt[2] + wsAmt[1] WHEN wsAmt[2] < 0
                          DTAmt[3] = DTAmt[3] + wsAmt[2] WHEN wsAmt[2] >= 0
                          DTAmt[4] = DTAmt[4] + wsAmt[2] WHEN wsAmt[2] < 0.
                   IF LAST-OF(gldBal.acct) THEN DO: 
                       IF ytdAmt[1] <> 0 OR ytdAmt[2] <> 0 THEN DO:
                            DISPLAY STREAM a glmf.acct UPPER(glmf.DESCRIPTION) @ glmf.DESCRIPTION curAmt[1]  curAmt[2]
                                       ytdAmt[1] ytdAmt[2] WITH FRAME frm-rpt.
                             DOWN STREAM a WITH FRAME frm-rpt.
                       END.
                   END.
                    
           END. /* each glDBal */
           UNDERLINE STREAM a glmf.DESCRIPTION curAmt[1] curAmt[2] ytdAmt[1] ytdAmt[2]
                       WITH FRAME frm-rpt.
           DISPLAY  STREAM a "DEPARTMENT TOTALS" @ glmf.DESCRIPTION DtAmt[1]  @ curAmt[1] 
                         DtAmt[2] @ curAmt[2] DtAmt[3]  @ ytdAmt[1] DtAmt[4]  @ ytdAmt[2] 
                       WITH FRAME frm-rpt.
           DOWN 2 STREAM a WITH FRAME frm-rpt.
        END.
    END. /* each gldept */
   UNDERLINE STREAM a glmf.DESCRIPTION curAmt[1] curAmt[2] ytdAmt[1] ytdAmt[2]
         WITH FRAME frm-rpt.
   DISPLAY STREAM a "GRAND TOTALS" @ glmf.DESCRIPTION
                        curAmt[3] @ curAmt[1] curAmt[4] @ curAmt[2]
                        ytdAmt[3] @ ytdAmt[1] ytdAmt[4] @ ytdAmt[2]  WITH FRAME frm-rpt.
   DOWN STREAM a WITH FRAME frm-rpt.
   DISPLAY STREAM a "DIFF" @  glmf.DESCRIPTION (curAmt[3] + curAmt[4]) @ curAmt[2]
                                               (ytdAmt[3] + ytdAmt[4]) @ ytdAmt[2] WITH FRAME frm-rpt.
   DOWN STREAM a WITH FRAME frm-rpt.
   RETURN.
END.


PROCEDURE Exc1.ip:
    /* HIDE FRAME frm-main. */
    ASSIGN wsAmt = 0
           curAmt = 0
           ytdAmt = 0.
    EXPORT STREAM B DELIMITER ','  "TRIAL BALANCE LEDGER REPORT FOR THE PERIOD:" + STRING(wsYear) + STRING(wsMonth,"99").
    EXPORT STREAM b  DELIMITER ','   "LEDGER" "NAME" ""   ""  "" "" "DEBITS" "CREDITS".
   FOR EACH glmf WHERE glmf.acct >= wsStart AND glmf.acct <= wsEnd NO-LOCK BY glmf.acct:
       ASSIGN wsAmt[1] = 0
              curAmt[1] = 0
              curAmt[2] = 0
              ytdAmt[1] = 0
              ytdAmt[2] = 0.
       FIND FIRST glbal WHERE glbal.acct = glmf.acct AND glbal.YEAR = wsYear NO-ERROR.
       IF NOT AVAILABLE glbal THEN  NEXT.
       ELSE IF AVAILABLE glbal THEN DO:
           ASSIGN wsAmt[1] = glbal.amt[wsMonth]
                  wsAmt[2] = bfbal.
           DO X = 1 TO wsMonth:
               ASSIGN wsAmt[2] = wsAmt[2] + glbal.amt[X].
           END.
           ASSIGN curAmt[1] = wsAmt[1] WHEN wsAmt[1] >= 0
                  curAmt[2] = wsAmt[1] WHEN wsAmt[1] < 0
                  curAmt[3] = curAmt[3] + wsAmt[1] WHEN wsAmt[1] >= 0
                  curAmt[4] = curAmt[4] + wsAmt[1] WHEN wsAmt[1] < 0
                  ytdAmt[1] = wsAmt[2] WHEN wsAmt[2] >= 0
                  ytdAmt[2] = wsAmt[2] WHEN wsAmt[2] < 0
                  ytdAmt[3] = ytdAmt[3] + wsAmt[2] WHEN wsAmt[2] >= 0
                  ytdAmt[4] = ytdAmt[4] + wsAmt[2] WHEN wsAmt[2] < 0.
          IF ytdAmt[1] <> 0 OR ytdAmt[2] <> 0 THEN DO:
             EXPORT STREAM b  DELIMITER ','
                                  glmf.acct UPPER(glmf.DESCRIPTION)   ""  "" ""  "" ytdAmt[1] ytdAmt[2] SKIP.
          END.
           
       END.
        LAmnt = 0.
                   /*EXPORT STREAM b DELIMITER ',' "" "" "PERIOD" "DATE"  "REFERENCE" "DESCRIPTION" "AMOUNT".*/
                   FOR EACH gltdf WHERE  integer(SUBstring(STRING(gltdf.period),1,4)) = wsYear
                         AND gltdf.acct >= wsStart AND gltdf.acct <= wsEnd  AND gltdf.acct = glmf.acct NO-LOCK
                          BREAK BY gltdf.acct:
                        EXPORT STREAM b DELIMITER ','
                            gltdf.acct gltdf.period gltdf.trdate   gltdf.ref UPPER(gltdf.descrip) gltdf.amt.
                        LAmnt = LAmnt + gltdf.amt.
                   END.
                        
   END.
   EXPORT  STREAM b DELIMITER ',' "GRAND TOTALS"    "" "" "" "" "" ytdAmt[3]  ytdAmt[4].
    EXPORT  STREAM b DELIMITER ',' "DIFF"   ""  "" "" "" "" " " (ytdAmt[3] + ytdAmt[4]).
    RETURN.
END.

PROCEDURE Exc2.ip:
    ASSIGN ytdAmt = 0
           curAmt = 0.
     EXPORT STREAM B DELIMITER ','  "TRIAL BALANCE LEDGER REPORT FOR THE PERIOD:" + STRING(wsYear) + STRING(wsMonth,"99").
        EXPORT STREAM b  DELIMITER ','   "LEDGER" "NAME" ""   ""  "" "" "DEBITS" "CREDITS".
     FOR EACH glproj WHERE glproj.proj >= st-proj AND glproj.proj <= end-proj BY proj:
        IF CAN-FIND (FIRST glPbal WHERE glPbal.proj = glproj.proj AND glPbal.YEAR = wsYear)  THEN DO:
            EXPORT  STREAM b DELIMITER ',' UPPER(glproj.descrip).
             
            ASSIGN wsAmt = 0
                   pTAmt = 0.
        FOR EACH glPbal WHERE glPbal.proj = glproj.proj AND glPbal.YEAR = wsYear
                             AND glPbal.acct >= wsStart AND glPbal.acct <= wsEnd NO-LOCK 
                              BREAK BY glPbal.acct:
               IF FIRST-OF(glPbal.acct) THEN DO:
                   FIND FIRST glmf WHERE glmf.acct = glPbal.acct NO-LOCK NO-ERROR.
                   IF NOT AVAILABLE glmf THEN NEXT.
                   ELSE 
                       ASSIGN wsAmt[1] = 0
                              curAmt[1] = 0
                              curAmt[2] = 0
                              ytdAmt[1] = 0
                              ytdAmt[2] = 0.
               END.
               ASSIGN wsAmt[1] = glPbal.amt[wsMonth]
                          wsAmt[2] = glPbal.bfbal.
                   DO X = 1 TO wsMonth:
                       ASSIGN wsAmt[2] = wsAmt[2] + glPbal.amt[X].
                   END.
                   ASSIGN curAmt[1] = wsAmt[1] WHEN wsAmt[1] >= 0
                          curAmt[2] = wsAmt[1] WHEN wsAmt[1] < 0
                          curAmt[3] = curAmt[3] + wsAmt[1] WHEN wsAmt[1] >= 0
                          curAmt[4] = curAmt[4] + wsAmt[1] WHEN wsAmt[1] < 0
                          ytdAmt[1] = wsAmt[2] WHEN wsAmt[2] >= 0
                          ytdAmt[2] = wsAmt[2] WHEN wsAmt[2] < 0
                          ytdAmt[3] = ytdAmt[3] + wsAmt[2] WHEN wsAmt[2] >= 0
                          ytdAmt[4] = ytdAmt[4] + wsAmt[2] WHEN wsAmt[2] < 0
                          pTAmt[1] = pTAmt[1] + wsAmt[1] WHEN wsAmt[2] >= 0
                          pTAmt[2] = pTAmt[2] + wsAmt[1] WHEN wsAmt[2] < 0
                          pTAmt[3] = pTAmt[3] + wsAmt[2] WHEN wsAmt[2] >= 0
                          pTAmt[4] = pTAmt[4] + wsAmt[2] WHEN wsAmt[2] < 0.
                   IF LAST-OF(glPbal.acct) THEN DO:
                       IF ytdAmt[1] <> 0 OR ytdAmt[2] <> 0 THEN DO:
                           EXPORT STREAM b  DELIMITER ','
                                  glmf.acct UPPER(glmf.DESCRIPTION)   ""  "" ""  "" ytdAmt[1] ytdAmt[2] SKIP.
                       END.
                   END.
                   LAmnt = 0.
                   /*EXPORT STREAM b DELIMITER ',' "" "" "" "PERIOD" "DATE" "SOURCE"  "REFERENCE" "DESCRIPTION" "AMOUNT".*/
                   FOR EACH gltdf WHERE gltdf.proj = glpbal.proj AND integer(SUBstring(STRING(gltdf.period),1,4)) = wsYear
                         AND gltdf.acct >= wsStart AND gltdf.acct <= wsEnd  AND gltdf.acct = glpbal.acct NO-LOCK
                          BREAK BY gltdf.acct:
                        EXPORT STREAM b DELIMITER ','
                            glmf.acct gltdf.period gltdf.trdate  gltdf.ref UPPER(gltdf.descrip) gltdf.amt.
                        LAmnt = LAmnt + gltdf.amt.
                   END.
                  /* EXPORT STREAM b DELIMITER ',' "" "LEDGER TOTALS" "" "" "" ""  "" "" LAmnt.*/
           END. /* each glPbal */
          /* EXPORT  STREAM b DELIMITER ',' "SEGMENT TOTALS" "" ""  "" "" ""  pTAmt[3] pTAmt[4] .*/
        END.
    END. /* each glproj */
    EXPORT  STREAM b DELIMITER ',' "GRAND TOTALS"    "" "" "" "" "" ytdAmt[3]  ytdAmt[4].
    EXPORT  STREAM b DELIMITER ',' "DIFF"   ""  "" "" "" "" " " (ytdAmt[3] + ytdAmt[4]).
    RETURN.
END.

PROCEDURE Exc3.ip:
    ASSIGN ytdAmt = 0
           curAmt = 0.
   EXPORT STREAM B DELIMITER ','  "TRIAL BALANCE LEDGER REPORT FOR THE PERIOD:" + STRING(wsYear) + STRING(wsMonth,"99").
    EXPORT STREAM b  DELIMITER ','   "LEDGER" "NAME" ""   ""  "" "" "DEBITS" "CREDITS".
    FOR EACH glfund WHERE glfund.fund >= st-fund AND glfund.fund <= end-fund BY glfund.fund:
        IF CAN-FIND (FIRST glFbal WHERE glFbal.fund = glfund.fund AND glFbal.YEAR = wsYear)  THEN DO:
            EXPORT  STREAM b DELIMITER ',' UPPER(glfund.descrip).
             
            ASSIGN wsAmt = 0
                   DTAmt = 0.
           FOR EACH glFbal WHERE glFbal.fund = glfund.fund AND glFbal.YEAR = wsYear
                             AND glFbal.acct >= wsStart AND glFbal.acct <= wsEnd NO-LOCK 
                              BREAK BY glFbal.acct:
               IF FIRST-OF(glFbal.acct) THEN DO:
                  FIND FIRST glmf WHERE glmf.acct = glFbal.acct NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE glmf THEN NEXT.
                  ELSE
                       ASSIGN wsAmt[1] = 0
                              curAmt[1] = 0
                              curAmt[2] = 0
                              ytdAmt[1] = 0
                              ytdAmt[2] = 0.
               END.
               ASSIGN wsAmt[1] = glFbal.amt[wsMonth]
                      wsAmt[2] = glFbal.bfbal.
               DO X = 1 TO wsMonth:
                     ASSIGN wsAmt[2] = wsAmt[2] + glFbal.amt[X].
               END.
               ASSIGN curAmt[1] = wsAmt[1] WHEN wsAmt[1] >= 0
                          curAmt[2] = wsAmt[1] WHEN wsAmt[1] < 0
                          curAmt[3] = curAmt[3] + wsAmt[1] WHEN wsAmt[1] >= 0
                          curAmt[4] = curAmt[4] + wsAmt[1] WHEN wsAmt[1] < 0
                          ytdAmt[1] = wsAmt[2] WHEN wsAmt[2] >= 0
                          ytdAmt[2] = wsAmt[2] WHEN wsAmt[2] < 0
                          ytdAmt[3] = ytdAmt[3] + wsAmt[2] WHEN wsAmt[2] >= 0
                          ytdAmt[4] = ytdAmt[4] + wsAmt[2] WHEN wsAmt[2] < 0
                          DTAmt[1] = DTAmt[1] + wsAmt[1] WHEN wsAmt[2] >= 0
                          DTAmt[2] = DTAmt[2] + wsAmt[1] WHEN wsAmt[2] < 0
                          DTAmt[3] = DTAmt[3] + wsAmt[2] WHEN wsAmt[2] >= 0
                          DTAmt[4] = DTAmt[4] + wsAmt[2] WHEN wsAmt[2] < 0.
                   IF LAST-OF(glFbal.acct) THEN DO: 
                       IF ytdAmt[1] <> 0 OR ytdAmt[2] <> 0 THEN DO:
                            EXPORT STREAM b  DELIMITER ','
                                  glmf.acct UPPER(glmf.DESCRIPTION)   ""  "" ""  "" ytdAmt[1] ytdAmt[2] SKIP.
                       END.
                   END.
                   LAmnt = 0.
                   /*EXPORT STREAM b DELIMITER ',' "" "" "" "PERIOD" "DATE" "SOURCE"  "REFERENCE" "DESCRIPTION" "AMOUNT".*/
                   FOR EACH gltdf WHERE gltdf.fund = glfbal.fund AND integer(SUBstring(STRING(gltdf.period),1,4)) = wsYear
                         AND gltdf.acct >= wsStart AND gltdf.acct <= wsEnd  AND gltdf.acct = glfbal.acct NO-LOCK
                          BREAK BY gltdf.acct:
                        EXPORT STREAM b DELIMITER ','
                            glmf.acct gltdf.period gltdf.trdate  gltdf.ref UPPER(gltdf.descrip) gltdf.amt.
                        LAmnt = LAmnt + gltdf.amt.
                   END.
                  /* EXPORT STREAM b DELIMITER ',' "" "LEDGER TOTALS" "" "" "" ""  "" "" LAmnt.*/
                    
           END. /* each glFbal */
          /* EXPORT  STREAM b DELIMITER ',' "SEGMENT TOTALS" "" ""  "" "" ""  DtAmt[3] DtAmt[4].*/ 
        END.
    END. /* each glFund */
   EXPORT  STREAM b DELIMITER ',' "GRAND TOTALS"    "" "" "" "" "" ytdAmt[3]  ytdAmt[4].
    EXPORT  STREAM b DELIMITER ',' "DIFF"   ""  "" "" "" "" " " (ytdAmt[3] + ytdAmt[4]).
    RETURN.
END.

PROCEDURE Exc4.ip:
    ASSIGN ytdAmt = 0
           curAmt = 0.
    EXPORT STREAM B DELIMITER ','  "TRIAL BALANCE LEDGER REPORT FOR THE PERIOD:" + STRING(wsYear) + STRING(wsMonth,"99").
    EXPORT STREAM b  DELIMITER ','   "LEDGER" "NAME" ""   ""  "" "" "DEBITS" "CREDITS".
    FOR EACH gldept WHERE gldept.dept >= st-dept AND gldept.dept <= end-dept BY dept:
        IF CAN-FIND (FIRST gldbal WHERE gldbal.dept = gldept.dept AND gldbal.YEAR = wsYear)  THEN DO:
            EXPORT  STREAM b DELIMITER ',' UPPER(gldept.descrip).
             
            ASSIGN wsAmt = 0
                   DTAmt = 0.
           FOR EACH gldBal WHERE gldbal.dept = gldept.dept AND gldbal.YEAR = wsYear
                             AND gldBal.acct >= wsStart AND gldBal.acct <= wsEnd NO-LOCK 
                              BREAK BY gldBal.acct:
               IF FIRST-OF(gldBal.acct) THEN DO:
                  FIND FIRST glmf WHERE glmf.acct = gldbal.acct NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE glmf THEN NEXT.
                  ELSE
                       ASSIGN wsAmt[1] = 0
                              curAmt[1] = 0
                              curAmt[2] = 0
                              ytdAmt[1] = 0
                              ytdAmt[2] = 0.
               END.
               ASSIGN wsAmt[1] = gldbal.amt[wsMonth]
                      wsAmt[2] = gldbal.bfbal.
               DO X = 1 TO wsMonth:
                     ASSIGN wsAmt[2] = wsAmt[2] + gldbal.amt[X].
               END.
               ASSIGN curAmt[1] = wsAmt[1] WHEN wsAmt[1] >= 0
                          curAmt[2] = wsAmt[1] WHEN wsAmt[1] < 0
                          curAmt[3] = curAmt[3] + wsAmt[1] WHEN wsAmt[1] >= 0
                          curAmt[4] = curAmt[4] + wsAmt[1] WHEN wsAmt[1] < 0
                          ytdAmt[1] = wsAmt[2] WHEN wsAmt[2] >= 0
                          ytdAmt[2] = wsAmt[2] WHEN wsAmt[2] < 0
                          ytdAmt[3] = ytdAmt[3] + wsAmt[2] WHEN wsAmt[2] >= 0
                          ytdAmt[4] = ytdAmt[4] + wsAmt[2] WHEN wsAmt[2] < 0
                          DTAmt[1] = DTAmt[1] + wsAmt[1] WHEN wsAmt[2] >= 0
                          DTAmt[2] = DTAmt[2] + wsAmt[1] WHEN wsAmt[2] < 0
                          DTAmt[3] = DTAmt[3] + wsAmt[2] WHEN wsAmt[2] >= 0
                          DTAmt[4] = DTAmt[4] + wsAmt[2] WHEN wsAmt[2] < 0.
                   IF LAST-OF(gldBal.acct) THEN DO: 
                       IF ytdAmt[1] <> 0 OR ytdAmt[2] <> 0 THEN DO:
                            EXPORT STREAM b  DELIMITER ','
                                  glmf.acct UPPER(glmf.DESCRIPTION)   ""  "" ""  "" ytdAmt[1] ytdAmt[2] SKIP.
                       END.
                   END.
                   LAmnt = 0.
                   /*EXPORT STREAM b DELIMITER ',' "" "" "" "PERIOD" "DATE" "SOURCE"  "REFERENCE" "DESCRIPTION" "AMOUNT".*/
                   FOR EACH gltdf WHERE gltdf.dept = gldbal.dept AND integer(SUBstring(STRING(gltdf.period),1,4)) = wsYear
                         AND gltdf.acct >= wsStart AND gltdf.acct <= wsEnd  AND gltdf.acct = gldbal.acct NO-LOCK
                          BREAK BY gltdf.acct:
                        EXPORT STREAM b DELIMITER ','
                            glmf.acct gltdf.period gltdf.trdate  gltdf.ref UPPER(gltdf.descrip) gltdf.amt.
                        LAmnt = LAmnt + gltdf.amt.
                   END.
                   /*EXPORT STREAM b DELIMITER ',' "" "LEDGER TOTALS" "" "" "" ""  "" "" LAmnt.*/
                    
           END. /* each glDBal */
            /*EXPORT  STREAM b DELIMITER ',' "SEGMENT TOTALS" "" ""  "" "" ""  DtAmt[3] DtAmt[4].*/
        END.
    END. /* each gldept */
    EXPORT  STREAM b DELIMITER ',' "GRAND TOTALS"    "" "" "" "" "" ytdAmt[3]  ytdAmt[4].
    EXPORT  STREAM b DELIMITER ',' "DIFF"   ""  "" "" "" "" " " (ytdAmt[3] + ytdAmt[4]).
    RETURN.
END.
