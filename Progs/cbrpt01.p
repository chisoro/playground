/* Program.................cbrpt02.p
   Notes:...... Cash Movement Management Report
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF STREAM b.
DEF VAR w-orientation AS CHAR      INITIAL /*"PORTRAIT"*/ "landscape"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10          NO-UNDO.
DEF VAR wsAmt  LIKE CBTrans.amount.
DEF VAR wsTAmt LIKE wsAmt.
DEF VAR wsBudg LIKE wsAmt.
DEF VAR wsPerc LIKE wsAmt.
DEF VAR wsCr LIKE wsAmt.
DEF VAR wsDr LIKE wsAmt.
DEF VAR wsAnt AS INT.
DEF VAR wsP AS INT.
DEF VAR st-per LIKE gltdf.period.
DEF VAR end-per LIKE gltdf.period.
DEF VAR wsper  LIKE gltdf.period. 
DEF VAR wsStatus LIKE gltdf.acct.
DEF VAR varDescr LIKE glvote.descrip FORM "X(60)".
DEF VAR wsBudget LIKE wsAmt.
DEF VAR wsTBudget LIKE wsAmt.
DEF VAR wsVar  AS DEC FORM "zz9.99-".
DEF VAR wsYear AS INT FORM "9999".
DEF VAR wsFile AS CHAR FORM "x(40)".
DEF VAR wsNar  AS CHAR FORM "x(40)".
DEF VAR wsOpt AS INT VIEW-AS RADIO-SET RADIO-BUTTONS 
    "Report By Program ", 1, "Report by Fund/Segment", 2, "Report by Department", 3.
DEF VAR wsProg AS CHAR FORM "x(20)".
DEF VAR X AS INT.

DEFINE  TEMP-TABLE tmpTRANS
    FIELD wsProj    LIKE cbtrans.Proj
    FIELD wsfund    LIKE cbtrans.fund
    FIELD wsdept    LIKE cbtrans.dept
    FIELD wsclas    AS INT
    /*FIELD cat       AS INT */
    FIELD cat    LIKE glcat.cat
    FIELD acct      LIKE CBTrans.ledger
    FIELD amount    LIKE CBTrans.amount
    FIELD TAmount   LIKE CBTrans.amount
    FIELD Budg      LIKE CBTrans.amount
    FIELD period    LIKE CBTrans.Accper.
DEFINE  TEMP-TABLE tmpTr
    FIELD wsProj    LIKE cbtrans.Proj
    FIELD wsfund    LIKE cbtrans.fund
    FIELD wsdept    LIKE cbtrans.dept
    FIELD wsclas    AS INT
    /*FIELD cat       AS INT */
    FIELD cat    LIKE glcat.cat
    FIELD acct      LIKE CBTrans.ledger
    FIELD amount    LIKE CBTrans.amount
    FIELD TAmount   LIKE CBTrans.amount
    FIELD Budg      LIKE CBTrans.amount
    FIELD period    LIKE CBTrans.Accper.

DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON btn-ok     LABEL "Print".
DEF BUTTON btn-Export LABEL "Export".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 8.5.

DEFINE FRAME frm-main
    SKIP(2)
    st-per    COLON 30 LABEL "START Accounting Period" SKIP(.5)
    end-per   COLON 30 LABEL "END Accounting Period" SKIP(.5)
    wsOpt     COLON 30 LABEL "Select Report" SKIP(.5)
    wsStatus  COLON 20 LABEL "Processing......" view-as text no-tab-stop
    skip(1.0)
    btn-ok     AT ROW 10.5 COL 10
    SPACE(25)  btn-Export 
    SPACE(25)  btn-close 
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 10 COL 3
    with view-as dialog-box keep-tab-order no-validate
         side-labels no-underline three-d SCROLLABLE CENTERED
    TITLE "CASH MOVEMENT MANAGEMENT REPORT".

FIND FIRST simctr NO-LOCK NO-ERROR.

FORM
    varDescr                                   FORM "X(50)"
    tmpTrans.Amount      LABEL "AMOUNT"        FORM "zzzzzzzzz9.99-"
    tmpTrans.TAmount     LABEL "TO-DATE MOUNT" FORM "zzzzzzzzz9.99-"
    tmpTrans.Budg        LABEL "BUDGET"        FORM "zzzzzzzzz9.99-"
    wsP                  LABEL "%TO-DATE"      FORM "zz9.99-"
    HEADER simctr.coname AT 30
    skip(1) "CASH MOVEMENT MANAGEMENT REPORT FOR THE PERIOD:" AT 5 SPACE(2)
    STRING(st-Per) SPACE(1) "TO" SPACE(1) STRING(end-Per) "Page: " AT 80 TRIM(STRING(PAGE-NUMBER(a)))
    SKIP(1) WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX WIDTH 132 FRAME a.

 FORM
    st-Per         LABEL "PERIOD" 
    cbTrans.trdate LABEL "DATE " SPACE(4)
    cbTrans.ref    LABEL "REFERENCE" 
    wsNar          LABEL "DESCRIPTION" 
    cbTrans.Ledger LABEL "ALLOCATION"
    cbTrans.amount NO-LABEL
    wsAmt          LABEL "AMOUNT" 
    cbkmf.txtCur   LABEL "CUR"
    HEADER skip(3) "PAYMENT DETAILS REPORT " "Page: " AT 80 PAGE-NUMBER(a)
     SKIP(2)
    WITH NO-LABEL DOWN STREAM-IO FONT 10 WIDTH 132 CENTERED NO-BOX FRAME frm-rpt.


ON CHOOSE OF btn-ok IN FRAME frm-main DO:
     session:set-wait-state("").
     ASSIGN st-per = INT(st-per:SCREEN-VALUE)
            end-per = INT(end-per:SCREEN-VALUE)
            wsOpt   = INT(wsOpt:SCREEN-VALUE).
     RUN CreData.ip.
     IF wsopt = 1 THEN
        wsProg = "ProjRep1.ip".
     ELSE IF wsopt = 2 THEN
        wsProg = "FundRep1.ip".
     ELSE IF wsopt = 3 THEN
        wsProg = "deptRep1.ip".
     {PrintOpt.i &stream-name="stream a"
                        &print-prog = VALUE(wsProg)
                        &pgorientation = w-orientation
                        &paged} 
    MESSAGE ".....PROCESSING COMPLETED....." VIEW-AS ALERT-BOX.
END.

ON CHOOSE OF btn-Export IN FRAME frm-main DO:
     session:set-wait-state("").
     OUTPUT STREAM b TO VALUE(wsFile).
     ASSIGN st-per = INT(st-per:SCREEN-VALUE)
            end-per = INT(end-per:SCREEN-VALUE)
            wsOpt   = INT(wsOpt:SCREEN-VALUE).
     RUN CreData.ip.
     IF wsopt = 1 THEN
        RUN projRep.ip.
     ELSE IF wsopt = 2 THEN
        RUN fundRep.ip.
     ELSE IF wsopt = 3 THEN
        RUN deptRep.ip.
     wsfile = "START " + wsFile.
     OUTPUT STREAM b CLOSE.
     OS-COMMAND NO-WAIT VALUE(wsFile).
END.

/********** MAIN LOGIC **********/

ENABLE ALL WITH FRAME frm-main.
ASSIGN st-per:SCREEN-VALUE = STRING(SIMCTR.CURPER)
       end-per:SCREEN-VALUE = STRING(SIMCTR.CURPER)
       wsFile = simctr.repDir + "cbrpt01" 
          + string(day(today),"99")    + "_"
          + string(month(today),"99")  + "_"
          + string(year(today),"9999") + "_"
          + substr(string(time,"HH:MM:SS"),1,2)
          + substr(string(time,"HH:MM:SS"),4,2)
          + substr(string(time,"HH:MM:SS"),7,2)
          + ".csv".
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.

PROCEDURE CreData.ip:
    FOR EACH tmptrans.
        DELETE tmpTrans.
    END.
    FOR EACH tmpTr.
        DELETE tmpTr.
    END.
    /* Create Temp-table */
    wsper = INT(SUBSTR(STRING(end-per),1,4) + "01").
    FOR EACH CBTrans WHERE cbtrans.seq <> 0 AND  CBTrans.Accper >= wsper
                        AND CBTrans.Accper  <= end-per NO-LOCK:
        FIND FIRST tmpTrans WHERE tmpTrans.acct = cbtrans.ledger
                              AND tmptrans.wsProj = cbtrans.Proj
                              AND tmptrans.wsfund = cbtrans.fund
                              AND tmptrans.wsdept = cbtrans.dept NO-ERROR.
        IF NOT AVAILABLE tmpTrans THEN DO:
            CREATE tmpTrans.
            ASSIGN tmpTrans.acct   = CBTrans.ledger
                   tmptrans.wsProj = cbtrans.Proj
                   tmptrans.wsfund = cbtrans.fund
                   tmptrans.wsdept = cbtrans.dept.
        END.
       ASSIGN tmpTrans.amount = tmpTrans.amount + (CBTrans.amount * CBTrans.decRate)   WHEN CBTrans.Accper >= st-per
              tmpTrans.Tamount = tmpTrans.Tamount + (CBTrans.amount * CBTrans.decRate).
      FIND FIRST tmpTr WHERE tmpTr.acct = cbtrans.ledger
                              AND tmpTr.wsProj = cbtrans.Proj
                              AND tmpTr.wsfund = cbtrans.fund
                              AND tmpTr.wsdept = cbtrans.dept NO-ERROR.
        IF NOT AVAILABLE tmpTr THEN DO:
            CREATE tmpTr.
            ASSIGN tmpTr.acct   = CBTrans.ledger
                   tmpTr.wsProj = cbtrans.Proj
                   tmpTr.wsfund = cbtrans.fund
                   tmpTr.wsdept = cbtrans.dept.
        END.
       ASSIGN tmpTr.amount = tmpTr.amount + CBTrans.amount    WHEN CBTrans.Accper >= st-per
              tmpTr.Tamount = tmpTr.Tamount + CBTrans.amount.
    END.
    FOR EACH tmpTrans.
        IF tmpTrans.TAmount = 0 THEN
            DELETE tmpTrans.
        ELSE DO:
            FIND FIRST glmf WHERE glmf.acct = tmpTrans.acct NO-LOCK NO-ERROR.
            IF AVAILABLE glmf THEN
                ASSIGN tmpTrans.cat = glmf.cat
                       tmpTrans.wsclas = glmf.clas WHEN (glmf.clas = 1 OR glmf.clas = 2)
                       tmpTrans.wsclas = 1 WHEN tmpTrans.Tamount > 0 
                       tmpTrans.wsclas = 2 WHEN tmpTrans.Tamount < 0.
            IF NOT AVAILABLE glmf THEN
                   ASSIGN tmpTrans.wsclas = 1 WHEN tmpTrans.Tamount > 0 
                          tmpTrans.wsclas = 2 WHEN tmpTrans.Tamount < 0.
        END. 
    END.
    FOR EACH tmpTr.
            IF tmpTr.TAmount = 0 THEN
                DELETE tmpTr.
            ELSE DO:
                FIND FIRST glmf WHERE glmf.acct = tmpTr.acct NO-LOCK NO-ERROR.
                IF AVAILABLE glmf THEN
                    ASSIGN tmpTr.cat = glmf.cat
                           tmpTr.wsclas = glmf.clas WHEN (glmf.clas = 1 OR glmf.clas = 2)
                           tmpTr.wsclas = 1 WHEN tmpTr.Tamount > 0 
                           tmpTr.wsclas = 2 WHEN tmpTr.Tamount < 0.
                IF NOT AVAILABLE glmf THEN
                       ASSIGN tmpTr.wsclas = 1 WHEN tmpTr.Tamount > 0 
                              tmpTr.wsclas = 2 WHEN tmpTr.Tamount < 0.
            END. 
        END.
    RETURN.
END. /*OFP CreData.ip */


PROCEDURE ProjRep.ip:
    FOR EACH tmpTrans:
        tmpTrans.Budg = 0.
       FOR EACH glpbal WHERE glpbal.YEAR = INT(SUBSTR(STRING(end-per),1,4))
                         AND glpbal.acct = tmpTrans.acct 
                         AND glpbal.proj = tmptrans.wsproj NO-LOCK:
           tmpTrans.Budg = tmpTrans.Budg + glpbal.bugTotal.
       END.
    END.
    /* Print Report */
    ASSIGN wsAmt = 0
           wsTAmt = 0.
    EXPORT STREAM b DELIMITER ',' "CASH MOVEMENT MANAGEMENT REPORT FOR THE PERIOD:" 
                                   + STRING(st-per) + " TO " + STRING(end-Per).
    EXPORT STREAM b DELIMITER ',' " " "CURRENT PERIOD" "TO DATE" "" "TO DATE %".
    EXPORT STREAM b DELIMITER ',' " " "ACTUAL" "ACTUAL" "BUDGETTED" "OF BUDGET". 
    FOR EACH tmpTrans  NO-LOCK BREAK  BY wsClas BY wsProj:
        ACCUMULATE tmpTrans.Amount (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Tamount (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Budg   (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Amount (TOTAL BY tmpTrans.wsProj).
        ACCUMULATE tmpTrans.Tamount (TOTAL BY tmpTrans.wsProj).
        ACCUMULATE tmpTrans.Budg   (TOTAL BY tmpTrans.wsProj).
        ASSIGN wsAmt  = wsAmt  + tmpTrans.Amount
               wsTAmt = wsTAmt + tmpTrans.Tamount.
        IF FIRST-OF(wsclas) AND tmpTrans.Tamount <> 0 THEN DO:
           IF  wsClas = 1 THEN
               EXPORT STREAM b DELIMITER ',' "IN-FLOWS".
           ELSE 
               EXPORT STREAM b DELIMITER ',' "OUT-FLOWS".
       END.
        IF FIRST-OF(wsclas) AND tmpTrans.Tamount <> 0 THEN DO:
            
            EXPORT STREAM b DELIMITER ',' .
        END.
        IF LAST-OF(wsProj) THEN DO:
            FIND FIRST glProj WHERE glProj.proj = tmpTrans.wsProj NO-ERROR.
            wsPerc = ROUND(((ACCUM SUB-TOTAL BY tmpTrans.wsProj tmpTrans.Tamount) / 
                           (ACCUM SUB-TOTAL BY tmpTrans.wsProj tmpTrans.Budg) * 100),3).
            IF wsPerc < 0 THEN
               wsPerc = wsPerc * -1.
            EXPORT STREAM b DELIMITER ',' "     " + glProj.descrip
                           (ACCUM SUB-TOTAL BY tmpTrans.wsProj tmpTrans.Amount)
                           (ACCUM SUB-TOTAL BY tmpTrans.wsProj tmpTrans.Tamount) 
                           (ACCUM SUB-TOTAL BY tmpTrans.wsProj tmpTrans.Budg)
                            wsPerc.               
        END. 
       IF LAST-OF(wsclas) THEN DO:
              EXPORT STREAM b DELIMITER ',' "SUB-TOTAL"
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Amount)
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Tamount) 
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Budg).
        EXPORT STREAM b DELIMITER ',' "".
       END.
    END.
    EXPORT STREAM b DELIMITER ',' "NET FLOW" wsAmt wsTAmt.
    EXPORT STREAM b DELIMITER ',' "".
    EXPORT STREAM b DELIMITER ',' "".
    EXPORT STREAM b DELIMITER ',' "CASH MOVEMENT MANAGEMENT DETAILED REPORT FOR THE PERIOD:" 
                                   + STRING(st-per) + " TO " + STRING(end-Per).
    EXPORT STREAM b DELIMITER ',' " " "CURRENT PERIOD" "TO DATE" "" "TO DATE %".
    EXPORT STREAM b DELIMITER ',' " " "ACTUAL" "ACTUAL" "BUDGETTED" "OF BUDGET". 
    ASSIGN wsAmt = 0
           wsTAmt = 0.
    FOR EACH tmpTrans NO-LOCK BREAK BY tmpTrans.wsClas BY tmpTrans.Acct:
        ACCUMULATE tmpTrans.Amount (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Tamount (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Budg   (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Amount (TOTAL BY tmpTrans.Acct).
        ACCUMULATE tmpTrans.Tamount (TOTAL BY tmpTrans.Acct).
        ACCUMULATE tmpTrans.Budg   (TOTAL BY tmpTrans.Acct).
        ASSIGN wsAmt  = wsAmt  + tmpTrans.Amount
               wsTAmt = wsTAmt + tmpTrans.Tamount.
        IF FIRST-OF(wsclas) AND tmpTrans.Tamount <> 0 THEN DO:
           IF  wsClas = 1 THEN
               EXPORT STREAM b DELIMITER ',' "IN-FLOWS".
           ELSE 
               EXPORT STREAM b DELIMITER ',' "OUT-FLOWS".
       END.
        IF FIRST-OF(wsclas) AND tmpTrans.Tamount <> 0 THEN DO:    
            EXPORT STREAM b DELIMITER ',' .
        END.
        IF LAST-OF(tmpTrans.Acct) THEN DO:
            FIND FIRST glmf WHERE glmf.Acct = tmpTrans.Acct NO-ERROR.
            IF AVAILABLE glmf THEN
                varDescr = glmf.DESCRIPTION.
            ELSE
                varDescr = STRING(tmpTrans.Acct).
            wsPerc = ROUND(((ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Tamount) / 
                           (ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Budg) * 100),3).
            IF wsPerc < 0 THEN
               wsPerc = wsPerc * -1.
            EXPORT STREAM b DELIMITER ',' "     " + varDescr
                           (ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Amount)
                           (ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Tamount) 
                           (ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Budg)
                            wsPerc.               
        END. 
        IF LAST-OF(tmpTrans.wsclas) THEN DO:
              EXPORT STREAM b DELIMITER ',' "SUB-TOTAL"
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Amount)
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Tamount) 
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Budg).
              EXPORT STREAM b DELIMITER ',' "".
        END.
    END.
    EXPORT STREAM b DELIMITER ',' "NET FLOW" wsAmt wsTAmt.
    EXPORT STREAM b DELIMITER ',' .
    RUN ExcDetail.ip.
END. /*OFP ProjRep.ip */

PROCEDURE ProjRep1.ip:
    FOR EACH tmpTrans:
        tmpTrans.Budg = 0.
       FOR EACH glpbal WHERE glpbal.YEAR = INT(SUBSTR(STRING(end-per),1,4))
                         AND glpbal.acct = tmpTrans.acct 
                         AND glpbal.proj = tmptrans.wsproj NO-LOCK:
           tmpTrans.Budg = tmpTrans.Budg + glpbal.bugTotal.
       END.
    END.
    /* Print Report */
    ASSIGN wsAmt = 0
           wsTAmt = 0.
    FOR EACH tmpTrans  NO-LOCK BREAK  BY wsClas BY wsProj:
        ACCUMULATE tmpTrans.Amount (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Tamount (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Budg   (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Amount (TOTAL BY tmpTrans.wsProj).
        ACCUMULATE tmpTrans.Tamount (TOTAL BY tmpTrans.wsProj).
        ACCUMULATE tmpTrans.Budg   (TOTAL BY tmpTrans.wsProj).
        ASSIGN wsAmt  = wsAmt  + tmpTrans.Amount
               wsTAmt = wsTAmt + tmpTrans.Tamount.
        IF FIRST-OF(wsclas) AND tmpTrans.Tamount <> 0 THEN DO:
           IF  wsClas = 1 THEN
               DISPLAY STREAM a "IN-FLOWS" @ varDescr WITH FRAME a.
           ELSE 
               DISPLAY STREAM a "OUT-FLOWS" @ varDescr WITH FRAME a.
          DOWN STREAM a WITH FRAME a.
       END.
        IF LAST-OF(wsProj) THEN DO:
            FIND FIRST glProj WHERE glProj.proj = tmpTrans.wsProj NO-ERROR.
            wsPerc = ROUND(((ACCUM SUB-TOTAL BY tmpTrans.wsProj tmpTrans.Tamount) / 
                           (ACCUM SUB-TOTAL BY tmpTrans.wsProj tmpTrans.Budg) * 100),3).
            IF wsPerc < 0 THEN
               wsPerc = wsPerc * -1.
             DISPLAY STREAM a "     " + glProj.descrip @ varDescr
                           (ACCUM SUB-TOTAL BY tmpTrans.wsProj tmpTrans.Amount)  @ tmpTrans.Amount
                           (ACCUM SUB-TOTAL BY tmpTrans.wsProj tmpTrans.Tamount) @ tmpTrans.Tamount
                           (ACCUM SUB-TOTAL BY tmpTrans.wsProj tmpTrans.Budg)    @ tmpTrans.Budg
                            wsPerc @ wsP WITH FRAME a.
              DOWN STREAM a WITH FRAME a.
        END. 
       IF LAST-OF(wsclas) THEN DO:
              UNDERLINE STREAM a tmpTrans.Amount tmpTrans.Tamount  tmpTrans.Budg WITH FRAME a.
              DOWN STREAM a WITH FRAME a.
              DISPLAY STREAM a  "SUB-TOTAL" @ varDescr
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Amount)  @ tmpTrans.Amount
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Tamount) @ tmpTrans.Tamount 
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Budg)    @ tmpTrans.Budg WITH FRAME a.
              DOWN  2 STREAM a WITH FRAME a.
       END.
    END.
    UNDERLINE STREAM a tmpTrans.Amount tmpTrans.Tamount WITH FRAME a.
    DOWN STREAM a WITH FRAME a.
    DISPLAY STREAM a "NET FLOW" @ varDescr wsAmt @ tmpTrans.Amount wsTAmt @ tmpTrans.Tamount
         WITH FRAME a.
    DOWN  2 STREAM a WITH FRAME a.
    PAGE STREAM a.
    ASSIGN wsAmt = 0
           wsTAmt = 0.
    FOR EACH tmpTrans NO-LOCK BREAK BY tmpTrans.wsClas BY tmpTrans.Acct:
        ACCUMULATE tmpTrans.Amount (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Tamount (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Budg   (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Amount (TOTAL BY tmpTrans.Acct).
        ACCUMULATE tmpTrans.Tamount (TOTAL BY tmpTrans.Acct).
        ACCUMULATE tmpTrans.Budg   (TOTAL BY tmpTrans.Acct).
        ASSIGN wsAmt  = wsAmt  + tmpTrans.Amount
               wsTAmt = wsTAmt + tmpTrans.Tamount.
        IF FIRST-OF(wsclas) AND tmpTrans.Tamount <> 0 THEN DO:
           IF  wsClas = 1 THEN
               DISPLAY STREAM a "IN-FLOWS" @ varDescr WITH FRAME a.
           ELSE 
               DISPLAY STREAM a "OUT-FLOWS" @ varDescr WITH FRAME a.
          DOWN STREAM a WITH FRAME a.
       END.
        IF FIRST-OF(wsclas) AND tmpTrans.Tamount <> 0 THEN DO:    
            EXPORT STREAM b DELIMITER ',' .
        END.
        IF LAST-OF(tmpTrans.Acct) THEN DO:
            FIND FIRST glmf WHERE glmf.Acct = tmpTrans.Acct NO-ERROR.
            IF AVAILABLE glmf THEN
                varDescr = glmf.DESCRIPTION.
            ELSE
                varDescr = STRING(tmpTrans.Acct).
            wsPerc = ROUND(((ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Tamount) / 
                           (ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Budg) * 100),3).
            IF wsPerc < 0 THEN
               wsPerc = wsPerc * -1.
            DISPLAY STREAM a "     " + varDescr @ varDescr
                           (ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Amount)  @ tmpTrans.Amount
                           (ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Tamount) @ tmpTrans.Tamount
                           (ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Budg)    @ tmpTrans.Budg
                            wsPerc @ wsP WITH FRAME a.
            DOWN STREAM a WITH FRAME a.
        END. 
        IF LAST-OF(tmpTrans.wsclas) THEN DO:
             UNDERLINE STREAM a tmpTrans.Amount tmpTrans.Tamount  tmpTrans.Budg WITH FRAME a.
             DOWN STREAM a WITH FRAME a.
               DISPLAY STREAM a "SUB-TOTAL" @  varDescr
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Amount)  @ tmpTrans.Amount
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Tamount) @ tmpTrans.Tamount
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Budg)     @ tmpTrans.Budg
                   WITH FRAME a.
              DOWN 2 STREAM a WITH FRAME a.
        END.
    END.
    UNDERLINE STREAM a tmpTrans.Amount tmpTrans.Tamount WITH FRAME a.
    DOWN STREAM a WITH FRAME a.
    DISPLAY STREAM a "NET FLOW" @ varDescr wsAmt @ tmpTrans.Amount wsTAmt @ tmpTrans.Tamount
         WITH FRAME a.
    DOWN  2 STREAM a WITH FRAME a.
    PAGE STREAM a.
    RUN RepDetail.ip.
    RETURN.
END. /*OFP ProjRep.ip */

PROCEDURE fundRep.ip:
    FOR EACH tmpTrans:
        tmpTrans.Budg = 0.
       FOR EACH glFbal WHERE glFbal.YEAR = INT(SUBSTR(STRING(end-per),1,4))
                         AND glFbal.acct = tmpTrans.acct 
                         AND glFbal.fund = tmptrans.wsfund NO-LOCK:
           tmpTrans.Budg = tmpTrans.Budg + glFbal.bugTotal.
       END.
    END.
    /* Print Report */
    ASSIGN wsAmt = 0
           wsTAmt = 0.
    EXPORT STREAM b DELIMITER ',' "CASH MOVEMENT MANAGEMENT REPORT FOR THE PERIOD:" 
                                   + STRING(st-per) + " TO " + STRING(end-Per).
    EXPORT STREAM b DELIMITER ',' " " "CURRENT PERIOD" "TO DATE" "" "TO DATE %".
    EXPORT STREAM b DELIMITER ',' " " "ACTUAL" "ACTUAL" "BUDGETTED" "OF BUDGET". 
    FOR EACH tmpTr  NO-LOCK BREAK  BY tmpTr.wsClas BY tmpTr.wsfund:
            ACCUMULATE tmpTr.Amount (TOTAL BY tmpTr.wsclas).
            ACCUMULATE tmpTr.Tamount (TOTAL BY tmpTr.wsclas).
            ACCUMULATE tmpTr.Budg   (TOTAL BY tmpTr.wsclas).
            ACCUMULATE tmpTr.Amount (TOTAL BY tmpTr.wsfund).
            ACCUMULATE tmpTr.Tamount (TOTAL BY tmpTr.wsfund).
            ACCUMULATE tmpTr.Budg   (TOTAL BY tmpTr.wsfund).
            ASSIGN wsAmt  = wsAmt  + tmpTr.Amount
                   wsTAmt = wsTAmt + tmpTr.Tamount.
            IF FIRST-OF(tmpTr.wsclas) AND tmpTr.Tamount <> 0 THEN DO:
               IF  tmpTr.wsClas = 1 THEN
                   EXPORT STREAM b DELIMITER ',' "IN-FLOWS".
               ELSE 
                   EXPORT STREAM b DELIMITER ',' "OUT-FLOWS".
           END.
            IF FIRST-OF(tmpTr.wsclas) AND tmpTr.Tamount <> 0 THEN DO:

                EXPORT STREAM b DELIMITER ',' .
            END.
            IF LAST-OF(tmpTr.wsfund) THEN DO:
                FIND FIRST glfund WHERE glfund.fund = tmpTr.wsfund NO-ERROR.
                wsPerc = ROUND(((ACCUM SUB-TOTAL BY tmpTr.wsfund tmpTr.Tamount) / 
                               (ACCUM SUB-TOTAL BY tmpTr.wsfund tmpTr.Budg) * 100),3).
                IF wsPerc < 0 THEN
                   wsPerc = wsPerc * -1.
                EXPORT STREAM b DELIMITER ',' "     " + glfund.descrip
                               (ACCUM SUB-TOTAL BY tmpTr.wsfund tmpTr.Amount)
                               (ACCUM SUB-TOTAL BY tmpTr.wsfund tmpTr.Tamount) 
                               (ACCUM SUB-TOTAL BY tmpTr.wsfund tmpTr.Budg)
                                wsPerc.               
            END. 
           IF LAST-OF(tmpTr.wsclas) THEN DO:
                  /*EXPORT STREAM b DELIMITER ',' "SUB-TOTAL"
                               (ACCUM SUB-TOTAL BY tmpTr.wsclas tmpTr.Amount)
                               (ACCUM SUB-TOTAL BY tmpTr.wsclas tmpTr.Tamount) 
                               (ACCUM SUB-TOTAL BY tmpTr.wsclas tmpTr.Budg).*/
                EXPORT STREAM b DELIMITER ',' "".
                EXPORT STREAM b DELIMITER ',' "".
           END.
        END.
        EXPORT STREAM b DELIMITER ',' "NET".
        FOR EACH tmpTr  NO-LOCK BREAK  BY tmpTr.wsfund:
            ACCUMULATE tmpTr.Amount (TOTAL BY tmpTr.wsfund).
            ACCUMULATE tmpTr.Tamount (TOTAL BY tmpTr.wsfund).
            ACCUMULATE tmpTr.Budg   (TOTAL BY tmpTr.wsfund).
            IF LAST-OF(tmpTr.wsfund) THEN DO:
                FIND FIRST glfund WHERE glfund.fund = tmpTr.wsfund NO-ERROR.
                EXPORT STREAM b DELIMITER ',' glfund.descrip
                                   (ACCUM TOTAL BY tmpTr.wsfund tmpTr.Amount)
                                   (ACCUM TOTAL BY tmpTr.wsfund tmpTr.Tamount)
                                   (ACCUM TOTAL BY tmpTr.wsfund tmpTr.Budg).
                                    wsPerc.
            END.
        END.
        EXPORT STREAM b DELIMITER ',' "".
        EXPORT STREAM b DELIMITER ',' "".
        EXPORT STREAM b DELIMITER ',' "CASH MOVEMENT MANAGEMENT DETAILED REPORT FOR THE PERIOD:" 
                                       + STRING(st-per) + " TO " + STRING(end-Per).
        EXPORT STREAM b DELIMITER ',' " " "CURRENT PERIOD" "TO DATE" "" "TO DATE %".
        EXPORT STREAM b DELIMITER ',' " " "ACTUAL" "ACTUAL" "BUDGETTED" "OF BUDGET". 
    ASSIGN wsAmt = 0
           wsTAmt = 0.
    FOR EACH tmpTrans NO-LOCK BREAK BY tmpTrans.wsClas BY tmpTrans.Acct:
        ACCUMULATE tmpTrans.Amount (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Tamount (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Budg   (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Amount (TOTAL BY tmpTrans.Acct).
        ACCUMULATE tmpTrans.Tamount (TOTAL BY tmpTrans.Acct).
        ACCUMULATE tmpTrans.Budg   (TOTAL BY tmpTrans.Acct).
        ASSIGN wsAmt  = wsAmt  + tmpTrans.Amount
               wsTAmt = wsTAmt + tmpTrans.Tamount.
        IF FIRST-OF(tmpTrans.wsclas) AND tmpTrans.Tamount <> 0 THEN DO:
           IF  tmpTrans.wsClas = 1 THEN
               EXPORT STREAM b DELIMITER ',' "IN-FLOWS".
           ELSE 
               EXPORT STREAM b DELIMITER ',' "OUT-FLOWS".
       END.
        IF FIRST-OF(tmpTrans.wsclas) AND tmpTrans.Tamount <> 0 THEN DO:    
            EXPORT STREAM b DELIMITER ',' .
        END.
        IF LAST-OF(tmpTrans.Acct) THEN DO:
            FIND FIRST glmf WHERE glmf.Acct = tmpTrans.Acct NO-ERROR.
            IF AVAILABLE glmf THEN
                varDescr = glmf.DESCRIPTION.
            ELSE
                varDescr = STRING(tmpTrans.Acct).
            wsPerc = ROUND(((ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Tamount) / 
                           (ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Budg) * 100),3).
            IF wsPerc < 0 THEN
               wsPerc = wsPerc * -1.
            EXPORT STREAM b DELIMITER ',' "     " + varDescr
                           (ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Amount)
                           (ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Tamount) 
                           (ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Budg)
                            wsPerc.               
        END. 
        IF LAST-OF(tmpTrans.wsclas) THEN DO:
              EXPORT STREAM b DELIMITER ',' "SUB-TOTAL"
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Amount)
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Tamount) 
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Budg).
              EXPORT STREAM b DELIMITER ',' "".
        END.
    END.
    EXPORT STREAM b DELIMITER ',' "NET FLOW" wsAmt wsTAmt.
    EXPORT STREAM b DELIMITER ',' .
    RUN ExcDetail.ip.
END. /*OFP fundRep.ip */

PROCEDURE fundRep1.ip:
    FOR EACH tmpTrans:
        tmpTrans.Budg = 0.
       FOR EACH glFbal WHERE glFbal.YEAR = INT(SUBSTR(STRING(end-per),1,4))
                         AND glFbal.acct = tmpTrans.acct 
                         AND glFbal.fund = tmptrans.wsfund NO-LOCK:
           tmpTrans.Budg = tmpTrans.Budg + glFbal.bugTotal.
       END.
    END.
    /* Print Report */
    ASSIGN wsAmt = 0
           wsTAmt = 0.
    FOR EACH tmpTr  NO-LOCK BREAK  BY tmpTr.wsClas BY tmpTr.wsfund:
        ACCUMULATE tmpTr.Amount (TOTAL BY tmpTr.wsclas).
        ACCUMULATE tmpTr.Tamount (TOTAL BY tmpTr.wsclas).
        ACCUMULATE tmpTr.Budg   (TOTAL BY tmpTr.wsclas).
        ACCUMULATE tmpTr.Amount (TOTAL BY tmpTr.wsfund).
        ACCUMULATE tmpTr.Tamount (TOTAL BY tmpTr.wsfund).
        ACCUMULATE tmpTr.Budg   (TOTAL BY tmpTr.wsfund).
        ASSIGN wsAmt  = wsAmt  + tmpTr.Amount
               wsTAmt = wsTAmt + tmpTr.Tamount.
        IF FIRST-OF(tmpTr.wsclas) AND tmpTr.Tamount <> 0 THEN DO:
           IF  tmpTr.wsClas = 1 THEN
               DISPLAY STREAM a "IN-FLOWS" @ varDescr WITH FRAME a.
           ELSE 
               DISPLAY STREAM a "OUT-FLOWS" @ varDescr WITH FRAME a.
          DOWN STREAM a WITH FRAME a.
       END.
        IF LAST-OF(tmpTr.wsfund) THEN DO:
            FIND FIRST glfund WHERE glfund.fund = tmpTr.wsfund NO-ERROR.
            wsPerc = ROUND(((ACCUM SUB-TOTAL BY tmpTr.wsfund tmpTr.Tamount) / 
                           (ACCUM SUB-TOTAL BY tmpTr.wsfund tmpTr.Budg) * 100),3).
            IF wsPerc < 0 THEN
               wsPerc = wsPerc * -1.
             DISPLAY STREAM a "     " + glfund.descrip @ varDescr
                           (ACCUM SUB-TOTAL BY tmpTr.wsfund tmpTr.Amount)  @ tmpTrans.Amount
                           (ACCUM SUB-TOTAL BY tmpTr.wsfund tmpTr.Tamount) @ tmpTrans.Tamount
                           (ACCUM SUB-TOTAL BY tmpTr.wsfund tmpTr.Budg)    @ tmpTrans.Budg
                            wsPerc @ wsP WITH FRAME a.
              DOWN STREAM a WITH FRAME a.
        END. 
       IF LAST-OF(tmpTr.wsclas) THEN DO:
              UNDERLINE STREAM a tmpTrans.Amount tmpTrans.Tamount  tmpTrans.Budg WITH FRAME a.
              /*DOWN STREAM a WITH FRAME a.
              DISPLAY STREAM a  "SUB-TOTAL" @ varDescr
                           (ACCUM SUB-TOTAL BY tmpTr.wsclas tmpTr.Amount)  @ tmpTrans.Amount
                           (ACCUM SUB-TOTAL BY tmpTr.wsclas tmpTr.Tamount) @ tmpTrans.Tamount 
                           (ACCUM SUB-TOTAL BY tmpTr.wsclas tmpTr.Budg)    @ tmpTrans.Budg WITH FRAME a. */
              DOWN  2 STREAM a WITH FRAME a.
       END.
    END.
    UNDERLINE STREAM a tmpTrans.Amount tmpTrans.Tamount WITH FRAME a.
    DOWN STREAM a WITH FRAME a.
    DISPLAY STREAM a "NET " @ varDescr /*wsAmt @ tmpTrans.Amount wsTAmt @ tmpTrans.Tamount */
         WITH FRAME a.
    DOWN STREAM a WITH FRAME a.
    FOR EACH tmpTr  NO-LOCK BREAK  BY tmpTr.wsfund:
        ACCUMULATE tmpTr.Amount (TOTAL BY tmpTr.wsfund).
        ACCUMULATE tmpTr.Tamount (TOTAL BY tmpTr.wsfund).
        ACCUMULATE tmpTr.Budg   (TOTAL BY tmpTr.wsfund).
        IF LAST-OF(tmpTr.wsfund) THEN DO:
            FIND FIRST glfund WHERE glfund.fund = tmpTr.wsfund NO-ERROR.
            DISPLAY STREAM a "     " + glfund.descrip @ varDescr
                               (ACCUM TOTAL BY tmpTr.wsfund tmpTr.Amount)  @ tmpTrans.Amount
                               (ACCUM TOTAL BY tmpTr.wsfund tmpTr.Tamount) @ tmpTrans.Tamount
                               (ACCUM TOTAL BY tmpTr.wsfund tmpTr.Budg)    @ tmpTrans.Budg
                                wsPerc @ wsP WITH FRAME a.
              DOWN STREAM a WITH FRAME a.
        END.
    END.
    PAGE STREAM a.
    ASSIGN wsAmt = 0
           wsTAmt = 0.
    FOR EACH tmpTrans NO-LOCK BREAK BY tmpTrans.wsClas BY tmpTrans.Acct:
        ACCUMULATE tmpTrans.Amount (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Tamount (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Budg   (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Amount (TOTAL BY tmpTrans.Acct).
        ACCUMULATE tmpTrans.Tamount (TOTAL BY tmpTrans.Acct).
        ACCUMULATE tmpTrans.Budg   (TOTAL BY tmpTrans.Acct).
        ASSIGN wsAmt  = wsAmt  + tmpTrans.Amount
               wsTAmt = wsTAmt + tmpTrans.Tamount.
        IF FIRST-OF(tmpTrans.wsclas) AND tmpTrans.Tamount <> 0 THEN DO:
           IF  tmpTrans.wsClas = 1 THEN
               DISPLAY STREAM a "IN-FLOWS" @ varDescr WITH FRAME a.
           ELSE 
               DISPLAY STREAM a "OUT-FLOWS" @ varDescr WITH FRAME a.
          DOWN STREAM a WITH FRAME a.
       END.
        IF FIRST-OF(tmpTrans.wsclas) AND tmpTrans.Tamount <> 0 THEN DO:    
            EXPORT STREAM b DELIMITER ',' .
        END.
        IF LAST-OF(tmpTrans.Acct) THEN DO:
            FIND FIRST glmf WHERE glmf.Acct = tmpTrans.Acct NO-ERROR.
            IF AVAILABLE glmf THEN
                varDescr = glmf.DESCRIPTION.
            ELSE
                varDescr = STRING(tmpTrans.Acct).
            wsPerc = ROUND(((ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Tamount) / 
                           (ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Budg) * 100),3).
            IF wsPerc < 0 THEN
               wsPerc = wsPerc * -1.
            DISPLAY STREAM a "     " + varDescr @ varDescr
                           (ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Amount)  @ tmpTrans.Amount
                           (ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Tamount) @ tmpTrans.Tamount
                           (ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Budg)    @ tmpTrans.Budg
                            wsPerc @ wsP WITH FRAME a.
            DOWN STREAM a WITH FRAME a.
        END. 
        IF LAST-OF(tmpTrans.wsclas) THEN DO:
             UNDERLINE STREAM a tmpTrans.Amount tmpTrans.Tamount  tmpTrans.Budg WITH FRAME a.
             DOWN STREAM a WITH FRAME a.
               DISPLAY STREAM a "SUB-TOTAL" @  varDescr
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Amount)  @ tmpTrans.Amount
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Tamount) @ tmpTrans.Tamount
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Budg)     @ tmpTrans.Budg
                   WITH FRAME a.
              DOWN 2 STREAM a WITH FRAME a.
        END.
    END.
    UNDERLINE STREAM a tmpTrans.Amount tmpTrans.Tamount WITH FRAME a.
    DOWN STREAM a WITH FRAME a.
    DISPLAY STREAM a "NET FLOW" @ varDescr wsAmt @ tmpTrans.Amount wsTAmt @ tmpTrans.Tamount
         WITH FRAME a.
    DOWN  2 STREAM a WITH FRAME a.
    PAGE STREAM a.
    RUN RepDetail.ip.
    RETURN.
END. /*OFP fundRep.ip */


PROCEDURE deptRep.ip:
    FOR EACH tmpTrans:
        tmpTrans.Budg = 0.
       FOR EACH glDbal WHERE glDbal.YEAR = INT(SUBSTR(STRING(end-per),1,4))
                         AND glDbal.acct = tmpTrans.acct 
                         AND glDbal.dept = tmptrans.wsdept NO-LOCK:
           tmpTrans.Budg = tmpTrans.Budg + glDbal.bugTotal.
       END.
    END.
    /* Print Report */
    ASSIGN wsAmt = 0
           wsTAmt = 0.
    EXPORT STREAM b DELIMITER ',' "CASH MOVEMENT MANAGEMENT REPORT FOR THE PERIOD:" 
                                   + STRING(st-per) + " TO " + STRING(end-Per).
    EXPORT STREAM b DELIMITER ',' " " "CURRENT PERIOD" "TO DATE" "" "TO DATE %".
    EXPORT STREAM b DELIMITER ',' " " "ACTUAL" "ACTUAL" "BUDGETTED" "OF BUDGET". 
    FOR EACH tmpTrans  NO-LOCK BREAK  BY wsClas BY wsdept:
        ACCUMULATE tmpTrans.Amount (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Tamount (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Budg   (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Amount (TOTAL BY tmpTrans.wsdept).
        ACCUMULATE tmpTrans.Tamount (TOTAL BY tmpTrans.wsdept).
        ACCUMULATE tmpTrans.Budg   (TOTAL BY tmpTrans.wsdept).
        ASSIGN wsAmt  = wsAmt  + tmpTrans.Amount
               wsTAmt = wsTAmt + tmpTrans.Tamount.
        IF FIRST-OF(wsclas) AND tmpTrans.Tamount <> 0 THEN DO:
           IF  wsClas = 1 THEN
               EXPORT STREAM b DELIMITER ',' "IN-FLOWS".
           ELSE 
               EXPORT STREAM b DELIMITER ',' "OUT-FLOWS".
       END.
        IF FIRST-OF(wsclas) AND tmpTrans.Tamount <> 0 THEN DO:
            
            EXPORT STREAM b DELIMITER ',' .
        END.
        IF LAST-OF(wsdept) THEN DO:
            FIND FIRST gldept WHERE gldept.dept = tmpTrans.wsdept NO-ERROR.
            wsPerc = ROUND(((ACCUM SUB-TOTAL BY tmpTrans.wsdept tmpTrans.Tamount) / 
                           (ACCUM SUB-TOTAL BY tmpTrans.wsdept tmpTrans.Budg) * 100),3).
            IF wsPerc < 0 THEN
               wsPerc = wsPerc * -1.
            EXPORT STREAM b DELIMITER ',' "     " + gldept.descrip
                           (ACCUM SUB-TOTAL BY tmpTrans.wsdept tmpTrans.Amount)
                           (ACCUM SUB-TOTAL BY tmpTrans.wsdept tmpTrans.Tamount) 
                           (ACCUM SUB-TOTAL BY tmpTrans.wsdept tmpTrans.Budg)
                            wsPerc.               
        END. 
       IF LAST-OF(wsclas) THEN DO:
              EXPORT STREAM b DELIMITER ',' "SUB-TOTAL"
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Amount)
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Tamount) 
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Budg).
        EXPORT STREAM b DELIMITER ',' "".
       END.
    END.
    EXPORT STREAM b DELIMITER ',' "NET FLOW" wsAmt wsTAmt.
    EXPORT STREAM b DELIMITER ',' "".
    EXPORT STREAM b DELIMITER ',' "".
    EXPORT STREAM b DELIMITER ',' "CASH MOVEMENT MANAGEMENT DETAILED REPORT FOR THE PERIOD:" 
                                   + STRING(st-per) + " TO " + STRING(end-Per).
    EXPORT STREAM b DELIMITER ',' " " "CURRENT PERIOD" "TO DATE" "" "TO DATE %".
    EXPORT STREAM b DELIMITER ',' " " "ACTUAL" "ACTUAL" "BUDGETTED" "OF BUDGET". 
    ASSIGN wsAmt = 0
           wsTAmt = 0.
    FOR EACH tmpTrans NO-LOCK BREAK BY tmpTrans.wsClas BY tmpTrans.Acct:
        ACCUMULATE tmpTrans.Amount (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Tamount (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Budg   (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Amount (TOTAL BY tmpTrans.Acct).
        ACCUMULATE tmpTrans.Tamount (TOTAL BY tmpTrans.Acct).
        ACCUMULATE tmpTrans.Budg   (TOTAL BY tmpTrans.Acct).
        ASSIGN wsAmt  = wsAmt  + tmpTrans.Amount
               wsTAmt = wsTAmt + tmpTrans.Tamount.
        IF FIRST-OF(wsclas) AND tmpTrans.Tamount <> 0 THEN DO:
           IF  wsClas = 1 THEN
               EXPORT STREAM b DELIMITER ',' "IN-FLOWS".
           ELSE 
               EXPORT STREAM b DELIMITER ',' "OUT-FLOWS".
       END.
        IF FIRST-OF(wsclas) AND tmpTrans.Tamount <> 0 THEN DO:    
            EXPORT STREAM b DELIMITER ',' .
        END.
        IF LAST-OF(tmpTrans.Acct) THEN DO:
            FIND FIRST glmf WHERE glmf.Acct = tmpTrans.Acct NO-ERROR.
            IF AVAILABLE glmf THEN
                varDescr = glmf.DESCRIPTION.
            ELSE
                varDescr = STRING(tmpTrans.Acct).
            wsPerc = ROUND(((ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Tamount) / 
                           (ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Budg) * 100),3).
            IF wsPerc < 0 THEN
               wsPerc = wsPerc * -1.
            EXPORT STREAM b DELIMITER ',' "     " + varDescr
                           (ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Amount)
                           (ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Tamount) 
                           (ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Budg)
                            wsPerc.               
        END. 
        IF LAST-OF(tmpTrans.wsclas) THEN DO:
              EXPORT STREAM b DELIMITER ',' "SUB-TOTAL"
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Amount)
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Tamount) 
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Budg).
              EXPORT STREAM b DELIMITER ',' "".
        END.
    END.
    EXPORT STREAM b DELIMITER ',' "NET FLOW" wsAmt wsTAmt.
    EXPORT STREAM b DELIMITER ',' .
    RUN ExcDetail.ip.
END. /*OFP deptRep.ip */

PROCEDURE deptRep1.ip:
    FOR EACH tmpTrans:
        tmpTrans.Budg = 0.
       FOR EACH glDbal WHERE glDbal.YEAR = INT(SUBSTR(STRING(end-per),1,4))
                         AND glDbal.acct = tmpTrans.acct 
                         AND glDbal.dept = tmptrans.wsdept NO-LOCK:
           tmpTrans.Budg = tmpTrans.Budg + glDbal.bugTotal.
       END.
    END.
    /* Print Report */
    ASSIGN wsAmt = 0
           wsTAmt = 0.
    FOR EACH tmpTrans  NO-LOCK BREAK  BY wsClas BY wsdept:
        ACCUMULATE tmpTrans.Amount (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Tamount (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Budg   (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Amount (TOTAL BY tmpTrans.wsdept).
        ACCUMULATE tmpTrans.Tamount (TOTAL BY tmpTrans.wsdept).
        ACCUMULATE tmpTrans.Budg   (TOTAL BY tmpTrans.wsdept).
        ASSIGN wsAmt  = wsAmt  + tmpTrans.Amount
               wsTAmt = wsTAmt + tmpTrans.Tamount.
        IF FIRST-OF(wsclas) AND tmpTrans.Tamount <> 0 THEN DO:
           IF  wsClas = 1 THEN
               DISPLAY STREAM a "IN-FLOWS" @ varDescr WITH FRAME a.
           ELSE 
               DISPLAY STREAM a "OUT-FLOWS" @ varDescr WITH FRAME a.
          DOWN STREAM a WITH FRAME a.
       END.
        IF LAST-OF(wsdept) THEN DO:
            FIND FIRST gldept WHERE gldept.dept = tmpTrans.wsdept NO-ERROR.
            wsPerc = ROUND(((ACCUM SUB-TOTAL BY tmpTrans.wsdept tmpTrans.Tamount) / 
                           (ACCUM SUB-TOTAL BY tmpTrans.wsdept tmpTrans.Budg) * 100),3).
            IF wsPerc < 0 THEN
               wsPerc = wsPerc * -1.
             DISPLAY STREAM a "     " + gldept.descrip @ varDescr
                           (ACCUM SUB-TOTAL BY tmpTrans.wsdept tmpTrans.Amount)  @ tmpTrans.Amount
                           (ACCUM SUB-TOTAL BY tmpTrans.wsdept tmpTrans.Tamount) @ tmpTrans.Tamount
                           (ACCUM SUB-TOTAL BY tmpTrans.wsdept tmpTrans.Budg)    @ tmpTrans.Budg
                            wsPerc @ wsP WITH FRAME a.
              DOWN STREAM a WITH FRAME a.
        END. 
       IF LAST-OF(wsclas) THEN DO:
              UNDERLINE STREAM a tmpTrans.Amount tmpTrans.Tamount  tmpTrans.Budg WITH FRAME a.
              DOWN STREAM a WITH FRAME a.
              DISPLAY STREAM a  "SUB-TOTAL" @ varDescr
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Amount)  @ tmpTrans.Amount
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Tamount) @ tmpTrans.Tamount 
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Budg)    @ tmpTrans.Budg WITH FRAME a.
              DOWN  2 STREAM a WITH FRAME a.
       END.
    END.
    UNDERLINE STREAM a tmpTrans.Amount tmpTrans.Tamount WITH FRAME a.
    DOWN STREAM a WITH FRAME a.
    DISPLAY STREAM a "NET FLOW" @ varDescr wsAmt @ tmpTrans.Amount wsTAmt @ tmpTrans.Tamount
         WITH FRAME a.
    DOWN  2 STREAM a WITH FRAME a.
    PAGE STREAM a.
    ASSIGN wsAmt = 0
           wsTAmt = 0.
    FOR EACH tmpTrans NO-LOCK BREAK BY tmpTrans.wsClas BY tmpTrans.Acct:
        ACCUMULATE tmpTrans.Amount (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Tamount (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Budg   (TOTAL BY tmpTrans.wsclas).
        ACCUMULATE tmpTrans.Amount (TOTAL BY tmpTrans.Acct).
        ACCUMULATE tmpTrans.Tamount (TOTAL BY tmpTrans.Acct).
        ACCUMULATE tmpTrans.Budg   (TOTAL BY tmpTrans.Acct).
        ASSIGN wsAmt  = wsAmt  + tmpTrans.Amount
               wsTAmt = wsTAmt + tmpTrans.Tamount.
        IF FIRST-OF(wsclas) AND tmpTrans.Tamount <> 0 THEN DO:
           IF  wsClas = 1 THEN
               DISPLAY STREAM a "IN-FLOWS" @ varDescr WITH FRAME a.
           ELSE 
               DISPLAY STREAM a "OUT-FLOWS" @ varDescr WITH FRAME a.
          DOWN STREAM a WITH FRAME a.
       END.
        IF FIRST-OF(wsclas) AND tmpTrans.Tamount <> 0 THEN DO:    
            EXPORT STREAM b DELIMITER ',' .
        END.
        IF LAST-OF(tmpTrans.Acct) THEN DO:
            FIND FIRST glmf WHERE glmf.Acct = tmpTrans.Acct NO-ERROR.
            IF AVAILABLE glmf THEN
                varDescr = glmf.DESCRIPTION.
            ELSE
                varDescr = STRING(tmpTrans.Acct).
            wsPerc = ROUND(((ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Tamount) / 
                           (ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Budg) * 100),3).
            IF wsPerc < 0 THEN
               wsPerc = wsPerc * -1.
            DISPLAY STREAM a "     " + varDescr @ varDescr
                           (ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Amount)  @ tmpTrans.Amount
                           (ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Tamount) @ tmpTrans.Tamount
                           (ACCUM SUB-TOTAL BY tmpTrans.Acct tmpTrans.Budg)    @ tmpTrans.Budg
                            wsPerc @ wsP WITH FRAME a.
            DOWN STREAM a WITH FRAME a.
        END. 
        IF LAST-OF(tmpTrans.wsclas) THEN DO:
             UNDERLINE STREAM a tmpTrans.Amount tmpTrans.Tamount  tmpTrans.Budg WITH FRAME a.
             DOWN STREAM a WITH FRAME a.
               DISPLAY STREAM a "SUB-TOTAL" @  varDescr
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Amount)  @ tmpTrans.Amount
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Tamount) @ tmpTrans.Tamount
                           (ACCUM SUB-TOTAL BY tmpTrans.wsclas tmpTrans.Budg)     @ tmpTrans.Budg
                   WITH FRAME a.
              DOWN 2 STREAM a WITH FRAME a.
        END.
    END.
    UNDERLINE STREAM a tmpTrans.Amount tmpTrans.Tamount WITH FRAME a.
    DOWN STREAM a WITH FRAME a.
    DISPLAY STREAM a "NET FLOW" @ varDescr wsAmt @ tmpTrans.Amount wsTAmt @ tmpTrans.Tamount
         WITH FRAME a.
    DOWN  2 STREAM a WITH FRAME a.
    PAGE STREAM a.
    RUN RepDetail.ip.
    RETURN.
END. /*OFP deptRep.ip */

PROCEDURE RepDetail.ip:
FOR EACH CBKMF:
    IF CAN-FIND(FIRST CbTrans WHERE  cbtrans.bank = cbkmf.bank AND CBTrans.TranType = 1
                AND  CBTrans.Accper >= wsper AND CBTrans.Accper  <= end-per NO-LOCK) THEN DO:
        DISPLAY STREAM a cbkmf.descrip @ wsNar WITH FRAME frm-rpt.
        DOWN STREAM a WITH FRAME frm-rpt.
    END.
    ELSE NEXT.
    FOR EACH cbtrans WHERE cbtrans.bank = cbkmf.bank AND CBTrans.TranType = 1
                       AND  CBTrans.Accper >= wsper AND CBTrans.Accper  <= end-per NO-LOCK
        BY cbtrans.trdate BY cbtrans.ref BY cbtrans.seq:
        IF cbtrans.seq = 0 THEN DO:
            DISPLAY STREAM a CBTrans.Accper @ st-per CBTrans.trDate CBTrans.Ref
                 CBTrans.Descrip @ wsNar CBTrans.amount @ wsAmt cbkmf.txtCur WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
        END.
        ELSE IF cbtrans.seq <> 0 THEN DO:
            DISPLAY STREAM a CBTrans.Descrip @ wsNar CBTrans.Ledger CBTrans.amount WITH FRAME frm-rpt.
            DOWN STREAM a WITH FRAME frm-rpt.
        END.
    END.
    DOWN 3 STREAM a WITH FRAME frm-rpt.
END.
END PROCEDURE.

PROCEDURE ExcDetail.ip:
    EXPORT STREAM b DELIMITER ',' "".
    EXPORT STREAM b DELIMITER ',' "".
    EXPORT STREAM b DELIMITER ',' "PAYMENTS DETAILS".
    EXPORT STREAM b DELIMITER ',' "".
    EXPORT STREAM b DELIMITER ',' "PERIOD" "DATE" "REFERENCE" "DESCRIPTION" "ALLOCATION" "" "AMOUNT".
FOR EACH CBKMF:
    IF CAN-FIND(FIRST CbTrans WHERE  cbtrans.bank = cbkmf.bank AND CBTrans.TranType = 1
                AND  CBTrans.Accper >= wsper AND CBTrans.Accper  <= end-per NO-LOCK) THEN DO:
        EXPORT STREAM b DELIMITER ',' "" "" "" cbkmf.descrip.
    END.
    ELSE NEXT.
    FOR EACH cbtrans WHERE cbtrans.bank = cbkmf.bank AND CBTrans.TranType = 1
                       AND  CBTrans.Accper >= wsper AND CBTrans.Accper  <= end-per NO-LOCK
        BY cbtrans.trdate BY cbtrans.ref BY cbtrans.seq:
        IF cbtrans.seq = 0 THEN DO:
            EXPORT STREAM b DELIMITER ',' CBTrans.Accper CBTrans.trDate CBTrans.Ref
                 CBTrans.Descrip "" "" CBTrans.amount cbkmf.txtCur.
        END.
        ELSE IF cbtrans.seq <> 0 THEN DO:
            EXPORT STREAM b DELIMITER ',' "" "" "" CBTrans.Descrip CBTrans.Ledger CBTrans.amount.
        END.
    END.
    EXPORT STREAM b DELIMITER ',' "".
END.
END PROCEDURE.
    





