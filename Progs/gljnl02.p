/* Program.................gljnl02.p.p
   Notes:................. General ledger batch update.
   Author:.................S. Mawire
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF SHARED VAR varUser LIKE simusr.usercode.
DEF VAR wsid LIKE glbctr.intBatch.
DEF VAR Current-Record AS ROWID.
DEF VAR method-status  AS LOGICAL.
DEF VAR wsbatch     LIKE glb.intBatch.
DEF VAR wscurr      LIKE SIMCTR.CURPER.
DEF VAR wsclosed    LIKE  SIMCTR.CLOSEPER.
DEF VAR wsper       LIKE simctr.closeper.
DEF VAR wsTransId      LIKE gltdf.TransID.
DEF  VAR wsyear     AS INT.
DEF VAR wsmonth     AS INT.
DEF VAR wsTime      AS CHAR FORM "x(8)".
DEF VAR wsDr        LIKE glb.amt.
DEF VAR wsCr        LIKE glb.amt.
DEF VAR wsTDr        LIKE glb.amt.
DEF VAR wsTCr        LIKE glb.amt.
DEF VAR wsdes       LIKE glmf.DESCRIPTION.
DEF VAR w-orientation AS CHAR      INITIAL "Landscape"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10.0          NO-UNDO.
DEF BUTTON btn-Valid   LABEL "Validate".
DEF BUTTON btn-Update   LABEL "Update".
DEF BUTTON btn-Print  LABEL "Print".
DEF BUTTON btn-Exit    LABEL "Close".


wsTime    = STRING(TIME,"HH:MM:SS").
wsTransID = DEC (string(YEAR(TODAY),"9999")
              + string(MONTH(TODAY),"99" )
              + string(DAY(TODAY),"99")
              + SUBSTR(STRING(wsTIME),1,2) 
              + SUBSTR(STRING(wsTIME),4,2) 
              + SUBSTR(STRING(wsTIME),7,2) ).


DEFINE QUERY qry-glbctr FOR glbctr scrolling.
DEF BROWSE brw-glbctr QUERY qry-glbctr
        DISPLAY UID COLUMN-LABEL "User" 
                period WIDTH 10 COLUMN-LABEL "Period"
                BDate WIDTH 12 COLUMN-LABEL "Batch Date"
                intBatch COLUMN-LABEL "Batch Number" 
                AMT[1]COLUMN-LABEL "Dr Amount" 
                AMT[2] COLUMN-LABEL "Cr Amount"
        WITH 20 DOWN SEPARATORS.

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 graphic-edge  NO-FILL
     SIZE 80 BY 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 80 BY 17.5.

FORM
    glb.TrDate             LABEL "TRANS-DATE"
    glb.acct               LABEL "ACCOUNT"
    glmf.DESCRIPTION       LABEL "ACCOUNT DESCRIPTION"
    glb.Ref                LABEL "REFERENCE"
    wsDr                   LABEL "DEBIT"
    wsCr                   LABEL "CREDIT"
HEADER skip(5) "                         TRANSACTION BATCH UPDATE FOR BATCH: " AT 10 
    TRIM(STRING(wsBatch)) SPACE(2) "Page: " AT 110 PAGE-NUMBER(a) SKIP(1)
    "TRANSACTION ID: " AT 20 wsTransId  SPACE(2) "UPDATE DATE: " TODAY SPACE(2) "User: " varUser 
    "--------------------------------------------------------------------------------------------------------------"
     SKIP(2)
    WITH DOWN STREAM-IO FONT 6 CENTERED NO-BOX
    WIDTH 132 FRAME frm-report.

DEF FRAME frm-main
    brw-glbctr AT ROW 2 COL 5
    btn-Valid AT ROW 19.7 COL 5 SPACE(15)
    btn-Update SPACE(15) 
    btn-Print SPACE(15)
    btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 19 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
    SIZE 90 BY 23
    TITLE "Journal Update" view-as dialog-box.


/******* Triggers ***** */
ON CHOOSE OF btn-Update IN FRAME frm-main 
    OR 'mouse-select-dblclick' OF brw-glbctr
DO:
    GET CURRENT qry-glbctr EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsbatch = glbctr.intbatch
           wsyear = int(substr(string(glbctr.period),1,4))
           wsmonth = int(substr(string(glbctr.period),5,2)).
    /* Select print */
   
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="Update-ip"
                    &paged} 
END.

ON 'mouse-select-click':U OF brw-glbctr  
DO:
    IF glbctr.AMT[1] <> glbctr.AMT[2] * -1 THEN
        DISABLE btn-update WITH FRAME frm-main.
    ELSE
        ENABLE btn-update WITH FRAME frm-main.
    RETURN.
END.

ON 'choose':U OF btn-print IN FRAME frm-main 
DO:
    {PrintOpt.i &stream-name="stream a"
                    &print-prog="Print.ip"
                    &paged}
    RETURN.
END.
/********** MAIN LOGIC **********/
OPEN QUERY qry-glbctr FOR EACH glbctr NO-LOCK.
ENABLE ALL /*EXCEPT btn-update */ WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
CLOSE QUERY qry-glbctr.
HIDE FRAME frm-main.

/* Internal Procedures */
PROCEDURE Update-ip:

DO TRANSACTION ON ERROR UNDO, LEAVE:
    FIND FIRST glbctr WHERE glbctr.intbatch = wsbatch EXCLUSIVE-LOCK NO-ERROR.
    FOR EACH glb WHERE glb.intbatch = wsbatch EXCLUSIVE-LOCK:
        FIND FIRST glmf WHERE glmf.acct = glb.acct NO-LOCK NO-ERROR.
        /* Post to gltdf */
        CREATE gltdf.
        ASSIGN gltdf.acct = glb.acct
               gltdf.dept = glb.dept
               gltdf.fund = glb.fund
               gltdf.proj = glb.proj
               gltdf.AMT  = glb.amt
               gltdf.CREDATE = TODAY
               gltdf.DESCRIP = glb.DESCRIPTION
               gltdf.TransId    = wsTransId
               gltdf.period  = glbctr.period
               gltdf.REF     = glb.REF
               gltdf.trDATE  = glb.trDATE
               gltdf.UID     = glb.UID
               gltdf.UID2    = varUser
               gltdf.SOURCE  = "GL".
        
        /* glbal*/
        FIND FIRST glbal WHERE GLBAL.YEAR = wsyear
                          AND gltdf.acct  = GLBAL.acct EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE glbal THEN DO:
                CREATE glbal.
                ASSIGN glbal.acct = glb.acct
                       glbal.YEAR = wsyear.
            END.
        ASSIGN wsDr = 0 wsCr = 0.
        ASSIGN  glbal.amt[wsmonth] = glbal.amt[wsmonth] + glb.amt
                wsDr               = glb.amt WHEN glb.amt >= 0
                wsCr               = glb.amt WHEN glb.amt < 0.
        DISPLAY STREAM a glb.TrDate glb.acct glmf.DESCRIPTION glb.Ref wsDr wsCr
            WITH FRAME frm-report.
        DOWN STREAM a WITH FRAME frm-report.
        /* Project Balances */
        IF  glb.proj <> 0 THEN DO:
            FIND FIRST glPbal WHERE GLPBal.proj = glb.proj AND GLPBal.acct = glb.acct 
                       AND GLPBal.YEAR = wsyear NO-ERROR.
            IF NOT AVAILABLE glPbal THEN DO:
                CREATE glPbal.
                ASSIGN glPbal.acct = glb.acct
                       GLPBal.proj = glb.proj
                       glPbal.YEAR = wsyear.
            END.
            ASSIGN glPbal.amt[wsmonth]= glPbal.amt[wsmonth] + glb.amt.
        END.

/* Department Balances */
        IF glb.dept <> 0 THEN DO:
            FIND FIRST glDbal WHERE GLDBal.dept = glb.dept AND glDbal.acct = glb.acct 
                           AND glDbal.YEAR = wsyear NO-ERROR.
            IF NOT AVAILABLE glDbal THEN DO:
                CREATE glDbal.
                ASSIGN glDbal.acct = glb.acct
                       glDbal.dept = glb.dept
                       glDbal.YEAR = wsyear.
            END.
            ASSIGN glDbal.amt[wsmonth]= glDbal.amt[wsmonth] + glb.amt.
        END.

/* Fund Balances */
        IF  glb.fund <> 0 THEN DO:
            FIND FIRST glFBal WHERE glFBal.fund = glb.fund AND glFBal.acct = glb.acct 
                           AND glFBal.YEAR = wsyear NO-ERROR.
            IF NOT AVAILABLE glFBal THEN DO:
                CREATE glFBal.
                ASSIGN glFBal.acct = glb.acct
                       glFBal.fund = glb.fund
                       glFBal.YEAR = wsyear.
            END.
            ASSIGN glFBal.amt[wsmonth]= glFBal.amt[wsmonth] + glb.amt.
        END.
        DELETE glb.
    END. /* eof for each glb...*/
    UNDERLINE STREAM a glmf.DESCRIPTION wsDr wsCr WITH FRAME frm-report.
    DISPLAY STREAM a "TOTALS" @ glmf.DESCRIPTION GLBCTR.AMT[1] @ wsDr
           GLBCTR.AMT[2] @ wsCr WITH FRAME frm-report.
        DOWN STREAM a WITH FRAME frm-report.
    DELETE glbctr.
    OPEN QUERY qry-glbctr FOR EACH glbctr NO-LOCK. 
END. /* eof do Transaction ... */
END PROCEDURE. 

PROCEDURE Print.ip:
    GET CURRENT qry-glbctr EXCLUSIVE-LOCK NO-WAIT.
    ASSIGN wsbatch = glbctr.intbatch.
    FOR EACH glb WHERE glb.intBatch = wsBatch:
        FIND FIRST glmf WHERE glmf.acct = glb.acct NO-ERROR.
        IF AVAILABLE glmf THEN
            wsdes = glmf.DESCRIPTION.
        ELSE wsdes = "***** INVALID ACCOUNT*******".
        ASSIGN wsDr = 0 wsCr = 0.
        ASSIGN  wsDr               = glb.amt WHEN glb.amt >= 0
                wsCr               = glb.amt WHEN glb.amt < 0.
        ASSIGN wsTDr = wstDr + wsDr
               wsTCr = wsTCr + wsCr.
        DISPLAY STREAM a glb.TrDate glb.acct wsdes @ glmf.DESCRIPTION glb.Ref wsDr wsCr
            WITH FRAME frm-report.
        DOWN STREAM a WITH FRAME frm-report.
    END.
    UNDERLINE STREAM a glb.TrDate glb.acct glmf.DESCRIPTION glb.Ref wsDr wsCr
              WITH FRAME frm-report.
    DOWN STREAM a WITH FRAME frm-report.
    DISPLAY STREAM a "TOTALS" @ glmf.DESCRIPTION wsTDr @ wsDr wsTCr @ wsCr WITH FRAME frm-report.
    DOWN STREAM a WITH FRAME frm-report.
    RETURN.
END.
