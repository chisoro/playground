/* Program.................gle02.p
   Notes:................. Ledger Enquiries by Fund
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
&SCOPED-DEFINE tmptable             glmf
&SCOPED-DEFINE skey                 glmf.acct

DEF VAR wsAcc    AS CHAR FORM "x(12)".
DEF VAR wsFund   AS CHAR FORM "x(6)".
DEF VAR wsDesc   LIKE glmf.DESCRIPTION.
DEF VAR wsMonth  AS CHAR FORM "x(5)" EXTENT 13.
DEF VAR wsCurA   AS DEC FORM "zzzzzzzz9.99-" EXTENT 13.
DEF VAR wsCurB   LIKE wsCurA.
DEF VAR wsPreA   LIKE wsCurA.
DEF VAR wsPreB   LIKE wsCurA.
DEF VAR wsYear   LIKE glFbal.YEAR.
DEF VAR wsPYear  LIKE glFbal.YEAR.
DEF VAR wsPBf    LIKE glFbal.bfbal EXTENT 2.
DEF  VAR wsAvail LIKE glFbal.bfbal.
DEF VAR  wsAn    AS LOGICAL.
DEF VAR wslabel  AS CHAR EXTENT 4 FORM "X(12)" VIEW-AS TEXT NO-UNDO.
DEF VAR X        AS INT.
DEF VAR st-per LIKE gltdf.period.
DEF VAR wslink LIKE gltdf.transid.
DEF BUTTON btn-link LABEL "LINKED Transaction".

{varlibrary.i}

DEF RECTANGLE rect-1 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL SIZE 90 BY 2.15.
DEF RECTANGLE rect-2 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL SIZE 90 by 3.2.

DEF    QUERY qry-trans FOR gltdf SCROLLING.
DEF BROWSE brw-trans QUERY qry-trans
    DISPLAY gltdf.SOURCE LABEL "SOURCE" gltdf.period gltdf.trDATE gltdf.REF gltdf.AMT gltdf.DESCRIP FORM "x(40)" WIDTH 40 gltdf.CREDATE 
    gltdf.TransID LABEL "TRANSACTIONID" WITH 20 DOWN SEPARATORS.

DEF    QUERY qry-link FOR gltdf SCROLLING.
DEF BROWSE brw-link QUERY qry-link
    DISPLAY gltdf.SOURCE LABEL "SOURCE" gltdf.acct gltdf.period gltdf.trDATE gltdf.REF gltdf.AMT gltdf.DESCRIP FORM "x(40)" WIDTH 30 gltdf.CREDATE 
    gltdf.TransID LABEL "TRANSACTIONID" WITH 20 DOWN SEPARATORS.

/*DEF QUERY qry-fund FOR glfund SCROLLING.
DEF BROWSE brw-fund QUERY qry-fund
    DISPLAY glfund.Fund glfund.DESCRIP WITH 10 DOWN SEPARATORS. 

DEF FRAME frm-pickFund
   brw-fund AT ROW 2 COL 5
    skip(1.5)
    btn-ok COLON 5
    btn-close COLON 20
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "FUND/SEGMENT SELECTION".*/

DEFINE FRAME frmPeriod
    st-per LABEL "Start Period" AT 5
    skip(1.5)
    btn-ok COLON 5
    btn-close COLON 20
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Period Selection".

DEFINE FRAME frm-trans 
    brw-trans AT ROW 2 COL 5
    skip(0.5)
    btn-link COLON 20 btn-close COLON 80
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Transaction Listing".

DEFINE FRAME frm-link 
    brw-link AT ROW 2 COL 5
    skip(0.5)
    btn-close COLON 60
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Transaction Listing".

DEF FRAME frmEnquiry
     btnAcc AT ROW 1.5 COL 5  NO-TAB-STOP SPACE(3)
     wsAcc NO-LABEL  AUTO-RETURN  
     glmf.description NO-LABEL NO-TAB-STOP VIEW-AS FILL-IN SIZE 40 by 1
     btnFund AT ROW 3 COL 5  NO-TAB-STOP SPACE(3)
     wsFund NO-LABEL  AUTO-RETURN   
     glfund.descrip NO-LABEL NO-TAB-STOP VIEW-AS FILL-IN SIZE 40 by 1
     wsYear LABEL "Year"   AUTO-RETURN
     /*glvote.DESCRIP label "Cost Center"AT ROW 2.9 COL 30 VIEW-AS FILL-IN SIZE 40 by 1 NO-TAB-STOP */
     wslabel[1] AT ROW 5 COL 18  NO-LABEL FONT 3 SPACE(12)
     wslabel[2] NO-LABEL FONT 3
     wslabel[3]AT ROW 5 COL 57 NO-LABEL FONT 3 SPACE(12)
     wslabel[4] NO-LABEL FONT 3
     "Balance B/F"             AT ROW 6 COL 4
     wsPBf[1]    AT ROW 6 COL 16   NO-LABEL NO-TAB-STOP FORMAT "zzzzzzzzzzzz9.99-"
     wsPBf[2]    AT ROW 6 COL 55.1 NO-LABEL NO-TAB-STOP FORMAT "zzzzzzzzzzzz9.99-"
     "JANUARY"   AT ROW 7 COL 4 
     wsCurA[1]   NO-TAB-STOP NO-LABEL AT ROW 7 COL 16  FORMAT "zzzzzzzzzzzz9.99-"
     wsCurB[1]   NO-TAB-STOP NO-LABEL FORMAT "zzzzzzzzzzz9-"
     wsPreA[1]   NO-TAB-STOP NO-LABEL FORMAT "zzzzzzzzzzzz9.99-"
     wsPreB[1]   NO-TAB-STOP NO-LABEL FORMAT "zzzzzzzzzzz9-"
     "FEBRUARY"      AT ROW 8 COL 4 
     wsCurA[2]  NO-TAB-STOP  NO-LABEL AT ROW 8 COL 16  FORMAT "zzzzzzzzzzzz9.99-"
     wsCurB[2]  NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzz9-"
     wsPreA[2]  NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzzz9.99-"
     wsPreB[2]  NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzz9-"
     "MARCH"      AT ROW 9 COL 4 
     wsCurA[3]  NO-TAB-STOP  NO-LABEL AT ROW 9 COL 16  FORMAT "zzzzzzzzzzzz9.99-"
     wsCurB[3]  NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzz9-"
     wsPreA[3]  NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzzz9.99-"
     wsPreB[3]  NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzz9-"
     "APRIL"      AT ROW 10 COL 4 
     wsCurA[4]  NO-TAB-STOP  NO-LABEL AT ROW 10 COL 16  FORMAT "zzzzzzzzzzzz9.99-"
     wsCurB[4]  NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzz9-"
     wsPreA[4]  NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzzz9.99-"
     wsPreB[4]  NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzz9-"
    "MAY"        AT ROW 11 COL 4 
     wsCurA[5]  NO-TAB-STOP  NO-LABEL AT ROW 11 COL 16  FORMAT "zzzzzzzzzzzz9.99-"
     wsCurB[5]  NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzz9-"
     wsPreA[5]  NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzzz9.99-"
     wsPreB[5]  NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzz9-"
     "JUNE"      AT ROW 12 COL 4 
     wsCurA[6]  NO-TAB-STOP  NO-LABEL AT ROW 12 COL 16  FORMAT "zzzzzzzzzzzz9.99-"
     wsCurB[6]  NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzz9-"
     wsPreA[6]  NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzzz9.99-"
     wsPreB[6]  NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzz9-".
     
DEF FRAME frmEnquiry        
     "JULY"      AT ROW 13 COL 4
     wsCurA[7]  NO-TAB-STOP  NO-LABEL AT ROW 13 COL 16 FORMAT "zzzzzzzzzzzz9.99-"
     wsCurB[7]  NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzz9-"
     wsPreA[7]  NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzzz9.99-"
     wsPreB[7]  NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzz9-"
    "AUGUST"      AT ROW 14 COL 4 
     wsCurA[8]  NO-TAB-STOP  NO-LABEL AT ROW 14 COL 16 FORMAT "zzzzzzzzzzzz9.99-"
     wsCurB[8]  NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzz9-"
     wsPreA[8]  NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzzz9.99-"
     wsPreB[8]  NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzz9-"
     "SEPTEMBER"      AT ROW 15 COL 4 
     wsCurA[9]  NO-TAB-STOP  NO-LABEL AT ROW 15 COL 16 FORMAT "zzzzzzzzzzzz9.99-"
     wsCurB[9]  NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzz9-"
     wsPreA[9]  NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzzz9.99-"
     wsPreB[9]  NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzz9-"
     "OCTOBER"      AT ROW 16 COL 4 
     wsCurA[10] NO-TAB-STOP  NO-LABEL AT ROW 16 COL 16 FORMAT "zzzzzzzzzzzz9.99-"
     wsCurB[10] NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzz9-"
     wsPreA[10] NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzzz9.99-"
     wsPreB[10] NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzz9-"
     "NOVEMBER"      AT ROW 17 COL 4 
     wsCurA[11]  NO-TAB-STOP  NO-LABEL AT ROW 17 COL 16 FORMAT "zzzzzzzzzzzz9.99-"
     wsCurB[11]  NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzz9-"
     wsPreA[11]  NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzzz9.99-"
     wsPreB[11]  NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzz9-"
     "DECEMBER"      AT ROW 18 COL 4 
     wsCurA[12]  NO-TAB-STOP  NO-LABEL AT ROW 18 COL 16 FORMAT "zzzzzzzzzzzz9.99-"
     wsCurB[12]  NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzz9-"
     wsPreA[12]  NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzzz9.99-"
     wsPreB[12]  NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzz9-"
     "TOTAL:"           AT ROW 19.5 COL 4
     wsCurA[13]  NO-TAB-STOP  NO-LABEL AT ROW 19.5 COL 16 FORMAT "zzzzzzzzzzzz9.99-"
     wsCurB[13]  NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzz9-"
     wsPreA[13]  NO-TAB-STOP  NO-LABEL  FORMAT "zzzzzzzzzzzz9.99-"
     wsPreB{13}  NO-TAB-STOP  NO-LABEL FORMAT "zzzzzzzzzzz9-"
     wsAvail     LABEL  "AVAILABLE BUDGET" NO-TAB-STOP AT ROW 21.2 COL 4 FORMAT "zzzzzzzzzzzz9.99-"
     btnTrans    AT ROW 22.92 COL 10 SPACE(42) 
     btn-close 
     rect-1      AT ROW 22.38 COL 3
     rect-2     AT ROW 1.27 COL 3
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED
         SIZE 98 BY 26 TITLE "LEDGER ENQUIRIES BY FUND".

ON CHOOSE OF btnAcc IN FRAME frmEnquiry
DO:
  VIEW FRAME frm-pick.
  OPEN QUERY qry-ledger FOR EACH glmf NO-LOCK.
  ENABLE ALL WITH FRAME frm-pick.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pick 
          OR close of THIS-PROCEDURE IN FRAME frm-pick
          OR CHOOSE OF btn-ok IN FRAME frm-pick 
          OR 'enter':u OF brw-ledger
          OR 'mouse-select-dblclick' OF brw-ledger.
  CLOSE QUERY qry-ledger.
  HIDE FRAME frm-pick.
  APPLY 'tab' TO btnAcc.
  APPLY 'tab' TO wsAcc.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pick 
    OR 'enter':u OF brw-ledger
    OR 'mouse-select-dblclick' OF brw-ledger
DO: 
   GET CURRENT qry-ledger NO-LOCK NO-WAIT.
   DISPLAY glmf.acct @ wsAcc glmf.DESCRIPTION WITH FRAME frmEnquiry.
   APPLY 'TAB' TO wsAcc IN FRAME frmEnquiry.
END.

ON 'tab' of wsAcc IN FRAME frmEnquiry
    OR 'enter' OF wsAcc IN FRAME frmEnquiry
DO:
   ASSIGN wsAcc = wsAcc:SCREEN-VALUE. 
   FIND FIRST glmf WHERE glmf.acct = DEC(wsAcc:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF NOT AVAIL glmf then
    do:
        wsAn = YES.
        MESSAGE "Account number" wsAcc "does not exist." SKIP
                "Get next available account number?."
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
                        UPDATE wsAn.
        IF wsAn THEN DO:
            FIND FIRST glmf NO-LOCK NO-ERROR.
        END.
        IF NOT AVAIL glmf THEN RETURN.
    end.
    DISPLAY glmf.acct @ wsAcc glmf.DESCRIPTION WITH FRAME frmEnquiry.
END.


ON CHOOSE OF btnFund IN FRAME frmEnquiry
DO:
  VIEW FRAME frm-pickFund.
  OPEN QUERY qry-fund FOR EACH glfund NO-LOCK.
  ENABLE ALL WITH FRAME frm-pickFund.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-pickFund 
          OR close of THIS-PROCEDURE IN FRAME frm-pickFund
          OR CHOOSE OF btn-ok IN FRAME frm-pickFund 
          OR 'enter':u OF brw-fund
          OR 'mouse-select-dblclick' OF brw-fund.
  CLOSE QUERY qry-fund.
  HIDE FRAME frm-pickFund.
  APPLY 'tab' TO btnFund.
  APPLY 'tab' TO wsFund.
  RETURN. 
END.

ON CHOOSE OF btn-ok IN FRAME frm-pickFund 
    OR 'enter':u OF brw-fund
    OR 'mouse-select-dblclick' OF brw-fund
DO: 
   GET CURRENT qry-fund NO-LOCK NO-WAIT.
   DISPLAY glfund.Fund @ wsFund glfund.DESCRIP WITH FRAME frmEnquiry.
   APPLY 'TAB' TO wsFund IN FRAME frmEnquiry.
END.

ON 'tab' of wsFund IN FRAME frmEnquiry
    OR 'enter' OF wsFund IN FRAME frmEnquiry
DO:
   ASSIGN wsFund = wsFund:SCREEN-VALUE. 
   FIND FIRST glfund WHERE glfund.Fund = INT(wsFund:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF NOT AVAIL glfund then
    do:
        wsAn = YES.
        MESSAGE "Fund/Segment" wsFund "does not exist." SKIP
                "Get next available Fund/Segment number?."
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
                        UPDATE wsAn.
        IF wsAn THEN DO:
            FIND FIRST glfund NO-LOCK NO-ERROR.
        END.
        IF NOT AVAIL glfund THEN RETURN.
    end.
    DISPLAY glfund.Fund @ wsFund glfund.DESCRIP WITH FRAME frmEnquiry.
END.

ON 'tab' OF wsYear IN FRAME frmEnquiry
    OR 'enter' OF wsYear IN FRAME frmEnquiry
DO:
    ASSIGN wsAcc wsFund wsYear.
    ASSIGN wsCurA = 0
           wsCurB = 0
           wsPbf  = 0
           wsPreA = 0
           wsPreB = 0.
    ASSIGN wslabel[1] = STRING(INT(wsYear:SCREEN-VALUE)) + " ACTUAL"
           wslabel[2] = STRING(INT(wsYear:SCREEN-VALUE)) + " BUDGET" 
           wslabel[3] = STRING(INT(wsYear:SCREEN-VALUE) - 1) + " ACTUAL" 
           wslabel[4] = STRING(INT(wsYear:SCREEN-VALUE) - 1) + " BUDGET". 
    FIND FIRST glFbal WHERE glFbal.YEAR = INT(wsYear:SCREEN-VALUE) - 1 AND glFbal.acct = glmf.acct 
                        AND glFbal.Fund = INT(wsFund:SCREEN-VALUE) NO-LOCK NO-ERROR.
    IF AVAILABLE glFbal THEN DO:
        ASSIGN wsPreA = 0
               wsPreB = 0.
        DO X = 1 TO 12:
            ASSIGN wsPreA[X] = glFbal.amt[X]
                   wsPreB[X] = glFbal.budget[X]
                   wsPreA[13] = wsPreA[13] + glFbal.amt[X]
                   wsPreB[13] = wsPreB[13] + glFbal.budget[X].
        END.
        ASSIGN wsPbf[2] = glFbal.BFBAL
               wsPreA[13] = wsPreA[13] + wsPbf[2]
               wsPBf[1] = wsPreA[13] WHEN wsPBf[1] = 0 
                                      AND glmf.ACCTYPE = "BS".

    END.
    ELSE ASSIGN wsPreA = 0
                wsPreB = 0.
    FIND FIRST glFbal WHERE glFbal.YEAR = INT(wsYear:SCREEN-VALUE) AND glFbal.acct = glmf.acct 
                        AND glFbal.Fund = INT(wsFund:SCREEN-VALUE)NO-LOCK NO-ERROR.
    IF AVAILABLE glFbal THEN DO:
         ASSIGN wsCurA = 0
                wsCurB = 0.
        DO X = 1 TO 12:
            ASSIGN wsCurA[X] = glFbal.amt[X]
                   wsCurB[X] = glFbal.budget[X]
                   wsCurA[13] = wsCurA[13] + glFbal.amt[X]
                   wsCurB[13] = wsCurB[13] + glFbal.budget[X].
        END.
        ASSIGN wsPbf[1] = glFbal.BFBAL WHEN glFbal.BFBAL <> 0
               wsAvail = wsCurB[13] - wsCurA[13]
               wscurA[13] = wscurA[13] + wsPbf[1].
    END.
    ELSE ASSIGN wsCurA = 0
                wsCurB = 0
                wsAvail = 0.
   DISPLAY wslabel[1] wslabel[2] wslabel[3] wslabel[4] wsPBf[1] wsPbf[2]
         wsCurA wsCurB wsPreA wsPreB wsAvail WITH FRAME frmEnquiry.
    IF NOT AVAILABLE glFbal THEN
        RETURN.
END.

ON CHOOSE OF btn-ok IN FRAME frmPeriod 
DO:
    ASSIGN st-per = INT(st-per:SCREEN-VALUE).
    HIDE FRAME frmPeriod.
    APPLY "close" TO THIS-PROCEDURE IN FRAME frmPeriod.
    RUN ViewTrans.ip.
    RETURN.
END.

ON CHOOSE OF btnTrans IN FRAME frmEnquiry 
DO:
  VIEW FRAME frmPeriod.
  ENABLE ALL WITH FRAME frmPeriod.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frmPeriod 
          OR CLOSE OF THIS-PROCEDURE IN FRAME frmPeriod.
  HIDE FRAME frmPeriod.
  RETURN.
END.

ON CHOOSE OF btn-link IN FRAME frm-trans 
DO:
  GET CURRENT qry-trans NO-LOCK.
  wslink = gltdf.transid.
  VIEW FRAME frm-link.
  ENABLE ALL WITH FRAME frm-link.
  OPEN QUERY qry-link FOR EACH gltdf WHERE gltdf.transid = wslink  NO-LOCK.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-link 
          OR CLOSE OF THIS-PROCEDURE IN FRAME frm-link.
  CLOSE QUERY qry-link.
  HIDE FRAME frm-link.
  RETURN.
END.

/********** MAIN LOGIC **********/
FIND FIRST simctr  NO-LOCK NO-ERROR.
st-per:SCREEN-VALUE = STRING(simctr.curper).
ENABLE ALL WITH FRAME frmEnquiry.
WAIT-FOR CHOOSE OF btn-close OR CLOSE of THIS-PROCEDURE IN FRAME frmEnquiry.
HIDE FRAME frmEnquiry.

PROCEDURE ViewTrans.ip:
  VIEW FRAME frm-trans.
  ENABLE ALL WITH FRAME frm-trans.
  OPEN QUERY qry-trans FOR EACH gltdf WHERE gltdf.acct = glmf.acct AND gltdf.period >= st-per 
                            AND gltdf.Fund = int(wsFund) NO-LOCK.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-trans 
          OR CLOSE OF THIS-PROCEDURE IN FRAME frm-trans.
  CLOSE QUERY qry-trans.
  HIDE FRAME frm-trans.
  RETURN.
END.

procedure Search.ip.
    hCol = browse  brw-ledger:current-column.
        view frame Search-opt.
        enable all with frame Search-opt.
        WAIT-FOR 'choose' of btnSearch OR 'tab' of btnSearch OR 'enter' of btnSearch 
            OR 'window-close' of frame Search-opt or close of this-procedure or
                'esc','f4' of frame Search-opt.
        hide frame Search-opt.
        case trim(hCol:label):
            when "Account" then
            do:
               OPEN QUERY qry-ledger 
                   FOR EACH glmf NO-LOCK
                            where glmf.acct >= DEC(wsSearch:SCREEN-VALUE)
                               BY glmf.acct.
            END.
            when "Description" then
            do:
               OPEN QUERY qry-ledger 
                   FOR EACH glmf NO-LOCK
                            where glmf.descrip >= wsSearch:SCREEN-VALUE
                               BY glmf.descrip.
            END. 
            when "Department" then
            do:
               OPEN QUERY qry-ledger 
                   FOR EACH glmf NO-LOCK
                            where glmf.dept >= INT(wsSearch:SCREEN-VALUE)
                               BY glmf.dept BY glmf.acct.

            END.
        END.
        RETURN.
            END.
