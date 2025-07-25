/* Program.................gle00.p
   Notes:................. Ledger Enquiries
   Author:.................S. Mawire
*/
SESSION:DATA-ENTRY-RETURN = TRUE.
&SCOPED-DEFINE tmptable             glmf
&SCOPED-DEFINE skey                 glmf.acct
DEF STREAM b.
DEF VAR wsAcc    AS CHAR FORM "x(12)".
DEF VAR wsDesc   LIKE glmf.DESCRIPTION.
DEF VAR wsMonth  AS CHAR FORM "x(5)" EXTENT 13.
DEF VAR wsCurA   AS DEC FORM "zzzzzzzz9.99-" EXTENT 13.
DEF VAR wsCurB   LIKE wsCurA.
DEF VAR wsPreA   LIKE wsCurA.
DEF VAR wsPreB   LIKE wsCurA.
DEF VAR wsYear   LIKE glbal.YEAR.
DEF VAR wsPYear  LIKE glbal.YEAR.
DEF VAR wsPBf    LIKE glbal.bfbal EXTENT 2.
DEF  VAR wsAvail LIKE glbal.bfbal.
DEF VAR  wsAn    AS LOGICAL.
DEF VAR wslabel  AS CHAR EXTENT 4 FORM "X(12)" VIEW-AS TEXT NO-UNDO.
DEF VAR X        AS INT.
DEF VAR st-per LIKE gltdf.period.
DEF VAR wslink LIKE gltdf.transid.
DEF VAR wsfile AS CHAR FORM "x(40)".

DEF BUTTON btn-Src    LABEL "SOURCE".
DEF BUTTON btn-link LABEL "LINKED Transaction".
{varlibrary.i}

DEF RECTANGLE rect-1 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL SIZE 90 BY 2.15.
DEF RECTANGLE rect-2 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL SIZE 90 by 3.2.

DEF    QUERY qry-trans FOR gltdf SCROLLING.
DEF BROWSE brw-trans QUERY qry-trans
    DISPLAY gltdf.SOURCE gltdf.period gltdf.trDATE gltdf.REF gltdf.AMT gltdf.DESCRIP FORM "x(40)" WIDTH 40 gltdf.CREDATE 
    gltdf.TransID WITH 20 DOWN SEPARATORS.

DEF    QUERY qry-link FOR gltdf SCROLLING.
DEF BROWSE brw-link QUERY qry-link
    DISPLAY gltdf.SOURCE LABEL "SOURCE" gltdf.acct gltdf.period gltdf.trDATE gltdf.REF gltdf.AMT gltdf.DESCRIP FORM "x(40)" WIDTH 30 gltdf.CREDATE 
    gltdf.TransID LABEL "TRANSACTIONID" WITH 20 DOWN SEPARATORS.

DEF    QUERY qry-dbsrc FOR dbhtf SCROLLING.
DEF BROWSE brw-dbsrc QUERY qry-dbsrc
    DISPLAY dbhtf.Accper dbhtf.dbAcc dbhtf.trDate dbhtf.DESCRIP dbhtf.Sgrp dbhtf.Ref dbhtf.Vat dbhtf.Amt dbhtf.TransID  WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-dbsrc 
    brw-dbsrc AT ROW 2 COL 5
    skip(0.5)
     btn-exit COLON 40 
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Source Transaction Listing".

DEF    QUERY qry-hssrc FOR hsehtf SCROLLING.
DEF BROWSE brw-hssrc QUERY qry-hssrc
    DISPLAY hsehtf.Accper hsehtf.dbAcc hsehtf.trDate hsehtf.DESCRIP hsehtf.Scheme hsehtf.Ref hsehtf.Vat hsehtf.Amt hsehtf.TransID  WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-hssrc 
    brw-hssrc AT ROW 2 COL 5
    skip(0.5)
     btn-exit COLON 40 
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Source Transaction Listing".

DEF    QUERY qry-cbsrc FOR cbtrans SCROLLING.
DEF BROWSE brw-cbsrc QUERY qry-cbsrc
    DISPLAY cbtrans.Accper cbtrans.bank cbtrans.trDate cbtrans.Ref cbtrans.seq  cbtrans.DESCRIP cbtrans.Amount cbtrans.TransID  WITH 20 DOWN SEPARATORS.

DEFINE FRAME frm-cbsrc 
    brw-cbsrc AT ROW 2 COL 5
    skip(0.5)
     btn-exit COLON 40 
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED TITLE "Source Transaction Listing".

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
    btn-link COLON 15 SPACE(30) btn-Src SPACE(30) btn-close
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
     wsYear LABEL "Year"   AUTO-RETURN  
     glmf.description NO-LABEL NO-TAB-STOP VIEW-AS FILL-IN SIZE 40 by 1 
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
         SIZE 98 BY 26 TITLE "LEDGER ENQUIRIES".



ON row-display OF  brw-trans DO:
   ASSIGN 
        gltdf.AMT:FGCOLOR IN BROWSE brw-trans = 12 WHEN gltdf.AMT < 0.
END.

ON row-display OF  brw-link DO:
   ASSIGN 
        gltdf.AMT:FGCOLOR IN BROWSE brw-link = 12 WHEN gltdf.AMT < 0.
END.

ON row-display OF  brw-dbsrc DO:
   ASSIGN 
        dbhtf.Amt:FGCOLOR IN BROWSE brw-dbsrc = 12 WHEN dbhtf.Amt < 0.
END.

ON row-display OF  brw-hssrc DO:
   ASSIGN 
        hsehtf.Amt:FGCOLOR IN BROWSE brw-hssrc = 12 WHEN hsehtf.Amt < 0.
END.

ON row-display OF  brw-cbsrc DO:
   ASSIGN 
        cbtrans.AmounT:FGCOLOR IN BROWSE brw-cbsrc = 12 WHEN cbtrans.amount < 0.
END.

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

ON 'tab' OF wsYear IN FRAME frmEnquiry
    OR 'enter' OF wsYear IN FRAME frmEnquiry
DO:
    ASSIGN wsCurA = 0
           wsCurB = 0
           wsPbf  = 0
           wsPreA = 0
           wsPreB = 0.
    ASSIGN wslabel[1] = STRING(INT(wsYear:SCREEN-VALUE)) + " ACTUAL"
           wslabel[2] = STRING(INT(wsYear:SCREEN-VALUE)) + " BUDGET" 
           wslabel[3] = STRING(INT(wsYear:SCREEN-VALUE) - 1) + " ACTUAL" 
           wslabel[4] = STRING(INT(wsYear:SCREEN-VALUE) - 1) + " BUDGET". 
    FIND FIRST glbal WHERE glbal.YEAR = INT(wsYear:SCREEN-VALUE) - 1 AND glbal.acct = glmf.acct NO-LOCK NO-ERROR.
    IF AVAILABLE glbal THEN DO:
        ASSIGN wsPreA = 0
               wsPreB = 0.
        DO X = 1 TO 12:
           
            ASSIGN wsPreA[X] = glbal.amt[X]
                   wsPreB[X] = glbal.budget[X]
                   wsPreA[13] = wsPreA[13] + glbal.amt[X]
                   wsPreB[13] = wsPreB[13] + glbal.budget[X].
        END.
        ASSIGN wsPbf[2] = glbal.BFBAL
               wsPreA[13] = wsPreA[13] + wsPbf[2]
               wsPBf[1] = wsPreA[13] WHEN wsPBf[1] = 0 
                                      AND glmf.ACCTYPE = "BS".

    END.
    ELSE ASSIGN wsPreA = 0
                wsPreB = 0.
    FIND FIRST glbal WHERE glbal.YEAR = INT(wsYear:SCREEN-VALUE) AND glbal.acct = glmf.acct NO-LOCK NO-ERROR.
    IF AVAILABLE glbal THEN DO:
         ASSIGN wsCurA = 0
                wsCurB = 0.
        DO X = 1 TO 12:
             
            ASSIGN wsCurA[X] = glbal.amt[X]
                   wsCurB[X] = glbal.budget[X]
                   wsCurA[13] = wsCurA[13] + glbal.amt[X]
                   wsCurB[13] = wsCurB[13] + glbal.budget[X].
            
        END.
        ASSIGN wsPbf[1] = glbal.BFBAL WHEN glbal.BFBAL <> 0
               wsAvail = wsCurB[13] - wsCurA[13]
               wscurA[13] = wscurA[13] + wsPbf[1].
    END.
    ELSE ASSIGN wsCurA = 0
                wsCurB = 0
                wsAvail = 0.
   IF wsAvail < 0 THEN DO:
       wsAvail:FGCOLOR = 12.
   END.
   ELSE wsAvail:FGCOLOR = 0.
   DISPLAY wslabel[1] wslabel[2] wslabel[3] wslabel[4] wsPBf[1] wsPbf[2]
         wsCurA wsCurB wsPreA wsPreB wsAvail WITH FRAME frmEnquiry.
    IF NOT AVAILABLE glbal THEN
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

ON 'right-mouse-click':U OF brw-trans 
DO:
    wsFile = simctr.repDir + "gle00" 
          + string(day(today),"99")    + "_"
          + string(month(today),"99")  + "_"
          + string(year(today),"9999") + "_"
          + substr(string(time,"HH:MM:SS"),1,2)
          + substr(string(time,"HH:MM:SS"),4,2)
          + substr(string(time,"HH:MM:SS"),7,2)
          + ".csv".
    GET CURRENT qry-trans NO-LOCK.
    wslink = gltdf.acct.
    OUTPUT STREAM b TO VALUE(wsfile).
    FOR EACH gltdf WHERE gltdf.acct = wslink NO-LOCK:
        EXPORT STREAM b DELIMITER ',' gltdf.SOURCE gltdf.acct gltdf.period gltdf.trDATE gltdf.REF gltdf.AMT gltdf.DESCRIP 
            gltdf.CREDATE gltdf.TransID. 
    END.
    wsfile = "START " + wsFile.
    OUTPUT STREAM b CLOSE.
    OS-COMMAND NO-WAIT VALUE(wsFile).
    RETURN.
END.

ON 'right-mouse-click':U OF brw-link 
DO:
    wsFile = simctr.repDir + "gle00" 
          + string(day(today),"99")    + "_"
          + string(month(today),"99")  + "_"
          + string(year(today),"9999") + "_"
          + substr(string(time,"HH:MM:SS"),1,2)
          + substr(string(time,"HH:MM:SS"),4,2)
          + substr(string(time,"HH:MM:SS"),7,2)
          + ".csv".
    GET CURRENT qry-trans NO-LOCK.
    wslink = gltdf.transid.
    OUTPUT STREAM b TO VALUE(wsfile).
    FOR EACH gltdf WHERE gltdf.transid = wslink NO-LOCK:
        EXPORT STREAM b DELIMITER ',' gltdf.SOURCE gltdf.acct gltdf.period gltdf.trDATE gltdf.REF gltdf.AMT gltdf.DESCRIP 
            gltdf.CREDATE gltdf.TransID. 
    END.
    wsfile = "START " + wsFile.
    OUTPUT STREAM b CLOSE.
    OS-COMMAND NO-WAIT VALUE(wsFile).
    RETURN.
END.

ON 'choose':U OF btn-src IN FRAME frm-trans 
DO:
    RUN source-ip.
    RETURN.
END.
/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK NO-ERROR.
st-per:SCREEN-VALUE = STRING(simctr.curper).
ENABLE ALL WITH FRAME frmEnquiry.
WAIT-FOR CHOOSE OF btn-close OR CLOSE of THIS-PROCEDURE IN FRAME frmEnquiry.
HIDE FRAME frmEnquiry.

PROCEDURE ViewTrans.ip:
  VIEW FRAME frm-trans.
  ENABLE ALL WITH FRAME frm-trans.
  OPEN QUERY qry-trans FOR EACH gltdf WHERE gltdf.acct = glmf.acct AND gltdf.period >= st-per NO-LOCK.
  WAIT-FOR CHOOSE OF btn-close IN FRAME frm-trans 
          OR CLOSE OF THIS-PROCEDURE IN FRAME frm-trans.
  CLOSE QUERY qry-trans.
  HIDE FRAME frm-trans.
  RETURN.
END.

PROCEDURE SEARCH.ip.
    hCol = browse  brw-ledger:current-column.
    view frame Search-opt.
    enable all with frame Search-opt.
    WAIT-FOR 'choose' of btnSearch OR 'tab' of btnSearch OR 'enter' of btnSearch 
            OR 'window-close' of frame Search-opt or close of this-procedure or
                'esc','f4' of frame Search-opt.
    hide frame Search-opt.
    CASE trim(hCol:label):
         WHEN "Account" THEN DO:
               OPEN QUERY qry-ledger 
                   FOR EACH glmf NO-LOCK
                            where glmf.acct >= DEC(wsSearch:SCREEN-VALUE) USE-INDEX acct.
         END.
         WHEN "Description" THEN DO:
               OPEN QUERY qry-ledger 
                   FOR EACH glmf NO-LOCK
                            where glmf.descrip >= wsSearch:SCREEN-VALUE USE-INDEX NAME.
         END. 
         WHEN "Department" THEN DO:
               OPEN QUERY qry-ledger 
                   FOR EACH glmf NO-LOCK
                            where glmf.dept >= INT(wsSearch:SCREEN-VALUE)
                               BY glmf.dept BY glmf.acct.

         END.
    END.
    RETURN.
END PROCEDURE.

PROCEDURE source-ip:
    CASE gltdf.SOURCE:
        WHEN "DB" THEN DO:
            VIEW FRAME frm-dbsrc.
            ENABLE ALL WITH FRAME frm-dbsrc.
            FIND FIRST dbsgr WHERE dbsgr.ctrledger = gltdf.acct NO-LOCK NO-ERROR.
             OPEN QUERY qry-dbsrc FOR EACH dbhtf WHERE dbhtf.TransId = gltdf.transId AND dbhtf.Sgrp = dbsgr.sgrp
                                     AND dbhtf.Accper = gltdf.per USE-INDEX dbacc NO-LOCK.
             WAIT-FOR CHOOSE OF btn-exit IN FRAME frm-dbsrc 
                      OR CLOSE OF THIS-PROCEDURE IN FRAME frm-dbsrc.
             CLOSE QUERY qry-dbsrc.
             HIDE FRAME frm-dbsrc.
        END.
        WHEN "HS" THEN DO:
            VIEW FRAME frm-hssrc.
            ENABLE ALL WITH FRAME frm-hssrc.
            OPEN QUERY qry-hssrc FOR EACH hsehtf WHERE hsehtf.TransId = gltdf.transId AND hsehtf.Accper = gltdf.per USE-INDEX dbacc NO-LOCK.
             WAIT-FOR CHOOSE OF btn-exit IN FRAME frm-hssrc 
                      OR CLOSE OF THIS-PROCEDURE IN FRAME frm-hssrc.
             CLOSE QUERY qry-hssrc.
             HIDE FRAME frm-hssrc.
        END.
        WHEN "CB" THEN DO:
            VIEW FRAME frm-cbsrc.
            ENABLE ALL WITH FRAME frm-cbsrc.
            OPEN QUERY qry-cbsrc FOR EACH cbtrans WHERE cbtrans.TransId = gltdf.transId /*AND cbTrans.Ledger = glbal.acct AND cbtrans.Ref = tmptdf.Ref*/ 
                                 AND cbTrans.Accper = gltdf.per  NO-LOCK BY cbtrans.ref BY cbtrans.seq.
             WAIT-FOR CHOOSE OF btn-exit IN FRAME frm-cbsrc 
                      OR CLOSE OF THIS-PROCEDURE IN FRAME frm-cbsrc.
             CLOSE QUERY qry-cbsrc.
             HIDE FRAME frm-cbsrc.
        END.
        OTHERWISE DO:
            IF gltdf.SOURCE = "RE" THEN DO:
                FIND FIRST dbsgr WHERE dbsgr.ctrledger = gltdf.acct NO-LOCK NO-ERROR. /* DB */
                IF AVAILABLE dbsgr THEN DO:
                    VIEW FRAME frm-dbsrc.
                    ENABLE ALL WITH FRAME frm-dbsrc.
                    OPEN QUERY qry-dbsrc FOR EACH dbhtf WHERE dbhtf.TransId = gltdf.transId AND dbhtf.Sgrp = dbsgr.sgrp
                                             AND dbhtf.Accper = gltdf.per USE-INDEX dbacc NO-LOCK.
                     WAIT-FOR CHOOSE OF btn-exit IN FRAME frm-dbsrc 
                              OR CLOSE OF THIS-PROCEDURE IN FRAME frm-dbsrc.
                     CLOSE QUERY qry-dbsrc.
                     HIDE FRAME frm-dbsrc.
                END. /* eof db source */
                ELSE IF NOT AVAILABLE dbsgr THEN DO: /* HSE */
                    FIND FIRST hsesch WHERE (hsesch.CtrLedger =  gltdf.acct OR hsesch.ILedger =  gltdf.acct OR hsesch.CLedger = gltdf.acct) NO-LOCK NO-ERROR.
                    IF AVAILABLE hsesch THEN DO:
                        VIEW FRAME frm-hssrc.
                        ENABLE ALL WITH FRAME frm-hssrc.
                        OPEN QUERY qry-hssrc FOR EACH hsehtf WHERE hsehtf.TransId = gltdf.transId AND hsehtf.Accper = gltdf.per USE-INDEX dbacc NO-LOCK.
                         WAIT-FOR CHOOSE OF btn-exit IN FRAME frm-hssrc 
                                  OR CLOSE OF THIS-PROCEDURE IN FRAME frm-hssrc.
                         CLOSE QUERY qry-hssrc.
                         HIDE FRAME frm-hssrc.
                    END. /* eof hse available */
                    ELSE DO: /* CB */
                        VIEW FRAME frm-cbsrc.
                        ENABLE ALL WITH FRAME frm-cbsrc.
                        OPEN QUERY qry-cbsrc FOR EACH cbtrans WHERE cbtrans.TransId = gltdf.transId 
                                             AND cbTrans.Accper = gltdf.per  NO-LOCK BY cbtrans.ref BY cbtrans.seq.
                         WAIT-FOR CHOOSE OF btn-exit IN FRAME frm-cbsrc 
                                  OR CLOSE OF THIS-PROCEDURE IN FRAME frm-cbsrc.
                         CLOSE QUERY qry-cbsrc.
                         HIDE FRAME frm-cbsrc.
                    END. /* eof CB source */
                END. 
            END. /* eof if... RE */
        END.
    END CASE.
END PROCEDURE.




