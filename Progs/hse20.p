/*Program.................................hse20.p
  Notes.................Capital Balance Report
  Author..................S. Chisoro
*/

&SCOPED-DEFINE wsTitle           "CAPITAL BALANCES REPORT"

session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF STREAM b.

DEF VAR wsFile AS CHAR FORM "x(40)".    
DEF VAR wsTitle AS CHAR FORM "x(80)".
DEF VAR w-orientation AS CHAR      INITIAL "Landscape"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10.0          NO-UNDO.
DEF VAR wsUser AS CHAR FORM "xxx" INITIAL "01".
DEF VAR wsYear AS INT FORM "9999".
DEF VAR wsMonth AS INT FORM "99".
DEF SHARED VAR wsName LIKE simusr.Name.
DEF VAR swAmt LIKE hsecmf.AmTdUE EXTENT 2.
DEF VAR wsTotal LIKE hsecmf.AmtDue EXTENT 5.
DEF VAR wsid LIKE hsesch.scheme.
DEF VAR wsstart LIKE hsesch.scheme.
DEF VAR wsend LIKE hsesch.scheme.

DEF BUTTON btn-Print   LABEL "PRINT".
DEF BUTTON btn-close  LABEL "CLOSE".
DEF BUTTON btn-Exp LABEL "EXPORT".

DEF TEMP-TABLE tmptable 
    FIELD wsScheme LIKE hsesch.scheme
    FIELD wsDescript LIKE hsesch.Descrip
    FIELD wsAmtDue LIKE hsecmf.AmTdUE EXTENT 2.
DEF TEMP-TABLE tmptable1
    FIELD wsAcc LIKE hsecmf.dbacc
    FIELD wsName LIKE hsecmf.NAME
    FIELD wsStand LIKE hsecmf.standno
    FIELD wsCapital LIKE hsecmf.bal
    FIELD wsInst LIKE hsecmf.insAmt
    FIELD wsLAmtDue LIKE hsecmf.LAmtDue
    FIELD wsAmtDue LIKE hsecmf.AmtDue.


DEF QUERY qry-tmptable FOR tmptable SCROLLING.
DEF BROWSE brw-tmptable QUERY qry-tmptable
    DISPLAY tmptable.wsscheme COLUMN-LABEL "SCHEME"
    WIDTH 10 tmptable.wsdescript COLUMN-LABEL "SCHEME NAME" 
    WIDTH 60 tmptable.wsAmtDue[1] COLUMN-LABEL "ACTUAL! BALANCE" 
    WIDTH 15 tmptable.wsAmtDue[2] COLUMN-LABEL "RATED !BALANCE" WIDTH 15
    WITH 22 DOWN SEPARATORS  NO-BOX LABEL-FGCOLOR 9 LABEL-BGCOLOR 3 
    LABEL-FONT 6.

DEF QUERY qry-tmptable1 FOR tmptable1 SCROLLING.
DEF BROWSE brw-tmptable1 QUERY qry-tmptable1
    DISPLAY tmptable1.wsAcc COLUMN-LABEL "ACCOUNT"
    WIDTH 10 tmptable1.wsName COLUMN-LABEL "NAME" 
    WIDTH 40 tmptable1.wsStand COLUMN-LABEL "STAND NUMBER"
    WIDTH 15 tmptable1.wsInst COLUMN-LABEL "INSTALLMENT"
    WIDTH 23 tmptable1.wsLAmtDue COLUMN-LABEL "LAST DUE"
    WIDTH 23 tmptable1.wsAmtDue[1] COLUMN-LABEL "TRADING !AMOUNT DUE" 
    WIDTH 23 tmptable1.wsAmtDue[2] COLUMN-LABEL "ACCOUNTING !AMOUNT DUE" 
    WIDTH 23 tmptable1.wsCapital COLUMN-LABEL "CAPITAL BALANCE" WIDTH 23
    WITH 20 DOWN SEPARATORS  NO-BOX LABEL-FGCOLOR 9 LABEL-BGCOLOR 3 
    LABEL-FONT 6.

DEF FRAME frm-main
    brw-tmptable AT ROW 1.5 COL 5
    btn-Print AT ROW 20.7 COL 10
    SPACE (20) "Double Click Scheme For More Details" FGCOLOR 12
    space(15) btn-close SKIP(1)
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
    SIZE 112 BY 23.5
    TITLE {&wsTitle} VIEW-AS DIALOG-BOX.

define frame frm-detail
     tmptable.wsScheme    AT ROW 1 COL 5 LABEL "SCHEME" view-as TEXT SPACE(5) 
     tmptable.wsdescript    NO-LABEL VIEW-AS TEXT NO-TAB-STOP SKIP(1)
     brw-tmptable1  AT ROW 3 COL 5
     btn-Print AT ROW 21 COL 10 LABEL "Print"
    SPACE (70) btn-Exp
    SPACE (70) btn-close SKIP(1)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-VALIDATE
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE CENTERED
    SIZE 193 BY 23.5
    TITLE "Scheme Details".

DEF FRAME frm-print
    SKIP(1)
    wsstart   LABEL "Start Scheme"   AT ROW 2 COL 3 
    wsend    LABEL  "End Scheme"    AT ROW 4 COL 3
    btn-print AT ROW 6 COL 3
    SPACE(5) btn-Exp SPACE(5) btn-close
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 45 BY 8
    TITLE "PRINT BALANCE REPORT" VIEW-AS DIALOG-BOX KEEP-TAB-ORDER.


FORM 
    tmptable1.wsAcc COLUMN-LABEL "ACCOUNT"
    tmptable1.wsName COLUMN-LABEL "NAME"   FORM "x(30)"
    tmptable1.wsStand COLUMN-LABEL "STAND!NUMBER" FORM "x(8)"
    tmptable1.wsCapital COLUMN-LABEL "CAPITAL!BALANCE"
    tmptable1.wsInst COLUMN-LABEL "INSTALLMENT"
    tmptable1.wsAmtDue[1] COLUMN-LABEL "TRADING!AMOUNT DUE" 
    tmptable1.wsAmtDue[2] COLUMN-LABEL "ACCOUNTING!AMOUNT DUE" 
    HEADER SKIP(1) wsTitle AT 20 skip(1) "                         CAPITAL BALANCE REPORT AS AT:" AT 10 SPACE(2)
    STRING(wsYear) + STRING(wsMonth,"99") SKIP(1)
    "Page: " AT 110 PAGE-NUMBER(a) SKIP(2)
    WITH DOWN STREAM-IO FONT 6 CENTERED NO-BOX
    WIDTH 132 FRAME frm-rpt.



FOR EACH hsesch:
    swAmt[1] = 0.00.
    swamt[2] = 0.00.
    FOR EACH hsecmf WHERE hsesch.scheme = hsecmf.scheme:
        swAmt[1] = swAmt[1] + bal.
    END.
    FIND FIRST tblForex WHERE tblForex.txtCur = hsesch.txtCur NO-ERROR.
    IF AVAILABLE tblForex THEN DO:
        swamt[2] = swAmt[1] * tblForex.decRate.
    END.
    CREATE tmptable.
    ASSIGN
        wsScheme = hsesch.scheme
        wsDescript = hsesch.Descrip
        tmptable.wsAmtDue[1] = swAmt[1]
        tmptable.wsAmtDue[2] = swamt[2].
END.
/*triigers*/

ON 'enter':u OF brw-tmptable
    OR 'mouse-select-dblclick' OF brw-tmptable
DO:
    GET CURRENT qry-tmptable EXCLUSIVE-LOCK NO-WAIT.
    run proc-detailed.
    RETURN.
END.

ON CHOOSE OF btn-Print IN FRAME frm-main
DO:
  wsStart = 0.
  wsEnd = 99999.
  VIEW FRAME frm-print.
   ENABLE ALL WITH FRAME frm-print.
   DISPLAY wsStart wsEnd WITH FRAME frm-print.
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-print.
   HIDE FRAME frm-print.
 RETURN.
END.

ON 'choose':U OF btn-Print IN FRAME frm-print 
DO:
  session:set-wait-state("").
  wsStart = INTEGER(wsStart:SCREEN-VALUE).
  wsEnd = INTEGER(wsEnd:SCREEN-VALUE).
 {PrintOpt.i &stream-name="stream a"
                    &print-prog="report1.ip"
                    &paged}

    RETURN.
END.

ON CHOOSE OF btn-Print IN FRAME frm-detail
DO:
  session:set-wait-state("").
 {PrintOpt.i &stream-name="stream a"
                    &print-prog="report.ip"
                    &paged}
 RETURN.
END.
ON 'choose':U OF btn-Exp IN FRAME frm-print 
DO:
    session:set-wait-state("").
    wsStart = INTEGER(wsStart:SCREEN-VALUE).
    wsEnd = INTEGER(wsEnd:SCREEN-VALUE).
      wsFile = simctr.repDir + "hse20" 
             + string(day(today),"99")    + "_"
             + string(month(today),"99")  + "_"
             + string(year(today),"9999") + "_"
             + substr(string(time,"HH:MM:SS"),1,2)
             + substr(string(time,"HH:MM:SS"),4,2)
             + substr(string(time,"HH:MM:SS"),7,2)
             + ".csv".    
      OUTPUT STREAM b TO VALUE(wsFile).
      RUN Expo1.ip.
      wsfile = "START " + wsFile.
      OUTPUT STREAM b CLOSE.
      OS-COMMAND NO-WAIT VALUE(wsFile).
    RETURN.
END.
 

ON CHOOSE OF btn-Exp IN FRAME frm-detail
DO:
    session:set-wait-state("").
      wsFile = simctr.repDir + "hse20" 
             + string(day(today),"99")    + "_"
             + string(month(today),"99")  + "_"
             + string(year(today),"9999") + "_"
             + substr(string(time,"HH:MM:SS"),1,2)
             + substr(string(time,"HH:MM:SS"),4,2)
             + substr(string(time,"HH:MM:SS"),7,2)
             + ".csv".    
      OUTPUT STREAM b TO VALUE(wsFile).
      RUN Expo.ip.
      wsfile = "START " + wsFile.
      OUTPUT STREAM b CLOSE.
      OS-COMMAND NO-WAIT VALUE(wsFile).
      RETURN.
   END.


   /*main logic*/

   FIND FIRST simctr.
   ASSIGN wsYear = YEAR(TODAY)
          wsMonth = MONTH(TODAY)
          wsTitle = simctr.CONAME.
   OPEN QUERY qry-tmptable FOR EACH tmptable NO-LOCK .
 ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-main.
CLOSE QUERY qry-tmptable.
HIDE FRAME frm-main.

PROCEDURE proc-detailed:
   ASSIGN wsid = tmptable.wsscheme.
   FOR EACH tmptable1:
       DELETE tmptable1.
   END.
   FIND FIRST tmptable WHERE tmptable.wsscheme = wsid EXCLUSIVE-LOCK NO-ERROR.
   FOR EACH hsecmf WHERE wsid = hsecmf.scheme AND hsecmf.accStat = 0:
        CREATE tmptable1.
        ASSIGN
            tmptable1.wsAcc = hsecmf.dbacc
            tmptable1.wsName = hsecmf.NAME
            tmptable1.wsStand = hsecmf.standno
            tmptable1.wsCapital = hsecmf.bal
            tmptable1.wsInst = hsecmf.insAmt
            tmptable1.wsLAmtDue = hsecmf.LAmtDue
            tmptable1.wsAmtDue[1] = hsecmf.AmtDue[1]
            tmptable1.wsAmtDue[2] = hsecmf.AmtDue[2].
    END. 
    OPEN QUERY qry-tmptable1 FOR EACH tmptable1 NO-LOCK .
   VIEW FRAME frm-detail.
   ENABLE ALL WITH FRAME frm-detail.
   DISPLAY tmptable.wsScheme tmptable.wsdescript WITH FRAME frm-detail.
   WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-detail.
   CLOSE QUERY qry-tmptable1.
   HIDE FRAME frm-detail.
END.

PROCEDURE report.ip:
    FIND FIRST tmptable WHERE tmptable.wsscheme = wsid EXCLUSIVE-LOCK NO-ERROR.
    DOWN 1 STREAM a WITH FRAME frm-rpt.
    DISPLAY STREAM a  wsid @  tmptable1.wsAcc
          UPPER(tmptable.wsdescript) @ tmptable1.wsName WITH FRAME frm-rpt.
            DOWN 1 STREAM a WITH FRAME frm-rpt.
   wsTotal[1] = 0.
   wsTotal[2] = 0.
   wsTotal[3] = 0.
   wsTotal[4] = 0.
   wsTotal[5] = 0.
   FOR EACH tmptable1:
           wsTotal[1] = wsTotal[1] + tmptable1.wsCapital.
           wsTotal[2] =  wsTotal[2] + tmptable1.wsInst.
           /*wsTotal[3] =  wsTotal[3] + tmptable1.wsLAmtDue.*/
           wsTotal[4] =  wsTotal[4] + tmptable1.wsAmtDue[1].
           wsTotal[5] =  wsTotal[5] + tmptable1.wsAmtDue[2].
           DISPLAY STREAM a tmptable1.wsAcc
                tmptable1.wsName
                tmptable1.wsStand tmptable1.wsInst 
                tmptable1.wsAmtDue[1] 
                tmptable1.wsAmtDue[2]
                tmptable1.wsCapital WITH FRAME frm-rpt.
                             DOWN STREAM a WITH FRAME frm-rpt.
   END.
   UNDERLINE STREAM a tmptable1.wsName tmptable1.wsInst
                tmptable1.wsAmtDue[1] 
                tmptable1.wsAmtDue[2]
                tmptable1.wsCapital
                       WITH FRAME frm-rpt.
           DISPLAY  STREAM a "SCHEME TOTALS" @ tmptable1.wsName
                        wsTotal[2] @ tmptable1.wsInst  
                wsTotal[4] @ tmptable1.wsAmtDue[1] 
                wsTotal[5] @ tmptable1.wsAmtDue[2]
                wsTotal[1] @ tmptable1.wsCapital
                       WITH FRAME frm-rpt.
          
           DOWN 2 STREAM a WITH FRAME frm-rpt.
         
   RETURN.
END.


PROCEDURE report1.ip:
   
  FOR EACH hsesch WHERE  hsesch.scheme >= wsStart AND  hsesch.scheme <= wsEnd:
   wsTotal[1] = 0.
   wsTotal[2] = 0.
   wsTotal[3] = 0.
   wsTotal[4] = 0.
   wsTotal[5] = 0.
          DOWN 1 STREAM a WITH FRAME frm-rpt.
          DISPLAY STREAM a hsesch.scheme @  tmptable1.wsAcc
          UPPER(hsesch.descrip) @ tmptable1.wsName WITH FRAME frm-rpt.
            DOWN 1 STREAM a WITH FRAME frm-rpt.
    FOR EACH hsecmf WHERE hsecmf.scheme = hsesch.scheme AND hsecmf.accStat = 0:
             wsTotal[1] = wsTotal[1] + bal.
             wsTotal[2] =  wsTotal[2] + InsAmt.
             wsTotal[4] =  wsTotal[4] + AmtDue[1].
             wsTotal[5] =  wsTotal[5] + AmtDue[2].
             DISPLAY STREAM a dbAcc @ tmptable1.wsAcc
                  NAME @ tmptable1.wsName
                  StandNo @ tmptable1.wsStand InsAmt @ tmptable1.wsInst 
                  AmtDue[1] @ tmptable1.wsAmtDue[1] 
                  AmtDue[2] @ tmptable1.wsAmtDue[2]
                  bal @ tmptable1.wsCapital WITH FRAME frm-rpt.
                               DOWN STREAM a WITH FRAME frm-rpt.
     END.
     UNDERLINE STREAM a tmptable1.wsName tmptable1.wsInst
                  tmptable1.wsAmtDue[1] 
                  tmptable1.wsAmtDue[2]
                  tmptable1.wsCapital
                         WITH FRAME frm-rpt.
             DISPLAY  STREAM a "SCHEME TOTALS" @ tmptable1.wsName
                          wsTotal[2] @ tmptable1.wsInst  
                  wsTotal[4] @ tmptable1.wsAmtDue[1] 
                  wsTotal[5] @ tmptable1.wsAmtDue[2]
                  wsTotal[1] @ tmptable1.wsCapital
                         WITH FRAME frm-rpt.
             DOWN 2 STREAM a WITH FRAME frm-rpt.

   END.   
 RETURN.
END.

PROCEDURE expo.ip:
   FIND FIRST tmptable WHERE tmptable.wsscheme = wsid EXCLUSIVE-LOCK NO-ERROR.
   EXPORT STREAM b DELIMITER ',' "          CAPITAL BALANCE REPORT AS AT:" + STRING(wsYear) + STRING(wsMonth,"99").
   EXPORT STREAM b DELIMITER ',' "  ".
   EXPORT STREAM b DELIMITER ',' "ACCOUNT" "NAME" "STAND NUMBER" "CAPITAL BALANCE""INSTALLMENT"
    "TRADING AMOUNT DUE" "ACCOUNTING AMOUNT DUE".
   EXPORT STREAM b DELIMITER ',' wsid UPPER(tmptable.wsdescript).
   wsTotal[1] = 0.
   wsTotal[2] = 0.
   wsTotal[3] = 0.
   wsTotal[4] = 0.
   wsTotal[5] = 0.
   FOR EACH tmptable1:
           wsTotal[1] = wsTotal[1] + tmptable1.wsCapital.
           wsTotal[2] =  wsTotal[2] + tmptable1.wsInst.
           wsTotal[4] =  wsTotal[4] + tmptable1.wsAmtDue[1].
           wsTotal[5] =  wsTotal[5] + tmptable1.wsAmtDue[2].
           EXPORT STREAM b DELIMITER ',' tmptable1.wsAcc tmptable1.wsName tmptable1.wsStand  tmptable1.wsCapital tmptable1.wsInst 
                tmptable1.wsAmtDue[1] 
                tmptable1.wsAmtDue[2].
             END.
   EXPORT  STREAM b DELIMITER ',' "SCHEME TOTALS" "" "" wsTotal[1] wsTotal[2]  wsTotal[4] wsTotal[5].
   EXPORT  STREAM b DELIMITER ',' "".             
   EXPORT  STREAM b DELIMITER ',' "".      
   RETURN.
END.

PROCEDURE expo1.ip:
   
   EXPORT STREAM b DELIMITER ',' "          CAPITAL BALANCE REPORT AS AT:" + STRING(wsYear) + STRING(wsMonth,"99").
   EXPORT STREAM b DELIMITER ',' "  ".
   EXPORT STREAM b DELIMITER ',' "ACCOUNT" "NAME" "STAND NUMBER" "CAPITAL BALANCE""INSTALLMENT"
    "TRADING AMOUNT DUE" "ACCOUNTING AMOUNT DUE".
   EXPORT STREAM b DELIMITER ',' "".
   FOR EACH hsesch WHERE  hsesch.scheme >= wsStart AND  hsesch.scheme <= wsEnd:
       wsTotal[1] = 0.
       wsTotal[2] = 0.
       wsTotal[3] = 0.
       wsTotal[4] = 0.
       wsTotal[5] = 0.
       EXPORT STREAM b DELIMITER ',' hsesch.scheme UPPER(hsesch.descrip).
       FOR EACH hsecmf WHERE hsecmf.scheme = hsesch.scheme AND hsecmf.accStat = 0:
           wsTotal[1] = wsTotal[1] + bal.
             wsTotal[2] =  wsTotal[2] + InsAmt.
             wsTotal[4] =  wsTotal[4] + AmtDue[1].
             wsTotal[5] =  wsTotal[5] + AmtDue[2].
           EXPORT STREAM b DELIMITER ',' dbAcc Name StandNo Bal insAmt 
                AmtDue[1] 
               AmtDue[2].
      END.
    EXPORT  STREAM b DELIMITER ',' "SCHEME TOTALS" "" "" wsTotal[1] wsTotal[2]  wsTotal[4] wsTotal[5].
    EXPORT  STREAM b DELIMITER ',' "".             
    EXPORT  STREAM b DELIMITER ',' "".
   END.  
   RETURN.
END.
