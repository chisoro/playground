/*Program.................................hse08.p
  Notes.................Statement Print
  Author..................S. Chisoro
*/
session:DATA-ENTRY-RETURN = TRUE.
/*&SCOPED-DEFINE pgorientation             "POTRAIT" */

DEF STREAM a.

DEF VAR wsFile AS CHAR FORM "x(40)".    
DEF VAR wsTitle AS CHAR FORM "x(80)".
DEF VAR w-orientation AS CHAR      INITIAL "POTRAIT"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10.0          NO-UNDO.
DEF VAR wsUser AS CHAR FORM "xxx" INITIAL "01".
DEF VAR wsYear AS INT FORM "9999".
DEF VAR wsMonth AS INT FORM "99".
DEF SHARED VAR wsName LIKE simusr.Name.
DEF VAR wsBal   LIKE hsemtf.Amt.
DEF VAR wsTotal LIKE hsemtf.Amt.
DEF VAR wsAge   LIKE wsBal EXTENT 6.
DEF VAR wsVat   LIKE hsemtf.vat.
DEF VAR wsAcc   LIKE hsecmf.dbAcc.
DEF VAR wsDate AS DATE.
DEF VAR wscell AS CHAR FORM "x(40)".
DEF VAR X AS INT.
DEF VAR wsstart LIKE hsecmf.dbAcc.
DEF VAR wsend LIKE hsecmf.dbAcc.
DEF VAR wsAccDate AS DATE.
DEF VAR wsDueDate AS DATE.
DEF VAR wsZero AS LOGICAL.
DEF VAR wsMessage1 AS CHAR FORM "x(60)".
DEF VAR wsMessage2 AS CHAR FORM "x(60)".
DEF VAR wsStatus   LIKE hsecmf.dbacc.
DEF VAR wsline AS CHAR FORM "x(80)"
    INIT "=============================================================================================".


DEF BUTTON btn-Print   LABEL "PRINT".
DEF BUTTON btn-close  LABEL "CANCEL".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 97 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 97 by 12.5.

DEF FRAME frm-main
    SKIP(1.5)
    wsStart     LABEL "Start Account" COLON 30 SKIP(0.5)
    wsEnd       LABEL "End Account"  COLON 30 SKIP(0.5)
    wsAccDate  LABEL "Accounting Date" COLON 30 SPACE(10)
    wsDueDate  LABEL "Due Date"                   SKIP(0.5)
    wsDate     LABEL "Last Receipt Date" COLON 30 SKIP(0.5)
    wsZero      LABEL "Print Zero Balance Accounts" COLON 30 SKIP(0.5)
     wsmessage1 LABEL "MESSAGE" COLON 15 SKIP          
     wsmessage2 NO-LABEL        COLON 15
    SKIP(1)
    wsStatus    COLON 30 LABEL " Processing Account......." VIEW-AS TEXT
    btn-Print AT ROW 14.7 COL 20
    space(50) btn-close SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 14 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 105 BY 17.5 KEEP-TAB-ORDER
    TITLE "MONTHLY STATEMENT PRINT" VIEW-AS DIALOG-BOX.   

FORM 
    wsTitle  COLON 5
       SKIP(2)
    /* SKIP(2)
             
     "VAT REG. NO.: "  AT 52 simctr.regno SKIP
     "INVOICE NO.: "     COLON 5 varInvoice
     "BPN.        : "   AT 52 SIMCTR.BPNO*/ SKIP 
   "ACCOUNT:"          COLON 66 hsecmf.dbacc   
    hsecmf.NAME         COLON 5  
    hsecmf.Add1         COLON 5 
    "STAND NUMBER: "    COLON 66 hsecmf.standNo
    hsecmf.Add2         COLON 5 
    hsecmf.Add3         COLON 5
   "CELL:"              COLON 5 wsCell    
   "E-MAIL:"            COLON 5 hsecmf.emailAdd 
   "PURCHASE PRICE  :"  COLON 66 Hsecmf.PPrice
   "CAPITAL BALANCE :"  COLON 66 Hsecmf.Bal 
   "PURCHASE RATE   :"  COLON 66 Hsecmf.PRate 
   "PURCHASE DATE   :"  COLON 66 Hsecmf.PDate 
   "REMAINING PERIOD:"  COLON 66 Hsecmf.Period  SKIP(1)
   "LAST RECEIPT DATE.:" COLON 65 hsemtf.trDate SKIP(2)
    "ACC DATE.: " COLON 5 wsAccDate SPACE(3)
    "DUE DATE.: "  wsDueDate SPACE(3)
    HEADER skip(1) "                                   LAND SALE STATEMENT" SKIP(1)
   WITH DOWN STREAM-IO FONT 8 CENTERED NO-BOX NO-LABELS
    WIDTH 132 FRAME  frm-rpt.

form                                              /* tran line */
    hsemtf.trDate  LABEL "DATE" AT 7
    hsemtf.Ref     LABEL "REFERENCE" form "x(12)"
    hsemtf.DESCRIP LABEL "DESCRIPTION" form "x(30)"
    hsemtf.Vat     LABEL "VAT"
    hsemtf.Amt     LABEL "AMOUNT"
    WITH DOWN STREAM-IO FONT 8 CENTERED NO-BOX NO-LABELS
    WIDTH 132 FRAM frm-trans.

FORM
     wsline          COLON 5 SKIP
     "CREDIT"        COLON 5
     "  90-days+"    SPACE (3)
     "   60-days"    SPACE (3)
     "   30-days"    SPACE (5)
     "CURRENT"       SPACE (8)
     "AMOUNT DUE"    SPACE (5)
     skip
     wsAge[1]      format "zzzzzzz9.99-"    
     wsage[5]  format "zzzzzzz9.99-" 
     wsage[4]  format "zzzzzzz9.99-" 
     wsage[3]  format "zzzzzzz9.99-" 
     wsage[2]  format "zzzzzzz9.99-" SPACE(3)      
     wsAge[6]  format "zzzzzzzz9.99-" 
     wsline    COLON 5 SKIP(1)
     wsmessage1                 COLON 5          
     wsmessage2                 COLON 5
     with width 132
	  no-box
	  no-labels
      stream-io
	  FRAME frm-footer.

/*triggers*/
ON 'choose':U OF btn-Print  
DO:
   session:set-wait-state("").
   ASSIGN  wsStart = DEC(wsStart:SCREEN-VALUE)
           wsEnd   = DEC(wsEnd:SCREEN-VALUE)
           wsAccDate 
           wsDueDate
           wsDate
           wsZero =  LOGICAL(wsZero:SCREEN-VALUE).
           wsMessage1 = wsMessage1:SCREEN-VALUE IN FRAME frm-main.
           wsMessage2 = wsMessage2:SCREEN-VALUE IN FRAME frm-main.
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="report.ip"
                    &paged} 
  MESSAGE "Statement Print Completed" VIEW-AS ALERT-BOX.
  APPLY 'close' TO THIS-PROCEDURE.
  HIDE FRAME frm-main.
  RETURN.
END.

/*Main Logic*/
FIND FIRST simctr.
   ASSIGN wsYear = YEAR(TODAY)
          wsMonth = MONTH(TODAY)
          wsTitle = simctr.CONAME.
   wsDate = TODAY.
   wsZero:SCREEN-VALUE = "NO".
   wsstart:SCREEN-VALUE = "0".
   wsend:SCREEN-VALUE = "999999999998".
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-close OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.

/*Procedures*/
PROCEDURE report.ip:
  FIND FIRST simctr.
  FOR EACH hsecmf WHERE  hsecmf.dbacc >= wsStart AND  hsecmf.dbacc <= wsEnd:
      IF wsZero = NO AND hsecmf.AmtDue[1] = 0 THEN NEXT.
      DISPLAY hsecmf.dbacc @ wsStatus WITH FRAME frm-main.
      PAUSE 0.
      wsage = 0.
      REPEAT:
            FIND NEXT hseblf WHERE hseblf.dbacc = hsecmf.dbacc NO-LOCK NO-ERROR.
            IF AVAILABLE hseblf THEN DO:
                ASSIGN wsage[6] = wsage[6] + INT.
                       wsage[3] = wsage[3] + INT.
                DO X = 1 TO 15:
                    ASSIGN wsage[1] = wsage[1] + hseblf.amt[1] WHEN X = 1 AND hseblf.amt[1] < 0
                           wsage[2] = wsage[2] + hseblf.amt[1] WHEN X = 1 AND hseblf.amt[1] > 0
                           wsage[3] = wsage[3] + hseblf.amt[X] WHEN X = 2
                           wsage[4] = wsage[4] + hseblf.amt[X] WHEN X = 3
                           wsage[5] = wsage[5] + hseblf.amt[X] WHEN X >= 4
                           wsage[6] = wsage[6] + hseblf.amt[X]. 
                END. 
            END.  
            IF NOT AVAILABLE hseblf THEN LEAVE.
      END.
   FIND FIRST hsesch WHERE hsesch.scheme = hsecmf.scheme NO-LOCK NO-ERROR.
   ASSIGN wsCell = STRING(Hsecmf.cell[1])
          wscell = wscell + ", " + STRING(Hsecmf.cell[2]) WHEN Hsecmf.cell[2] <> 0
          wsBal = hsecmf.bfBal.
          wsVat = hsecmf.bfBal * hsecmf.vat / (hsecmf.vat + 100).
   DISPLAY STREAM a wsTitle hsecmf.dbacc hsecmf.Add1 hsecmf.Add2 hsecmf.Add3 hsecmf.NAME
                    wsCell hsecmf.emailAdd hsecmf.standno /*hsesch.descrip */ 
                    Hsecmf.Bal Hsecmf.PDate Hsecmf.Period Hsecmf.PPrice Hsecmf.PRate
                    wsDate @ hsemtf.trDate wsAccDate wsDueDate WITH FRAME frm-rpt.
   X = 16.
   DISPLAY STREAM a "Balance B/F" @ hsemtf.DESCRIP wsBal @ hsemtf.amt wsVat @ hsemtf.vat
           WITH FRAME frm-trans.
   DOWN STREAM a WITH FRAME frm-trans.
   X = X - 1.
   FOR EACH hsemtf WHERE hsemtf.dbacc = hsecmf.dbacc:
       DISPLAY STREAM a hsemtf.DESCRIP hsemtf.amt hsemtf.vat hsemtf.trdate hsemtf.ref
           WITH FRAME frm-trans.
       DOWN STREAM a WITH FRAME frm-trans.
       ASSIGN X = X - 1
              wsBal = wsBal + hsemtf.amt.
              wsVat = wsVat + hsemtf.vat.
   END.
   DOWN X STREAM a WITH FRAME frm-trans.
   UNDERLINE STREAM a hsemtf.DESCRIP hsemtf.amt hsemtf.vat
       WITH FRAME frm-trans.
   DISPLAY STREAM a "Balance C/F" @ hsemtf.DESCRIP wsBal @ hsemtf.amt wsVat @ hsemtf.vat
    WITH FRAME frm-trans.
   UNDERLINE STREAM a hsemtf.DESCRIP hsemtf.amt hsemtf.vat WITH FRAME frm-trans.
   DISPLAY STREAM a wsage[1] wsage[2] wsage[3] wsage[4] wsage[5] wsage[6]
                  wsline  wsMessage1 wsMessage2  WITH FRAME frm-footer. 
    PAGE STREAM a.
   END.   
 RETURN.
END.
