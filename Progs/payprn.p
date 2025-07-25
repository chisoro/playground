/* Program................payprn.p
   Notes:................. Payrol Paramiter Listing
   Author:.................S. Chisoro
   
*/
session:DATA-ENTRY-RETURN = TRUE.
DEF STREAM a.
DEF VAR w-orientation AS CHAR      INITIAL "PORTRAIT"    NO-UNDO.
DEF VAR w-letterhead  AS CHAR      INITIAL ""            NO-UNDO.
DEF VAR w-fontsize    AS INT       INITIAL 10.0          NO-UNDO.
DEF VAR varRep        AS INT  INITIAL 1.
DEF VAR wsYear AS INT FORM "9999".

DEF BUTTON btn-exit   LABEL "EXIT".
DEF BUTTON btn-ok     LABEL "Print".

DEF VAR wsFile as CHAR.
DEF BUTTON btn-Exp LABEL "Export".

DEFINE RECTANGLE rect-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 2.3.

DEFINE RECTANGLE rect-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 95 by 10.5.

DEF FRAME frm-main
    SKIP(1.5)
    varRep       LABEL "Which Report?" COLON 30 
    VIEW-AS RADIO-SET RADIO-BUTTONS
    "PAYROLL BANKS", 1, "PAYROLL ITEMS", 2,"TAX TABLES", 3, "PAYROLL SYSTEMS", 4,
    "ITF 16 CODES", 5,"DEPARTMENT LIST", 6, "POST LIST", 7,  "EMPLOYEE LIST", 8
    SKIP(0.5)  
    btn-ok AT ROW 12.7 COL 20 space(20) 
    btn-exp space(20)
    btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 12 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 15.5 KEEP-TAB-ORDER
    TITLE "PAYROLL PARAMETER LISTING" VIEW-AS DIALOG-BOX.

FORM 
     paybnk.bank  AT 10 LABEL "BANK"
     paybnk.bankname     LABEL "DESCRIPTION"
     paybnk.bcode     LABEL "BRANCH CODE"
    HEADER skip(2) "          PAYROLL BANKS LISTING REPORT AS AT: " AT 10 SPACE(2)
    STRING(TODAY)
    "Page: " AT 75 PAGE-NUMBER(a)
    SKIP(3)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-pbank.

FORM 
    Payitem.Itemcode AT 10 LABEL "CODE"
    Payitem.Suffix LABEL "LEDGER"
    Payitem.Descrip LABEL "DESCRIPTION"
    Payitem.cat     LABEL "CATEGORY"
    Payitem.Type  LABEL "TYPE"
    Payitem.Regular  LABEL "REGULAR"
    Payitem.Linked LABEL "LINKED"
    Payitem.LLAmt LABEL "LOWER AMOUNT"
    Payitem.stdAmt LABEL "STANDARD AMOUNT"
    Payitem.ULAmt LABEL "UPPER AMOUNT"
    Payitem.Tax LABEL "TAXABLE"
    HEADER skip(2) "      PAYITEM LISTING REPORT AS AT: " AT 10 SPACE(2)
    STRING(TODAY)
    "Page: " AT 75 PAGE-NUMBER(a)
    SKIP(3)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-pitem.

FORM
     PayTax.Taxfrom  AT 10 LABEL "FROM"
     PayTax.Taxto LABEL "TO"
     PayTax.Amount LABEL "AMOUNT"
     PayTax.Tax% LABEL "PERCENTAGE"
     PayTax.Deduct LABEL "DEDUCT"
     HEADER skip(2) "      PAYEE  TAX TABLE LISTING REPORT AS AT: " AT 10 SPACE(2)
    STRING(TODAY)
    "Page: " AT 75 PAGE-NUMBER(a)
    SKIP(3)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-ttable.

FORM 
     PAYSYS.PAYSYS    AT 10 LABEL "PAYROLL"
     PAYSYS.DESCRIP         LABEL "DESCRIPTION"
    PAYSYS.TXTCUR           LABEL "CURRENCY"
    HEADER skip(2) "      PAYROLL SYSTEM LISTING REPORT AS AT: " AT 10 SPACE(2)
    STRING(TODAY)
    "Page: " AT 75 PAGE-NUMBER(a)
    SKIP(3)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-psys.
 
FORM 
     ITF16.Itemcode     AT 10 LABEL "CODE"
     ITF16.PAYE         LABEL "CLASSIFICATION"
    HEADER skip(2) "      ITF16 LISTING REPORT AS AT: " AT 10 SPACE(2)
    STRING(TODAY)
    "Page: " AT 75 PAGE-NUMBER(a)
    SKIP(3)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-ITF.

FORM 
    PayPost.Post      AT 10 LABEL "POST"
    PayPost.Descrip LABEL "NAME"
    PayPost.Grade LABEL "GRADE"
    PayPost.Vacant LABEL "VACANT"
    PayPost.Dept LABEL "DEPARTMENT"
    PayPost.Fund LABEL "FUND"
    PayPost.Proj LABEL "PROJECT"
    HEADER skip(2) "      POSTS LISTING REPORT AS AT: " AT 10 SPACE(2)
    STRING(TODAY)
    "Page: " AT 75 PAGE-NUMBER(a)
    SKIP(3)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-ppo.

FORM 
    Paydept.Dept     AT 10 LABEL "CODE"
    Paydept.Descrip         LABEL "DESCRIPTION"
    HEADER skip(2) "      DEPARTMENT LISTING REPORT AS AT: " AT 10 SPACE(2)
    STRING(TODAY)
    "Page: " AT 75 PAGE-NUMBER(a)
    SKIP(3)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-pdep.

FORM 
   payemf.empcode AT 5 LABEL "CODE"
   payemf.FName LABEL "FIRSTNAME" FORM "x(20)"
   payemf.SName  LABEL "SURNAME" FORM "x(20)"
   payemf.IDNo  LABEL "ID NUMBER"
   payemf.sex LABEL "SEX"
   payemf.DOB  LABEL "DOB"
   payemf.Bank  LABEL "BANK"
   payemf.Dept LABEL "DEPARTMENT"
   payemf.Paysys LABEL "PAYSYS"
   payemf.Designation LABEL "DESIGNATION"
   payemf.Grade LABEL "GRADE"
   payemf.Post LABEL "POST"
   payemf.DOE LABEL "DOE"
    payemf.StartDate LABEL "START DATE"
    HEADER skip(2) "      EMPLOYEE LISTING REPORT AS AT: " AT 10 SPACE(2)
    STRING(TODAY)
    "Page: " AT 75 PAGE-NUMBER(a)
    SKIP(3)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-pemf.

/* */

/* ******** Triggers  ***********/

ON 'choose':U OF btn-ok  
DO:
   session:set-wait-state("").
   ASSIGN  VarRep.
   {PrintOpt.i &stream-name="stream a"
                    &print-prog="Report.ip"
                    &paged} 
  
END.


 ON 'choose':U OF btn-exp  
   DO:
      session:set-wait-state("").
      ASSIGN  VarRep.
      RUN Reporti.ip.

   END.
/********** MAIN LOGIC **********/
FIND FIRST simctr NO-LOCK.
wsFile = simctr.repDir.
VIEW FRAME frm-main IN WINDOW CURRENT-WINDOW.
ENABLE ALL WITH FRAME frm-main.
WAIT-FOR CHOOSE OF btn-exit OR close of THIS-PROCEDURE IN FRAME frm-main.
HIDE FRAME frm-main.
RETURN.


PROCEDURE report.ip:
CASE varRep:
    WHEN 1 THEN DO:
        FOR EACH PAYBNK NO-LOCK:
            DISPLAY STREAM a paybnk.Bank paybnk.BankName paybnk.BCode WITH FRAME frm-pbank.
            DOWN STREAM a WITH FRAME frm-pbank.
        END.
        APPLY 'ENTRY' TO btn-exit IN FRAME frm-main.
    END.
    WHEN 2 THEN DO:
        FOR EACH payitem NO-LOCK:
            DISPLAY STREAM a Payitem.cat Payitem.Descrip Payitem.Itemcode Payitem.Linked Payitem.LLAmt Payitem.Regular Payitem.stdAmt Payitem.Suffix Payitem.Tax Payitem.Type Payitem.ULAmt WITH FRAME frm-pitem.
            DOWN STREAM a WITH FRAME frm-pitem.
        END.
        APPLY 'ENTRY' TO btn-exit.
    END.
    WHEN 3 THEN DO:
        FOR EACH paytax NO-LOCK:
            DISPLAY STREAM a PayTax.Amount PayTax.Deduct PayTax.Tax% PayTax.Taxfrom PayTax.Taxto WITH FRAME frm-ttable.
            DOWN STREAM a WITH FRAME frm-ttable.
        END.
        APPLY 'ENTRY' TO btn-exit IN FRAME frm-main.
    END.
    WHEN 4 THEN DO:
        FOR EACH paysys NO-LOCK:
            DISPLAY STREAM a Paysys.Paysys Paysys.Descrip Paysys.txtCur WITH FRAME frm-psys.
            DOWN STREAM a WITH FRAME frm-psys.
        END.
        APPLY 'ENTRY' TO btn-exit.
    END.
    WHEN 5 THEN DO:
        FOR EACH itf16 NO-LOCK:
            DISPLAY STREAM a ITF16.Itemcode ITF16.PAYE WITH FRAME frm-itf.
            DOWN STREAM a WITH FRAME frm-itf.
        END.
        APPLY 'ENTRY' TO btn-exit IN FRAME frm-main.
    END.
    WHEN 6  THEN DO:
        FOR EACH paydept  NO-LOCK:
            DISPLAY STREAM a Paydept.Dept Paydept.Descrip WITH FRAME frm-pdep.
            DOWN STREAM a WITH FRAME frm-pdep.
        END.
        APPLY 'ENTRY' TO btn-exit IN FRAME frm-main.
    END.
    WHEN 7 THEN DO:
        FOR EACH paypost NO-LOCK:
            DISPLAY STREAM a PayPost.Dept PayPost.Descrip PayPost.Fund PayPost.Grade PayPost.Post PayPost.Proj PayPost.Vacant WITH FRAME frm-ppo.
            DOWN STREAM a WITH FRAME frm-ppo.
        END.
        APPLY 'ENTRY' TO btn-exit IN FRAME frm-main.
    END.
    WHEN 8 THEN DO:
        FOR EACH payemf NO-LOCK:
            DISPLAY STREAM a  payemf.Bank   payemf.Dept payemf.Designation payemf.DOB payemf.DOE payemf.empcode  payemf.FName payemf.Grade payemf.IDNo   payemf.Paysys payemf.Post payemf.sex payemf.SName payemf.StartDate  WITH FRAME frm-pemf.
            DOWN STREAM a WITH FRAME frm-pemf.
        END.
        APPLY 'ENTRY' TO btn-exit IN FRAME frm-main.
    END.
END CASE.
END.


PROCEDURE reporti.ip:
CASE varRep:
    WHEN 1 THEN DO:
        wsFile =  wsFile + "paybank.CSV".
        OUTPUT TO VALUE(wsFile).
        EXPORT DELIMITER "," "BANK"  "NAME" "BRANCH CODE".
        FOR EACH PAYBNK NO-LOCK:
            EXPORT DELIMITER "," paybnk.Bank paybnk.BankName paybnk.BCode.
            
        END.
         OS-COMMAND NO-WAIT VALUE(wsFile)).
        APPLY 'ENTRY' TO btn-exit IN FRAME frm-main.
    END.
    WHEN 2 THEN DO:
        wsFile =  wsFile + "payITEM.CSV".
        OUTPUT TO VALUE(wsFile).
        EXPORT DELIMITER "," "ITEM" "LEDGER" "DESCRIPTION" "CATEGORY" "TYPE" "REGULAR" "LINKED" "LOWER AMOUNT" "STANDARD AMOUNT" "UPPER AMOUNT" "TAXABLE".
        FOR EACH payitem NO-LOCK:
            EXPORT DELIMITER "," Payitem.Itemcode   Payitem.Suffix Payitem.Descrip  Payitem.cat Payitem.TYPE Payitem.Regular Payitem.Linked Payitem.LLAmt  Payitem.stdAmt   Payitem.ULAmt Payitem.Tax.
           
        END.
         OS-COMMAND NO-WAIT VALUE(wsFile)).
        APPLY 'ENTRY' TO btn-exit IN FRAME FRM-MAIN.
    END.
    WHEN 3 THEN DO:
        wsFile =  wsFile + "TAX.CSV".
        OUTPUT TO VALUE(wsFile).
        EXPORT DELIMITER "," "FROM" "TO" "AMOUNT" "PERCENTAGE" "DEDUCT".
        FOR EACH paytax NO-LOCK:
            EXPORT DELIMITER "," PayTax.Taxfrom PayTax.Taxto PayTax.Amount PayTax.Tax% PayTax.Deduct.
           
        END.
         OS-COMMAND NO-WAIT VALUE(wsFile)).
        APPLY 'ENTRY' TO btn-exit IN FRAME frm-main.
    END.
    WHEN 4 THEN DO:
        wsFile =  wsFile + "PSYS.CSV".
        OUTPUT TO VALUE(wsFile).
        EXPORT DELIMITER "," "PAYROLL" "DESCRIPTION" "CURRENCY".
        FOR EACH paysys NO-LOCK:
            EXPORT DELIMITER ","  Paysys.Paysys Paysys.Descrip Paysys.txtCur.
            
        END.
         OS-COMMAND NO-WAIT VALUE(wsFile)).
        APPLY 'ENTRY' TO btn-exit IN FRAME FRM-MAIN.
    END.
    WHEN 5 THEN DO:
        wsFile =  wsFile + "ITF.CSV".
        OUTPUT TO VALUE(wsFile).
        EXPORT DELIMITER "," "CODE" "CLASSIFICATION".
        FOR EACH itf16 NO-LOCK:
            EXPORT DELIMITER "," ITF16.Itemcode ITF16.PAYE.
            
        END.
         OS-COMMAND NO-WAIT VALUE(wsFile)).
        APPLY 'ENTRY' TO btn-exit IN FRAME frm-main.
    END.
    WHEN 6  THEN DO:
        wsFile =  wsFile + "DEPT.CSV".
        OUTPUT TO VALUE(wsFile).
        EXPORT DELIMITER "," "CODE" "DESCRIPTION".
        FOR EACH paydept  NO-LOCK:
            EXPORT DELIMITER "," Paydept.Dept Paydept.Descrip.
            
        END.
         OS-COMMAND NO-WAIT VALUE(wsFile)).
        APPLY 'ENTRY' TO btn-exit IN FRAME frm-main.
    END.
    WHEN 7 THEN DO:
        wsFile =  wsFile + "POST.CSV".
        OUTPUT TO VALUE(wsFile).
        EXPORT DELIMITER "," "POST" "NAME" "GRADE" "VACANT" "DEPARTMENT" "FUND" "PROJECT".
        FOR EACH paypost NO-LOCK:
            EXPORT DELIMITER ","  PayPost.Post PayPost.Descrip PayPost.Grade  PayPost.Vacant PayPost.Dept PayPost.Fund   PayPost.Proj.
            
        END.
         OS-COMMAND NO-WAIT VALUE(wsFile)).
        APPLY 'ENTRY' TO btn-exit IN FRAME frm-main.
    END.
    WHEN 8 THEN DO:
        wsFile =  wsFile + "EMPLOYEE.CSV".
        OUTPUT TO VALUE(wsFile).
        EXPORT DELIMITER "," "CODE" "FIRSTNAME" "SURNAME" "ID NUMBER"  "SEX" "DOB" "BANK" "DEPARTMENT" "PAYROLL" "DESIGNATION" "GRADE" "POST" "DOE" "START DATE".
        FOR EACH payemf NO-LOCK:
            EXPORT DELIMITER ","  payemf.empcode  payemf.FName payemf.SName payemf.IDNo   payemf.sex  payemf.DOB   payemf.Bank   payemf.Dept payemf.Paysys payemf.Designation payemf.Grade   payemf.Post  payemf.DOE payemf.StartDate.
            
        END.
         OS-COMMAND NO-WAIT VALUE(wsFile)).
        APPLY 'ENTRY' TO btn-exit IN FRAME frm-main.
    END.
END CASE.
END.
