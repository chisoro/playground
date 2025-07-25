/* Program................glpar01.p
   Notes:................. General ledger parameter list/print
   Author:.................S. Mawire
   Modified:..............S. Chisoro
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
    "Fund List", 1, "Vote List", 2,"Sub-vote List", 3, "Category List", 4,
    "Sub-category", 5, "ITEM List", 6, "Ledger List", 7
    SKIP(0.5)  
    btn-ok AT ROW 12.7 COL 20 space(20) 
    btn-exp space(20)
    btn-exit SKIP(1)
    rect-2 AT ROW 1.4 COL 3
    rect-1 AT ROW 12 COL 3
    WITH 1 DOWN OVERLAY SIDE-LABELS CENTERED THREE-D 
     SIZE 100 BY 15.5 KEEP-TAB-ORDER
    TITLE "GENERAL LEDGER PARAMETER LISTING" VIEW-AS DIALOG-BOX.

FORM 
     glfund.FUND  AT 10 LABEL "CODE"
     glfund.DESCRIP     LABEL "DESCRIPTION"
      glfund.SURACC     LABEL "SURPLUS ACCOUNT"
    HEADER skip(2) "          FUND LISTING REPORT AS AT: " AT 10 SPACE(2)
    STRING(TODAY)
    "Page: " AT 75 PAGE-NUMBER(a)
    SKIP(3)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-fund.

FORM 
     GLVOTE.VOTE     AT 10 LABEL "CODE"
     GLVOTE.DESCRIP         LABEL "DESCRIPTION"
    HEADER skip(2) "      VOTE LISTING REPORT AS AT: " AT 10 SPACE(2)
    STRING(TODAY)
    "Page: " AT 75 PAGE-NUMBER(a)
    SKIP(3)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-vote.

FORM 
     GLSUBVOTE.SUBVOTE  AT 10 LABEL "CODE"
     GLSUBVOTE.DESCRIP     LABEL "DESCRIPTION"
    HEADER skip(2) "          SUB-VOTE LISTING REPORT AS AT: " AT 10 SPACE(2)
    STRING(TODAY)
    "Page: " AT 75 PAGE-NUMBER(a)
    SKIP(3)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-subvote.

FORM 
     GLITEM.ITEM    AT 10 LABEL "CODE"
     GLITEM.DESCRIP         LABEL "DESCRIPTION"
    HEADER skip(2) "      LEDGER ITEM LISTING REPORT AS AT: " AT 10 SPACE(2)
    STRING(TODAY)
    "Page: " AT 75 PAGE-NUMBER(a)
    SKIP(3)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-item.
 
FORM 
     glsubcat.subcat     AT 10 LABEL "CODE"
     glsubcat.DESCRIP         LABEL "DESCRIPTION"
    HEADER skip(2) "      SUB-CATEGORY LISTING REPORT AS AT: " AT 10 SPACE(2)
    STRING(TODAY)
    "Page: " AT 75 PAGE-NUMBER(a)
    SKIP(3)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-subcat.

FORM 
     glcat.cat     AT 10 LABEL "CODE"
     glcat.DESCRIP         LABEL "DESCRIPTION"
    HEADER skip(2) "      CATEGORY LISTING REPORT AS AT: " AT 10 SPACE(2)
    STRING(TODAY)
    "Page: " AT 75 PAGE-NUMBER(a)
    SKIP(3)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-cat.

FORM 
     glmf.acct     AT 5 LABEL "LEDGER"
     glmf.DESCRIPTION         LABEL "DESCRIPTION" FORM "X(20)"
    glmf.ACCTYPE LABEL "TYPE"
    glmf.DrCr LABEL "DEBIT/CREDIT"
    glmf.CAT LABEL "CATEGORY"
    glmf.subcat LABEL "SUB-CATEGORY"
    glmf.Clas LABEL "CLASS"
    glmf.ITEM LABEL "ITEM"
    glmf.Dept  LABEL "DEPARTMENT"
    glmf.FUND  LABEL "FUND"
    glmf.Proj LABEL "PROJECT" 
    HEADER skip(2) "      LEDGER LISTING REPORT AS AT: " AT 10 SPACE(2)
    STRING(TODAY)
    "Page: " AT 75 PAGE-NUMBER(a)
    SKIP(3)
    WITH DOWN STREAM-IO FONT 10 NO-LABEL CENTERED NO-BOX
    WIDTH 132 FRAME frm-ledger.
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
        FOR EACH glfund NO-LOCK:
            DISPLAY STREAM a GLFUND.DESCRIP GLFUND.FUND GLFUND.SURACC WITH FRAME frm-fund.
            DOWN STREAM a WITH FRAME frm-fund.
        END.
        APPLY 'ENTRY' TO btn-exit IN FRAME frm-main.
    END.
    WHEN 2 THEN DO:
        FOR EACH glvote NO-LOCK:
            DISPLAY STREAM a GLVOTE.VOTE GLVOTE.DESCRIP WITH FRAME frm-vote.
            DOWN STREAM a WITH FRAME frm-vote.
        END.
        APPLY 'ENTRY' TO btn-exit.
    END.
    WHEN 3 THEN DO:
        FOR EACH glsubvote NO-LOCK:
            DISPLAY STREAM a GLSUBVOTE.SUBVOTE GLSUBVOTE.DESCRIP WITH FRAME frm-subvote.
            DOWN STREAM a WITH FRAME frm-subvote.
        END.
        APPLY 'ENTRY' TO btn-exit IN FRAME frm-main.
    END.
    WHEN 4 THEN DO:
        FOR EACH glcat NO-LOCK:
            DISPLAY STREAM a GLCAT.DESCRIP GLCAT.CAT WITH FRAME frm-cat.
            DOWN STREAM a WITH FRAME frm-cat.
        END.
        APPLY 'ENTRY' TO btn-exit.
    END.
    WHEN 5 THEN DO:
        FOR EACH glsubcat NO-LOCK:
            DISPLAY STREAM a glsubcat.subcat glsubcat.DESCRIP WITH FRAME frm-subcat.
            DOWN STREAM a WITH FRAME frm-subcat.
        END.
        APPLY 'ENTRY' TO btn-exit IN FRAME frm-main.
    END.
    WHEN 6 THEN DO:
        FOR EACH glitem NO-LOCK:
            DISPLAY STREAM a GLITEM.DESCRIP GLITEM.ITEM WITH FRAME frm-item.
            DOWN STREAM a WITH FRAME frm-item.
        END.
        APPLY 'ENTRY' TO btn-exit.
    END.
    WHEN 7 THEN DO:
        FOR EACH glmf NO-LOCK:
            DISPLAY STREAM a glmf.acct glmf.DESCRIPTION glmf.ACCTYPE glmf.DrCr glmf.CAT glmf.subcat glmf.Clas glmf.ITEM glmf.Dept  glmf.FUND  glmf.Proj WITH FRAME frm-ledger.
            DOWN STREAM a WITH FRAME frm-ledger.
        END.
        APPLY 'ENTRY' TO btn-exit.
    END.
END CASE.
END.



PROCEDURE reporti.ip:
CASE varRep:
    WHEN 1 THEN DO:
        wsFile =  wsFile + "FUND.CSV".
        OUTPUT TO VALUE(wsFile).
        EXPORT DELIMITER "," "FUND"  "DESCRIPTION" "SURACC".
        FOR EACH glfund NO-LOCK:
            EXPORT DELIMITER "," GLFUND.FUND  GLFUND.DESCRIP GLFUND.SURACC.
            
        END.
        OS-COMMAND NO-WAIT VALUE(wsFile)).
        APPLY 'ENTRY' TO btn-exit IN FRAME frm-main.
    END.
    WHEN 2 THEN DO:
        wsFile =  wsFile + "VOTE.CSV".
        OUTPUT TO VALUE(wsFile).
        EXPORT DELIMITER "," ""  "DEOTESCRIPTION".
        FOR EACH glvote NO-LOCK:
            EXPORT DELIMITER "," GLVOTE.VOTE GLVOTE.DESCRIP.
            
        END.
        OS-COMMAND NO-WAIT VALUE(wsFile)).
        APPLY 'ENTRY' TO btn-exit.
    END.
    WHEN 3 THEN DO:
        wsFile =  wsFile + "SUBVOTE.CSV".
        OUTPUT TO VALUE(wsFile).
        EXPORT DELIMITER "," "SUBVOTE"  "DESCRIPTION".
        FOR EACH glsubvote NO-LOCK:
            EXPORT DELIMITER "," GLSUBVOTE.SUBVOTE GLSUBVOTE.DESCRIP.
            
        END.
        OS-COMMAND NO-WAIT VALUE(wsFile)).
        APPLY 'ENTRY' TO btn-exit IN FRAME frm-main.
    END.
    WHEN 4 THEN DO:
        wsFile =  wsFile + "GLCATEGORY.CSV".
        OUTPUT TO VALUE(wsFile).
        EXPORT DELIMITER "," "CATEGORY"  "DESCRIPTION".
        FOR EACH glcat NO-LOCK:
            EXPORT DELIMITER "," GLCAT.CAT GLCAT.DESCRIP.
            
        END.
        OS-COMMAND NO-WAIT VALUE(wsFile)).
        APPLY 'ENTRY' TO btn-exit.
    END.
    WHEN 5 THEN DO:
        wsFile =  wsFile + "GLsubCATEGORY.CSV".
        OUTPUT TO VALUE(wsFile).
        EXPORT DELIMITER "," "SUBCATEGORY"  "DESCRIPTION".
        FOR EACH glsubcat NO-LOCK:
            EXPORT DELIMITER "," glsubcat.subcat glsubcat.DESCRIP.
            
        END.
        OS-COMMAND NO-WAIT VALUE(wsFile)).
        APPLY 'ENTRY' TO btn-exit IN FRAME frm-main.
    END.
    WHEN 6 THEN DO:
        wsFile =  wsFile + "GLITEM.CSV".
        OUTPUT TO VALUE(wsFile).
        EXPORT DELIMITER "," "ITEM"  "DESCRIPTION".
        FOR EACH glitem NO-LOCK:
            EXPORT DELIMITER "," GLITEM.DESCRIP GLITEM.ITEM.
            
        END.
        OS-COMMAND NO-WAIT VALUE(wsFile)).
        APPLY 'ENTRY' TO btn-exit.
    END.
    WHEN 7 THEN DO:
        wsFile =  wsFile + "LEDGER.CSV".
        OUTPUT TO VALUE(wsFile).
        EXPORT DELIMITER "," "LEDGER" "DESCRIPTION" "TYPE" "DEBIT/CREDIT" "CATEGORY" "SUBCATEGORY" "CLASS" "ITEM" "DEPARTMENT"  "FUND"  "PROJECT".
        FOR EACH glmf NO-LOCK:
            EXPORT DELIMITER "," glmf.acct glmf.DESCRIPTION glmf.ACCTYPE glmf.DrCr glmf.CAT glmf.subcat glmf.Clas glmf.ITEM glmf.Dept  glmf.FUND  glmf.Proj.
          
        END.
        OS-COMMAND NO-WAIT VALUE(wsFile)).
        APPLY 'ENTRY' TO btn-exit.
    END.
END CASE.
END.
