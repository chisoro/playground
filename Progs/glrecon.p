DEF STREAM a.
DEF VAR X AS INT.
DEF VAR wsAmt AS DEC FORM "zzz,zzz,zzz,zz9.99-" EXTENT 2.
DEF VAR wsyear AS INT.
DEF VAR wsdes  LIKE dbsgr.descrip.
DEF VAR wsLedger LIKE glmf.acct.
DEF VAR wsSgrp   LIKE dbsgr.sgrp.

OUTPUT STREAM a TO TERMINAL.

FORM                                              /* tran line */
     wsDes     LABEL "DESCRIPTION" 
     wsSgrp    LABEL "SERVICE"
     wsLedger  LABEL "LEDGER"
     wsAmt[1] LABEL "LIST AMOUNT"
     wsAmt[2] LABEL "LEDGER AMOUNT" 
     with width 132
	  no-box
	  no-labels
      DOWN
      STREAM-IO
	  frame frm-rpt.

FIND FIRST simctr NO-LOCK NO-ERROR.
FIND FIRST tblforex WHERE txtcur = "USD" NO-LOCK NO-ERROR.

wsyear = YEAR(TODAY).

FOR EACH dbblf BREAK BY sgrp.
    ACCUM (dbamt[1] * tblforex.decrate) (TOTAL BY dbblf.sgrp).
    IF LAST-OF(dbblf.sgrp) THEN DO:
        wsAmt[2] = 0.
        FIND FIRST dbsgr WHERE dbsgr.sgrp = dbblf.sgrp NO-LOCK NO-ERROR.
        FIND FIRST glbal WHERE glbal.YEAR = wsyear AND glbal.acct = dbsgr.ctrledger NO-LOCK NO-ERROR.
        IF AVAILABLE glbal THEN DO:
            wsAmt[2] = glbal.bfbal.
            DO X = 1 TO INT(MONTH(TODAY)):
                wsAmt[2] = wsAmt[2] + glbal.amt[X].
            END.
        END.
        DISPLAY STREAM a dbsgr.descrip @ wsdes dbsgr.sgrp @ wsSgrp dbsgr.ctrLedger @ wsLedger (ACCUM TOTAL BY dbblf.sgrp dbamt[1] * tblforex.decrate) @ wsAmt[1] wsAmt[2] WITH FRAME frm-rpt.
        DOWN STREAM a WITH FRAME frm-rpt.
    END.
END.
