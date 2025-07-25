
ASSIGN wsDr = 0
       wsCr = 0
       wsTDr = 0
       wsTCr = 0
       wsTitle = "   USD DEBIT RAISING CONSOLIDATION REPORT FOR THE PERIOD:".
FOR EACH dbgl  WHERE dbgl.TransID = wsTransID AND  dbgl.SOURCE = wsSource:
    IF  dbgl.amt < 0 THEN
        ASSIGN wsDr = 0
               wsCr = ROUND((dbgl.amt / wsRate),2)
               wsTCr = wsTCr + ROUND((dbgl.amt / wsRate),2).
    ELSE
        ASSIGN wsCr = 0
               wsDr = ROUND((dbgl.amt / wsRate),2)
               wsTDr = wsTDr + ROUND((dbgl.amt / wsRate),2).
    FIND FIRST glmf WHERE glmf.acct = dbgl.acct NO-LOCK NO-ERROR.
    IF NOT AVAILABLE glmf THEN
        MESSAGE dbgl.acct " Account does not exist" VIEW-AS ALERT-BOX.
    DISPLAY STREAM a  dbgl.fund dbgl.Proj dbgl.dept dbgl.acct glmf.DESCRIPTION wsDr wsCr WITH FRAME frm-rpt1.
    DOWN STREAM a WITH FRAME frm-rpt1.
END.
UNDERLINE STREAM a glmf.DESCRIPTION wsDr wsCr WITH FRAME frm-rpt1.
DISPLAY  STREAM a "TOTAL " @ glmf.DESCRIPTION wsTDr @ wsDr wsTCr @ wsCr WITH FRAME frm-rpt1.
DOWN STREAM a WITH FRAME frm-rpt1.
PAGE STREAM a.
