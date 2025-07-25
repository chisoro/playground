DEF VAR wsAmt1  AS DEC FORM  "zzzzzz9.99-".
DEF VAR wsAmt2  LIKE wsAmt1.
DEF VAR wsAmt3  LIKE wsAmt1.
DEF VAR wsNet   AS DEC FORM "zzzzzz9.99-".
DEF VAR wsDes1  AS CHAR FORM "x(15)".
DEF VAR wsName  AS CHAR FORM "x(60)".
DEF VAR wsLeav  AS DEC FORM "zz9.99-".
DEF VAR wsSex   AS CHAR FORM "x(6)".
DEF VAR wsBCode AS CHAR FORM "x(6)".
DEF VAR wsAcc   AS CHAR   FORM "x(20)".
DEF VAR wsDate  AS DATE INITIAL TODAY.
DEF VAR i       AS INT.
DEF VAR j       AS INT.
DEF VAR k       AS INT.
DEF VAR X       AS INT.
DEF STREAM a.

FORM  wsDes1       AT 8
      wsAmt1       AT 42
      wsAmt2       AT 55
      wsAmt3       AT 71
          WITH WIDTH 132
              FONT 6
               NO-BOX
               NO-LABELS
               DOWN
               FRAME detail STREAM-IO.
OUTPUT STREAM a TO g:\simacc\rep1.txt.
FOR EACH payemf:
    ASSIGN i      = 1
           j      = 1
           k      = 1
           wsNet  = 0
           wsName = payemf.FName + " " + payemf.SName
           wsSex  = "MALE" WHEN payemf.sex = "M"
           wsSex  = "FEMALE" WHEN payemf.sex = "F"
           wsBcode = "".
           DO X = 1 TO 6:
               IF SUBSTR(payemf.Account,X,1) <> "-" THEN
                   wsBCode = wsBcode + SUBSTR(payemf.Account,X,1).
                 ELSE IF SUBSTR(payemf.Account,X,1) = "-" THEN 
                     X = 6.
           END.
    /*DISPLAY STREAM a payemf.IDNo wsBCode payemf.empcode wsName payemf.Account wssex 
                     wsLeav payemf.startdate WITH FRAME head.*/
    DOWN STREAM a WITH FRAME head.
    FOR EACH paymtf WHERE paymtf.empcode = payemf.empcode AND paymtf.CurAmt <> 0:
        FIND FIRST payitem WHERE payitem.itemcode = paymtf.itemcode NO-ERROR.
        IF Payitem.cat = 1 THEN DO:
            wsAmt1 = wsAmt1 + paymtf.CurAmt.
            DISPLAY STREAM a Payitem.Descrip @ wsDes1  paymtf.CurAmt @ wsAmt1 WITH FRAME detail.
            DOWN STREAM a WITH FRAME detail.
        END.
            
        ELSE IF Payitem.cat = 2 THEN DO:
            wsAmt2 = wsAmt2 + paymtf.CurAmt.
            DISPLAY STREAM a Payitem.Descrip @ wsDes1  paymtf.CurAmt @ wsAmt2 WITH FRAME detail.
            DOWN STREAM a WITH FRAME detail.
        END.
        ELSE IF Payitem.cat = 3 THEN DO:
            wsAmt3 = wsAmt3 + paymtf.CurAmt.
            DISPLAY STREAM a Payitem.Descrip @ wsDes1  paymtf.CurAmt @ wsAmt3 WITH FRAME detail.
            DOWN STREAM a WITH FRAME detail.
        END.
    END.
    
    DISPLAY STREAM a wsAmt1 wsAmt2 wsAmt3 WITH FRAME detail.
    DOWN STREAM a WITH FRAME detail.
END.

