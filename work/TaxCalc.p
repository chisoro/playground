DEF VAR wsAmt AS DEC INITIAL 6000.
DEF VAR yAmt AS DEC.
DEF VAR wsTax AS DEC.
yAmt = wsAmt * 12.
FIND FIRST PayTax WHERE yAmt <= TaxTo AND yAmt >= Taxfrom NO-ERROR.
IF AVAILABLE payTax THEN DO:
    wsTax =( (yAmt * (tax% / 100)) - deduct) / 12.
    DISPLAY TaxFrom TaxTo yAmt wsTax WITH FRAME a.
END.
 .
