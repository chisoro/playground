DEF  VAR wsPer AS DEC FORM "999999" INITIAL 200905.
DEF VAR curPer AS DEC FORM "999999" INITIAL 201912.
DEF VAR yy AS INT.
DEF VAR mm AS INT.
DEF VAR ter AS LOGICAL INITIAL NO.
DEF VAR life AS INT INITIAL 60.
DEF VAR relife AS INT.
DEF VAR cnt AS INT INITIAL 1.
ASSIGN yy = INT(SUBSTR(STRING(wsPer),1,4))
       mm = INT(SUBSTR(STRING(wsPer),5,2)).
DO  WHILE ter = NO:     
    DO WHILE mm <= 12:
        ASSIGN mm = mm + 1
               wsPer = DEC(STRING(yy) + STRING(mm,"99"))
               cnt = cnt + 1.
        DISPLAY curPer wsPer cnt yy mm ter. PAUSE 2.
    END.
    ASSIGN mm = 1
           yy = yy + 1
           wsPer = DEC(STRING(yy) + STRING(mm,"99")).
      IF CurPer = wsPer THEN
          ter = YES.
      IF cnt = life THEN
          ter = YES.      .
END.
