DEF VAR acc LIKE dbcmf.dbAcc.
DEF VAR nm LIKE dbcmf.NAME.
DEF VAR mail LIKE dbcmf.emailAdd.
input from g:\simacc\work\emailList.csv.
repeat :
   import delimiter "," Acc nm mail.
   FIND FIRST dbcmf WHERE dbcmf.dbacc = Acc NO-ERROR.
   IF AVAILABLE dbcmf THEN DO:
       ASSIGN
           dbcmf.emailAdd = mail.
   END.
END.
MESSAGE "Finished" VIEW-AS ALERT-BOX.

