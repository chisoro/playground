FOR EACH pass-file WHERE pass-code <> "1":
    DISPLAY NAME . PAUSE 0.
    CREATE simusr.
    ASSIGN
        iPhone = int(pass-file.h-phone)
        simusr.Name = pass-file.name
        NextExpire = pass-file.exp-date 
        txtDept = string(pass-file.dept-code) 
        txtPass = string(pass-file.pass-word) 
        usercode =  string(pass-file.pass-code).
        
        
           
END.
