ASSIGN  
    FRAME login-frame:scrollable = FALSE
    g-code:width                 = 15
    login-password:width         = 15
    pass-file.name:y             = pass-file.name:y + 2
    login-image:height-pixels    = 168
    login-image:width-pixels     = 290
    FRAME login-frame:height     = 12.4
    FRAME login-frame:width      = 60.


/* build main menu */
procedure buildfrontmenu.ip :
    create widget-pool "front-menu-widgets" persistent no-error.
    
    if not ll-treeview then
    do:
        for each menu-item no-lock where
            menu-item.cType = "G" and menu-item.cParent = "main" and
                lookup(g-code, menu-item.user-links) <> 0 
                    by menu-item.iSequence:

            create menu-item hMainMenuItem in widget-pool "front-menu-widgets"
                assign
                    parent = hMainSubMenu
                    name = menu-item.cKey
                    label = replace(menu-item.cLabel,"&","&&") /* 4.11 */
                    triggers:
                        on choose persistent run mainmenuitemchoose.ip 
                            in this-procedure (input hMainMenuItem:name).
                    end.
        end.
        
        create menu-item hMainMenuItem in widget-pool "front-menu-widgets"
            assign
                subtype = "RULE"
                parent  = hMainSubMenu.

    end.

    create menu-item hMainMenuItem in widget-pool "front-menu-widgets"
        assign
            parent  = hMainSubMenu
            label   = "Log off &user " + trim(g-code)
            triggers:
                on choose persistent run systemlogoff.ip
                    in this-procedure.
            end.
    
    create menu-item hMainMenuItem in widget-pool "front-menu-widgets"
        assign
            parent  = hMainSubMenu
            label   = "E&xit"
            triggers:
                on choose persistent run systemshutdown.ip
                    in this-procedure.
            end.
end procedure.

/* menu builder */
procedure menu_builder.ip :
    define input parameter hItem as widget-handle.
    define variable        hOut as widget-handle.
    define variable        hSubMenu as widget-handle.
    define variable        hMenuItem as widget-handle.
    define buffer          bMenuBuffer for menu-item.

    for each bMenuBuffer no-lock where
        bMenuBuffer.cType = "g" and bMenuBuffer.cParent = hItem:name and
            lookup(g-code, bMenubuffer.user-links) <> 0
                by bMenuBuffer.iSequence :
        
        if bMenuBuffer.lChildren then 
        do:
            create sub-menu hSubMenu in widget-pool "gui-menu"
                assign
                    parent = hItem
                    label  = replace(bMenuBuffer.cLabel,"&","&&") /* 4.11 */
                    .
            assign hOut = hSubMenu.
            assign hOut:name = bMenuBuffer.cKey.
            run menu_builder.ip (hOut).
        end.
        else
        do:
            lc-prog  = bMenuBuffer.cProgram.
            lc-label = replace(bMenuBuffer.cLabel,"&","&&"). /* 4.11 */
            
            li-tot = font-table:get-text-width-pixels(lc-label) + 
                font-table:get-text-width-pixels(lc-prog).
            
            lc-spaces = chr(9) + fill(" ",10).
            li-scroll = li-tot.
              
            create menu-item hMenuItem in widget-pool "gui-menu"
                assign
                    parent = hItem
                    label = lc-label + lc-spaces + lc-prog
                    name = bMenuBuffer.cKey
                    triggers:
                        on choose persistent run run_program.ip 
                            in this-procedure (input hMenuItem:name).
                    end triggers.
                    .
        end.
    end.
end procedure.
