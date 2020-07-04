local script = {
    guiHandleEvent = function(id, name, param)
        if id == "constructionBuilder" then
            if name == "builder.proposalCreate" then
                local toAdd = param.proposal.toAdd
                if toAdd and #toAdd == 1 then
                    local con = toAdd[1]
                    if (con.fileName == [[station/rail/mus.con]]) then
                        if not api.gui.util.getById("mus.menu.img") then
                            local trackIconList = api.res.constructionRep.get(api.res.constructionRep.find("station/rail/mus.con")).createTemplateScript.params.trackIconList
                            local menu = api.gui.util.getById("menu.construction.rail.settings")
                            local menuLayout = menu:getLayout()
                            local tr = menuLayout:getItem(8)
                            local layout = tr:getLayout()
                            local cbox = layout:getItem(1)
                            local img = api.gui.comp.ImageView.new(trackIconList[cbox:getCurrentIndex() + 1])
                            img:setId("mus.menu.img")
                            layout:addItem(img)
                            layout:removeItem(cbox)
                            layout:addItem(cbox)
                            cbox:onIndexChanged(function(i) img:setImage(trackIconList[i + 1], true) end)
                        end
                    end
                end
            elseif name == "builder.apply" then
                local toAdd = param.proposal.toAdd
                if toAdd and #toAdd > 0 then
                    for i = 1, #toAdd do
                        local con = toAdd[i]
                        if (con.fileName == [[station/rail/mus.con]]) then
                            if (con.params.isFinalized == 0) then
                                game.interface.sendScriptEvent("__underpassEvent__", "new", {id = param.result[1], isStation = true})
                            end
                        end
                    end
                end
            end
        end
    end
}

function data()
    return script
end
