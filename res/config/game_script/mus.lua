local state = {
    warningShaderMod = false
}

local dump = require "luadump"

local entryWarning = function()
    if (not (game.config.underpassMod and game.config.shaderMod)) then
        if not state.warningShaderMod then
            local textview = gui.textView_create(
                "mus.warning.textView",
                _("UNDERPASS_WARNING"),
                400
            )
            local layout = gui.boxLayout_create("mus.warning.boxLayout", "VERTICAL")
            layout:addItem(textview)
            state.warningShaderMod = gui.window_create(
                "mus.warning.window",
                _("Warning"),
                layout
            )
            state.warningShaderMod:onClose(function()state.warningShaderMod = false end)
        end
        
        local mainView = game.gui.getContentRect("mainView")
        local mainMenuHeight = game.gui.getContentRect("mainMenuTopBar")[4] + game.gui.getContentRect("mainMenuBottomBar")[4]
        local size = game.gui.calcMinimumSize(state.warningShaderMod.id)
        local y = mainView[4] - size[2] - mainMenuHeight
        local x = mainView[3] - size[1]
        
        game.gui.window_setPosition(state.warningShaderMod.id, x * 0.5, y * 0.5)
        game.gui.setHighlighted(state.warningShaderMod.id, true)
    end
end

local script = {
    guiHandleEvent = function(id, name, param)
        entryWarning()
        if id == "constructionBuilder" then
            if name == "builder.proposalCreate" then
                local toAdd = param.proposal.toAdd
                if toAdd and #toAdd == 1 then
                    local con = toAdd[1]
                    if (con.fileName == [[station/rail/mus.con]]) then
                        if not api.gui.util.getById("mus.menu.img") then
                            local trackIconList = api.res.constructionRep.get(api.res.constructionRep.find("station/rail/mus.con_a")).createTemplateScript.params.trackIconList
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
                            cbox:onIndexChanged(function(i) img:setImage(trackIconList[i + 1], false) end)
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
