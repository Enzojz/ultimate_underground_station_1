local trackIndices = {
    ["standard.lua"] = "mus_track_std",
    ["high_speed.lua"] = "mus_track_hs",
}
-- local dump = require "luadump"

function data()
    return {
        info = {
            minorVersion = 7,
            severityAdd = "NONE",
            severityRemove = "CRITICAL",
            name = _("name"),
            description = _("desc"),
            authors = {
                {
                    name = "Enzojz",
                    role = "CREATOR",
                    text = "Idea, Scripting, Modeling",
                    steamProfile = "enzojz",
                    tfnetId = 27218,
                }
            },
            tags = {"Train Station", "Station"},
        },
        runFn = function(_)
            game.config.undergroundStationMod = true
        end,
        postRunFn = function(settings, params)
            local tracks = {}
            for __, trackName in pairs(api.res.trackTypeRep.getAll()) do
                if trackName == "standard.lua" then
                    table.insert(tracks, 1, trackName)
                elseif trackName == "high_speed.lua" then
                    table.insert(tracks, tracks[1] == "standard.lua" and 2 or 1, trackName)
                else
                    table.insert(tracks, trackName)
                end
            end
            local moduleList = {}
            local trackIconList = {}
            local trackNames = {}
            for __, trackName in ipairs(tracks) do
                local track = api.res.trackTypeRep.get(api.res.trackTypeRep.find(trackName))
                local baseFileName = ("station/rail/%s"):format(trackIndices[trackName] or trackName)
                for __, catenary in pairs({false, true}) do
                    local mod = api.type.ModuleDesc.new()
                    mod.fileName = ("%s%s.module"):format(baseFileName, catenary and "_catenary" or "")

                    mod.availability.yearFrom = track.yearFrom
                    mod.availability.yearTo = track.yearTo
                    mod.cost.price = 0

                    mod.description.name = track.name .. (catenary and _(" with catenary") or "")
                    mod.description.description = track.desc .. (catenary and _(" (with catenary)") or "")
                    mod.description.icon = track.icon

                    mod.type = "mus_track"
                    mod.order.value = trackIndices[trackName] and 0 or 100
                    mod.metadata = {
                        isTrack = true,
                        width = track.trackDistance,
                        type = "mus_track"
                    }

                    mod.category.categories = catenary and {_("MENU_TRACK_CAT")} or {_("MENU_TRACK")}

                    mod.updateScript.fileName = "construction/station/rail/mus_track_module.updateFn"
                    mod.updateScript.params = {
                        trackType = trackName,
                        catenary = catenary,
                        trackWidth = track.trackDistance
                    }
                    mod.getModelsScript.fileName = "construction/station/rail/mus_track_module.getModelsFn"
                    mod.getModelsScript.params = {}

                    api.res.moduleRep.add(mod.fileName, mod, true)
                end
                table.insert(moduleList, baseFileName)
                table.insert(trackIconList, track.icon)
                table.insert(trackNames, track.name)
            end

            local con = api.res.constructionRep.get(api.res.constructionRep.find("station/rail/mus.con"))
            local data = api.type.DynamicConstructionTemplate.new()
            for i = 1, #con.constructionTemplates[1].data.params do
                local p = con.constructionTemplates[1].data.params[i]
                local param = api.type.ScriptParam.new()
                param.key = p.key
                param.name = p.name
                if (p.key == "trackType") then
                    param.values = trackNames
                else
                    param.values = p.values
                end
                param.defaultIndex = p.defaultIndex or 0
                param.uiType = p.uiType
                data.params[i] = param
            end
            con.constructionTemplates[1].data = data

            con.createTemplateScript.fileName = "construction/station/rail/mus_create_template.fn"
            con.createTemplateScript.params = {moduleList = moduleList, trackIconList = trackIconList}

        --
        -- con.createTemplateScript.params[#con.createTemplateScript.params + 1] = newParams
        -- local streets = api.res.streetTypeRep.getAll()
        -- for __, streetName in pairs(streets) do
        --     local street = api.res.streetTypeRep.get(api.res.streetTypeRep.find(streetName))
        --     if (street.sidewalkWidth == 0 and #street.categories > 0 and not streetName:match("street_depot/") and not streetName:match("street_station/")) then
        --         local nBackward = 0
        --         for i = 1, #street.laneConfigs do
        --             if (street.laneConfigs[i].forward == false) then nBackward = nBackward + 1 end
        --         end
        --         local isOneWay = nBackward == 0
        --         for i = 1, (isOneWay and 2 or 1) do
        --             local isRev = i == 2
        --             local mod = api.type.ModuleDesc.new()
        --             mod.fileName = ("mus/street/%s%s.module"):format(streetName:match("(.+).lua"), isRev and "_rev" or "")
        --             mod.availability.yearFrom = street.yearFrom
        --             mod.availability.yearTo = street.yearTo
        --             mod.cost.price = 0
        --             mod.description.name = street.name
        --             mod.description.description = street.desc
        --             mod.description.icon = street.icon
        --             mod.type = "mus_track"
        --             mod.order.value = 0
        --             mod.metadata = {
        --                 isTrack = true,
        --                 width = street.streetWidth,
        --                 type = "mus_track"
        --             }
        --             mod.category.categories = {isRev and _("ONE_WAY_REV") or isOneWay and _("ONE_WAY") or _("STREET")}
        --             mod.updateScript.fileName = "construction/station/rail/mus_track_module.updateFn"
        --             mod.updateScript.params = {
        --                 isStreet = true,
        --                 laneNumber = #street.laneConfigs,
        --                 isOneWay = isOneWay,
        --                 sidewalkHeight = street.sidewalkHeight,
        --                 trackType = streetName,
        --                 catenary = false,
        --                 trackWidth = street.streetWidth
        --             }
        --             mod.getModelsScript.fileName = "construction/station/rail/mus_track_module.getModelsFn"
        --             mod.getModelsScript.params = {}
        --             api.res.moduleRep.add(mod.fileName, mod, true)
        --         end
        --     end
        -- end
        end
    }
end
