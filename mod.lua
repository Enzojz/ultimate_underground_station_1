local trackIndices = {
    ["standard.lua"] = "mus_track_std",
    ["high_speed.lua"] = "mus_track_hs",
}

function data()
    return {
        info = {
            minorVersion = 5,
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
            local tracks = api.res.trackTypeRep.getAll()
            local moduleList = {}
            local trackIconList = {}
            local trackNames = {}
            for __, trackName in pairs(tracks) do
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

                    mod.category.categories = catenary and {_("TRACK_CAT")} or {_("TRACK")}

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
            local nCon = api.type.ConstructionDesc.new()

            nCon.fileName = con.fileName
            nCon.type = con.type
            nCon.description = con.description
            nCon.availability.yearFrom = con.availability.yearFrom
            nCon.availability.yearTo = con.availability.yearTo
            nCon.buildMode = con.buildMode
            nCon.categories = con.categories
            nCon.order = con.order
            nCon.skipCollision = con.skipCollision
            nCon.autoRemovable = con.autoRemovable

            for i = 1, #con.params do
                local p = con.params[i]
                local param = api.type.ScriptParam.new()
                param.key = p.key
                param.name = p.name
                param.values = p.values
                param.defaultIndex = p.defaultIndex or 0
                param.uiType = p.uiType
                nCon.params[i] = param
            end

            local temp = api.type.ConstructionTemplate.new()
            temp.desc.name = con.constructionTemplates[1].desc.name
            temp.desc.description = con.constructionTemplates[1].desc.description
            temp.constructionType = api.type.enum.ConstructionType.RAIL_STATION

            local params = api.type.DynamicConstructionTemplate.new()
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
                params.params[i] = param
            end
            temp.data = params

            nCon.constructionTemplates[1] = temp
            nCon.createTemplateScript.fileName = "construction/station/rail/mus_s.createTemplateFn"
            nCon.createTemplateScript.params = {moduleList = moduleList, trackIconList = trackIconList}
            nCon.updateScript.fileName = "construction/station/rail/mus_s.updateFn"
            nCon.upgradeScript.fileName = "construction/station/rail/mus_s.upgradeFn"
            nCon.upgradeScript.params = {moduleList = moduleList, trackIconList = trackIconList}
            nCon.preProcessScript.fileName = "construction/station/rail/mus_s.preProcessFn"

            api.res.constructionRep.add(nCon.fileName .. "_a", nCon, true) -- fileName, resource, visible

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
