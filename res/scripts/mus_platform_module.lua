local func = require "entry/func"
local coor = require "entry/coor"
local pipe = require "entry/pipe"
local general = require "entry/general"
local mus = require "mus_platform"
local quat = require "entry/quaternion"

local mType = "mus_platform"
local function fn(platformWidth, stairsWidth, desc, order, fakeTracks)
    local platformArcs = mus.platformArcs(platformWidth, stairsWidth)
    return function()
        return {
            availability = {
                yearFrom = 0,
                yearTo = 0,
            },
            buildMode = "SINGLE",
            cost = {
                price = 20000,
            },
            description = desc,
            category = {
                categories = {"platform"},
            },
            type = mType,
            order = {
                value = order,
            },
            metadata = {
                isPlatform = true,
                width = platformWidth,
                type = mType
            },
            
            updateFn = function(result, transform, tag, slotId, addModelFn, params)
                local info = mus.slotInfo(slotId)
                local group = result.group[info.pos.z]
                local allArcs = platformArcs(group.config, group.arcs[info.pos.x])
                
                result.allArcs[slotId] = allArcs
                
                local leftSideWall = group.modules[info.pos.x - 1] and {} or mus.platformSideWallModels(group.config, allArcs, true)
                local rightSideWall = group.modules[info.pos.x + 1] and {} or mus.platformSideWallModels(group.config, allArcs, false)
                
                local sign = mus.platformSigns(group.config, allArcs, info.pos.x, not group.modules[info.pos.x - 1], not group.modules[info.pos.x + 1])
                local chairs = mus.platformChairs(group.config, allArcs, not group.modules[info.pos.x - 1], not group.modules[info.pos.x + 1])
                
                local newModels = mus.platformModels(group.config, allArcs) + leftSideWall + rightSideWall + sign + chairs
                local withTag = general.withTag(tag)
                
                local refArc = pipe.new
                    * fakeTracks
                    * pipe.map(function(f) return allArcs.ref()()(f) end)
                    * pipe.map(pipe.map(mus.arc2Edges))
                    * pipe.flatten()
                    * pipe.flatten()
                    * pipe.map(function(e) return {e[1] .. group.config.transf.pt, e[2] .. group.config.transf.vec} end)
                    * pipe.map(pipe.map(coor.vec2Tuple))
                
                local edges = {
                    type = "TRACK",
                    alignTerrain = false,
                    params = {
                        type = "mus_mock.lua",
                        catenary = false,
                    },
                    edgeType = "TUNNEL",
                    edgeTypeName = "mus_void.lua",
                    edges = refArc,
                    snapNodes = {},
                    tag2nodes = {
                        [tag] = func.seq(0, #refArc - 1)
                    }
                }
                
                local leftTerminals, rightTerminals, linkings, terminalCounts = mus.generateTerminals(allArcs)
                
                table.insert(result.invoke[1], 
                    function() 
                        result.models = result.models
                            + withTag(newModels)
                            + withTag(linkings)
                            + (group.config.isFinalized and {} or withTag({general.newModel("mus/remove_helper.mdl", coor.rotX(math.pi * 0.5) * coor.scale(coor.xyz(3, 3, 1)) * coor.transZ(25) * transform)}))
                        
                        if (group.config.isFinalized) then
                            result.edgeLists = result.edgeLists / edges
                        end
                    end)
                
                group.terminalInfo[info.pos.x] = {
                    {#result.models, #result.models + terminalCounts - 1},
                    {#result.models + terminalCounts, #result.models + terminalCounts + terminalCounts - 1},
                }
                
                result.models = result.models
                    + withTag(leftTerminals)
                    + withTag(rightTerminals)
                
                result.slots = result.slots
                    + func.mapi(allArcs.blockCoords.platform.central.mc,
                        function(mc, p)
                            local i = p < allArcs.count and (p * 2 - 1) or ((allArcs.blockCount - p) * 2 + 2)
                            return {
                                id = slotId + 1000 + i * 100000,
                                transf = quat.byVec(coor.xyz(0, 1, 0), (mc.s - mc.i)):mRot() * coor.trans((mc.s + mc.i) * 0.5),
                                type = "mus_downstairs",
                                spacing = {stairsWidth * 0.5, stairsWidth * 0.5, 2.75, 2.75},
                                shape = 0
                            }
                        end)
                    + func.mapi(mus.interlace(allArcs.blockCoords.platform.central.mc),
                        function(mc, p)
                            local i = p < allArcs.count and (p * 2 - 1) or ((allArcs.blockCount - p) * 2)
                            return {
                                id = slotId + 2000 + i * 100000,
                                transf = quat.byVec(coor.xyz(0, 1, 0), (mc.s.s - mc.i.i)):mRot() * coor.trans(mc.i.s),
                                type = "mus_upstairs",
                                spacing = {stairsWidth * 0.5, stairsWidth * 0.5, 5.25, 5.25},
                                shape = 0
                            }
                        end)
                
                local terrainFaces = mus.terrain(group.config, allArcs.blockCoords.terrain.high)
                table.insert(result.terrainAlignmentLists, {type = "GREATER", faces = terrainFaces})
                if (not group.config.isFinalized) then
                    for _, f in ipairs(terrainFaces) do
                        table.insert(result.groundFaces, {face = f, modes = {{type = "FILL", key = "hole.lua"}}})
                    end
                end
            end,
            
            getModelsFn = function(params)
                return {}
            end
        }
    end
end

return fn