local func = require "entry/func"
local coor = require "entry/coor"
local pipe = require "entry/pipe"
local general = require "entry/general"
local mus = require "mus_track"

local mType = "mus_track"
return function(trackWidth, trackType, catenary, desc, order)
    return function()
        return {
            availability = {
                yearFrom = 0,
                yearTo = 0,
            },
            buildMode = "SINGLE",
            cost = {
                price = 5000,
            },
            description = desc,
            category = {
                categories = {"track"},
            },
            type = mType,
            order = {
                value = order,
            },
            metadata = {
                isTrack = true,
                width = trackWidth,
                type = mType
            },
            
            updateFn = function(result, transform, tag, slotId, addModelFn, params)
                local withTag = general.withTag(tag)
                local info = mus.slotInfo(slotId)
                local group = result.group[info.pos.z]
                local allArcs = mus.trackArcs(trackWidth)(group.config, group.arcs[info.pos.x])
                result.allArcs[slotId] = allArcs

                local refArc = pipe.new 
                    * allArcs.ref()()() 
                    * pipe.map(mus.arc2Edges) 
                    * pipe.flatten()
                    * pipe.map(function(e) return {e[1] .. group.config.transf.pt, e[2] .. group.config.transf.vec} end)
                    * pipe.map(pipe.map(coor.vec2Tuple))

                local edges = {
                    type = "TRACK",
                    alignTerrain = false,
                    params = {
                        type = trackType,
                        catenary = catenary,
                    },
                    edgeType = group.config.isFinalized and "TUNNEL" or nil,
                    edgeTypeName = "mus_void.lua",
                    edges = refArc,
                    snapNodes = {5, 11},
                    tag2nodes = {
                        [tag] = func.seq(0, #refArc - 1)
                    }
                }
                
                local leftSideWall = group.modules[info.pos.x - 1] and {} or mus.trackSideWallModels(group.config, allArcs, true) 
                local rightSideWall = group.modules[info.pos.x + 1] and {} or mus.trackSideWallModels(group.config, allArcs, false) 
                
                local sign = mus.trackSigns(group.config, allArcs, not group.modules[info.pos.x - 1], not group.modules[info.pos.x + 1])

                local newModels = mus.trackModels(group.config, allArcs) + leftSideWall + rightSideWall + sign
                group.terminalInfo[info.pos.x] = func.fold(result.edgeLists, 0, function(sum, e) return sum + #e.edges end)
                
                table.insert(result.invoke[1], 
                    function() 
                        result.models = result.models 
                            + withTag(newModels)
                            + (group.config.isFinalized and {} or withTag({general.newModel("mus/remove_helper.mdl", coor.rotX(math.pi * 0.5) * coor.scale(coor.xyz(3, 3, 1)) * coor.transZ(25) * transform)}))
                    end)
                
                result.edgeLists = result.edgeLists / edges

                
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