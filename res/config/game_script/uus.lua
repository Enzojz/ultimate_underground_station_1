-- local dump = require "luadump"
local pipe = require "entry/pipe"
local func = require "entry/func"
local coor = require "entry/coor"
local dump = require "luadump"

local state = {
    constructionSlotConfig = {}
}

local cov = function(m)
    return func.seqMap({0, 3}, function(r)
        return func.seqMap({1, 4}, function(c)
            return m[r * 4 + c]
        end)
    end)
end

local pure = function(pa)
    local params = {}
    for key, value in pairs(pa) do
        if (key ~= "seed") then
            params[key] = value
        end
    end
    return params
end

local script = {
    save = function() return state end,
    load = function(data)
        if data then
            state.constructionSlotConfig = {}
            for k, v in pairs(data.constructionSlotConfig) do
                state.constructionSlotConfig[k] = data.constructionSlotConfig[k]
            end
        end
    end,
    handleEvent = function(src, id, name, param)
    end,
    guiHandleEvent = function(id, name, param)
        if name == "builder.apply" then
            local toRemove = param.proposal.toRemove
            local toAdd = param.proposal.toAdd
            if toRemove then
                end
            if toAdd and #toAdd > 0 then
                for i = 1, #toAdd do
                    local con = toAdd[i]
                    if (con.fileName == [[station/rail/mus2.con]]) then
                        
                        end
                end
            end
        end
    end
}

function data()
    return script
end
