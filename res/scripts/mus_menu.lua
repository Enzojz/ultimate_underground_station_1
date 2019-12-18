local func = require "entry/func"
local pipe = require "entry/pipe"
local musm = {}

local infi = 1e8
musm.slopeList = {0, 2.5, 5, 7.5, 10, 12.5, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 90, 100}
musm.rList = {infi * 0.001, 5, 3.5, 2, 1, 4 / 5, 2 / 3, 3 / 5, 1 / 2, 1 / 3, 1 / 4, 1 / 5, 1 / 6, 1 / 8, 1 / 10, 1 / 20}
musm.hPlatformList = {200, 280, 380, 550, 680, 760, 915, 960, 1100, 1219, 1250, 1380}
musm.wPlatformList = {5, 10, 15}
musm.wStairsList = {3, 7, 7}
musm.hStation = {0, 1, 2, 3, 4, 5, 6}
musm.extWidthList = {100, 75, 50, 25, 10}
musm.extLengthList = {100, 90, 80, 75, 70, 65, 60, 55, 50}
musm.varUnaffectedList = {0, 10, 25, 50, 75, 90}
musm.yOffsetList = {0, 10, 20, 30, 40}
musm.trackLengths = {40, 60, 80, 100, 140, 160, 200, 240, 320, 400, 480, 500, 550, 850, 1050}
musm.trackNumberList = {1, 2, 3, 4, 5, 6, 7, 8, 10, 12, 14, 16, 18, 20}
musm.convAngle = {0, 5, 15, 30, 45, 60, 75, 90}
musm.trackList = {"standard.lua", "high_speed.lua"}
musm.trackWidthList = {5, 5}
musm.fencesLengthList = {2, 2.5, 2}
musm.middlePlatformLength = {0, 20, 25, 33, 45, 50, 55, 66, 75, 80, 100}


local sp = "·:·:·:·:·:·:·:·:·:·:·:·:·:·:·:·:·:·:·:·:·:·:·:·:·\n"

musm.trackType = pipe.exec * function()
    local list = {
        {
            key = "trackType",
            name = sp .. "\n" .. _("Track type"),
            values = {_("Standard"), _("High-speed")},
            yearFrom = 1925,
            yearTo = 0
        },
        {
            key = "catenary",
            name = _("Catenary"),
            values = {_("No"), _("Yes")},
            defaultIndex = 1,
            yearFrom = 1910,
            yearTo = 0
        }
    }
    if (commonapi and commonapi.uiparameter) then
        commonapi.uiparameter.modifyTrackCatenary(list, {selectionlist = musm.trackList})
        musm.trackWidthList = func.map(musm.trackList, function(e) return (function(w) return (w and w > 0) and w or 5 end)(commonapi.repos.track.getByName(e).data.trackDistance) end)
    end
    
    return list
end

musm.slope = {
    {
        key = "slopeSign",
        name = sp,
        values = {"+", "-"},
        defaultIndex = 0
    },
    {
        key = "slope",
        name = _("Slope") .. " " .. "(‰)",
        values = func.map(musm.slopeList, tostring),
        defaultIndex = 0
    }
}

musm.platform = {
    {
        key = "hPlatform",
        name = _("Height") .. " " .. "(mm)",
        values = func.map(musm.hPlatformList, tostring),
        defaultIndex = 3
    },
    {
        key = "wPlatform",
        name = _("Width") .. " " .. "(m)",
        values = func.map(musm.wPlatformList, tostring),
        defaultIndex = 1
    },
    {
        key = "hasLeftPlatform",
        name = _("Leftmost Platform"),
        values = {_("No"), _("Yes")},
        defaultIndex = 1
    },
    {
        key = "hasRightPlatform",
        name = _("Rightmost Platform"),
        values = {_("No"), _("Yes")},
        defaultIndex = 1
    }
}

musm.exclu = function(...)
    local keys = {...}
    return pipe.filter(function(i) return not func.contains(keys, i.key) end)
end

return musm
