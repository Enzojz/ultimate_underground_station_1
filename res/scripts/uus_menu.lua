local func = require "uus/func"
local pipe = require "uus/pipe"
local uus = require "uus"
local uusm = {}

uusm.slopeList = {0, 2.5, 5, 7.5, 10, 12.5, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 90, 100}
uusm.rList = {uus.infi * 0.001, 5, 3.5, 2, 1, 4 / 5, 2 / 3, 3 / 5, 1 / 2, 1 / 3, 1 / 4, 1 / 5, 1 / 6, 1 / 8, 1 / 10, 1 / 20}
uusm.hPlatformList = {200, 280, 380, 550, 680, 760, 915, 960, 1100, 1219, 1250, 1380}
uusm.wPlatformList = {5, 10, 15}
uusm.wStairsList = {3, 7, 7}
uusm.hStation = {0, 1, 2, 3, 4, 5, 6}
uusm.roofLengthList = {100, 95, 80, 75, 50, 25, 0}
uusm.extWidthList = {100, 75, 50, 25, 10}
uusm.extLengthList = {100, 90, 80, 75, 70, 65, 60, 55, 50}
uusm.varUnaffectedList = {0, 10, 25, 50, 75, 90}
uusm.yOffsetList = {0, 10, 20, 30, 40}
uusm.trackLengths = {40, 60, 80, 100, 140, 160, 200, 240, 320, 400, 480, 500, 550, 850, 1050}
uusm.trackNumberList = {1, 2, 3, 4, 5, 6, 7, 8, 10, 12, 14, 16, 18, 20}
uusm.convAngle = {0, 5, 15, 30, 45, 60, 75, 90}
uusm.trackList = {"standard.lua", "high_speed.lua"}
uusm.trackWidthList = {5, 5}
uusm.fencesLengthList = {2, 2.5, 2}
uusm.middlePlatformLength = {0, 20, 25, 33, 45, 50, 55, 66, 75, 80, 100}


local sp = "·:·:·:·:·:·:·:·:·:·:·:·:·:·:·:·:·:·:·:·:·:·:·:·:·\n"

uusm.trackType = pipe.exec * function()
    local list = {
        {
            key = "trackType",
            name = _("Track type"),
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
        commonapi.uiparameter.modifyTrackCatenary(list, {selectionlist = uusm.trackList})
        uusm.trackWidthList = func.map(uusm.trackList, function(e) return (function(w) return (w and w > 0) and w or 5 end)(commonapi.repos.track.getByName(e).data.trackDistance) end)
    end
    
    return list
end

uusm.slope = {
    {
        key = "slopeSign",
        name = sp,
        values = {"+", "-"},
        defaultIndex = 0
    },
    {
        key = "slope",
        name = _("Slope") .. " " .. "(‰)",
        values = func.map(uusm.slopeList, tostring),
        defaultIndex = 0
    }
}

uusm.alt = {
    {
        key = "altitudeSign",
        name = sp,
        values = {"+", "-"},
        defaultIndex = 0
    },
    {
        key = "altitude",
        name = _("General Altitude") .. "(m)",
        values = func.map(uusm.hStation, tostring),
        defaultIndex = 0
    }
}

uusm.platform = {
    {
        key = "hPlatform",
        name = _("Height") .. " " .. "(mm)",
        values = func.map(uusm.hPlatformList, tostring),
        defaultIndex = 3
    },
    {
        key = "wPlatform",
        name = _("Width") .. " " .. "(m)",
        values = func.map(uusm.wPlatformList, tostring),
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

uusm.exclu = function(...)
    local keys = {...}
    return pipe.filter(function(i) return not func.contains(keys, i.key) end)
end

return uusm
