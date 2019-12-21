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
musm.trackLengths = {40, 60, 80, 100, 140, 160, 200, 240, 320, 400, 480, 500, 550, 850, 1050}
musm.trackNumberList = {1, 2, 3, 4, 5, 6, 7, 8, 10, 12, 14, 16, 18, 20}

musm.exclu = function(...)
    local keys = {...}
    return pipe.filter(function(i) return not func.contains(keys, i.key) end)
end

return musm
