local func = require "entry/func"
local coor = require "entry/coor"
local arc = require "mus/coorarc"
local line = require "mus/coorline"
local quat = require "entry/quaternion"
local station = require "mus/stationlib"
local pipe = require "entry/pipe"
local dump = require "luadump"

local mus = require "mus"

local math = math
local pi = math.pi
local abs = math.abs
local ceil = math.ceil
local floor = math.floor
local pow = math.pow
local min = math.min
local e = math.exp(1)
local unpack = table.unpack

mus.trackArcs = function(trackWidth)
    return function(config, arcs)
        local refZ = config.hPlatform
        
        local arc = arcs[1]
        local ceil = mus.arcGen(
            {
                l = arc(refZ)(),
                r = arc(refZ)()
            },
            -trackWidth * 0.5)
        
        local terrain = mus.arcGen(
            {
                l = arc(refZ + 7.75)(function(l) return l + 5 end),
                r = arc(refZ + 7.75)(function(l) return l + 5 end)
            },
            -trackWidth * 0.5)
        
        local lpc, rpc, c = mus.biLatCoords(5)(ceil.l, ceil.r)
        local ltc, rtc, tc = mus.biLatCoords(5)(terrain.l, terrain.r)
        
        return {
            [1] = arc,
            count = c,
            ceil = func.with(ceil, {lc = lpc, rc = rpc, mc = mus.mc(lpc, rpc), c = c}),
            terrain = func.with(terrain, {lc = ltc, rc = rtc, mc = mus.mc(ltc, rtc), c = tc}),
            isTrack = true
        }
    end
end

mus.trackModels = function(config, arcs, fitModel)
    local platformZ = config.hPlatform + 0.53 --target Z
    local buildCeil = mus.buildSurface(fitModel, config, platformZ, coor.I())
    local ceilTop = pipe.rep(2 * arcs.ceil.c - 2)(config.models.top.track.central)
    
    return pipe.new
        * pipe.mapn(
            func.seq(1, 2 * arcs.ceil.c - 2),
            ceilTop,
            mus.interlace(arcs.ceil.lc), mus.interlace(arcs.ceil.rc)
        )(buildCeil(arcs.ceil.c, 5))
        * pipe.flatten()
end


return mus
