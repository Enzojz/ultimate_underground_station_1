local func = require "entry/func"
local coor = require "entry/coor"
local arc = require "mus/coorarc"
local line = require "mus/coorline"
local quat = require "entry/quaternion"
local pipe = require "entry/pipe"
local general = require "entry/general"
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
    return function(config, arcRef)
        local refZ = config.hPlatform + 0.53
        
        local ceil = mus.arcGen(
            {
                l = arcRef(refZ)(),
                r = arcRef(refZ)()
            },
            -trackWidth * 0.5)
        
        local terrain = mus.arcGen(
            {
                l = arcRef(refZ + 7.75)(function(l) return l + 5 end),
                r = arcRef(refZ + 7.75)(function(l) return l + 5 end)
            },
            -trackWidth * 0.5)
        
        local lpc, rpc, c = mus.biLatCoords(5)(ceil.l, ceil.r)
        local ltc, rtc, tc = mus.biLatCoords(5)(terrain.l, terrain.r)
        
        return {
            ref = arcRef,
            count = c,
            ceil = func.with(ceil, {lc = lpc, rc = rpc, mc = mus.mc(lpc, rpc), c = c}),
            terrain = func.with(terrain, {lc = ltc, rc = rtc, mc = mus.mc(ltc, rtc), c = tc}),
            isTrack = true
        }
    end
end

mus.trackModels = function(config, arcs)
    local fitModel = config.fitModel
    local platformZ = config.hPlatform + 0.53 --target Z
    local buildCeil = mus.buildSurface(fitModel, config, platformZ, coor.I())
    local ceilTop = pipe.rep(2 * arcs.ceil.c - 2)(config.models.top.track.central)
    
    return pipe.new
        * pipe.mapn(
            func.seq(1, 2 * arcs.ceil.c - 2),
            ceilTop,
            mus.interlace(arcs.ceil.lc), mus.interlace(arcs.ceil.rc)
        )(buildCeil(5))
        * pipe.flatten()
end

mus.trackSideWallModels = function(config, arcRef, isLeft)
    local platformZ = config.hPlatform + 0.53
    local c = isLeft and arcRef.ceil.lc or arcRef.ceil.rc
    local newModels =
        pipe.new * mus.interlace(c)
        * pipe.map(function(ic)
            local vec = ic.i - ic.s
            return general.newModel(config.models.wallTrack .. ".mdl",
                coor.rotZ(isLeft and -0.5 * pi or 0.5 * pi),
                coor.scaleX(vec:length() / 5),
                quat.byVec(coor.xyz(5, 0, 0), vec):mRot(),
                coor.trans(ic.s:avg(ic.i) + coor.xyz(0, 0, -platformZ)))
        end)
    return newModels
end


return mus
