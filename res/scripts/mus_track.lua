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
        local baseL, baseR, c = mus.biLatCoords(5)(arcRef(config.refZ)()(-trackWidth * 0.5), arcRef(config.refZ)()(trackWidth * 0.5))
        
        local coords = {
            ceil = {lc = {}, rc = {}, mc = {}, c = c},
            wall = {lc = {}, rc = {}, mc = {}, c = c},
        }
        
        for i = 1, (c * 2 - 1) do
            local ptL = baseL[i] .. config.transf.pt
            local ptR = baseR[i] .. config.transf.pt
            
            local transL = (ptL - ptR):normalized()
            local function offset(o, ls)
                ls.lc[i] = ptL + transL * o
                ls.rc[i] = ptR - transL * o
                ls.mc[i] = (ptL + ptR) * 0.5
            end
            
            offset(0, coords.ceil)
            offset(-0.05, coords.wall)
        end
        
        -- local tlc, trc, tc = mus.biLatCoords(5)(arcs.terrain.l, arcs.terrain.r)
        local function interlaceCoords(coords)
            return {
                lc = mus.interlace(coords.lc),
                rc = mus.interlace(coords.rc),
                mc = mus.interlace(coords.mc),
                count = c * 2 - 2
            }
        end
        
        local blockCoords = {
            ceil = interlaceCoords(coords.ceil),
            wall = interlaceCoords(coords.wall)
        }
        
        -- local terrain = mus.arcGen(
        --     {
        --         l = arcRef(config.refZ + 7.75)(function(l) return l + 5 end),
        --         r = arcRef(config.refZ + 7.75)(function(l) return l + 5 end)
        --     },
        --     -trackWidth * 0.5)
        return {
            ref = arcRef,
            count = c,
            blockCount = c * 2 - 2,
            coords = coords,
            blockCoords = blockCoords,
            isTrack = true
        }
    end
end

mus.trackModels = function(config, arcs)
    local buildCeil = mus.buildSurface(config, config.refZ, coor.I())
    local ceilTop = pipe.rep(arcs.blockCount)(config.models.top.track.central)
    
    return pipe.new
        * pipe.mapn(
            func.seq(1, arcs.blockCount),
            ceilTop,
            arcs.blockCoords.ceil.lc, arcs.blockCoords.ceil.rc
        )(buildCeil(5))
        * pipe.flatten()
end

mus.trackSideWallModels = function(config, arcRef, isLeft)
    return func.map(
        isLeft and arcRef.blockCoords.wall.lc or arcRef.blockCoords.wall.rc,
        function(ic)
            local vec = ic.i - ic.s
            return general.newModel(config.models.wallTrack .. ".mdl",
                coor.rotZ(isLeft and -0.5 * pi or 0.5 * pi),
                coor.scaleX(vec:length() / 5),
                quat.byVec(coor.xyz(5, 0, 0), vec):mRot(),
                coor.trans(ic.s:avg(ic.i) + coor.xyz(0, 0, -config.refZ)))
        end)
end

mus.trackSigns = function(config, arcs, isLeftmost, isRightmost)
    local transZ = coor.xyz(0, 0, -config.refZ + 4)
    local indices = func.seq(1, arcs.blockCount)
    
    local indicesN = pipe.new * indices
        * pipe.fold({pipe.new}, function(r, i) return i and func.with(r, {[#r] = r[#r] / i}) or func.with(r, {[#r + 1] = pipe.new}) end)
        * pipe.filter(function(g) return #g > 6 end)
        * pipe.map(
            function(g)
                local n = floor(#g / 6)
                local length = #g / n
                return
                    pipe.new
                    * func.seq(1, n)
                    * pipe.map(function(i) return g[1] + length * (i - 0.5) end)
                    * pipe.map(function(p) return p < arcs.count and floor(p) or ceil(p) end)
            end)
        * pipe.flatten()
    
    local fn = function()
        return pipe.mapn(
            indices,
            arcs.blockCoords.wall.lc,
            arcs.blockCoords.wall.rc
        )(function(i, lc, rc)
            if (indicesN * pipe.contains(i)) then
                local transL = quat.byVec(coor.xyz(-1, 0, 0), lc.i - lc.s):mRot() * coor.trans((i < arcs.count and lc.s or lc.i) + transZ)
                local transR = quat.byVec(coor.xyz(1, 0, 0), rc.i - rc.s):mRot() * coor.trans((i < arcs.count and rc.s or rc.i) + transZ)
                return
                    pipe.new
                    / (isLeftmost and func.with(general.newModel("mus/signs/platform_signs_2.mdl", transL), {pos = i}) or nil)
                    / (isRightmost and func.with(general.newModel("mus/signs/platform_signs_2.mdl", transR), {pos = i}) or nil)
            else
                return false
            end
        end
    )
    end
    
    return pipe.new * fn()
        * pipe.filter(pipe.noop())
        * pipe.flatten()

end

return mus
