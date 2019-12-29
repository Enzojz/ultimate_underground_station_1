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

mus.platformArcs = function(platformWidth, stairsWidth)
    return function(config, arcs)
        local refZ = config.hPlatform
        local arcRef, arcL, arcR = unpack(arcs)
        local general = {
            l = arcL(refZ)(),
            r = arcR(refZ)()
        }
        
        local arcs = {
            platform = {
                lane = mus.arcGen(general, 0.6),
                edge = mus.arcGen(general, -0.5),
                central = mus.arcGen(general, 0.3)
            },
            ceil = {
                edge = mus.arcGen(general, -0.5),
                central = mus.arcGen(general, 0.2),
            },
            stairs = {
                outer = mus.arcGen(general, (platformWidth - stairsWidth) * 0.5 + 0.3),
                inner = mus.arcGen(general, (platformWidth - stairsWidth) * 0.5 + 0.55)
            },
            terrain = mus.arcGen({
                l = arcL(refZ + 7.75)(function(l) return l + 5 end),
                r = arcR(refZ + 7.75)(function(l) return l + 5 end)
            }, -0.5)
        }
        
        local lsc, rsc, lsuc, rsuc, lc, rc, lpc, rpc, lpic, rpic, lsoc, rsoc, lsic, rsic, c = mus.biLatCoords(5)(
            arcs.platform.edge.l, arcs.platform.edge.r,
            arcs.platform.central.l, arcs.platform.central.r,
            arcs.platform.lane.l, arcs.platform.lane.r,
            arcs.ceil.edge.l, arcs.ceil.edge.r,
            arcs.ceil.central.l, arcs.ceil.central.r,
            arcs.stairs.outer.l, arcs.stairs.outer.r,
            arcs.stairs.inner.l, arcs.stairs.inner.r
        )
        local tlc, trc, tc = mus.biLatCoords(5)(arcs.terrain.l, arcs.terrain.r)
        return {
            [1] = arcL,
            [2] = arcR,
            [3] = arcRef,
            count = c,
            platform = {
                edge = func.with(arcs.platform.edge, {lc = lsc, rc = rsc, mc = mus.mc(lsc, rsc), c = c}),
                central = func.with(arcs.platform.central, {lc = lsuc, rc = rsuc, mc = mus.mc(lsuc, rsuc), c = c}),
                lane = func.with(arcs.platform.lane, {lc = lc, rc = rc, mc = mus.mc(lc, rc), c = c})
            },
            ceil = {
                edge = func.with(arcs.ceil.edge, {lc = lpc, rc = rpc, mc = mus.mc(lpc, rpc), c = c}),
                central = func.with(arcs.ceil.central, {lc = lpic, rc = rpic, mc = mus.mc(lpic, rpic), c = c}),
            },
            stairs = {
                outer = func.with(arcs.stairs.outer, {lc = lsoc, rc = rsoc, mc = mus.mc(lsoc, rsoc), c = c}),
                inner = func.with(arcs.stairs.inner, {lc = lsic, rc = rsic, mc = mus.mc(lsic, rsic), c = c}),
            },
            terrain = func.with(arcs.terrain, {lc = tlc, rc = trc, mc = mus.mc(tlc, trc), c = tc}),
            isPlatform = true
        }
    
    end
end

mus.platformModels = function(config, arcs, fitModel)
    local tZ = coor.transZ(config.hPlatform - 1.4) -- model height = 1.93 - 1.4 -> 0.53 -> adjust model level to rail level
    local platformZ = config.hPlatform + 0.53 --target Z
    
    local buildPlatform = mus.buildSurface(fitModel, config, platformZ, tZ)
    local buildCeil = mus.buildSurface(fitModel, config, platformZ, coor.I())
    local buildWall = mus.buildSurface(fitModel, config, platformZ, coor.scaleZ(5 - platformZ) * coor.transZ(platformZ))
    
    
    local c = arcs.count
    local cModels = 2 * c - 2
    local indices = func.seq(1, cModels)
    local fnModels = function(normal, down, upA, upB)
        local fn = function(i)
            return normal
        -- local posD, posA, posB = stepPos(i)
        -- if posD then return down
        -- elseif posA then return upA
        -- elseif posB then return upB
        -- else return normal end
        end
        return pipe.new * indices * pipe.map(fn)
    end
    
    local models = {
        platform = {
            left = pipe.rep(cModels)(config.models.platform.left),
            right = pipe.rep(cModels)(config.models.platform.right),
            edgeLeft = pipe.rep(cModels)(config.models.platform.edgeLeft),
            edgeRight = pipe.rep(cModels)(config.models.platform.edgeRight),
            central = pipe.rep(cModels)(config.models.platform.central)
        },
        
        stair = {
            central = fnModels(false, config.models.downstep.central, config.models.upstep.a, config.models.upstep.b),
            left = fnModels(false, config.models.downstep.left, config.models.upstep.aLeft, config.models.upstep.bLeft),
            right = fnModels(false, config.models.downstep.right, config.models.upstep.aRight, config.models.upstep.bRight),
            inner = fnModels(false, false, config.models.upstep.aInner, config.models.upstep.bInner),
            back = fnModels(false, config.models.downstep.back, false, config.models.upstep.back)
        },
        
        ceil = {
            left = fnModels(config.models.ceil.left, config.models.ceil.left, config.models.ceil.aLeft, config.models.ceil.bLeft),
            right = fnModels(config.models.ceil.right, config.models.ceil.right, config.models.ceil.aRight, config.models.ceil.bRight),
            edge = pipe.rep(cModels)(config.models.ceil.edge),
            central = fnModels(config.models.ceil.central, config.models.ceil.central, false, false)
        },
        
        top = {
            central = fnModels(config.models.top.platform.central, config.models.top.platform.central, false, false),
            left = pipe.rep(cModels)(config.models.top.platform.left),
            right = pipe.rep(cModels)(config.models.top.platform.right)
        }
    }
    
    local platforms = pipe.new
        + pipe.mapn(
            indices,
            models.platform.central,
            mus.interlace(arcs.stairs.outer.lc), mus.interlace(arcs.stairs.outer.rc)
        )(buildPlatform(c, 5, function(i, lc, rc) return
            i >= c
            and mus.assembleSize(lc, rc)
            or mus.assembleSize({s = rc.i, i = rc.s}, {s = lc.i, i = lc.s})
        end))
        + pipe.mapn(
            indices,
            models.platform.left,
            mus.interlace(arcs.platform.central.lc), mus.interlace(arcs.stairs.outer.lc)
        )(buildPlatform(c, 1.7))
        + pipe.mapn(
            indices,
            models.platform.right,
            mus.interlace(arcs.stairs.outer.rc), mus.interlace(arcs.platform.central.rc)
        )(buildPlatform(c, 1.7))
        + pipe.mapn(
            indices,
            models.platform.edgeLeft,
            mus.interlace(arcs.platform.edge.lc), mus.interlace(arcs.platform.central.lc)
        )(buildPlatform(c, 0.8))
        + pipe.mapn(
            indices,
            models.platform.edgeRight,
            mus.interlace(arcs.platform.central.rc), mus.interlace(arcs.platform.edge.rc)
        )(buildPlatform(c, 0.8))
    
    local ceils =
        pipe.new
        + pipe.mapn(
            indices,
            models.ceil.central,
            mus.interlace(arcs.stairs.outer.lc), mus.interlace(arcs.stairs.outer.rc)
        )(buildCeil(c, 5))
        + pipe.mapn(
            indices,
            models.ceil.left,
            mus.interlace(arcs.ceil.central.lc), mus.interlace(arcs.stairs.outer.lc)
        )(buildCeil(c, 1.8))
        + pipe.mapn(
            indices,
            models.ceil.right,
            mus.interlace(arcs.stairs.outer.rc), mus.interlace(arcs.ceil.central.rc)
        )(buildCeil(c, 1.8))
        + pipe.mapn(
            indices,
            models.ceil.edge,
            mus.interlace(arcs.ceil.edge.lc), mus.interlace(arcs.ceil.central.lc)
        )(buildCeil(c, 0.7))
        + pipe.mapn(
            indices,
            models.ceil.edge,
            mus.interlace(func.rev(arcs.ceil.edge.rc)), mus.interlace(func.rev(arcs.ceil.central.rc))
        )(buildCeil(c, 0.7))
    
    local tops = pipe.new
        + pipe.mapn(
            indices,
            models.top.central,
            mus.interlace(arcs.stairs.outer.lc), mus.interlace(arcs.stairs.outer.rc)
        )(buildCeil(c, 5))
        + pipe.mapn(
            indices,
            models.top.left,
            mus.interlace(arcs.ceil.edge.lc), mus.interlace(arcs.stairs.outer.lc)
        )(buildCeil(c, 2.5))
        + pipe.mapn(
            indices,
            models.top.right,
            mus.interlace(arcs.stairs.outer.rc), mus.interlace(arcs.ceil.edge.rc)
        )(buildCeil(c, 2.5))
    
    local extremity = pipe.mapn(
        {
            {arcs.ceil.edge.lc[1], arcs.ceil.central.lc[1]},
            {arcs.ceil.central.lc[1], arcs.ceil.central.rc[1]},
            {arcs.ceil.central.rc[1], arcs.ceil.edge.rc[1]},
            {arcs.ceil.central.lc[c * 2 - 1], arcs.ceil.edge.lc[c * 2 - 1]},
            {arcs.ceil.central.rc[c * 2 - 1], arcs.ceil.central.lc[c * 2 - 1]},
            {arcs.ceil.edge.rc[c * 2 - 1], arcs.ceil.central.rc[c * 2 - 1]},
        },
        {
            config.models.wallExtremityEdge .. "_left", config.models.wallExtremity, config.models.wallExtremityEdge .. "_right",
            config.models.wallExtremityEdge .. "_left", config.models.wallExtremity, config.models.wallExtremityEdge .. "_right"
        },
        {
            0.7, 8.6, 0.7,
            0.7, 8.6, 0.7
        }
    )
    (function(c, m, w)
        local lc, rc = unpack(c)
        local vec = rc - lc
        return general.newModel(m .. ".mdl",
            coor.scale(coor.xyz(vec:length() / w, 1, 5 - platformZ)),
            quat.byVec(coor.xyz(1, 0, 0), vec):mRot(),
            coor.trans(lc:avg(rc))
    )
    end)
    
    local extremityPlatform = pipe.mapn(
        {
            {arcs.ceil.edge.lc[1], arcs.ceil.central.lc[1]},
            {arcs.ceil.central.lc[1], arcs.ceil.edge.rc[1]},
            {arcs.ceil.edge.lc[c * 2 - 1], arcs.ceil.central.lc[c * 2 - 1]},
            {arcs.ceil.central.rc[c * 2 - 1], arcs.ceil.edge.rc[c * 2 - 1]},
        },
        {"l", "r", "r", "l"},
        {
            coor.I(), coor.I(),
            coor.rotZ(pi), coor.rotZ(pi)
        }
    )
    (function(c, p, r)
        local lc, rc = unpack(c)
        local vec = rc - lc
        return {
            general.newModel(config.models.wallExtremityPlatform .. "_" .. p .. ".mdl",
                coor.transZ(-platformZ),
                tZ, r,
                quat.byVec(coor.xyz(1, 0, 0), vec):mRot(),
                coor.trans(lc:avg(rc))
            ),
            general.newModel(config.models.wallExtremityTop .. "_" .. p .. ".mdl",
                coor.transZ(-platformZ),
                r,
                quat.byVec(coor.xyz(1, 0, 0), vec):mRot(),
                coor.trans(lc:avg(rc))
        )
        }
    end)
    
    return (pipe.new / platforms / ceils / tops / extremityPlatform) * pipe.flatten() * pipe.flatten() + extremity
end

return mus
