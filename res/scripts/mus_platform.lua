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
        local refZ = config.hPlatform + 0.53
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
        local function interlaceCoords(coords)
            return {
                lc = mus.interlace(coords.lc),
                rc = mus.interlace(coords.rc),
                mc = mus.interlace(coords.mc),
                count = c * 2 - 2
            }
        end
        local coords = {
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
        }
        
        local blockCoords = {
            platform = {
                edge = interlaceCoords(coords.platform.edge),
                central = interlaceCoords(coords.platform.central),
                lane = interlaceCoords(coords.platform.lane)
            },
            ceil = {
                edge = interlaceCoords(coords.ceil.edge),
                central = interlaceCoords(coords.ceil.central)
            },
            stairs = {
                outer = interlaceCoords(coords.stairs.outer),
                inner = interlaceCoords(coords.stairs.inner)
            },
            terrain = interlaceCoords(coords.terrain)
        }
        
        return {
            [1] = arcL,
            [2] = arcR,
            [3] = arcRef,
            count = c,
            blockCount = c * 2 - 2,
            coords = coords,
            blockCoords = blockCoords,
            isPlatform = true
        }
    
    end
end

mus.upstairsModels = function(config, arcs, fitModel, pos)
    local tZ = coor.transZ(config.hPlatform - 1.4)-- model height = 1.93 - 1.4 -> 0.53 -> adjust model level to rail level
    local platformZ = config.hPlatform + 0.53 --target Z
    
    local buildPlatform = mus.buildSurface(fitModel, config, platformZ, tZ)
    local buildCeil = mus.buildSurface(fitModel, config, platformZ, coor.I())
    local buildWall = mus.buildSurface(fitModel, config, platformZ, coor.scaleZ(5 - platformZ) * coor.transZ(platformZ))
    
    local c = arcs.count
    
    local fnSizeSym = function(i, lc, rc) return
        i >= c
        and mus.assembleSize(lc, rc)
        or mus.assembleSize({s = rc.i, i = rc.s}, {s = lc.i, i = lc.s})
    end
    
    local models = {
        platform = {
            left = config.models.platform.left,
            right = config.models.platform.right,
            edgeLeft = config.models.platform.edgeLeft,
            edgeRight = config.models.platform.edgeRight,
        },
        
        stair = {
            central = {config.models.upstep.a, config.models.upstep.b},
            left = {config.models.upstep.aLeft, config.models.upstep.bLeft},
            right = {config.models.upstep.aRight, config.models.upstep.bRight},
            inner = {config.models.upstep.aInner, config.models.upstep.bInner},
            back = config.models.upstep.back
        },
        
        ceil = {
            left = {config.models.ceil.aLeft, config.models.ceil.bLeft},
            right = {config.models.ceil.aRight, config.models.ceil.bRight},
            edge = {config.models.ceil.edge, config.models.ceil.edge},
        },
        
        top = {
            left = config.models.top.platform.left,
            right = config.models.top.platform.right
        }
    }
    
    local steps = pipe.new
        / buildWall(4.5, fnSizeSym)(
            pos,
            models.stair.back,
            arcs.blockCoords.stairs.inner.lc[pos + 1],
            arcs.blockCoords.stairs.inner.rc[pos + 1]
        )
        +
        pipe.mapn(
            {pos, pos + 1},
            models.stair.central,
            {arcs.blockCoords.stairs.inner.lc[pos], arcs.blockCoords.stairs.inner.lc[pos + 1]},
            {arcs.blockCoords.stairs.inner.rc[pos], arcs.blockCoords.stairs.inner.rc[pos + 1]}
        )(buildPlatform(4.5, fnSizeSym))
        +
        pipe.mapn(
            {pos, pos + 1},
            models.stair.inner,
            {arcs.blockCoords.stairs.inner.lc[pos], arcs.blockCoords.stairs.inner.lc[pos + 1]},
            {arcs.blockCoords.stairs.inner.rc[pos], arcs.blockCoords.stairs.inner.rc[pos + 1]}
        )(buildPlatform(4.5, fnSizeSym))
        +
        pipe.mapn(
            {pos, pos + 1},
            models.stair.left,
            {arcs.blockCoords.stairs.outer.lc[pos], arcs.blockCoords.stairs.outer.lc[pos + 1]},
            {arcs.blockCoords.stairs.inner.lc[pos], arcs.blockCoords.stairs.inner.lc[pos + 1]},
            {arcs.blockCoords.stairs.outer.rc[pos], arcs.blockCoords.stairs.outer.rc[pos + 1]},
            {arcs.blockCoords.stairs.inner.rc[pos], arcs.blockCoords.stairs.inner.rc[pos + 1]}
        )(buildWall(0.25,
            function(i, loc, lic, roc, ric) return
                i >= c
                and mus.assembleSize(loc, lic)
                or mus.assembleSize({s = roc.i, i = roc.s}, {s = ric.i, i = ric.s})
            end))
        +
        pipe.mapn(
            {pos, pos + 1},
            models.stair.right,
            {arcs.blockCoords.stairs.outer.lc[pos], arcs.blockCoords.stairs.outer.lc[pos + 1]},
            {arcs.blockCoords.stairs.inner.lc[pos], arcs.blockCoords.stairs.inner.lc[pos + 1]},
            {arcs.blockCoords.stairs.outer.rc[pos], arcs.blockCoords.stairs.outer.rc[pos + 1]},
            {arcs.blockCoords.stairs.inner.rc[pos], arcs.blockCoords.stairs.inner.rc[pos + 1]}
        )(buildWall(0.25,
            function(i, loc, lic, roc, ric) return
                i >= c
                and mus.assembleSize(ric, roc)
                or mus.assembleSize({s = lic.i, i = lic.s}, {s = loc.i, i = loc.s})
            end))
    
    local platforms = func.seqMap({0, 1},
        function(i)
            local pos = pos + i
            return pipe.new
                + buildPlatform(1.7)(
                    pos,
                    models.platform.left,
                    arcs.blockCoords.platform.central.lc[pos],
                    arcs.blockCoords.stairs.outer.lc[pos]
                )
                + buildPlatform(1.7)(
                    pos,
                    models.platform.right,
                    arcs.blockCoords.stairs.outer.rc[pos],
                    arcs.blockCoords.platform.central.rc[pos]
                )
                + buildPlatform(0.8)(
                    pos,
                    models.platform.edgeLeft,
                    arcs.blockCoords.platform.edge.lc[pos],
                    arcs.blockCoords.platform.central.lc[pos]
                )
                + buildPlatform(0.8)(
                    pos,
                    models.platform.edgeRight,
                    arcs.blockCoords.platform.central.rc[pos],
                    arcs.blockCoords.platform.edge.rc[pos]
        )
        end
    )
    local ceils =
        pipe.mapn(
            {0, 1},
            models.ceil.left,
            models.ceil.right,
            models.ceil.edge
        )(function(i, left, right, edge)
            local pos = pos + i
            return pipe.new
                + buildCeil(1.7)(
                    pos,
                    left,
                    arcs.blockCoords.ceil.central.lc[pos],
                    arcs.blockCoords.stairs.outer.lc[pos]
                )
                + buildCeil(1.7)(
                    pos,
                    right,
                    arcs.blockCoords.stairs.outer.rc[pos],
                    arcs.blockCoords.ceil.central.rc[pos]
                )
                + buildCeil(0.8)(
                    pos,
                    edge,
                    arcs.blockCoords.ceil.edge.lc[pos],
                    arcs.blockCoords.ceil.central.lc[pos]
                )
                + buildCeil(0.8, function(i, lc, rc) return mus.assembleSize({s = rc.i, i = rc.s}, {s = lc.i, i = lc.s}) end)(
                    pos,
                    edge,
                    arcs.blockCoords.ceil.central.rc[pos],
                    arcs.blockCoords.ceil.edge.rc[pos]
        )
        end
    )
    local tops =
        func.seqMap({0, 1}, function(i)
            local pos = pos + i
            return pipe.new
                + buildCeil(5)(
                    pos,
                    models.top.central,
                    arcs.blockCoords.stairs.outer.lc[pos],
                    arcs.blockCoords.stairs.outer.rc[pos]
                )
                +
                buildCeil(2.5)(
                    pos,
                    models.top.left,
                    arcs.blockCoords.ceil.edge.lc[pos],
                    arcs.blockCoords.stairs.outer.lc[pos]
                )
                +
                buildCeil(2.5)(
                    pos,
                    models.top.right,
                    arcs.blockCoords.stairs.outer.rc[pos],
                    arcs.blockCoords.ceil.edge.rc[pos]
        )
        end
    )
    return (steps + platforms + ceils + tops) * pipe.flatten()
end

mus.downstairsModels = function(config, arcs, fitModel, pos)
    local tZ = coor.transZ(config.hPlatform - 1.4)-- model height = 1.93 - 1.4 -> 0.53 -> adjust model level to rail level
    local platformZ = config.hPlatform + 0.53 --target Z
    
    local buildPlatform = mus.buildSurface(fitModel, config, platformZ, tZ)
    local buildCeil = mus.buildSurface(fitModel, config, platformZ, coor.I())
    
    local c = arcs.count
    
    local fnSizeSym = function(i, lc, rc) return
        i >= c
        and mus.assembleSize(lc, rc)
        or mus.assembleSize({s = rc.i, i = rc.s}, {s = lc.i, i = lc.s})
    end
    
    local models = {
        platform = {
            left = config.models.platform.left,
            right = config.models.platform.right,
            edgeLeft = config.models.platform.edgeLeft,
            edgeRight = config.models.platform.edgeRight,
            central = config.models.platform.central
        },
        
        stair = {
            central = config.models.downstep.central,
            left = config.models.downstep.left,
            right = config.models.downstep.right,
            back = config.models.downstep.back
        },
        
        ceil = {
            left = config.models.ceil.left,
            right = config.models.ceil.right,
            edge = config.models.ceil.edge,
            central = config.models.ceil.central
        },
        
        top = {
            central = config.models.top.platform.central,
            left = config.models.top.platform.left,
            right = config.models.top.platform.right
        }
    }
    
    local steps = pipe.new +
        buildPlatform(4.5, fnSizeSym)(
            pos,
            models.stair.central,
            arcs.blockCoords.stairs.inner.lc[pos],
            arcs.blockCoords.stairs.inner.rc[pos]
        )
        +
        buildPlatform(4.5, fnSizeSym)(
            pos,
            models.stair.back,
            arcs.blockCoords.stairs.inner.lc[pos],
            arcs.blockCoords.stairs.inner.rc[pos]
        )
        +
        buildPlatform(0.25,
            function(i, loc, lic, roc, ric) return
                i >= c
                and mus.assembleSize(loc, lic)
                or mus.assembleSize({s = roc.i, i = roc.s}, {s = ric.i, i = ric.s})
            end
            )(
            pos,
            models.stair.left,
            arcs.blockCoords.stairs.outer.lc[pos],
            arcs.blockCoords.stairs.inner.lc[pos],
            arcs.blockCoords.stairs.outer.rc[pos],
            arcs.blockCoords.stairs.inner.rc[pos]
        )
        +
        buildPlatform(0.25,
            function(i, loc, lic, roc, ric) return
                i >= c
                and mus.assembleSize(ric, roc)
                or mus.assembleSize({s = lic.i, i = lic.s}, {s = loc.i, i = loc.s})
            end
            )(
            pos,
            models.stair.right,
            arcs.blockCoords.stairs.outer.lc[pos],
            arcs.blockCoords.stairs.inner.lc[pos],
            arcs.blockCoords.stairs.outer.rc[pos],
            arcs.blockCoords.stairs.inner.rc[pos]
    )
    local platforms = pipe.new
        + buildPlatform(1.7)(
            pos,
            models.platform.left,
            arcs.blockCoords.platform.central.lc[pos],
            arcs.blockCoords.stairs.outer.lc[pos]
        )
        + buildPlatform(1.7)(
            pos,
            models.platform.right,
            arcs.blockCoords.stairs.outer.rc[pos],
            arcs.blockCoords.platform.central.rc[pos]
        )
        + buildPlatform(0.8)(
            pos,
            models.platform.edgeLeft,
            arcs.blockCoords.platform.edge.lc[pos],
            arcs.blockCoords.platform.central.lc[pos]
        )
        + buildPlatform(0.8)(
            pos,
            models.platform.edgeRight,
            arcs.blockCoords.platform.central.rc[pos],
            arcs.blockCoords.platform.edge.rc[pos]
    )
    local ceils = pipe.new +
        buildCeil(5, fnSizeSym)(
            pos,
            models.ceil.central,
            arcs.blockCoords.stairs.outer.lc[pos],
            arcs.blockCoords.stairs.outer.rc[pos]
        )
        + buildCeil(1.7)(
            pos,
            models.ceil.left,
            arcs.blockCoords.ceil.central.lc[pos],
            arcs.blockCoords.stairs.outer.lc[pos]
        )
        + buildCeil(1.7)(
            pos,
            models.ceil.right,
            arcs.blockCoords.stairs.outer.rc[pos],
            arcs.blockCoords.ceil.central.rc[pos]
        )
        + buildCeil(0.8)(
            pos,
            models.ceil.edge,
            arcs.blockCoords.ceil.edge.lc[pos],
            arcs.blockCoords.ceil.central.lc[pos]
        )
        + buildCeil(0.8, function(i, lc, rc) return mus.assembleSize({s = rc.i, i = rc.s}, {s = lc.i, i = lc.s}) end)(
            pos,
            models.ceil.edge,
            arcs.blockCoords.ceil.central.rc[pos],
            arcs.blockCoords.ceil.edge.rc[pos]
    )
    local tops = pipe.new +
        buildCeil(5)(
            pos,
            models.top.central,
            arcs.blockCoords.stairs.outer.lc[pos],
            arcs.blockCoords.stairs.outer.rc[pos]
        )
        +
        buildCeil(2.5)(
            pos,
            models.top.left,
            arcs.blockCoords.ceil.edge.lc[pos],
            arcs.blockCoords.stairs.outer.lc[pos]
        )
        +
        buildCeil(2.5)(
            pos,
            models.top.right,
            arcs.blockCoords.stairs.outer.rc[pos],
            arcs.blockCoords.ceil.edge.rc[pos]
    )
    return platforms + ceils + tops + steps
end

mus.platformModels = function(config, arcs, fitModel)
    local tZ = coor.transZ(config.hPlatform - 1.4)-- model height = 1.93 - 1.4 -> 0.53 -> adjust model level to rail level
    local platformZ = config.hPlatform + 0.53 --target Z
    
    local buildPlatform = mus.buildSurface(fitModel, config, platformZ, tZ)
    local buildCeil = mus.buildSurface(fitModel, config, platformZ, coor.I())
    local buildWall = mus.buildSurface(fitModel, config, platformZ, coor.scaleZ(5 - platformZ) * coor.transZ(platformZ))
    
    local c = arcs.count
    local cModels = 2 * c - 2
    local indices = func.seq(1, arcs.blockCount)
    
    local models = {
        platform = {
            left = pipe.rep(cModels)(config.models.platform.left),
            right = pipe.rep(cModels)(config.models.platform.right),
            edgeLeft = pipe.rep(cModels)(config.models.platform.edgeLeft),
            edgeRight = pipe.rep(cModels)(config.models.platform.edgeRight),
            central = pipe.rep(cModels)(config.models.platform.central)
        },
        
        ceil = {
            left = pipe.rep(cModels)(config.models.ceil.left),
            right = pipe.rep(cModels)(config.models.ceil.right),
            edge = pipe.rep(cModels)(config.models.ceil.edge),
            central = pipe.rep(cModels)(config.models.ceil.central)
        },
        
        top = {
            central = pipe.rep(cModels)(config.models.top.platform.central),
            left = pipe.rep(cModels)(config.models.top.platform.left),
            right = pipe.rep(cModels)(config.models.top.platform.right)
        }
    }
    
    local platforms = pipe.new
        + pipe.mapn(
            indices,
            models.platform.central,
            arcs.blockCoords.stairs.outer.lc, arcs.blockCoords.stairs.outer.rc
        )(buildPlatform(5, function(i, lc, rc) return
            i >= c
            and mus.assembleSize(lc, rc)
            or mus.assembleSize({s = rc.i, i = rc.s}, {s = lc.i, i = lc.s})
        end))
        + pipe.mapn(
            indices,
            models.platform.left,
            arcs.blockCoords.platform.central.lc, arcs.blockCoords.stairs.outer.lc
        )(buildPlatform(1.7))
        + pipe.mapn(
            indices,
            models.platform.right,
            arcs.blockCoords.stairs.outer.rc, arcs.blockCoords.platform.central.rc
        )(buildPlatform(1.7))
        + pipe.mapn(
            indices,
            models.platform.edgeLeft,
            arcs.blockCoords.platform.edge.lc, arcs.blockCoords.platform.central.lc
        )(buildPlatform(0.8))
        + pipe.mapn(
            indices,
            models.platform.edgeRight,
            arcs.blockCoords.platform.central.rc, arcs.blockCoords.platform.edge.rc
        )(buildPlatform(0.8))
    
    local ceils =
        pipe.new
        + pipe.mapn(
            indices,
            models.ceil.central,
            arcs.blockCoords.stairs.outer.lc, arcs.blockCoords.stairs.outer.rc
        )(buildCeil(5))
        + pipe.mapn(
            indices,
            models.ceil.left,
            arcs.blockCoords.ceil.central.lc, arcs.blockCoords.stairs.outer.lc
        )(buildCeil(1.8))
        + pipe.mapn(
            indices,
            models.ceil.right,
            arcs.blockCoords.stairs.outer.rc, arcs.blockCoords.ceil.central.rc
        )(buildCeil(1.8))
        + pipe.mapn(
            indices,
            models.ceil.edge,
            arcs.blockCoords.ceil.edge.lc, arcs.blockCoords.ceil.central.lc
        )(buildCeil(0.7))
        + pipe.mapn(
            indices,
            models.ceil.edge,
            arcs.blockCoords.ceil.central.rc, arcs.blockCoords.ceil.edge.rc
        )(buildCeil(0.7), function(i, lc, rc) return mus.assembleSize({s = rc.i, i = rc.s}, {s = lc.i, i = lc.s}) end)
    
    local tops = pipe.new
        + pipe.mapn(
            indices,
            models.top.central,
            arcs.blockCoords.stairs.outer.lc, arcs.blockCoords.stairs.outer.rc
        )(buildCeil(5))
        + pipe.mapn(
            indices,
            models.top.left,
            arcs.blockCoords.ceil.edge.lc, arcs.blockCoords.stairs.outer.lc
        )(buildCeil(2.5))
        + pipe.mapn(
            indices,
            models.top.right,
            arcs.blockCoords.stairs.outer.rc, arcs.blockCoords.ceil.edge.rc
        )(buildCeil(2.5))
    
    local extremity = pipe.mapn(
        {
            {arcs.coords.ceil.edge.lc[1], arcs.coords.ceil.central.lc[1]},
            {arcs.coords.ceil.central.lc[1], arcs.coords.ceil.central.rc[1]},
            {arcs.coords.ceil.central.rc[1], arcs.coords.ceil.edge.rc[1]},
            {arcs.coords.ceil.central.lc[c * 2 - 1], arcs.coords.ceil.edge.lc[c * 2 - 1]},
            {arcs.coords.ceil.central.rc[c * 2 - 1], arcs.coords.ceil.central.lc[c * 2 - 1]},
            {arcs.coords.ceil.edge.rc[c * 2 - 1], arcs.coords.ceil.central.rc[c * 2 - 1]},
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
            {arcs.coords.ceil.edge.lc[1], arcs.coords.ceil.central.lc[1]},
            {arcs.coords.ceil.central.rc[1], arcs.coords.ceil.edge.rc[1]},
            {arcs.coords.ceil.edge.lc[c * 2 - 1], arcs.coords.ceil.central.lc[c * 2 - 1]},
            {arcs.coords.ceil.central.rc[c * 2 - 1], arcs.coords.ceil.edge.rc[c * 2 - 1]},
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

mus.generateTerminals = function(arcs)
    local newLanes = pipe.new
        * pipe.mapn(
            func.seq(1, 2 * arcs.count - 2),
            arcs.blockCoords.platform.lane.lc,
            arcs.blockCoords.platform.lane.rc,
            arcs.blockCoords.platform.lane.mc
        )
        (function(i, lc, rc, mc)
            return {
                l = general.newModel("mus/terminal_lane.mdl", general.mRot(lc.s - lc.i), coor.trans(lc.i)),
                r = general.newModel("mus/terminal_lane.mdl", general.mRot(rc.i - rc.s), coor.trans(rc.s)),
                link = (lc.s:avg(lc.i) - rc.s:avg(rc.i)):length() > 0.5
                and func.with(general.newModel("mus/standard_lane.mdl", general.mRot(lc.s:avg(lc.i) - rc.s:avg(rc.i)), coor.trans(rc.i:avg(rc.s))), { pos = i })
            }
        end)
    return
        func.map(newLanes, pipe.select("l")),
        func.map(newLanes, pipe.select("r")),
        (newLanes * pipe.map(pipe.select("link")) * pipe.filter(pipe.noop())),
        2 * arcs.count - 2
end

return mus
