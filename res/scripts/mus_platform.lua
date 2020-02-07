local func = require "entry/func"
local coor = require "entry/coor"
local quat = require "entry/quaternion"
local pipe = require "entry/pipe"
local general = require "entry/general"

local mus = require "mus"

local math = math
local pi = math.pi
local ceil = math.ceil
local floor = math.floor
local unpack = table.unpack

mus.platformArcs = function(platformWidth, stairsWidth)
    return function(config, arcRef)
        local baseL, baseR, c = mus.biLatCoords(5)(arcRef(config.refZ)()(-platformWidth * 0.5), arcRef(config.refZ)()(platformWidth * 0.5))
        
        local coords = {
            platform = {
                edge = {lc = {}, rc = {}, mc = {}, c = c},
                central = {lc = {}, rc = {}, mc = {}, c = c},
                lane = {lc = {}, rc = {}, mc = {}, c = c}
            },
            ceil = {
                edge = {lc = {}, rc = {}, mc = {}, c = c},
                central = {lc = {}, rc = {}, mc = {}, c = c},
            },
            stairs = {
                outer = {lc = {}, rc = {}, mc = {}, c = c},
                inner = {lc = {}, rc = {}, mc = {}, c = c},
            },
            terrain = {
                low = {lc = {}, rc = {}, mc = {}, c = c},
                high = {lc = {}, rc = {}, mc = {}, c = c}
            }
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
            
            offset(0.5, coords.platform.edge)
            offset(-0.3, coords.platform.central)
            offset(-0.6, coords.platform.lane)
            
            offset(0.5, coords.ceil.edge)
            offset(-0.2, coords.ceil.central)
            
            offset(-(platformWidth - stairsWidth) * 0.5 - 0.3, coords.stairs.outer)
            offset(-(platformWidth - stairsWidth) * 0.5 - 0.55, coords.stairs.inner)
            
            offset(2, coords.terrain.low)
            
            coords.terrain.high.lc[i] = coords.terrain.low.lc[i] + coor.xyz(0, 0, 8)
            coords.terrain.high.rc[i] = coords.terrain.low.rc[i] + coor.xyz(0, 0, 8)
            coords.terrain.high.mc[i] = coords.terrain.low.mc[i] + coor.xyz(0, 0, 8)
        end
        
        local function interlaceCoords(coords)
            return {
                lc = mus.interlace(coords.lc),
                rc = mus.interlace(coords.rc),
                mc = mus.interlace(coords.mc),
                count = c * 2 - 2
            }
        end
        
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
            terrain = {
                low = interlaceCoords(coords.terrain.low),
                high = interlaceCoords(coords.terrain.high)
            }
        }
        
        return {
            ref = arcRef,
            count = c,
            blockCount = c * 2 - 2,
            coords = coords,
            blockCoords = blockCoords,
            isPlatform = true
        }
    
    end
end

mus.platformSideWallModels = function(config, arcRef, isLeft)
    local newModels =
        pipe.new * (isLeft and arcRef.blockCoords.stairs.inner.lc or arcRef.blockCoords.stairs.inner.rc)
        * pipe.mapi(function(ic, i)
            local vec = ic.i - ic.s
            return func.with(
                general.newModel(config.models.wallPlatform .. ".mdl",
                    coor.rotZ(isLeft and -0.5 * pi or 0.5 * pi),
                    coor.scaleZ(5 - config.refZ),
                    coor.scaleX(vec:length() / 5),
                    quat.byVec(coor.xyz(5, 0, 0), vec):mRot(),
                    coor.trans(ic.s:avg(ic.i))),
                {pos = i, wall = true}
        )
        end)
    return newModels
end


mus.upstairsModels = function(config, arcs, pos, isBackward)
    local tZ = coor.transZ(config.hPlatform - 1.4)-- model height = 1.93 - 1.4 -> 0.53 -> adjust model level to rail level
    
    local buildPlatform = mus.buildSurface(config, tZ)
    local buildCeil = mus.buildSurface(config, coor.I())
    local buildWall = mus.buildSurface(config, coor.scaleZ(5 - config.refZ) * coor.transZ(config.refZ))
    
    local c = arcs.count
    
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
    
    local pos0 = isBackward and pos + 1 or pos
    local pos1 = isBackward and pos or pos + 1
    
    local steps = pipe.new
        / buildWall(4.5, isBackward and function(i, lc, rc) return mus.assembleSize({s = rc.i, i = rc.s}, {s = lc.i, i = lc.s}) end or nil)(
            pos,
            models.stair.back,
            arcs.blockCoords.stairs.inner.lc[pos1],
            arcs.blockCoords.stairs.inner.rc[pos1]
        )
        +
        pipe.mapn(
            {pos, pos + 1},
            models.stair.central,
            {arcs.blockCoords.stairs.inner.lc[pos0], arcs.blockCoords.stairs.inner.lc[pos1]},
            {arcs.blockCoords.stairs.inner.rc[pos0], arcs.blockCoords.stairs.inner.rc[pos1]}
        )(buildPlatform(4.5, isBackward and function(i, lc, rc) return mus.assembleSize({s = rc.i, i = rc.s}, {s = lc.i, i = lc.s}) end) or nil)
        +
        pipe.mapn(
            {pos, pos + 1},
            models.stair.inner,
            {arcs.blockCoords.stairs.inner.lc[pos0], arcs.blockCoords.stairs.inner.lc[pos1]},
            {arcs.blockCoords.stairs.inner.rc[pos0], arcs.blockCoords.stairs.inner.rc[pos1]}
        )(buildPlatform(4.5, isBackward and function(i, lc, rc) return mus.assembleSize({s = rc.i, i = rc.s}, {s = lc.i, i = lc.s}) end) or nil)
        +
        pipe.mapn(
            {pos, pos + 1},
            models.stair.left,
            {arcs.blockCoords.stairs.outer.lc[pos0], arcs.blockCoords.stairs.outer.lc[pos1]},
            {arcs.blockCoords.stairs.inner.lc[pos0], arcs.blockCoords.stairs.inner.lc[pos1]},
            {arcs.blockCoords.stairs.outer.rc[pos0], arcs.blockCoords.stairs.outer.rc[pos1]},
            {arcs.blockCoords.stairs.inner.rc[pos0], arcs.blockCoords.stairs.inner.rc[pos1]}
        )(buildWall(0.25, function(i, loc, lic, roc, ric) return isBackward and mus.assembleSize({s = roc.i, i = roc.s}, {s = ric.i, i = ric.s}) or mus.assembleSize(loc, lic) end))
        +
        pipe.mapn(
            {pos, pos + 1},
            models.stair.right,
            {arcs.blockCoords.stairs.outer.lc[pos0], arcs.blockCoords.stairs.outer.lc[pos1]},
            {arcs.blockCoords.stairs.inner.lc[pos0], arcs.blockCoords.stairs.inner.lc[pos1]},
            {arcs.blockCoords.stairs.outer.rc[pos0], arcs.blockCoords.stairs.outer.rc[pos1]},
            {arcs.blockCoords.stairs.inner.rc[pos0], arcs.blockCoords.stairs.inner.rc[pos1]}
        )(buildWall(0.25, function(i, loc, lic, roc, ric) return isBackward and mus.assembleSize({s = lic.i, i = lic.s}, {s = loc.i, i = loc.s}) or mus.assembleSize(ric, roc) end))
    
    local platforms = func.map({pos, pos + 1},
        function(pos)
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
            {pos, pos + 1},
            models.ceil.left,
            models.ceil.right,
            models.ceil.edge
        )(function(pos, left, right, edge)
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
        func.map({pos, pos + 1}, function(pos)
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
    return config.isFinalized 
        and (steps + platforms + ceils + tops) * pipe.flatten()
        or (steps + platforms + tops) * pipe.flatten()
end

mus.downstairsModels = function(config, arcs, pos, isBackward)
    local tZ = coor.transZ(config.hPlatform - 1.4)-- model height = 1.93 - 1.4 -> 0.53 -> adjust model level to rail level
    
    local buildPlatform = mus.buildSurface(config, tZ)
    local buildCeil = mus.buildSurface(config, coor.I())
    
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
        buildPlatform(4.5, isBackward and function(i, lc, rc) return mus.assembleSize({s = rc.i, i = rc.s}, {s = lc.i, i = lc.s}) end or nil)(
            pos,
            models.stair.central,
            arcs.blockCoords.stairs.inner.lc[pos],
            arcs.blockCoords.stairs.inner.rc[pos]
        )
        +
        buildPlatform(4.5, isBackward and function(i, lc, rc) return mus.assembleSize({s = rc.i, i = rc.s}, {s = lc.i, i = lc.s}) end or nil)(
            pos,
            models.stair.back,
            arcs.blockCoords.stairs.inner.lc[pos],
            arcs.blockCoords.stairs.inner.rc[pos]
        )
        +
        buildPlatform(0.25,
            function(i, loc, lic, roc, ric) return isBackward and mus.assembleSize({s = roc.i, i = roc.s}, {s = ric.i, i = ric.s}) or mus.assembleSize(loc, lic) end)(
            pos,
            models.stair.left,
            arcs.blockCoords.stairs.outer.lc[pos],
            arcs.blockCoords.stairs.inner.lc[pos],
            arcs.blockCoords.stairs.outer.rc[pos],
            arcs.blockCoords.stairs.inner.rc[pos]
        )
        +
        buildPlatform(0.25,
            function(i, loc, lic, roc, ric) return isBackward and mus.assembleSize({s = lic.i, i = lic.s}, {s = loc.i, i = loc.s}) or mus.assembleSize(ric, roc) end)(
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
        buildCeil(5)(
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
    return config.isFinalized
        and platforms + ceils + tops + steps
        or platforms + steps + tops
end

mus.platformModels = function(config, arcs)
    local tZ = coor.transZ(config.hPlatform - 1.4)-- model height = 1.93 - 1.4 -> 0.53 -> adjust model level to rail level
    
    local buildPlatform = mus.buildSurface(config, tZ)
    local buildCeil = mus.buildSurface(config, coor.I())
    
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
        )(buildPlatform(5))
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
            coor.scale(coor.xyz(vec:length() / w, 1, 5 - config.refZ)),
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
                coor.transZ(-config.refZ),
                tZ, r,
                quat.byVec(coor.xyz(1, 0, 0), vec):mRot(),
                coor.trans(lc:avg(rc))
            ),
            general.newModel(config.models.wallExtremityTop .. "_" .. p .. ".mdl",
                coor.transZ(-config.refZ),
                r,
                quat.byVec(coor.xyz(1, 0, 0), vec):mRot(),
                coor.trans(lc:avg(rc))
        )
        }
    end)
    
    return config.isFinalized
        and (pipe.new + platforms + ceils + tops + extremityPlatform) * pipe.flatten() + extremity
        or (pipe.new + platforms + tops + extremityPlatform) * pipe.flatten() + extremity
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
                and func.with(general.newModel("mus/standard_lane.mdl", general.mRot(lc.s:avg(lc.i) - rc.s:avg(rc.i)), coor.trans(rc.i:avg(rc.s))), {pos = i})
            }
        end)
    return
        func.map(newLanes, pipe.select("l")),
        func.map(newLanes, pipe.select("r")),
        (newLanes * pipe.map(pipe.select("link")) * pipe.filter(pipe.noop())),
        2 * arcs.count - 2
end

mus.platformSigns = function(config, arcs, isLeftmost, isRightmost)
    local transZ = coor.xyz(0, 0, -config.refZ + 4)
    local cModels = 2 * arcs.count - 2
    
    local indices = func.seq(1, cModels)
    
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
            arcs.blockCoords.platform.central.mc,
            arcs.blockCoords.stairs.inner.lc,
            arcs.blockCoords.stairs.inner.rc
        )
        (function(i, mc, lw, rw)
            if (func.contains(indicesN, i)) then
                local transL = quat.byVec(coor.xyz(-1, 0, 0), lw.i - lw.s):mRot() * coor.trans((i < arcs.count and lw.s or lw.i) + transZ)
                local transR = quat.byVec(coor.xyz(1, 0, 0), rw.i - rw.s):mRot() * coor.trans((i < arcs.count and rw.s or rw.i) + transZ)
                local transM = quat.byVec(coor.xyz(1, 0, 0), mc.i - mc.s):mRot() * coor.trans((i < arcs.count and mc.s or mc.i) + transZ)
                return
                    pipe.new
                    / (isLeftmost and func.with(general.newModel("mus/signs/platform_signs_2.mdl", transL), {pos = i}) or nil)
                    / (isRightmost and func.with(general.newModel("mus/signs/platform_signs_2.mdl", transR), {pos = i}) or nil)
                    / (not (isRightmost or isLeftmost) and func.with(general.newModel("mus/signs/platform_signs_2_arm.mdl", transM), {pos = i}) or nil)
            else
                return false
            end
        end)
    end
    
    return pipe.new * fn()
        * pipe.filter(pipe.noop())
        * pipe.flatten()

end

mus.platformChairs = function(config, arcs, isLeftmost, isRightmost)
    local cModels = 2 * arcs.count - 2
    
    local indices = func.seq(1, cModels)
    
    local indicesN = pipe.new * indices
        * pipe.fold({pipe.new}, function(r, i) return i and func.with(r, {[#r] = r[#r] / i}) or func.with(r, {[#r + 1] = pipe.new}) end)
        * pipe.filter(function(g) return #g > 4 end)
        * pipe.map(
            function(g)
                local n = floor(#g / 4)
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
            arcs.blockCoords.stairs.inner.lc,
            arcs.blockCoords.stairs.inner.rc,
            arcs.blockCoords.platform.central.mc
        )
        (function(i, lc, rc, mc, lw, rw)
            if (indicesN * pipe.contains(i)) then
                local newModel = function(...) return func.with(general.newModel(...), {pos = i, chair = true}) end
                local transL = quat.byVec(coor.xyz(-1, 0, 0), lc.i - lc.s):mRot()
                local transR = quat.byVec(coor.xyz(1, 0, 0), rc.i - rc.s):mRot()
                local transM = quat.byVec(coor.xyz(1, 0, 0), mc.i - mc.s):mRot()
                return
                    pipe.new
                    / (isLeftmost and newModel(config.models.chair .. ".mdl", coor.rotZ(0.5 * pi), transL, coor.trans(lc.s)) or nil)
                    / (isLeftmost and newModel(config.models.chair .. ".mdl", coor.rotZ(0.5 * pi), transL, coor.trans(lc.i)) or nil)
                    / (isRightmost and newModel(config.models.chair .. ".mdl", coor.rotZ(0.5 * pi), transR, coor.trans(rc.s)) or nil)
                    / (isRightmost and newModel(config.models.chair .. ".mdl", coor.rotZ(0.5 * pi), transR, coor.trans(rc.i)) or nil)
                    / (not (isRightmost or isLeftmost) and newModel(config.models.chair .. ".mdl", coor.rotZ(0.5 * pi), transM, coor.trans(mc.s)) or nil)
                    / (not (isRightmost or isLeftmost) and newModel(config.models.chair .. ".mdl", coor.rotZ(0.5 * pi), transM, coor.trans(mc.i)) or nil)
            else
                return false
            end
        end)
    end
    
    return pipe.new * fn()
        * pipe.filter(pipe.noop())
        * pipe.flatten()
end

return mus
