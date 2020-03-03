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
        local baseL0, baseR0, c = mus.biLatCoords(5)(arcRef()()(-platformWidth * 0.5), arcRef()()(platformWidth * 0.5))
        
        local coords = {
            platform = {
                edge = {lc = {}, rc = {}, mc = {}, c = c},
                central = {lc = {}, rc = {}, mc = {}, c = c},
                lane = {lc = {}, rc = {}, mc = {}, c = c}
            },
            ceil = {
                edge = {lc = {}, rc = {}, mc = {}, c = c},
                central = {lc = {}, rc = {}, mc = {}, c = c},
                outer = {lc = {}, rc = {}, mc = {}, c = c},
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
            
            offset(-(platformWidth - stairsWidth) * 0.5 - 0.3, coords.stairs.outer)
            offset(-(platformWidth - stairsWidth) * 0.5 - 0.55, coords.stairs.inner)
        end
        
        for i = 1, (c * 2 - 1) do
            local ptL = baseL0[i] .. config.transf.pt
            local ptR = baseR0[i] .. config.transf.pt
            
            local transL = (ptL - ptR):normalized()
            local function offset(o, ls)
                ls.lc[i] = ptL + transL * o
                ls.rc[i] = ptR - transL * o
                ls.mc[i] = (ptL + ptR) * 0.5
            end
            
            offset(0.5, coords.ceil.edge)
            offset(-0.2, coords.ceil.central)
            offset(-(platformWidth - stairsWidth) * 0.5 - 0.3, coords.ceil.outer)
            
            offset(2, coords.terrain.low)
            
            coords.terrain.high.lc[i] = coords.terrain.low.lc[i] + coor.xyz(0, 0, 9)
            coords.terrain.high.rc[i] = coords.terrain.low.rc[i] + coor.xyz(0, 0, 9)
            coords.terrain.high.mc[i] = coords.terrain.low.mc[i] + coor.xyz(0, 0, 9)
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
                outer = interlaceCoords(coords.ceil.outer),
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
    
    return pipe.mapn(
        func.seq(1, arcRef.blockCount),
        arcRef.blockCoords.stairs.inner.lc,
        arcRef.blockCoords.stairs.inner.rc
        )(
        function(i, lc, rc)
            local size = isLeft and mus.assembleSize(lc, rc) or mus.assembleSize({s = rc.i, i = rc.s}, {s = lc.i, i = lc.s})
            return func.with(general.newModel(config.models.wallPlatform .. ".mdl", coor.rotZ(pi), coor.transY(-2.5), config.fitModels.platform.wall(size, true, 5 - config.refZ)), {pos = i, wall = true})
    end)
end


mus.upstairsModels = function(config, arcs, pos, isBackward)
    local buildPlatform = config.build.platform
    local buildCeil = config.build.ceil
    local buildWall = config.build.wall
    
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
        / buildWall(config.fitModels.step.central, isBackward and function(i, lc, rc) return mus.assembleSize({s = rc.i, i = rc.s}, {s = lc.i, i = lc.s}) end or nil)(
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
        )(buildPlatform(config.fitModels.step.central, isBackward and function(i, lc, rc) return mus.assembleSize({s = rc.i, i = rc.s}, {s = lc.i, i = lc.s}) end) or nil)
        +
        pipe.mapn(
            {pos, pos + 1},
            models.stair.inner,
            {arcs.blockCoords.stairs.inner.lc[pos0], arcs.blockCoords.stairs.inner.lc[pos1]},
            {arcs.blockCoords.stairs.inner.rc[pos0], arcs.blockCoords.stairs.inner.rc[pos1]}
        )(buildPlatform(config.fitModels.step.central, isBackward and function(i, lc, rc) return mus.assembleSize({s = rc.i, i = rc.s}, {s = lc.i, i = lc.s}) end) or nil)
        +
        pipe.mapn(
            {pos, pos + 1},
            models.stair.left,
            {arcs.blockCoords.stairs.outer.lc[pos0], arcs.blockCoords.stairs.outer.lc[pos1]},
            {arcs.blockCoords.stairs.inner.lc[pos0], arcs.blockCoords.stairs.inner.lc[pos1]},
            {arcs.blockCoords.stairs.outer.rc[pos0], arcs.blockCoords.stairs.outer.rc[pos1]},
            {arcs.blockCoords.stairs.inner.rc[pos0], arcs.blockCoords.stairs.inner.rc[pos1]}
        )(buildWall(config.fitModels.step.wall, function(i, loc, lic, roc, ric) return isBackward and mus.assembleSize({s = roc.i, i = roc.s}, {s = ric.i, i = ric.s}) or mus.assembleSize(loc, lic) end))
        +
        pipe.mapn(
            {pos, pos + 1},
            models.stair.right,
            {arcs.blockCoords.stairs.outer.lc[pos0], arcs.blockCoords.stairs.outer.lc[pos1]},
            {arcs.blockCoords.stairs.inner.lc[pos0], arcs.blockCoords.stairs.inner.lc[pos1]},
            {arcs.blockCoords.stairs.outer.rc[pos0], arcs.blockCoords.stairs.outer.rc[pos1]},
            {arcs.blockCoords.stairs.inner.rc[pos0], arcs.blockCoords.stairs.inner.rc[pos1]}
        )(buildWall(config.fitModels.step.wall, function(i, loc, lic, roc, ric) return isBackward and mus.assembleSize({s = lic.i, i = lic.s}, {s = loc.i, i = loc.s}) or mus.assembleSize(ric, roc) end))
    
    local platforms = func.map({pos, pos + 1},
        function(pos)
            return pipe.new
                + buildPlatform(config.fitModels.platform.side)(
                    pos,
                    models.platform.left,
                    arcs.blockCoords.platform.central.lc[pos],
                    arcs.blockCoords.stairs.outer.lc[pos]
                )
                + buildPlatform(config.fitModels.platform.side)(
                    pos,
                    models.platform.right,
                    arcs.blockCoords.stairs.outer.rc[pos],
                    arcs.blockCoords.platform.central.rc[pos]
                )
                + buildPlatform(config.fitModels.platform.edge)(
                    pos,
                    models.platform.edgeLeft,
                    arcs.blockCoords.platform.edge.lc[pos],
                    arcs.blockCoords.platform.central.lc[pos]
                )
                + buildPlatform(config.fitModels.platform.edge)(
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
                + buildCeil(config.fitModels.ceil.side)(
                    pos,
                    left,
                    arcs.blockCoords.ceil.central.lc[pos],
                    arcs.blockCoords.ceil.outer.lc[pos]
                )
                + buildCeil(config.fitModels.ceil.side)(
                    pos,
                    right,
                    arcs.blockCoords.ceil.outer.rc[pos],
                    arcs.blockCoords.ceil.central.rc[pos]
                )
                + buildCeil(config.fitModels.ceil.edge)(
                    pos,
                    edge,
                    arcs.blockCoords.ceil.edge.lc[pos],
                    arcs.blockCoords.ceil.central.lc[pos]
                )
                + buildCeil(config.fitModels.ceil.edge, function(i, lc, rc) return mus.assembleSize({s = rc.i, i = rc.s}, {s = lc.i, i = lc.s}) end)(
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
                + buildCeil(config.fitModels.top.central)(
                    pos,
                    models.top.central,
                    arcs.blockCoords.ceil.outer.lc[pos],
                    arcs.blockCoords.ceil.outer.rc[pos]
                )
                +
                buildCeil(config.fitModels.top.side)(
                    pos,
                    models.top.left,
                    arcs.blockCoords.ceil.edge.lc[pos],
                    arcs.blockCoords.ceil.outer.lc[pos]
                )
                +
                buildCeil(config.fitModels.top.side)(
                    pos,
                    models.top.right,
                    arcs.blockCoords.ceil.outer.rc[pos],
                    arcs.blockCoords.ceil.edge.rc[pos]
        )
        end
    )
    return config.isFinalized
        and (steps + platforms + ceils + tops) * pipe.flatten()
        or (steps + platforms) * pipe.flatten()
end

mus.downstairsModels = function(config, arcs, pos, isBackward)
    local buildPlatform = config.build.platform
    local buildCeil = config.build.ceil
    
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
        buildPlatform(config.fitModels.step.central, isBackward and function(i, lc, rc) return mus.assembleSize({s = rc.i, i = rc.s}, {s = lc.i, i = lc.s}) end or nil)(
            pos,
            models.stair.central,
            arcs.blockCoords.stairs.inner.lc[pos],
            arcs.blockCoords.stairs.inner.rc[pos]
        )
        +
        buildPlatform(config.fitModels.step.central, isBackward and function(i, lc, rc) return mus.assembleSize({s = rc.i, i = rc.s}, {s = lc.i, i = lc.s}) end or nil)(
            pos,
            models.stair.back,
            arcs.blockCoords.stairs.inner.lc[pos],
            arcs.blockCoords.stairs.inner.rc[pos]
        )
        +
        buildPlatform(config.fitModels.step.wall,
            function(i, loc, lic, roc, ric) return isBackward and mus.assembleSize({s = roc.i, i = roc.s}, {s = ric.i, i = ric.s}) or mus.assembleSize(loc, lic) end)(
            pos,
            models.stair.left,
            arcs.blockCoords.stairs.outer.lc[pos],
            arcs.blockCoords.stairs.inner.lc[pos],
            arcs.blockCoords.stairs.outer.rc[pos],
            arcs.blockCoords.stairs.inner.rc[pos]
        )
        +
        buildPlatform(config.fitModels.step.wall,
            function(i, loc, lic, roc, ric) return isBackward and mus.assembleSize({s = lic.i, i = lic.s}, {s = loc.i, i = loc.s}) or mus.assembleSize(ric, roc) end)(
            pos,
            models.stair.right,
            arcs.blockCoords.stairs.outer.lc[pos],
            arcs.blockCoords.stairs.inner.lc[pos],
            arcs.blockCoords.stairs.outer.rc[pos],
            arcs.blockCoords.stairs.inner.rc[pos]
    )
    local platforms = pipe.new
        + buildPlatform(config.fitModels.platform.side)(
            pos,
            models.platform.left,
            arcs.blockCoords.platform.central.lc[pos],
            arcs.blockCoords.stairs.outer.lc[pos]
        )
        + buildPlatform(config.fitModels.platform.side)(
            pos,
            models.platform.right,
            arcs.blockCoords.stairs.outer.rc[pos],
            arcs.blockCoords.platform.central.rc[pos]
        )
        + buildPlatform(config.fitModels.platform.edge)(
            pos,
            models.platform.edgeLeft,
            arcs.blockCoords.platform.edge.lc[pos],
            arcs.blockCoords.platform.central.lc[pos]
        )
        + buildPlatform(config.fitModels.platform.edge)(
            pos,
            models.platform.edgeRight,
            arcs.blockCoords.platform.central.rc[pos],
            arcs.blockCoords.platform.edge.rc[pos]
    )
    local ceils = pipe.new +
        buildCeil(config.fitModels.ceil.central)(
            pos,
            models.ceil.central,
            arcs.blockCoords.ceil.outer.lc[pos],
            arcs.blockCoords.ceil.outer.rc[pos]
        )
        + buildCeil(config.fitModels.ceil.side)(
            pos,
            models.ceil.left,
            arcs.blockCoords.ceil.central.lc[pos],
            arcs.blockCoords.ceil.outer.lc[pos]
        )
        + buildCeil(config.fitModels.ceil.side)(
            pos,
            models.ceil.right,
            arcs.blockCoords.ceil.outer.rc[pos],
            arcs.blockCoords.ceil.central.rc[pos]
        )
        + buildCeil(config.fitModels.ceil.edge)(
            pos,
            models.ceil.edge,
            arcs.blockCoords.ceil.edge.lc[pos],
            arcs.blockCoords.ceil.central.lc[pos]
        )
        + buildCeil(config.fitModels.ceil.edge, function(i, lc, rc) return mus.assembleSize({s = rc.i, i = rc.s}, {s = lc.i, i = lc.s}) end)(
            pos,
            models.ceil.edge,
            arcs.blockCoords.ceil.central.rc[pos],
            arcs.blockCoords.ceil.edge.rc[pos]
    )
    local tops = pipe.new +
        buildCeil(config.fitModels.top.central)(
            pos,
            models.top.central,
            arcs.blockCoords.ceil.outer.lc[pos],
            arcs.blockCoords.ceil.outer.rc[pos]
        )
        +
        buildCeil(config.fitModels.top.side)(
            pos,
            models.top.left,
            arcs.blockCoords.ceil.edge.lc[pos],
            arcs.blockCoords.ceil.outer.lc[pos]
        )
        +
        buildCeil(config.fitModels.top.side)(
            pos,
            models.top.right,
            arcs.blockCoords.ceil.outer.rc[pos],
            arcs.blockCoords.ceil.edge.rc[pos]
    )
    return config.isFinalized
        and platforms + ceils + tops + steps
        or platforms + steps
end

mus.platformModels = function(config, arcs)
    local buildPlatform = config.build.platform
    local buildCeil = config.build.ceil
    
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
        )(buildPlatform(config.fitModels.platform.central))
        + pipe.mapn(
            indices,
            models.platform.left,
            arcs.blockCoords.platform.central.lc, arcs.blockCoords.stairs.outer.lc
        )(buildPlatform(config.fitModels.platform.side))
        + pipe.mapn(
            indices,
            models.platform.right,
            arcs.blockCoords.stairs.outer.rc, arcs.blockCoords.platform.central.rc
        )(buildPlatform(config.fitModels.platform.side))
        + pipe.mapn(
            indices,
            models.platform.edgeLeft,
            arcs.blockCoords.platform.edge.lc, arcs.blockCoords.platform.central.lc
        )(buildPlatform(config.fitModels.platform.edge))
        + pipe.mapn(
            indices,
            models.platform.edgeRight,
            arcs.blockCoords.platform.central.rc, arcs.blockCoords.platform.edge.rc
        )(buildPlatform(config.fitModels.platform.edge))
    
    local ceils =
        pipe.new
        + pipe.mapn(
            indices,
            models.ceil.central,
            arcs.blockCoords.ceil.outer.lc,
            arcs.blockCoords.ceil.outer.rc
        )(buildCeil(config.fitModels.ceil.central))
        + pipe.mapn(
            indices,
            models.ceil.left,
            arcs.blockCoords.ceil.central.lc,
            arcs.blockCoords.ceil.outer.lc
        )(buildCeil(config.fitModels.ceil.side))
        + pipe.mapn(
            indices,
            models.ceil.right,
            arcs.blockCoords.ceil.outer.rc,
            arcs.blockCoords.ceil.central.rc
        )(buildCeil(config.fitModels.ceil.side))
        + pipe.mapn(
            indices,
            models.ceil.edge,
            arcs.blockCoords.ceil.edge.lc, arcs.blockCoords.ceil.central.lc
        )(buildCeil(config.fitModels.ceil.edge))
        + pipe.mapn(
            indices,
            models.ceil.edge,
            arcs.blockCoords.ceil.central.rc, arcs.blockCoords.ceil.edge.rc
        )(buildCeil(config.fitModels.ceil.edge), function(i, lc, rc) return mus.assembleSize({s = rc.i, i = rc.s}, {s = lc.i, i = lc.s}) end)
    
    local tops = pipe.new
        + pipe.mapn(
            indices,
            models.top.central,
            arcs.blockCoords.ceil.outer.lc,
            arcs.blockCoords.ceil.outer.rc
        )(buildCeil(config.fitModels.top.central))
        + pipe.mapn(
            indices,
            models.top.left,
            arcs.blockCoords.ceil.edge.lc,
            arcs.blockCoords.ceil.outer.lc
        )(buildCeil(config.fitModels.top.side))
        + pipe.mapn(
            indices,
            models.top.right,
            arcs.blockCoords.ceil.outer.rc,
            arcs.blockCoords.ceil.edge.rc
        )(buildCeil(config.fitModels.top.side))
    
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
        local vec = (rc - lc):withZ(0)
        return general.newModel(m .. ".mdl",
            coor.scale(coor.xyz(vec:length() / w, 1, 5 - config.refZ)),
            quat.byVec(coor.xyz(1, 0, 0), vec):mRot(),
            coor.trans(lc:avg(rc) + coor.xyz(0, 0, config.refZ))
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
                coor.transZ(-1.93), r,
                quat.byVec(coor.xyz(1, 0, 0), vec):mRot(),
                coor.trans(lc:avg(rc) + coor.xyz(0, 0, config.refZ))
            ),
            general.newModel(config.models.wallExtremityTop .. "_" .. p .. ".mdl",
                r,
                quat.byVec(coor.xyz(1, 0, 0), vec):mRot(),
                coor.trans(lc:avg(rc))
        )
        }
    end)
    
    return config.isFinalized
        and (pipe.new + platforms + ceils + tops + extremityPlatform) * pipe.flatten() + extremity
        or (pipe.new + platforms + extremityPlatform) * pipe.flatten() + extremity
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

mus.platformSigns = function(config, arcs, posx, isLeftmost, isRightmost)
    local transZ = coor.xyz(0, 0, -config.refZ + 4)
    
    local indices = func.seq(1, 2 * arcs.count - 2)
    
    local indicesN = (function()
        local n = floor(#indices / 6)
        local length = #indices / n
        return
            pipe.new
            * func.seq(1, n)
            * pipe.map(function(i) return indices[1] + length * (i - 0.5) end)
            * pipe.map(function(p) return p < arcs.count and floor(p) or ceil(p) end)
    end)()
    
    local indicesP = (function()
        local n = floor(#indices / 3)
        local length = #indices / n
        return
            pipe.new
            * func.seq(1, n)
            * pipe.map(function(i) return indices[1] + length * (i - 0.5) end)
            * pipe.map(function(p) return p < arcs.count and floor(p) or ceil(p) end)
    end)()
    
    local fn = function()
        return pipe.mapn(
            indices,
            arcs.blockCoords.platform.central.mc,
            arcs.blockCoords.stairs.inner.lc,
            arcs.blockCoords.stairs.inner.rc,
            arcs.blockCoords.platform.lane.lc,
            arcs.blockCoords.platform.lane.rc
        )
        (function(i, mc, lw, rw, ll, rl)
            if (func.contains(indicesN, i)) then
                local transL = quat.byVec(coor.xyz(-1, 0, 0), (lw.i - lw.s):withZ(0)):mRot() * coor.trans((i < arcs.count and lw.s or lw.i) + transZ)
                local transR = quat.byVec(coor.xyz(1, 0, 0), (rw.i - rw.s):withZ(0)):mRot() * coor.trans((i < arcs.count and rw.s or rw.i) + transZ)
                local transM = quat.byVec(coor.xyz(1, 0, 0), (mc.i - mc.s):withZ(0)):mRot() * coor.trans((i < arcs.count and mc.s or mc.i) + transZ)
                return
                    pipe.new
                    / (isLeftmost and func.with(general.newModel("mus/signs/platform_signs_2.mdl", transL), {pos = i}) or nil)
                    / (isRightmost and func.with(general.newModel("mus/signs/platform_signs_2.mdl", transR), {pos = i}) or nil)
                    / (not (isRightmost or isLeftmost) and func.with(general.newModel("mus/signs/platform_signs_2_arm.mdl", transM), {pos = i}) or nil)
            elseif (func.contains(indicesP, i)) then
                local transL = quat.byVec(coor.xyz(-1, 0, 0), (ll.i - ll.s):withZ(0)):mRot() * coor.trans((i < arcs.count and ll.s or ll.i) + transZ)
                local transR = quat.byVec(coor.xyz(1, 0, 0), (rl.i - rl.s):withZ(0)):mRot() * coor.trans((i < arcs.count and rl.s or rl.i) + transZ)
                return
                    pipe.new
                    / ((not isLeftmost) and func.with(general.newModel("mus/signs/platform_signs_nr.mdl", coor.rotZ(pi * 0.5) * transL), {pos = i, posx = posx, isNrLeft = true}) or nil)
                    / ((not isRightmost) and func.with(general.newModel("mus/signs/platform_signs_nr.mdl", coor.rotZ(pi * 0.5) * transR), {pos = i, posx = posx, isNrRight = true}) or nil)
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
    
    return pipe.new
        * pipe.mapn(
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
        * pipe.filter(pipe.noop())
        * pipe.flatten()
end

return mus
