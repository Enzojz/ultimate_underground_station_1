local func = require "entry/func"
local coor = require "entry/coor"
local arc = require "mus/coorarc"
local line = require "mus/coorline"
local quat = require "entry/quaternion"
local station = require "mus/stationlib"
local pipe = require "entry/pipe"
local dump = require "luadump"

local hasLivetext, livetext = xpcall(
    require,
    function(e)
        print "Ultimate Underground Station: Livetext not found."
    end,
    "livetext"
)
local mus = {}

local math = math
local pi = math.pi
local abs = math.abs
local ceil = math.ceil
local floor = math.floor
local pow = math.pow
local min = math.min
local e = math.exp(1)
local unpack = table.unpack

mus.normalizeRad = function(rad)
    return (rad < pi * -0.5) and mus.normalizeRad(rad + pi * 2) or
        ((rad > pi + pi * 0.5) and mus.normalizeRad(rad - pi * 2) or rad)
end

mus.generateArc = function(arc)
    local sup = arc:pt(arc.sup)
    local inf = arc:pt(arc.inf)
    
    local vecSup = arc:tangent(arc.sup)
    local vecInf = arc:tangent(arc.inf)
    
    return
        {inf, sup, vecInf, vecSup}
end

mus.generateArcExt = function(arc)
    local extArc = arc:extendLimits(5)
    
    local sup = arc:pt(arc.sup)
    local inf = arc:pt(arc.inf)
    
    local vecSup = arc:tangent(arc.sup)
    local vecInf = arc:tangent(arc.inf)
    
    local supExt = arc:pt(extArc.sup)
    local infExt = arc:pt(extArc.inf)
    
    local vecSupExt = arc:tangent(extArc.sup)
    local vecInfExt = arc:tangent(extArc.inf)
    
    return {
        {infExt, inf, vecInfExt, vecInf},
        {sup, supExt, vecSup, vecSupExt},
    }
end

mus.arcPacker = function(length, slope, r)
    return function(radius, o)
        local initRad = (radius > 0 and pi or 0)
        return function(z)
            local z = z or 0
            return function(lengthOverride)
                local l = lengthOverride and lengthOverride(length) or length
                return function(xDr)
                    local dr = xDr or 0
                    local ar = arc.byOR(o + coor.xyz(0, 0, z), abs(radius - dr))
                    local rad = l / r * 0.5
                    return pipe.new
                        / ar:withLimits({
                            sup = initRad - rad,
                            inf = initRad,
                            slope = -slope
                        })
                        / ar:withLimits({
                            inf = initRad,
                            sup = initRad + rad,
                            slope = slope
                        })
                end
            end
        end
    end
end

mus.mRot = function(vec)
    return coor.scaleX(vec:length()) * quat.byVec(coor.xyz(1, 0, 0), (vec)):mRot()
end

local retriveNSeg = function(length, l, ...)
    return (function(x) return (x < 1 or (x % 1 > 0.5)) and ceil(x) or floor(x) end)(l:length() / length), l, ...
end

local retriveBiLatCoords = function(nSeg, l, ...)
    local rst = pipe.new * {l, ...}
    local lscale = l:length() / (nSeg * station.segmentLength)
    return unpack(
        func.map(rst,
            function(s) return abs(lscale) < 1e-5 and pipe.new * {} or pipe.new * func.seqMap({0, nSeg},
                function(n) return s:pt(s.inf + n * ((s.sup - s.inf) / nSeg)) end)
            end)
)
end

local function ungroup(fst, ...)
    local f = {...}
    return function(lst, ...)
        local l = {...}
        return function(result, c)
            if (fst and lst) then
                return ungroup(unpack(f))(unpack(l))(
                    result /
                    (
                    (fst[1] - lst[1]):length2() < (fst[1] - lst[#lst]):length2()
                    and (fst * pipe.range(2, #fst) * pipe.rev() + {fst[1]:avg(lst[1])} + lst * pipe.range(2, #lst))
                    or (fst * pipe.range(2, #fst) * pipe.rev() + {fst[1]:avg(lst[#lst])} + lst * pipe.rev() * pipe.range(2, #lst))
                    ),
                    floor((#fst + #lst) * 0.5)
            )
            else
                return result / c
            end
        end
    end
end

local biLatCoords = function(length)
    return function(...)
        local arcs = pipe.new * {...}
        local arcsInf = func.map({...}, pipe.select(1))
        local arcsSup = func.map({...}, pipe.select(2))
        local nSegInf = retriveNSeg(length, unpack(arcsInf))
        local nSegSup = retriveNSeg(length, unpack(arcsSup))
        if (nSegInf % 2 ~= nSegSup % 2) then
            if (nSegInf > nSegSup) then
                nSegSup = nSegSup + 1
            else
                nSegInf = nSegInf + 1
            end
        end
        return unpack(ungroup
            (retriveBiLatCoords(nSegInf, unpack(arcsInf)))
            (retriveBiLatCoords(nSegSup, unpack(arcsSup)))
            (pipe.new)
    )
    end
end

mus.biLatCoords = biLatCoords

local assembleSize = function(lc, rc)
    return {
        lb = lc.i,
        lt = lc.s,
        rb = rc.i,
        rt = rc.s
    }
end

mus.assembleSize = assembleSize

mus.fitModel2D = function(w, h, _, size, fitTop, fitLeft)
    local s = {
        coor.xyz(0, 0),
        coor.xyz(fitLeft and w or -w, 0),
        coor.xyz(0, fitTop and -h or h),
    }
    
    local t = fitTop and
        {
            fitLeft and size.lt or size.rt,
            fitLeft and size.rt or size.lt,
            fitLeft and size.lb or size.rb,
        } or {
            fitLeft and size.lb or size.rb,
            fitLeft and size.rb or size.lb,
            fitLeft and size.lt or size.rt,
        }
    
    local mX = {
        {s[1].x, s[1].y, 1},
        {s[2].x, s[2].y, 1},
        {s[3].x, s[3].y, 1},
    }
    
    local mU = {
        t[1].x, t[1].y, 1,
        t[2].x, t[2].y, 1,
        t[3].x, t[3].y, 1,
    }
    
    local dX = coor.det(mX)
    
    local miX = coor.minor(mX)
    local mXI = func.mapFlatten(func.seq(1, 3),
        function(l)
            return func.seqMap({1, 3}, function(c)
                return ((l + c) % 2 == 0 and 1 or -1) * coor.det(miX(c, l)) / dX
            end)
        end)
    
    local function mul(m1, m2)
        local m = function(line, col)
            local l = (line - 1) * 3
            return m1[l + 1] * m2[col + 0] + m1[l + 2] * m2[col + 3] + m1[l + 3] * m2[col + 6]
        end
        return {
            m(1, 1), m(1, 2), m(1, 3),
            m(2, 1), m(2, 2), m(2, 3),
            m(3, 1), m(3, 2), m(3, 3),
        }
    end
    
    local mXi = mul(mXI, mU)
    
    return coor.I() * {
        mXi[1], mXi[2], 0, mXi[3],
        mXi[4], mXi[5], 0, mXi[6],
        0, 0, 1, 0,
        mXi[7], mXi[8], 0, mXi[9]
    }
end

mus.fitModel = function(w, h, d, size, fitTop, fitLeft)
    local s = {
        coor.xyz(0, 0, d),
        coor.xyz(fitLeft and w or -w, 0, d),
        coor.xyz(0, fitTop and -h or h, d),
        coor.xyz(0, 0, 0)
    }
    
    local t = fitTop and
        {
            fitLeft and size.lt or size.rt,
            fitLeft and size.rt or size.lt,
            fitLeft and size.lb or size.rb,
        } or {
            fitLeft and size.lb or size.rb,
            fitLeft and size.rb or size.lb,
            fitLeft and size.lt or size.rt,
        }
    
    local mX = {
        {s[1].x, s[1].y, s[1].z, 1},
        {s[2].x, s[2].y, s[2].z, 1},
        {s[3].x, s[3].y, s[3].z, 1},
        {s[4].x, s[4].y, s[4].z, 1}
    }
    
    local mU = {
        t[1].x, t[1].y, t[1].z, 1,
        t[2].x, t[2].y, t[2].z, 1,
        t[3].x, t[3].y, t[3].z, 1,
        t[1].x, t[1].y, t[1].z - d, 1
    }
    
    local dX = coor.det(mX)
    
    local miX = coor.minor(mX)
    local mXI = func.mapFlatten(func.seq(1, 4),
        function(l)
            return func.seqMap({1, 4}, function(c)
                return ((l + c) % 2 == 0 and 1 or -1) * coor.det(miX(c, l)) / dX
            end)
        end)
    
    return coor.I() * mXI * mU
end

local il = pipe.interlace({"s", "i"})

mus.unitLane = function(f, t) return ((t - f):length2() > 1e-2 and (t - f):length2() < 562500) and station.newModel("mus/person_lane.mdl", mus.mRot(t - f), coor.trans(f)) or nil end

mus.generateSideWalls = function(fitModel, config)
    local platformZ = config.hPlatform + 0.53
    return function(arcRef, isLeft, filter)
        local filter = filter and filter(isLeft, isTrack) or function(_) return true end
        local isTrack = arcRef.isTrack
        local c = arcRef.isTrack and
            (isLeft and arcRef.ceil.lc or arcRef.ceil.rc) or
            (isLeft and arcRef.stairs.inner.lc or arcRef.stairs.inner.rc)
        local newModels =
            pipe.new * il(c)
            * pipe.filter(filter)
            * pipe.map(function(ic)
                local vec = ic.i - ic.s
                return station.newModel((isTrack and config.models.wallTrack or config.models.wallPlatform) .. ".mdl",
                    coor.rotZ(isLeft and -0.5 * pi or 0.5 * pi),
                    isTrack and coor.I() or coor.scaleZ(5 - platformZ),
                    coor.scaleX(vec:length() / 5),
                    quat.byVec(coor.xyz(5, 0, 0), vec):mRot(),
                    coor.trans(ic.s:avg(ic.i) + (isTrack and coor.xyz(0, 0, -platformZ) or coor.xyz(0, 0, 0))))
            end)
        return newModels
    end
end

mus.generateTerrain = function(config)
    return pipe.mapFlatten(function(arcs)
        return pipe.new
            / {
                greater = pipe.new
                * pipe.mapn(il(arcs.terrain.lc), il(arcs.terrain.rc))
                (function(lc, rc)
                    local size = assembleSize(lc, rc)
                    return pipe.new / size.lt / size.lb / size.rb / size.rt * station.finalizePoly
                end)
            }
    end)
end

local arcGen = function(p, o) return {
    l = p.l(o),
    r = p.r(-o)
} end

local mc = function(lc, rc) return func.map2(lc, rc, function(l, r) return l:avg(r) end) end

mus.allArcs = function(config)
    local refZ = config.hPlatform + 0.53
    
    return pipe.map(function(p)
        if (#p == 3) then
            local arcL, arcR, arcRef = unpack(p)
            local general = {
                l = arcL(refZ)(),
                r = arcR(refZ)()
            }
            
            local arcs = {
                platform = {
                    lane = arcGen(general, 0.6),
                    edge = arcGen(general, -0.5),
                    central = arcGen(general, 0.3)
                },
                ceil = {
                    edge = arcGen(general, -0.5),
                    central = arcGen(general, 0.2),
                },
                stairs = {
                    outer = arcGen(general, (config.wPlatform - config.wStairs) * 0.5 + 0.3),
                    inner = arcGen(general, (config.wPlatform - config.wStairs) * 0.5 + 0.55)
                },
                terrain = arcGen({
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
                    edge = func.with(arcs.platform.edge, {lc = lsc, rc = rsc, mc = mc(lsc, rsc), c = c}),
                    central = func.with(arcs.platform.central, {lc = lsuc, rc = rsuc, mc = mc(lsuc, rsuc), c = c}),
                    lane = func.with(arcs.platform.lane, {lc = lc, rc = rc, mc = mc(lc, rc), c = c})
                },
                ceil = {
                    edge = func.with(arcs.ceil.edge, {lc = lpc, rc = rpc, mc = mc(lpc, rpc), c = c}),
                    central = func.with(arcs.ceil.central, {lc = lpic, rc = rpic, mc = mc(lpic, rpic), c = c}),
                },
                stairs = {
                    outer = func.with(arcs.stairs.outer, {lc = lsoc, rc = rsoc, mc = mc(lsoc, rsoc), c = c}),
                    inner = func.with(arcs.stairs.inner, {lc = lsic, rc = rsic, mc = mc(lsic, rsic), c = c}),
                },
                terrain = func.with(arcs.terrain, {lc = tlc, rc = trc, mc = mc(tlc, trc), c = tc}),
                isPlatform = true
            }
        else
            local arc = p[1]
            
            local ceil = arcGen(
                {
                    l = arc(refZ)(),
                    r = arc(refZ)()
                },
                -config.wTrack * 0.5)
            
            local terrain = arcGen(
                {
                    l = arc(refZ + 7.75)(function(l) return l + 5 end),
                    r = arc(refZ + 7.75)(function(l) return l + 5 end)
                },
                -config.wTrack * 0.5)
            
            local lpc, rpc, c = mus.biLatCoords(5)(ceil.l, ceil.r)
            local ltc, rtc, tc = mus.biLatCoords(5)(terrain.l, terrain.r)
            
            return {
                [1] = arc,
                count = c,
                ceil = func.with(ceil, {lc = lpc, rc = rpc, mc = mc(lpc, rpc), c = c}),
                terrain = func.with(terrain, {lc = ltc, rc = rtc, mc = mc(ltc, rtc), c = tc}),
                isTrack = true
            }
        end
    end)
end

mus.models = function(set)
    local c = "mus/ceil/"
    local t = "mus/top/"
    local p = "mus/platform/" .. set.platform .. "/"
    local w = "mus/wall/" .. set.wall .. "/"
    return {
        platform = {
            edgeLeft = p .. "platform_edge_left",
            edgeRight = p .. "platform_edge_right",
            central = p .. "platform_central",
            left = p .. "platform_left",
            right = p .. "platform_right",
        },
        upstep = {
            a = p .. "platform_upstep_a",
            b = p .. "platform_upstep_b",
            aLeft = w .. "platform_upstep_a_left",
            aRight = w .. "platform_upstep_a_right",
            aInner = w .. "platform_upstep_a_inner",
            bLeft = w .. "platform_upstep_b_left",
            bRight = w .. "platform_upstep_b_right",
            bInner = w .. "platform_upstep_b_inner",
            back = w .. "platform_upstep_back"
        },
        downstep = {
            right = w .. "platform_downstep_left",
            left = w .. "platform_downstep_right",
            central = p .. "platform_downstep",
            back = w .. "platform_downstep_back"
        },
        ceil = {
            edge = c .. "ceil_edge",
            central = c .. "ceil_central",
            left = c .. "ceil_left",
            right = c .. "ceil_right",
            aLeft = c .. "ceil_upstep_a_left",
            aRight = c .. "ceil_upstep_a_right",
            bLeft = c .. "ceil_upstep_b_left",
            bRight = c .. "ceil_upstep_b_right",
        },
        top = {
            track = {
                left = t .. "top_track_left",
                right = t .. "top_track_right",
                central = t .. "top_track_central"
            },
            platform = {
                left = t .. "top_platform_left",
                right = t .. "top_platform_right",
                central = t .. "top_platform_central"
            },
        },
        wallTrack = w .. "wall_track",
        wallPlatform = w .. "wall_platform",
        wallExtremity = w .. "wall_extremity",
        wallExtremityEdge = w .. "wall_extremity_edge",
        wallExtremityPlatform = w .. "wall_extremity_platform",
        wallExtremityTop = w .. "wall_extremity_top",
        chair = p .. "platform_chair"
    }
end

mus.defaultParams = function(params)
    local defParams = params()
    return function(param)
        local function limiter(d, u)
            return function(v) return v and v < u and v or d end
        end
        param.trackType = param.trackType or 0
        param.catenary = param.catenary or 0
        
        func.forEach(
            func.filter(defParams, function(p) return p.key ~= "tramTrack" end),
            function(i)param[i.key] = limiter(i.defaultIndex or 0, #i.values)(param[i.key]) end)
        return param
    end
end

mus.safeBuild = function(params, updateFn)
    local defaultParams = mus.defaultParams(params)
    local paramsOnFail = params() *
        pipe.mapPair(function(i) return i.key, i.defaultIndex or 0 end)
    
    return function(param)
        local r, result = xpcall(
            updateFn,
            function(e)
                print("========================")
                print("Ultimate Station failure")
                print("Algorithm failure:", debug.traceback())
                print("Params:")
                func.forEach(
                    params() * pipe.filter(function(i) return param[i.key] ~= (i.defaultIndex or 0) end),
                    function(i)print(i.key .. ": " .. param[i.key]) end)
                print("End of Ultimate Station failure")
                print("========================")
            end,
            defaultParams(param)
        )
        return r and result or updateFn(defaultParams(paramsOnFail))
    -- return updateFn(defaultParams(param))
    end
end

return mus
