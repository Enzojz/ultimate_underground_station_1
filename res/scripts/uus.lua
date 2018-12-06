local func = require "uus/func"
local coor = require "uus/coor"
local arc = require "uus/coorarc"
local line = require "uus/coorline"
local quat = require "uus/quaternion"
local station = require "uus/stationlib"
local pipe = require "uus/pipe"
dump = require "luadump"
local uus = {}

local math = math
local pi = math.pi
local abs = math.abs
local ceil = math.ceil
local floor = math.floor
local pow = math.pow
local e = math.exp(1)
local unpack = table.unpack

uus.infi = 1e8

uus.varFn = function(base) return
    {
        function(_) return 1 end,
        function(x) return x end,
        function(x) return x * x end,
        function(x) return pow(x, 4) end,
        function(x) return 1 - pow(e, -x * x * 4.5) end,
        function(x) return pow(e, -pow(6 * x - 3, 2) * 0.5) end,
    }
end


uus.normalizeRad = function(rad)
    return (rad < pi * -0.5) and uus.normalizeRad(rad + pi * 2) or
        ((rad > pi + pi * 0.5) and uus.normalizeRad(rad - pi * 2) or rad)
end


local nSeg = function(length, base)
    local nSeg = ceil((length - base * 0.5) / base)
    return nSeg > 0 and nSeg or 1
end

uus.subDivide = function(size, w, h, nSegH, nSegV)
    local vecT = size.rt - size.lt
    local vecB = size.rb - size.lb
    local vecL = size.lb - size.lt
    local vecR = size.rb - size.rt
    local nSegH = nSegH or nSeg((vecT + vecB):length() * 0.5, w)
    local nSegV = nSegV or nSeg((vecL + vecR):length() * 0.5, h)
    local segLengthT = vecT:length() / nSegH
    local segLengthB = vecB:length() / nSegH
    local vecTN = vecT:normalized()
    local vecBN = vecB:normalized()
    return pipe.new
        * func.seq(1, nSegH)
        * pipe.map(function(h)
            local lt = size.lt + vecTN * (h - 1) * segLengthT
            local rt = lt + vecTN * segLengthT
            local lb = size.lb + vecBN * (h - 1) * segLengthB
            local rb = lb + vecBN * segLengthB
            local vecL = lb - lt
            local vecR = rb - rt
            local segLengthL = vecL:length() / nSegV
            local segLengthR = vecR:length() / nSegV
            local vecLN = vecL:normalized()
            local vecRN = vecR:normalized()
            return pipe.new
                * func.seq(1, nSegV)
                * pipe.map(function(v)
                    return
                        {
                            lt = lt + vecLN * (v - 1) * segLengthL,
                            lb = lt + vecLN * v * segLengthL,
                            rt = rt + vecRN * (v - 1) * segLengthR,
                            rb = rt + vecRN * v * segLengthR
                        }
                end)
        end)
        * pipe.flatten()
end


uus.generateArc = function(arc)
    local sup = arc:pt(arc.sup)
    local inf = arc:pt(arc.inf)
    
    local vecSup = arc:tangent(arc.sup)
    local vecInf = arc:tangent(arc.inf)
    
    return
        {inf, sup, vecInf, vecSup}
end

uus.generateArcExt = function(arc)
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

uus.arcPacker = function(length, slope, r)
    return function(radius, o, lengthVar, dislodge)
        local dislodge = dislodge and (dislodge * length / radius) or 0
        local length = lengthVar and (length * lengthVar) or length
        local initRad = (radius > 0 and pi or 0) + dislodge
        return function(minLength)
            local length = minLength and (length / minLength * length) or length
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
end

uus.mRot = function(vec)
    return coor.scaleX(vec:length()) * quat.byVec(coor.xyz(1, 0, 0), (vec)):mRot()
end

local retriveNSeg = function(length, l, ...)
    return (function(x) return (x < 1 or (x % 1 > 0.5)) and ceil(x) or floor(x) end)(l:length() / length), l, ...
end

local retriveBiLatCoords = function(nSeg, l, ...)
    local rst = pipe.new * {l, ...}
    local lscale = l:length() / (nSeg * length)
    return table.unpack(
        func.map(rst,
            function(s) return abs(lscale) < 1e-5 and pipe.new * {} or pipe.new * func.seqMap({0, nSeg},
                function(n) return s:pt(s.inf + n * ((s.sup - s.inf) / nSeg)) end)
            end)
)
end

local equalizeArcs = function(f, s, ...)
    local arcs = pipe.new * {f, s, ...}
    local ptInf = f:pt(f.inf):avg(s:pt(s.inf))
    local ptSup = f:pt(f.sup):avg(s:pt(s.sup))
    local lnInf = line.byPtPt(arc.ptByPt(f, ptInf), arc.ptByPt(s, ptInf))
    local lnSup = line.byPtPt(arc.ptByPt(f, ptSup), arc.ptByPt(s, ptSup))
    return arcs * pipe.map(function(ar)
        local intInf = ar / lnInf
        local intSup = ar / lnSup
        
        return ar:withLimits({
            inf = ar:rad(((intInf[1] - ptInf):length2() < (intInf[2] - ptInf):length2()) and intInf[1] or intInf[2]),
            sup = ar:rad(((intSup[1] - ptSup):length2() < (intSup[2] - ptSup):length2()) and intSup[1] or intSup[2])
        }
    )
    end)
end

local function ungroup(fst, ...)
    local f = {...}
    return function(lst, ...)
        local l = {...}
        return function(result, c)
            if (fst and lst) then
                return ungroup(table.unpack(f))(table.unpack(l))(
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
        local arcsInf = equalizeArcs(table.unpack(func.map({...}, pipe.select(1))))
        local arcsSup = equalizeArcs(table.unpack(func.map({...}, pipe.select(2))))
        local nSegInf = retriveNSeg(length, table.unpack(arcsInf))
        local nSegSup = retriveNSeg(length, table.unpack(arcsSup))
        if (nSegInf % 2 ~= nSegSup % 2) then
            if (nSegInf > nSegSup) then
                nSegSup = nSegSup + 1
            else
                nSegInf = nSegInf + 1
            end
        end
        return table.unpack(ungroup
            (retriveBiLatCoords(nSegInf, table.unpack(arcsInf)))
            (retriveBiLatCoords(nSegSup, table.unpack(arcsSup)))
            (pipe.new)
    )
    end
end

uus.biLatCoords = biLatCoords

local assembleSize = function(lc, rc)
    return {
        lb = lc.i,
        lt = lc.s,
        rb = rc.i,
        rt = rc.s
    }
end

uus.assembleSize = assembleSize

uus.fitModel2D = function(w, h, _, size, fitTop, fitLeft)
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

uus.fitModel = function(w, h, d, size, fitTop, fitLeft)
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

uus.unitLane = function(f, t) return ((t - f):length2() > 1e-2 and (t - f):length2() < 562500) and station.newModel("uus/person_lane.mdl", uus.mRot(t - f), coor.trans(f)) or nil end

uus.generateEdges = function(edges, isLeft, arcPacker)
    local arcs = arcPacker()()()
    local eInf, eSup = table.unpack(arcs * pipe.map2(isLeft and {pipe.noop(), arc.rev} or {arc.rev, pipe.noop()}, function(a, op) return op(a) end) * pipe.map(uus.generateArc))
    if isLeft then
        eInf[1] = eInf[1]:avg(eSup[2])
        eSup[2] = eInf[1]
        eInf[3] = eInf[3]:avg(eSup[4])
        eSup[4] = eInf[3]
    else
        eInf[2] = eInf[2]:avg(eSup[1])
        eSup[1] = eInf[2]
        eInf[4] = eInf[4]:avg(eSup[3])
        eSup[3] = eInf[4]
    end
    return edges /
        {
            edge = pipe.new / eInf / eSup + arcs * pipe.mapFlatten(uus.generateArcExt) * function(ls) return {ls[2], ls[4]} end,
            snap = pipe.new / {false, false} / {false, false} / {false, true} / {false, true}
        }
end

uus.generateMockEdges = function(config)
    local offsets =
    ({
        [5] = {0},
        [10] = {-2.5, 2.5},
        [15] = {-5, 0, 5}
    })[config.wPlatform]
    return function(mockEdges, arcPacker)
        return mockEdges + func.map(offsets, function(o)
            local arcs = arcPacker()()(o)
            local eInf, eSup = table.unpack(arcs * pipe.map2({pipe.noop(), arc.rev}, function(a, op) return op(a) end) * pipe.map(uus.generateArc))
            eInf[1] = eInf[1]:avg(eSup[2])
            eSup[2] = eInf[1]
            eInf[3] = eInf[3]:avg(eSup[4])
            eSup[4] = eInf[3]
            return {
                edge = pipe.new / eInf / eSup, -- + arcs * pipe.mapFlatten(uus.generateArcExt) * function(ls) return {ls[2], ls[4]} end,
                snap = pipe.new / {false, false} / {false, false}-- / {false, false} / {false, true}
            }
        end)
    end
end

uus.generateTerminals = function(config)
    return function(edges, terminals, terminalsGroup, arcs, enablers)
        local lc, rc, c = arcs.lane.lc, arcs.lane.rc, arcs.lane.c
        local newTerminals = pipe.new
            * pipe.mapn(il(lc), il(rc))(function(lc, rc)
                return {
                    l = station.newModel(enablers[1] and "uus/terminal_lane.mdl" or "uus/standard_lane.mdl", uus.mRot(lc.s - lc.i), coor.trans(lc.i)),
                    r = station.newModel(enablers[2] and "uus/terminal_lane.mdl" or "uus/standard_lane.mdl", uus.mRot(rc.i - rc.s), coor.trans(rc.s)),
                    link = (lc.s:avg(lc.i) - rc.s:avg(rc.i)):length() > 0.5 and station.newModel("uus/standard_lane.mdl", uus.mRot(lc.s:avg(lc.i) - rc.s:avg(rc.i)), coor.trans(rc.i:avg(rc.s)))
                }
            end)
            * function(ls)
                return pipe.new
                    / func.map(ls, pipe.select("l"))
                    / func.map(ls, pipe.select("r"))
                    / (ls * pipe.map(pipe.select("link")) * pipe.filter(pipe.noop()))
            end
        
        local newTerminalGroups = func.map(
            (enablers[1] and enablers[2]) and {
                {
                    terminals = pipe.new * func.seq(1, #newTerminals[1]) * pipe.map(function(s) return {s - 1 + #terminals, 0} end),
                    fVehicleNodeOverride = function(n) return #edges * n - n * 2 end
                },
                {
                    terminals = pipe.new * func.seq(1, #newTerminals[2]) * pipe.map(function(s) return {s - 1 + #terminals + #newTerminals[1], 0} end),
                    fVehicleNodeOverride = function(n) return #edges * n - n + 1 end
                }
            } or enablers[1] and {
                {
                    terminals = pipe.new * func.seq(1, #newTerminals[1]) * pipe.map(function(s) return {s - 1 + #terminals, 0} end),
                    fVehicleNodeOverride = function(n) return #edges * n - n end
                }
            } or enablers[2] and {
                {
                    terminals = pipe.new * func.seq(1, #newTerminals[2]) * pipe.map(function(s) return {s - 1 + #terminals + #newTerminals[1], 0} end),
                    fVehicleNodeOverride = function(n) return #edges * n - n + 1 end
                }
            } or {},
            function(t)
                return {
                    terminals = t.terminals,
                    vehicleNodeOverride = t.fVehicleNodeOverride(8)
                }
            end
        )
        return terminals + newTerminals * pipe.flatten(),
            terminalsGroup
            + newTerminalGroups
    end
end

local buildSurface = function(fitModel, config, platformZ, tZ)
    return function(c, w)
        return function(i, s, lic, ric)
            local lic = i >= c and lic or {s = lic.i, i = lic.s}
            local ric = i >= c and ric or {s = ric.i, i = ric.s}
            
            local sizeS = uus.assembleSize(lic, ric)
            
            local vecs = {
                top = (sizeS.rt - sizeS.lt):normalized(),
                bottom = (sizeS.rb - sizeS.lb):normalized()
            }
            
            return pipe.new
                / station.newModel(s .. "_br.mdl", tZ, fitModel(w, 5, platformZ, sizeS, false, false))
                / station.newModel(s .. "_tl.mdl", tZ, fitModel(w, 5, platformZ, sizeS, true, true))
        end
    end
end

local retriveModels = function(fitModel, config, platformZ, tZ)
    local buildSurface = buildSurface(fitModel, config, platformZ, tZ)
    return function(c, ccl, ccr, w)
        local buildSurface = buildSurface(c, 10 - w * 2)
        return function(i, el, er, s, lc, rc, lic, ric)
            local surface = buildSurface(i, s, lic, ric)
            
            local lce = i >= ccl and lc or {s = lc.i, i = lc.s}
            local rce = i >= ccr and rc or {s = rc.i, i = rc.s}
            local lice = i >= ccl and lic or {s = lic.i, i = lic.s}
            local rice = i >= ccr and ric or {s = ric.i, i = ric.s}
            
            local sizeLe = uus.assembleSize(lce, lice)
            local sizeRe = uus.assembleSize(rice, rce)
            
            return surface
                / station.newModel(el .. "_br.mdl", tZ, fitModel(w, 5, platformZ, sizeLe, false, false))
                / station.newModel(el .. "_tl.mdl", tZ, fitModel(w, 5, platformZ, sizeLe, true, true))
                / station.newModel(er .. "_bl.mdl", tZ, fitModel(w, 5, platformZ, sizeRe, false, true))
                / station.newModel(er .. "_tr.mdl", tZ, fitModel(w, 5, platformZ, sizeRe, true, false))
        end
    end, buildSurface
end

local function buildPoles(config, platformZ, tZ)
    return function(mc, c, f, t)
        local seq = pipe.new
            * pipe.rep(c - 2)(config.models.roofPole)
            / config.models.roofPoleExtreme
            * function(ls) return ls * pipe.rev() + ls end
        
        return pipe.mapn(
            pipe.range(f, t)(pipe.mapi(function(mc, i) return i >= c and coor.I() or coor.flipY() end)(seq)),
            pipe.range(f, t)(il(mc)),
            pipe.range(f, t)(seq)
        )
        (function(t, mc, m)
            local vecPo = mc.s - mc.i
            return station.newModel(m .. ".mdl", tZ, t,
                coor.scaleY(vecPo:length() / 10),
                quat.byVec(coor.xyz(0, 10, 0), vecPo):mRot(),
                coor.trans(mc.i:avg(mc.s)),
                coor.transZ(-platformZ))
        end)
    end
end

local function buildChairs(config, platformZ, tZ)
    return function(lc, rc, mc, c, f, t)
        local platformChairs = pipe.new
            * func.seq(1, c - 1)
            * pipe.map(function(i)
                return c > 3 and i ~= 2 and i % floor(c * 0.5) ~= 2 and i ~= c - 1 and (i % 6 == 4 or (i - 1) % 6 == 4 or (i + 1) % 6 == 4) and
                    (i % 3 ~= 1 and config.models.chair .. ".mdl" or config.models.trash .. ".mdl")
            end)
            * (function(ls) return ls * pipe.rev() + {c < 6 and config.models.chair .. ".mdl"} + ls end)
        local r = pipe.range(f, t)
        return
            pipe.mapn(
                r(lc),
                r(rc),
                r(mc),
                r(platformChairs)
            )
            (function(lc, rc, mc, m)
                return m
                    and {
                        station.newModel(m,
                            quat.byVec(coor.xyz(0, i == 1 and 1 or -1, 0), (rc - lc):withZ(0) .. coor.rotZ(0.5 * pi)):mRot(),
                            coor.trans(mc))
                    }
                    or {}
            end)
    end
end

uus.generateFences = function(fitModel, config)
    local platformZ = config.hPlatform + 0.53
    return function(arcRef, isLeft, filter)
        local filter = filter and filter(isLeft, isTrack) or function(_) return true end
        local isTrack = arcRef.isTrack
        local c = arcRef.isTrack and
            (isLeft and arcRef.roof.edge.lc or arcRef.roof.edge.rc) or
            (isLeft and arcRef.roof.surface.lc or arcRef.roof.surface.rc)
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

uus.generateModels = function(fitModel, config)
    local tZ = coor.transZ(config.hPlatform - 1.4)-- model height = 1.93 - 1.4 -> 0.53 -> adjust model level to rail level
    local platformZ = config.hPlatform + 0.53 --target Z
    
    local retriveCeil, buildCeil = retriveModels(fitModel, config, platformZ, coor.I())
    local retriveModels = retriveModels(fitModel, config, platformZ, tZ)
    
    local platform = function(arcs)
        local lc, rc, lic, ric, c = arcs.platform.lc, arcs.platform.rc, arcs.surface.lc, arcs.surface.rc, arcs.surface.c
        local lpc, rpc, lpic, rpic, pc = arcs.roof.edge.lc, arcs.roof.edge.rc, arcs.roof.surface.lc, arcs.roof.surface.rc, arcs.roof.edge.c
        -- local lpp, rpp, mpp, ppc = arcs.roof.pole.lc, arcs.roof.pole.rc, arcs.roof.pole.mc, arcs.roof.pole.c
        -- local lcc, rcc, mcc, cc = arcs.chair.lc, arcs.chair.rc, arcs.chair.mc, arcs.chair.c
        local platformSurface = pipe.new
            * pipe.rep(c - 1)(config.models.central)
            * (function(ls) return ls * pipe.rev() + ls end)
        
        local platformEdge = pipe.new
            * pipe.rep(c - 1)(config.models.edge)
            * (function(ls) return ls * pipe.rev() + ls end)
        
        local ceilCentral = pipe.new
            * pipe.rep(pc - 1)(config.models.ceilCentral)
            * (function(ls) return ls * pipe.rev() + ls end)
        
        local ceilEdge = pipe.new
            * pipe.rep(pc - 1)(config.models.ceilEdge)
            * (function(ls) return ls * pipe.rev() + ls end)
        
        local ceilTop = pipe.new
            * pipe.rep(pc - 1)(config.models.topPlatform)
            * (function(ls) return ls * pipe.rev() + ls end)
        
        local platforms = pipe.mapn(
            func.seq(1, 2 * c - 2),
            platformEdge,
            platformEdge,
            platformSurface,
            il(lc), il(rc), il(lic), il(ric)
        )(retriveModels(c, c, c, 0.8))
        
        local ceils = pipe.mapn(
            func.seq(1, 2 * pc - 2),
            ceilEdge,
            ceilEdge,
            ceilCentral,
            il(lpc), il(rpc), il(lpic), il(rpic)
        )(retriveCeil(pc, pc, pc, 0.7))
        
        local tops = pipe.mapn(
            func.seq(1, 2 * pc - 2),
            ceilTop,
            il(lpc), il(rpc)
        )(buildCeil(pc, 10))
        
        local extremity = pipe.mapn(
            {
                {lpc[1], lpic[1]},
                {lpic[1], rpic[1]},
                {rpic[1], rpc[1]},
                {lpic[#lpic], lpc[#lpc]},
                {rpic[#rpic], lpic[#lpic]},
                {rpc[#rpc], rpic[#rpic]},
            },
            {
                config.models.wallExtremityEdge .. "_l", config.models.wallExtremity, config.models.wallExtremityEdge .. "_r",
                config.models.wallExtremityEdge .. "_l", config.models.wallExtremity, config.models.wallExtremityEdge .. "_r"
            },
            {
                0.7, 8.6, 0.7,
                0.7, 8.6, 0.7
            }
        )
        (function(c, m, w)
            local lc, rc = unpack(c)
            local vec = rc - lc
            return station.newModel(m .. ".mdl",
                coor.scale(coor.xyz(vec:length() / w, 1, 5 - platformZ)),
                quat.byVec(coor.xyz(1, 0, 0), vec):mRot(),
                coor.trans(lc:avg(rc))
        )
        end)
        
        local extremityPlatform = pipe.mapn(
            {
                {lpc[1], lpic[1]},
                {rpic[1], rpc[1]},
                {lpc[#lpc], lpic[#lpic]},
                {rpic[#rpic], rpc[#rpc]},
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
                station.newModel(config.models.wallExtremityPlatform .. "_" .. p .. ".mdl",
                    coor.transZ(-platformZ),
                    tZ, r,
                    quat.byVec(coor.xyz(1, 0, 0), vec):mRot(),
                    coor.trans(lc:avg(rc))
                ),
                station.newModel(config.models.wallExtremityTop .. "_" .. p .. ".mdl",
                    coor.transZ(-platformZ),
                    r,
                    quat.byVec(coor.xyz(1, 0, 0), vec):mRot(),
                    coor.trans(lc:avg(rc))
            )
            }
        end)
        
        return (pipe.new / platforms / ceils / tops / extremityPlatform) * pipe.flatten() * pipe.flatten() + extremity
    end
    
    local track = function(arcs)
        local lpc, rpc, pc = arcs.roof.edge.lc, arcs.roof.edge.rc, arcs.roof.edge.c
        
        local ceilTop = pipe.new
            * pipe.rep(pc - 1)(config.models.topTrackCenter)
            * (function(ls) return ls * pipe.rev() + ls end)
        
        return pipe.new
            * pipe.mapn(
                func.seq(1, 2 * pc - 2),
                ceilTop,
                il(lpc), il(rpc)
            )(buildCeil(pc, 5))
            * pipe.flatten()
    end
    
    return function(arcs)
        return pipe.new * arcs * pipe.map(function(a) return a.isTrack and track(a) or platform(a) end) * pipe.flatten()
    end
end

uus.generateTerrain = function(config)
    return function(arcs)
        return pipe.new
            / {
                equal = pipe.new
                * pipe.mapn(il(arcs.terrain.lc), il(arcs.terrain.rc))
                (function(lc, rc)
                    local size = assembleSize(lc, rc)
                    return pipe.new / size.lt / size.lb / size.rb / size.rt * station.finalizePoly
                end)
            }
    end
end

uus.generateTrackTerrain = function(config)
    return function(arc)
        local ar = arc()()
        local arl = ar(-0.5 * config.wTrack)
        local arr = ar(0.5 * config.wTrack)
        local lc, rc, c = uus.biLatCoords(5)(arl, arr)
        return pipe.new
            / {
                equal = pipe.new
                * pipe.mapn(il(lc), il(rc))
                (function(lc, rc)
                    local size = assembleSize(lc, rc)
                    return pipe.new / size.lt / size.lb / size.rb / size.rt * station.finalizePoly
                end)
            }
    end
end

local arcGen = function(p, o) return {
    l = p.l(o),
    r = p.r(-o)
} end

local mc = function(lc, rc) return func.map2(lc, rc, function(l, r) return l:avg(r) end) end

uus.allArcs = function(config)
    local refZ = config.hPlatform + 0.53
    
    return pipe.map(function(p)
        if (#p == 3) then
            local arcL, arcR, arcRef = table.unpack(p)
            
            local lane = {
                l = arcL(refZ)(function(l) return l - 3 end),
                r = arcR(refZ)(function(l) return l - 3 end)
            }
            
            local general = {
                l = arcL(refZ)(),
                r = arcR(refZ)()
            }
            
            local terrain = {
                l = arcL()(function(l) return l + 5 end),
                r = arcR()(function(l) return l + 5 end)
            }
            
            local arcs = {
                lane = arcGen(lane, 1),
                laneEdge = arcGen(lane, -0.5),
                edge = arcGen(general, -0.5),
                surface = arcGen(general, 0.3),
                roof = {
                    edge = arcGen(general, -0.5),
                    surface = arcGen(general, 0.2)
                },
                terrain = arcGen(terrain, -0.5)
            }
            
            local lc, rc, lec, rec, c = uus.biLatCoords(5)(arcs.lane.l, arcs.lane.r, arcs.laneEdge.l, arcs.laneEdge.r)
            local lsc, rsc, lsuc, rsuc, ltc, rtc, sc = uus.biLatCoords(5)(arcs.edge.l, arcs.edge.r, arcs.surface.l, arcs.surface.r, arcs.terrain.l, arcs.terrain.r)
            local lcc, rcc, cc = uus.biLatCoords(10)(arcs.edge.l, arcs.edge.r)
            local lpc, rpc, lpic, rpic, pc = uus.biLatCoords(5)(arcs.roof.edge.l, arcs.roof.edge.r, arcs.roof.surface.l, arcs.roof.surface.r)
            local lppc, rppc, ppc = uus.biLatCoords(10)(arcs.roof.edge.l, arcs.roof.edge.r)
            return {
                [1] = arcL,
                [2] = arcR,
                [3] = arcRef,
                lane = func.with(arcs.lane, {lc = lc, rc = rc, mc = mc(lc, rc), c = c}),
                laneEdge = func.with(arcs.laneEdge, {lc = lec, rc = rec, mc = mc(lec, rec), c = c}),
                platform = func.with(arcs.edge, {lc = lsc, rc = rsc, mc = mc(lsc, rsc), c = sc}),
                surface = func.with(arcs.surface, {lc = lsuc, rc = rsuc, mc = mc(lsuc, rsuc), c = sc}),
                chair = func.with(arcs.edge, {lc = lcc, rc = rcc, mc = mc(lcc, rcc), c = cc}),
                roof = {
                    edge = func.with(arcs.roof.edge, {lc = lpc, rc = rpc, mc = mc(lpc, rpc), c = pc}),
                    surface = func.with(arcs.roof.surface, {lc = lpic, rc = rpic, mc = mc(lpic, rpic), c = pc}),
                    pole = func.with(arcs.roof.edge, {lc = lppc, rc = rppc, mc = mc(lppc, rppc), c = ppc})
                },
                terrain = func.with(arcs.terrain, {lc = ltc, rc = rtc, mc = mc(ltc, rtc), c = sc}),
                isPlatform = true
            }
        else
            local arc = p[1]
            
            local general = {
                l = arc(refZ)(),
                r = arc(refZ)()
            }
            
            local arcs = {
                edge = arcGen(general, -config.wTrack * 0.5 + 0.5),
                surface = arcGen(general, -config.wTrack * 0.5 + 1.3),
                roof = {
                    edge = arcGen(general, -config.wTrack * 0.5),
                    surface = arcGen(general, -config.wTrack * 0.5 + 1)
                }
            }
            
            local lsc, rsc, lsuc, rsuc, sc = uus.biLatCoords(5)(arcs.edge.l, arcs.edge.r, arcs.surface.l, arcs.surface.r)
            local lpc, rpc, lpic, rpic, pc = uus.biLatCoords(5)(arcs.roof.edge.l, arcs.roof.edge.r, arcs.roof.surface.l, arcs.roof.surface.r)
            
            return {
                [1] = arc,
                platform = func.with(arcs.edge, {lc = lsc, rc = rsc, mc = mc(lsc, rsc), c = sc}),
                surface = func.with(arcs.surface, {lc = lsuc, rc = rsuc, mc = mc(lsuc, rsuc), c = sc}),
                roof = {
                    edge = func.with(arcs.roof.edge, {lc = lpc, rc = rpc, mc = mc(lpc, rpc), c = pc}),
                    surface = func.with(arcs.roof.surface, {lc = lpic, rc = rpic, mc = mc(lpic, rpic), c = pc}),
                },
                isTrack = true
            }
        end
    end)
end

uus.build = function(config, fitModel, generateEdges)
    local generateEdges = uus.generateEdges
    local generateMockEdges = uus.generateMockEdges(config)
    local generateModels = uus.generateModels(fitModel, config)
    local generateTerminals = uus.generateTerminals(config)
    local generateTerrain = uus.generateTerrain(config)
    local generateTrackTerrain = uus.generateTrackTerrain(config)
    local generateFences = uus.generateFences(fitModel, config)
    
    local function build(edges, mockEdges, terminals, terminalsGroup, models, terrain, gr, ...)
        local isLeftmost = #models == 0
        local isRightmost = #{...} == 0
        
        if (gr == nil) then
            return edges, mockEdges, terminals, terminalsGroup,
                models * pipe.filter(pipe.noop()),
                terrain
        elseif (#gr == 3 and gr[1].isTrack and gr[2].isPlatform and gr[3].isTrack) then
            local edges = generateEdges(edges, true, gr[1][1])
            local edges = generateEdges(edges, false, gr[3][1])
            local mockEdges = generateMockEdges(mockEdges, gr[2][3])
            local terminals, terminalsGroup = generateTerminals(edges, terminals, terminalsGroup, gr[2], {true, true})
            return build(
                edges,
                mockEdges,
                terminals,
                terminalsGroup,
                models + generateModels(gr)
                + (isLeftmost and generateFences(gr[1], true) or {})
                + (isRightmost and generateFences(gr[3], false) or {}),
                terrain + generateTerrain(gr[2]) + generateTrackTerrain(gr[1][1]) + generateTrackTerrain(gr[3][1]),
                ...)
        elseif (#gr == 2 and gr[1].isTrack and gr[2].isPlatform) then
            local edges = generateEdges(edges, true, gr[1][1])
            local mockEdges = generateMockEdges(mockEdges, gr[2][3])
            local terminals, terminalsGroup = generateTerminals(edges, terminals, terminalsGroup, gr[2], {true, false})
            return build(
                edges,
                mockEdges,
                terminals,
                terminalsGroup,
                models + generateModels(gr)
                + (isLeftmost and generateFences(gr[1], true) or {})
                + (isRightmost and generateFences(gr[2], false) or {}),
                terrain + generateTerrain(gr[2]) + generateTrackTerrain(gr[1][1]),
                ...)
        elseif (#gr == 2 and gr[1].isPlatform and gr[2].isTrack) then
            local edges = generateEdges(edges, false, gr[2][1])
            local mockEdges = generateMockEdges(mockEdges, gr[1][3])
            local terminals, terminalsGroup = generateTerminals(edges, terminals, terminalsGroup, gr[1], {false, true})
            return build(
                edges,
                mockEdges,
                terminals,
                terminalsGroup,
                models + generateModels(gr)
                + (isLeftmost and generateFences(gr[1], true) or {})
                + (isRightmost and generateFences(gr[2], false) or {}),
                terrain + generateTerrain(gr[1]) + generateTrackTerrain(gr[2][1]),
                ...)
        elseif (#gr == 1 and gr[1].isPlatform) then
            local terminals, terminalsGroup = generateTerminals(edges, terminals, terminalsGroup, gr[1], {false, false})
            local mockEdges = generateMockEdges(mockEdges, gr[1][3])
            return build(
                edges,
                mockEdges,
                terminals,
                terminalsGroup,
                models + generateModels(gr)
                + (isLeftmost and generateFences(gr[1], true) or {})
                + (isRightmost and generateFences(gr[1], false) or {}),
                terrain + generateTerrain(gr[1]),
                ...)
        else
            local edges = generateEdges(edges, false, gr[1][1])
            return build(
                edges,
                mockEdges,
                terminals,
                terminalsGroup,
                models + generateModels(gr)
                + (isLeftmost and generateFences(gr[1], true) or {})
                + (isRightmost and generateFences(gr[1], false) or {}),
                terrain + generateTrackTerrain(gr[1][1]),
                ...)
        end
    end
    return build
end

local function trackGrouping(result, ar1, ar2, ar3, ar4, ...)
    if (ar1 == nil) then return unpack(result) end
    
    if (ar1 and ar2 and ar3) then
        if ar1.isTrack and ar2.isPlatform and ar3.isTrack then
            if (ar4 and ar4.isPlatform and #{...} == 0) then
                return trackGrouping(result / {ar1, ar2} / {ar3, ar4}, ...)
            else
                return trackGrouping(result / {ar1, ar2, ar3}, ar4, ...)
            end
        elseif ar1.isPlatform and ar2.isTrack and ar3.isPlatform and not ar4 then
            return trackGrouping(result / {ar1} / {ar2, ar3}, ar4, ...)
        elseif ar1.isTrack and ar2.isPlatform and ar3.isPlatform and ar4 and ar4.isTrack then
            return trackGrouping(result / {ar1, ar2, ar3, ar4}, ...)
        elseif ar1.isPlatform and ar2.isPlatform and ar3.isTrack and ar4 and ar4.isPlatform then
            return trackGrouping(result / {ar1, ar2} / {ar3, ar4}, ...)
        elseif ar1.isPlatform and ar2.isPlatform and ar3.isTrack then
            return trackGrouping(result / {ar1, ar2, ar3}, ar4, ...)
        end
    end
    
    if (ar1 and ar2) then
        if ((ar1.isTrack and ar2.isPlatform) or (ar2.isTrack and ar1.isPlatform)) then
            return trackGrouping(result / {ar1, ar2}, ar3, ar4, ...)
        end
    end
    
    return trackGrouping(result / {ar1}, ar2, ar3, ar4, ...)
end

uus.trackGrouping = trackGrouping

uus.models = function(prefixM)
    local prefixM = function(p) return prefixM .. p end
    return {
        central = prefixM("platform/platform_central"),
        edge = prefixM("platform/platform_edge"),
        ceilCentral = prefixM("platform/ceil_central"),
        ceilEdge = prefixM("platform/ceil_edge"),
        topPlatform = prefixM("platform/top_platform"),
        topTrackCenter = prefixM("platform/top_center"),
        topTrackLeft = prefixM("platform/top_left"),
        topTrackRight = prefixM("platform/top_right"),
        wallTrack = prefixM("platform/wall_track"),
        wallPlatform = prefixM("platform/wall_platform"),
        wallExtremity = prefixM("platform/wall_extremity"),
        wallExtremityEdge = prefixM("platform/wall_extremity_edge"),
        wallExtremityPlatform = prefixM("platform/wall_extremity_platform"),
        wallExtremityTop = prefixM("platform/wall_extremity_top"),
        trash = prefixM("platform/platform_trash"),
        chair = prefixM("platform/platform_chair"),
        underground = prefixM("underground_entry.mdl")
    }
end

uus.defaultParams = function(params)
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

uus.safeBuild = function(params, updateFn)
    local defaultParams = uus.defaultParams(params)
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

uus.preBuild = function(totalTracks, nbTransitTracks, posTransitTracks, ignoreFst, ignoreLst)
    local function preBuild(nbTracks, result)
        local p = false
        local t = true
        local transitSeq = pipe.new * pipe.rep(nbTransitTracks)(t)
        if (nbTracks == 0) then
            local result = ignoreLst and result or (result[#result] and (result / p) or result)
            if (#transitSeq > 0) then
                if (posTransitTracks == 1) then
                    result = result + transitSeq
                elseif (posTransitTracks == -2) then
                    result = transitSeq + result
                elseif (posTransitTracks == 0) then
                    result = pipe.new * pipe.rep(ceil(nbTransitTracks * 0.5))(t) + result + pipe.new * pipe.rep(floor(nbTransitTracks * 0.5))(t)
                else
                    local idx = result * pipe.zip(func.seq(1, #result), {"t", "i"}) * pipe.filter(function(p) return not p.t end) * pipe.map(pipe.select("i"))
                    result = result * pipe.range(1, idx[ceil(#idx * 0.5)]) + transitSeq + result * pipe.range(idx[ceil(#idx * 0.5)] + 1, #result)
                end
            end
            return result
        elseif (nbTracks == totalTracks and ignoreFst) then
            return preBuild(nbTracks - 1, result / t / p)
        elseif (nbTracks == totalTracks and not ignoreFst) then
            return preBuild(nbTracks - 1, result / p / t)
        elseif (nbTracks == 1 and ignoreLst) then
            return preBuild(nbTracks - 1, ((not result) or result[#result]) and (result / p / t) or (result / t))
        elseif (nbTracks == 1 and not ignoreLst) then
            return preBuild(nbTracks - 1, result / t / p)
        else
            return preBuild(nbTracks - 2, result / t / p / t)
        end
    end
    return preBuild
end

return uus
