local func = require "uus/func"
local coor = require "uus/coor"
local arc = require "uus/coorarc"
local line = require "uus/coorline"
local quat = require "uus/quaternion"
local station = require "uus/stationlib"
local pipe = require "uus/pipe"
local livetext = require "livetext"
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

local stepsLanePos = function(config)
    return function(c)
        local c = c - 1
        return
            pipe.new
            + (config.hasDown and {{pos = floor(c * 0.25) + 3, vec = -1, z = -3}, {pos = ceil(c * 0.75) - 2, vec = 1, z = -3}} or {})
            + (config.hasUp and {{pos = floor(c * 0.25) + 1, vec = -2, z = 6}, {pos = ceil(c * 0.75), vec = 2, z = 6}} or {})
    end
end

local isStepsPos = function(config)
    return function(c)
        local c = c + 1
        return function(i)
            return
                config.hasDown and (i == floor(c * 0.25) + 2 or i == ceil(c * 0.75) - 2),
                config.hasUp and (i == floor(c * 0.25) or i == ceil(c * 0.75)),
                config.hasUp and (i == floor(c * 0.25) - 1 or i == ceil(c * 0.75) + 1)
        end
    end
end

uus.generateTerminals = function(config)
    local isStepsPos = isStepsPos(config)
    local stepsLanePos = stepsLanePos(config)
    return function(edges, terminals, terminalsGroup, lanePos, arcs, enablers)
        local isStepsPos = isStepsPos(2 * arcs.count - 2)
        
        local newLanePos = func.map(
            stepsLanePos(2 * arcs.count - 1),
            function(i) return {f = arcs.platform.lane.mc[i.pos], t = arcs.platform.lane.mc[i.pos + i.vec] + coor.xyz(0, 0, i.z)} end
        )
        local newTerminals = pipe.new
            * pipe.mapn(
                func.seq(1, 2 * arcs.count - 2),
                il(arcs.platform.lane.lc),
                il(arcs.platform.lane.rc),
                il(arcs.platform.lane.mc)
            )
            (function(i, lc, rc, mc)
                local isStep = func.fold({isStepsPos(i)}, false, function(r, v) return v or r end)
                return {
                    l = station.newModel(enablers[1] and "uus/terminal_lane.mdl" or "uus/standard_lane.mdl", uus.mRot(lc.s - lc.i), coor.trans(lc.i)),
                    r = station.newModel(enablers[2] and "uus/terminal_lane.mdl" or "uus/standard_lane.mdl", uus.mRot(rc.i - rc.s), coor.trans(rc.s)),
                    c = not isStep and station.newModel("uus/standard_lane.mdl", uus.mRot(mc.i - mc.s), coor.trans(mc.s)),
                    link = not isStep and (lc.s:avg(lc.i) - rc.s:avg(rc.i)):length() > 0.5
                    and station.newModel("uus/standard_lane.mdl", uus.mRot(lc.s:avg(lc.i) - rc.s:avg(rc.i)), coor.trans(rc.i:avg(rc.s)))
                }
            end)
            * function(ls)
                return pipe.new
                    / func.map(ls, pipe.select("l"))
                    / func.map(ls, pipe.select("r"))
                    / (ls * pipe.map(pipe.select("c")) * pipe.filter(pipe.noop()))
                    / (ls * pipe.map(pipe.select("link")) * pipe.filter(pipe.noop()))
                    / func.map(newLanePos, function(l) return station.newModel("uus/standard_lane.mdl", uus.mRot(l.t - l.f), coor.trans(l.f)) end)
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
            + newTerminalGroups,
            lanePos / func.map(newLanePos, pipe.select("t"))
    end
end

local buildSurface = function(fitModel, config, platformZ, tZ)
    return function(c, w, fnSize)
        local fnSize = fnSize or function(_, lc, rc) return uus.assembleSize(lc, rc) end
        return function(i, s, ...)
            local sizeS = fnSize(i, ...)
            
            return s
                and pipe.new
                / station.newModel(s .. "_tl.mdl", tZ, fitModel(w, 5, platformZ, sizeS, true, true))
                / station.newModel(s .. "_br.mdl", tZ, fitModel(w, 5, platformZ, sizeS, false, false))
                or pipe.new * {}
        end
    end
end

uus.generateSideWalls = function(fitModel, config)
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

local placeSign = function(name, transBoard, hasPole)
    local signColor = "C00257E"
    local decoColor = "CF2F2F2"
    local textColor = "CFFFFFF_emissive"
    
    local font = "alte_din_1451_mittelschrift"
    local livetext = livetext(font, nil, textColor)
    local nameModelsF, width = table.unpack({livetext(0.35)(name or "?")} or {})
    local height = 0.5
    local thickness = 0.1
    
    return
        pipe.new
        / station.newModel("uus/1990/signs/platform_signs.mdl",
            coor.scale(coor.xyz(width + 1, thickness, height)),
            coor.trans(coor.xyz(0, 0, 0.25)),
            transBoard
        )
        / station.newModel("uus/1990/signs/platform_signs_left.mdl",
            coor.scale(coor.xyz(1, thickness, (height + 0.04) / 1.04)),
            coor.trans(coor.xyz(-width * 0.5 - 0.5, 0, 0.25)),
            transBoard
        )
        / station.newModel("uus/1990/signs/platform_signs_right.mdl",
            coor.scale(coor.xyz(1, thickness, (height + 0.04) / 1.04)),
            coor.trans(coor.xyz(width * 0.5 + 0.5, 0, 0.25)),
            transBoard
        )
        / station.newModel("uus/1990/signs/platform_signs_top.mdl",
            coor.scale(coor.xyz(width + 1, thickness, 1)),
            coor.trans(coor.xyz(0, 0, height * 0.5 + 0.25)),
            transBoard
        )
        / station.newModel("uus/1990/signs/platform_signs_bottom.mdl",
            coor.scale(coor.xyz(width + 1, thickness, 1)),
            coor.trans(coor.xyz(0, 0, -height * 0.5 + 0.25)),
            transBoard
        )
        + nameModelsF(function(w) return coor.trans(coor.xyz(-0.5 * w, -0.055, 0.175 * 3 / 4)) * transBoard end)
        + (hasPole and nameModelsF(function(w) return coor.trans(coor.xyz(-0.5 * w, -0.055, 0.175 * 3 / 4)) * coor.rotZ(pi) * transBoard end) or {})
        + (
        hasPole
        and
        func.map({width * 0.5 + 0.575, -width * 0.5 - 0.575}, function(p)
            return station.newModel("platform_signs/pole_cuboid.mdl",
                coor.scaleZ(1.125),
                coor.trans(coor.xyz(p, 0, -2.25)),
                transBoard
        )
        end)
        or
        {}
)
end

uus.generatePlatformSigns = function(config)
    local isStepsPos = isStepsPos(config)
    local sign = function(arcs, isLeftmost, isRightmost)
        if (arcs.isTrack and not isLeftmost and not isRightmost) then return false end
        local c = arcs.count
        local cModels = 2 * c - 2
        
        local stepPos = isStepsPos(cModels)
        local indices = func.seq(1, cModels)
        
        local indicesN = pipe.new * indices * pipe.map(function(i) local posD, posA, posB = stepPos(i) if (posD or posA or posB) then return false else return i end end)
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
                        * pipe.map(function(p) return p < c and floor(p) or ceil(p) end)
                end)
            * pipe.flatten()
        
        local fn = arcs.isTrack and function()
            return pipe.mapn(
                indices,
                il(arcs.ceil.lc),
                il(arcs.ceil.rc)
            )
            (function(i, lc, rc)
                local posD, posA, posB = stepPos(i)
                if (posA) then
                    local transL = quat.byVec(coor.xyz(-1, 0, 0), lc.i - lc.s):mRot() * coor.trans((i < c and lc.s or lc.i) + coor.xyz(0, 0, 2.25))
                    local transR = quat.byVec(coor.xyz(1, 0, 0), rc.i - rc.s):mRot() * coor.trans((i < c and rc.s or rc.i) + coor.xyz(0, 0, 2.25))
                    return
                        pipe.new
                        / (isLeftmost and placeSign("test", transL) or nil)
                        / (isRightmost and placeSign("test", transR) or nil)
                else
                    return false
                end
            end)
        end
        or
        function()
            return pipe.mapn(
                indices,
                il(arcs.stairs.outer.lc),
                il(arcs.stairs.outer.rc),
                il(arcs.platform.central.mc),
                il(arcs.stairs.inner.lc),
                il(arcs.stairs.inner.rc)
            )
            (function(i, lc, rc, mc, lw, rw)
                local posD, posA, posB = stepPos(i)
                if (posA) then
                    local transL = quat.byVec(coor.xyz(1, 0, 0), lc.i - lc.s):mRot() * coor.trans((i < c and lc.s or lc.i) + coor.xyz(0, 0, 2.25))
                    local transR = quat.byVec(coor.xyz(-1, 0, 0), rc.i - rc.s):mRot() * coor.trans((i < c and rc.s or rc.i) + coor.xyz(0, 0, 2.25))
                    return
                        pipe.new
                        / (not isLeftmost and placeSign("test", transL) or nil)
                        / (not isRightmost and placeSign("test", transR) or nil)
                elseif (indicesN * pipe.contains(i)) then
                    local transL = quat.byVec(coor.xyz(-1, 0, 0), lw.i - lw.s):mRot() * coor.trans((i < c and lw.s or lw.i) + coor.xyz(0, 0, 2.25))
                    local transR = quat.byVec(coor.xyz(1, 0, 0), rw.i - rw.s):mRot() * coor.trans((i < c and rw.s or rw.i) + coor.xyz(0, 0, 2.25))
                    local transM = quat.byVec(coor.xyz(1, 0, 0), mc.i - mc.s):mRot() * coor.trans((i < c and mc.s or mc.i) + coor.xyz(0, 0, 2.25))
                    return
                        pipe.new
                        / (isLeftmost and placeSign("test", transL) or nil)
                        / (isRightmost and placeSign("test", transR) or nil)
                        / (not (isRightmost or isLeftmost) and placeSign("test", transM, true) or nil)
                else
                    return false
                end
            end)
        end
        
        return pipe.new * fn()
            * pipe.filter(pipe.noop())
            * pipe.flatten()
            * pipe.flatten()
    
    end
    
    return function(arcs, isLeftmost, isRightmost)
        return pipe.new * arcs
            * pipe.mapi(function(a, i) return sign(a, isLeftmost and i == 1, isRightmost and i == #arcs) end)
            * pipe.filter(pipe.noop())
            * pipe.flatten()
    end
end


uus.generateModels = function(fitModel, config)
    local tZ = coor.transZ(config.hPlatform - 1.4)-- model height = 1.93 - 1.4 -> 0.53 -> adjust model level to rail level
    local platformZ = config.hPlatform + 0.53 --target Z
    
    local buildPlatform = buildSurface(fitModel, config, platformZ, tZ)
    local buildCeil = buildSurface(fitModel, config, platformZ, coor.I())
    local buildWall = buildSurface(fitModel, config, platformZ, coor.scaleZ(5 - platformZ) * coor.transZ(platformZ))
    local isStepsPos = isStepsPos(config)
    
    local platform = function(arcs)
        local c = arcs.count
        local cModels = 2 * c - 2
        local indices = func.seq(1, cModels)
        local stepPos = isStepsPos(cModels)
        local fnModels = function(normal, down, upA, upB)
            local fn = function(i)
                local posD, posA, posB = stepPos(i)
                if posD then return down
                elseif posA then return upA
                elseif posB then return upB
                else return normal end
            end
            return pipe.new * indices * pipe.map(fn)
        end
        
        local models = {
            platform = {
                left = pipe.rep(cModels)(config.models.platform.left),
                right = pipe.rep(cModels)(config.models.platform.right),
                edge = pipe.rep(cModels)(config.models.platform.edge),
                central = fnModels(config.models.platform.central, false, false, false)
            },
            
            stair = {
                central = fnModels(false, config.models.downstep.central, config.models.upstep.a, config.models.upstep.b),
                left = fnModels(false, config.models.downstep.left, config.models.upstep.aLeft, config.models.upstep.bLeft),
                right = fnModels(false, config.models.downstep.right, config.models.upstep.aRight, config.models.upstep.bRight),
                back = fnModels(false, false, false, config.models.upstep.back),
            
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
        
        local steps = pipe.new
            + pipe.mapn(
                indices,
                models.stair.central,
                il(arcs.stairs.inner.lc), il(arcs.stairs.inner.rc)
            )(buildPlatform(c, 4.5, function(i, lc, rc) return
                i >= c
                and uus.assembleSize(lc, rc)
                or uus.assembleSize({s = rc.i, i = rc.s}, {s = lc.i, i = lc.s})
            end))
            + pipe.mapn(
                indices,
                models.stair.back,
                il(arcs.stairs.inner.lc), il(arcs.stairs.inner.rc)
            )(buildWall(c, 4.5, function(i, lc, rc) return
                i >= c
                and uus.assembleSize(lc, rc)
                or uus.assembleSize({s = rc.i, i = rc.s}, {s = lc.i, i = lc.s})
            end))
            + pipe.mapn(
                indices,
                models.stair.left,
                il(arcs.stairs.outer.lc),
                il(arcs.stairs.inner.lc),
                il(arcs.stairs.outer.rc),
                il(arcs.stairs.inner.rc)
            )(function(i, ...)
                local _, posA, posB = stepPos(i)
                return ((posA or posB) and buildWall or buildPlatform)(c, 0.25, function(i, loc, lic, roc, ric) return
                    i >= c
                    and uus.assembleSize(loc, lic)
                    or uus.assembleSize({s = roc.i, i = roc.s}, {s = ric.i, i = ric.s})
                end)(i, ...)
            end
            )
            + pipe.mapn(
                indices,
                models.stair.right,
                il(arcs.stairs.outer.lc),
                il(arcs.stairs.inner.lc),
                il(arcs.stairs.outer.rc),
                il(arcs.stairs.inner.rc)
            )(function(i, ...)
                local _, posA, posB = stepPos(i)
                return ((posA or posB) and buildWall or buildPlatform)(c, 0.25, function(i, loc, lic, roc, ric) return
                    i >= c
                    and uus.assembleSize(ric, roc)
                    or uus.assembleSize({s = lic.i, i = lic.s}, {s = loc.i, i = loc.s})
                end)(i, ...)
            end
        )
        local platforms = pipe.new
            + pipe.mapn(
                indices,
                models.platform.central,
                il(arcs.stairs.outer.lc), il(arcs.stairs.outer.rc)
            )(buildPlatform(c, 5, function(i, lc, rc) return
                i >= c
                and uus.assembleSize(lc, rc)
                or uus.assembleSize({s = rc.i, i = rc.s}, {s = lc.i, i = lc.s})
            end))
            + pipe.mapn(
                indices,
                models.platform.left,
                il(arcs.platform.central.lc), il(arcs.stairs.outer.lc)
            )(buildPlatform(c, 1.7))
            + pipe.mapn(
                indices,
                models.platform.right,
                il(arcs.stairs.outer.rc), il(arcs.platform.central.rc)
            )(buildPlatform(c, 1.7))
            + pipe.mapn(
                indices,
                models.platform.edge,
                il(arcs.platform.edge.lc), il(arcs.platform.central.lc)
            )(buildPlatform(c, 0.8))
            + pipe.mapn(
                indices,
                models.platform.edge,
                il(func.rev(arcs.platform.edge.rc)), il(func.rev(arcs.platform.central.rc))
            )(buildPlatform(c, 0.8))
        
        local ceils =
            pipe.new
            + pipe.mapn(
                indices,
                models.ceil.central,
                il(arcs.stairs.outer.lc), il(arcs.stairs.outer.rc)
            )(buildCeil(c, 5))
            + pipe.mapn(
                indices,
                models.ceil.left,
                il(arcs.ceil.central.lc), il(arcs.stairs.outer.lc)
            )(buildCeil(c, 1.8))
            + pipe.mapn(
                indices,
                models.ceil.right,
                il(arcs.stairs.outer.rc), il(arcs.ceil.central.rc)
            )(buildCeil(c, 1.8))
            + pipe.mapn(
                indices,
                models.ceil.edge,
                il(arcs.ceil.edge.lc), il(arcs.ceil.central.lc)
            )(buildCeil(c, 0.7))
            + pipe.mapn(
                indices,
                models.ceil.edge,
                il(func.rev(arcs.ceil.edge.rc)), il(func.rev(arcs.ceil.central.rc))
            )(buildCeil(c, 0.7))
        
        local tops = pipe.new
            + pipe.mapn(
                indices,
                models.top.central,
                il(arcs.stairs.outer.lc), il(arcs.stairs.outer.rc)
            )(buildCeil(c, 5))
            + pipe.mapn(
                indices,
                models.top.left,
                il(arcs.ceil.edge.lc), il(arcs.stairs.outer.lc)
            )(buildCeil(c, 2.5))
            + pipe.mapn(
                indices,
                models.top.right,
                il(arcs.stairs.outer.rc), il(arcs.ceil.edge.rc)
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
        
        return (pipe.new / platforms / ceils / tops / extremityPlatform / steps) * pipe.flatten() * pipe.flatten() + extremity
    end
    
    local track = function(arcs)
        local ceilTop = pipe.rep(2 * arcs.ceil.c - 2)(config.models.top.track.central)
        
        return pipe.new
            * pipe.mapn(
                func.seq(1, 2 * arcs.ceil.c - 2),
                ceilTop,
                il(arcs.ceil.lc), il(arcs.ceil.rc)
            )(buildCeil(arcs.ceil.c, 5))
            * pipe.flatten()
    end
    
    return function(arcs)
        return pipe.new * arcs * pipe.map(function(a) return a.isTrack and track(a) or platform(a) end) * pipe.flatten()
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
                }
            }
            
            local lsc, rsc, lsuc, rsuc, lc, rc, lpc, rpc, lpic, rpic, lsoc, rsoc, lsic, rsic, c = uus.biLatCoords(5)(
                arcs.platform.edge.l, arcs.platform.edge.r,
                arcs.platform.central.l, arcs.platform.central.r,
                arcs.platform.lane.l, arcs.platform.lane.r,
                arcs.ceil.edge.l, arcs.ceil.edge.r,
                arcs.ceil.central.l, arcs.ceil.central.r,
                arcs.stairs.outer.l, arcs.stairs.outer.r,
                arcs.stairs.inner.l, arcs.stairs.inner.r
            )
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
                isPlatform = true
            }
        else
            local arc = p[1]
            
            local general = {
                l = arc(refZ)(),
                r = arc(refZ)()
            }
            
            local ceil = arcGen(general, -config.wTrack * 0.5)
            
            local lpc, rpc, c = uus.biLatCoords(5)(ceil.l, ceil.r)
            
            return {
                [1] = arc,
                count = c,
                ceil = func.with(ceil, {lc = lpc, rc = rpc, mc = mc(lpc, rpc), c = c}),
                isTrack = true
            }
        end
    end)
end

uus.generateLanes = function(config)
    return function(allLanePos)
        local nTransversal = allLanePos * pipe.map(function(l) return #l end) * pipe.max()
        local transversal =
            pipe.new
            * func.seq(1, nTransversal)
            * pipe.map(function(t)
                local pts = allLanePos * pipe.map(pipe.select(t)) * pipe.filter(pipe.noop())
                return func.map(il(pts), function(pt) return station.newModel("uus/standard_lane.mdl", uus.mRot(pt.s - pt.i), coor.trans(pt.i)) end)
            end)
            * pipe.flatten()
        
        local centre = allLanePos * pipe.flatten() * function(pts) return #pts > 0 and pts[1]:avg(unpack(func.range(pts, 2, #pts))) or nil end
        local lineCentre = allLanePos * pipe.flatten() * pipe.map(function(pt)
            return station.newModel("uus/standard_lane.mdl", uus.mRot(centre - pt), coor.trans(pt)) end)
        return transversal + lineCentre, allLanePos * pipe.flatten() / centre * pipe.rev()
    end
end

uus.build = function(config, fitModel, generateEdges)
    local generateEdges = uus.generateEdges
    local generateMockEdges = uus.generateMockEdges(config)
    local generateModels = uus.generateModels(fitModel, config)
    local generateTerminals = uus.generateTerminals(config)
    local generateTrackTerrain = uus.generateTrackTerrain(config)
    local generateSideWalls = uus.generateSideWalls(fitModel, config)
    local generateLanes = uus.generateLanes(config)
    local generatePlatformSigns = uus.generatePlatformSigns(config)
    
    local function build(edges, mockEdges, terminals, terminalsGroup, lanePos, models, terrain, gr, ...)
        local isLeftmost = #models == 0
        local isRightmost = #{...} == 0
        
        if (gr == nil) then
            local upLanePos = lanePos * pipe.map(pipe.filter(function(p) return p.z > config.hPlatform end))
            local downLanePos = lanePos * pipe.map(pipe.filter(function(p) return p.z < config.hPlatform end))
            local upLanes, upConnectors = generateLanes(upLanePos)
            local downLanes, downConnectors = generateLanes(downLanePos)
            return edges, mockEdges, terminals, terminalsGroup,
                (models + upLanes + downLanes) * pipe.filter(pipe.noop()),
                terrain,
                upConnectors * pipe.map(function(c) return func.with(c, {toUpLevel = true}) end)
                + downConnectors * pipe.map(function(c) return func.with(c, {toUpLevel = false}) end)
        elseif (#gr == 3 and gr[1].isTrack and gr[2].isPlatform and gr[3].isTrack) then
            local edges = generateEdges(edges, true, gr[1][1])
            local edges = generateEdges(edges, false, gr[3][1])
            local mockEdges = generateMockEdges(mockEdges, gr[2][3])
            local terminals, terminalsGroup, lanePos = generateTerminals(edges, terminals, terminalsGroup, lanePos, gr[2], {true, true})
            return build(
                edges,
                mockEdges,
                terminals,
                terminalsGroup,
                lanePos,
                models + generateModels(gr)
                + generatePlatformSigns(gr, isLeftmost, isRightmost)
                + (isLeftmost and generateSideWalls(gr[1], true) or {})
                + (isRightmost and generateSideWalls(gr[3], false) or {}),
                terrain,
                ...)
        elseif (#gr == 2 and gr[1].isTrack and gr[2].isPlatform) then
            local edges = generateEdges(edges, true, gr[1][1])
            local mockEdges = generateMockEdges(mockEdges, gr[2][3])
            local terminals, terminalsGroup, lanePos = generateTerminals(edges, terminals, terminalsGroup, lanePos, gr[2], {true, false})
            return build(
                edges,
                mockEdges,
                terminals,
                terminalsGroup,
                lanePos,
                models + generateModels(gr)
                + generatePlatformSigns(gr, isLeftmost, isRightmost)
                + (isLeftmost and generateSideWalls(gr[1], true) or {})
                + (isRightmost and generateSideWalls(gr[2], false) or {}),
                terrain,
                ...)
        elseif (#gr == 2 and gr[1].isPlatform and gr[2].isTrack) then
            local edges = generateEdges(edges, false, gr[2][1])
            local mockEdges = generateMockEdges(mockEdges, gr[1][3])
            local terminals, terminalsGroup, lanePos = generateTerminals(edges, terminals, terminalsGroup, lanePos, gr[1], {false, true})
            return build(
                edges,
                mockEdges,
                terminals,
                terminalsGroup,
                lanePos,
                models + generateModels(gr)
                + generatePlatformSigns(gr, isLeftmost, isRightmost)
                + (isLeftmost and generateSideWalls(gr[1], true) or {})
                + (isRightmost and generateSideWalls(gr[2], false) or {}),
                terrain,
                ...)
        elseif (#gr == 1 and gr[1].isPlatform) then
            local terminals, terminalsGroup, lanePos = generateTerminals(edges, terminals, terminalsGroup, lanePos, gr[1], {false, false})
            local mockEdges = generateMockEdges(mockEdges, gr[1][3])
            return build(
                edges,
                mockEdges,
                terminals,
                terminalsGroup,
                lanePos,
                models + generateModels(gr)
                + generatePlatformSigns(gr, isLeftmost, isRightmost)
                + (isLeftmost and generateSideWalls(gr[1], true) or {})
                + (isRightmost and generateSideWalls(gr[1], false) or {}),
                terrain,
                ...)
        else
            local edges = generateEdges(edges, false, gr[1][1])
            return build(
                edges,
                mockEdges,
                terminals,
                terminalsGroup,
                lanePos,
                models + generateModels(gr)
                + generatePlatformSigns(gr, isLeftmost, isRightmost)
                + (isLeftmost and generateSideWalls(gr[1], true) or {})
                + (isRightmost and generateSideWalls(gr[1], false) or {}),
                terrain,
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
        platform = {
            edge = prefixM("platform/platform_edge"),
            central = prefixM("platform/platform_central"),
            left = prefixM("platform/platform_left"),
            right = prefixM("platform/platform_right"),
        },
        upstep = {
            a = prefixM("platform/platform_upstep_a"),
            b = prefixM("platform/platform_upstep_b"),
            aLeft = prefixM("platform/platform_upstep_a_left"),
            aRight = prefixM("platform/platform_upstep_a_right"),
            bLeft = prefixM("platform/platform_upstep_b_left"),
            bRight = prefixM("platform/platform_upstep_b_right"),
            back = prefixM("platform/platform_upstep_back")
        },
        downstep = {
            right = prefixM("platform/platform_downstep_left"),
            left = prefixM("platform/platform_downstep_right"),
            central = prefixM("platform/platform_downstep")
        },
        ceil = {
            edge = prefixM("platform/ceil_edge"),
            central = prefixM("platform/ceil_central"),
            left = prefixM("platform/ceil_left"),
            right = prefixM("platform/ceil_right"),
            aLeft = prefixM("platform/ceil_upstep_a_left"),
            aRight = prefixM("platform/ceil_upstep_a_right"),
            bLeft = prefixM("platform/ceil_upstep_b_left"),
            bRight = prefixM("platform/ceil_upstep_b_right"),
        },
        top = {
            track = {
                left = prefixM("platform/top_track_left"),
                right = prefixM("platform/top_track_right"),
                central = prefixM("platform/top_track_central")
            },
            platform = {
                left = prefixM("platform/top_platform_left"),
                right = prefixM("platform/top_platform_right"),
                central = prefixM("platform/top_platform_central")
            },
        },
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
