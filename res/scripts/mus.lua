local func = require "entry/func"
local coor = require "entry/coor"
local arc = require "mus/coorarc"
local general = require "entry/general"
local pipe = require "entry/pipe"

local mus = {}

local math = math
local pi = math.pi
local abs = math.abs
local ceil = math.ceil
local floor = math.floor
local unpack = table.unpack

local segmentLength = 20

mus.slotInfo = function(slotId)
        -- Platform/track
        -- 1 ~ 3 : pos x (0~500 > right, 501 ~ 999 < left)
        -- 4: 1 for platform, 0 for track
        -- 5: group (pos z)
        -- Stairs
        -- 1 ~ 3 : pos x
        -- 4 : 2 for downstairs 3 for upstairs
        -- 5: group (pos z)
        -- 6 ~ 8 : pos y
        -- Entries
        -- 5: 9
        local d13 = slotId % 1000
        local d14 = slotId % 10000
        local d15 = slotId % 100000
        local posX = d13
        local typeId = (d14 - d13) / 1000
        local posZ = (d15 - d14) / 10000
        local posY = (slotId > 0 and (slotId - d15) or (slotId + d15)) / 100000
        if posX > 500 then posX = posX - 1000 end
        return {
            pos = coor.xyz(posX, posY, posZ),
            typeId = typeId
        }
end

mus.normalizeRad = function(rad)
    return (rad < pi * -0.5) and mus.normalizeRad(rad + pi * 2) or
        ((rad > pi + pi * 0.5) and mus.normalizeRad(rad - pi * 2) or rad)
end

mus.arc2Edges = function(arc)
    local extLength = 2
    local extArc = arc:extendLimits(-extLength)
    local length = arc.r * abs(arc.sup - arc.inf)
    
    local sup = arc:pt(arc.sup)
    local inf = arc:pt(arc.inf)
    
    local supExt = arc:pt(extArc.sup)
    local infExt = arc:pt(extArc.inf)
    
    local vecSupExt = arc:tangent(extArc.sup)
    local vecInfExt = arc:tangent(extArc.inf)
    
    local vecSup = arc:tangent(arc.sup)
    local vecInf = arc:tangent(arc.inf)
    
    return {
        {inf, vecInf * extLength},
        {infExt, vecInfExt * extLength},
        
        {infExt, vecInfExt * (length - extLength)},
        {supExt, vecSupExt * (length - extLength)},
        
        {supExt, vecSupExt * extLength},
        {sup, vecSup * extLength}
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

local retriveNSeg = function(length, l, ...)
    return (function(x) return (x < 1 or (x % 1 > 0.5)) and ceil(x) or floor(x) end)(l:length() / length), l, ...
end

local retriveBiLatCoords = function(nSeg, l, ...)
    local rst = pipe.new * {l, ...}
    local lscale = l:length() / (nSeg * segmentLength)
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

mus.biLatCoords = function(length)
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

mus.assembleSize = function(lc, rc)
    return {
        lb = lc.i,
        lt = lc.s,
        rb = rc.i,
        rt = rc.s
    }
end

mus.fitModel2D = function(zOffset)
    return function(w, h, _, size, fitTop, fitLeft)
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
        } * coor.transZ(zOffset)
    end
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

mus.interlace = pipe.interlace({"s", "i"})

mus.unitLane = function(f, t) return ((t - f):length2() > 1e-2 and (t - f):length2() < 562500) and general.newModel("mus/person_lane.mdl", general.mRot(t - f), coor.trans(f)) or nil end

mus.buildSurface = function(config, tZ)
    return function(w, fnSize)
        local fnSize = fnSize or function(_, lc, rc) return mus.assembleSize(lc, rc) end
        return function(i, s, ...)
            local sizeS = fnSize(i, ...)
            
            return s
                and pipe.new
                / func.with(general.newModel(s .. "_tl.mdl", tZ, config.fitModel(w, 5, config.refZ, sizeS, true, true)), {pos = i})
                / func.with(general.newModel(s .. "_br.mdl", tZ, config.fitModel(w, 5, config.refZ, sizeS, false, false)), {pos = i})
                or pipe.new * {}
        end
    end
end

mus.linkConnectors = function(allConnectors)
    return
        allConnectors
        * pipe.interlace()
        * pipe.map(function(conn)
            local recordL = {}
            local recordR = {}
            if (#conn[1] == 0 and #conn[2] == 0) then return {} end
            
            for i, l in ipairs(conn[1]) do
                for j, r in ipairs(conn[2]) do
                    local vec = l - r
                    local dist = vec:length2()
                    recordL[i] = recordL[i] and recordL[i].dist < dist and recordL[i] or {dist = dist, vec = vec, i = i, j = j, l = l, r = r}
                    recordR[j] = recordR[j] and recordR[j].dist < dist and recordR[j] or {dist = dist, vec = vec, i = i, j = j, l = l, r = r}
                end
            end
            
            return #recordL == 0 and #recordR == 0 and {} or (pipe.new + recordL + recordR)
                * pipe.sort(function(l, r) return l.i < r.i or (l.i == r.i and l.j < r.j) end)
                * pipe.fold(pipe.new * {}, function(result, e)
                    if #result > 0 then
                        local lastElement = result[#result]
                        return (lastElement.i == e.i and lastElement.j == e.j) and result or (result / e)
                    else
                        return result / e
                    end
                end)
                * pipe.map(function(e) return mus.unitLane(e.l, e.r) end)
        end)
        * pipe.flatten()
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

mus.terrain = function(config, ref)
    return pipe.mapn(
        ref.lc,
        ref.rc
    )(function(lc, rc)
        local size = mus.assembleSize(lc, rc)
        return pipe.new / size.lt / size.lb / size.rb / size.rt * pipe.map(coor.vec2Tuple)
    end)
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
