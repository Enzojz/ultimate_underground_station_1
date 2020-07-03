local func = require "entry/func"
local coor = require "entry/coor"
local arc = require "mus/coorarc"
local general = require "entry/general"
local pipe = require "entry/pipe"
local musm = require "mus_menu"
-- local dump = require "luadump"

local mus = {}

local math = math
local pi = math.pi
local abs = math.abs
local ceil = math.ceil
local floor = math.floor
local unpack = table.unpack
local min = math.min

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

mus.fitModel2D = function(w, h, d, fitTop, fitLeft)
    local s = {
        {
            coor.xy(0, 0),
            coor.xy(fitLeft and w or -w, 0),
            coor.xy(0, fitTop and -h or h),
        },
        {
            coor.xy(0, 0),
            coor.xy(fitLeft and -w or w, 0),
            coor.xy(0, fitTop and h or -h),
        }
    }
    
    local mX = func.map(s,
        function(s) return {
            {s[1].x, s[1].y, 1},
            {s[2].x, s[2].y, 1},
            {s[3].x, s[3].y, 1},
        }
        end)
    
    local mXI = func.map(mX, coor.inv3)
    
    local fitTop = {fitTop, not fitTop}
    local fitLeft = {fitLeft, not fitLeft}

    return function(size, mode, z)
        local mXI = mXI[mode and 1 or 2]
        local fitTop = fitTop[mode and 1 or 2]
        local fitLeft = fitLeft[mode and 1 or 2]
        local refZ = size.lt.z

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
        
        local mU = {
            t[1].x, t[1].y, 1,
            t[2].x, t[2].y, 1,
            t[3].x, t[3].y, 1,
        }
        
        local mXi = mul(mXI, mU)
        
        return coor.I() * {
            mXi[1], mXi[2], 0, mXi[3],
            mXi[4], mXi[5], 0, mXi[6],
            0, 0, 1, 0,
            mXi[7], mXi[8], 0, mXi[9]
        } * coor.scaleZ(z and (z / d) or 1) * coor.transZ(refZ)
    end
end

mus.fitModel = function(w, h, d, fitTop, fitLeft)
    local s = {
        {
            coor.xyz(0, 0, 0),
            coor.xyz(fitLeft and w or -w, 0, 0),
            coor.xyz(0, fitTop and -h or h, 0),
            coor.xyz(0, 0, d)
        },
        {
            coor.xyz(0, 0, 0),
            coor.xyz(fitLeft and -w or w, 0, 0),
            coor.xyz(0, fitTop and h or -h, 0),
            coor.xyz(0, 0, d)
        },
    }
    
    local mX = func.map(s, function(s)
        return {
            {s[1].x, s[1].y, s[1].z, 1},
            {s[2].x, s[2].y, s[2].z, 1},
            {s[3].x, s[3].y, s[3].z, 1},
            {s[4].x, s[4].y, s[4].z, 1}
        }
    end)
    
    local mXI = func.map(mX, coor.inv)
    
    local fitTop = {fitTop, not fitTop}
    local fitLeft = {fitLeft, not fitLeft}
    
    return function(size, mode, z)
        local z = z or d
        local mXI = mXI[mode and 1 or 2]
        local fitTop = fitTop[mode and 1 or 2]
        local fitLeft = fitLeft[mode and 1 or 2]
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
        local mU = {
            t[1].x, t[1].y, t[1].z, 1,
            t[2].x, t[2].y, t[2].z, 1,
            t[3].x, t[3].y, t[3].z, 1,
            t[1].x, t[1].y, t[1].z + z, 1
        }
        
        return mXI * mU
    end
end

mus.interlace = pipe.interlace({"s", "i"})

mus.unitLane = function(f, t) return ((t - f):length2() > 1e-2 and (t - f):length2() < 562500) and general.newModel("mus/person_lane.mdl", general.mRot(t - f), coor.trans(f)) or nil end

mus.stepLane = function(f, t)
    local vec = t -f
    local length = vec:length()
    if (length > 750 or length < 0.1) then return {} end
    local dZ = abs((f - t).z)
    if (length > dZ * 3 or length < 5) then
        return {general.newModel("mus/person_lane.mdl", general.mRot(vec), coor.trans(f))}
    else
        local hVec = vec:withZ(0) / 3
        local fi = f + hVec
        local ti = t - hVec
        return {
            general.newModel("mus/person_lane.mdl", general.mRot(fi - f), coor.trans(f)),
            general.newModel("mus/person_lane.mdl", general.mRot(ti - fi), coor.trans(fi)),
            general.newModel("mus/person_lane.mdl", general.mRot(t - ti), coor.trans(ti))
        }
    end
end

mus.buildSurface = function(tZ)
    return function(fitModel, fnSize)
        local fnSize = fnSize or function(_, lc, rc) return mus.assembleSize(lc, rc) end
        return function(i, s, ...)
            local sizeS = fnSize(i, ...)
            return s
                and pipe.new
                / func.with(general.newModel(s .. "_tl.mdl", tZ and (tZ * fitModel(sizeS, true)) or fitModel(sizeS, true)), {pos = i})
                / func.with(general.newModel(s .. "_br.mdl", tZ and (tZ * fitModel(sizeS, false)) or fitModel(sizeS, false)), {pos = i})
                or pipe.new * {}
        end
    end
end

mus.linkConnectors = function(allConnectors)
    return
        #allConnectors < 2 
        and {} 
        or allConnectors
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


local makeLayout = function(totalTracks, ignoreFst, ignoreLst)
    local function makeLayout(nbTracks, result)
        local p = false
        local t = true
        if (nbTracks == 0) then
            local result = ignoreLst and result or (result[#result] and (result / p) or result)
            return result
        elseif (nbTracks == totalTracks and ignoreFst) then
            return makeLayout(nbTracks - 1, result / t)
        elseif (nbTracks == totalTracks and not ignoreFst) then
            return makeLayout(nbTracks - 1, result / p / t)
        elseif (nbTracks == 1 and ignoreLst) then
            return makeLayout(nbTracks - 1, ((not result) or result[#result]) and (result / p / t) or (result / t))
        elseif (nbTracks == 1 and not ignoreLst) then
            return makeLayout(nbTracks - 1, result / t / p)
        elseif (result[#result] == t) then
            return makeLayout(nbTracks - 2, result / t / p / t)
        else
            return makeLayout(nbTracks - 1, result / t)
        end
    end
    return makeLayout(totalTracks, pipe.new)
end

mus.createTemplateFn = function(params, ...)
    local radius = musm.rList[params.radius + 1] * 1000
    local length = min(musm.trackLengths[params.lPlatform + 1], abs(radius * pi * 1.5))
    
    local nbTracks = musm.trackNumberList[params.trackNb + 1]
    local layout = makeLayout(nbTracks, params.platformLeft == 0, params.platformRight == 0)
    local midPos = ceil(#layout / 2)
    local nSeg = length / 5
    local stair = floor(nSeg / 4)
    local result = {}
    local trackType = ("%s%s.module"):format(params.capturedParams.moduleList[params.trackType + 1], params.catenary == 1 and "_catenary" or "")
    local platformType = musm.platformWidthList[(params.platformWidth) + 1]
    for i, t in ipairs(layout) do
        if t then
            result[(i - midPos >= 0 and i or 1000 + i) - midPos] = trackType
        else
            local slot = (i - midPos >= 0 and i or 1000 + i) + 1000 - midPos
            result[slot] = platformType
            result[slot + 2000 + stair * 100000] = "station/rail/mus_platform_upstair.module"
            result[slot + 2000 + (stair + 1) * 100000] = "station/rail/mus_platform_upstair.module"
        end
    end
    
    return result
end

local offsetGen = function(meta)
    local function gen(result, base, lastPos, fst, snd, ...)
        if (fst and snd) then
            local pos = snd.pos
            local offset = base + (fst.width + snd.width) * 0.5
            
            return gen(result + {{pos, offset}}, offset, pos, snd, ...)
        else
            return result
        end
    end
    return gen(pipe.new * {}, 0, nil, {width = 0}, unpack(meta))
end

local function connectionLevel(lhs, rhs)
    local result = lhs
        * pipe.map(
            function(l)
                return func.map(rhs,
                    function(r)
                        return {
                            distance = (l - r):length2(),
                            pt = {l, r}
                        }
                    end)
            end)
        * pipe.flatten()
        * pipe.sort(function(l, r) return l.distance < r.distance end)
    
    return result[1]
end

local function closestConnection(result, measure, ...)
    local rest = {...}
    if (#rest == 0) then return result end
    local r = false
    for i = 1, #rest do
        local a = rest[i]
        for j = 1, #result do
            local b = result[j].index
            r = r and r.measure and r.measure.distance < measure[a][b].distance and r or {index = a, measure = measure[a][b]}
        end
    end
    return closestConnection(result / r, measure, unpack(func.filter(rest, function(i) return i ~= r.index end)))
end

local function connectionNetwork(allConns)
    local measure = {}
    for i = 1, #allConns do
        measure[i] = {}
        for j = 1, i - 1 do
            measure[i][j] = connectionLevel(allConns[i], allConns[j])
            measure[j][i] = measure[i][j]
        end
    end
    
    local result = closestConnection(pipe.new / {index = 1}, measure, unpack(func.seq(2, #allConns)))
    return result * pipe.filter(pipe.select("measure")) * pipe.map(function(r) return mus.stepLane(unpack(r.measure.pt)) end) * pipe.flatten()
end

local depth = -12
local makeConfig = function(params, transf)
    local radius = musm.rList[params.radius + 1] * 1000
    local length = min(musm.trackLengths[params.lPlatform + 1], abs(radius * pi * 1.5))
    local hPlatform = musm.hPlatformList[params.hPlatform + 1] * 0.001
    local slope = musm.slopeList[params.slope + 1] * 0.001
    
    local modelType = mus.models({platform = params.floor + 1, wall = params.wall + 1})
    
    local trans, mr, _ = coor.decomposite(transf)
    local refZ = hPlatform + 0.53
    local fitModel = slope == 0 and mus.fitModel2D or mus.fitModel
    return {
        isFinalized = params.isFinalized == 1,
        fitModel = fitModel,
        hPlatform = hPlatform,
        radius = radius,
        refZ = refZ,
        slope = slope,
        models = modelType,
        depth = depth,
        length = length,
        transf = {
            pt = transf * coor.transZ(depth),
            vec = mr
        },
        build = {
            platform = mus.buildSurface(coor.transZ(-1.93)),
            ceil = mus.buildSurface(),
            wall = mus.buildSurface(coor.scaleZ(5 - refZ))
        },
        fitModels = {
            platform = {
                central = fitModel(5, 5, 1.93, true, true),
                side = fitModel(1.7, 5, 1.93, true, true),
                edge = fitModel(0.8, 5, 1.93, true, true),
                wall = fitModel(5, 5, 1, true, true),
            },
            ceil = {
                central = fitModel(5, 5, 5.4, true, true),
                side = fitModel(1.8, 5, 5.4, true, true),
                edge = fitModel(0.7, 5, 5.4, true, true)
            },
            top = {
                central = fitModel(5, 5, 5.4, true, true),
                side = fitModel(2.5, 5, 7.5, true, true)
            },
            step = {
                central = fitModel(4.5, 5, 1.93, true, true),
                wall = fitModel(0.25, 5, 1.93, true, true),
            },
            track = {
                wall = fitModel(5, 5, 7.5, true, true),
                top = fitModel(5, 5, 7.5, true, true)
            }
        }
    }
end

mus.upgradeFn = function(params)
    local modules = {}
    local slotId = params.slotId
    if params.modules[slotId] then
        local function SwapModule(from, to, ...)
            if (from and to) then
                if params.modules[slotId].name == from then
                    table.insert(modules, {slotId, to})
                elseif params.modules[slotId].name == to then
                    table.insert(modules, {slotId, from})
                else
                    SwapModule(...)
                end
            end
        end
        
        if (params.catenaryToggle == 1) then
            SwapModule(
                "station/rail/mus_track_hs.module",
                "station/rail/mus_track_hs_catenary.module",
                "station/rail/mus_track_std.module",
                "station/rail/mus_track_std_catenary.module"
        )
        elseif (params.trackTypeToggle == 1) then
            SwapModule(
                "station/rail/mus_track_std.module",
                "station/rail/mus_track_hs.module",
                "station/rail/mus_track_std_catenary.module",
                "station/rail/mus_track_hs_catenary.module"
        )
        end
    end
    return modules
end

mus.updateFn = function(params)
    if #func.keys(params.modules) == 0 then
        return {
            edgeLists = {},
            models = {{
                id = "asset/icon/marker_exclamation.mdl",
                transf = coor.I(),
            }},
            terrainAlignmentLists = {{type = "EQUAL", faces = {}}}
        }
    end
    
    local result = {
        group = {},
        allArcs = {},
        slots = pipe.new * {},
        invoke = {{}, {}},
        models = pipe.new * {},
        terrainAlignmentLists = {},
        edgeLists = pipe.new * {},
        colliders = {},
        groundFaces = {},
        terminalGroups = {},
        stations = {},
        dependentSlots = {},
        slotConfig = {},
        terminalInfo = {},
        entryConnectors = {},
        entryModules = {},
        labelText = {},
        cost = 0
    }
    
    for slotId, m in pairs(params.modules) do
        local info = mus.slotInfo(slotId)
        if (info.pos.z < 9) then
            if (not result.group[info.pos.z]) then
                result.group[info.pos.z] =
                    {
                        allPos = pipe.new * {},
                        pos2Slot = {},
                        connectors = {},
                        modules = {},
                        arcs = {},
                        terminalInfo = {}
                    }
            
            end
            if (params.modules[slotId].params and not result.group[info.pos.z].config) then
                result.group[info.pos.z].config = makeConfig(params.modules[slotId].params, params.modules[slotId].transf)
            end
        elseif (info.pos.z == 9) then
            result.entryModules[slotId] = m
            table.insert(result.slots,
                {
                    id = slotId,
                    transf = m.transf,
                    type = "entry",
                    spacing = {0, 0, 0, 0}
                }
        )
        end
    end
    
    if (#func.keys(result.group) == 1) then
        local key = unpack(func.keys(result.group))
        result.group[key].config = makeConfig(params, coor.I())
    end
    
    for _, g in pairs(result.group) do
        if (not g.config) then
            g.config = makeConfig(params, coor.I())
        end
    end
    
    for slotId, m in pairs(params.modules) do
        local info = mus.slotInfo(slotId)
        if info.pos.z < 9 and info.typeId < 2 then
            local gr = result.group[info.pos.z]
            local pos = info.pos.x
            gr.allPos[#gr.allPos + 1] = pos
            gr.pos2Slot[pos] = slotId
            gr.modules[pos] = func.with(m,
                {
                    slotId = slotId,
                    metadata = func.with(m.metadata, {pos = pos})
                }
        )
        end
    end
    
    for n, g in pairs(result.group) do
        g.allPos = g.allPos * pipe.sort()
        local config = g.config
        local posMin = func.min(g.allPos)
        local posMax = func.max(g.allPos)
        
        local offsets = pipe.new
            * func.seq(posMin > 0 and -1 or (posMin - 1), posMax + 1)
            * pipe.map(function(pos)
                return g.modules[pos] and g.modules[pos].metadata or {pos = pos, width = 5}
            end)
            * offsetGen
        
        local reOffset = func.filter(offsets, function(o) return o[1] == 0 end)[1][2]
        offsets = offsets * pipe.map(function(o) return {o[1], o[2] - reOffset} end)
        
        local mrr = config.radius - offsets[config.radius < 0 and 1 or #offsets][2]
        
        local refLength = (config.radius / mrr) * config.length
        config.arcPacker = mus.arcPacker(refLength, config.slope, config.radius)
        
        local entityOffsets = offsets * pipe.fold(pipe.new * {}, function(r, o) return func.contains(g.allPos, o[1]) and r / o[2] or r end)
        
        for i, pos in ipairs(g.allPos) do
            g.arcs[pos] = config.arcPacker(config.radius - entityOffsets[i], coor.xyz(config.radius, 0, 0))
        end
        local slots = offsets
            * pipe.map(function(ph)
                local pos, offset = unpack(ph)
                return {
                    {
                        id = n * 10000 + (pos < 0 and (pos + 1000) or pos),
                        transf = coor.transX(offset) * g.config.transf.pt,
                        type = "mus_track",
                        spacing = {2.5, 2.5, 5, 5}
                    },
                    {
                        id = n * 10000 + (pos < 0 and (pos + 1000 + 1000) or (pos + 1000)),
                        transf = coor.transX(offset) * g.config.transf.pt,
                        type = "mus_platform",
                        spacing = {2.5, 2.5, 5, 5}
                    }
                }
            end)
            * pipe.flatten()
        result.slots = result.slots + slots
        
        local protectionEdges = pipe.mapn(
            {posMin, posMax},
            {-5, 5}
        )(function(pos, offset)
            if (g.modules[pos].metadata.isTrack) then
                return function()
                    local refArc = pipe.new
                        * g.arcs[pos]()()(offset)
                        * pipe.map(mus.arc2Edges)
                        * pipe.flatten()
                        * pipe.map(function(e) return {e[1] .. g.config.transf.pt, e[2] .. g.config.transf.vec} end)
                        * pipe.map(pipe.map(coor.vec2Tuple))
                    
                    local edges = {
                        type = "TRACK",
                        alignTerrain = false,
                        params = {
                            type = "mus_mock.lua",
                            catenary = false,
                        },
                        edgeType = "TUNNEL",
                        edgeTypeName = "mus_void.lua",
                        edges = refArc,
                        snapNodes = {},
                        tag2nodes = {}
                    }
                    
                    result.edgeLists = result.edgeLists / edges
                end
            else
                return function() end
            end
        end)
        table.insert(result.invoke[2], protectionEdges[1])
        table.insert(result.invoke[2], protectionEdges[2])
    end
    
    result.terminateConstructionHook = function()
        for _, f in ipairs(result.invoke[1]) do f() end
        for _, f in ipairs(result.invoke[2]) do f() end
        
        local signNrList = {}
        for i, m in ipairs(result.models) do
            if (m.posx) then
                if not signNrList[m.posx] then
                    signNrList[m.posx] = {left = {}, right = {}}
                end
                if m.isNrLeft then
                    table.insert(signNrList[m.posx].left, i)
                elseif m.isNrRight then
                    table.insert(signNrList[m.posx].right, i)
                end
            end
        end
        
        local connectors = pipe.new * {}
        
        local terminalTypes = {}
        for _, g in pairs(result.group) do
            local modules = g.modules
            local posSeq = pipe.new * func.seq(func.min(g.allPos), func.max(g.allPos))
            local orderedPos = posSeq * pipe.filter(function(pos) return pos >= 0 end) + posSeq * pipe.filter(function(pos) return pos < 0 end) * pipe.rev()
            
            for _, pos in ipairs(orderedPos) do
                local m = modules[pos]
                if m then
                    if m.metadata.isTrack then
                        local fn = {
                            function()
                                if (modules[pos - 1] and modules[pos - 1].metadata.isPlatform) then
                                    local node = g.terminalInfo[pos] + 7
                                    local tType = g.terminalInfo[pos - 1][2][3]
                                    result.terminalGroups[#result.terminalGroups + 1] = {
                                        callbacks = {},
                                        terminals = func.seqMap(g.terminalInfo[pos - 1][2], function(t) return {t, 0} end),
                                        vehicleNodeOverride = node,
                                        vehicleNodeOverrideList = {node},
                                        vehicleNodeOverrideListCenter = {node + 3}
                                    }
                                    if not terminalTypes[tType] then terminalTypes[tType] = {} end
                                    table.insert(terminalTypes[tType], #result.terminalGroups - 1)
                                    if signNrList[pos - 1] then
                                        local nr = tostring(#result.terminalGroups)
                                        for _, s in ipairs(signNrList[pos - 1].right) do
                                            result.models[s] = func.with(result.models[s], {nr = nr})
                                        end
                                    end
                                end
                            end,
                            function()
                                if (modules[pos + 1] and modules[pos + 1].metadata.isPlatform) then
                                    local node = g.terminalInfo[pos] + 1
                                    local tType = g.terminalInfo[pos + 1][1][3]
                                    result.terminalGroups[#result.terminalGroups + 1] = {
                                        callbacks = {},
                                        terminals = func.seqMap(g.terminalInfo[pos + 1][1], function(t) return {t, 0} end),
                                        vehicleNodeOverride = node,
                                        vehicleNodeOverrideList = {node},
                                        vehicleNodeOverrideListCenter = {node + 3}
                                    }
                                    if not terminalTypes[tType] then terminalTypes[tType] = {} end
                                    table.insert(terminalTypes[tType], #result.terminalGroups - 1)
                                    if signNrList[pos + 1] then
                                        local nr = tostring(#result.terminalGroups)
                                        for _, s in ipairs(signNrList[pos + 1].left) do
                                            result.models[s] = func.with(result.models[s], {nr = nr})
                                        end
                                    end
                                end
                            end
                        }
                        if (pos >= 0) then
                            fn[1]()
                            fn[2]()
                        else
                            fn[2]()
                            fn[1]()
                        end
                    end
                    if m.metadata.isPlatform then
                        if (g.connectors[pos]) then
                            local lanes = pipe.new * {
                                #g.connectors[pos].up > 1 and func.interlace(g.connectors[pos].up) or {},
                                #g.connectors[pos].down > 1 and func.interlace(g.connectors[pos].down) or {}
                            }
                            * pipe.flatten()
                            * pipe.map(function(c) return mus.unitLane(unpack(c)) end)
                            result.models = result.models + lanes
                        end
                    end
                end
            end

            result.stations = {}
            for _, t in pairs(terminalTypes) do
                result.stations[#result.stations + 1] = {
                    terminals = t,
                    tag = #result.stations + 1
                }
            end

            result.models = result.models * pipe.filter(function(m) return (not m.isNrLeft and not m.isNrRight) or m.nr end)
            
            for i, m in ipairs(result.models) do
                if m.nr then
                    result.labelText[i - 1] = {m.nr, m.nr}
                end
            end
            
            local allConnectors = pipe.new
                * g.allPos
                * pipe.filter(function(pos) return g.modules[pos].metadata.isPlatform end)
                * pipe.map(function(pos) return g.connectors[pos] end)
            
            result.models = result.models
                + mus.linkConnectors(allConnectors * pipe.map(pipe.select("up")))
                + mus.linkConnectors(allConnectors * pipe.map(pipe.select("down")))
            
            table.insert(connectors, (allConnectors * pipe.map(pipe.select("up")) + allConnectors * pipe.map(pipe.select("down"))) * pipe.flatten())
        end
        
        local conn = connectionNetwork(connectors)
        result.models = result.models + conn
        
        local allUpConn = connectors * pipe.flatten() * pipe.filter(function(c) return c.up end)
        local entrySide = {}
        result.entryConnectors = func.map(result.entryConnectors, pipe.map(function(c) return coor.xyz(c.x, c.y, c.z) end))
        for i = 1, #result.entryConnectors do
            entrySide[i] = {false, false}
            local dist = {}
            for j = 1, #allUpConn do
                dist[#dist + 1] = pipe.new
                    * func.seq(1, 2)
                    * pipe.map(function(u)
                        local vec = result.entryConnectors[i][u] - allUpConn[j]
                        return {
                            conn = mus.stepLane(result.entryConnectors[i][u], allUpConn[j]),
                            distance = vec:length2(),
                            vec = vec:normalized(),
                            info = {i, u}
                        }
                    end)
                    * pipe.filter(function(c) return #c.conn > 0 end)
                    * pipe.min(function(l, r) return l.distance < r.distance end)
            end
            dist = func.sort(dist, function(l, r) return l.distance < r.distance end)
            if (#dist == 1) then
                result.models = result.models + dist[1].conn 
            elseif (#dist > 1) then
                local vecl = dist[1].vec
                local vecr = dist[2].vec
                if (vecl:cross(vecr).z > 0.5) then
                    result.models = result.models + dist[1].conn + dist[2].conn
                else
                    result.models = result.models + dist[1].conn 
                end
            end
        end
    end
    
    return result
end


return mus
