local func = require "entry/func"
local pipe = require "entry/pipe"
local musm = {}

musm.infi = 1e8
musm.slopeList = 
{2.5, 5, 7.5, 10, 12.5, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 90, 100}
musm.rList = {15, 12, 10, 8, 7.5, 6, 5, 4, 3.5, 3, 2.5, 2, 1.75, 1.5, 1.25, 1, 0.9, 0.85, 0.8, 0.75, 0.7, 0.666, 0.633, 0.6, 0.566, 0.533, 0.5, 0.475, 0.45, 0.425, 0.4, 0.375, 0.35, 0.325, 0.3, 0.28, 0.26, 0.24, 0.22, 0.2, 0.18, 0.16, 0.14, 0.12, 0.1, 0.09, 0.08, 0.07, 0.06, 0.05}
musm.hPlatformList = {200, 280, 380, 550, 680, 760, 915, 960, 1100, 1219, 1250, 1380}
musm.trackLengths = {40, 60, 80, 100, 140, 160, 200, 240, 320, 400, 480, 500, 550, 850, 1050}
musm.trackNumberList = {1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20}
musm.degree = func.seq(0, 270)

musm.rList = pipe.new * musm.rList * pipe.map(pipe.neg()) * pipe.rev() / musm.infi + musm.rList

musm.slopeList = pipe.new * musm.slopeList * pipe.map(pipe.neg()) * pipe.rev() / 0 + musm.slopeList

musm.platformWidthList = {
    "station/rail/mus_platform_5m.module",
    "station/rail/mus_platform_10m.module",
    "station/rail/mus_platform_15m.module",
    "station/rail/mus_platform_20m.module"
}

musm.trackTypeList = {
    "station/rail/mus_track_std.module",
    "station/rail/mus_track_std_catenary.module",
    "station/rail/mus_track_hs.module",
    "station/rail/mus_track_hs_catenary.module"
}
return musm
