function data()
	local t = { }

	t.name = _("Mock tracks")
	t.desc = _("Mock tracks for underground station")

	t.yearFrom = 1800
	t.yearTo = 1800

	t.shapeWidth = 4.0
	t.shapeStep = 4.0
	t.shapeSleeperStep = 8.0 / 12.0

	t.ballastHeight = .3
	t.ballastCutOff = .1

	t.sleeperBase = t.ballastHeight
	t.sleeperLength = .26
	t.sleeperWidth = 2.6
	t.sleeperHeight = .08
	t.sleeperCutOff = .02

	t.railTrackWidth = 1.435
	t.railBase = t.sleeperBase + t.sleeperHeight
	t.railHeight = .15
	t.railWidth = .07
	t.railCutOff = .02

	t.catenaryBase = 5.917 + t.railBase + t.railHeight
	t.catenaryHeight = 1.35

	t.trackDistance = 5.0

	t.speedLimit = 5.0 / 3.6
	t.speedCoeffs = { .85, 30.0, .6 }		-- curve speed limit = a * (radius + b) ^ c

	t.ballastMaterial = "track/ballast.mtl"
	t.sleeperMaterial = "track/transparent.mtl"
	t.railMaterial = "track/transparent.mtl"
	t.catenaryMaterial = "track/catenary.mtl"
	t.trackMaterial = "track/transparent.mtl"
	t.tunnelWallMaterial = "track/transparent.mtl"
	t.tunnelHullMaterial = "track/transparent.mtl"

	t.catenaryPoleModel = "uus/void.mdl"
	t.catenaryMultiPoleModel = "uus/void.mdl"
	t.catenaryMultiGirderModel = "uus/void.mdl"
	t.catenaryMultiInnerPoleModel = "uus/void.mdl"

	t.bumperModel = "uus/void.mdl"
	t.switchSignalModel = "railroad/switch_box.mdl"

	t.fillGroundTex = "ballast_fill"
	t.borderGroundTex = "ballast"

	t.cost = 0.0

	return t
end
