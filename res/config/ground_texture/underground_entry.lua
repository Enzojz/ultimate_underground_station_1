local tu = require "texutil"

function data()
return {
	detailTex = tu.makeTextureMipmapRepeat("models/uus/albedo/concrete_tiles2.dds", true, true),
	detailNrmlTex = tu.makeTextureMipmapRepeat("models/uus/normal/concrete_tiles2.dds", true, true, true),
	detailSize = { 1.0, 1.0 },
	priority = 50
}
end
