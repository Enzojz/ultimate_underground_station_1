local tu = require "texutil"

function data()
return {
	detailTex = tu.makeTextureMipmapRepeat("streets/sidewalk_new_0_border_inner.tga", true, true),
	detailNrmlTex = tu.makeTextureMipmapRepeat("streets/sidewalk_new_0_border_inner_nrml.tga", true, true, true),
	detailSize = { 1.0, 1.0 },
	priority = 90
}
end
