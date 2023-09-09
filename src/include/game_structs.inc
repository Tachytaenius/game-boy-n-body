INCLUDE "include/structs.inc"

	rgbds_structs_version 3.0.1

	struct Particle
	words 1, VelY ; Signed 8.8 fixed
	words 1, PosY ; Unsigned 8.8 fixed
	words 1, VelX ; Signed 8.8 fixed
	words 1, PosX ; Unsigned 8.8 fixed
	bytes 1, Mass ; Unsigned
	end_struct
