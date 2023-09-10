INCLUDE "include/structs.inc"

	rgbds_structs_version 3.0.1

	struct Particle
	longs 1, VelY ; Signed 24.8 fixed
	longs 1, PosY ; Signed 24.8 fixed
	longs 1, VelX ; Signed 24.8 fixed
	longs 1, PosX ; Signed 24.8 fixed
	bytes 1, Mass ; Unsigned
	end_struct
