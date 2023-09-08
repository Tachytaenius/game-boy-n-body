MACRO define_tile
	DEF \1 RB
	EXPORT \1
	INCBIN STRCAT("res/tiles/", \2, ".2bpp")
ENDM

SECTION "Tileset graphics", ROM0

TilesetGraphics::
	RSRESET
	define_tile TILE_EMPTY, "empty"
	define_tile TILE_PARTICLE, "particle"
.end::

DEF NUM_TILES RB 0
