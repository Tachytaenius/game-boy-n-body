INCLUDE "include/hardware.inc"

DEF STARTING_BODIES EQU 10
DEF GRAVITY_STRENGTH EQU 1
DEF START_POS_PADDING EQU 32

SECTION "Game Init", ROM0

GameInit::
	; Load tileset
	ld bc, TilesetGraphics.end - TilesetGraphics
	ld hl, TilesetGraphics
	ld de, _VRAM
	call CopyBytes

	; Clear background
	ld bc, SCRN_VX_B * SCRN_Y_B ; Not SCRN_VY_B (for speed)
	ld hl, _SCRN0
	xor a
	call FillBytes

	; Start paused
	ld a, 1
	ld [wPaused], a

	ld a, GRAVITY_STRENGTH
	ld [wGravityStrength], a

	xor a
	ld [wNumParticles], a
	ld [wParticlesInitialised], a

	ret

; This is delayed
InitialiseParticles::
	ldh a, [rDIV]
	ld [RandState], a
	ld [RandState + 1], a
	ld [RandState + 2], a
	ld [RandState + 3], a

	ld a, STARTING_BODIES
	ld [wNumParticles], a
	ldh [hLoopCounter], a
	inc a
	dec a
	ret z
	ld hl, wParticles

.generateParticle
	; VelY
	push hl
	call Rand
	pop hl
	ld a, d
	ld [hl+], a
	; Use e as a roll to determine whether to fill the rest of the bytes with 0 or -1
	ld a, e
	cp a, 128
	ld a, 0 ; no xor a, need to preserve carry
	jr c, :+
	ld a, -1
:
	ld [hl+], a
	ld [hl+], a
	ld [hl+], a

	; PosY
	xor a
	ld [hl+], a
	push hl
	ld h, START_POS_PADDING
	ld l, SCRN_Y - START_POS_PADDING * 2
	call RandRange
	pop hl
	ld [hl+], a
	xor a
	ld [hl+], a
	ld [hl+], a

	; VelX
	push hl
	call Rand
	pop hl
	ld a, d
	ld [hl+], a
	; Use e as a roll to determine whether to fill the rest of the bytes with 0 or -1
	ld a, e
	cp a, 128
	ld a, 0 ; no xor a, need to preserve carry
	jr c, :+
	ld a, -1
:
	ld [hl+], a
	ld [hl+], a
	ld [hl+], a

	; PosX
	xor a
	ld [hl+], a
	push hl
	ld h, START_POS_PADDING
	ld l, SCRN_X - START_POS_PADDING * 2
	call RandRange
	pop hl
	ld [hl+], a
	xor a
	ld [hl+], a
	ld [hl+], a

	; Mass
	ld a, 1
	ld [hl+], a

	ldh a, [hLoopCounter]
	dec a
	ldh [hLoopCounter], a
	jr nz, .generateParticle

	ld a, 1
	ld [wParticlesInitialised], a
	ret
