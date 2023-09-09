INCLUDE "include/hardware.inc"
INCLUDE "include/game_structs.inc"
INCLUDE "include/constants.inc"

DEF MAX_PARTICLES EQU 10

DEF GRAVITY_Y = 32
DEF GRAVITY_X = 0

SECTION "Main Loop Variables", WRAM0

wPaused::
	ds 1

wGravityStrength::
	ds 1

wParticlesInitialised::
	ds 1
wNumParticles::
	ds 1
wParticles::
	dstructs MAX_PARTICLES, Particle, wParticle

SECTION "Main Loop", ROM0

MainLoop::
	; Wait for VBlank
	halt
	ldh a, [hVBlankFlag]
	and a
	jr z, MainLoop
	xor a
	ldh [hVBlankFlag], a

	call UpdateJoypad

	ld a, [hJoypad.pressed]
	and a, JOY_START_MASK
	jr z, :+
	; Toggle pause
	ld a, [wPaused]
	xor a, 1
	ld [wPaused], a
	; Initialise particles?
	ld a, [wParticlesInitialised]
	and a
	jr nz, :+
	ld a, 1
	ld [wParticlesInitialised], a
	ldh a, [rDIV]
	ld [RandState], a
	ld [RandState + 1], a
	ld [RandState + 2], a
	ld [RandState + 3], a
	call InitialiseParticles
:

	ld a, [wPaused]
	and a
	jp nz, .finishMainLoop

	; Check for no bodies
	ld a, [wNumParticles]
	and a
	jr z, .noBodies

	; Accelerate bodies
	ld a, [wNumParticles]
	ld b, a
	ld hl, wParticles + Particle_VelY
.accelerateBodies
	; Add GRAVITY_Y to PosY
	ld a, [hl]
	add a, LOW(GRAVITY_Y)
	ld [hl+], a
	ld a, [hl]
	adc a, HIGH(GRAVITY_Y)
	ld [hl+], a
	; Move to PosX
	assert Particle_VelY + 4 == Particle_VelX
	inc hl
	inc hl
	; Add GRAVITY_X to PosX
	ld a, [hl]
	add a, LOW(GRAVITY_X)
	ld [hl+], a
	ld a, [hl]
	adc a, HIGH(GRAVITY_X)
	ld [hl+], a
	; Move to next particle by adding to hl
	ld a, (sizeof_Particle + Particle_PosY) - (Particle_PosX + 2)
	add a, l
	ld l, a
	jr nc, :+
	inc h
:
	dec b
	jr nz, .accelerateBodies

	; Move bodies by velocity
	ld a, [wNumParticles]
	ld b, a
	ld hl, wParticles
.moveBodies
	; assume hl is beginning of struct
	assert Particle_VelY == 0 ; hl is particle VelY
REPT 2
	ld a, [hl+]
	ld e, a
	ld a, [hl+]
	ld d, a
	assert Particle_VelY + 2 == Particle_PosY
	assert Particle_VelX + 2 == Particle_PosX
	ld a, [hl]
	add a, e
	ld [hl+], a
	ld a, [hl]
	adc a, d
	ld [hl+], a
	assert Particle_PosY + 2 == Particle_VelX
ENDR
	inc hl
	assert Particle_PosX + 2 + 1 == sizeof_Particle ; is an inc hl sufficient?
	dec b
	jr nz, .moveBodies

.noBodies

	; Clear bodies in Shadow OAM
	ld hl, wShadowOAM + OAMA_TILEID
	ld b, OAM_COUNT
.clearBodiesLoop
	xor a
	ld [hl], a
	; Add sizeof_OAM_ATTRS to hl
	ld a, sizeof_OAM_ATTRS
	add a, l
	ld l, a
	jr nc, :+
	inc h
:
	dec b
	jr nz, .clearBodiesLoop

	; Update body positions in Shadow OAM
	; TODO: Flicker when there are more bodies than drawable sprites
	ld hl, wShadowOAM + OAMA_Y
	ld de, wParticles + Particle_PosY + 1
	ld a, [wNumParticles]
	ld b, a
	inc b
	dec b
	jr z, .skipUpdatingBodySprites ; Skip if there are no particles
.updatingBodySpritesLoop
	; Set sprite Y
	ld a, [de]
	add a, OAM_Y_OFS
	ld [hl+], a
	; Go from Y to X
	ld a, Particle_PosX - Particle_PosY
	add a, e
	ld e, a
	jr nc, :+
	inc d
:
	; Set sprite X
	ld a, [de]
	add a, OAM_X_OFS
	ld [hl+], a
	ld a, TILE_PARTICLE
	ld [hl+], a
	; Prepare for next particle/sprite
	inc hl
	ld a, (sizeof_Particle + Particle_PosY + 1) - (Particle_PosX + 1)
	add a, e
	ld e, a
	jr nc, :+
	inc d
:
	dec b
	jr nz, .updatingBodySpritesLoop
.skipUpdatingBodySprites

.finishMainLoop
	jp MainLoop
