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

wParticleAAddress:
	ds 2
wParticleBAddress:
	ds 2

SECTION "Main Loop", ROM0

MainLoop::
	; Wait for VBlank
	halt
	nop
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
:

	ld a, [wPaused]
	and a
	jp nz, MainLoop

	; Initialise particles?
	ld a, [wParticlesInitialised]
	and a
	call z, InitialiseParticles

	; Step simulation!
	call AccelerateBodies
	call MoveBodiesByVelocity

	; "Draw"
	call UpdateSprites

	jp MainLoop

AccelerateBodies:
	; Accelerate bodies
	ld a, [wNumParticles]
	cp 2
	ret c ; Not enough bodies (need at least 2)
	; For b backwards from len - 1 inclusive to 1 inclusive
	ld b, a
	dec b
.outerLoop
	; For c backwards from i - 1 inclusive to 0 inclusive
	ld c, b
	dec c
.innerLoop
	; b: particle A's index, c: particle B's index
	push bc

	; Get and apply forces
	
	ld a, b ; b (particle A) has to go first as it gets destroyed
	call GetParticleAInHl
	ld a, l
	ld [wParticleAAddress], a
	ld a, h
	ld [wParticleAAddress + 1], a

	ld a, c
	call GetParticleAInHl
	ld a, l
	ld [wParticleBAddress], a
	ld a, h
	ld [wParticleBAddress + 1], a

	; TODO

	pop bc
	; Has c finished counting down?
	dec c
	ld a, c
	cp -1 ; Check for underflow
	jr nz, .innerLoop
	; Has b finished counting down?
	dec b
	jr nz, .outerLoop
	ret

; Destroys af b
GetParticleAInHl:
	ld hl, wParticles
	and a
	ret z ; Return if index 0
	ld b, a
.loop
	ld a, sizeof_Particle
	add a, l
	ld l, a
	jr nc, :+
	inc h
:
	dec b
	jr nz, .loop
	ret

MoveBodiesByVelocity:
	; Move bodies by velocity
	ld a, [wNumParticles]
	and a
	ret z
	ld b, a
	ld hl, wParticles
.loop
	; assume hl is beginning of struct
	assert Particle_VelY == 0 ; hl is particle VelY
	assert Particle_VelY + 4 == Particle_PosY ; Are positions right after velocities?
	assert Particle_VelX + 4 == Particle_PosX
	assert Particle_PosY + 4 == Particle_VelX ; Is X after Y?
	ld e, l
	ld d, h
	; Move hl ahead of de
	ld a, Particle_PosY - Particle_VelY
	add a, l
	ld l, a
	jr nc, :+
	inc h
:
	; hl: PosY, de: VelY
	; PosY += VelY
	; First byte
	ld a, [de]
	add a, [hl]
	ld [hl+], a
	inc de
	; Next 3 bytes consider carry
REPT 3
	ld a, [de]
	adc a, [hl]
	ld [hl+], a
	inc de
ENDR

	; Move hl and de ahead to next struct fields
	ld a, Particle_VelX - Particle_PosY
	add a, e
	ld e, a
	jr nc, :+
	inc d
:
	ld a, Particle_PosX - Particle_VelX
	add a, l
	ld l, a
	jr nc, :+
	inc h
:

	; hl: PosY, de: VelY
	; PosX += VelX
	; First byte
	ld a, [de]
	add a, [hl]
	ld [hl+], a
	inc de
	; Next 3 bytes consider carry
REPT 2
	ld a, [de]
	adc a, [hl]
	ld [hl+], a
	inc de
ENDR
	ld a, [de]
	adc a, [hl]
	ld [hl+], a
	; no inc de

	inc hl
	assert Particle_PosX + 4 + 1 == sizeof_Particle ; is an inc hl sufficient?
	dec b
	jr nz, .loop
	ret

UpdateSprites:
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
	ret z ; Return if there are no particles
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
	ret
