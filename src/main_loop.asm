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
	; For c backwards from b - 1 inclusive to 0 inclusive
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
	assert Particle_VelY == 0
	assert Particle_VelY + 4 == Particle_PosY
	assert Particle_PosY + 4 == Particle_VelX
	assert Particle_VelX + 4 == Particle_PosX
	assert Particle_PosX + 4 == Particle_Mass
	assert Particle_Mass + 1 == sizeof_Particle

	; TODO: Flicker when there are more bodies than drawable sprites

	; Clear bodies in Shadow OAM by moving them all to the y 0
	ld hl, wShadowOAM + OAMA_Y
	ld b, OAM_COUNT
.clearBodiesLoop
	xor a
	ld [hl+], a ; Set y to zero
	inc hl
	ld [hl+], a ; Set tile to zero as well
	inc hl
	dec b
	jr nz, .clearBodiesLoop

	; Update body positions in Shadow OAM
	ld hl, wParticles + Particle_PosY + 1
	ld de, wShadowOAM + OAMA_Y
	ld a, [wNumParticles]
	and a
	ret z ; Return if there are no particles
	ld b, a
.updateBodySpritesLoop

	; Check that PosY is within [0, SCRN_Y) and set sprite y to PosY's second-
	; lowest byte + offset, unsetting it if the rest of PosY shows that it's out of range.
	; We first check the range of the lowest non-fractional byte (and set sprite y to it + offset)
	; Then we check if the higher two bytes are both zero

	ld a, [hl+] ; 2nd lowest byte of PosY
	add OAM_Y_OFS
	ld [de], a ; We'll set this to 0 if it turns out to be out of range
	sub OAM_Y_OFS
	cp SCRN_Y
	jr c, .skipSpriteDeactivationCode1 ; Jump if a >= SCRN_Y

	; Deactivate sprite
	xor a
	ld [de], a ; Set sprite y to 0
	; Set de to next sprite y (add sizeof_OAM_ATTRS)
	ld a, sizeof_OAM_ATTRS
	add a, e
	ld e, a
	jr nc, :+
	inc d
:
	; Set hl to next Particle_PosY + 1
	ld a, sizeof_Particle - 1 ; A whole sizeof_Partilce minus amount traversed
	add a, l
	ld l, a
	jr nc, :+
	inc h
:
	jr .loopCheck
.skipSpriteDeactivationCode1

	; Check higher two bytes
	ld a, [hl+] ; 2nd highest byte of PosY
	ld c, a
	ld a, [hl+] ; Highest byte of PosY
	or c ; Are they both zero?
	jr z, .skipSpriteDeactivationCode2

	; Deactivate sprite
	xor a
	ld [de], a ; Set sprite y to 0
	; Set de to next sprite y (add sizeof_OAM_ATTRS)
	ld a, sizeof_OAM_ATTRS
	add a, e
	ld e, a
	jr nc, :+
	inc d
:
	; Set hl to next Particle_PosY + 1
	ld a, sizeof_Particle - 3 ; A whole sizeof_Partilce minus amount traversed
	add a, l
	ld l, a
	jr nc, :+
	inc h
:
	jr .loopCheck
.skipSpriteDeactivationCode2

	; We're done with PosY

	; Check that PosX is within [0, SCRN_X) and set sprite x to PosX's second-lowest byte
	; + offset, setting sprite y to 0 if the rest of PosX shows that it's out of range.
	; We first check the range of the lowest non-fractional byte (and set sprite x to it + offset)
	; Then we check if the higher two bytes are both zero

	; We were on VelY + 0, so add 5
	ld a, 5
	add a, l
	ld l, a
	jr nc, :+
	inc h
:
	ld a, [hl+] ; Second lowest byte of PosX
	add OAM_X_OFS
	inc de ; To sprite x
	ld [de], a
	sub OAM_X_OFS
	cp SCRN_X
	jr c, .skipSpriteDeactivationCode3

	; Deactivate sprite
	xor a
	dec de ; Back to sprite y
	ld [de], a
	; Set de to next sprite y
	ld a, sizeof_OAM_ATTRS
	add a, e
	ld e, a
	jr nc, :+
	inc d
:
	; Set hl to next Particle_PosY + 1
	ld a, sizeof_Particle - 9 ; A whole sizeof_Partilce minus amount traversed
	add a, l
	ld l, a
	jr nc, :+
	inc h
:
	jr .loopCheck
.skipSpriteDeactivationCode3

	; Check higher two bytes
	ld a, [hl+] ; 2nd highest byte of PosX
	ld c, a
	ld a, [hl+] ; Highest byte of PosX
	or c ; Are they both zero?
	jr z, .skipSpriteDeactivationCode4

	; Deactivate sprite
	xor a
	dec de ; Back to sprite y
	ld [de], a
	; Set de to next sprite y
	ld a, sizeof_OAM_ATTRS
	add a, e
	ld e, a
	jr nc, :+
	inc d
:
	; Set hl to next Particle_PosY + 1
	ld a, sizeof_Particle - 11 ; A whole sizeof_Partilce minus amount traversed
	add a, l
	ld l, a
	jr nc, :+
	inc h
:
	jr .loopCheck
.skipSpriteDeactivationCode4

	; Set tile to visible
	ld a, TILE_PARTICLE
	inc de
	ld [de], a
	inc de
	inc de ; To next sprite y
	; Set hl to next Particle_PosY + 1
	ld a, sizeof_Particle - 11 ; A whole sizeof_Partilce minus amount traversed
	add a, l
	ld l, a
	jr nc, :+
	inc h
:

.loopCheck
	dec b
	jp nz, .updateBodySpritesLoop
	ret
