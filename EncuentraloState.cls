"Filed out from Dolphin Smalltalk 7"!

State subclass: #EncuentraloState
	instanceVariableNames: 'player'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
EncuentraloState guid: (GUID fromString: '{ccbdc51e-b08f-4941-b85a-5b13365a745b}')!
EncuentraloState comment: ''!
!EncuentraloState categoriesForClass!Unclassified! !
!EncuentraloState methodsFor!

player
	^player!

player: anObject
	player := anObject!

randomResize

	|newDim|
	newDim:= (Time microsecondClockValue rem: 50).
	^55+newDim@50+newDim!

updateCanvas

	self subclassResponsibility! !
!EncuentraloState categoriesFor: #player!accessing!private! !
!EncuentraloState categoriesFor: #player:!accessing!private! !
!EncuentraloState categoriesFor: #randomResize!public! !
!EncuentraloState categoriesFor: #updateCanvas!public! !

!EncuentraloState class methodsFor!

onPlayer: encuentraloPlayer

	|state|
	state:= self new.
	state player: encuentraloPlayer.
	^state! !
!EncuentraloState class categoriesFor: #onPlayer:!public! !

