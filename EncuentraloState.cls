"Filed out from Dolphin Smalltalk 7"!

State subclass: #EncuentraloState
	instanceVariableNames: 'player'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
EncuentraloState guid: (GUID fromString: '{CCBDC51E-B08F-4941-B85A-5B13365A745B}')!
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
	^50+newDim@50+newDim!

updateCanvas

	self subclassResponsibility! !
!EncuentraloState categoriesFor: #player!accessing!private! !
!EncuentraloState categoriesFor: #player:!accessing!private! !
!EncuentraloState categoriesFor: #randomResize!public! !
!EncuentraloState categoriesFor: #updateCanvas!public! !

!EncuentraloState class methodsFor!

onPlayer: encuentraloPlayer

	|state|
	state:= super new.
	state player: encuentraloPlayer.
	^state! !
!EncuentraloState class categoriesFor: #onPlayer:!public! !

