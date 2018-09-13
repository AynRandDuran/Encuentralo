"Filed out from Dolphin Smalltalk 7"!

Playground subclass: #EncuentraloPlayer
	instanceVariableNames: 'state clearState leftState bothState deck leftCardViews rightCardViews pair leftPositions rightPositions'
	classVariableNames: ''
	poolDictionaries: 'GdiplusConstants Win32Constants'
	classInstanceVariableNames: ''!
EncuentraloPlayer guid: (GUID fromString: '{69BA04F5-0D7B-404A-BA27-45A49F1AE8FA}')!
EncuentraloPlayer comment: ''!
!EncuentraloPlayer categoriesForClass!Unclassified! !
!EncuentraloPlayer methodsFor!

bothState
	^bothState!

bothState: anObject
	bothState := anObject!

clearState
	^clearState!

clearState: anObject
	clearState := anObject!

cycle

	self state updateCanvas!

deck

	^deck!

deck: aDeck

	deck:= aDeck!

initialize
	super initialize.
	!

leftPositions

	^leftPositions!

leftPositions: anOrderedCollection

	leftPositions:= anOrderedCollection!

leftState
	^leftState!

leftState: anObject
	leftState := anObject!

newLeftPositions

	"eventually make sure these don't overlap or go out of bounds"
	self leftPositions: OrderedCollection new.
	1 to: 8 do: [:each |
		self leftPositions
			add: ((Time microsecondClockValue raisedTo: 2) rem: ((self parentView extent x)/2)-200) + 50
				@((Time microsecondClockValue raisedTo: 2) rem: (self parentView extent y - 200)) + 50
	].!

newRightPositions

	"eventually make sure these don't overlap or go out of bounds"
	self rightPositions: OrderedCollection new.
	1 to: 8 do: [:each |
		self rightPositions
			add: (((Time microsecondClockValue raisedTo: 2) rem: ((self parentView extent x)/2)-200) + 50) + (self parentView extent x/2)
				@((Time microsecondClockValue raisedTo: 2) rem: (self parentView extent y - 200)) + 50
	].!

onPaintRequired: aPaintEvent
	"Handler for aPaintEvent. 
	This is called whenever the receiver window needs to be completely or partially redrawn."

	|canvas graphics|
	canvas := aPaintEvent canvas.
	canvas erase.
	"self shapes do: [:each | each drawOn: canvas]."

	canvas
		erase;
		pen: (Pen red width: 4);
		brush: Color gray brush;
		lineFrom: ((self parentView extent x /2) asInteger)@0 to: ((self parentView extent x /2) asInteger)@(self parentView extent y).!

pair

	^pair!

pair: anEncuentraloCardPair

	pair:= anEncuentraloCardPair!

rightPositions

	^rightPositions!

rightPositions: anOrderedCollection

	rightPositions:= anOrderedCollection!

state

	^state!

state: aState

	state:= aState! !
!EncuentraloPlayer categoriesFor: #bothState!accessing!private! !
!EncuentraloPlayer categoriesFor: #bothState:!accessing!private! !
!EncuentraloPlayer categoriesFor: #clearState!accessing!private! !
!EncuentraloPlayer categoriesFor: #clearState:!accessing!private! !
!EncuentraloPlayer categoriesFor: #cycle!public! !
!EncuentraloPlayer categoriesFor: #deck!public! !
!EncuentraloPlayer categoriesFor: #deck:!public! !
!EncuentraloPlayer categoriesFor: #initialize!public! !
!EncuentraloPlayer categoriesFor: #leftPositions!public! !
!EncuentraloPlayer categoriesFor: #leftPositions:!public! !
!EncuentraloPlayer categoriesFor: #leftState!accessing!private! !
!EncuentraloPlayer categoriesFor: #leftState:!accessing!private! !
!EncuentraloPlayer categoriesFor: #newLeftPositions!public! !
!EncuentraloPlayer categoriesFor: #newRightPositions!public! !
!EncuentraloPlayer categoriesFor: #onPaintRequired:!public! !
!EncuentraloPlayer categoriesFor: #pair!public! !
!EncuentraloPlayer categoriesFor: #pair:!public! !
!EncuentraloPlayer categoriesFor: #rightPositions!public! !
!EncuentraloPlayer categoriesFor: #rightPositions:!public! !
!EncuentraloPlayer categoriesFor: #state!public! !
!EncuentraloPlayer categoriesFor: #state:!public! !

!EncuentraloPlayer class methodsFor!

playedWith: anOrderedCollection

	|player cycleButton|
	player:= super new.
	player deck: (SpotDeckGenerator for: anOrderedCollection);
	caption: 'Encuentralo';
	extent: player parentView extent;
	pair: (SpotCardPair drawFrom: player deck limit: 8).
	player newLeftPositions; newRightPositions.

	player clearState: (EncuentraloStateClear onPlayer: player).
	player leftState: (EncuentraloStateLeft onPlayer: player).
	player bothState: (EncuentraloStateBoth onPlayer: player).
	player state: player clearState.

	cycleButton:= PushButton new.
	player addSubView: cycleButton.
	cycleButton
		text: 'Cycle';
		command: #cycle.
	player cycle.
	^player! !
!EncuentraloPlayer class categoriesFor: #playedWith:!public! !

