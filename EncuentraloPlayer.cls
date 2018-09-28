"Filed out from Dolphin Smalltalk 7"!

Playground subclass: #EncuentraloPlayer
	instanceVariableNames: 'state clearState leftState bothState deck leftCardViews rightCardViews pair leftPositions rightPositions'
	classVariableNames: ''
	poolDictionaries: 'GdiplusConstants Win32Constants'
	classInstanceVariableNames: ''!
EncuentraloPlayer guid: (GUID fromString: '{69ba04f5-0d7b-404a-ba27-45a49f1ae8fa}')!
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

	|grid point xSplit ySplit xMax yMax rows columns|
	xMax:= self parentView extent x quo: 2.
	yMax:= self parentView extent y -150.
	columns:= xMax quo: 150.
	rows:= yMax quo: 150.
	xSplit:= xMax quo: columns.
	ySplit:= yMax quo: rows.
	grid:= OrderedCollection new.
	1 to: rows do:[:r |
		1 to: columns do: [:c | grid add: (c*(xSplit)-(xSplit))@(r*(ySplit)-(ySplit) + 100)]
	].

	self leftPositions: OrderedCollection new.
	1 to: 8 do: [:each |
		point:= ((Time microsecondClockValue) rem: (grid size)) +1.
		self leftPositions add: (grid at: point).
		grid removeAtIndex: point.
	].
	!

newRightPositions

	|grid point xSplit ySplit xMax yMax rows columns|
	xMax:= self parentView extent x.
	yMax:= self parentView extent y -150.
	columns:= xMax quo: 300.
	rows:= yMax quo: 150.
	xSplit:= xMax quo: columns.
	ySplit:= yMax quo: rows.
	grid:= OrderedCollection new.
	1 to: rows do:[:r |
		1 to: columns do: [:c | grid add: (c*(xSplit) - (xSplit) + xMax quo: 2)@(r*(ySplit)-(ySplit) + 100)]
	].
	self rightPositions: OrderedCollection new.
	1 to: 8 do: [:each |
		point:= ((Time microsecondClockValue) rem: (grid size)) +1.
		self rightPositions
			add: (grid at: point).
		grid removeAtIndex: point.
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

	|player cycleButton deck|
	deck:= (SpotDeckGenerator for: anOrderedCollection).
	deck size < 15
		ifTrue: [^nil].
	player:= super new.
	player deck: deck;
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

