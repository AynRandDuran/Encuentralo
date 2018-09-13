"Filed out from Dolphin Smalltalk 7"!

EncuentraloState subclass: #EncuentraloStateBoth
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
EncuentraloStateBoth guid: (GUID fromString: '{BF780C8C-3904-40EA-971F-E680820B92EA}')!
EncuentraloStateBoth comment: ''!
!EncuentraloStateBoth categoriesForClass!Unclassified! !
!EncuentraloStateBoth methodsFor!

updateCanvas

	|graphics|
	graphics:= OrderedCollection new.
	1 to: 8 do: [:n |
		graphics add: (GdiplusGraphics fromCanvas: self player canvas).
		graphics last
			drawImage: ((self player pair right symbols at: n) image)
			at: (self player rightPositions at: n)
			extent: self randomResize
			from: 0@0
			extent: 100@100].

	self player pair: (SpotCardPair drawFrom: self player deck limit: 8).
	self player state: self player clearState.! !
!EncuentraloStateBoth categoriesFor: #updateCanvas!public! !

