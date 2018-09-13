"Filed out from Dolphin Smalltalk 7"!

EncuentraloState subclass: #EncuentraloStateLeft
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
EncuentraloStateLeft guid: (GUID fromString: '{A03F2060-BA5A-4B45-8D7C-FCFF1BCC9C34}')!
EncuentraloStateLeft comment: ''!
!EncuentraloStateLeft categoriesForClass!Unclassified! !
!EncuentraloStateLeft methodsFor!

updateCanvas

	|graphics|
	graphics:= OrderedCollection new.
	1 to: 8 do: [:n |
		graphics add: (GdiplusGraphics fromCanvas: self player canvas).
		graphics last
			drawImage: ((self player pair left symbols at: n) image)
			at: (self player leftPositions at: n)
			extent: self randomResize
			from: 0@0
			extent: 100@100].

	self player state: self player bothState.! !
!EncuentraloStateLeft categoriesFor: #updateCanvas!public! !

