"Filed out from Dolphin Smalltalk 7"!

EncuentraloState subclass: #EncuentraloStateLeft
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
EncuentraloStateLeft guid: (GUID fromString: '{a03f2060-ba5a-4b45-8d7c-fcff1bcc9c34}')!
EncuentraloStateLeft comment: ''!
!EncuentraloStateLeft categoriesForClass!Unclassified! !
!EncuentraloStateLeft methodsFor!

updateCanvas

	1 to: 8 do: [:n |
		(GdiplusImage fromFile: ((self player pair left symbols at: n) path))
			drawOn: self player canvas
			at: (self player leftPositions at: n)
			extent: self randomResize
	].
	self player state: self player bothState.! !
!EncuentraloStateLeft categoriesFor: #updateCanvas!public! !

