"Filed out from Dolphin Smalltalk 7"!

EncuentraloState subclass: #EncuentraloStateBoth
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
EncuentraloStateBoth guid: (GUID fromString: '{bf780c8c-3904-40ea-971f-e680820b92ea}')!
EncuentraloStateBoth comment: ''!
!EncuentraloStateBoth categoriesForClass!Unclassified! !
!EncuentraloStateBoth methodsFor!

updateCanvas

	1 to: 8 do: [:n |
		(GdiplusImage fromFile: ((self player pair right symbols at: n) path))
			drawOn: self player canvas
			at: (self player rightPositions at: n)
			extent: self randomResize
	].
	self player pair: (SpotCardPair drawFrom: self player deck limit: 8).
	self player state: self player clearState.! !
!EncuentraloStateBoth categoriesFor: #updateCanvas!public! !

