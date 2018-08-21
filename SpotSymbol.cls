"Filed out from Dolphin Smalltalk 7"!

Object subclass: #SpotSymbol
	instanceVariableNames: 'path image rotation'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SpotSymbol guid: (GUID fromString: '{539218CC-FE26-428B-A214-5CD2B375FB86}')!
SpotSymbol comment: ''!
!SpotSymbol categoriesForClass!Unclassified! !
!SpotSymbol methodsFor!

image

	^image!

image: aGdiplusGraphics

	image:= aGdiplusGraphics!

path

	^path!

path: aPath

	path:= aPath!

rotation

	^rotation!

rotation: anAngle

	rotation:= anAngle! !
!SpotSymbol categoriesFor: #image!public! !
!SpotSymbol categoriesFor: #image:!public! !
!SpotSymbol categoriesFor: #path!public! !
!SpotSymbol categoriesFor: #path:!public! !
!SpotSymbol categoriesFor: #rotation!public! !
!SpotSymbol categoriesFor: #rotation:!public! !

!SpotSymbol class methodsFor!

for: aPath
	"Create a symbol for the Spot It game with an image at aPath, where aPath should be absolute maybe sorta kinda"

	|sym|
	sym:= super new.
	sym path: aPath.
	sym image: (GdiplusGraphics fromImage: (GdiplusImage fromFile: sym path)).
	sym rotation: 0.
	^sym! !
!SpotSymbol class categoriesFor: #for:!public! !

