"Filed out from Dolphin Smalltalk 7"!

Object subclass: #SpotGame
	instanceVariableNames: 'deck'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SpotGame guid: (GUID fromString: '{D16CF910-D113-40B0-B5CC-C39BFE77028A}')!
SpotGame comment: ''!
!SpotGame categoriesForClass!Unclassified! !
!SpotGame class methodsFor!

with: anOrderedCollection

	|deck|
	deck:= (SpotDeckGenerator with: anOrderedCollection)

! !
!SpotGame class categoriesFor: #with:!public! !

