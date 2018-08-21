"Filed out from Dolphin Smalltalk 7"!

Object subclass: #SpotCard
	instanceVariableNames: 'symbols size match'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SpotCard guid: (GUID fromString: '{F406714C-7D5A-463F-9832-D32335C19732}')!
SpotCard comment: ''!
!SpotCard categoriesForClass!Unclassified! !
!SpotCard methodsFor!

match

	^match!

match: aSpotSymbol
	"The symbol meant to be the single match between two cards"

	match:= aSpotSymbol!

symbols

	^symbols!

symbols: someSpotSymbols

	symbols:= someSpotSymbols! !
!SpotCard categoriesFor: #match!public! !
!SpotCard categoriesFor: #match:!public! !
!SpotCard categoriesFor: #symbols!public! !
!SpotCard categoriesFor: #symbols:!public! !

!SpotCard class methodsFor!

drawFrom: aSpotDeck limit: anInteger
	"Create a card with a number of symbols on it defined by limit"
	|card|
	card:= super new.
	card match: aSpotDeck drawRandom.
	! !
!SpotCard class categoriesFor: #drawFrom:limit:!public! !

