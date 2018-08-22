"Filed out from Dolphin Smalltalk 7"!

Object subclass: #SpotCard
	instanceVariableNames: 'symbols size deck'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SpotCard guid: (GUID fromString: '{F406714C-7D5A-463F-9832-D32335C19732}')!
SpotCard comment: ''!
!SpotCard categoriesForClass!Unclassified! !
!SpotCard methodsFor!

deck

	^deck!

deck: aSpotDeck

	deck:= aSpotDeck!

size

	^size!

size: anInt

	size:= anInt!

symbols

	^symbols!

symbols: someSpotSymbols

	symbols:= someSpotSymbols! !
!SpotCard categoriesFor: #deck!public! !
!SpotCard categoriesFor: #deck:!public! !
!SpotCard categoriesFor: #size!public! !
!SpotCard categoriesFor: #size:!public! !
!SpotCard categoriesFor: #symbols!public! !
!SpotCard categoriesFor: #symbols:!public! !

!SpotCard class methodsFor!

drawFrom: aSpotDeck limit: anInteger
	"Create a card with a number of empty symbol slots on it defined by limit"

	|card|
	card:= super new.
	card size: anInteger.
	card deck: aSpotDeck.
	card symbols: OrderedCollection new.
	^card
	! !
!SpotCard class categoriesFor: #drawFrom:limit:!public! !

