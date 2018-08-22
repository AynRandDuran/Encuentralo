"Filed out from Dolphin Smalltalk 7"!

Object subclass: #SpotCardPair
	instanceVariableNames: 'match left right blacklist'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SpotCardPair guid: (GUID fromString: '{CB6FB36C-02A0-4A7C-93EE-D84F06DCC9A4}')!
SpotCardPair comment: ''!
!SpotCardPair categoriesForClass!Unclassified! !
!SpotCardPair methodsFor!

blacklist

	^blacklist!

blacklist: anOrderedCollection
	"When populating a left card, we need to blacklist its symbols so the right card is unique"

	blacklist:= anOrderedCollection!

left

	^left!

left: aSpotCard

	left:= aSpotCard!

match

	^match!

match: aSpotSymbol

	match:= aSpotSymbol!

populatePair

	|safeDeck|
	self blacklist add: self match.
	safeDeck:= (SpotDeck with: (self blacklist symmetricDifference: self left deck symbols ) asOrderedCollection named: 'dumb name').
	1 to: left size - 1 do: [:each | 
		self blacklist add: safeDeck drawRandom.
		safeDeck symbols remove: (self blacklist last).
		].

	self left symbols addAll: self blacklist.
	self right symbols add: self match.

	1 to: left size - 1 do: [:each |
		self right symbols add: safeDeck drawRandom.
		safeDeck symbols remove: (self right symbols last).
		].!

right

	^right!

right: aSpotCard

	right:= aSpotCard! !
!SpotCardPair categoriesFor: #blacklist!public! !
!SpotCardPair categoriesFor: #blacklist:!public! !
!SpotCardPair categoriesFor: #left!public! !
!SpotCardPair categoriesFor: #left:!public! !
!SpotCardPair categoriesFor: #match!public! !
!SpotCardPair categoriesFor: #match:!public! !
!SpotCardPair categoriesFor: #populatePair!public! !
!SpotCardPair categoriesFor: #right!public! !
!SpotCardPair categoriesFor: #right:!public! !

!SpotCardPair class methodsFor!

drawFrom: aSpotDeck limit: anInteger

	|pair|
	pair:= super new.
	pair left: (SpotCard drawFrom: aSpotDeck limit: anInteger).
	pair right: (SpotCard drawFrom: aSpotDeck limit: anInteger).
	pair match: aSpotDeck drawRandom.
	pair blacklist: OrderedCollection new.
	pair populatePair.
	pair right symbols: (pair right symbols randomizeUsing: Random new).
	pair left symbols: (pair left symbols randomizeUsing: Random new).
	^pair! !
!SpotCardPair class categoriesFor: #drawFrom:limit:!public! !

