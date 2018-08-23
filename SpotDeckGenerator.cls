"Filed out from Dolphin Smalltalk 7"!

Object subclass: #SpotDeckGenerator
	instanceVariableNames: 'paths'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SpotDeckGenerator guid: (GUID fromString: '{59CC83A9-492A-4D51-BA09-534D97258D00}')!
SpotDeckGenerator comment: ''!
!SpotDeckGenerator categoriesForClass!Unclassified! !
!SpotDeckGenerator class methodsFor!

for: aPathCollection

	|workingDeck|
	workingDeck:= OrderedCollection new.
	aPathCollection do: [:each| 
		File forAll: '*.bmp' in: each do: [:bmp | workingDeck add: (SpotSymbol for: bmp path)]
	].
	^SpotDeck with: workingDeck named: 'A generated deck'! !
!SpotDeckGenerator class categoriesFor: #for:!public! !

