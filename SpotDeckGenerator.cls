"Filed out from Dolphin Smalltalk 7"!

Object subclass: #SpotDeckGenerator
	instanceVariableNames: 'paths'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SpotDeckGenerator guid: (GUID fromString: '{59cc83a9-492a-4d51-ba09-534d97258d00}')!
SpotDeckGenerator comment: ''!
!SpotDeckGenerator categoriesForClass!Unclassified! !
!SpotDeckGenerator class methodsFor!

for: aPathCollection

	|workingDeck|
	workingDeck:= OrderedCollection new.
	aPathCollection do: [:each| 
		File forAll: '*.bmp' in: each do: [:bmp | workingDeck add: (SpotSymbol for: bmp path)].
		File forAll: '*.jpg' in: each do: [:bmp | workingDeck add: (SpotSymbol for: bmp path)].
		File forAll: '*.png' in: each do: [:bmp | workingDeck add: (SpotSymbol for: bmp path)].
	].
	^SpotDeck with: workingDeck named: 'A generated deck'! !
!SpotDeckGenerator class categoriesFor: #for:!public! !

