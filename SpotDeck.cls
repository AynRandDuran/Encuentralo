"Filed out from Dolphin Smalltalk 7"!

Object subclass: #SpotDeck
	instanceVariableNames: 'symbols size name'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SpotDeck guid: (GUID fromString: '{E7FE8300-6E35-4E91-97C8-6127986F399A}')!
SpotDeck comment: ''!
!SpotDeck categoriesForClass!Unclassified! !
!SpotDeck methodsFor!

drawRandom

	^symbols at: (Time microsecondClockValue rem: (self size))!

name

	^name!

name: aString

	name:= aString!

size

	^size!

size: anInt

	size:= anInt!

symbols

	^symbols!

symbols: spotSymbolCollection

	symbols:= spotSymbolCollection! !
!SpotDeck categoriesFor: #drawRandom!public! !
!SpotDeck categoriesFor: #name!public! !
!SpotDeck categoriesFor: #name:!public! !
!SpotDeck categoriesFor: #size!public! !
!SpotDeck categoriesFor: #size:!public! !
!SpotDeck categoriesFor: #symbols!public! !
!SpotDeck categoriesFor: #symbols:!public! !

!SpotDeck class methodsFor!

testDeck
	"Create a test deck"
	|cards|
	cards:= OrderedCollection new.
	cards add: (SpotSymbol for: 'C:\Users\pletts\Pictures\1.bmp').
	cards add: (SpotSymbol for: 'C:\Users\pletts\Pictures\2.bmp').
	cards add: (SpotSymbol for: 'C:\Users\pletts\Pictures\3.bmp').
	cards add: (SpotSymbol for: 'C:\Users\pletts\Pictures\4.bmp').
	cards add: (SpotSymbol for: 'C:\Users\pletts\Pictures\5.bmp').
	cards add: (SpotSymbol for: 'C:\Users\pletts\Pictures\6.bmp').
	cards add: (SpotSymbol for: 'C:\Users\pletts\Pictures\7.bmp').
	cards add: (SpotSymbol for: 'C:\Users\pletts\Pictures\8.bmp').
	cards add: (SpotSymbol for: 'C:\Users\pletts\Pictures\9.bmp').
	cards add: (SpotSymbol for: 'C:\Users\pletts\Pictures\10.bmp').
	cards add: (SpotSymbol for: 'C:\Users\pletts\Pictures\11.bmp').
	cards add: (SpotSymbol for: 'C:\Users\pletts\Pictures\12.bmp').
	cards add: (SpotSymbol for: 'C:\Users\pletts\Pictures\13.bmp').
	cards add: (SpotSymbol for: 'C:\Users\pletts\Pictures\14.bmp').
	cards add: (SpotSymbol for: 'C:\Users\pletts\Pictures\15.bmp').
	^SpotDeck with: cards named: 'testDeck'
!

with: spotSymbolCollection named: aName
	"spotSymbolCollection should be an OrderedCollection"

	|deck|
	deck:= super new.
	deck symbols: spotSymbolCollection.
	deck size: (deck symbols size).
	deck name: aName.
	^deck! !
!SpotDeck class categoriesFor: #testDeck!public! !
!SpotDeck class categoriesFor: #with:named:!public! !

