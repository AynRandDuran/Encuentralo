| package |
package := Package name: 'Encuentralo'.
package paxVersion: 1;
	basicComment: ''.

package imageStripperBytes: (ByteArray fromBase64String: 'IVNUQiA0IEYPDQAEAAAASW1hZ2VTdHJpcHBlcgAAAABSAAAACwAAAEVuY3VlbnRyYWxvUgAAABQA
AABFbmN1ZW50cmFsby10ZXN0LmV4ZZoAAABSAAAACwAAAEVuY3VlbnRyYWxvUgAAABkAAABFbmN1
ZW50cmFsb1Nlc3Npb25NYW5hZ2Vy77wlAAAAAAAGAw8AVmVyc2lvblJlc291cmNlBgEQAFZTX0ZJ
WEVERklMRUlORk9yAAAANAAAAL0E7/4AAAEAAAABAAEAAAAAAAEAAQAAAD8AAAAAAAAABAAAAAIA
AAAAAAAAAAAAAAAAAADqAAAA8AAAAGIAAAACAAAAUgAAAAgAAAAwNDA5MDRiMOoAAADwAAAAYgAA
AA4AAABSAAAADgAAAFByb2R1Y3RWZXJzaW9uNgALAFV0ZjE2U3RyaW5nFAAAADEALAAgADAALAAg
ADAALAAgADEAUgAAABAAAABPcmlnaW5hbEZpbGVuYW1lUgAAABQAAABFbmN1ZW50cmFsby10ZXN0
LmV4ZVIAAAALAAAAUHJvZHVjdE5hbWWyAgAANAAAAEEAIABEAG8AbABwAGgAaQBuACAAVABvAEcA
bwAgAEEAcABwAGwAaQBjAGEAdABpAG8AbgBSAAAADgAAAExlZ2FsQ29weXJpZ2h0sgIAAFYAAABQ
AG8AcgB0AGkAbwBuAHMAIABDAG8AcAB5AHIAaQBnAGgAdAAgAKkAIABPAGIAagBlAGMAdAAgAEEA
cgB0AHMAIAAxADkAOQA3AC0AMgAwADEANgAuAFIAAAAPAAAARmlsZURlc2NyaXB0aW9usgIAADAA
AABEAG8AbABwAGgAaQBuACAAVABvAEcAbwAgAEEAcABwAGwAaQBjAGEAdABpAG8AbgBSAAAACwAA
AEZpbGVWZXJzaW9usgIAABQAAAAxACwAIAAwACwAIAAwACwAIAAxAFIAAAAIAAAAQ29tbWVudHOy
AgAAOAAAAFAAbwB3AGUAcgBlAGQAIABiAHkAIABEAG8AbABwAGgAaQBuACAAUwBtAGEAbABsAHQA
YQBsAGsAygAAANAAAABiAAAAAQAAAAYCCgBEV09SREFycmF5cgAAAAQAAAAJBLAEAwAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA').

package classNames
	add: #DirectoryCollectionPresenter;
	add: #Encuentralo;
	add: #EncuentraloAbout;
	add: #EncuentraloPlayer;
	add: #EncuentraloSessionManager;
	add: #EncuentraloState;
	add: #EncuentraloStateBoth;
	add: #EncuentraloStateClear;
	add: #EncuentraloStateLeft;
	add: #SpotCard;
	add: #SpotCardPair;
	add: #SpotDeck;
	add: #SpotDeckGenerator;
	add: #SpotGame;
	add: #SpotSymbol;
	add: #State;
	yourself.

package methodNames
	add: #Shell -> #aboutEncuentralo;
	add: #Shell -> #openEncuentraloHome;
	add: #Shell -> #playEncuentralo;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Dolphin\Core\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Dolphin\Core\Object Arts\Dolphin\MVP\Presenters\Collection\Dolphin Collection Presenters';
	add: '..\Dolphin\Core\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	add: '..\Dolphin\Core\Object Arts\Dolphin\System\Random\Dolphin Random Stream';
	add: '..\Dolphin\Core\Object Arts\Dolphin\MVP\Gdiplus\Gdiplus';
	add: '..\Dolphin\Core\Object Arts\Samples\MVP\Playground\Playground';
	add: '..\Dolphin\Core\Object Arts\Dolphin\ActiveX\Shell\Windows Shell';
	yourself).

package!

"Class Definitions"!

Object subclass: #SpotCard
	instanceVariableNames: 'symbols size deck'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #SpotCardPair
	instanceVariableNames: 'match left right blacklist'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #SpotDeck
	instanceVariableNames: 'symbols size name'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #SpotDeckGenerator
	instanceVariableNames: 'paths'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #SpotGame
	instanceVariableNames: 'deck'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #SpotSymbol
	instanceVariableNames: 'path image rotation'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #State
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #Encuentralo
	instanceVariableNames: 'deckListPresenter shell deckList startButton'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #EncuentraloAbout
	instanceVariableNames: 'shell linkButton'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
CollectionPresenter subclass: #DirectoryCollectionPresenter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RuntimeSessionManager subclass: #EncuentraloSessionManager
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
State subclass: #EncuentraloState
	instanceVariableNames: 'player'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
EncuentraloState subclass: #EncuentraloStateBoth
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
EncuentraloState subclass: #EncuentraloStateClear
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
EncuentraloState subclass: #EncuentraloStateLeft
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Playground subclass: #EncuentraloPlayer
	instanceVariableNames: 'state clearState leftState bothState deck leftCardViews rightCardViews pair leftPositions rightPositions'
	classVariableNames: ''
	poolDictionaries: 'GdiplusConstants Win32Constants'
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!Shell methodsFor!

aboutEncuentralo

	EncuentraloAbout new!

openEncuentraloHome

	ShellLibrary default shellOpen: 'https://github.com/AynRandDuran/Encuentralo#Encuentralo'.
	self exit.!

playEncuentralo

	EncuentraloPlayer playedWith: self subPresenters first model value! !
!Shell categoriesFor: #aboutEncuentralo!commands!public! !
!Shell categoriesFor: #openEncuentraloHome!commands!public! !
!Shell categoriesFor: #playEncuentralo!commands!public! !

"End of package definition"!

"Source Globals"!

"Classes"!

SpotCard guid: (GUID fromString: '{f406714c-7d5a-463f-9832-d32335c19732}')!
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

SpotCardPair guid: (GUID fromString: '{cb6fb36c-02a0-4a7c-93ee-d84f06dcc9a4}')!
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

SpotDeck guid: (GUID fromString: '{e7fe8300-6e35-4e91-97c8-6127986f399a}')!
SpotDeck comment: ''!
!SpotDeck categoriesForClass!Unclassified! !
!SpotDeck methodsFor!

drawRandom

	^symbols at: (Time microsecondClockValue rem: (self symbols size)) + 1!

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

SpotGame guid: (GUID fromString: '{d16cf910-d113-40b0-b5cc-c39bfe77028a}')!
SpotGame comment: ''!
!SpotGame categoriesForClass!Unclassified! !
!SpotGame class methodsFor!

with: anOrderedCollection

	|deck|
	deck:= (SpotDeckGenerator with: anOrderedCollection)

! !
!SpotGame class categoriesFor: #with:!public! !

SpotSymbol guid: (GUID fromString: '{539218cc-fe26-428b-a214-5cd2b375fb86}')!
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

State guid: (GUID fromString: '{34effcfa-13ef-4f8d-a4a7-30273515b55e}')!
State comment: ''!
!State categoriesForClass!Unclassified! !
Encuentralo guid: (GUID fromString: '{8246c5ae-a0c7-4fd5-8c21-a24c5bfedddb}')!
Encuentralo comment: '"Smalltalk implementation of the card game Spot It. Please don''t sue me"'!
!Encuentralo categoriesForClass!Unclassified! !
!Encuentralo methodsFor!

createComponents

	super createComponents.
	shell:= Shell create: Shell defaultView.
	shell
		extent: 500@300;
		caption: 'Encuentralo'.
	deckListPresenter:= DirectoryCollectionPresenter createIn: shell on: OrderedCollection new.
	shell add: deckListPresenter.
	deckListPresenter position: 10@75.
	deckListPresenter extent: 350@175.
	self drawMainMenu.
	shell show.
	!

createSchematicWiring

	super createSchematicWiring.!

deckListPresenter

	^deckListPresenter!

drawMainMenu

	|banner aboutButton|
	banner:= StaticText new.
	shell view addSubView: banner.
	banner
		text: 'ENCUENTRALO';
		extent: 500@50;
		alignment: #center;
		backcolor: Color white;
		font: (Font name: 'Terminal' pointSize: 32).

	startButton:= PushButton new.
	aboutButton:= PushButton new.
	shell view addSubView: aboutButton.
	shell view addSubView: startButton.
	startButton
		text: 'Play';
		position: 370 @ 75;
		extent: 100 @ 25;
		command: #playEncuentralo. "um yes, Object Arts? uh yeah why the fuck is this a super send"
	aboutButton
		text: 'About';
		position: 370 @ 105;
		extent: 100 @ 25;
		command: #aboutEncuentralo.
!

shell

	^shell! !
!Encuentralo categoriesFor: #createComponents!public! !
!Encuentralo categoriesFor: #createSchematicWiring!public! !
!Encuentralo categoriesFor: #deckListPresenter!public! !
!Encuentralo categoriesFor: #drawMainMenu!public! !
!Encuentralo categoriesFor: #shell!public! !

!Encuentralo class methodsFor!

new

	^super new! !
!Encuentralo class categoriesFor: #new!public! !

EncuentraloAbout guid: (GUID fromString: '{583a966f-7f29-43b7-9309-3a7828563b8e}')!
EncuentraloAbout comment: ''!
!EncuentraloAbout categoriesForClass!MVP-Views! !
!EncuentraloAbout methodsFor!

createComponents

	super createComponents.
	shell:= Shell create: Shell defaultView.
	shell
		extent: 400@230;
		caption: 'Encuentralo'.
	self drawAbout.
	shell show.!

drawAbout

	|text homeButton text2|

	text:= StaticText new.
	text2:= StaticText new.
	shell view addSubView: text.
	text
		text: ('Encuentralo is an adaptation of the Asmodee game Spot It written and maintained by Patrick Letts in Dolphin Smalltalk.%0A%0AMore information and new releases can be found at:' unescapePercents);
		extent: 300@90;
		position: 25@25;
		alignment: #left;
		backcolor: Color white;
		font: (Font name: 'Terminal' pointSize: 10).

	homeButton:= PushButton new.
	shell view addSubView: homeButton.
	homeButton
		text: 'Encuentralo Home';
		backcolor: Color green;
		extent: 100 @ 40;
		position: 130@115;
		command: #openEncuentraloHome.

	shell view addSubView: text2.
	text2
		text: 'For questions and concerns, contact lpj1496@gmail.com';
		extent: 400@40;
		position: 25@160;
		alignment: #left;
		backcolor: Color white;
		font: (Font name: 'Terminal' pointSize: 10).!

initialize

	super initialize.! !
!EncuentraloAbout categoriesFor: #createComponents!public! !
!EncuentraloAbout categoriesFor: #drawAbout!public! !
!EncuentraloAbout categoriesFor: #initialize!public! !

!EncuentraloAbout class methodsFor!

new
	"Create and show a new instance of the receiver. Answer the new instance"

	^super new show caption: 'About Encuentralo'.! !
!EncuentraloAbout class categoriesFor: #new!public! !

DirectoryCollectionPresenter guid: (GUID fromString: '{316d5ea6-5a4d-49f4-9378-7981af2a7463}')!
DirectoryCollectionPresenter comment: ''!
!DirectoryCollectionPresenter categoriesForClass!Unclassified! !
!DirectoryCollectionPresenter methodsFor!

defaultAddItem
	"Private - Prompts to add an item to the receiver. Used if no addItemBlock has been specified"

	^BrowseFolderDialog showModal! !
!DirectoryCollectionPresenter categoriesFor: #defaultAddItem!private! !

EncuentraloSessionManager guid: (GUID fromString: '{1943fd47-167e-479d-abcc-8ad112815b0e}')!
EncuentraloSessionManager comment: ''!
!EncuentraloSessionManager categoriesForClass!Unclassified! !
!EncuentraloSessionManager methodsFor!

main

	Encuentralo new! !
!EncuentraloSessionManager categoriesFor: #main!public! !

EncuentraloState guid: (GUID fromString: '{ccbdc51e-b08f-4941-b85a-5b13365a745b}')!
EncuentraloState comment: ''!
!EncuentraloState categoriesForClass!Unclassified! !
!EncuentraloState methodsFor!

player
	^player!

player: anObject
	player := anObject!

randomResize

	|newDim|
	newDim:= (Time microsecondClockValue rem: 50).
	^55+newDim@50+newDim!

updateCanvas

	self subclassResponsibility! !
!EncuentraloState categoriesFor: #player!accessing!private! !
!EncuentraloState categoriesFor: #player:!accessing!private! !
!EncuentraloState categoriesFor: #randomResize!public! !
!EncuentraloState categoriesFor: #updateCanvas!public! !

!EncuentraloState class methodsFor!

onPlayer: encuentraloPlayer

	|state|
	state:= self new.
	state player: encuentraloPlayer.
	^state! !
!EncuentraloState class categoriesFor: #onPlayer:!public! !

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

EncuentraloStateClear guid: (GUID fromString: '{9beb9355-ff7c-404f-a32a-9c1dc96d34e2}')!
EncuentraloStateClear comment: ''!
!EncuentraloStateClear categoriesForClass!Unclassified! !
!EncuentraloStateClear methodsFor!

updateCanvas

	self player canvas
		erase;
		pen: (Pen red width: 4);
		brush: Color gray brush;
		lineFrom: ((self player parentView extent x /2) asInteger)@0 to: ((self player parentView extent x /2) asInteger)@(self player parentView extent y).

	self player state: self player leftState.
	self player
		newRightPositions;
		newLeftPositions.
		! !
!EncuentraloStateClear categoriesFor: #updateCanvas!public! !

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

EncuentraloPlayer guid: (GUID fromString: '{69ba04f5-0d7b-404a-ba27-45a49f1ae8fa}')!
EncuentraloPlayer comment: ''!
!EncuentraloPlayer categoriesForClass!Unclassified! !
!EncuentraloPlayer methodsFor!

bothState
	^bothState!

bothState: anObject
	bothState := anObject!

clearState
	^clearState!

clearState: anObject
	clearState := anObject!

cycle

	self state updateCanvas!

deck

	^deck!

deck: aDeck

	deck:= aDeck!

initialize
	super initialize.
	!

leftPositions

	^leftPositions!

leftPositions: anOrderedCollection

	leftPositions:= anOrderedCollection!

leftState
	^leftState!

leftState: anObject
	leftState := anObject!

newLeftPositions

	|grid point xSplit ySplit xMax yMax rows columns|
	xMax:= self parentView extent x quo: 2.
	yMax:= self parentView extent y -150.
	columns:= xMax quo: 150.
	rows:= yMax quo: 150.
	xSplit:= xMax quo: columns.
	ySplit:= yMax quo: rows.
	grid:= OrderedCollection new.
	1 to: rows do:[:r |
		1 to: columns do: [:c | grid add: (c*(xSplit)-(xSplit))@(r*(ySplit)-(ySplit) + 100)]
	].

	self leftPositions: OrderedCollection new.
	1 to: 8 do: [:each |
		point:= ((Time microsecondClockValue) rem: (grid size)) +1.
		self leftPositions add: (grid at: point).
		grid removeAtIndex: point.
	].
	!

newRightPositions

	|grid point xSplit ySplit xMax yMax rows columns|
	xMax:= self parentView extent x.
	yMax:= self parentView extent y -150.
	columns:= xMax quo: 300.
	rows:= yMax quo: 150.
	xSplit:= xMax quo: columns.
	ySplit:= yMax quo: rows.
	grid:= OrderedCollection new.
	1 to: rows do:[:r |
		1 to: columns do: [:c | grid add: (c*(xSplit) - (xSplit) + xMax quo: 2)+5@(r*(ySplit)-(ySplit) + 100)]
	].
	self rightPositions: OrderedCollection new.
	1 to: 8 do: [:each |
		point:= ((Time microsecondClockValue) rem: (grid size)) +1.
		self rightPositions
			add: (grid at: point).
		grid removeAtIndex: point.
	].!

onPaintRequired: aPaintEvent
	"Handler for aPaintEvent. 
	This is called whenever the receiver window needs to be completely or partially redrawn."

	|canvas graphics|
	canvas := aPaintEvent canvas.
	canvas erase.
	"self shapes do: [:each | each drawOn: canvas]."

	canvas
		erase;
		pen: (Pen red width: 4);
		brush: Color gray brush;
		lineFrom: ((self parentView extent x /2) asInteger)@0 to: ((self parentView extent x /2) asInteger)@(self parentView extent y).!

pair

	^pair!

pair: anEncuentraloCardPair

	pair:= anEncuentraloCardPair!

rightPositions

	^rightPositions!

rightPositions: anOrderedCollection

	rightPositions:= anOrderedCollection!

state

	^state!

state: aState

	state:= aState! !
!EncuentraloPlayer categoriesFor: #bothState!accessing!private! !
!EncuentraloPlayer categoriesFor: #bothState:!accessing!private! !
!EncuentraloPlayer categoriesFor: #clearState!accessing!private! !
!EncuentraloPlayer categoriesFor: #clearState:!accessing!private! !
!EncuentraloPlayer categoriesFor: #cycle!public! !
!EncuentraloPlayer categoriesFor: #deck!public! !
!EncuentraloPlayer categoriesFor: #deck:!public! !
!EncuentraloPlayer categoriesFor: #initialize!public! !
!EncuentraloPlayer categoriesFor: #leftPositions!public! !
!EncuentraloPlayer categoriesFor: #leftPositions:!public! !
!EncuentraloPlayer categoriesFor: #leftState!accessing!private! !
!EncuentraloPlayer categoriesFor: #leftState:!accessing!private! !
!EncuentraloPlayer categoriesFor: #newLeftPositions!public! !
!EncuentraloPlayer categoriesFor: #newRightPositions!public! !
!EncuentraloPlayer categoriesFor: #onPaintRequired:!public! !
!EncuentraloPlayer categoriesFor: #pair!public! !
!EncuentraloPlayer categoriesFor: #pair:!public! !
!EncuentraloPlayer categoriesFor: #rightPositions!public! !
!EncuentraloPlayer categoriesFor: #rightPositions:!public! !
!EncuentraloPlayer categoriesFor: #state!public! !
!EncuentraloPlayer categoriesFor: #state:!public! !

!EncuentraloPlayer class methodsFor!

playedWith: anOrderedCollection

	|player cycleButton deck|
	deck:= (SpotDeckGenerator for: anOrderedCollection).
	deck size < 15
		ifTrue: [^nil].
	player:= super new.
	player deck: deck;
	caption: 'Encuentralo';
	extent: player parentView extent;
	pair: (SpotCardPair drawFrom: player deck limit: 8).
	player newLeftPositions; newRightPositions.

	player clearState: (EncuentraloStateClear onPlayer: player).
	player leftState: (EncuentraloStateLeft onPlayer: player).
	player bothState: (EncuentraloStateBoth onPlayer: player).
	player state: player clearState.

	cycleButton:= PushButton new.
	player addSubView: cycleButton.
	cycleButton
		text: 'Cycle';
		command: #cycle.
	player cycle.
	^player! !
!EncuentraloPlayer class categoriesFor: #playedWith:!public! !

"Binary Globals"!

