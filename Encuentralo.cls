"Filed out from Dolphin Smalltalk 7"!

Shell subclass: #Encuentralo
	instanceVariableNames: 'deckListPresenter LeftCardPresenter RightCardPresenter shell startButton'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Encuentralo guid: (GUID fromString: '{8246C5AE-A0C7-4FD5-8C21-A24C5BFEDDDB}')!
Encuentralo comment: '"Smalltalk implementation of the card game Spot It. Please don''t sue me"'!
!Encuentralo categoriesForClass!Unclassified! !
!Encuentralo methodsFor!

createComponents

	super createComponents.
	shell:= Shell create: Shell defaultView.
	shell extent: 500@300.
	deckListPresenter:= DirectoryCollectionPresenter createIn: shell on: OrderedCollection new.
	shell add: deckListPresenter.
	deckListPresenter position: 10@75.
	deckListPresenter extent: 350@175.
	self drawMainMenu.
	shell show.
	startButton
	!

createSchematicWiring

	!

drawMainMenu

	|banner playButton|
	banner:= StaticText new.
	shell view addSubView: banner.
	banner
		text: 'ENCUENTRALO';
		extent: 500@50;
		alignment: #center;
		backcolor: Color white;
		font: (Font name: 'Terminal' pointSize: 32). "Uh yeah why the fuck doesn't this work"

	playButton:= PushButton new.
	shell view addSubView: playButton.
	playButton
		text: 'Play';
		position: 370 @ 75;
		extent: 100 @ 25;
		backcolor: (Color black);
		command: #startGame.
		self halt.
	startButton:= playButton.!

startGame

	Transcript show: 'ah'; cr.! !
!Encuentralo categoriesFor: #createComponents!public! !
!Encuentralo categoriesFor: #createSchematicWiring!public! !
!Encuentralo categoriesFor: #drawMainMenu!public! !
!Encuentralo categoriesFor: #startGame!public! !

!Encuentralo class methodsFor!

playButton

	|button|

	^button! !
!Encuentralo class categoriesFor: #playButton!public! !

