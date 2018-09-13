"Filed out from Dolphin Smalltalk 7"!

Shell subclass: #Encuentralo
	instanceVariableNames: 'deckListPresenter shell deckList startButton'
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

	Transcript show: 'FUCK'.
	^deckListPresenter!

drawMainMenu

	|banner buttonPresenter|
	banner:= StaticText new.
	shell view addSubView: banner.
	banner
		text: 'ENCUENTRALO';
		extent: 500@50;
		alignment: #center;
		backcolor: Color white;
		font: (Font name: 'Terminal' pointSize: 32).

	startButton:= PushButton new.
	shell view addSubView: startButton.
	startButton
		text: 'Play';
		position: 370 @ 75;
		extent: 100 @ 25;
		command: #playEncuentralo. "um yes, Object Arts? uh yeah why the fuck is this a super send"
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

