"Filed out from Dolphin Smalltalk 7"!

Shell subclass: #EncuentraloAbout
	instanceVariableNames: 'shell linkButton'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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

