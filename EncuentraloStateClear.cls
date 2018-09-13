"Filed out from Dolphin Smalltalk 7"!

EncuentraloState subclass: #EncuentraloStateClear
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
EncuentraloStateClear guid: (GUID fromString: '{9BEB9355-FF7C-404F-A32A-9C1DC96D34E2}')!
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

