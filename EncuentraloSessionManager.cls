"Filed out from Dolphin Smalltalk 7"!

RuntimeSessionManager subclass: #EncuentraloSessionManager
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
EncuentraloSessionManager guid: (GUID fromString: '{1943fd47-167e-479d-abcc-8ad112815b0e}')!
EncuentraloSessionManager comment: ''!
!EncuentraloSessionManager categoriesForClass!Unclassified! !
!EncuentraloSessionManager methodsFor!

main

	Encuentralo new! !
!EncuentraloSessionManager categoriesFor: #main!public! !

!EncuentraloSessionManager class methodsFor!

mainShellClass
	"Answer the class of the application's main window (a <Shell> presenter).
	This should be overridden by subclasses, and would be a subclass responsibility
	method if it were not for the need to support instance based deployment."

	^Smalltalk lookup: #Encuentralo! !
!EncuentraloSessionManager class categoriesFor: #mainShellClass!public! !

