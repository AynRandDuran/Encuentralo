"Filed out from Dolphin Smalltalk 7"!

CollectionPresenter subclass: #DirectoryCollectionPresenter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DirectoryCollectionPresenter guid: (GUID fromString: '{316D5EA6-5A4D-49F4-9378-7981AF2A7463}')!
DirectoryCollectionPresenter comment: ''!
!DirectoryCollectionPresenter categoriesForClass!Unclassified! !
!DirectoryCollectionPresenter methodsFor!

defaultAddItem
	"Private - Prompts to add an item to the receiver. Used if no addItemBlock has been specified"

	^BrowseFolderDialog showModal! !
!DirectoryCollectionPresenter categoriesFor: #defaultAddItem!private! !

