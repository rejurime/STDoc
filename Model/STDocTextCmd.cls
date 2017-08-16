"Filed out from Dolphin Smalltalk 7"!

STDocRenderCmd subclass: #STDocTextCmd
	instanceVariableNames: 'name params'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocTextCmd guid: (GUID fromString: '{7a8a3190-c10e-4536-914a-c75332cabfcd}')!
STDocTextCmd comment: ''!
!STDocTextCmd categoriesForClass!Unclassified! !
!STDocTextCmd methodsFor!

execute: obj on: aStream
	| args |
	args := Array
				with: obj
				with: self params
				with: aStream.
	self renderer perform: self name withArguments: args!

name
	^name!

name: anObject
	name := anObject!

params
	^params!

params: anObject
	params := anObject! !
!STDocTextCmd categoriesFor: #execute:on:!public! !
!STDocTextCmd categoriesFor: #name!accessing!public! !
!STDocTextCmd categoriesFor: #name:!accessing!public! !
!STDocTextCmd categoriesFor: #params!accessing!public! !
!STDocTextCmd categoriesFor: #params:!accessing!public! !

