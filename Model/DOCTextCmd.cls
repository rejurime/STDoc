"Filed out from Dolphin Smalltalk 7"!

DOCRenderCmd subclass: #DOCTextCmd
	instanceVariableNames: 'name params'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCTextCmd guid: (GUID fromString: '{7A8A3190-C10E-4536-914A-C75332CABFCD}')!
DOCTextCmd comment: ''!
!DOCTextCmd categoriesForClass!Unclassified! !
!DOCTextCmd methodsFor!

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
!DOCTextCmd categoriesFor: #execute:on:!public! !
!DOCTextCmd categoriesFor: #name!accessing!public! !
!DOCTextCmd categoriesFor: #name:!accessing!public! !
!DOCTextCmd categoriesFor: #params!accessing!public! !
!DOCTextCmd categoriesFor: #params:!accessing!public! !

