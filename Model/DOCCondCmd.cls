"Filed out from Dolphin Smalltalk 7"!

DOCRenderCmd subclass: #DOCCondCmd
	instanceVariableNames: 'trueCommands falseCommands condition'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCCondCmd guid: (GUID fromString: '{88A937F1-D937-4D1F-BF82-3AFD9AFDC30E}')!
DOCCondCmd comment: ''!
!DOCCondCmd categoriesForClass!Unclassified! !
!DOCCondCmd methodsFor!

condition
	^condition!

condition: anObject
	condition := anObject!

execute: obj on: aStream
	(self condition value: obj)
		ifTrue: [self trueCommands do: [:c | c execute: obj on: aStream]]
		ifFalse: [self falseCommands do: [:c | c execute: obj on: aStream]]!

falseCommands
	^falseCommands!

falseCommands: anObject
	falseCommands := anObject!

initialize
	super initialize.
	trueCommands := OrderedCollection new.
	falseCommands := OrderedCollection new!

trueCommands
	^trueCommands!

trueCommands: anObject
	trueCommands := anObject! !
!DOCCondCmd categoriesFor: #condition!accessing!public! !
!DOCCondCmd categoriesFor: #condition:!accessing!public! !
!DOCCondCmd categoriesFor: #execute:on:!public! !
!DOCCondCmd categoriesFor: #falseCommands!accessing!public! !
!DOCCondCmd categoriesFor: #falseCommands:!accessing!public! !
!DOCCondCmd categoriesFor: #initialize!private! !
!DOCCondCmd categoriesFor: #trueCommands!accessing!public! !
!DOCCondCmd categoriesFor: #trueCommands:!accessing!public! !

