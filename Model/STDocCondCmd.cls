"Filed out from Dolphin Smalltalk 7"!

STDocRenderCmd subclass: #STDocCondCmd
	instanceVariableNames: 'trueCommands falseCommands condition'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocCondCmd guid: (GUID fromString: '{88a937f1-d937-4d1f-bf82-3afd9afdc30e}')!
STDocCondCmd comment: ''!
!STDocCondCmd categoriesForClass!Unclassified! !
!STDocCondCmd methodsFor!

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
!STDocCondCmd categoriesFor: #condition!accessing!public! !
!STDocCondCmd categoriesFor: #condition:!accessing!public! !
!STDocCondCmd categoriesFor: #execute:on:!public! !
!STDocCondCmd categoriesFor: #falseCommands!accessing!public! !
!STDocCondCmd categoriesFor: #falseCommands:!accessing!public! !
!STDocCondCmd categoriesFor: #initialize!private! !
!STDocCondCmd categoriesFor: #trueCommands!accessing!public! !
!STDocCondCmd categoriesFor: #trueCommands:!accessing!public! !

