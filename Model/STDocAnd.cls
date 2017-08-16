"Filed out from Dolphin Smalltalk 7"!

STDocCondition subclass: #STDocAnd
	instanceVariableNames: 'conditions'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocAnd guid: (GUID fromString: '{c1999a98-807b-4465-8668-e6fb3b1699ef}')!
STDocAnd comment: ''!
!STDocAnd categoriesForClass!Unclassified! !
!STDocAnd methodsFor!

conditions
	^conditions!

conditions: anObject
	conditions := anObject!

value: anObject
	"@override"

	self conditions isEmpty ifTrue: [^true].
	^self conditions allSatisfy: [:cond | cond value: anObject]! !
!STDocAnd categoriesFor: #conditions!accessing!public! !
!STDocAnd categoriesFor: #conditions:!accessing!public! !
!STDocAnd categoriesFor: #value:!public! !

