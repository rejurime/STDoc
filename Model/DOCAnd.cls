"Filed out from Dolphin Smalltalk 7"!

DOCCondition subclass: #DOCAnd
	instanceVariableNames: 'conditions'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCAnd guid: (GUID fromString: '{C1999A98-807B-4465-8668-E6FB3B1699EF}')!
DOCAnd comment: ''!
!DOCAnd categoriesForClass!Unclassified! !
!DOCAnd methodsFor!

conditions
	^conditions!

conditions: anObject
	conditions := anObject!

value: anObject
	"@override"

	self conditions isEmpty ifTrue: [^true].
	^self conditions allSatisfy: [:cond | cond value: anObject]! !
!DOCAnd categoriesFor: #conditions!accessing!public! !
!DOCAnd categoriesFor: #conditions:!accessing!public! !
!DOCAnd categoriesFor: #value:!public! !

