"Filed out from Dolphin Smalltalk 7"!

DOCCondition subclass: #DOCOr
	instanceVariableNames: 'conditions'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCOr guid: (GUID fromString: '{10E41E62-8400-4301-A827-FC14D205BEDE}')!
DOCOr comment: ''!
!DOCOr categoriesForClass!Unclassified! !
!DOCOr methodsFor!

conditions
	^conditions!

conditions: anObject
	conditions := anObject!

value: anObject
	"@override"

	self conditions isEmpty ifTrue: [^true].
	^self conditions anySatisfy: [:cond | cond value: anObject]! !
!DOCOr categoriesFor: #conditions!accessing!public! !
!DOCOr categoriesFor: #conditions:!accessing!public! !
!DOCOr categoriesFor: #value:!public! !

