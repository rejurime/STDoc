"Filed out from Dolphin Smalltalk 7"!

STDocCondition subclass: #STDocOr
	instanceVariableNames: 'conditions'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocOr guid: (GUID fromString: '{10e41e62-8400-4301-a827-fc14d205bede}')!
STDocOr comment: ''!
!STDocOr categoriesForClass!Unclassified! !
!STDocOr methodsFor!

conditions
	^conditions!

conditions: anObject
	conditions := anObject!

value: anObject
	"@override"

	self conditions isEmpty ifTrue: [^true].
	^self conditions anySatisfy: [:cond | cond value: anObject]! !
!STDocOr categoriesFor: #conditions!accessing!public! !
!STDocOr categoriesFor: #conditions:!accessing!public! !
!STDocOr categoriesFor: #value:!public! !

