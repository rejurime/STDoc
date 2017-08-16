"Filed out from Dolphin Smalltalk 7"!

STDocCondition subclass: #STDocPredefinedCond
	instanceVariableNames: 'renderer condition'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocPredefinedCond guid: (GUID fromString: '{bc28fd66-6780-4568-b1b4-aad654d01c0a}')!
STDocPredefinedCond comment: ''!
!STDocPredefinedCond categoriesForClass!Unclassified! !
!STDocPredefinedCond methodsFor!

condition
	^condition!

condition: anObject
	condition := anObject!

renderer
	^renderer!

renderer: anObject
	renderer := anObject!

value: anObject
	"@override"

	^self renderer isTrue: self condition of: anObject! !
!STDocPredefinedCond categoriesFor: #condition!accessing!public! !
!STDocPredefinedCond categoriesFor: #condition:!accessing!public! !
!STDocPredefinedCond categoriesFor: #renderer!accessing!public! !
!STDocPredefinedCond categoriesFor: #renderer:!accessing!public! !
!STDocPredefinedCond categoriesFor: #value:!public! !

!STDocPredefinedCond class methodsFor!

str: conditionString on: aRenderer
	^(self new)
		renderer: aRenderer;
		condition: conditionString;
		yourself! !
!STDocPredefinedCond class categoriesFor: #str:on:!public! !

