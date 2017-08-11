"Filed out from Dolphin Smalltalk 7"!

DOCCondition subclass: #DOCPredefinedCond
	instanceVariableNames: 'renderer condition'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCPredefinedCond guid: (GUID fromString: '{BC28FD66-6780-4568-B1B4-AAD654D01C0A}')!
DOCPredefinedCond comment: ''!
!DOCPredefinedCond categoriesForClass!Unclassified! !
!DOCPredefinedCond methodsFor!

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
!DOCPredefinedCond categoriesFor: #condition!accessing!public! !
!DOCPredefinedCond categoriesFor: #condition:!accessing!public! !
!DOCPredefinedCond categoriesFor: #renderer!accessing!public! !
!DOCPredefinedCond categoriesFor: #renderer:!accessing!public! !
!DOCPredefinedCond categoriesFor: #value:!public! !

!DOCPredefinedCond class methodsFor!

str: conditionString on: aRenderer
	^(self new)
		renderer: aRenderer;
		condition: conditionString;
		yourself! !
!DOCPredefinedCond class categoriesFor: #str:on:!public! !

