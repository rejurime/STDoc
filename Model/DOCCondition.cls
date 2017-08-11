"Filed out from Dolphin Smalltalk 7"!

DOCObject subclass: #DOCCondition
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCCondition guid: (GUID fromString: '{0F0025E8-3122-42A2-A1A1-45429C02E814}')!
DOCCondition comment: ''!
!DOCCondition categoriesForClass!Unclassified! !
!DOCCondition methodsFor!

cand: aCondition
	^(DOCAnd new)
		conditions: self;
		condition2: aCondition;
		yourself!

cor: aCondition
	^(DOCOr new)
		conditions: self;
		condition2: aCondition;
		yourself!

value: anObject
	"Debe procesar lo que sea necesario y retornar el valor booleano que corresponda.

	@param anObject es lo que se está renderizando, puede ser una clase, un método o una variable
	@returns true si la condición es válida o en caso contrario false"

	^self subclassResponsibility! !
!DOCCondition categoriesFor: #cand:!public! !
!DOCCondition categoriesFor: #cor:!public! !
!DOCCondition categoriesFor: #value:!public! !

