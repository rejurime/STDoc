"Filed out from Dolphin Smalltalk 7"!

STDocObject subclass: #STDocCondition
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocCondition guid: (GUID fromString: '{0f0025e8-3122-42a2-a1a1-45429c02e814}')!
STDocCondition comment: ''!
!STDocCondition categoriesForClass!Unclassified! !
!STDocCondition methodsFor!

cand: aCondition
	^(STDocAnd new)
		conditions: self;
		condition2: aCondition;
		yourself!

cor: aCondition
	^(STDocOr new)
		conditions: self;
		condition2: aCondition;
		yourself!

value: anObject
	"Debe procesar lo que sea necesario y retornar el valor booleano que corresponda.

	@param anObject es lo que se está renderizando, puede ser una clase, un método o una variable
	@returns true si la condición es válida o en caso contrario false"

	^self subclassResponsibility! !
!STDocCondition categoriesFor: #cand:!public! !
!STDocCondition categoriesFor: #cor:!public! !
!STDocCondition categoriesFor: #value:!public! !

