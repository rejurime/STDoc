"Filed out from Dolphin Smalltalk 7"!

STDocObject subclass: #STDocVariable
	instanceVariableNames: 'name docClass type'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocVariable guid: (GUID fromString: '{b766ee9c-5dfb-479a-ba71-d12299382387}')!
STDocVariable comment: ''!
!STDocVariable categoriesForClass!Unclassified! !
!STDocVariable methodsFor!

comment
	| str |
	(self docClass docComment attributeTable at: 'var' ifAbsent: [^'']) do: 
			[:string |
			str := string readStream.
			(STDocUtil firstWordOf: str) = self name ifTrue: [^str upToEnd]].
	^''!

docClass
	^docClass!

docClass: anObject
	docClass := anObject!

firstWordOf: aStream
	"Private - Fetches the first word of the stream, it ignores all the separator characters at the beggining.
	This method consumes the stream. When this method finished the stream is positioned after the first separater char that comes after the first word.


	@param aStream the stream that's going to be used
	@returns a string with the first word"

	| word c |
	word := String writeStream.
	aStream skipSeparators.
	[aStream atEnd not and: [(c := aStream next) isSeparator not]] whileTrue: [word nextPut: c].
	^word contents!

isClass
	^self type = #class!

isClassInst
	^self type = #classInst!

isInst
	^self type = #inst!

name
	^name!

name: anObject
	name := anObject!

type
	^type!

type: anObject
	type := anObject! !
!STDocVariable categoriesFor: #comment!public! !
!STDocVariable categoriesFor: #docClass!accessing!public! !
!STDocVariable categoriesFor: #docClass:!accessing!public! !
!STDocVariable categoriesFor: #firstWordOf:!private! !
!STDocVariable categoriesFor: #isClass!public! !
!STDocVariable categoriesFor: #isClassInst!public! !
!STDocVariable categoriesFor: #isInst!public! !
!STDocVariable categoriesFor: #name!accessing!public! !
!STDocVariable categoriesFor: #name:!accessing!public! !
!STDocVariable categoriesFor: #type!accessing!public! !
!STDocVariable categoriesFor: #type:!accessing!public! !

!STDocVariable class methodsFor!

forClassInstVar: aClass name: aString
	^self
		forType: #classInst
		class: aClass
		name: aString!

forClassVar: aClass name: aString
	^self
		forType: #class
		class: aClass
		name: aString!

forInstVar: aClass name: aString
	^self
		forType: #inst
		class: aClass
		name: aString!

forType: aType class: aClass name: aString
	^(self new)
		type: aType;
		docClass: aClass;
		name: aString;
		yourself! !
!STDocVariable class categoriesFor: #forClassInstVar:name:!public! !
!STDocVariable class categoriesFor: #forClassVar:name:!public! !
!STDocVariable class categoriesFor: #forInstVar:name:!public! !
!STDocVariable class categoriesFor: #forType:class:name:!public! !

