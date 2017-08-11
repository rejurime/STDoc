"Filed out from Dolphin Smalltalk 7"!

DOCObject subclass: #DOCVariable
	instanceVariableNames: 'name docClass type'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCVariable guid: (GUID fromString: '{B766EE9C-5DFB-479A-BA71-D12299382387}')!
DOCVariable comment: ''!
!DOCVariable categoriesForClass!Unclassified! !
!DOCVariable methodsFor!

comment
	| str |
	(self docClass docComment attributeTable at: 'var' ifAbsent: [^'']) do: 
			[:string |
			str := string readStream.
			(DOCSUtils firstWordOf: str) = self name ifTrue: [^str upToEnd]].
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
!DOCVariable categoriesFor: #comment!public! !
!DOCVariable categoriesFor: #docClass!accessing!public! !
!DOCVariable categoriesFor: #docClass:!accessing!public! !
!DOCVariable categoriesFor: #firstWordOf:!private! !
!DOCVariable categoriesFor: #isClass!public! !
!DOCVariable categoriesFor: #isClassInst!public! !
!DOCVariable categoriesFor: #isInst!public! !
!DOCVariable categoriesFor: #name!accessing!public! !
!DOCVariable categoriesFor: #name:!accessing!public! !
!DOCVariable categoriesFor: #type!accessing!public! !
!DOCVariable categoriesFor: #type:!accessing!public! !

!DOCVariable class methodsFor!

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
!DOCVariable class categoriesFor: #forClassInstVar:name:!public! !
!DOCVariable class categoriesFor: #forClassVar:name:!public! !
!DOCVariable class categoriesFor: #forInstVar:name:!public! !
!DOCVariable class categoriesFor: #forType:class:name:!public! !

