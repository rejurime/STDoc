"Filed out from Dolphin Smalltalk 7"!

STDocObject subclass: #STDocComment
	instanceVariableNames: 'comment attrTable fullComment smallComment combined'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocComment guid: (GUID fromString: '{fdcd6495-e6e7-44c5-a2f4-34eb915bda97}')!
STDocComment comment: ''!
!STDocComment categoriesForClass!Unclassified! !
!STDocComment methodsFor!

attributeIncludes: aName
	^self attributeTable includesKey: aName!

attributeIncludesAll: anArray
	| at |
	at := self attributeTable.
	^anArray allSatisfy: [:aName | at includesKey: aName]!

attributesAt: aName do: aBlock
	(self attributesAt: aName ifAbsent: [^self]) do: [:a | aBlock value: a]!

attributesAt: aName ifAbsent: aBlock
	^self attributeTable at: aName ifAbsent: [aBlock value]!

attributesIn: anArray do: aBlock
	anArray do: [:aName | self attributesAt: aName do: [:attr | aBlock value: aName value: attr]]!

attributeTable
	"Generates a LookupTable with the attributes that are detailed at the end of the comment starting with the @ char.

	@return a LookupTable that has as key the type of attribute and as value a collection of all the attributes texts"

	| str attribute result |
	attrTable notNil ifTrue: [^attrTable].
	str := self comment readStream.
	STDocUtil ignoreFrom: str upTo: [:c | c == $@].
	result := LookupTable new.
	[(attribute := STDocUtil stringFrom: str upTo: [:c | c == $@]) notNil] whileTrue: 
			[| id partStream |
			partStream := attribute readStream.
			id := STDocUtil firstWordOf: partStream.
			(result at: id ifAbsentPut: [OrderedCollection new]) add: partStream upToEnd trimBlanks].
	attrTable := result.
	^result!

combine: overComment
	combined ifTrue: [^self].
	self fullComment.
	(fullComment notEmpty and: [fullComment notNil])
		ifTrue: [fullComment := fullComment , String lineDelimiter].
	fullComment := fullComment , '<i>(Comment copied from superclass)</i>' , String lineDelimiter
				, overComment fullComment.
	smallComment := overComment smallComment.
	self attributeTable addAll: overComment attributeTable associations.
	combined := true!

comment
	^comment!

comment: anObject
	comment := anObject.
	attrTable := nil!

fullComment
	"Gets all the description part of the comment without including the descriptions detailed with @.

	@return a string with the description comment"

	^fullComment ifNil: [fullComment := (self comment readStream upTo: $@) trimBlanks]!

hasLargeComment
	^self smallComment size < self fullComment size or: [self attributeTable notEmpty]!

initialize
	super initialize.
	self comment: ''.
	combined := false!

parseMethod: aMethod
	"Private - Searches for the comment that identifies this method.
	
	@param aMethod the method
	@returns a string with the first comment that comes after the method definition"

	| source |
	source := aMethod getSource readStream.
	source nextLine.	"Skip definition"
	source skipSeparators.
	source atEnd
		ifTrue: [self comment: '']
		ifFalse: [self comment: (source next = $" ifTrue: [source upTo: $"] ifFalse: [''])]!

smallComment
	"Gets the first sentence or the first line of the comment. It stops when it reaches a punctuation sign or the new line.

	@return a string with the first sentence of the comment"

	^smallComment
		ifNil: 
			[| line |
			line := self fullComment readStream nextLine readStream.
			smallComment := ((STDocUtil stringFrom: line upTo: [:c | c = $.])
						ifNil: ['']
						ifNotNil: [:s | s , '.']) trimBlanks]! !
!STDocComment categoriesFor: #attributeIncludes:!public! !
!STDocComment categoriesFor: #attributeIncludesAll:!public! !
!STDocComment categoriesFor: #attributesAt:do:!public! !
!STDocComment categoriesFor: #attributesAt:ifAbsent:!public! !
!STDocComment categoriesFor: #attributesIn:do:!public! !
!STDocComment categoriesFor: #attributeTable!public! !
!STDocComment categoriesFor: #combine:!public! !
!STDocComment categoriesFor: #comment!accessing!public! !
!STDocComment categoriesFor: #comment:!accessing!public! !
!STDocComment categoriesFor: #fullComment!public! !
!STDocComment categoriesFor: #hasLargeComment!public! !
!STDocComment categoriesFor: #initialize!private! !
!STDocComment categoriesFor: #parseMethod:!private! !
!STDocComment categoriesFor: #smallComment!public! !

!STDocComment class methodsFor!

for: aString
	^(self new)
		comment: aString;
		yourself!

forClass: aClass
	^self for: aClass comment!

forMethod: aMethod
	^(self new)
		parseMethod: aMethod;
		yourself! !
!STDocComment class categoriesFor: #for:!public! !
!STDocComment class categoriesFor: #forClass:!public! !
!STDocComment class categoriesFor: #forMethod:!public! !

