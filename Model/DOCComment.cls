"Filed out from Dolphin Smalltalk 7"!

DOCObject subclass: #DOCComment
	instanceVariableNames: 'comment attrTable fullComment smallComment combined'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCComment guid: (GUID fromString: '{FDCD6495-E6E7-44C5-A2F4-34EB915BDA97}')!
DOCComment comment: ''!
!DOCComment categoriesForClass!Unclassified! !
!DOCComment methodsFor!

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
	DOCSUtils ignoreFrom: str upTo: [:c | c == $@].
	result := LookupTable new.
	[(attribute := DOCSUtils stringFrom: str upTo: [:c | c == $@]) notNil] whileTrue: 
			[| id partStream |
			partStream := attribute readStream.
			id := DOCSUtils firstWordOf: partStream.
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
			smallComment := ((DOCSUtils stringFrom: line upTo: [:c | c = $.])
						ifNil: ['']
						ifNotNil: [:s | s , '.']) trimBlanks]! !
!DOCComment categoriesFor: #attributeIncludes:!public! !
!DOCComment categoriesFor: #attributeIncludesAll:!public! !
!DOCComment categoriesFor: #attributesAt:do:!public! !
!DOCComment categoriesFor: #attributesAt:ifAbsent:!public! !
!DOCComment categoriesFor: #attributesIn:do:!public! !
!DOCComment categoriesFor: #attributeTable!public! !
!DOCComment categoriesFor: #combine:!public! !
!DOCComment categoriesFor: #comment!accessing!public! !
!DOCComment categoriesFor: #comment:!accessing!public! !
!DOCComment categoriesFor: #fullComment!public! !
!DOCComment categoriesFor: #hasLargeComment!public! !
!DOCComment categoriesFor: #initialize!private! !
!DOCComment categoriesFor: #parseMethod:!private! !
!DOCComment categoriesFor: #smallComment!public! !

!DOCComment class methodsFor!

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
!DOCComment class categoriesFor: #for:!public! !
!DOCComment class categoriesFor: #forClass:!public! !
!DOCComment class categoriesFor: #forMethod:!public! !

