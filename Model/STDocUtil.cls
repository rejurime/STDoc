"Filed out from Dolphin Smalltalk 7"!

STDocObject subclass: #STDocUtil
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocUtil guid: (GUID fromString: '{8235fd37-c1ce-4a8f-9d60-4e92ee9905f8}')!
STDocUtil comment: 'This class is used as a bag of functions to process strings and streams.
Al kinds of common functions are included here, most of them are for parsing texts'!
!STDocUtil categoriesForClass!Unclassified! !
!STDocUtil class methodsFor!

charToHTML: char
	^char == Character lf
		ifTrue: ['<br />' , String lineDelimiter]
		ifFalse: 
			[char == Character cr
				ifTrue: ['']
				ifFalse: 
					[Character tab == char
						ifTrue: 
							["'&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'"
							Character space asString]
						ifFalse: 
							[$< == char
								ifTrue: ['&lt;']
								ifFalse: [$> == char ifTrue: ['&gt;'] ifFalse: [$& == char ifTrue: ['&amp;'] ifFalse: [nil]]]]]]!

consume: aStream do: aBlock
	"Consumes the stream and executes the block for each object consumed

	@param aStream the stream to be consumed
	@param aBlock the monadic valuable to be executed"

	[aStream atEnd] whileFalse: [aBlock value: aStream next]!

firstWordOf: aStream
	"Fetches the first word of the stream, it ignores all the separator characters at the beggining.
	This method consumes the stream. When this method finished the stream is positioned after the first separater char that comes after the first word.

	@param aStream the stream that's going to be used
	@returns a string with the first word"

	| word c |
	word := String writeStream.
	aStream skipSeparators.
	[aStream atEnd not and: [(c := aStream next) isSeparator not]] whileTrue: [word nextPut: c].
	^word contents!

ignore: aStream where: aBlock
	"Ignores the objects of a stream while the block returns true

	@param aStream the stream to be used
	@param aBlock a monadic valuable, it has to return true when the object has to be skipped"

	[aStream atEnd] whileFalse: [(aBlock value: aStream peek) ifTrue: [aStream next] ifFalse: [^self]]!

ignoreFrom: aStream upTo: aBlock
	"Ignores the objects of a stream until the block returns true. The object that makes the processor stop ignoring is ignored too.

	@param aStream the stream to be used
	@param aBlock a monadic valuable, it has to return true when the processor has to stop ignoring objects"

	self consume: aStream do: [:c | (aBlock value: c) ifTrue: [^self]]!

isValidTag: tag
	| realTag |
	tag isEmpty ifTrue: [^false].
	tag first = $/ ifTrue: [realTag := tag copyFrom: 2] ifFalse: [realTag := tag].
	^##(#('b' 'pre' 'code' 'i' 'p' 'br' 'h1' 'h2' 'h3' 'h4' 'li')) includes: realTag asLowercase!

makeHTMLString: aString on: outStream
	"Replaces special HTML characters of the string and makes a valid text that does not conflicts with HTML tags. For example replaces character '<' with '&gt;'.
	The text is wrote to outStream

	@param aString with any character
	@param outStream a stream where the result text will be wrote"

	| reader c |
	reader := aString readStream.
	[reader atEnd] whileFalse: 
			[c := reader next.
			c = $<
				ifTrue: [self makeTag: reader on: outStream]
				ifFalse: 
					[(self charToHTML: c) ifNil: [outStream nextPut: c] ifNotNil: [:rpl | outStream nextPutAll: rpl]]]!

makeIDString: aString
	"Replaces all symbols from the string that are not alphanumeric with the character '_'

	@param aString the string to process
	@return a processed string only with alphanumeric characters and slashes"

	| out |
	out := String writeStream.
	self makeIDString: aString on: out.
	^out contents!

makeIDString: aString on: out
	"Replaces all symbols from the string that are not alphanumeric with the character '_'

	@param aString the string to process
	@param out the stream where the id is going to be rendered
	@return a processed string only with alphanumeric characters and slashes"

	| in replacements |
	in := aString trimBlanks readStream.
	[in atEnd] whileFalse: 
			[| c |
			c := in next.
			c isAlphaNumeric ifTrue: [out nextPut: c] ifFalse: [out nextPut: $_]]!

makeTag: reader on: outStream
	| c tag aparecioBarra |
	tag := String writeStream.
	aparecioBarra := false.
	[reader atEnd] whileFalse: 
			[c := reader next.
			(c isAlphaNumeric or: [aparecioBarra not and: [c = $/]])
				ifTrue: 
					[c = $/ ifTrue: [aparecioBarra := true].
					tag nextPut: c]
				ifFalse: 
					[(c = $> and: [self isValidTag: tag contents])
						ifTrue: 
							[outStream nextPutAll: '<' , tag contents , '>'.
							reader skipSeparators.
							^self].
					outStream
						nextPutAll: (self charToHTML: $<) , tag contents , ((self charToHTML: c) ifNil: [c asString]).
					^self]].
	outStream
		nextPutAll: (self charToHTML: $<);
		nextPutAll: tag contents!

stringFrom: aStream filterBlock: aBlock
	"Returns the string formed from the current position applying filters to each char. The filter to apply is determined using the returned value of the block passed by parameter.
	The block is evaluated for every char in the stream, and it has to return 1 if the char has to be added, 0 if it has to end the string and 2 if the evaluated char has to be skipped.

	@param aStream the stream to be used
	@param aBlock a monadic valuable that returns 1 if it has to add the char, 0 if it has to end the string and 2 if it has to skip the character
	@return the string between the current position and the end position"

	| result |
	result := String writeStream.
	self consume: aStream
		do: 
			[:c |
			| v |
			v := aBlock value: c.
			v = 0 ifTrue: [^result contents] ifFalse: [v = 1 ifTrue: [result nextPut: c]]].
	^result isEmpty ifTrue: [nil] ifFalse: [result contents]!

stringFrom: aStream from: startBlock to: endBlock
	"Returns the string that is from the position where startBlock returns true up to the position where the endBlock returns true. The startBlock object and end Block object are not included in the string but they are consumed from the string.

	@param aStream the stream to be used
	@param startBlock a monadic valuable that returns true when the string has to start
	@param endBlock a monadic valuable that returns true when the string has to end
	@return the string between the starting position, without including it, and the end position, without including it"

	self ignoreFrom: aStream upTo: startBlock.
	^self stringFrom: aStream upTo: endBlock!

stringFrom: aStream upTo: aBlock
	"Returns the string that is from the current position to the position where the block returns true

	@param aStream the stream to be used
	@param aBlock a monadic valuable that returns true when the string is finished
	@return the string between the current position and the end position"

	| result |
	result := String writeStream.
	self consume: aStream
		do: [:c | (aBlock value: c) ifTrue: [^result contents] ifFalse: [result nextPut: c]].
	^result isEmpty ifTrue: [nil] ifFalse: [result contents]! !
!STDocUtil class categoriesFor: #charToHTML:!private! !
!STDocUtil class categoriesFor: #consume:do:!public! !
!STDocUtil class categoriesFor: #firstWordOf:!public! !
!STDocUtil class categoriesFor: #ignore:where:!public! !
!STDocUtil class categoriesFor: #ignoreFrom:upTo:!public! !
!STDocUtil class categoriesFor: #isValidTag:!private! !
!STDocUtil class categoriesFor: #makeHTMLString:on:!public! !
!STDocUtil class categoriesFor: #makeIDString:!public! !
!STDocUtil class categoriesFor: #makeIDString:on:!public! !
!STDocUtil class categoriesFor: #makeTag:on:!private! !
!STDocUtil class categoriesFor: #stringFrom:filterBlock:!public! !
!STDocUtil class categoriesFor: #stringFrom:from:to:!public! !
!STDocUtil class categoriesFor: #stringFrom:upTo:!public! !

