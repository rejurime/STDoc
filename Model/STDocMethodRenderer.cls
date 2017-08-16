"Filed out from Dolphin Smalltalk 7"!

STDocClassTemplateRenderer subclass: #STDocMethodRenderer
	instanceVariableNames: 'methodType'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocMethodRenderer guid: (GUID fromString: '{c8c1537f-e0cc-4838-a084-8b97c141cda6}')!
STDocMethodRenderer comment: ''!
!STDocMethodRenderer categoriesForClass!Unclassified! !
!STDocMethodRenderer methodsFor!

availableCommands
	"The available renderers that can be used to render objects

	@returns a lookup table that has as key the command name and as value the method selector"

	^(super availableCommands)
		at: 'METHODNAME' put: #renderMethodName:params:on:;
		at: 'METHODPARAMS' put: #renderMethodParams:params:on:;
		at: 'METHODRETURN' put: #renderMethodReturn:params:on:;
		at: 'METHODSMALLCOMMENT' put: #renderMethodSmallComment:params:on:;
		at: 'METHODLARGECOMMENT' put: #renderMethodLargeComment:params:on:;
		at: 'METHODACCESS' put: #renderMethodAccess:params:on:;
		at: 'METHODOVERRIDE' put: #renderMethodOverride:params:on:;
		at: 'METHODCLASS' put: #renderMethodClass:params:on:;
		at: 'METHODID' put: #renderMethodID:params:on:;
		at: 'METHODCATEGORIES' put: #renderMethodCategories:params:on:;
		yourself!

getObjectsOf: aDOCClass
	"Private - Returns a Collection with objects that are going to be rendered"

	| methods |
	methods := methodType = 'CLASS'
				ifTrue: [aDOCClass classMethods]
				ifFalse: 
					[methodType = 'INST'
						ifTrue: [aDOCClass methods]
						ifFalse: 
							[methodType = 'ALL'
								ifTrue: [aDOCClass classMethods , aDOCClass methods]
								ifFalse: [OrderedCollection new]]].
	methods do: [:method | self setCommentsOf: method].
	^methods!

isTrue: condString of: anObject
	(condString sameAs: 'hasMoreComment') ifTrue: [^anObject comment hasLargeComment].
	super isTrue: condString of: anObject!

methodID: aMethod on: aStream
	^aMethod definitionIDOn: aStream!

parseNode: aNode
	super parseNode: aNode.
	methodType := (aNode attributes at: 'methodType' ifAbsent: ['INST']) asUppercase!

renderMethodAccess: aMethod params: params on: aStream
	"Renders the method access image.
	
	@param aMethod the method
	@param params parameters for the render command
	@param aStream the stream where you are going to render"

	aStream
		nextPutAll: (self manager imageTag: (aMethod methodObject isPrivate
							ifTrue: [self manager privIcon]
							ifFalse: [self manager pubIcon]));
		cr!

renderMethodCategories: aMethod params: params on: aStream
	"Renders the method categories list.
	
	@param aMethod the method
	@param params parameters for the render command
	@param aStream the stream where you are going to render"

	| categoriesNames |
	categoriesNames := aMethod methodObject categories collect: [:c | c name].
	(categoriesNames notEmpty and: [params notEmpty]) ifTrue: [aStream nextPutAll: params first].
	categoriesNames do: [:cName | aStream nextPutAll: cName] separatedBy: [aStream nextPutAll: ', ']!

renderMethodClass: aMethod params: params on: aStream
	"Renders the method class image.
	
	@param aMethod the method
	@param params parameters for the render command
	@param aStream the stream where you are going to render"

	aStream
		nextPutAll: (self manager imageTag: (aMethod methodObject isClassMethod
							ifTrue: [self manager classIcon]
							ifFalse: [self manager blankIcon]));
		cr!

renderMethodID: aMethod params: params on: aStream
	"Renders an id for the method that doesn't have spaces nor ':' to use for identifying the method.
	
	@param aMethod the method
	@param params parameters for the render command
	@param aStream the stream where you are going to render"

	self methodID: aMethod on: aStream!

renderMethodLargeComment: aMethod params: params on: aStream
	"Renders the method basic description.
	
	@param aMethod the method
	@param params parameters for the render command
	@param aStream the stream where you are going to render"

	"aStream nextPutAll: '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'."

	STDocUtil makeHTMLString: aMethod comment fullComment on: aStream!

renderMethodName: aMethod params: params on: aStream
	"Renders the method name.
	
	@param aMethod the method
	@param params parameters for the render command
	@param aStream the stream where you are going to render"

	| parts methodDef |
	methodDef := aMethod definition.
	parts := methodDef subStrings: ' '.
	1 to: parts size
		do: 
			[:i |
			i odd
				ifTrue: 
					[aStream
						nextPutAll: '<span style="color: blue;font-weight:bold">';
						nextPutAll: (parts at: i) trimBlanks;
						nextPutAll: ' </span> ']
				ifFalse: 
					[aStream
						nextPutAll: (parts at: i) trimBlanks;
						nextPutAll: ' ']]!

renderMethodOverride: aMethod params: params on: aStream
	"Renders the method override image.
	
	@param aMethod the method
	@param params parameters for the render command
	@param aStream the stream where you are going to render"

	aMethod methodObject isOverride
		ifTrue: 
			[(self searchOverrideClassOf: aMethod methodObject)
				ifNotNil: 
					[:c |
					aStream nextPutAll: '(Overridden from class '.
					self
						printClassLink: c
						currentClass: aMethod methodObject methodClass
						on: aStream.
					aStream nextPutAll: ')']]!

renderMethodParams: aMethod params: params on: aStream
	"Renders the method parameter descriptions.
	
	@param aMethod the method
	@param params parameters for the render command
	@param aStream the stream where you are going to render"

	| colClass paramsSize strings colStyle |
	strings := aMethod comment attributesAt: 'param' ifAbsent: [^self].
	paramsSize := params size.
	paramsSize > 0 ifTrue: [aStream nextPutAll: params first].
	aStream nextPutAll: '<table'.
	paramsSize > 2
		ifTrue: 
			[params second ~= ''
				ifTrue: 
					[aStream
						nextPutAll: ' class="';
						nextPutAll: params second;
						nextPutAll: '"'].
			colClass := params third.
			colClass = '' ifTrue: [colClass := nil]].
	aStream nextPutAll: '>'.
	colClass notNil ifTrue: [colStyle := ' class="' , colClass , '"'] ifFalse: [colStyle := ''].
	strings do: 
			[:string |
			| name stream |
			stream := string readStream.
			name := STDocUtil firstWordOf: stream.
			aStream
				nextPutAll: '<tr><td border="0" style="width:12px"';
				nextPutAll: colStyle;
				nextPutAll: '>&nbsp;</td><td style="font-weight:bold"';
				nextPutAll: colStyle;
				nextPutAll: '>';
				nextPutAll: name;
				nextPutAll: '&nbsp;&nbsp;&nbsp;</td><td';
				nextPutAll: colStyle;
				nextPutAll: '>';
				nextPutAll: (stream atEnd ifTrue: [''] ifFalse: [stream upToEnd]);
				nextPutAll: '</td></tr>';
				cr].
	aStream
		nextPutAll: '</table>';
		cr!

renderMethodReturn: aMethod params: params on: aStream
	"Renders the method return comment.
	
	@param aMethod the method
	@param params parameters for the render command
	@param aStream the stream where you are going to render"

	| strings |
	strings := aMethod comment attributesAt: 'returns'
				ifAbsent: [aMethod comment attributesAt: 'return' ifAbsent: [^self]].
	params notEmpty ifTrue: [aStream nextPutAll: params first].
	aStream nextPutAll: '<table>'.
	strings do: 
			[:string |
			aStream
				nextPutAll: '<tr><td class="' , self manager defaultStyle
							, '" style="width:12px">&nbsp;</td><td class="' , self manager defaultStyle
							, '">';
				nextPutAll: string;
				nextPutAll: '</td></tr>';
				cr].
	aStream
		nextPutAll: '</table>';
		cr!

renderMethodSmallComment: aMethod params: params on: aStream
	"Renders the method basic description.
	
	@param aMethod the method
	@param params parameters for the render command
	@param aStream the stream where you are going to render"

	STDocUtil makeHTMLString: aMethod comment smallComment on: aStream!

searchOverrideClassOf: aMethod
	| c |
	c := aMethod methodClass superclass.
	[c notNil] whileTrue: 
			[(c compiledMethodAt: aMethod selector ifAbsent: []) ifNotNil: [:m | ^c].
			c := c superclass].
	^nil!

setCommentsOf: method
	(method comment attributesAt: 'override' ifAbsent: []) notNil
		ifTrue: 
			[(self searchOverrideClassOf: method methodObject)
				ifNotNil: 
					[:cls |
					(self manager searchClass: cls name)
						ifNotNil: 
							[:docCls |
							(docCls searchMethod: method methodObject selector)
								ifNotNil: [:met | method comment combine: met comment]]]]! !
!STDocMethodRenderer categoriesFor: #availableCommands!public! !
!STDocMethodRenderer categoriesFor: #getObjectsOf:!private! !
!STDocMethodRenderer categoriesFor: #isTrue:of:!public! !
!STDocMethodRenderer categoriesFor: #methodID:on:!private! !
!STDocMethodRenderer categoriesFor: #parseNode:!public! !
!STDocMethodRenderer categoriesFor: #renderMethodAccess:params:on:!public! !
!STDocMethodRenderer categoriesFor: #renderMethodCategories:params:on:!public! !
!STDocMethodRenderer categoriesFor: #renderMethodClass:params:on:!public! !
!STDocMethodRenderer categoriesFor: #renderMethodID:params:on:!public! !
!STDocMethodRenderer categoriesFor: #renderMethodLargeComment:params:on:!public! !
!STDocMethodRenderer categoriesFor: #renderMethodName:params:on:!public! !
!STDocMethodRenderer categoriesFor: #renderMethodOverride:params:on:!public! !
!STDocMethodRenderer categoriesFor: #renderMethodParams:params:on:!public! !
!STDocMethodRenderer categoriesFor: #renderMethodReturn:params:on:!public! !
!STDocMethodRenderer categoriesFor: #renderMethodSmallComment:params:on:!public! !
!STDocMethodRenderer categoriesFor: #searchOverrideClassOf:!private! !
!STDocMethodRenderer categoriesFor: #setCommentsOf:!private! !

!STDocMethodRenderer class methodsFor!

rendererName
	"This is a custom name used to identify the renderer in the xml code, each renderer must have it's own name"

	^'method'! !
!STDocMethodRenderer class categoriesFor: #rendererName!public! !

