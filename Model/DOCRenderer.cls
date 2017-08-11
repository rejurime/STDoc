"Filed out from Dolphin Smalltalk 7"!

DOCObject subclass: #DOCRenderer
	instanceVariableNames: 'manager headerCommands bodyCommands footerCommands childRenderers parent'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCRenderer guid: (GUID fromString: '{CE39D2A6-0443-46FB-8114-A0A267B389BC}')!
DOCRenderer comment: 'This class handles the rendering of the documentation. They are instantiated from the XML.
It has two parts, the first is the template, where some special tags are replaced with information that the render formats. The other part is the child templates, those same tags make the child renderers of this render to print it''s data.

@var manager the manager that contains the renderer
@var commands the commands that are replaced with information. Each renderer has different commands
@var childRenderers a collection of child renderers identified by an id
@var parent if this render is a child render it contains the renderer that has this renderer as a child, otherwise is nil'!
!DOCRenderer categoriesForClass!Unclassified! !
!DOCRenderer methodsFor!

addCommand: commandMethod params: paramArray
	"Adds a command to be executed at the end of the command list.
	
	@param commandMethod the command method to be executed
	@param paramArray an array with parameters"

	self
		addCommand: commandMethod
		params: paramArray
		to: bodyCommands!

addCommand: commandMethod params: paramArray to: aCollection
	"Adds a command to be executed at the end of the command list.
	
	@param commandMethod the command method to be executed
	@param paramArray an array with parameters
	@param aCollection a collection where to add"

	aCollection add: ((DOCTextCmd for: self)
				name: commandMethod;
				params: paramArray;
				yourself)!

availableCommands
	"The available renderers that can be used to render objects

	@returns a lookup table that has as key the command name and as value the method selector"

	^(LookupTable new)
		at: '' put: #renderText:params:on:;
		at: 'CHILDRENDER' put: #renderChild:params:on:;
		at: 'LABEL' put: #renderLabel:params:on:;
		yourself!

bodyCommands
	^bodyCommands!

bodyCommands: anObject
	bodyCommands := anObject!

container
	"Returns the object that contains this renderer and is not a renderer. In general it is the processor that contains the renderer

	@returns the processor that contains the renderer or nil"

	| container |
	[(container := self parent) isKindOf: DOCRenderer] whileTrue.
	^container!

getObjectsOf: aDOCClass
	"Private - Returns a Collection with objects that are going to be rendered"

	^Array with: aDOCClass!

initialize
	super initialize.
	bodyCommands := OrderedCollection new.
	childRenderers := LookupTable new!

isTrue: condString of: anObject
	self
		error: 'La condición ' , condString , ' no es soportada por ' , self class name , ' para el objeto '
				, anObject displayString!

manager
	^manager!

manager: anObject
	manager := anObject!

parent
	^parent!

parent: anObject
	parent := anObject!

parseCommandName: aCmdStr
	"Extracts the name of the command from the string.
	
	@returns a string with the name"

	^aCmdStr readStream upTo: $:!

parseCommandParams: aCmdStr
	"Extracts the parameter array from the command string.
	
	@returns an array with the parameters"

	| str params escaped |
	str := aCmdStr readStream.
	params := OrderedCollection new.
	str skipTo: $:.
	escaped := false.
	[str atEnd] whileFalse: 
			[| param |
			param := DOCSUtils stringFrom: str
						filterBlock: 
							[:c |
							escaped
								ifTrue: 
									[escaped := false.
									1]
								ifFalse: 
									[c = $\
										ifTrue: 
											[escaped := true.
											2]
										ifFalse: [c = $: ifTrue: [0] ifFalse: [1]]]].
			param isNil ifFalse: [params addLast: param trimBlanks]].
	^params!

parseCondition: node
	| predefined op cond |
	predefined := node attributes at: 'predefined' ifAbsent: [].
	predefined
		ifNil: 
			[op := node attributes at: 'op'
						ifAbsent: [self error: 'Condición invalida, no contiene el atributo "op" o "predefined"'].
			(op sameAs: 'and')
				ifTrue: [cond := DOCAnd new]
				ifFalse: 
					[(op sameAs: 'or') ifTrue: [cond := DOCOr new] ifFalse: [self error: 'Operador ' , op , ' inválido']].
			cond conditions: (node childNodes collect: [:condNode | self parseCondition: condNode]).
			^cond].
	^(DOCPredefinedCond new)
		condition: predefined;
		renderer: self;
		yourself!

parseConditionFrom: node
	^Error notYetImplemented!

parseConditionFrom: node on: cmdCol
	| cmd |
	cmd := (DOCCondCmd new)
				condition: (self parseCondition: (node selectSingleNode: './condition'));
				yourself.
	self
		parseNode: node
		nodeName: 'ifTrue'
		commandsOn: cmd trueCommands.
	self
		parseNode: node
		nodeName: 'ifFalse'
		commandsOn: cmd falseCommands.
	cmdCol add: cmd!

parseNode: aNode
	"It parses the renderer node and initializes the internal data.

	@param aNode the xml node"

	bodyCommands := OrderedCollection new.
	headerCommands := OrderedCollection new.
	footerCommands := OrderedCollection new.
	self
		parseNode: aNode
		nodeName: 'template'
		commandsOn: bodyCommands.
	self
		parseNode: aNode
		nodeName: 'footer'
		commandsOn: footerCommands.
	self
		parseNode: aNode
		nodeName: 'header'
		commandsOn: headerCommands.
	(aNode selectNodes: './/render') do: 
			[:childNode |
			| childRenderer |
			childRenderer := DOCRenderer
						createRenderer: childNode
						manager: self manager
						parent: self.
			childRenderers
				at: (childNode attributes at: 'id'
						ifAbsent: [self parseError: 'Missing id attribute in child renderer']) asUppercase
				put: childRenderer]!

parseNode: aNode nodeName: aName commandsOn: cmdCol
	"Private - It parses the renderer node and initializes the internal data.

	@param aNode the xml node"

	| subjectNode |
	subjectNode := (aNode selectSingleNode: './' , aName) ifNil: [^self].
	subjectNode childNodes do: 
			[:node |
			(node nodeName sameAs: 'text')
				ifTrue: [self parseTextFrom: node text readStream on: cmdCol]
				ifFalse: 
					[(node nodeName sameAs: 'conditionText')
						ifTrue: [self parseConditionFrom: node on: cmdCol]
						ifFalse: [self error: 'Nodo de texto ' , node nodeName , ' desconocido']]]!

parseTextFrom: aStream on: aCmdCol
	"Private - Receives a stream that has to be parsed to obtain the template that's going to be used to render the objects.

	@param aStream a stream that has the necessary information for this object
	@param aCmdCol una colección donde guardar los comandos"

	| availableCom txtCommand text isCommand readChar |
	availableCom := self availableCommands.
	txtCommand := availableCom at: ''.
	isCommand := false.
	text := String writeStream.
	[aStream atEnd] whileFalse: 
			[readChar := aStream next.
			readChar = $%
				ifTrue: 
					[isCommand
						ifTrue: 
							[| command |
							command := text contents.
							self
								addCommand: (availableCom at: (self parseCommandName: command asUppercase)
										ifAbsent: [self parseError: 'The command ' , (self parseCommandName: command) , ' was not found.'])
								params: (self parseCommandParams: command)
								to: aCmdCol.
							text := String writeStream.
							isCommand := false]
						ifFalse: 
							[| nextChar |
							aStream atEnd
								ifTrue: [self parseError: 'Incomplete command string. You are missing a % character somewhere.'].
							nextChar := aStream next.
							nextChar = $%
								ifFalse: 
									[self
										addCommand: txtCommand
										params: (Array with: text contents)
										to: aCmdCol.
									text := String writeStream.
									isCommand := true].
							text nextPut: nextChar]]
				ifFalse: 
					[(String lineDelimiter includes: readChar) ifTrue: [text cr] ifFalse: [text nextPut: readChar]]].
	isCommand
		ifTrue: [self parseError: 'Incomplete command string. You are missing a % character somewhere.'].
	text isEmpty not
		ifTrue: 
			[self
				addCommand: txtCommand
				params: (Array with: text contents)
				to: aCmdCol]!

possibleChildTypes
	"Returns a Set of classes of renderers that can be childs of this type of renderer"

	^Set with: DOCTextRenderer!

printClassLink: aClass currentClass: curClass on: aStream
	"Private - Prints on the stream a class link if the class exists in the classes that are going to be rendered

	@param aClass the class that's going to be linked
	@param aStream the stream where the link is going to be rendered"

	((curClass isNil or: [aClass ~= curClass]) and: [self manager includesClass: aClass])
		ifTrue: 
			[aStream
				nextPutAll: '<a href="';
				nextPutAll: (self manager
							classFileName: (aClass isMeta ifTrue: [aClass instanceClass] ifFalse: [aClass]));
				nextPutAll: '">';
				nextPutAll: aClass name asString;
				nextPutAll: '</a>']
		ifFalse: [aStream nextPutAll: aClass name asString]!

printPackageLinkOf: aPackage on: aStream
	"Private - Prints on the stream a package link if the package exists in the packages that are going to be rendered

	@param aPackage the package that's going to be linked
	@param aStream the stream where the link is going to be rendered"

	aPackage isNil
		ifTrue: 
			[aStream nextPutAll: '<i>Sin paquete</i>'.
			^self].
	(self manager includesPackage: aPackage)
		ifTrue: 
			[aStream
				nextPutAll: '<a href="';
				nextPutAll: (self manager packageFileName: aPackage);
				nextPutAll: '">';
				nextPutAll: aPackage name asString;
				nextPutAll: '</a>']
		ifFalse: [aStream nextPutAll: aPackage name asString]!

render: anObject on: aStream
	"Renders the specified objet on the stream. Each renderer knows how to render a specific type of object.
	In  this case the renderers can only receive DOCClass objects
	
	@param anObject the object that's going to be rendered.
	@param aStream the stream where the object is going to be rendered"

	| renderableObjects |
	renderableObjects := self getObjectsOf: anObject.
	renderableObjects notEmpty
		ifTrue: 
			[self
				renderCommands: headerCommands
				on: aStream
				for: anObject].
	renderableObjects do: 
			[:obj |
			self
				renderCommands: bodyCommands
				on: aStream
				for: obj].
	renderableObjects notEmpty
		ifTrue: 
			[self
				renderCommands: footerCommands
				on: aStream
				for: anObject]!

renderChild: anObject params: params on: aStream
	"This is a default command method that writes the text on the stream.

	@param anObject the rendered object	
	@param params an array that in this case contains this id of the child renderer to be rendered
	@param aStream the stream where the text is going to be written"

	(childRenderers at: params first asUppercase
		ifAbsent: [self parseError: 'Renderer ' , params first displayString , ' does not exist.'])
			render: anObject
			on: aStream!

renderCommands: commands on: aStream for: obj
	^commands do: [:cmd | cmd execute: obj on: aStream]!

renderCommandsOn: aStream for: obj
	^self bodyCommands do: 
			[:arr |
			| args |
			args := Array
						with: obj
						with: arr second
						with: aStream.
			self perform: arr first withArguments: args]!

renderLabel: anObject params: params on: aStream
	"This command inserts a label text configured before the documentation process.

	@param anObject the rendered object	
	@param params an array that in this case contains one element with the text to be rendered
	@param aStream the stream where the text is going to be written"

	params size >= 1 ifTrue: [aStream nextPutAll: (self manager labelAt: params first)]!

renderTableFor: elements renderBlock: renderBlock tableClass: tableClass colClass: colClass columns: aNumber cellspacing: cellspacing on: aStream
	"Private - Renders all the elements using the render block inside a table of the specified number of columns.
	The render block must recieve two parameters, the first is the element to render and the second is the stream where it has to be rendered.
	Each element is rendered inside a cell of the table.

	@param elements the elements to render
	@param renderBlock the rendering block used to render each element
	@param tableClass a string that contains a css class to add style to bar table. It can be nil
	@param colClass a string that contains a css class to give style to bar cells. It can be nil
	@param aNumber the amount of columns of the table
	@param cellspacing the cellspacing property of the table
	@param aStream the stream where the links are going to be rendered"

	| rendered |
	rendered := 0.
	aStream nextPutAll: '<table width="100%"'.
	cellspacing notNil
		ifTrue: 
			[aStream
				nextPutAll: ' cellspacing="';
				nextPutAll: cellspacing;
				nextPutAll: '"'].
	tableClass notNil
		ifTrue: 
			[aStream
				nextPutAll: ' class="';
				nextPutAll: tableClass;
				nextPutAll: '"'].
	aStream nextPutAll: '>'.
	elements do: 
			[:element |
			rendered \\ aNumber = 0
				ifTrue: 
					[rendered = 0 ifFalse: [aStream nextPutAll: '</tr>'].
					aStream nextPutAll: '<tr>'].
			aStream nextPutAll: '<td'.
			colClass notNil
				ifTrue: 
					[aStream
						nextPutAll: ' class="';
						nextPutAll: colClass;
						nextPutAll: '"'].
			aStream nextPutAll: '>'.
			renderBlock value: aStream value: element.
			aStream
				nextPutAll: '</td>';
				cr.
			rendered := rendered + 1].
	rendered > aNumber
		ifTrue: 
			[| lastRendered |
			lastRendered := rendered \\ aNumber.
			lastRendered ~= 0 ifTrue: [aNumber - lastRendered timesRepeat: [aStream nextPutAll: '<td></td>']]].
	aStream
		nextPutAll: '</tr></table>';
		cr!

renderText: anObject params: params on: aStream
	"This is a default command method that writes the text on the stream.

	@param anObject the rendered object	
	@param params an array that in this case contains one element with the text to be rendered
	@param aStream the stream where the text is going to be written"

	aStream nextPutAll: params first! !
!DOCRenderer categoriesFor: #addCommand:params:!parsing!public! !
!DOCRenderer categoriesFor: #addCommand:params:to:!parsing!public! !
!DOCRenderer categoriesFor: #availableCommands!commands!public!rendering! !
!DOCRenderer categoriesFor: #bodyCommands!accessing!public! !
!DOCRenderer categoriesFor: #bodyCommands:!accessing!public! !
!DOCRenderer categoriesFor: #container!public! !
!DOCRenderer categoriesFor: #getObjectsOf:!private!rendering! !
!DOCRenderer categoriesFor: #initialize!private! !
!DOCRenderer categoriesFor: #isTrue:of:!public!rendering! !
!DOCRenderer categoriesFor: #manager!accessing!public! !
!DOCRenderer categoriesFor: #manager:!accessing!public! !
!DOCRenderer categoriesFor: #parent!accessing!public! !
!DOCRenderer categoriesFor: #parent:!accessing!public! !
!DOCRenderer categoriesFor: #parseCommandName:!parsing!private! !
!DOCRenderer categoriesFor: #parseCommandParams:!parsing!private! !
!DOCRenderer categoriesFor: #parseCondition:!parsing!private! !
!DOCRenderer categoriesFor: #parseConditionFrom:!parsing!private! !
!DOCRenderer categoriesFor: #parseConditionFrom:on:!parsing!private! !
!DOCRenderer categoriesFor: #parseNode:!parsing!public! !
!DOCRenderer categoriesFor: #parseNode:nodeName:commandsOn:!parsing!private! !
!DOCRenderer categoriesFor: #parseTextFrom:on:!parsing!private! !
!DOCRenderer categoriesFor: #possibleChildTypes!public! !
!DOCRenderer categoriesFor: #printClassLink:currentClass:on:!private! !
!DOCRenderer categoriesFor: #printPackageLinkOf:on:!private! !
!DOCRenderer categoriesFor: #render:on:!public!rendering! !
!DOCRenderer categoriesFor: #renderChild:params:on:!commands!public!rendering! !
!DOCRenderer categoriesFor: #renderCommands:on:for:!private!rendering! !
!DOCRenderer categoriesFor: #renderCommandsOn:for:!private!rendering! !
!DOCRenderer categoriesFor: #renderLabel:params:on:!commands!public!rendering! !
!DOCRenderer categoriesFor: #renderTableFor:renderBlock:tableClass:colClass:columns:cellspacing:on:!private!rendering! !
!DOCRenderer categoriesFor: #renderText:params:on:!commands!public!rendering! !

!DOCRenderer class methodsFor!

createRenderer: aNode manager: aManager parent: aParent 
	"Instantiates a renderer class and initializes it's variables using an XMLNode parsed from the template file.

	@param aNode the XML node
	@param aManager the current manager
	@param aParent the object that contains this renderer"

	| rendererClass renderer rendererName |
	rendererName := aNode attributes at: 'type' ifAbsent: ['text'].
	rendererClass := self renderClassFor: rendererName
				ifAbsent: [self parseError: 'Invalid render type ' , rendererName].
	(aParent possibleChildTypes includes: rendererClass) 
		ifFalse: 
			[self 
				parseError: 'Renderers of type ' , aParent class rendererName 
						, ' can''t contain renderers of type ' , rendererClass rendererName].
	renderer := rendererClass forManager: aManager.
	renderer parseNode: aNode.
	renderer parent: aParent.
	^renderer!

forManager: aDOCSmalltalkManager 
	"Instantiates a Renderer for the specified manager
	
	@param aDOCSmalltalkManager the manager for the renderer object"

	^(self basicNew)
		manager: aDOCSmalltalkManager;
		initialize;
		yourself!

renderClassFor: rendererName ifAbsent: aBlock 
	^self allSubclasses 
		detect: [:renderC | renderC rendererName notNil and: [renderC rendererName = rendererName]]
		ifNone: [aBlock value]!

rendererName
	"This is a custom name used to identify the renderer in the xml code, each renderer must have it's own name"

	^self subclassResponsibility! !
!DOCRenderer class categoriesFor: #createRenderer:manager:parent:!public! !
!DOCRenderer class categoriesFor: #forManager:!public! !
!DOCRenderer class categoriesFor: #renderClassFor:ifAbsent:!private! !
!DOCRenderer class categoriesFor: #rendererName!public! !

