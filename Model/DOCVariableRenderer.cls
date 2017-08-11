"Filed out from Dolphin Smalltalk 7"!

DOCClassTemplateRenderer subclass: #DOCVariableRenderer
	instanceVariableNames: 'varType'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCVariableRenderer guid: (GUID fromString: '{49B74884-0F99-4FB4-9E50-3555950F3C6A}')!
DOCVariableRenderer comment: ''!
!DOCVariableRenderer categoriesForClass!Unclassified! !
!DOCVariableRenderer methodsFor!

availableCommands
	"The available renderers that can be used to render objects

	@returns a lookup table that has as key the command name and as value the method selector"

	^(super availableCommands)
		at: 'VARNAME' put: #renderVarName:params:on:;
		at: 'VARID' put: #renderVarID:params:on:;
		at: 'VARCOMMENT' put: #renderVarComment:params:on:;
		at: 'VARTYPE' put: #renderVarType:params:on:;
		yourself!

getObjectsOf: aDOCClass
	"Private - Returns a Collection with objects that are going to be rendered"

	^varType = 'INST'
		ifTrue: [aDOCClass variables]
		ifFalse: 
			[varType = 'CLASS'
				ifTrue: [aDOCClass classVariables]
				ifFalse: 
					[varType = 'INSTCLASS'
						ifTrue: [aDOCClass instanceClassVariables]
						ifFalse: 
							[varType = 'ALL'
								ifTrue: [aDOCClass variables , aDOCClass classVariables , aDOCClass instanceClassVariables]
								ifFalse: [OrderedCollection new]]]]!

parseNode: aNode
	super parseNode: aNode.
	varType := (aNode attributes at: 'varType' ifAbsent: ['ALL']) asUppercase!

renderVarComment: aVariable params: params on: aStream
	"Renders the variable associated comment of the class.
	
	@param aVariable the variable name
	@param params parameters for the render command
	@param aStream the stream where you are going to render"

	aVariable comment isEmpty
		ifTrue: [aStream nextPutAll: '&nbsp;']
		ifFalse: [aStream nextPutAll: aVariable comment displayString]!

renderVarID: aVariable params: params on: aStream
	"Renders the variable name.
	
	@param aVariable the variable name
	@param params parameters for the render command
	@param aStream the stream where you are going to render"

	aStream nextPutAll: (DOCSUtils makeIDString: aVariable name displayString)!

renderVarName: aVariable params: params on: aStream
	"Renders the variable name.
	
	@param aVariable the variable name
	@param params parameters for the render command
	@param aStream the stream where you are going to render"

	aStream nextPutAll: aVariable name displayString!

renderVarType: aVariable params: params on: aStream
	"Renders the variable type. If the variable is of the instance it renders an 'I', if it is a class variable it renders 'C' and if it is an class instance variable it renders 'IC'
	
	@param aVariable the variable name
	@param params parameters for the render command
	@param aStream the stream where you are going to render"

	aStream nextPutAll: (aVariable isClass
				ifTrue: ['C']
				ifFalse: [aVariable isInst ifTrue: ['I'] ifFalse: ['IC']])! !
!DOCVariableRenderer categoriesFor: #availableCommands!public! !
!DOCVariableRenderer categoriesFor: #getObjectsOf:!private! !
!DOCVariableRenderer categoriesFor: #parseNode:!public! !
!DOCVariableRenderer categoriesFor: #renderVarComment:params:on:!public! !
!DOCVariableRenderer categoriesFor: #renderVarID:params:on:!public! !
!DOCVariableRenderer categoriesFor: #renderVarName:params:on:!public! !
!DOCVariableRenderer categoriesFor: #renderVarType:params:on:!public! !

!DOCVariableRenderer class methodsFor!

rendererName
	"This is a custom name used to identify the renderer in the xml code, each renderer must have it's own name"

	^'variable'! !
!DOCVariableRenderer class categoriesFor: #rendererName!public! !

