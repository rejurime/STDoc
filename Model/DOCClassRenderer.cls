"Filed out from Dolphin Smalltalk 7"!

DOCClassTemplateRenderer subclass: #DOCClassRenderer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCClassRenderer guid: (GUID fromString: '{0C4741F2-F128-4994-B58D-A58EB2BEFB28}')!
DOCClassRenderer comment: ''!
!DOCClassRenderer categoriesForClass!Unclassified! !
!DOCClassRenderer methodsFor!

availableCommands
	"The available renderers that can be used to render objects

	@returns a lookup table that has as key the command name and as value the method selector"

	^(super availableCommands)
		at: 'CLASSID' put: #renderClassID:params:on:;
		at: 'CLASSNAME' put: #renderClassName:params:on:;
		at: 'CLASSDEFINITION' put: #renderClassDefinition:params:on:;
		at: 'CLASSPACKAGE' put: #renderClassPackage:params:on:;
		at: 'CLASSSUPERCLASSES' put: #renderClassSuperclasses:params:on:;
		at: 'CLASSSUPERCLASS' put: #renderClassSuperclass:params:on:;
		at: 'CLASSCOMMENT' put: #renderClassComment:params:on:;
		at: 'CLASSHIERARCHY' put: #renderClassHierarchy:params:on:;
		yourself!

possibleChildTypes
	"Returns a Set of classes of renderers that can be childs of this type of renderer"

	^(super possibleChildTypes)
		add: DOCClassRenderer;
		add: DOCMethodRenderer;
		add: DOCVariableRenderer;
		yourself!

renderClassComment: aClass params: params on: aStream
	"Renders the class comment.
	
	@param aClass the class
	@param params parameters for the render command
	@param aStream the stream where you are going to render the result"

	params notEmpty ifTrue: [aStream nextPutAll: params first].
	DOCSUtils makeHTMLString: aClass docComment fullComment on: aStream!

renderClassDefinition: aClass params: params on: aStream
	"Renders the class definition on the stream.
	
	@param aClass the class
	@param params parameters for the render command
	@param aStream the stream where you are going to render the class name"

	aClass classObject printDefinitionOn: aStream!

renderClassHierarchy: aClass params: params on: aStream
	"Renders a tree for tge class that shows the superclasses and the subclasses.
	
	@param aClass the class
	@param params parameters for the render command
	@param aStream the stream where you are going to render the class hierarchy"

	| spaces superclasses |
	superclasses := aClass superClasses.
	superclasses addLast: aClass.
	spaces := ''.
	superclasses do: 
			[:cls |
			aStream
				nextPutAll: spaces;
				nextPutAll: '<b>'.
			self
				printClassLink: cls
				currentClass: aClass
				on: aStream.
			aStream
				nextPutAll: '</b><br />';
				cr.
			spaces := spaces , '&nbsp;&nbsp;&nbsp;'].
	aClass subclasses do: 
			[:cls |
			(self manager includesClass: cls)
				ifTrue: 
					[aStream
						nextPutAll: spaces;
						nextPutAll: '<b>'.
					self
						printClassLink: cls
						currentClass: aClass
						on: aStream.
					aStream
						nextPutAll: '</b><br />';
						cr]]!

renderClassID: aClass params: params on: aStream
	"Renders the class name on the stream.
	
	@param aClass the class name
	@param params parameters for the render command
	@param aStream the stream where you are going to render the class name"

	aStream nextPutAll: (DOCSUtils makeIDString: aClass name asString)!

renderClassName: aClass params: params on: aStream
	"Renders the class name on the stream.
	
	@param aClass the class name
	@param params parameters for the render command
	@param aStream the stream where you are going to render the class name"

	aStream nextPutAll: aClass name asString!

renderClassPackage: aClass params: params on: aStream
	"Renders the class package name on the stream.
	
	@param aClass the class
	@param params parameters for the render command
	@param aStream the stream where you are going to render the package name"

	self printPackageLinkOf: aClass package on: aStream!

renderClassSuperclass: aClass params: params on: aStream
	"Renders the inmediate superclass name.
	
	@param aClass the class
	@param params parameters for the render command
	@param aStream the stream where you are going to render the class name"

	aClass superclass
		ifNil: [aStream nextPutAll: '<code>nil</code>']
		ifNotNil: 
			[:superClass |
			aStream nextPutAll: '<b>'.
			self
				printClassLink: superClass
				currentClass: aClass
				on: aStream.
			aStream nextPutAll: '</b>']!

renderClassSuperclasses: aClass params: params on: aStream
	"Renders the superclasses tree for the class.
	
	@param aClass the class
	@param params parameters for the render command
	@param aStream the stream where you are going to render the class name"

	| spaces superclasses |
	superclasses := aClass superClasses.
	spaces := ''.
	superclasses do: 
			[:cls |
			aStream
				nextPutAll: spaces;
				nextPutAll: '<b>'.
			self
				printClassLink: cls
				currentClass: aClass
				on: aStream.
			aStream
				nextPutAll: '</b><br />';
				cr.
			spaces := spaces , '&nbsp;&nbsp;&nbsp;']! !
!DOCClassRenderer categoriesFor: #availableCommands!public! !
!DOCClassRenderer categoriesFor: #possibleChildTypes!public! !
!DOCClassRenderer categoriesFor: #renderClassComment:params:on:!public! !
!DOCClassRenderer categoriesFor: #renderClassDefinition:params:on:!public! !
!DOCClassRenderer categoriesFor: #renderClassHierarchy:params:on:!public! !
!DOCClassRenderer categoriesFor: #renderClassID:params:on:!public! !
!DOCClassRenderer categoriesFor: #renderClassName:params:on:!public! !
!DOCClassRenderer categoriesFor: #renderClassPackage:params:on:!public! !
!DOCClassRenderer categoriesFor: #renderClassSuperclass:params:on:!public! !
!DOCClassRenderer categoriesFor: #renderClassSuperclasses:params:on:!public! !

!DOCClassRenderer class methodsFor!

rendererName
	"This is a custom name used to identify the renderer in the xml code, each renderer must have it's own name"

	^'class'! !
!DOCClassRenderer class categoriesFor: #rendererName!public! !

