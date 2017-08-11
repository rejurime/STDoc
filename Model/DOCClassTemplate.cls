"Filed out from Dolphin Smalltalk 7"!

DOCProcessor subclass: #DOCClassTemplate
	instanceVariableNames: 'renderers outDir'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCClassTemplate guid: (GUID fromString: '{6B507EDB-98CB-4802-8589-29F466ACAB0E}')!
DOCClassTemplate comment: 'Makes an HTML page using a template file that is parsed beforehand.
The methods to render the classes are called by the DOCSmalltalkManager.

The main rendering method is #processDoc. This method tells each one of the renderers to render itself for each class.

This Documenter makes one file for each one of the classes that are going to be documented. The file name is the class name plus the html extension, if the class is Object the output file will be ''Object.html''.'!
!DOCClassTemplate categoriesForClass!Unclassified! !
!DOCClassTemplate methodsFor!

fileNameFor: anObject
	"Generates the file name that's going to be wrote for this object.

	@param anObject the object
	@returns a string with the file name"

	^self manager classFileName: anObject!

initialize
	super initialize.
	renderers := OrderedCollection new!

loadConfig: aXMLNode
	"Loads extra information that may be in the main template file.
	
	@param anXMLNode is the node that is used to instantiate the object"

	super loadConfig: aXMLNode.
	outDir := aXMLNode attributes at: 'outDir' ifAbsent: [nil]!

outDir
	^outDir!

outDir: anObject
	outDir := anObject!

parseDocument: anXMLNode
	"Initializes the state of the object with information contained in an XML file.
	Parses the renderers and adds them to the list of the template.
	
	@param anXMLNode the root node of an XML file"

	anXMLNode childNodes do: 
			[:xmlNode |
			xmlNode nodeName = 'render'
				ifTrue: 
					[renderers addLast: (DOCRenderer
								createRenderer: xmlNode
								manager: self manager
								parent: self)]
				ifFalse: [self parseError: 'Tag name "' , xmlNode nodeName , '" in template file not recognized']]!

possibleChildTypes
	"Returns a Set of classes of renderers that can be childs of this type of renderer"

	^(super possibleChildTypes)
		add: DOCClassRenderer;
		add: DOCMethodRenderer;
		add: DOCVariableRenderer;
		add: DOCTextRenderer;
		yourself!

processDoc
	"Executes the main purpose of the documenter, in general generates the output documentation files.
	Writes a file for each one of the classes selected for documentarion using the parsed template and the renderers for classes. It contains a collection of renderers that where parsed from the xml file. Those renderers are the ones that handle the output."

	| someClasses len |
	someClasses := self manager classes asOrderedCollection.
	len := someClasses size.
	self manager progressText: 'Making documentation for classes...'.
	self manager progressValue: 0.
	1 to: len
		do: 
			[:index |
			| aClass |
			aClass := someClasses at: index.
			self renderize: aClass.
			self manager progressValue: index / len * 100]!

renderers
	^renderers!

renderers: anObject
	renderers := anObject!

renderize: anObject
	"Private - Iterates the renderers and generates the output for the object.

	@param anObject any object that can be documented. It may be a class or a package"

	| str |
	str := self manager
				createOutFileNamed: (self outDir ifNil: [''] ifNotNil: [:d | File appendPathDelimiter: d])
						, (self fileNameFor: anObject).
	self renderers do: [:renderer | renderer render: anObject on: str].
	^str close! !
!DOCClassTemplate categoriesFor: #fileNameFor:!document!private! !
!DOCClassTemplate categoriesFor: #initialize!private! !
!DOCClassTemplate categoriesFor: #loadConfig:!parsing!public! !
!DOCClassTemplate categoriesFor: #outDir!accessing!private! !
!DOCClassTemplate categoriesFor: #outDir:!accessing!private! !
!DOCClassTemplate categoriesFor: #parseDocument:!parsing!public! !
!DOCClassTemplate categoriesFor: #possibleChildTypes!public! !
!DOCClassTemplate categoriesFor: #processDoc!document!public! !
!DOCClassTemplate categoriesFor: #renderers!accessing!private! !
!DOCClassTemplate categoriesFor: #renderers:!accessing!private! !
!DOCClassTemplate categoriesFor: #renderize:!document!private! !

!DOCClassTemplate class methodsFor!

processorName
	"The name of the listener used in the XML files"

	^'class'! !
!DOCClassTemplate class categoriesFor: #processorName!private! !

