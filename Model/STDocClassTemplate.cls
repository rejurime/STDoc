"Filed out from Dolphin Smalltalk 7"!

STDocProcessor subclass: #STDocClassTemplate
	instanceVariableNames: 'renderers outDir'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocClassTemplate guid: (GUID fromString: '{6b507edb-98cb-4802-8589-29f466acab0e}')!
STDocClassTemplate comment: 'Makes an HTML page using a template file that is parsed beforehand.
The methods to render the classes are called by the DOCSmalltalkManager.

The main rendering method is #processDoc. This method tells each one of the renderers to render itself for each class.

This Documenter makes one file for each one of the classes that are going to be documented. The file name is the class name plus the html extension, if the class is Object the output file will be ''Object.html''.'!
!STDocClassTemplate categoriesForClass!Unclassified! !
!STDocClassTemplate methodsFor!

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
					[renderers addLast: (STDocRenderer
								createRenderer: xmlNode
								manager: self manager
								parent: self)]
				ifFalse: [self parseError: 'Tag name "' , xmlNode nodeName , '" in template file not recognized']]!

possibleChildTypes
	"Returns a Set of classes of renderers that can be childs of this type of renderer"

	^(super possibleChildTypes)
		add: STDocClassRenderer;
		add: STDocMethodRenderer;
		add: STDocVariableRenderer;
		add: STDocTextRenderer;
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
!STDocClassTemplate categoriesFor: #fileNameFor:!document!private! !
!STDocClassTemplate categoriesFor: #initialize!private! !
!STDocClassTemplate categoriesFor: #loadConfig:!parsing!public! !
!STDocClassTemplate categoriesFor: #outDir!accessing!private! !
!STDocClassTemplate categoriesFor: #outDir:!accessing!private! !
!STDocClassTemplate categoriesFor: #parseDocument:!parsing!public! !
!STDocClassTemplate categoriesFor: #possibleChildTypes!public! !
!STDocClassTemplate categoriesFor: #processDoc!document!public! !
!STDocClassTemplate categoriesFor: #renderers!accessing!private! !
!STDocClassTemplate categoriesFor: #renderers:!accessing!private! !
!STDocClassTemplate categoriesFor: #renderize:!document!private! !

!STDocClassTemplate class methodsFor!

processorName
	"The name of the listener used in the XML files"

	^'class'! !
!STDocClassTemplate class categoriesFor: #processorName!private! !

