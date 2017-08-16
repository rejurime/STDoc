"Filed out from Dolphin Smalltalk 7"!

STDocProcessor subclass: #STDocDefaultProcessor
	instanceVariableNames: 'renderers outFile'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocDefaultProcessor guid: (GUID fromString: '{7a50e236-f9b3-4fd3-8e6d-cfe47bfa2387}')!
STDocDefaultProcessor comment: ''!
!STDocDefaultProcessor categoriesForClass!Unclassified! !
!STDocDefaultProcessor methodsFor!

initialize
	super initialize.
	renderers := OrderedCollection new!

loadConfig: aXMLNode
	"Loads extra information that may be in the main template file.
	
	@param anXMLNode is the node that is used to instantiate the object"

	super loadConfig: aXMLNode.
	outFile := aXMLNode attributes at: 'outFile'
				ifAbsent: [self parseError: 'The template type "default" must have and "outFile" attribute']!

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
		add: STDocTextRenderer;
		yourself!

processDoc
	"Executes the main purpose of the documenter, in general generates the output documentation files.
	Writes a file for each one of the classes selected for documentarion using the parsed template and the renderers for classes. It contains a collection of renderers that where parsed from the xml file. Those renderers are the ones that handle the output."

	| str |
	str := self manager createOutFileNamed: outFile.
	self renderers do: [:renderer | renderer render: nil on: str].
	^str close!

renderers
	^renderers!

renderers: anObject
	renderers := anObject! !
!STDocDefaultProcessor categoriesFor: #initialize!private! !
!STDocDefaultProcessor categoriesFor: #loadConfig:!parsing!public! !
!STDocDefaultProcessor categoriesFor: #parseDocument:!parsing!public! !
!STDocDefaultProcessor categoriesFor: #possibleChildTypes!public! !
!STDocDefaultProcessor categoriesFor: #processDoc!document!public! !
!STDocDefaultProcessor categoriesFor: #renderers!accessing!private! !
!STDocDefaultProcessor categoriesFor: #renderers:!accessing!private! !

!STDocDefaultProcessor class methodsFor!

processorName
	"The name of the listener used in the XML files"

	^'default'! !
!STDocDefaultProcessor class categoriesFor: #processorName!private! !

