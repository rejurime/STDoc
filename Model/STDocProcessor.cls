"Filed out from Dolphin Smalltalk 7"!

STDocObject subclass: #STDocProcessor
	instanceVariableNames: 'manager fileName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocProcessor guid: (GUID fromString: '{c9c2770e-9f5c-4edd-af80-819fdf7fe214}')!
STDocProcessor comment: 'The subclasses of this class are the ones that handle the documentation process and the template files.
A type of template is associated to each class.
Each class has a different purpose and output. Some handle each class and makes documentation for them, other generates index files to navigate the files, other copy files and instantiates new documenters, etc.

The class that invoques the documenters is the DOCSmalltalkManager when the documentation process starts.'!
!STDocProcessor categoriesForClass!Unclassified! !
!STDocProcessor methodsFor!

fileName
	^fileName!

fileName: anObject
	fileName := anObject!

loadConfig: aXMLNode
	"Loads extra information that may be in the main template file.
	
	@param anXMLNode is the node that is used to instantiate the object"

	!

loadFile
	| doc root |
	doc := IXMLDOMDocument new.
	doc loadURL: (self manager config makeTemplateAbsolute: self fileName).
	root := doc documentElement.
	self parseDocument: root!

manager
	^manager!

manager: anObject
	manager := anObject!

parseDocument: anXMLNode
	"Initializes the state of the object with information contained in an XML file.
	
	@param anXMLNode the root node of an XML file"

	^self subclassResponsibility!

possibleChildTypes
	"Returns a Set of classes of renderers that can be childs of this type of renderer"

	^Set new!

processDoc
	"Executes the main purpose of the documenter, in general generates the output documentation files"

	^self subclassResponsibility! !
!STDocProcessor categoriesFor: #fileName!accessing!public! !
!STDocProcessor categoriesFor: #fileName:!accessing!public! !
!STDocProcessor categoriesFor: #loadConfig:!parsing!public! !
!STDocProcessor categoriesFor: #loadFile!parsing!public! !
!STDocProcessor categoriesFor: #manager!accessing!public! !
!STDocProcessor categoriesFor: #manager:!accessing!public! !
!STDocProcessor categoriesFor: #parseDocument:!parsing!public! !
!STDocProcessor categoriesFor: #possibleChildTypes!public! !
!STDocProcessor categoriesFor: #processDoc!document!public! !

!STDocProcessor class methodsFor!

forManager: aDOCSmalltalkManager
	^(self basicNew)
		manager: aDOCSmalltalkManager;
		initialize;
		yourself!

processorClassFor: processorName ifAbsent: aBlock
	^self allSubclasses
		detect: [:procC | procC processorName notNil and: [procC processorName = processorName]]
		ifNone: [aBlock value]!

processorName
	"The name of the listener used in the XML files"

	^self subclassResponsibility! !
!STDocProcessor class categoriesFor: #forManager:!public! !
!STDocProcessor class categoriesFor: #processorClassFor:ifAbsent:!private! !
!STDocProcessor class categoriesFor: #processorName!private! !

