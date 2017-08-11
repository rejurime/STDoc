"Filed out from Dolphin Smalltalk 7"!

DOCObject subclass: #DOCProcessor
	instanceVariableNames: 'manager fileName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCProcessor guid: (GUID fromString: '{C9C2770E-9F5C-4EDD-AF80-819FDF7FE214}')!
DOCProcessor comment: 'The subclasses of this class are the ones that handle the documentation process and the template files.
A type of template is associated to each class.
Each class has a different purpose and output. Some handle each class and makes documentation for them, other generates index files to navigate the files, other copy files and instantiates new documenters, etc.

The class that invoques the documenters is the DOCSmalltalkManager when the documentation process starts.'!
!DOCProcessor categoriesForClass!Unclassified! !
!DOCProcessor methodsFor!

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
!DOCProcessor categoriesFor: #fileName!accessing!public! !
!DOCProcessor categoriesFor: #fileName:!accessing!public! !
!DOCProcessor categoriesFor: #loadConfig:!parsing!public! !
!DOCProcessor categoriesFor: #loadFile!parsing!public! !
!DOCProcessor categoriesFor: #manager!accessing!public! !
!DOCProcessor categoriesFor: #manager:!accessing!public! !
!DOCProcessor categoriesFor: #parseDocument:!parsing!public! !
!DOCProcessor categoriesFor: #possibleChildTypes!public! !
!DOCProcessor categoriesFor: #processDoc!document!public! !

!DOCProcessor class methodsFor!

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
!DOCProcessor class categoriesFor: #forManager:!public! !
!DOCProcessor class categoriesFor: #processorClassFor:ifAbsent:!private! !
!DOCProcessor class categoriesFor: #processorName!private! !

