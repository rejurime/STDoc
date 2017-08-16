"Filed out from Dolphin Smalltalk 7"!

STDocProcessor subclass: #STDocTemplateSpec
	instanceVariableNames: 'filesToCopy'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocTemplateSpec guid: (GUID fromString: '{fff2e958-806f-4656-86f6-8f32dc98a44e}')!
STDocTemplateSpec comment: 'This class handles the main template file ''template.xml''.
It is the main Documenter, It instantiates all the other documenter based on the template file used.

It also handles the copying of extra files that can be used in the pages.'!
!STDocTemplateSpec categoriesForClass!Unclassified! !
!STDocTemplateSpec methodsFor!

addConfig: aNode
	"Sets a configuration item in the documentation manager.
	Configuration keys are PrivIcon, PubIcon, OverIcon, ClassIcon, BlankIcon, DefaultStyle.

	@param aNode the XML node with info about the configuration setting"

	| key value |
	key := (aNode attributes at: 'key'
				ifAbsent: [^self parseError: 'Missing "key" attribute in a config tag']) asUppercase.
	value := aNode attributes at: 'value'
				ifAbsent: [^self parseError: 'Missing "value" attribute in a config tag'].
	key = 'PRIVICON'
		ifTrue: 
			[self manager privIcon: value.
			^self].
	key = 'PUBICON'
		ifTrue: 
			[self manager pubIcon: value.
			^self].
	key = 'OVERICON'
		ifTrue: 
			[self manager overIcon: value.
			^self].
	key = 'DEFAULTSTYLE'
		ifTrue: 
			[self manager defaultStyle: value.
			^self].
	key = 'CLASSICON'
		ifTrue: 
			[self manager classIcon: value.
			^self].
	key = 'BLANKICON'
		ifTrue: 
			[self manager blankIcon: value.
			^self].
	self error: 'Configuration setting ' , key , ' not found.'!

addFileNode: aNode
	"Enquese a file to be copied when the template is made.

	@param aNode the XML node with info about the file"

	| src dest replace |
	src := aNode attributes at: 'src'
				ifAbsent: 
					[aNode attributes at: 'source' ifAbsent: [^self parseError: 'Missing "src" attribute in a file tag']].
	dest := aNode attributes at: 'dest'
				ifAbsent: 
					[aNode attributes at: 'destiny'
						ifAbsent: [^self parseError: 'Missing "dest" attribute in a file tag']].
	replace := (aNode attributes at: 'replace' ifAbsent: ['true']) sameAs: 'true'.
	filesToCopy add: (Array
				with: src
				with: dest
				with: replace)!

addTemplateNode: aNode
	"Adds a template to the manager that contains this object.

	@param aNode the XML node with info about the file"

	| type fName |
	type := aNode attributes at: 'type' ifAbsent: ['default'].
	fName := aNode attributes at: 'templateFile'
				ifAbsent: [^self parseError: 'Missing "templateFile" attribute in a <template> tag'].
	self manager
		addProcessor: (((STDocProcessor processorClassFor: type
				ifAbsent: [^self parseError: 'The type "' , type , '" of template does not exist'])
					forManager: self manager)
				fileName: fName;
				loadConfig: aNode;
				loadFile;
				yourself)!

filesToCopy
	^filesToCopy!

filesToCopy: anObject
	filesToCopy := anObject!

initialize
	super initialize.
	filesToCopy := OrderedCollection new!

parseDocument: anXMLNode
	"Initializes the state of the object with information contained in an XML file.
	
	@param anXMLNode the root node of an XML file"

	| childNodes len |
	childNodes := anXMLNode childNodes.
	len := childNodes size.
	self manager progressText: 'Parsing template...'.
	self manager progressValue: 0.
	1 to: len
		do: 
			[:index |
			| node nodeName |
			node := childNodes at: index.
			nodeName := node nodeName.
			nodeName = 'file'
				ifTrue: [self addFileNode: node]
				ifFalse: 
					[nodeName = 'template'
						ifTrue: [self addTemplateNode: node]
						ifFalse: 
							[nodeName = 'config'
								ifTrue: [self addConfig: node]
								ifFalse: [self parseError: 'Tag ' , nodeName , ' not recognized in the main template file']]].
			self manager progressValue: index / len * 100]!

processDoc
	"Executes the main purpose of the documenter, in general generates the output documentation files"

	| len |
	self manager progressText: 'Copying files...'.
	self manager progressValue: 0.
	len := filesToCopy size.
	1 to: len
		do: 
			[:n |
			| fCopy srcDir destDir |
			fCopy := filesToCopy at: n.
			srcDir := self manager config makeTemplateAbsolute: fCopy first.
			destDir := self manager config makeDocAbsolute: fCopy second.
			(fCopy third or: [(File exists: destDir) not]) ifTrue: [File copy: srcDir to: destDir].
			self manager progressValue: n / len * 100]! !
!STDocTemplateSpec categoriesFor: #addConfig:!parsing!public! !
!STDocTemplateSpec categoriesFor: #addFileNode:!parsing!public! !
!STDocTemplateSpec categoriesFor: #addTemplateNode:!parsing!public! !
!STDocTemplateSpec categoriesFor: #filesToCopy!accessing!private! !
!STDocTemplateSpec categoriesFor: #filesToCopy:!accessing!private! !
!STDocTemplateSpec categoriesFor: #initialize!private! !
!STDocTemplateSpec categoriesFor: #parseDocument:!parsing!public! !
!STDocTemplateSpec categoriesFor: #processDoc!document!public! !

!STDocTemplateSpec class methodsFor!

processorName
	"The name of the listener used in the XML files"

	^'templateSpec'! !
!STDocTemplateSpec class categoriesFor: #processorName!private! !

