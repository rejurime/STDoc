"Filed out from Dolphin Smalltalk 7"!

DOCProcessor subclass: #DOCIndexTemplate
	instanceVariableNames: 'hierarchy renderers'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCIndexTemplate guid: (GUID fromString: '{7B81FD24-9ED2-4AA7-A278-317BB6E06F93}')!
DOCIndexTemplate comment: 'This Documenter makes files to have quick access to the main documentation files, such as documentation for classes.

The available index types are:
	- Hierarchy Index: The link for the classes are displayed as a hierarchy of all the classes involved.
	- Alphabetical Index: The links to the classes are displayed as a list of alphabetically ordered names.
	- Package Index: A list of documented packages and links to the pages are included.

It generates a file per index type, the name can be specified with the attribute outFile of the index tag in the index template file. If the attribute is not specified it uses a default name.

The Index layout is parsed from an xml file. The index template also tells wich of the indexes are going to be used'!
!DOCIndexTemplate categoriesForClass!Unclassified! !
!DOCIndexTemplate methodsFor!

activeIndexesLinks
	"Returns a collection of arrays of two elements of the indexes that are active and it's page.
	Each array has as first element the name of the index and as second element the page of the index"

	| result |
	result := OrderedCollection new.
	self renderers do: 
			[:r |
			| info |
			info := self indexTypeInfo at: r type.
			result add: (Array with: info first with: (r fileName ifNil: [info second]))].
	^result!

addClass: aClass to: aCollection
	"Private - Adds a class in alphabetical order and and returns the added node as parameter

	@param aClass the class object to be added
	@param aCollection the collection where is going to be added
	@return the added node, or the previous node that was in the collection"

	| nodes |
	nodes := aCollection select: [:n | n first = aClass].
	^nodes isEmpty
		ifTrue: [aCollection add: (Array with: aClass with: self sortedCollection)]
		ifFalse: [nodes first]!

addFullClass: aClass
	"Processes each class and generates output. This method is called by DOCSmalltalkManager to render each of the classes that are going to be documented.

	@param aClass the class to be processed"

	| col |
	col := hierarchy.
	((aClass classObject allSuperclasses)
		addFirst: aClass classObject;
		reverse) do: [:c | col := (self addClass: c to: col) second]!

allClasses
	| classes |
	classes := SortedCollection sortBlock: [:c1 :c2 | c1 name <= c2 name].
	self classesDo: [:c | classes add: c].
	^classes!

classesDo: aBlock
	"Iterates all the classes in the index"

	self iterateClasses: hierarchy do: [:c | aBlock value: c]!

indexTypeInfo
	^(LookupTable new)
		at: #hierarchy
			put: (Array
					with: 'Hierarchy'
					with: '_indexHierarchy.html'
					with: #renderHierarchyIndexOn:);
		at: #alphabetical
			put: (Array
					with: 'Alphabetical'
					with: '_indexAlpha.html'
					with: #renderAlphabeticalIndexOn:);
		at: #package
			put: (Array
					with: 'Packages'
					with: '_indexPackage.html'
					with: #renderPackageIndexOn:);
		yourself!

initialize
	super initialize.
	hierarchy := self sortedCollection.
	renderers := OrderedCollection new!

iterateClasses: aCollection do: aBlock
	"Iterates all the classes from the nodes and it's children

	@param aCollection a collection of nodes
	@param aBlock a monadic valuable that receives a class"

	aCollection do: 
			[:node |
			aBlock value: node first.
			self iterateClasses: node second do: aBlock]!

parseDocument: anXMLNode
	"Initializes the state of the object with information contained in an XML file.
	
	@param anXMLNode the root node of an XML file"

	(anXMLNode selectNodes: './/index') do: 
			[:node |
			self parseRenderer: node
				type: (node attributes at: 'type'
						ifAbsent: [^self parseError: 'Index tag is missing the attribute "type"']) asSymbol]!

parseRenderer: aNode type: indexType
	renderers add: ((DOCIndexRenderer forManager: self manager)
				parent: self;
				type: indexType;
				parseNode: aNode;
				yourself)!

possibleChildTypes
	"Returns a Set of classes of renderers that can be childs of this type of renderer"

	^(super possibleChildTypes)
		add: DOCIndexRenderer;
		add: DOCTextRenderer;
		yourself!

printClassLinkOf: aClass on: aStream
	"Private - Prints on the stream a class link if the class exists in the classes that are going to be rendered

	@param aClass the class that's going to be linked
	@param aStream the stream where the link is going to be rendered"

	(self manager includesClass: aClass)
		ifTrue: 
			[aStream
				nextPutAll: '<a href="javascript:browseClass(''';
				nextPutAll: (self manager classFileName: aClass);
				nextPutAll: ''');">';
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
				nextPutAll: '<a href="javascript:browseClass(''';
				nextPutAll: (self manager packageFileName: aPackage);
				nextPutAll: ''');">';
				nextPutAll: aPackage name asString;
				nextPutAll: '</a>']
		ifFalse: [aStream nextPutAll: aPackage name asString]!

processDoc
	"Executes the main purpose of the documenter, in general generates the output documentation files.
	It organizes the tree of classes and uses the renderers to generate the index files."

	| someClasses len |
	someClasses := self manager classes asOrderedCollection.
	len := someClasses size.
	self manager progressText: 'Making indexes...'.
	self manager progressValue: 0.
	1 to: len
		do: 
			[:index |
			| aClass |
			aClass := someClasses at: index.
			self addFullClass: aClass.
			self manager progressValue: index / len * 50].
	len := renderers size.
	1 to: len
		do: 
			[:index |
			| renderer str |
			renderer := renderers at: index.
			str := FileStream write: (self manager config
								makeDocAbsolute: (renderer fileName ifNil: [(self indexTypeInfo at: renderer type) second]))
						text: true.
			renderer render: self on: str.
			str close.
			self manager progressValue: 50 + (index / len * 50)]!

renderAlphabeticalIndexOn: aStream
	"Private - Creates an index rendered as a class collection ordered by class name. The output file is hierarchyAlpha.html"

	hierarchy isEmpty ifTrue: [^self].
	(self allClasses select: [:c | self manager includesClass: c]) do: 
			[:c |
			self printClassLinkOf: c on: aStream.
			aStream
				nextPutAll: '<br />';
				cr]!

renderers
	^renderers!

renderers: anObject
	renderers := anObject!

renderHierarchyIndexOn: aStream
	"Private - Creates an index rendered as a hierarchy tree. The output file is hierarchyIndex.html"

	hierarchy isEmpty ifTrue: [^self].
	self
		renderNode: hierarchy
		sepparator: '&nbsp;&nbsp;&nbsp;'
		currentSep: ''
		on: aStream!

renderIndex: indexType on: aStream
	"Creates an index of the type specified on the stream specified.

	@param indexType a Symbol object that identifies a type of index
	@param aStream the stream where the index is going to be rendered"

	self perform: (self indexTypeInfo at: indexType) last with: aStream!

renderIndexName: indexType on: aStream
	"Renders the native name of the index.

	@param indexType a Symbol object that identifies a type of index
	@param aStream the stream where the index name is going to be rendered"

	aStream
		nextPutAll: (self indexTypeInfo at: indexType
				ifAbsent: [self parseError: 'Index type "' , indexType displayString , '" does not exist']) first!

renderNode: aCollection sepparator: sepString currentSep: curSepString on: aStream
	"Private - Renders a link for each class in the collection. Each link has the curSepString.
	When this method is called for the childs the sepString is added for the curSepString.

	@param aCollection the classes that are going to be rendered
	@param sepString the string that is added to the curSepString
	@param curSepString the string rendered before the class link
	@param aStream the stream where the index is going to be rendered"

	aCollection do: 
			[:arr |
			aStream nextPutAll: curSepString.
			self printClassLinkOf: arr first on: aStream.
			aStream
				nextPutAll: '<br />';
				cr.
			self
				renderNode: arr second
				sepparator: sepString
				currentSep: curSepString , sepString
				on: aStream]!

renderPackageIndexOn: aStream
	"Private - Creates an index for the packages."

	hierarchy isEmpty ifTrue: [^self].
	(self manager inferedPackages asSortedCollection: [:p1 :p2 | p1 name <= p2 name]) do: 
			[:package |
			self printPackageLinkOf: package on: aStream.
			aStream
				nextPutAll: '<br />';
				cr]!

sortedCollection
	^SortedCollection sortBlock: [:cArr1 :cArr2 | cArr1 first name <= cArr2 first name]! !
!DOCIndexTemplate categoriesFor: #activeIndexesLinks!public!rendering! !
!DOCIndexTemplate categoriesFor: #addClass:to:!hierarchy!private! !
!DOCIndexTemplate categoriesFor: #addFullClass:!document!public! !
!DOCIndexTemplate categoriesFor: #allClasses!hierarchy!private! !
!DOCIndexTemplate categoriesFor: #classesDo:!hierarchy!private! !
!DOCIndexTemplate categoriesFor: #indexTypeInfo!private! !
!DOCIndexTemplate categoriesFor: #initialize!private! !
!DOCIndexTemplate categoriesFor: #iterateClasses:do:!hierarchy!private! !
!DOCIndexTemplate categoriesFor: #parseDocument:!parsing!public! !
!DOCIndexTemplate categoriesFor: #parseRenderer:type:!parsing!private! !
!DOCIndexTemplate categoriesFor: #possibleChildTypes!public! !
!DOCIndexTemplate categoriesFor: #printClassLinkOf:on:!private!rendering! !
!DOCIndexTemplate categoriesFor: #printPackageLinkOf:on:!private! !
!DOCIndexTemplate categoriesFor: #processDoc!document!public!rendering! !
!DOCIndexTemplate categoriesFor: #renderAlphabeticalIndexOn:!private!rendering! !
!DOCIndexTemplate categoriesFor: #renderers!accessing!private! !
!DOCIndexTemplate categoriesFor: #renderers:!accessing!private! !
!DOCIndexTemplate categoriesFor: #renderHierarchyIndexOn:!private!rendering! !
!DOCIndexTemplate categoriesFor: #renderIndex:on:!public!rendering! !
!DOCIndexTemplate categoriesFor: #renderIndexName:on:!public! !
!DOCIndexTemplate categoriesFor: #renderNode:sepparator:currentSep:on:!private!rendering! !
!DOCIndexTemplate categoriesFor: #renderPackageIndexOn:!private!rendering! !
!DOCIndexTemplate categoriesFor: #sortedCollection!private! !

!DOCIndexTemplate class methodsFor!

processorName
	"The name of the listener used in the XML files"

	^'index'! !
!DOCIndexTemplate class categoriesFor: #processorName!private! !

