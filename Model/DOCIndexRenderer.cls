"Filed out from Dolphin Smalltalk 7"!

DOCRenderer subclass: #DOCIndexRenderer
	instanceVariableNames: 'type fileName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCIndexRenderer guid: (GUID fromString: '{7C97BDA5-AE36-4E72-8D3B-ABC8F70DC07B}')!
DOCIndexRenderer comment: ''!
!DOCIndexRenderer categoriesForClass!Unclassified! !
!DOCIndexRenderer methodsFor!

availableCommands
	"The available renderers that can be used to render objects

	@returns a lookup table that has as key the command name and as value the method selector"

	^(super availableCommands)
		at: 'INDEXBAR' put: #renderIndexBar:params:on:;
		at: 'INDEXBODY' put: #renderIndexBody:params:on:;
		at: 'INDEXNAME' put: #renderIndexName:params:on:;
		yourself!

fileName
	^fileName!

fileName: anObject
	fileName := anObject!

parseNode: aNode
	"It parses the renderer node and initializes the internal data.

	@param aNode the xml node"

	super parseNode: aNode.
	fileName := aNode attributes at: 'fileName' ifAbsent: []!

printClassLinkOf: aClass on: aStream
	"Private - Prints on the stream a class link if the class exists in the classes that are going to be rendered

	@param aClass the class that's going to be linked
	@param aStream the stream where the link is going to be rendered"

	(self manager includesClass: aClass)
		ifTrue: 
			[aStream
				nextPutAll: '<a href="javascript:browseClass(''';
				nextPutAll: (self manager classFileName: aClass);
				nextPutAll: '.html'');">';
				nextPutAll: aClass name asString;
				nextPutAll: '</a>']
		ifFalse: [aStream nextPutAll: aClass name asString]!

renderIndexBar: anIndex params: params on: aStream
	"Renders the index name in the stream.
	
	@param aClass the class
	@param params parameters for the render command
	@param aStream the stream where you are going to render the result"

	| tableClass colClass |
	params size = 2
		ifTrue: 
			[tableClass := params first = '' ifFalse: [params first].
			colClass := params second = '' ifFalse: [params second]].
	self
		renderTableFor: anIndex activeIndexesLinks
		renderBlock: 
			[:str :lnk |
			str
				nextPutAll: '<a href="';
				nextPutAll: lnk second;
				nextPutAll: '">';
				nextPutAll: lnk first;
				nextPutAll: '</a>']
		tableClass: tableClass
		colClass: colClass
		columns: 2
		cellspacing: nil
		on: aStream!

renderIndexBody: anIndex params: params on: aStream
	"Renders the index body in the stream.
	
	@param anIndex the index that contains this renderer
	@param params parameters for the render command
	@param aStream the stream where you are going to render the result"

	anIndex renderIndex: type on: aStream!

renderIndexName: anIndex params: params on: aStream
	"Renders the index name in the stream.
	
	@param anIndex the index that contains this renderer
	@param params parameters for the render command
	@param aStream the stream where you are going to render the result"

	anIndex renderIndexName: type on: aStream!

type
	^type!

type: anObject
	type := anObject! !
!DOCIndexRenderer categoriesFor: #availableCommands!public! !
!DOCIndexRenderer categoriesFor: #fileName!accessing!public! !
!DOCIndexRenderer categoriesFor: #fileName:!accessing!public! !
!DOCIndexRenderer categoriesFor: #parseNode:!public! !
!DOCIndexRenderer categoriesFor: #printClassLinkOf:on:!private! !
!DOCIndexRenderer categoriesFor: #renderIndexBar:params:on:!public! !
!DOCIndexRenderer categoriesFor: #renderIndexBody:params:on:!public! !
!DOCIndexRenderer categoriesFor: #renderIndexName:params:on:!public! !
!DOCIndexRenderer categoriesFor: #type!accessing!public! !
!DOCIndexRenderer categoriesFor: #type:!accessing!public! !

!DOCIndexRenderer class methodsFor!

rendererName
	"This is a custom name used to identify the renderer in the xml code, each renderer must have it's own name"

	^'index'! !
!DOCIndexRenderer class categoriesFor: #rendererName!public! !

