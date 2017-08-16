"Filed out from Dolphin Smalltalk 7"!

STDocRenderer subclass: #STDocClassTemplateRenderer
	instanceVariableNames: 'header footer'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocClassTemplateRenderer guid: (GUID fromString: '{7e802f1c-cb2d-4127-ad5b-14f2c1d5036b}')!
STDocClassTemplateRenderer comment: ''!
!STDocClassTemplateRenderer categoriesForClass!Unclassified! !
!STDocClassTemplateRenderer methodsFor!

parseNode: aNode
	"It parses the renderer node and initializes the internal data.

	@param aNode the xml node"

	super parseNode: aNode.
	header := (aNode selectSingleNode: './header') ifNil: [''] ifNotNil: [:titleNode | titleNode text].
	footer := (aNode selectSingleNode: './footer') ifNil: [''] ifNotNil: [:titleNode | titleNode text]! !
!STDocClassTemplateRenderer categoriesFor: #parseNode:!parsing!public! !

!STDocClassTemplateRenderer class methodsFor!

rendererName
	"This is a custom name used to identify the renderer in the xml code, each renderer must have it's own name"

	^nil! !
!STDocClassTemplateRenderer class categoriesFor: #rendererName!public! !

