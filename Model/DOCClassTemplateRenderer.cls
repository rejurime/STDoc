"Filed out from Dolphin Smalltalk 7"!

DOCRenderer subclass: #DOCClassTemplateRenderer
	instanceVariableNames: 'header footer'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCClassTemplateRenderer guid: (GUID fromString: '{7E802F1C-CB2D-4127-AD5B-14F2C1D5036B}')!
DOCClassTemplateRenderer comment: ''!
!DOCClassTemplateRenderer categoriesForClass!Unclassified! !
!DOCClassTemplateRenderer methodsFor!

parseNode: aNode
	"It parses the renderer node and initializes the internal data.

	@param aNode the xml node"

	super parseNode: aNode.
	header := (aNode selectSingleNode: './header') ifNil: [''] ifNotNil: [:titleNode | titleNode text].
	footer := (aNode selectSingleNode: './footer') ifNil: [''] ifNotNil: [:titleNode | titleNode text]! !
!DOCClassTemplateRenderer categoriesFor: #parseNode:!parsing!public! !

!DOCClassTemplateRenderer class methodsFor!

rendererName
	"This is a custom name used to identify the renderer in the xml code, each renderer must have it's own name"

	^nil! !
!DOCClassTemplateRenderer class categoriesFor: #rendererName!public! !

