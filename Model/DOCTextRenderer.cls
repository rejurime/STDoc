"Filed out from Dolphin Smalltalk 7"!

DOCRenderer subclass: #DOCTextRenderer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCTextRenderer guid: (GUID fromString: '{CAD47FF8-F359-48CA-BCBD-D48F6366B016}')!
DOCTextRenderer comment: ''!
!DOCTextRenderer categoriesForClass!Unclassified! !
!DOCTextRenderer methodsFor!

possibleChildTypes
	"Returns a Set of classes of renderers that can be childs of this type of renderer"

	^(super possibleChildTypes)
		add: DOCIndexRenderer;
		add: DOCClassRenderer;
		add: DOCMethodRenderer;
		add: DOCVariableRenderer;
		yourself! !
!DOCTextRenderer categoriesFor: #possibleChildTypes!public! !

!DOCTextRenderer class methodsFor!

rendererName
	"This is a custom name used to identify the renderer in the xml code, each renderer must have it's own name"

	^'text'! !
!DOCTextRenderer class categoriesFor: #rendererName!public! !

