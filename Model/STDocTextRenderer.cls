"Filed out from Dolphin Smalltalk 7"!

STDocRenderer subclass: #STDocTextRenderer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocTextRenderer guid: (GUID fromString: '{cad47ff8-f359-48ca-bcbd-d48f6366b016}')!
STDocTextRenderer comment: ''!
!STDocTextRenderer categoriesForClass!Unclassified! !
!STDocTextRenderer methodsFor!

possibleChildTypes
	"Returns a Set of classes of renderers that can be childs of this type of renderer"

	^(super possibleChildTypes)
		add: STDocIndexRenderer;
		add: STDocClassRenderer;
		add: STDocMethodRenderer;
		add: STDocVariableRenderer;
		yourself! !
!STDocTextRenderer categoriesFor: #possibleChildTypes!public! !

!STDocTextRenderer class methodsFor!

rendererName
	"This is a custom name used to identify the renderer in the xml code, each renderer must have it's own name"

	^'text'! !
!STDocTextRenderer class categoriesFor: #rendererName!public! !

