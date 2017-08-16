"Filed out from Dolphin Smalltalk 7"!

STDocObject subclass: #STDocRenderCmd
	instanceVariableNames: 'renderer'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocRenderCmd guid: (GUID fromString: '{637b76e4-9b2e-41d4-9a3e-5be530042b80}')!
STDocRenderCmd comment: ''!
!STDocRenderCmd categoriesForClass!Unclassified! !
!STDocRenderCmd methodsFor!

execute: obj on: aStream
	^self subclassResponsibility!

renderer
	^renderer!

renderer: anObject
	renderer := anObject! !
!STDocRenderCmd categoriesFor: #execute:on:!public! !
!STDocRenderCmd categoriesFor: #renderer!accessing!private! !
!STDocRenderCmd categoriesFor: #renderer:!accessing!private! !

!STDocRenderCmd class methodsFor!

for: aRenderer
	^(self new)
		renderer: aRenderer;
		yourself! !
!STDocRenderCmd class categoriesFor: #for:!public! !

