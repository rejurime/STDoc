"Filed out from Dolphin Smalltalk 7"!

DOCObject subclass: #DOCRenderCmd
	instanceVariableNames: 'renderer'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCRenderCmd guid: (GUID fromString: '{637B76E4-9B2E-41D4-9A3E-5BE530042B80}')!
DOCRenderCmd comment: ''!
!DOCRenderCmd categoriesForClass!Unclassified! !
!DOCRenderCmd methodsFor!

execute: obj on: aStream
	^self subclassResponsibility!

renderer
	^renderer!

renderer: anObject
	renderer := anObject! !
!DOCRenderCmd categoriesFor: #execute:on:!public! !
!DOCRenderCmd categoriesFor: #renderer!accessing!private! !
!DOCRenderCmd categoriesFor: #renderer:!accessing!private! !

!DOCRenderCmd class methodsFor!

for: aRenderer
	^(self new)
		renderer: aRenderer;
		yourself! !
!DOCRenderCmd class categoriesFor: #for:!public! !

