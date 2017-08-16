"Filed out from Dolphin Smalltalk 7"!

ValuePresenter subclass: #DoubleListPresenter
	instanceVariableNames: 'optionList selectionList allowRepeat maxSelections'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DoubleListPresenter guid: (GUID fromString: '{e4469a0a-38cd-450f-a320-53b31b296eb1}')!
DoubleListPresenter comment: ''!
!DoubleListPresenter categoriesForClass!Unclassified! !
!DoubleListPresenter methodsFor!

btnAdd
	optionList selections do: 
			[:anElement | 
			(self checkAdd: anElement) 
				ifTrue: 
					[self value add: anElement.
					self updateSelections]]!

btnRemove
	selectionList selections do: 
			[:anElement | 
			self value remove: anElement.
			self updateSelections]!

checkAdd: anElement 
	^((self value includes: anElement) not or: [allowRepeat]) 
		and: [self value size < maxSelections or: [maxSelections = -1]]!

createComponents
	super createComponents.
	selectionList := self add: ListPresenter new name: 'selectList'.
	optionList := self add: ListPresenter new name: 'optionList'!

initialize
	super initialize.
	maxSelections := -1.
	allowRepeat := false!

onValueChanged
	self updateSelections.
	super onValueChanged!

options
	^optionList model!

options: anObject 
	optionList model list: anObject.
	optionList view refreshContents!

updateSelections
	selectionList model list: self value.
	selectionList view refreshContents! !
!DoubleListPresenter categoriesFor: #btnAdd!public! !
!DoubleListPresenter categoriesFor: #btnRemove!public! !
!DoubleListPresenter categoriesFor: #checkAdd:!private! !
!DoubleListPresenter categoriesFor: #createComponents!public! !
!DoubleListPresenter categoriesFor: #initialize!private! !
!DoubleListPresenter categoriesFor: #onValueChanged!public! !
!DoubleListPresenter categoriesFor: #options!accessing!public! !
!DoubleListPresenter categoriesFor: #options:!accessing!public! !
!DoubleListPresenter categoriesFor: #updateSelections!private! !

!DoubleListPresenter class methodsFor!

defaultModel
	^OrderedCollection new!

options: aCollection 
	^(self new)
		options: aCollection;
		yourself!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ContainerView) 34 15 0 0 34 2 8 1409286144 131073 416 0 0 0 5 0 0 0 416 852230 ##(Smalltalk.FramingLayout) 170 176 34 6 410 ##(Smalltalk.ListBox) 34 17 0 416 34 2 8 1412499713 1025 544 590662 2 ##(Smalltalk.ListModel) 138 144 8 #() 0 1310726 ##(Smalltalk.IdentitySearchPolicy) 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 5 0 0 0 544 0 8 4294903279 459270 ##(Smalltalk.Message) #displayString 8 #() 656 false 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 1 1 930 271 531 544 882 #horizontalExtent: 8 #(0) 544 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 135 0 0 0 9 1 0 0] 8 #() 930 193 193 0 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 1 1122 #fixedPreviousLeft 1 1122 #fixedParentTop 1 1122 #fixedParentBottom 1 410 ##(Smalltalk.ListBox) 34 17 0 416 34 2 8 1412499713 1025 1200 610 138 144 656 0 688 706 736 0 5 0 0 0 1200 0 8 4294903279 770 #displayString 8 #() 656 false 818 138 144 34 2 882 #createAt:extent: 34 2 930 471 1 930 271 531 1200 882 #horizontalExtent: 8 #(0) 1200 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 235 0 0 0 0 0 0 0 114 1 0 0 9 1 0 0] 8 #() 1072 0 27 1090 1122 #fixedPreviousRight 201 1122 #fixedParentRight 1 1168 1 1184 1 410 ##(Smalltalk.ContainerView) 34 15 0 416 34 2 8 1140850688 131073 1600 0 0 0 5 0 0 0 1600 482 170 176 34 2 410 ##(Smalltalk.ContainerView) 34 15 0 1600 34 2 8 1140850688 131073 1712 0 0 0 5 0 0 0 1712 656390 ##(Smalltalk.GridLayout) 1 3 1 21 170 192 656 0 818 138 144 34 1 882 #createAt:extent: 34 2 930 17 185 930 169 141 1712 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 8 0 0 0 92 0 0 0 92 0 0 0 162 0 0 0] 34 2 410 ##(Smalltalk.PushButton) 34 20 0 1712 34 2 8 1140924416 1 1984 0 0 0 5 0 0 0 1984 0 8 4294903585 1180998 4 ##(Smalltalk.CommandDescription) #btnAdd 8 '>>' 1 1 0 0 false 0 0 0 818 138 144 34 3 882 #createAt:extent: 34 2 930 1 1 930 169 61 1984 882 #isEnabled: 8 #(false) 1984 882 #text: 34 1 8 '>>' 1984 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 84 0 0 0 30 0 0 0] 8 #() 1072 0 29 410 ##(Smalltalk.PushButton) 34 20 0 1712 34 2 8 1140924416 1 2352 0 0 0 5 0 0 0 2352 0 8 4294903585 2066 #btnRemove 8 '<<' 1 1 0 0 false 0 0 0 818 138 144 34 3 882 #createAt:extent: 34 2 930 1 81 930 169 61 2352 882 #isEnabled: 8 #(false) 2352 882 #text: 34 1 8 '<<' 2352 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 40 0 0 0 84 0 0 0 70 0 0 0] 8 #() 1072 0 29 1072 0 27 1090 1136 7 1584 -5 1122 #centerRelativeParentHeight 524806 ##(Smalltalk.Fraction) 3 5 1122 #fixedViewTop 141 170 192 656 590342 ##(Smalltalk.Rectangle) 930 11 11 930 11 11 818 138 144 34 1 882 #createAt:extent: 34 2 930 271 1 930 201 531 1600 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 135 0 0 0 0 0 0 0 235 0 0 0 9 1 0 0] 34 1 1712 1072 0 27 1090 1122 #centerRelativeParentWidth 2738 3 5 1122 #fixedViewLeft 201 1168 1 1184 1 170 192 34 4 544 8 'optionList' 1200 8 'selectList' 0 818 138 144 34 1 882 #createAt:extent: 34 2 930 2731 21 930 741 531 416 1010 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 85 5 0 0 236 255 255 255 199 6 0 0 245 0 0 0] 34 3 1600 544 1200 1072 0 27 )! !
!DoubleListPresenter class categoriesFor: #defaultModel!public! !
!DoubleListPresenter class categoriesFor: #options:!public! !
!DoubleListPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

