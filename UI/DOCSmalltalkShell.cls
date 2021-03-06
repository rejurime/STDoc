"Filed out from Dolphin Smalltalk 7"!

Shell subclass: #DOCSmalltalkShell
	instanceVariableNames: 'renderPrivate renderPublic renderClassPublic renderClassPrivate templateName templateDir docPath packages labels renderRelatedClasses renderSuperclasses'
	classVariableNames: 'DocModel'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCSmalltalkShell guid: (GUID fromString: '{A7A96838-D1A8-40BD-9324-347B3C1C62F7}')!
DOCSmalltalkShell comment: ''!
!DOCSmalltalkShell categoriesForClass!Unclassified! !
!DOCSmalltalkShell methodsFor!

addLabel
	| key value |
	key := Prompter prompt: 'Write a key'.
	key ifNil: [^self].
	value := Prompter prompt: 'Write the label text'.
	value ifNil: [^self].
	self model labelAt: key put: value.
	self updateLabels!

btnMakeDoc
	self model packages: packages value.
	ProgressDialog showModalWhile: [:pd | self model makeDoc: pd].
	MessageBox notify: 'Documentation complete.' caption: 'Smalltalk DOC'!

createComponents
	super createComponents.
	"(BeginAutoGenerated) No modificar. Generado el 2:29:37, mi�rcoles, 12 de diciembre de 2007"
	docPath := self add: TextPresenter new name: 'docPath'.
	templateDir := self add: TextPresenter new name: 'templateDir'.
	templateName := self add: TextPresenter new name: 'templateName'.
	renderClassPrivate := self add: BooleanPresenter new name: 'renderClassPrivate'.
	renderClassPublic := self add: BooleanPresenter new name: 'renderClassPublic'.
	renderPublic := self add: BooleanPresenter new name: 'renderPublic'.
	renderPrivate := self add: BooleanPresenter new name: 'renderPrivate'.
	renderSuperclasses := self add: BooleanPresenter new name: 'renderSuperclasses'.
	renderRelatedClasses := self add: BooleanPresenter new name: 'renderRelatedClasses'.
	packages := self add: DoubleListPresenter new name: 'packages'.
	labels := self add: ListPresenter new name: 'labels'.
	"(EndAutoGenerated) No modificar"
	packages options: self packages!

createSchematicWiring
	super createSchematicWiring.
	labels when: #actionPerformed send: #editLabel to: self!

editLabel
	labels selectionOrNil 
		ifNotNil: 
			[:assoc | 
			| key value |
			key := assoc key.
			value := Prompter 
						on: assoc value
						prompt: 'Write the label text'
						caption: 'Label text'.
			value ifNil: [^self].
			self model labelAt: key put: value.
			self updateLabels]!

model: aDOCManager 
	| config |
	super model: aDOCManager.
	config := aDOCManager config.
	renderPrivate model: (config aspectValue: #renderPrivate).
	renderPublic model: (config aspectValue: #renderPublic).
	renderClassPublic model: (config aspectValue: #renderClassPublic).
	renderClassPrivate model: (config aspectValue: #renderClassPrivate).
	templateName model: (config aspectValue: #templateName).
	templateDir model: (config aspectValue: #templateDir).
	docPath model: (config aspectValue: #docPath).
	renderRelatedClasses model: (config aspectValue: #renderRelatedClasses).
	renderSuperclasses model: (config aspectValue: #renderSuperclasses).
	packages value: self model packages.
	self updateLabels!

navegarResultado
	| path |
	path := File fullPathOf: 'index.html' relativeTo: docPath value.
	(File exists: path) ifTrue: [ShellLibrary default shellOpen: path]!

packages
	^(Package manager packages values asSortedCollection: [:p1 :p2 | p1 name <= p2 name]) 
		asOrderedCollection!

updateLabels
	labels list: self model labelDictionary associations.
	labels view refreshContents! !
!DOCSmalltalkShell categoriesFor: #addLabel!public! !
!DOCSmalltalkShell categoriesFor: #btnMakeDoc!public! !
!DOCSmalltalkShell categoriesFor: #createComponents!public! !
!DOCSmalltalkShell categoriesFor: #createSchematicWiring!public! !
!DOCSmalltalkShell categoriesFor: #editLabel!public! !
!DOCSmalltalkShell categoriesFor: #model:!public! !
!DOCSmalltalkShell categoriesFor: #navegarResultado!public! !
!DOCSmalltalkShell categoriesFor: #packages!public! !
!DOCSmalltalkShell categoriesFor: #updateLabels!public! !

!DOCSmalltalkShell class methodsFor!

defaultModel
	^DocModel ifNil: [DocModel := DOCSmalltalkManager new]!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 0 0 8 #(13303808 65536) 416 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 167 0 0 0 416 788230 ##(Smalltalk.BorderLayout) 1 11 0 410 ##(Smalltalk.ContainerView) 34 15 0 416 34 2 8 1140850688 131073 544 0 0 0 7 0 0 0 544 514 1 1 0 0 410 ##(Smalltalk.PushButton) 34 20 0 544 34 2 8 1140924416 1 624 0 0 0 7 0 0 0 624 0 8 4294902563 1180998 4 ##(Smalltalk.CommandDescription) #exit 8 'Exit' 1 1 0 0 false 0 0 0 983302 ##(Smalltalk.MessageSequence) 138 144 34 2 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 328198 ##(Smalltalk.Point) 1019 11 866 201 61 624 818 #text: 34 1 8 'Exit' 624 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 253 1 0 0 5 0 0 0 97 2 0 0 35 0 0 0] 8 #() 866 193 193 0 29 410 ##(Smalltalk.PushButton) 34 20 0 544 34 2 8 1140924416 1 1040 0 0 0 7 0 0 0 1040 0 8 4294902563 706 #btnMakeDoc 8 'Make Doc' 1 1 0 0 false 0 0 0 754 138 144 34 3 818 #createAt:extent: 34 2 866 11 11 866 201 61 1040 818 #isEnabled: 8 #(false) 1040 818 #text: 34 1 8 'Make Doc' 1040 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 5 0 0 0 105 0 0 0 35 0 0 0] 8 #() 1024 0 29 0 170 192 8 #() 590342 ##(Smalltalk.Rectangle) 866 11 11 866 11 11 754 138 144 34 1 818 #createAt:extent: 34 2 866 11 793 866 1229 81 544 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 140 1 0 0 107 2 0 0 180 1 0 0] 34 2 1040 624 1024 0 27 0 0 410 ##(Smalltalk.CardContainer) 34 16 0 416 34 2 8 1140850688 131073 1648 0 0 0 7 0 0 0 1648 655878 ##(Smalltalk.CardLayout) 138 144 34 3 721414 ##(Smalltalk.Association) 8 'General' 410 ##(Smalltalk.ContainerView) 34 15 0 1648 34 2 8 1140850688 131073 1824 0 0 0 7 0 0 0 1824 514 1 1 410 ##(Smalltalk.ContainerView) 34 15 0 1824 34 2 8 1140850688 131073 1904 0 0 0 7 0 0 0 1904 514 1 1 410 ##(Smalltalk.ContainerView) 34 15 0 1904 34 2 8 1140850688 131073 1984 0 0 0 7 0 0 0 1984 0 170 192 1408 0 754 138 144 34 1 818 #createAt:extent: 34 2 866 1 1 866 1193 55 1984 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 84 2 0 0 27 0 0 0] 34 1 410 ##(Smalltalk.StaticText) 34 16 0 1984 34 2 8 1140850944 1 2224 0 0 0 7 0 0 0 2224 0 8 4294902093 852486 ##(Smalltalk.NullConverter) 0 0 0 754 138 144 34 2 818 #createAt:extent: 34 2 866 1 1 866 607 57 2224 818 #text: 34 1 8 'Select what to document:' 2224 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 47 1 0 0 28 0 0 0] 8 #() 1024 0 27 1024 0 27 0 0 0 410 ##(Smalltalk.ContainerView) 34 15 0 1904 34 2 8 1140850688 131073 2544 0 0 0 7 0 0 0 2544 656390 ##(Smalltalk.GridLayout) 7 5 1 1 170 192 34 12 410 ##(Smalltalk.CheckBox) 34 16 0 2544 34 2 8 1140924419 1 2672 721990 2 ##(Smalltalk.ValueHolder) 0 0 1114118 ##(Smalltalk.NeverSearchPolicy) false 0 0 7 0 0 0 2672 0 8 4294902563 2306 0 0 0 754 138 144 34 2 818 #createAt:extent: 34 2 866 597 111 866 597 57 2672 818 #text: 34 1 8 'Render Related Classes' 2672 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 42 1 0 0 55 0 0 0 84 2 0 0 83 0 0 0] 8 #() 1024 0 27 8 'renderRelatedClasses' 410 ##(Smalltalk.CheckBox) 34 16 0 2544 34 2 8 1140924419 1 3056 2738 0 0 2784 false 0 0 7 0 0 0 3056 0 8 4294902563 2306 0 0 0 754 138 144 34 2 818 #createAt:extent: 34 2 866 1 1 866 597 57 3056 818 #text: 34 1 8 'Document private methods' 3056 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 42 1 0 0 28 0 0 0] 8 #() 1024 0 27 8 'renderPrivate' 410 ##(Smalltalk.CheckBox) 34 16 0 2544 34 2 8 1140924419 1 3392 2738 0 0 2784 false 0 0 7 0 0 0 3392 0 8 4294902563 2306 0 0 0 754 138 144 34 2 818 #createAt:extent: 34 2 866 1 111 866 597 57 3392 818 #text: 34 1 8 'Render Super Classes' 3392 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 55 0 0 0 42 1 0 0 83 0 0 0] 8 #() 1024 0 27 8 'renderSuperclasses' 410 ##(Smalltalk.CheckBox) 34 16 0 2544 34 2 8 1140924419 1 3728 2738 0 0 2784 false 0 0 7 0 0 0 3728 0 8 4294902563 2306 0 0 0 754 138 144 34 2 818 #createAt:extent: 34 2 866 597 57 866 597 55 3728 818 #text: 34 1 8 'Document public class methods' 3728 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 42 1 0 0 28 0 0 0 84 2 0 0 55 0 0 0] 8 #() 1024 0 27 8 'renderClassPublic' 410 ##(Smalltalk.CheckBox) 34 16 0 2544 34 2 8 1140924419 1 4064 2738 0 0 2784 false 0 0 7 0 0 0 4064 0 8 4294902563 2306 0 0 0 754 138 144 34 2 818 #createAt:extent: 34 2 866 1 57 866 597 55 4064 818 #text: 34 1 8 'Document public methods' 4064 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 28 0 0 0 42 1 0 0 55 0 0 0] 8 #() 1024 0 27 8 'renderPublic' 410 ##(Smalltalk.CheckBox) 34 16 0 2544 34 2 8 1140924419 1 4400 2738 0 0 2784 false 0 0 7 0 0 0 4400 0 8 4294902563 2306 0 0 0 754 138 144 34 2 818 #createAt:extent: 34 2 866 597 1 866 597 57 4400 818 #text: 34 1 8 'Document private class methods' 4400 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 42 1 0 0 0 0 0 0 84 2 0 0 28 0 0 0] 8 #() 1024 0 27 8 'renderClassPrivate' 0 754 138 144 34 1 818 #createAt:extent: 34 2 866 1 55 866 1193 167 2544 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 27 0 0 0 84 2 0 0 110 0 0 0] 34 6 3056 4400 4064 3728 3392 2672 1024 0 27 170 192 1408 0 754 138 144 34 1 818 #createAt:extent: 34 2 866 11 11 866 1193 221 1904 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 5 0 0 0 89 2 0 0 115 0 0 0] 34 2 1984 2544 1024 0 27 0 0 0 410 ##(Smalltalk.ContainerView) 34 15 0 1824 34 2 8 1140850688 131073 5072 0 0 0 7 0 0 0 5072 2610 9 1 1 21 170 192 1408 0 754 138 144 34 1 818 #createAt:extent: 34 2 866 11 231 866 1193 471 5072 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 115 0 0 0 89 2 0 0 94 1 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 0 5072 34 2 8 1140850688 131073 5328 0 0 0 7 0 0 0 5328 2610 5 1 1 1 170 192 1408 0 754 138 144 34 1 818 #createAt:extent: 34 2 866 1 1 866 1193 103 5328 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 84 2 0 0 51 0 0 0] 34 2 410 ##(Smalltalk.ContainerView) 34 15 0 5328 34 2 8 1140850688 131073 5584 0 0 0 7 0 0 0 5584 0 170 192 1408 0 754 138 144 34 1 818 #createAt:extent: 34 2 866 1 1 866 1193 53 5584 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 84 2 0 0 26 0 0 0] 34 1 410 ##(Smalltalk.StaticText) 34 16 0 5584 34 2 8 1140850944 1 5824 0 0 0 7 0 0 0 5824 0 8 4294902093 2306 0 0 0 754 138 144 34 2 818 #createAt:extent: 34 2 866 1 1 866 541 33 5824 818 #text: 34 1 8 'Output directory:' 5824 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 14 1 0 0 16 0 0 0] 8 #() 1024 0 27 1024 0 27 410 ##(Smalltalk.ContainerView) 34 15 0 5328 34 2 8 1140850688 131073 6128 0 0 0 7 0 0 0 6128 514 1 1 0 0 410 ##(Smalltalk.PushButton) 34 20 0 6128 34 2 8 1140924416 1 6208 0 0 0 7 0 0 0 6208 0 8 4294902563 706 #navegarResultado 8 'Browse' 1 1 0 0 false 0 0 0 754 138 144 34 3 818 #createAt:extent: 34 2 866 993 1 866 201 51 6208 818 #isEnabled: 8 #(false) 6208 818 #text: 34 1 8 'Browse' 6208 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 240 1 0 0 0 0 0 0 84 2 0 0 25 0 0 0] 8 #() 1024 0 29 0 410 ##(Smalltalk.TextEdit) 34 16 0 6128 34 2 8 1140916352 1025 6560 0 0 0 7 0 0 0 6560 0 8 4294903205 2306 0 0 1 754 138 144 34 1 818 #createAt:extent: 34 2 866 1 1 866 993 51 6560 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 240 1 0 0 25 0 0 0] 8 #() 1024 0 27 170 192 34 2 6560 8 'docPath' 0 754 138 144 34 1 818 #createAt:extent: 34 2 866 1 53 866 1193 51 6128 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 26 0 0 0 84 2 0 0 51 0 0 0] 34 2 6560 6208 1024 0 27 1024 0 27 410 ##(Smalltalk.ContainerView) 34 15 0 5072 34 2 8 1140850688 131073 7024 0 0 0 7 0 0 0 7024 2610 5 1 1 1 170 192 34 2 410 ##(Smalltalk.TextEdit) 34 16 0 7024 34 2 8 1140916352 1025 7136 0 0 0 7 0 0 0 7136 0 8 4294903205 2306 0 0 1 754 138 144 34 1 818 #createAt:extent: 34 2 866 1 53 866 1193 53 7136 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 26 0 0 0 84 2 0 0 52 0 0 0] 8 #() 1024 0 27 8 'templateDir' 0 754 138 144 34 1 818 #createAt:extent: 34 2 866 1 123 866 1193 105 7024 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 61 0 0 0 84 2 0 0 113 0 0 0] 34 2 410 ##(Smalltalk.StaticText) 34 16 0 7024 34 2 8 1140850944 1 7568 0 0 0 7 0 0 0 7568 0 8 4294902093 2306 0 0 0 754 138 144 34 2 818 #createAt:extent: 34 2 866 1 1 866 1193 53 7568 818 #text: 34 1 8 'Template directory:' 7568 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 84 2 0 0 26 0 0 0] 8 #() 1024 0 27 7136 1024 0 27 410 ##(Smalltalk.ContainerView) 34 15 0 5072 34 2 8 1140850688 131073 7872 0 0 0 7 0 0 0 7872 2610 5 1 1 1 170 192 34 2 410 ##(Smalltalk.TextEdit) 34 16 0 7872 34 2 8 1140916352 1025 7984 0 0 0 7 0 0 0 7984 0 8 4294903205 2306 0 0 1 754 138 144 34 1 818 #createAt:extent: 34 2 866 1 53 866 1193 51 7984 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 26 0 0 0 84 2 0 0 51 0 0 0] 8 #() 1024 0 27 8 'templateName' 0 754 138 144 34 1 818 #createAt:extent: 34 2 866 1 247 866 1193 103 7872 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 123 0 0 0 84 2 0 0 174 0 0 0] 34 2 410 ##(Smalltalk.StaticText) 34 16 0 7872 34 2 8 1140850944 1 8416 0 0 0 7 0 0 0 8416 0 8 4294902093 2306 0 0 0 754 138 144 34 2 818 #createAt:extent: 34 2 866 1 1 866 1193 53 8416 818 #text: 34 1 8 'Template name:' 8416 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 84 2 0 0 26 0 0 0] 8 #() 1024 0 27 7984 1024 0 27 1024 0 27 170 192 1408 1426 866 11 11 866 11 11 754 138 144 34 2 818 #createAt:extent: 34 2 866 9 55 866 1213 711 1824 818 #text: 34 1 8 'General' 1824 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 98 2 0 0 126 1 0 0] 34 2 1904 5072 1024 0 27 1778 8 'Packages' 410 ##(Smalltalk.ContainerView) 34 15 0 1648 34 2 8 1140850688 131073 9024 0 0 0 5 0 0 0 9024 514 1 1 410 ##(Smalltalk.StaticText) 34 16 0 9024 34 2 8 1140850944 1 9104 0 0 0 5 0 0 0 9104 0 8 4294902093 2306 0 0 0 754 138 144 34 2 818 #createAt:extent: 34 2 866 11 11 866 1193 41 9104 818 #text: 34 1 8 'Select the packages you want to document:' 9104 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 5 0 0 0 89 2 0 0 25 0 0 0] 8 #() 1024 0 27 0 0 0 410 ##(Smalltalk.ReferenceView) 34 14 0 9024 34 2 8 1140850688 131073 9408 0 0 0 5 0 0 0 9408 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.DoubleListPresenter) #resource_Default_view 0 754 138 144 34 1 818 #createAt:extent: 34 2 866 11 51 866 1193 651 9408 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 25 0 0 0 89 2 0 0 94 1 0 0] 1408 1024 0 27 170 192 34 2 9408 8 'packages' 1426 866 11 11 866 11 11 754 138 144 34 2 818 #createAt:extent: 34 2 866 9 55 866 1213 711 9024 818 #text: 34 1 8 'Packages' 9024 962 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 98 2 0 0 126 1 0 0] 34 2 9104 9408 1024 0 27 1778 8 'Labels' 410 ##(Smalltalk.ContainerView) 34 15 0 1648 34 2 8 1140850688 131073 9984 0 0 0 5 0 0 0 9984 852230 ##(Smalltalk.FramingLayout) 170 176 34 6 410 ##(Smalltalk.PushButton) 34 20 0 9984 34 2 8 1140924416 1 10112 0 0 0 5 0 0 0 10112 0 8 4294902563 706 #editLabel 8 'Edit' 1 1 0 0 false 0 0 0 754 138 144 34 3 818 #createAt:extent: 34 2 866 661 81 866 181 61 10112 818 #isEnabled: 8 #(false) 10112 818 #text: 34 1 8 'Edit' 10112 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 74 1 0 0 40 0 0 0 164 1 0 0 70 0 0 0] 8 #() 1024 0 29 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 661 10498 #fixedViewLeft 181 10498 #fixedPreviousBottom 11 10498 #fixedViewTop 61 410 ##(Smalltalk.PushButton) 34 20 0 9984 34 2 8 1140924416 1 10576 0 0 0 5 0 0 0 10576 0 8 4294902563 706 #addLabel 8 'Add' 1 1 0 0 false 0 0 0 754 138 144 34 3 818 #createAt:extent: 34 2 866 661 11 866 181 61 10576 818 #isEnabled: 8 #(false) 10576 818 #text: 34 1 8 'Add' 10576 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 74 1 0 0 5 0 0 0 164 1 0 0 35 0 0 0] 8 #() 1024 0 29 10466 10512 661 10528 181 10498 #fixedParentTop 11 10560 61 410 ##(Smalltalk.ListView) 34 30 0 9984 34 2 8 1140920397 1025 10960 590662 2 ##(Smalltalk.ListModel) 138 144 1408 0 1310726 ##(Smalltalk.IdentitySearchPolicy) 0 0 5 0 0 0 10960 0 8 4294902367 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) 0 0 0 0 0 0 138 144 34 2 920646 5 ##(Smalltalk.ListViewColumn) 8 'Key' 201 #left 787814 3 ##(Smalltalk.BlockClosure) 0 0 1180966 ##(Smalltalk.CompiledExpression) 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:ass | ass key]' 8 #[30 105 226 0 106] #key 11248 7 257 0 ##(Smalltalk.SortedCollection) 0 0 10960 0 1 0 0 11186 8 'Label' 433 #left 11234 0 0 11266 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:ass | ass value]' 8 #[29 105 17 142 106] 11376 7 257 0 ##(Smalltalk.SortedCollection) 0 0 10960 0 3 0 0 #report 1408 0 131169 0 34 4 0 0 866 1 1 0 754 138 144 34 2 818 #createAt:extent: 34 2 866 11 11 866 641 689 10960 818 #text: 34 1 8 'Key' 10960 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 5 0 0 0 69 1 0 0 93 1 0 0] 8 #() 1024 0 27 10466 10512 11 10528 641 10944 11 10498 #fixedParentBottom -11 170 192 34 2 10960 8 'labels' 0 754 138 144 34 2 818 #createAt:extent: 34 2 866 9 55 866 1213 711 9984 818 #text: 34 1 8 'Labels' 9984 962 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 98 2 0 0 126 1 0 0] 34 3 10960 10576 10112 1024 0 27 1824 170 192 1408 0 410 ##(Smalltalk.TabView) 34 23 0 1648 34 2 8 1140916736 1 12000 11026 138 144 34 3 1808 9008 9968 0 11088 0 0 1 0 0 0 12000 0 8 4294902495 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 11136 0 0 0 0 0 #noIcons 754 138 144 34 3 818 #createAt:extent: 34 2 866 1 1 866 1229 773 12000 818 #basicSelectionsByIndex: 34 1 8 #(1) 12000 818 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 12000 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 102 2 0 0 130 1 0 0] 8 #() 1024 0 27 754 138 144 34 1 818 #createAt:extent: 34 2 866 11 11 866 1229 773 1648 962 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 5 0 0 0 107 2 0 0 135 1 0 0] 34 4 1824 9024 9984 12000 1024 0 27 170 192 1408 1426 866 11 11 866 11 11 0 0 0 0 1 0 0 0 866 1281 961 1 0 0 754 138 144 34 3 818 #createAt:extent: 34 2 866 3839 21 866 1281 961 416 818 #text: 34 1 8 'Smalltalk DOC' 416 818 #updateMenuBar 1408 416 962 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 255 9 0 0 234 1 0 0] 34 2 1648 544 1024 0 27 )! !
!DOCSmalltalkShell class categoriesFor: #defaultModel!public! !
!DOCSmalltalkShell class categoriesFor: #resource_Default_view!public!resources-views! !

