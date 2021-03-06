"Filed out from Dolphin Smalltalk 7"!

Shell subclass: #STDocShell
	instanceVariableNames: 'renderPrivate renderPublic renderClassPublic renderClassPrivate templateName templateDir docPath packages labels renderRelatedClasses renderSuperclasses'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocShell guid: (GUID fromString: '{a7a96838-d1a8-40bd-9324-347b3c1c62f7}')!
STDocShell comment: ''!
!STDocShell categoriesForClass!Unclassified! !
!STDocShell methodsFor!

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
	labels
		when: #actionPerformed
		send: #editLabel
		to: self!

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

navegarPlantilla
	self halt!

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
!STDocShell categoriesFor: #addLabel!public! !
!STDocShell categoriesFor: #btnMakeDoc!public! !
!STDocShell categoriesFor: #createComponents!public! !
!STDocShell categoriesFor: #createSchematicWiring!public! !
!STDocShell categoriesFor: #editLabel!public! !
!STDocShell categoriesFor: #model:!public! !
!STDocShell categoriesFor: #navegarPlantilla!public! !
!STDocShell categoriesFor: #navegarResultado!public! !
!STDocShell categoriesFor: #packages!public! !
!STDocShell categoriesFor: #updateLabels!public! !

!STDocShell class methodsFor!

defaultModel
	^STDocSmalltalkManager new!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 4 788558 10 ##(Smalltalk.STBViewProxy) ##(Smalltalk.ShellView) 34 27 0 0 8 #(13107200 65536) 416 0 524550 ##(Smalltalk.ColorRef) 8 4278190080 0 167 0 0 0 416 788230 ##(Smalltalk.BorderLayout) 1 11 0 410 ##(Smalltalk.ContainerView) 34 15 0 416 34 2 8 1140850688 131073 544 0 0 0 7 0 0 0 544 656198 1 ##(Smalltalk.FlowLayout) 1 1 5 170 192 8 #() 590342 ##(Smalltalk.Rectangle) 328198 ##(Smalltalk.Point) 11 11 706 11 11 983302 ##(Smalltalk.MessageSequence) 138 144 34 1 721670 ##(Smalltalk.MessageSend) #createAt:extent: 34 2 706 11 793 706 1229 81 544 983302 ##(Smalltalk.WINDOWPLACEMENT) 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 140 1 0 0 107 2 0 0 180 1 0 0] 34 2 410 ##(Smalltalk.PushButton) 34 20 0 544 34 2 8 1140924416 1 960 0 0 0 7 0 0 0 960 0 8 4294903133 1180998 4 ##(Smalltalk.CommandDescription) #btnMakeDoc 8 'Make Doc' 1 1 0 0 false 0 0 0 754 138 144 34 3 818 #createAt:extent: 34 2 706 819 11 706 201 61 960 818 #isEnabled: 8 #(false) 960 818 #text: 34 1 8 'Make Doc' 960 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 153 1 0 0 5 0 0 0 253 1 0 0 35 0 0 0] 8 #() 706 193 193 0 29 410 ##(Smalltalk.PushButton) 34 20 0 544 34 2 8 1140924416 1 1344 0 0 0 7 0 0 0 1344 0 8 4294903133 1042 #exit 8 'Exit' 1 1 0 0 false 0 0 0 754 138 144 34 2 818 #createAt:extent: 34 2 706 1019 11 706 201 61 1344 818 #text: 34 1 8 'Exit' 1344 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 253 1 0 0 5 0 0 0 97 2 0 0 35 0 0 0] 8 #() 1328 0 29 1328 0 27 0 0 410 ##(Smalltalk.CardContainer) 34 16 0 416 34 2 8 1140850688 131073 1664 0 0 0 7 0 0 0 1664 655878 ##(Smalltalk.CardLayout) 138 144 34 3 721414 ##(Smalltalk.Association) 8 'General' 410 ##(Smalltalk.ContainerView) 34 15 0 1664 34 2 8 1140850688 131073 1840 0 0 0 7 0 0 0 1840 514 1 21 410 ##(Smalltalk.ContainerView) 34 15 0 1840 34 2 8 1140850688 131073 1920 0 0 0 7 0 0 0 1920 514 1 1 410 ##(Smalltalk.ContainerView) 34 15 0 1920 34 2 8 1140850688 131073 2000 0 0 0 7 0 0 0 2000 0 170 192 656 0 754 138 144 34 1 818 #createAt:extent: 34 2 706 1 1 706 1193 55 2000 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 84 2 0 0 27 0 0 0] 34 1 410 ##(Smalltalk.StaticText) 34 16 0 2000 34 2 8 1140850944 1 2240 0 0 0 7 0 0 0 2240 0 8 4294902375 852486 ##(Smalltalk.NullConverter) 0 0 0 754 138 144 34 2 818 #createAt:extent: 34 2 706 1 1 706 607 57 2240 818 #text: 34 1 8 'Select what to document:' 2240 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 47 1 0 0 28 0 0 0] 8 #() 1328 0 27 1328 0 27 0 0 0 410 ##(Smalltalk.ContainerView) 34 15 0 1920 34 2 8 1140850688 131073 2560 0 0 0 7 0 0 0 2560 656390 ##(Smalltalk.GridLayout) 7 5 5 5 170 192 34 12 410 ##(Smalltalk.CheckBox) 34 16 0 2560 34 2 8 1140924419 1 2688 721990 2 ##(Smalltalk.ValueHolder) 0 0 1114118 ##(Smalltalk.NeverSearchPolicy) false 0 0 7 0 0 0 2688 0 8 4294903133 2322 0 0 0 754 138 144 34 2 818 #createAt:extent: 34 2 706 1 1 706 595 53 2688 818 #text: 34 1 8 'Document private methods' 2688 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 41 1 0 0 26 0 0 0] 8 #() 1328 0 27 8 'renderPrivate' 410 ##(Smalltalk.CheckBox) 34 16 0 2560 34 2 8 1140924419 1 3072 2754 0 0 2800 false 0 0 7 0 0 0 3072 0 8 4294903133 2322 0 0 0 754 138 144 34 2 818 #createAt:extent: 34 2 706 599 1 706 595 53 3072 818 #text: 34 1 8 'Document private class methods' 3072 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 43 1 0 0 0 0 0 0 84 2 0 0 26 0 0 0] 8 #() 1328 0 27 8 'renderClassPrivate' 410 ##(Smalltalk.CheckBox) 34 16 0 2560 34 2 8 1140924419 1 3408 2754 0 0 2800 false 0 0 7 0 0 0 3408 0 8 4294903133 2322 0 0 0 754 138 144 34 2 818 #createAt:extent: 34 2 706 599 57 706 595 55 3408 818 #text: 34 1 8 'Document public class methods' 3408 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 43 1 0 0 28 0 0 0 84 2 0 0 55 0 0 0] 8 #() 1328 0 27 8 'renderClassPublic' 410 ##(Smalltalk.CheckBox) 34 16 0 2560 34 2 8 1140924419 1 3744 2754 0 0 2800 false 0 0 7 0 0 0 3744 0 8 4294903133 2322 0 0 0 754 138 144 34 2 818 #createAt:extent: 34 2 706 1 57 706 595 55 3744 818 #text: 34 1 8 'Document public methods' 3744 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 28 0 0 0 41 1 0 0 55 0 0 0] 8 #() 1328 0 27 8 'renderPublic' 410 ##(Smalltalk.CheckBox) 34 16 0 2560 34 2 8 1140924419 1 4080 2754 0 0 2800 false 0 0 7 0 0 0 4080 0 8 4294903133 2322 0 0 0 754 138 144 34 2 818 #createAt:extent: 34 2 706 1 115 706 595 53 4080 818 #text: 34 1 8 'Render Super Classes' 4080 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 57 0 0 0 41 1 0 0 83 0 0 0] 8 #() 1328 0 27 8 'renderSuperclasses' 410 ##(Smalltalk.CheckBox) 34 16 0 2560 34 2 8 1140924419 1 4416 2754 0 0 2800 false 0 0 7 0 0 0 4416 0 8 4294903133 2322 0 0 0 754 138 144 34 2 818 #createAt:extent: 34 2 706 599 115 706 595 53 4416 818 #text: 34 1 8 'Render Related Classes' 4416 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 43 1 0 0 57 0 0 0 84 2 0 0 83 0 0 0] 8 #() 1328 0 27 8 'renderRelatedClasses' 0 754 138 144 34 1 818 #createAt:extent: 34 2 706 1 55 706 1193 167 2560 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 27 0 0 0 84 2 0 0 110 0 0 0] 34 6 2688 3072 3744 3408 4080 4416 1328 0 27 170 192 656 0 754 138 144 34 1 818 #createAt:extent: 34 2 706 11 11 706 1193 221 1920 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 5 0 0 0 89 2 0 0 115 0 0 0] 34 2 2000 2560 1328 0 27 0 0 0 410 ##(Smalltalk.ContainerView) 34 15 0 1840 34 2 8 1140850688 131073 5088 0 0 0 7 0 0 0 5088 2626 9 1 1 5 170 192 656 0 754 138 144 34 1 818 #createAt:extent: 34 2 706 11 251 706 1193 451 5088 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 125 0 0 0 89 2 0 0 94 1 0 0] 34 3 410 ##(Smalltalk.ContainerView) 34 15 0 5088 34 2 8 1140850688 131073 5344 0 0 0 7 0 0 0 5344 2626 5 1 1 1 170 192 656 0 754 138 144 34 1 818 #createAt:extent: 34 2 706 1 1 706 1193 111 5344 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 84 2 0 0 55 0 0 0] 34 2 410 ##(Smalltalk.ContainerView) 34 15 0 5344 34 2 8 1140850688 131073 5600 0 0 0 7 0 0 0 5600 0 170 192 656 0 754 138 144 34 1 818 #createAt:extent: 34 2 706 1 1 706 1193 57 5600 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 84 2 0 0 28 0 0 0] 34 1 410 ##(Smalltalk.StaticText) 34 16 0 5600 34 2 8 1140850944 1 5840 0 0 0 7 0 0 0 5840 0 8 4294902375 2322 0 0 0 754 138 144 34 2 818 #createAt:extent: 34 2 706 -1 -3 706 399 57 5840 818 #text: 34 1 8 'Template directory:' 5840 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 254 255 255 255 198 0 0 0 26 0 0 0] 8 #() 1328 0 27 1328 0 27 410 ##(Smalltalk.ContainerView) 34 15 0 5344 34 2 8 1140850688 131073 6144 0 0 0 7 0 0 0 6144 514 1 1 0 0 410 ##(Smalltalk.PushButton) 34 20 0 6144 34 2 8 1140924416 1 6224 0 0 0 7 0 0 0 6224 0 8 4294903133 1042 #navegarPlantilla 8 'Browse' 1 1 0 0 false 0 0 0 754 138 144 34 3 818 #createAt:extent: 34 2 706 993 1 706 201 55 6224 818 #isEnabled: 8 #(false) 6224 818 #text: 34 1 8 'Browse' 6224 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 240 1 0 0 0 0 0 0 84 2 0 0 27 0 0 0] 8 #() 1328 0 29 0 410 ##(Smalltalk.TextEdit) 34 16 0 6144 34 2 8 1140916352 1025 6576 0 0 0 7 0 0 0 6576 0 8 4294903161 2322 0 0 35 754 138 144 34 1 818 #createAt:extent: 34 2 706 1 1 706 993 55 6576 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 240 1 0 0 27 0 0 0] 8 #() 1328 0 27 170 192 34 4 6224 8 'btnBrowseTemplate' 6576 8 'templateDir' 0 754 138 144 34 1 818 #createAt:extent: 34 2 706 1 57 706 1193 55 6144 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 28 0 0 0 84 2 0 0 55 0 0 0] 34 2 6576 6224 1328 0 27 1328 0 27 410 ##(Smalltalk.ContainerView) 34 15 0 5088 34 2 8 1140850688 131073 7056 0 0 0 7 0 0 0 7056 2626 5 1 1 1 170 192 34 2 410 ##(Smalltalk.TextEdit) 34 16 0 7056 34 2 8 1140916352 1025 7168 0 0 0 7 0 0 0 7168 0 8 4294903161 2322 0 0 1 754 138 144 34 1 818 #createAt:extent: 34 2 706 1 57 706 1193 55 7168 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 28 0 0 0 84 2 0 0 55 0 0 0] 8 #() 1328 0 27 8 'templateName' 0 754 138 144 34 1 818 #createAt:extent: 34 2 706 1 115 706 1193 111 7056 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 57 0 0 0 84 2 0 0 112 0 0 0] 34 2 410 ##(Smalltalk.StaticText) 34 16 0 7056 34 2 8 1140850944 1 7600 0 0 0 7 0 0 0 7600 0 8 4294902375 2322 0 0 0 754 138 144 34 2 818 #createAt:extent: 34 2 706 1 1 706 1193 57 7600 818 #text: 34 1 8 'Template name:' 7600 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 84 2 0 0 28 0 0 0] 8 #() 1328 0 27 7168 1328 0 27 410 ##(Smalltalk.ContainerView) 34 15 0 5088 34 2 8 1140850688 131073 7904 0 0 0 7 0 0 0 7904 2626 5 1 1 1 170 192 656 0 754 138 144 34 1 818 #createAt:extent: 34 2 706 1 229 706 1193 109 7904 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 114 0 0 0 84 2 0 0 168 0 0 0] 34 2 410 ##(Smalltalk.ContainerView) 34 15 0 7904 34 2 8 1140850688 131073 8160 0 0 0 7 0 0 0 8160 0 170 192 656 0 754 138 144 34 1 818 #createAt:extent: 34 2 706 1 1 706 1193 55 8160 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 84 2 0 0 27 0 0 0] 34 1 410 ##(Smalltalk.StaticText) 34 16 0 8160 34 2 8 1140850944 1 8400 0 0 0 7 0 0 0 8400 0 8 4294902375 2322 0 0 0 754 138 144 34 2 818 #createAt:extent: 34 2 706 1 1 706 541 33 8400 818 #text: 34 1 8 'Output directory:' 8400 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 14 1 0 0 16 0 0 0] 8 #() 1328 0 27 1328 0 27 410 ##(Smalltalk.ContainerView) 34 15 0 7904 34 2 8 1140850688 131073 8704 0 0 0 7 0 0 0 8704 514 1 1 0 0 410 ##(Smalltalk.PushButton) 34 20 0 8704 34 2 8 1140924416 1 8784 0 0 0 7 0 0 0 8784 0 8 4294903133 1042 #navegarResultado 8 'Open Doc' 1 1 0 0 false 0 0 0 754 138 144 34 3 818 #createAt:extent: 34 2 706 993 1 706 201 55 8784 818 #isEnabled: 8 #(false) 8784 818 #text: 34 1 8 'Open Doc' 8784 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 240 1 0 0 0 0 0 0 84 2 0 0 27 0 0 0] 8 #() 1328 0 29 0 410 ##(Smalltalk.TextEdit) 34 16 0 8704 34 2 8 1140916352 1025 9136 0 0 0 7 0 0 0 9136 0 8 4294903161 2322 0 0 33 754 138 144 34 1 818 #createAt:extent: 34 2 706 1 1 706 993 55 9136 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 240 1 0 0 27 0 0 0] 8 #() 1328 0 27 170 192 34 4 8784 8 'btnBrowseOutput' 9136 8 'docPath' 0 754 138 144 34 1 818 #createAt:extent: 34 2 706 1 55 706 1193 55 8704 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 27 0 0 0 84 2 0 0 54 0 0 0] 34 2 9136 8784 1328 0 27 1328 0 27 1328 0 27 170 192 656 674 706 11 11 706 11 11 754 138 144 34 2 818 #createAt:extent: 34 2 706 9 55 706 1213 711 1840 818 #text: 34 1 8 'General' 1840 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 98 2 0 0 126 1 0 0] 34 2 1920 5088 1328 0 27 1794 8 'Packages' 410 ##(Smalltalk.ContainerView) 34 15 0 1664 34 2 8 1140850688 131073 9920 0 0 0 5 0 0 0 9920 514 1 1 410 ##(Smalltalk.StaticText) 34 16 0 9920 34 2 8 1140850944 1 10000 0 0 0 5 0 0 0 10000 0 8 4294902375 2322 0 0 0 754 138 144 34 2 818 #createAt:extent: 34 2 706 11 11 706 1193 41 10000 818 #text: 34 1 8 'Select the packages you want to document:' 10000 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 5 0 0 0 89 2 0 0 25 0 0 0] 8 #() 1328 0 27 0 0 0 410 ##(Smalltalk.ReferenceView) 34 14 0 9920 34 2 8 1140850688 131073 10304 0 0 0 5 0 0 0 10304 1180166 ##(Smalltalk.ResourceIdentifier) ##(Smalltalk.DoubleListPresenter) #resource_Default_view 0 754 138 144 34 1 818 #createAt:extent: 34 2 706 11 51 706 1193 651 10304 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 25 0 0 0 89 2 0 0 94 1 0 0] 656 1328 0 27 170 192 34 2 10304 8 'packages' 674 706 11 11 706 11 11 754 138 144 34 2 818 #createAt:extent: 34 2 706 9 55 706 1213 711 9920 818 #text: 34 1 8 'Packages' 9920 898 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 98 2 0 0 126 1 0 0] 34 2 10000 10304 1328 0 27 1794 8 'Labels' 410 ##(Smalltalk.ContainerView) 34 15 0 1664 34 2 8 1140850688 131073 10880 0 0 0 5 0 0 0 10880 852230 ##(Smalltalk.FramingLayout) 170 176 34 6 410 ##(Smalltalk.ListView) 34 30 0 10880 34 2 8 1140920397 1025 11008 590662 2 ##(Smalltalk.ListModel) 138 144 656 0 1310726 ##(Smalltalk.IdentitySearchPolicy) 0 0 5 0 0 0 11008 0 8 4294902429 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 1049926 1 ##(Smalltalk.IconImageManager) 0 0 0 0 0 0 138 144 34 2 920646 5 ##(Smalltalk.ListViewColumn) 8 'Key' 313 #left 787814 3 ##(Smalltalk.BlockClosure) 0 0 1180966 ##(Smalltalk.CompiledExpression) 2 1 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:ass | ass key]' 8 #[30 105 226 0 106] #key 11296 7 257 0 ##(Smalltalk.SortedCollection) 0 0 11008 0 3 0 0 11234 8 'Label' 313 #left 11282 0 0 11314 1 83886081 ##(Smalltalk.UndefinedObject) 8 'doIt' 8 '[:ass | ass value]' 8 #[29 105 17 142 106] 11424 7 257 0 ##(Smalltalk.SortedCollection) 0 0 11008 0 3 0 0 #report 656 0 131169 0 34 4 0 0 706 1 1 0 754 138 144 34 2 818 #createAt:extent: 34 2 706 11 11 706 641 689 11008 818 #text: 34 1 8 'Key' 11008 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 5 0 0 0 69 1 0 0 93 1 0 0] 8 #() 1328 0 27 1181766 2 ##(Smalltalk.FramingConstraints) 1180678 ##(Smalltalk.FramingCalculation) #fixedParentLeft 11 11778 #fixedViewLeft 641 11778 #fixedParentTop 11 11778 #fixedParentBottom -11 410 ##(Smalltalk.PushButton) 34 20 0 10880 34 2 8 1140924416 1 11856 0 0 0 5 0 0 0 11856 0 8 4294903133 1042 #editLabel 8 'Edit' 1 1 0 0 false 0 0 0 754 138 144 34 3 818 #createAt:extent: 34 2 706 661 81 706 181 61 11856 818 #isEnabled: 8 #(false) 11856 818 #text: 34 1 8 'Edit' 11856 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 74 1 0 0 40 0 0 0 164 1 0 0 70 0 0 0] 8 #() 1328 0 29 11746 11792 661 11808 181 11778 #fixedPreviousBottom 11 11778 #fixedViewTop 61 410 ##(Smalltalk.PushButton) 34 20 0 10880 34 2 8 1140924416 1 12256 0 0 0 5 0 0 0 12256 0 8 4294903133 1042 #addLabel 8 'Add' 1 1 0 0 false 0 0 0 754 138 144 34 3 818 #createAt:extent: 34 2 706 661 11 706 181 61 12256 818 #isEnabled: 8 #(false) 12256 818 #text: 34 1 8 'Add' 12256 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 74 1 0 0 5 0 0 0 164 1 0 0 35 0 0 0] 8 #() 1328 0 29 11746 11792 661 11808 181 11824 11 12240 61 170 192 34 2 11008 8 'labels' 0 754 138 144 34 2 818 #createAt:extent: 34 2 706 9 55 706 1213 711 10880 818 #text: 34 1 8 'Labels' 10880 898 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 0 27 0 0 0 98 2 0 0 126 1 0 0] 34 3 11008 12256 11856 1328 0 27 1840 170 192 656 0 410 ##(Smalltalk.TabView) 34 23 0 1664 34 2 8 1140916736 1 12896 11074 138 144 34 3 1824 9904 10864 0 11136 0 0 1 0 0 0 12896 0 8 4294903055 ##(Smalltalk.BasicListAbstract) ##(Smalltalk.IconicListAbstract) 11184 0 0 0 0 0 #noIcons 754 138 144 34 3 818 #createAt:extent: 34 2 706 1 1 706 1229 773 12896 818 #basicSelectionsByIndex: 34 1 8 #(1) 12896 818 #tcmSetExtendedStyle:dwExStyle: 8 #(-1 0) 12896 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 102 2 0 0 130 1 0 0] 8 #() 1328 0 27 754 138 144 34 1 818 #createAt:extent: 34 2 706 11 11 706 1229 773 1664 898 8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 5 0 0 0 5 0 0 0 107 2 0 0 135 1 0 0] 34 4 1840 9920 10880 12896 1328 0 27 170 192 656 674 706 11 11 706 11 11 0 0 0 0 1 0 0 0 706 1281 961 1 0 0 754 138 144 34 3 818 #createAt:extent: 34 2 706 3839 21 706 1281 961 416 818 #text: 34 1 8 'Smalltalk Documentation' 416 818 #updateMenuBar 656 416 898 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 127 7 0 0 10 0 0 0 255 9 0 0 234 1 0 0] 34 2 1664 544 1328 0 27 )! !
!STDocShell class categoriesFor: #defaultModel!public! !
!STDocShell class categoriesFor: #resource_Default_view!public!resources-views! !

