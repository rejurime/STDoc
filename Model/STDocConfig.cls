"Filed out from Dolphin Smalltalk 7"!

STDocObject subclass: #STDocConfig
	instanceVariableNames: 'renderPrivate renderPublic renderClassPublic renderClassPrivate templateName listeners templateDir docPath renderSuperclasses renderRelatedClasses renderClassAccessor renderAccessors'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocConfig guid: (GUID fromString: '{734557a4-5fc3-4d8e-b1f3-f2e27bd8a23e}')!
STDocConfig comment: ''!
!STDocConfig categoriesForClass!Unclassified! !
!STDocConfig methodsFor!

docPath
	^docPath!

docPath: anObject
	docPath := anObject!

initialize
	#todo.	"Quitar las carpetas clavadas y la mala configuracion."
	super initialize.
	listeners := OrderedCollection new.
	renderClassPublic := true.
	renderClassPrivate := false.
	renderPublic := true.
	renderPrivate := false.
	templateName := 'template1'.
	templateDir := FileLocator default basePath , 'STDoc\Resources'.
	docPath := FileLocator default basePath , 'models'.
	renderSuperclasses := true.
	renderRelatedClasses := true.
	renderClassAccessor := true.
	renderAccessors := true!

listeners
	^listeners!

listeners: anObject
	listeners := anObject!

makeDocAbsolute: aFileName
	"Composes the path with the document output path. If an absolute path is passes as the filename it remains unchanged, but if the path is relative it changes it to make it absolute with the documentation output path.

	@param aFileName the file name or path that you want to make absolute
	@return an absolute path"

	^File composePath: (File appendPathDelimiter: self docPath) subPath: aFileName!

makeTemplateAbsolute: aFileName
	"Composes the path with the template path. If an absolute path is passed as the filename it remains unchanged, but if the path is relative it changes it to make it absolute with the template path.

	@param aFileName the file name or path that you want to make absolute
	@return an absolute path"

	^File composePath: self templateDirectory subPath: aFileName!

renderAccessors
	^renderAccessors ifNil: [true]!

renderAccessors: anObject
	renderAccessors := anObject!

renderClassAccessor
	^renderClassAccessor!

renderClassAccessor: anObject
	renderClassAccessor := anObject!

renderClassPrivate
	^renderClassPrivate!

renderClassPrivate: anObject
	renderClassPrivate := anObject!

renderClassPublic
	^renderClassPublic!

renderClassPublic: anObject
	renderClassPublic := anObject!

renderPrivate
	^renderPrivate!

renderPrivate: anObject
	renderPrivate := anObject!

renderPublic
	^renderPublic!

renderPublic: anObject
	renderPublic := anObject!

renderRelatedClasses
	^renderRelatedClasses!

renderRelatedClasses: anObject
	renderRelatedClasses := anObject!

renderSuperclasses
	^renderSuperclasses!

renderSuperclasses: anObject
	renderSuperclasses := anObject!

templateDir
	^templateDir!

templateDir: anObject
	templateDir := anObject!

templateDirectory
	"The current template directory. This is the templateDir plus the template name"

	^File appendPathDelimiter: (File appendPathDelimiter: self templateDir) , self templateName!

templateFileName
	^self makeTemplateAbsolute: 'template.xml'!

templateName
	^templateName!

templateName: anObject
	templateName := anObject! !
!STDocConfig categoriesFor: #docPath!accessing!public! !
!STDocConfig categoriesFor: #docPath:!accessing!public! !
!STDocConfig categoriesFor: #initialize!public! !
!STDocConfig categoriesFor: #listeners!accessing!public! !
!STDocConfig categoriesFor: #listeners:!accessing!public! !
!STDocConfig categoriesFor: #makeDocAbsolute:!public! !
!STDocConfig categoriesFor: #makeTemplateAbsolute:!public! !
!STDocConfig categoriesFor: #renderAccessors!accessing!public! !
!STDocConfig categoriesFor: #renderAccessors:!accessing!public! !
!STDocConfig categoriesFor: #renderClassAccessor!accessing!public! !
!STDocConfig categoriesFor: #renderClassAccessor:!accessing!public! !
!STDocConfig categoriesFor: #renderClassPrivate!accessing!public! !
!STDocConfig categoriesFor: #renderClassPrivate:!accessing!public! !
!STDocConfig categoriesFor: #renderClassPublic!accessing!public! !
!STDocConfig categoriesFor: #renderClassPublic:!accessing!public! !
!STDocConfig categoriesFor: #renderPrivate!accessing!public! !
!STDocConfig categoriesFor: #renderPrivate:!accessing!public! !
!STDocConfig categoriesFor: #renderPublic!accessing!public! !
!STDocConfig categoriesFor: #renderPublic:!accessing!public! !
!STDocConfig categoriesFor: #renderRelatedClasses!accessing!public! !
!STDocConfig categoriesFor: #renderRelatedClasses:!accessing!public! !
!STDocConfig categoriesFor: #renderSuperclasses!accessing!public! !
!STDocConfig categoriesFor: #renderSuperclasses:!accessing!public! !
!STDocConfig categoriesFor: #templateDir!accessing!public! !
!STDocConfig categoriesFor: #templateDir:!accessing!public! !
!STDocConfig categoriesFor: #templateDirectory!public! !
!STDocConfig categoriesFor: #templateFileName!public! !
!STDocConfig categoriesFor: #templateName!accessing!public! !
!STDocConfig categoriesFor: #templateName:!accessing!public! !

