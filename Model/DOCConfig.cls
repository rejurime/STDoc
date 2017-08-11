"Filed out from Dolphin Smalltalk 7"!

DOCObject subclass: #DOCConfig
	instanceVariableNames: 'renderPrivate renderPublic renderClassPublic renderClassPrivate templateName listeners templateDir docPath renderSuperclasses renderRelatedClasses renderClassAccessor renderAccessors'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCConfig guid: (GUID fromString: '{734557A4-5FC3-4D8E-B1F3-F2E27BD8A23E}')!
DOCConfig comment: ''!
!DOCConfig categoriesForClass!Unclassified! !
!DOCConfig methodsFor!

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
!DOCConfig categoriesFor: #docPath!accessing!public! !
!DOCConfig categoriesFor: #docPath:!accessing!public! !
!DOCConfig categoriesFor: #initialize!public! !
!DOCConfig categoriesFor: #listeners!accessing!public! !
!DOCConfig categoriesFor: #listeners:!accessing!public! !
!DOCConfig categoriesFor: #makeDocAbsolute:!public! !
!DOCConfig categoriesFor: #makeTemplateAbsolute:!public! !
!DOCConfig categoriesFor: #renderAccessors!accessing!public! !
!DOCConfig categoriesFor: #renderAccessors:!accessing!public! !
!DOCConfig categoriesFor: #renderClassAccessor!accessing!public! !
!DOCConfig categoriesFor: #renderClassAccessor:!accessing!public! !
!DOCConfig categoriesFor: #renderClassPrivate!accessing!public! !
!DOCConfig categoriesFor: #renderClassPrivate:!accessing!public! !
!DOCConfig categoriesFor: #renderClassPublic!accessing!public! !
!DOCConfig categoriesFor: #renderClassPublic:!accessing!public! !
!DOCConfig categoriesFor: #renderPrivate!accessing!public! !
!DOCConfig categoriesFor: #renderPrivate:!accessing!public! !
!DOCConfig categoriesFor: #renderPublic!accessing!public! !
!DOCConfig categoriesFor: #renderPublic:!accessing!public! !
!DOCConfig categoriesFor: #renderRelatedClasses!accessing!public! !
!DOCConfig categoriesFor: #renderRelatedClasses:!accessing!public! !
!DOCConfig categoriesFor: #renderSuperclasses!accessing!public! !
!DOCConfig categoriesFor: #renderSuperclasses:!accessing!public! !
!DOCConfig categoriesFor: #templateDir!accessing!public! !
!DOCConfig categoriesFor: #templateDir:!accessing!public! !
!DOCConfig categoriesFor: #templateDirectory!public! !
!DOCConfig categoriesFor: #templateFileName!public! !
!DOCConfig categoriesFor: #templateName!accessing!public! !
!DOCConfig categoriesFor: #templateName:!accessing!public! !

