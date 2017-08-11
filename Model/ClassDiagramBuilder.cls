"Filed out from Dolphin Smalltalk 7"!

DOCObject subclass: #ClassDiagramBuilder
	instanceVariableNames: 'subjectClass methods classMethods outFile templateFile renderSuperclasses'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ClassDiagramBuilder guid: (GUID fromString: '{69F10040-C125-4CB3-9967-4D89253F3D69}')!
ClassDiagramBuilder comment: ''!
!ClassDiagramBuilder categoriesForClass!Unclassified! !
!ClassDiagramBuilder methodsFor!

addClassMethods: aBlock
	classMethods := ((classMethods ifNil: [Set new]) asSet)
				addAll: ((subjectClass class methodDictionary values select: aBlock) collect: [:m | m selector]);
				yourself!

addClassPrivateMethods
	self addClassMethods: [:com | com isPrivate]!

addClassPublicMethods
	self addClassMethods: [:com | com isPublic]!

addMethods: aBlock
	methods := ((methods ifNil: [Set new]) asSet)
				addAll: ((subjectClass methodDictionary values select: aBlock) collect: [:m | m selector]);
				yourself!

addPrivateMethods
	self addMethods: [:com | com isPrivate]!

addPublicMethods
	self addMethods: [:com | com isPublic]!

blankImage
	^'<img src="c:\templates\blank.jpg" />'!

classImage
	^'<img src="c:\templates\class.jpg" />'!

forClass: aClass
	subjectClass := aClass!

formatMethod: methodDef
	| parts result |
	result := String writeStream.
	parts := methodDef subStrings: ' '.
	1 to: parts size
		do: 
			[:i |
			i odd
				ifTrue: 
					[result
						nextPutAll: '<span style="color: blue;font-weight:bold">';
						nextPutAll: (parts at: i) trimBlanks;
						nextPutAll: ' </span> ']
				ifFalse: 
					[result
						nextPutAll: (parts at: i) trimBlanks;
						nextPutAll: ' ']].
	^result contents!

forSelectors: someMethods
	methods := someMethods!

getTemplate
	^templateFile!

inFile: aPath
	outFile := aPath!

initialize
	super initialize.
	subjectClass := Object.
	methods := #().
	classMethods := #().
	outFile := ''.
	templateFile := ''.
	renderSuperclasses := false!

methodTable
	| table |
	table := String writeStream.
	table
		nextPutAll: '<table style="border:0;width:100%">';
		cr.
	self
		renderMethods: methods
		in: table
		isClass: false.
	self
		renderMethods: classMethods
		in: table
		isClass: true.
	table nextPutAll: '</table>'.
	^table contents!

orderMethods
	methods := methods asSortedCollection: 
					[:s1 :s2 |
					| m1 m2 |
					m1 := subjectClass compiledMethodAt: s1.
					m2 := subjectClass compiledMethodAt: s2.
					m1 isPublic
						ifTrue: [m2 isPrivate or: [m1 selector <= m2 selector]]
						ifFalse: [m2 isPrivate and: [m1 selector < m2 selector]]].
	classMethods := classMethods asSortedCollection: 
					[:s1 :s2 |
					| m1 m2 |
					m1 := subjectClass class compiledMethodAt: s1.
					m2 := subjectClass class compiledMethodAt: s2.
					m1 isPublic
						ifTrue: [m2 isPrivate or: [m1 selector <= m2 selector]]
						ifFalse: [m2 isPrivate and: [m1 selector < m2 selector]]]!

overrideImage
	^'<img src="c:\templates\override.jpg" />'!

privateImage
	^'<img src="c:\templates\private.jpg" />'!

publicImage
	^'<img src="c:\templates\public.jpg" />'!

removeAccessorMethods
	self removeMethods: 
			[:com |
			"com categories inspect."
			false]!

removeClassAccessorMethods
	self removeClassMethods: 
			[:com |
			"com categories inspect."
			false]!

removeClassMethods: aBlock
	| selectors |
	selectors := classMethods ifNil: [Set new].
	selectors removeAll: (selectors
				select: [:selector | aBlock value: (subjectClass class compiledMethodAt: selector)]).
	classMethods := selectors!

removeMethods: aBlock
	| selectors |
	selectors := methods ifNil: [Set new].
	selectors
		removeAll: (selectors select: [:selector | aBlock value: (subjectClass compiledMethodAt: selector)]).
	methods := selectors!

renderMethods: someMethods in: table
	someMethods do: 
			[:selector |
			(subjectClass compiledMethodAt: selector ifAbsent: [])
				ifNotNil: 
					[:compMethod |
					table
						nextPutAll: '<tr><td style="border:1px inset #EEEEEE;color:#000000">';
						nextPutAll: self blankImage;
						nextPutAll: (compMethod isOverriden ifTrue: [self overrideImage] ifFalse: [self blankImage]);
						nextPutAll: (compMethod isPublic ifTrue: [self publicImage] ifFalse: [self privateImage]);
						nextPutAll: (self formatMethod: compMethod getSource readStream nextLine);
						nextPutAll: '</td></tr>';
						cr]]!

renderMethods: someMethods in: table isClass: isClass
	someMethods do: 
			[:selector |
			(subjectClass compiledMethodAt: selector ifAbsent: [])
				ifNotNil: 
					[:compMethod |
					table
						nextPutAll: '<tr><td style="border:1px inset #EEEEEE;color:#000000">';
						nextPutAll: (isClass ifTrue: [self classImage] ifFalse: [self blankImage]);
						nextPutAll: (compMethod isOverridden ifTrue: [self overrideImage] ifFalse: [self blankImage]);
						nextPutAll: (compMethod isPublic ifTrue: [self publicImage] ifFalse: [self privateImage]);
						nextPutAll: (self formatMethod: compMethod getSource readStream nextLine);
						nextPutAll: '</td></tr>';
						cr]]!

renderSuperclasses: aBool
	renderSuperclasses := aBool!

superclassesTable
	renderSuperclasses
		ifTrue: 
			[| table cls |
			table := String writeStream.
			table
				nextPutAll: '<table style="border:0;width:100%">';
				cr.
			cls := subjectClass superclass.
			[cls ~= nil] whileTrue: 
					[subjectClass instVarNames do: 
							[:var |
							table
								nextPutAll: '<tr><td colspan="2" style="border:1px inset #EEEEEE;color:#000000"><a href="';
								nextPutAll: cls name asString;
								nextPutAll: '.html">';
								nextPutAll: cls name asString;
								nextPutAll: '</a></td></tr>';
								cr;
								nextPutAll: '<tr><td style="border-right-color:#000000;border-right-width:1px"></td>';
								nextPutAll: '<td style="border-left-color:#000000;border-left-width:1px"></td></tr>';
								cr].
					cls := cls superclass].
			table nextPutAll: '</table>'.
			^table contents]
		ifFalse: [^'']!

templateFile: aPath
	templateFile := aPath!

variableTable
	| table |
	table := String writeStream.
	table
		nextPutAll: '<table style="border:0;width:100%">';
		cr.
	subjectClass classVarNames do: 
			[:var |
			table
				nextPutAll: '<tr><td style="border:1px inset #EEEEEE;color:#00FF00">';
				nextPutAll: self classImage;
				nextPutAll: var;
				nextPutAll: '</td></tr>';
				cr].
	subjectClass instVarNames do: 
			[:var |
			table
				nextPutAll: '<tr><td style="border:1px inset #EEEEEE;color:#000000">';
				nextPutAll: self blankImage;
				nextPutAll: var;
				nextPutAll: '</td></tr>';
				cr].
	table nextPutAll: '</table>'.
	^table contents!

writeFile
	| template |
	template := self getTemplate.
	template formatWith: ((LookupTable new)
				at: 'className' put: subjectClass name asString;
				at: 'superclasses' put: self superclassesTable;
				at: 'variables' put: self variableTable;
				at: 'methods' put: self methodTable;
				shrink;
				yourself)
		in: outFile! !
!ClassDiagramBuilder categoriesFor: #addClassMethods:!methods!public! !
!ClassDiagramBuilder categoriesFor: #addClassPrivateMethods!methods!public! !
!ClassDiagramBuilder categoriesFor: #addClassPublicMethods!methods!public! !
!ClassDiagramBuilder categoriesFor: #addMethods:!methods!public! !
!ClassDiagramBuilder categoriesFor: #addPrivateMethods!methods!public! !
!ClassDiagramBuilder categoriesFor: #addPublicMethods!methods!public! !
!ClassDiagramBuilder categoriesFor: #blankImage!images!private! !
!ClassDiagramBuilder categoriesFor: #classImage!images!private! !
!ClassDiagramBuilder categoriesFor: #forClass:!public! !
!ClassDiagramBuilder categoriesFor: #formatMethod:!methods!private! !
!ClassDiagramBuilder categoriesFor: #forSelectors:!public! !
!ClassDiagramBuilder categoriesFor: #getTemplate!private! !
!ClassDiagramBuilder categoriesFor: #inFile:!public! !
!ClassDiagramBuilder categoriesFor: #initialize!methods!private! !
!ClassDiagramBuilder categoriesFor: #methodTable!methods!private! !
!ClassDiagramBuilder categoriesFor: #orderMethods!public! !
!ClassDiagramBuilder categoriesFor: #overrideImage!images!private! !
!ClassDiagramBuilder categoriesFor: #privateImage!images!private! !
!ClassDiagramBuilder categoriesFor: #publicImage!images!private! !
!ClassDiagramBuilder categoriesFor: #removeAccessorMethods!methods!public! !
!ClassDiagramBuilder categoriesFor: #removeClassAccessorMethods!methods!public! !
!ClassDiagramBuilder categoriesFor: #removeClassMethods:!methods!public! !
!ClassDiagramBuilder categoriesFor: #removeMethods:!methods!public! !
!ClassDiagramBuilder categoriesFor: #renderMethods:in:!methods!private! !
!ClassDiagramBuilder categoriesFor: #renderMethods:in:isClass:!methods!private! !
!ClassDiagramBuilder categoriesFor: #renderSuperclasses:!public! !
!ClassDiagramBuilder categoriesFor: #superclassesTable!private!variables! !
!ClassDiagramBuilder categoriesFor: #templateFile:!public! !
!ClassDiagramBuilder categoriesFor: #variableTable!private!variables! !
!ClassDiagramBuilder categoriesFor: #writeFile!public! !

!ClassDiagramBuilder class methodsFor!

forClass: aClass
	^self forClass: (aClass forFile: 'c:\pepe.html')!

generarDiagramasPara: unasClases en: unPath
	self
		generarDiagramasPara: unasClases
		en: unPath
		private: false
		accessors: false!

generarDiagramasPara: unasClases en: unPath private: renderPrivates accessors: renderAccessors
	self
		generarDiagramasPara: unasClases
		en: unPath
		renderSuperclasses: true
		private: renderPrivates
		accessors: renderAccessors
		classPublics: true
		classPrivates: false
		classAccessors: false!

generarDiagramasPara: unasClases en: unPath renderSuperclasses: renderSuperclasses private: renderPrivates accessors: renderAccessors classPublics: renderCPub classPrivates: renderCPriv classAccessors: renderClassAcc
	| path |
	path := File appendPathDelimiter: unPath.
	(renderSuperclasses ifTrue: [self searchSuperclasses: unasClases] ifFalse: [unasClases]) do: 
			[:class |
			| builder |
			builder := ClassDiagramBuilder forClass: (class forFile: path , class name asString , '.html').
			builder addPublicMethods.
			builder renderSuperclasses: renderSuperclasses.
			renderPrivates ifTrue: [builder addPrivateMethods].
			renderAccessors ifFalse: [builder removeAccessorMethods].
			renderCPub ifTrue: [builder addClassPublicMethods].
			renderCPriv ifTrue: [builder addClassPrivateMethods].
			renderClassAcc ifFalse: [builder removeClassAccessorMethods].
			builder orderMethods.
			builder writeFile]!

new
	^self basicNew initialize!

searchSuperclasses: unasClases
	| clases |
	clases := Set new.
	unasClases do: 
			[:cls |
			clases
				add: cls;
				addAll: cls allSuperclasses].
	^clases! !
!ClassDiagramBuilder class categoriesFor: #forClass:!public! !
!ClassDiagramBuilder class categoriesFor: #generarDiagramasPara:en:!public! !
!ClassDiagramBuilder class categoriesFor: #generarDiagramasPara:en:private:accessors:!public! !
!ClassDiagramBuilder class categoriesFor: #generarDiagramasPara:en:renderSuperclasses:private:accessors:classPublics:classPrivates:classAccessors:!public! !
!ClassDiagramBuilder class categoriesFor: #new!public! !
!ClassDiagramBuilder class categoriesFor: #searchSuperclasses:!private! !

