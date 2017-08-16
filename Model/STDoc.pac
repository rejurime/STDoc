| package |
package := Package name: 'STDoc'.
package paxVersion: 1;
	basicComment: 'STDoc 1.1
------------------------
Creado:	21 de febrero del 2007
Autores:	Javier Capanegra

Descripcion:
-----------------
Este paquete es utilizado para documentar las clases, los métodos y los paquetes que componen un aplicación. La sintaxis de documentación es similar al Javadoc.

Este paquete interpreta los comentarios de cada uno de estos elementos (metodos, clases y paquetes) y genera archivos HTML con la documentación correspondiente.

Este paquete realiza índices por:
* órden alfabético
* jerarquía de clases
* paquetes

Para ver ejemplos de como documentar se pueden observar los paquetes de componentes que tienen todos sus elementos bien documentados.


generarDiagramasPara: unasClases en: unPath renderSuperclasses: renderSuperclasses private: renderPrivates accessors: renderAccessors classPublics: renderCPub classPrivates: renderCPriv classAccessors: renderClassAcc

forClass: aClass'.

package basicPackageVersion: '1.0'.

package imageStripperBytes: (ByteArray fromBase64String: 'IVNUQiAxIAAAAAA=').

package classNames
	add: #ClassDiagramBuilder;
	add: #STDocAnd;
	add: #STDocClass;
	add: #STDocClassRenderer;
	add: #STDocClassTemplate;
	add: #STDocClassTemplateRenderer;
	add: #STDocComment;
	add: #STDocCondCmd;
	add: #STDocCondition;
	add: #STDocConfig;
	add: #STDocDefaultProcessor;
	add: #STDocIndexRenderer;
	add: #STDocIndexTemplate;
	add: #STDocMethod;
	add: #STDocMethodRenderer;
	add: #STDocObject;
	add: #STDocOr;
	add: #STDocPackageRenderer;
	add: #STDocPackageTemplate;
	add: #STDocParseError;
	add: #STDocPredefinedCond;
	add: #STDocProcessor;
	add: #STDocRenderCmd;
	add: #STDocRenderer;
	add: #STDocSmalltalkManager;
	add: #STDocTemplateSpec;
	add: #STDocTextCmd;
	add: #STDocTextRenderer;
	add: #STDocUtil;
	add: #STDocVariable;
	add: #STDocVariableRenderer;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\..\Core\Object Arts\Dolphin\Base\Dolphin';
	add: '..\..\Core\Object Arts\Dolphin\ActiveX\Components\XML DOM\XML DOM';
	yourself).

package!

"Class Definitions"!

Object subclass: #STDocObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Error subclass: #STDocParseError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocObject subclass: #ClassDiagramBuilder
	instanceVariableNames: 'subjectClass methods classMethods outFile templateFile renderSuperclasses'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocObject subclass: #STDocClass
	instanceVariableNames: 'manager classObject classMethods methods variables classVariables instanceClassVariables docComment superClasses subClasses'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocObject subclass: #STDocComment
	instanceVariableNames: 'comment attrTable fullComment smallComment combined'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocObject subclass: #STDocCondition
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocObject subclass: #STDocConfig
	instanceVariableNames: 'renderPrivate renderPublic renderClassPublic renderClassPrivate templateName listeners templateDir docPath renderSuperclasses renderRelatedClasses renderClassAccessor renderAccessors'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocObject subclass: #STDocMethod
	instanceVariableNames: 'comment methodObject definition id'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocObject subclass: #STDocProcessor
	instanceVariableNames: 'manager fileName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocObject subclass: #STDocRenderCmd
	instanceVariableNames: 'renderer'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocObject subclass: #STDocRenderer
	instanceVariableNames: 'manager headerCommands bodyCommands footerCommands childRenderers parent'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocObject subclass: #STDocSmalltalkManager
	instanceVariableNames: 'config processors configKeys progress defaultStyle privIcon pubIcon overIcon classIcon blankIcon classes stClassSet packages'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocObject subclass: #STDocUtil
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocObject subclass: #STDocVariable
	instanceVariableNames: 'name docClass type'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocCondition subclass: #STDocAnd
	instanceVariableNames: 'conditions'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocCondition subclass: #STDocOr
	instanceVariableNames: 'conditions'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocCondition subclass: #STDocPredefinedCond
	instanceVariableNames: 'renderer condition'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocProcessor subclass: #STDocClassTemplate
	instanceVariableNames: 'renderers outDir'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocProcessor subclass: #STDocDefaultProcessor
	instanceVariableNames: 'renderers outFile'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocProcessor subclass: #STDocIndexTemplate
	instanceVariableNames: 'hierarchy renderers'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocProcessor subclass: #STDocTemplateSpec
	instanceVariableNames: 'filesToCopy'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocClassTemplate subclass: #STDocPackageTemplate
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocRenderCmd subclass: #STDocCondCmd
	instanceVariableNames: 'trueCommands falseCommands condition'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocRenderCmd subclass: #STDocTextCmd
	instanceVariableNames: 'name params'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocRenderer subclass: #STDocClassTemplateRenderer
	instanceVariableNames: 'header footer'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocRenderer subclass: #STDocIndexRenderer
	instanceVariableNames: 'type fileName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocRenderer subclass: #STDocPackageRenderer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocRenderer subclass: #STDocTextRenderer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocClassTemplateRenderer subclass: #STDocClassRenderer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocClassTemplateRenderer subclass: #STDocMethodRenderer
	instanceVariableNames: 'methodType'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
STDocClassTemplateRenderer subclass: #STDocVariableRenderer
	instanceVariableNames: 'varType'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

STDocObject guid: (GUID fromString: '{592a9d63-f819-4782-a5a0-197d460fb430}')!
STDocObject comment: ''!
!STDocObject categoriesForClass!Unclassified! !
!STDocObject methodsFor!

parseError: aString
	^STDocParseError signal: aString! !
!STDocObject categoriesFor: #parseError:!private! !

!STDocObject class methodsFor!

new
	"Answer a new initialized instance."

	^self basicNew initialize! !
!STDocObject class categoriesFor: #new!public! !

STDocParseError guid: (GUID fromString: '{b4569eaa-abd3-4562-b3c7-5af2a2b3221d}')!
STDocParseError comment: ''!
!STDocParseError categoriesForClass!Unclassified! !
ClassDiagramBuilder guid: (GUID fromString: '{69f10040-c125-4cb3-9967-4d89253f3d69}')!
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

STDocClass guid: (GUID fromString: '{4f92fa62-a6bc-4ff7-8fcb-0c823a5b6155}')!
STDocClass comment: ''!
!STDocClass categoriesForClass!Unclassified! !
!STDocClass methodsFor!

= anObject
	^self class = anObject class and: [self name = anObject name]!

classMethods
	^classMethods
		ifNil: 
			[self selectMethods.
			classMethods]!

classMethods: anObject
	classMethods := anObject!

classObject
	^classObject!

classObject: anObject
	classObject := anObject!

classReferencesFrom: aMethod
	"Private - Returns a collection with classes referenced by the method"

	| col |
	col := Set new.
	1 to: aMethod size
		do: 
			[:c |
			| obj |
			obj := aMethod at: c.
			(obj isKindOf: Association)
				ifTrue: 
					[obj value isBehavior ifTrue: [col add: (STDocClass forClass: obj value manager: self manager)]]].
	^col!

classVariables
	^classVariables
		ifNil: 
			[self selectVariables.
			classVariables]!

classVariables: anObject
	classVariables := anObject!

docComment
	^docComment ifNil: [docComment := STDocComment forClass: self classObject]!

hash
	^self class hash bitXor: self name hash!

instanceClassVariables
	^instanceClassVariables!

instanceClassVariables: anObject
	instanceClassVariables := anObject!

manager
	^manager!

manager: anObject
	manager := anObject!

methods
	^methods
		ifNil: 
			[self selectMethods.
			methods]!

methods: anObject
	methods := anObject!

name
	^self classObject name!

package
	^Package manager packageOfClass: self classObject!

printOn: aStream
	aStream
		basicPrint: self;
		nextPut: $(;
		display: self name;
		nextPut: $)!

relatedClasses
	| result |
	result := Set new.
	self classObject
		selectorsAndMethodsDo: [:selctor :compiledMethod | result addAll: (self classReferencesFrom: compiledMethod)].
	^result asOrderedCollection!

searchMethod: aSymbol
	^self methods detect: [:met | met methodObject selector = aSymbol]
		ifNone: [self classMethods detect: [:met | met methodObject selector = aSymbol] ifNone: []]!

selectMethods
	methods := self
				sortMethods: (classObject methodDictionary values select: 
							[:m |
							(manager config renderClassPrivate and: [m isPrivate])
								or: [manager config renderClassPublic and: [m isPrivate not]]]).
	classMethods := self
				sortMethods: (classObject class methodDictionary values select: 
							[:m |
							(manager config renderPrivate and: [m isPrivate])
								or: [manager config renderPublic and: [m isPrivate not]]])!

selectVariables
	classVariables := (classObject classVarNames collect: [:v | STDocVariable forClassVar: self name: v])
				asOrderedCollection.
	variables := (classObject instVarNames collect: [:v | STDocVariable forInstVar: self name: v])
				asOrderedCollection.
	instanceClassVariables := (classObject class instVarNames
				collect: [:v | STDocVariable forClassInstVar: self name: v]) asOrderedCollection!

setFor: aClass
	classObject := aClass!

sortMethods: someMethods
	^(someMethods collect: [:m | STDocMethod for: m]) asSortedCollection!

subclasses
	subClasses notNil ifTrue: [^subClasses].
	subClasses := self classObject subclasses asSortedCollection: [:c1 :c2 | c1 name <= c2 name].
	^subClasses!

superclass
	^self classObject superclass
		ifNil: []
		ifNotNil: [:sprCls | STDocClass forClass: sprCls manager: self manager]!

superClasses
	"Makes a Collection of super classes of the contained class, that are ordered by proximity to the contained class reversed.
	The first element is the farest superclass of the class, generally Object class and the last element is the inmediate superclass.

	@return an OrderedCollection with the contained class super classes"

	| supers sprClass |
	superClasses notNil ifTrue: [^superClasses].
	supers := OrderedCollection new.
	sprClass := self superclass ifNotNil: [:cls | cls classObject].
	[sprClass notNil] whileTrue: 
			[supers add: (STDocClass forClass: sprClass manager: self manager).
			sprClass := sprClass superclass].
	superClasses := supers reverse.
	^superClasses!

variables
	^variables
		ifNil: 
			[self selectVariables.
			variables]!

variables: anObject
	variables := anObject! !
!STDocClass categoriesFor: #=!public! !
!STDocClass categoriesFor: #classMethods!accessing!public! !
!STDocClass categoriesFor: #classMethods:!accessing!public! !
!STDocClass categoriesFor: #classObject!accessing!public! !
!STDocClass categoriesFor: #classObject:!accessing!public! !
!STDocClass categoriesFor: #classReferencesFrom:!private! !
!STDocClass categoriesFor: #classVariables!accessing!public! !
!STDocClass categoriesFor: #classVariables:!accessing!public! !
!STDocClass categoriesFor: #docComment!public! !
!STDocClass categoriesFor: #hash!public! !
!STDocClass categoriesFor: #instanceClassVariables!accessing!public! !
!STDocClass categoriesFor: #instanceClassVariables:!accessing!public! !
!STDocClass categoriesFor: #manager!accessing!public! !
!STDocClass categoriesFor: #manager:!accessing!public! !
!STDocClass categoriesFor: #methods!accessing!public! !
!STDocClass categoriesFor: #methods:!accessing!public! !
!STDocClass categoriesFor: #name!public! !
!STDocClass categoriesFor: #package!public! !
!STDocClass categoriesFor: #printOn:!public! !
!STDocClass categoriesFor: #relatedClasses!public! !
!STDocClass categoriesFor: #searchMethod:!public! !
!STDocClass categoriesFor: #selectMethods!private! !
!STDocClass categoriesFor: #selectVariables!private! !
!STDocClass categoriesFor: #setFor:!public! !
!STDocClass categoriesFor: #sortMethods:!private! !
!STDocClass categoriesFor: #subclasses!public! !
!STDocClass categoriesFor: #superclass!public! !
!STDocClass categoriesFor: #superClasses!public! !
!STDocClass categoriesFor: #variables!accessing!public! !
!STDocClass categoriesFor: #variables:!accessing!public! !

!STDocClass class methodsFor!

forClass: aClass manager: aManager
	"Creates a DOCClassfor a specific class and manager

	@param aClass the real smalltalk class
	@param aManager the manager that has the configuration"

	^(self new)
		manager: aManager;
		setFor: aClass;
		yourself! !
!STDocClass class categoriesFor: #forClass:manager:!public! !

STDocComment guid: (GUID fromString: '{fdcd6495-e6e7-44c5-a2f4-34eb915bda97}')!
STDocComment comment: ''!
!STDocComment categoriesForClass!Unclassified! !
!STDocComment methodsFor!

attributeIncludes: aName
	^self attributeTable includesKey: aName!

attributeIncludesAll: anArray
	| at |
	at := self attributeTable.
	^anArray allSatisfy: [:aName | at includesKey: aName]!

attributesAt: aName do: aBlock
	(self attributesAt: aName ifAbsent: [^self]) do: [:a | aBlock value: a]!

attributesAt: aName ifAbsent: aBlock
	^self attributeTable at: aName ifAbsent: [aBlock value]!

attributesIn: anArray do: aBlock
	anArray do: [:aName | self attributesAt: aName do: [:attr | aBlock value: aName value: attr]]!

attributeTable
	"Generates a LookupTable with the attributes that are detailed at the end of the comment starting with the @ char.

	@return a LookupTable that has as key the type of attribute and as value a collection of all the attributes texts"

	| str attribute result |
	attrTable notNil ifTrue: [^attrTable].
	str := self comment readStream.
	STDocUtil ignoreFrom: str upTo: [:c | c == $@].
	result := LookupTable new.
	[(attribute := STDocUtil stringFrom: str upTo: [:c | c == $@]) notNil] whileTrue: 
			[| id partStream |
			partStream := attribute readStream.
			id := STDocUtil firstWordOf: partStream.
			(result at: id ifAbsentPut: [OrderedCollection new]) add: partStream upToEnd trimBlanks].
	attrTable := result.
	^result!

combine: overComment
	combined ifTrue: [^self].
	self fullComment.
	(fullComment notEmpty and: [fullComment notNil])
		ifTrue: [fullComment := fullComment , String lineDelimiter].
	fullComment := fullComment , '<i>(Comment copied from superclass)</i>' , String lineDelimiter
				, overComment fullComment.
	smallComment := overComment smallComment.
	self attributeTable addAll: overComment attributeTable associations.
	combined := true!

comment
	^comment!

comment: anObject
	comment := anObject.
	attrTable := nil!

fullComment
	"Gets all the description part of the comment without including the descriptions detailed with @.

	@return a string with the description comment"

	^fullComment ifNil: [fullComment := (self comment readStream upTo: $@) trimBlanks]!

hasLargeComment
	^self smallComment size < self fullComment size or: [self attributeTable notEmpty]!

initialize
	super initialize.
	self comment: ''.
	combined := false!

parseMethod: aMethod
	"Private - Searches for the comment that identifies this method.
	
	@param aMethod the method
	@returns a string with the first comment that comes after the method definition"

	| source |
	source := aMethod getSource readStream.
	source nextLine.	"Skip definition"
	source skipSeparators.
	source atEnd
		ifTrue: [self comment: '']
		ifFalse: [self comment: (source next = $" ifTrue: [source upTo: $"] ifFalse: [''])]!

smallComment
	"Gets the first sentence or the first line of the comment. It stops when it reaches a punctuation sign or the new line.

	@return a string with the first sentence of the comment"

	^smallComment
		ifNil: 
			[| line |
			line := self fullComment readStream nextLine readStream.
			smallComment := ((STDocUtil stringFrom: line upTo: [:c | c = $.])
						ifNil: ['']
						ifNotNil: [:s | s , '.']) trimBlanks]! !
!STDocComment categoriesFor: #attributeIncludes:!public! !
!STDocComment categoriesFor: #attributeIncludesAll:!public! !
!STDocComment categoriesFor: #attributesAt:do:!public! !
!STDocComment categoriesFor: #attributesAt:ifAbsent:!public! !
!STDocComment categoriesFor: #attributesIn:do:!public! !
!STDocComment categoriesFor: #attributeTable!public! !
!STDocComment categoriesFor: #combine:!public! !
!STDocComment categoriesFor: #comment!accessing!public! !
!STDocComment categoriesFor: #comment:!accessing!public! !
!STDocComment categoriesFor: #fullComment!public! !
!STDocComment categoriesFor: #hasLargeComment!public! !
!STDocComment categoriesFor: #initialize!private! !
!STDocComment categoriesFor: #parseMethod:!private! !
!STDocComment categoriesFor: #smallComment!public! !

!STDocComment class methodsFor!

for: aString
	^(self new)
		comment: aString;
		yourself!

forClass: aClass
	^self for: aClass comment!

forMethod: aMethod
	^(self new)
		parseMethod: aMethod;
		yourself! !
!STDocComment class categoriesFor: #for:!public! !
!STDocComment class categoriesFor: #forClass:!public! !
!STDocComment class categoriesFor: #forMethod:!public! !

STDocCondition guid: (GUID fromString: '{0f0025e8-3122-42a2-a1a1-45429c02e814}')!
STDocCondition comment: ''!
!STDocCondition categoriesForClass!Unclassified! !
!STDocCondition methodsFor!

cand: aCondition
	^(STDocAnd new)
		conditions: self;
		condition2: aCondition;
		yourself!

cor: aCondition
	^(STDocOr new)
		conditions: self;
		condition2: aCondition;
		yourself!

value: anObject
	"Debe procesar lo que sea necesario y retornar el valor booleano que corresponda.

	@param anObject es lo que se está renderizando, puede ser una clase, un método o una variable
	@returns true si la condición es válida o en caso contrario false"

	^self subclassResponsibility! !
!STDocCondition categoriesFor: #cand:!public! !
!STDocCondition categoriesFor: #cor:!public! !
!STDocCondition categoriesFor: #value:!public! !

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

STDocMethod guid: (GUID fromString: '{29133369-fefe-4cf1-942d-e5553bc40746}')!
STDocMethod comment: ''!
!STDocMethod categoriesForClass!Unclassified! !
!STDocMethod methodsFor!

<= aDOCMethod
	^self species = aDOCMethod species and: 
			[self methodObject isPublic
				ifTrue: 
					[aDOCMethod methodObject isPrivate
						or: [self methodObject selector <= aDOCMethod methodObject selector]]
				ifFalse: 
					[aDOCMethod methodObject isPrivate
						and: [self methodObject selector < aDOCMethod methodObject selector]]]!

comment
	^comment ifNil: [comment := STDocComment forMethod: self methodObject]!

comment: anObject
	comment := anObject!

definition
	definition notNil ifTrue: [^definition].
	definition := self methodObject getSource readStream nextLine.
	^definition!

definitionIDOn: aStream
	STDocUtil makeIDString: self definition on: aStream!

methodObject
	^methodObject!

methodObject: anObject
	methodObject := anObject! !
!STDocMethod categoriesFor: #<=!public! !
!STDocMethod categoriesFor: #comment!accessing!public! !
!STDocMethod categoriesFor: #comment:!accessing!public! !
!STDocMethod categoriesFor: #definition!public! !
!STDocMethod categoriesFor: #definitionIDOn:!public! !
!STDocMethod categoriesFor: #methodObject!accessing!public! !
!STDocMethod categoriesFor: #methodObject:!accessing!public! !

!STDocMethod class methodsFor!

for: aMethod
	"Creates an instance for the specified method.

	@param aMethod the CompiledMethod that's going to be contained"

	^(self new)
		methodObject: aMethod;
		yourself! !
!STDocMethod class categoriesFor: #for:!public! !

STDocProcessor guid: (GUID fromString: '{c9c2770e-9f5c-4edd-af80-819fdf7fe214}')!
STDocProcessor comment: 'The subclasses of this class are the ones that handle the documentation process and the template files.
A type of template is associated to each class.
Each class has a different purpose and output. Some handle each class and makes documentation for them, other generates index files to navigate the files, other copy files and instantiates new documenters, etc.

The class that invoques the documenters is the DOCSmalltalkManager when the documentation process starts.'!
!STDocProcessor categoriesForClass!Unclassified! !
!STDocProcessor methodsFor!

fileName
	^fileName!

fileName: anObject
	fileName := anObject!

loadConfig: aXMLNode
	"Loads extra information that may be in the main template file.
	
	@param anXMLNode is the node that is used to instantiate the object"

	!

loadFile
	| doc root |
	doc := IXMLDOMDocument new.
	doc loadURL: (self manager config makeTemplateAbsolute: self fileName).
	root := doc documentElement.
	self parseDocument: root!

manager
	^manager!

manager: anObject
	manager := anObject!

parseDocument: anXMLNode
	"Initializes the state of the object with information contained in an XML file.
	
	@param anXMLNode the root node of an XML file"

	^self subclassResponsibility!

possibleChildTypes
	"Returns a Set of classes of renderers that can be childs of this type of renderer"

	^Set new!

processDoc
	"Executes the main purpose of the documenter, in general generates the output documentation files"

	^self subclassResponsibility! !
!STDocProcessor categoriesFor: #fileName!accessing!public! !
!STDocProcessor categoriesFor: #fileName:!accessing!public! !
!STDocProcessor categoriesFor: #loadConfig:!parsing!public! !
!STDocProcessor categoriesFor: #loadFile!parsing!public! !
!STDocProcessor categoriesFor: #manager!accessing!public! !
!STDocProcessor categoriesFor: #manager:!accessing!public! !
!STDocProcessor categoriesFor: #parseDocument:!parsing!public! !
!STDocProcessor categoriesFor: #possibleChildTypes!public! !
!STDocProcessor categoriesFor: #processDoc!document!public! !

!STDocProcessor class methodsFor!

forManager: aDOCSmalltalkManager
	^(self basicNew)
		manager: aDOCSmalltalkManager;
		initialize;
		yourself!

processorClassFor: processorName ifAbsent: aBlock
	^self allSubclasses
		detect: [:procC | procC processorName notNil and: [procC processorName = processorName]]
		ifNone: [aBlock value]!

processorName
	"The name of the listener used in the XML files"

	^self subclassResponsibility! !
!STDocProcessor class categoriesFor: #forManager:!public! !
!STDocProcessor class categoriesFor: #processorClassFor:ifAbsent:!private! !
!STDocProcessor class categoriesFor: #processorName!private! !

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

STDocRenderer guid: (GUID fromString: '{ce39d2a6-0443-46fb-8114-a0a267b389bc}')!
STDocRenderer comment: 'This class handles the rendering of the documentation. They are instantiated from the XML.
It has two parts, the first is the template, where some special tags are replaced with information that the render formats. The other part is the child templates, those same tags make the child renderers of this render to print it''s data.

@var manager the manager that contains the renderer
@var commands the commands that are replaced with information. Each renderer has different commands
@var childRenderers a collection of child renderers identified by an id
@var parent if this render is a child render it contains the renderer that has this renderer as a child, otherwise is nil'!
!STDocRenderer categoriesForClass!Unclassified! !
!STDocRenderer methodsFor!

addCommand: commandMethod params: paramArray
	"Adds a command to be executed at the end of the command list.
	
	@param commandMethod the command method to be executed
	@param paramArray an array with parameters"

	self
		addCommand: commandMethod
		params: paramArray
		to: bodyCommands!

addCommand: commandMethod params: paramArray to: aCollection
	"Adds a command to be executed at the end of the command list.
	
	@param commandMethod the command method to be executed
	@param paramArray an array with parameters
	@param aCollection a collection where to add"

	aCollection add: ((STDocTextCmd for: self)
				name: commandMethod;
				params: paramArray;
				yourself)!

availableCommands
	"The available renderers that can be used to render objects

	@returns a lookup table that has as key the command name and as value the method selector"

	^(LookupTable new)
		at: '' put: #renderText:params:on:;
		at: 'CHILDRENDER' put: #renderChild:params:on:;
		at: 'LABEL' put: #renderLabel:params:on:;
		yourself!

bodyCommands
	^bodyCommands!

bodyCommands: anObject
	bodyCommands := anObject!

container
	"Returns the object that contains this renderer and is not a renderer. In general it is the processor that contains the renderer

	@returns the processor that contains the renderer or nil"

	| container |
	[(container := self parent) isKindOf: STDocRenderer] whileTrue.
	^container!

getObjectsOf: aDOCClass
	"Private - Returns a Collection with objects that are going to be rendered"

	^Array with: aDOCClass!

initialize
	super initialize.
	bodyCommands := OrderedCollection new.
	childRenderers := LookupTable new!

isTrue: condString of: anObject
	self
		error: 'La condición ' , condString , ' no es soportada por ' , self class name , ' para el objeto '
				, anObject displayString!

manager
	^manager!

manager: anObject
	manager := anObject!

parent
	^parent!

parent: anObject
	parent := anObject!

parseCommandName: aCmdStr
	"Extracts the name of the command from the string.
	
	@returns a string with the name"

	^aCmdStr readStream upTo: $:!

parseCommandParams: aCmdStr
	"Extracts the parameter array from the command string.
	
	@returns an array with the parameters"

	| str params escaped |
	str := aCmdStr readStream.
	params := OrderedCollection new.
	str skipTo: $:.
	escaped := false.
	[str atEnd] whileFalse: 
			[| param |
			param := STDocUtil stringFrom: str
						filterBlock: 
							[:c |
							escaped
								ifTrue: 
									[escaped := false.
									1]
								ifFalse: 
									[c = $\
										ifTrue: 
											[escaped := true.
											2]
										ifFalse: [c = $: ifTrue: [0] ifFalse: [1]]]].
			param isNil ifFalse: [params addLast: param trimBlanks]].
	^params!

parseCondition: node
	| predefined op cond |
	predefined := node attributes at: 'predefined' ifAbsent: [].
	predefined
		ifNil: 
			[op := node attributes at: 'op'
						ifAbsent: [self error: 'Condición invalida, no contiene el atributo "op" o "predefined"'].
			(op sameAs: 'and')
				ifTrue: [cond := STDocAnd new]
				ifFalse: 
					[(op sameAs: 'or')
						ifTrue: [cond := STDocOr new]
						ifFalse: [self error: 'Operador ' , op , ' inválido']].
			cond conditions: (node childNodes collect: [:condNode | self parseCondition: condNode]).
			^cond].
	^(STDocPredefinedCond new)
		condition: predefined;
		renderer: self;
		yourself!

parseConditionFrom: node
	^Error notYetImplemented!

parseConditionFrom: node on: cmdCol
	| cmd |
	cmd := (STDocCondCmd new)
				condition: (self parseCondition: (node selectSingleNode: './condition'));
				yourself.
	self
		parseNode: node
		nodeName: 'ifTrue'
		commandsOn: cmd trueCommands.
	self
		parseNode: node
		nodeName: 'ifFalse'
		commandsOn: cmd falseCommands.
	cmdCol add: cmd!

parseNode: aNode
	"It parses the renderer node and initializes the internal data.

	@param aNode the xml node"

	bodyCommands := OrderedCollection new.
	headerCommands := OrderedCollection new.
	footerCommands := OrderedCollection new.
	self
		parseNode: aNode
		nodeName: 'template'
		commandsOn: bodyCommands.
	self
		parseNode: aNode
		nodeName: 'footer'
		commandsOn: footerCommands.
	self
		parseNode: aNode
		nodeName: 'header'
		commandsOn: headerCommands.
	(aNode selectNodes: './/render') do: 
			[:childNode |
			| childRenderer |
			childRenderer := STDocRenderer
						createRenderer: childNode
						manager: self manager
						parent: self.
			childRenderers
				at: (childNode attributes at: 'id'
						ifAbsent: [self parseError: 'Missing id attribute in child renderer']) asUppercase
				put: childRenderer]!

parseNode: aNode nodeName: aName commandsOn: cmdCol
	"Private - It parses the renderer node and initializes the internal data.

	@param aNode the xml node"

	| subjectNode |
	subjectNode := (aNode selectSingleNode: './' , aName) ifNil: [^self].
	subjectNode childNodes do: 
			[:node |
			(node nodeName sameAs: 'text')
				ifTrue: [self parseTextFrom: node text readStream on: cmdCol]
				ifFalse: 
					[(node nodeName sameAs: 'conditionText')
						ifTrue: [self parseConditionFrom: node on: cmdCol]
						ifFalse: [self error: 'Nodo de texto ' , node nodeName , ' desconocido']]]!

parseTextFrom: aStream on: aCmdCol
	"Private - Receives a stream that has to be parsed to obtain the template that's going to be used to render the objects.

	@param aStream a stream that has the necessary information for this object
	@param aCmdCol una colección donde guardar los comandos"

	| availableCom txtCommand text isCommand readChar |
	availableCom := self availableCommands.
	txtCommand := availableCom at: ''.
	isCommand := false.
	text := String writeStream.
	[aStream atEnd] whileFalse: 
			[readChar := aStream next.
			readChar = $%
				ifTrue: 
					[isCommand
						ifTrue: 
							[| command |
							command := text contents.
							self
								addCommand: (availableCom at: (self parseCommandName: command asUppercase)
										ifAbsent: [self parseError: 'The command ' , (self parseCommandName: command) , ' was not found.'])
								params: (self parseCommandParams: command)
								to: aCmdCol.
							text := String writeStream.
							isCommand := false]
						ifFalse: 
							[| nextChar |
							aStream atEnd
								ifTrue: [self parseError: 'Incomplete command string. You are missing a % character somewhere.'].
							nextChar := aStream next.
							nextChar = $%
								ifFalse: 
									[self
										addCommand: txtCommand
										params: (Array with: text contents)
										to: aCmdCol.
									text := String writeStream.
									isCommand := true].
							text nextPut: nextChar]]
				ifFalse: 
					[(String lineDelimiter includes: readChar) ifTrue: [text cr] ifFalse: [text nextPut: readChar]]].
	isCommand
		ifTrue: [self parseError: 'Incomplete command string. You are missing a % character somewhere.'].
	text isEmpty not
		ifTrue: 
			[self
				addCommand: txtCommand
				params: (Array with: text contents)
				to: aCmdCol]!

possibleChildTypes
	"Returns a Set of classes of renderers that can be childs of this type of renderer"

	^Set with: STDocTextRenderer!

printClassLink: aClass currentClass: curClass on: aStream
	"Private - Prints on the stream a class link if the class exists in the classes that are going to be rendered

	@param aClass the class that's going to be linked
	@param aStream the stream where the link is going to be rendered"

	((curClass isNil or: [aClass ~= curClass]) and: [self manager includesClass: aClass])
		ifTrue: 
			[aStream
				nextPutAll: '<a href="';
				nextPutAll: (self manager
							classFileName: (aClass isMeta ifTrue: [aClass instanceClass] ifFalse: [aClass]));
				nextPutAll: '">';
				nextPutAll: aClass name asString;
				nextPutAll: '</a>']
		ifFalse: [aStream nextPutAll: aClass name asString]!

printPackageLinkOf: aPackage on: aStream
	"Private - Prints on the stream a package link if the package exists in the packages that are going to be rendered

	@param aPackage the package that's going to be linked
	@param aStream the stream where the link is going to be rendered"

	aPackage isNil
		ifTrue: 
			[aStream nextPutAll: '<i>Sin paquete</i>'.
			^self].
	(self manager includesPackage: aPackage)
		ifTrue: 
			[aStream
				nextPutAll: '<a href="';
				nextPutAll: (self manager packageFileName: aPackage);
				nextPutAll: '">';
				nextPutAll: aPackage name asString;
				nextPutAll: '</a>']
		ifFalse: [aStream nextPutAll: aPackage name asString]!

render: anObject on: aStream
	"Renders the specified objet on the stream. Each renderer knows how to render a specific type of object.
	In  this case the renderers can only receive DOCClass objects
	
	@param anObject the object that's going to be rendered.
	@param aStream the stream where the object is going to be rendered"

	| renderableObjects |
	renderableObjects := self getObjectsOf: anObject.
	renderableObjects notEmpty
		ifTrue: 
			[self
				renderCommands: headerCommands
				on: aStream
				for: anObject].
	renderableObjects do: 
			[:obj |
			self
				renderCommands: bodyCommands
				on: aStream
				for: obj].
	renderableObjects notEmpty
		ifTrue: 
			[self
				renderCommands: footerCommands
				on: aStream
				for: anObject]!

renderChild: anObject params: params on: aStream
	"This is a default command method that writes the text on the stream.

	@param anObject the rendered object	
	@param params an array that in this case contains this id of the child renderer to be rendered
	@param aStream the stream where the text is going to be written"

	(childRenderers at: params first asUppercase
		ifAbsent: [self parseError: 'Renderer ' , params first displayString , ' does not exist.'])
			render: anObject
			on: aStream!

renderCommands: commands on: aStream for: obj
	^commands do: [:cmd | cmd execute: obj on: aStream]!

renderCommandsOn: aStream for: obj
	^self bodyCommands do: 
			[:arr |
			| args |
			args := Array
						with: obj
						with: arr second
						with: aStream.
			self perform: arr first withArguments: args]!

renderLabel: anObject params: params on: aStream
	"This command inserts a label text configured before the documentation process.

	@param anObject the rendered object	
	@param params an array that in this case contains one element with the text to be rendered
	@param aStream the stream where the text is going to be written"

	params size >= 1 ifTrue: [aStream nextPutAll: (self manager labelAt: params first)]!

renderTableFor: elements renderBlock: renderBlock tableClass: tableClass colClass: colClass columns: aNumber cellspacing: cellspacing on: aStream
	"Private - Renders all the elements using the render block inside a table of the specified number of columns.
	The render block must recieve two parameters, the first is the element to render and the second is the stream where it has to be rendered.
	Each element is rendered inside a cell of the table.

	@param elements the elements to render
	@param renderBlock the rendering block used to render each element
	@param tableClass a string that contains a css class to add style to bar table. It can be nil
	@param colClass a string that contains a css class to give style to bar cells. It can be nil
	@param aNumber the amount of columns of the table
	@param cellspacing the cellspacing property of the table
	@param aStream the stream where the links are going to be rendered"

	| rendered |
	rendered := 0.
	aStream nextPutAll: '<table width="100%"'.
	cellspacing notNil
		ifTrue: 
			[aStream
				nextPutAll: ' cellspacing="';
				nextPutAll: cellspacing;
				nextPutAll: '"'].
	tableClass notNil
		ifTrue: 
			[aStream
				nextPutAll: ' class="';
				nextPutAll: tableClass;
				nextPutAll: '"'].
	aStream nextPutAll: '>'.
	elements do: 
			[:element |
			rendered \\ aNumber = 0
				ifTrue: 
					[rendered = 0 ifFalse: [aStream nextPutAll: '</tr>'].
					aStream nextPutAll: '<tr>'].
			aStream nextPutAll: '<td'.
			colClass notNil
				ifTrue: 
					[aStream
						nextPutAll: ' class="';
						nextPutAll: colClass;
						nextPutAll: '"'].
			aStream nextPutAll: '>'.
			renderBlock value: aStream value: element.
			aStream
				nextPutAll: '</td>';
				cr.
			rendered := rendered + 1].
	rendered > aNumber
		ifTrue: 
			[| lastRendered |
			lastRendered := rendered \\ aNumber.
			lastRendered ~= 0 ifTrue: [aNumber - lastRendered timesRepeat: [aStream nextPutAll: '<td></td>']]].
	aStream
		nextPutAll: '</tr></table>';
		cr!

renderText: anObject params: params on: aStream
	"This is a default command method that writes the text on the stream.

	@param anObject the rendered object	
	@param params an array that in this case contains one element with the text to be rendered
	@param aStream the stream where the text is going to be written"

	aStream nextPutAll: params first! !
!STDocRenderer categoriesFor: #addCommand:params:!parsing!public! !
!STDocRenderer categoriesFor: #addCommand:params:to:!parsing!public! !
!STDocRenderer categoriesFor: #availableCommands!commands!public!rendering! !
!STDocRenderer categoriesFor: #bodyCommands!accessing!public! !
!STDocRenderer categoriesFor: #bodyCommands:!accessing!public! !
!STDocRenderer categoriesFor: #container!public! !
!STDocRenderer categoriesFor: #getObjectsOf:!private!rendering! !
!STDocRenderer categoriesFor: #initialize!private! !
!STDocRenderer categoriesFor: #isTrue:of:!public!rendering! !
!STDocRenderer categoriesFor: #manager!accessing!public! !
!STDocRenderer categoriesFor: #manager:!accessing!public! !
!STDocRenderer categoriesFor: #parent!accessing!public! !
!STDocRenderer categoriesFor: #parent:!accessing!public! !
!STDocRenderer categoriesFor: #parseCommandName:!parsing!private! !
!STDocRenderer categoriesFor: #parseCommandParams:!parsing!private! !
!STDocRenderer categoriesFor: #parseCondition:!parsing!private! !
!STDocRenderer categoriesFor: #parseConditionFrom:!parsing!private! !
!STDocRenderer categoriesFor: #parseConditionFrom:on:!parsing!private! !
!STDocRenderer categoriesFor: #parseNode:!parsing!public! !
!STDocRenderer categoriesFor: #parseNode:nodeName:commandsOn:!parsing!private! !
!STDocRenderer categoriesFor: #parseTextFrom:on:!parsing!private! !
!STDocRenderer categoriesFor: #possibleChildTypes!public! !
!STDocRenderer categoriesFor: #printClassLink:currentClass:on:!private! !
!STDocRenderer categoriesFor: #printPackageLinkOf:on:!private! !
!STDocRenderer categoriesFor: #render:on:!public!rendering! !
!STDocRenderer categoriesFor: #renderChild:params:on:!commands!public!rendering! !
!STDocRenderer categoriesFor: #renderCommands:on:for:!private!rendering! !
!STDocRenderer categoriesFor: #renderCommandsOn:for:!private!rendering! !
!STDocRenderer categoriesFor: #renderLabel:params:on:!commands!public!rendering! !
!STDocRenderer categoriesFor: #renderTableFor:renderBlock:tableClass:colClass:columns:cellspacing:on:!private!rendering! !
!STDocRenderer categoriesFor: #renderText:params:on:!commands!public!rendering! !

!STDocRenderer class methodsFor!

createRenderer: aNode manager: aManager parent: aParent
	"Instantiates a renderer class and initializes it's variables using an XMLNode parsed from the template file.

	@param aNode the XML node
	@param aManager the current manager
	@param aParent the object that contains this renderer"

	| rendererClass renderer rendererName |
	rendererName := aNode attributes at: 'type' ifAbsent: ['text'].
	rendererClass := self renderClassFor: rendererName
				ifAbsent: [self parseError: 'Invalid render type ' , rendererName].
	(aParent possibleChildTypes includes: rendererClass)
		ifFalse: 
			[self
				parseError: 'Renderers of type ' , aParent class rendererName , ' can''t contain renderers of type '
						, rendererClass rendererName].
	renderer := rendererClass forManager: aManager.
	renderer parseNode: aNode.
	renderer parent: aParent.
	^renderer!

forManager: aDOCSmalltalkManager
	"Instantiates a Renderer for the specified manager
	
	@param aDOCSmalltalkManager the manager for the renderer object"

	^(self basicNew)
		manager: aDOCSmalltalkManager;
		initialize;
		yourself!

renderClassFor: rendererName ifAbsent: aBlock
	^self allSubclasses
		detect: [:renderC | renderC rendererName notNil and: [renderC rendererName = rendererName]]
		ifNone: [aBlock value]!

rendererName
	"This is a custom name used to identify the renderer in the xml code, each renderer must have it's own name"

	^self subclassResponsibility! !
!STDocRenderer class categoriesFor: #createRenderer:manager:parent:!public! !
!STDocRenderer class categoriesFor: #forManager:!public! !
!STDocRenderer class categoriesFor: #renderClassFor:ifAbsent:!private! !
!STDocRenderer class categoriesFor: #rendererName!public! !

STDocSmalltalkManager guid: (GUID fromString: '{5ae38f9c-d577-4e86-90ad-2a90678ff390}')!
STDocSmalltalkManager comment: 'This manager handles the documentation process. It has a configuration object that specifies wich objects are going to be rendered, the template that''s going to be used, the output directory and the classes that will be documented.

It has a bunch of listeners that handles the rendering process. Those listeners are parsed from the template file.

To document you first have to set the configuration and then start the process by calling the method #makeDoc

Writer: Javier
1st Assistant: Leo Arias (A.K.A. "Capa Basica" Designer)
CoPilot: Svanlacke (A.K.A. Reprimed Hawk)'!
!STDocSmalltalkManager categoriesForClass!Unclassified! !
!STDocSmalltalkManager methodsFor!

addProcessor: aDOCProcessor
	"Private - Adds a processor to the processor collection and sets this manager as the processor's manager.

	@param aDOCProcessor the listener to be added"

	aDOCProcessor manager: self.
	self processors add: aDOCProcessor!

blankIcon
	^blankIcon!

blankIcon: anObject
	blankIcon := anObject!

classes
	^classes!

classes: anObject
	classes := anObject!

classesDo: aBlock
	"Private - Iterates all classes and executes the block.

	@param aBlock a monadic valuable that's going to be executed for each class"

	self classes do: aBlock!

classFileName: aClass
	"Returns the name of the html file that's going to be generated for the class

	@param aClass the class
	@return a string with the file name"

	^aClass name asString , '.html'!

classIcon
	^classIcon!

classIcon: anObject
	classIcon := anObject!

config
	^config!

config: anObject
	config := anObject!

configKeys
	^configKeys!

configKeys: anObject
	configKeys := anObject!

createOutFileNamed: aString
	"Creates a file for output in the output directory.

	@param aString the name of the file to be created"

	^FileStream write: (self config makeDocAbsolute: aString) text: true!

defaultStyle
	^defaultStyle!

defaultStyle: anObject
	defaultStyle := anObject!

imageTag: aFileName
	"Makes an IMG tag that has an absolute path to the template directory.

	@param aFileName the name of the image file"

	^'<img src="' , aFileName , '"/>'!

includesClass: aClass
	"Tells if the class is going to be documented by the system.

	@param aClass a smalltalk class
	@returns true if the class is going to be documented<br /> false if the class is not going to be documented"

	^self stClassSet includes: aClass!

includesPackage: aPackage
	"Este método indica si el paquete va a ser documentado por el sistema.

	@param aPackage a smalltalk package
	@returns true si el paquete solicitado va a ser documentado<br>false si el paquete no esta inicluido"

	self inferedPackages do: [:p | p = aPackage ifTrue: [^true]].
	^false!

inferedPackages
	| somePackages |
	somePackages := Set new.
	self classes do: [:c | somePackages add: c package].
	^somePackages asOrderedCollection!

initialize
	super initialize.
	self resetProcessors.
	config := STDocConfig new.
	classes := Set new.
	configKeys := LookupTable new.
	self initializeLabels!

initializeDoc
	"Private - Initializes the settings for documentation process"

	File createDirectoryPath: self config docPath!

initializeLabels
	| fecha ts |
	ts := TimeStamp current.
	fecha := String writeStream.
	fecha
		nextPutAll: (Locale default
					printDate: ts date
					format: 'dd-MM-yyyy'
					flags: 0);
		space;
		nextPutAll: (Locale default
					printTime: ts time
					format: 'HH:mm:ss'
					flags: 0).
	self
		labelAt: 'PROJECT_NAME' put: 'Project';
		labelAt: 'PROJECT_VER' put: '1.0';
		labelAt: 'YEAR' put: Date today year displayString;
		labelAt: 'TIMESTAMP' put: fecha contents;
		labelAt: 'COPYRIGHT' put: '©Organization 2007'!

labelAt: aKey
	"Returns a string with the label associated to the key

	@param aKey a string used as the label key
	@return the label string. If the label does not exist it returns an empty string"

	^self labelDictionary at: aKey asUppercase ifAbsent: ['']!

labelAt: aKey put: aString
	"Adds a label to the label dictionary.

	@param aKey a string used as the label identifier, it is not case sensitive
	@param aString the label text"

	self labelDictionary at: aKey asUppercase put: aString!

labelDictionary
	"Returns the label table

	@return a LookupTable used to store labels"

	^self configKeys at: 'labels' ifAbsentPut: [LookupTable new]!

makeDoc: aProgress
	"Generates the documentation through the processors. Informs the progress on the progress dialog passed as parameter.

	@param progress a dialog that is used to inform the progress"

	progress := aProgress.
	self progressText: 'Initializing process...'.
	self initializeDoc.
	self progressText: 'Parsing template...'.
	self parseTemplate.
	self progressText: 'Documenting...'.
	self processorsDo: [:l | l processDoc].
	progress := nil!

overIcon
	^overIcon!

overIcon: anObject
	overIcon := anObject!

packageFileName: aPackage
	"Returns the name of the html file that's going to be generated for the package

	@param aPackage the package
	@return a string with the file name"

	^'P_' , aPackage name asString , '.html'!

packages
	^packages ifNil: [OrderedCollection new]!

packages: somePackages
	"Establece el valor de los paquetes y selecciona las clases que se van a documentar.

	@param somePackages una colección de paquetes Smalltalk"

	| someClasses |
	someClasses := Set new.
	somePackages do: [:aPackage | someClasses addAll: aPackage classes].
	self setClasses: someClasses asOrderedCollection.
	packages := somePackages!

parseTemplate
	"Private - Parses the template file and creates the listeners"

	| template |
	self resetProcessors.
	template := STDocTemplateSpec forManager: self.
	template fileName: (self config makeTemplateAbsolute: 'template.xml').
	template loadFile.
	self addProcessor: template!

privIcon
	^privIcon!

privIcon: anObject
	privIcon := anObject!

processors
	^processors!

processors: anObject
	processors := anObject!

processorsDo: aBlock
	"Private - Iterates all DOCProcessors and executes the block.

	@param aBlock a monadic valuable that's going to be executed for each processor"

	self processors do: aBlock!

progressText: aString
	"Sets a text in the progress dialog of the manager
	
	@param aString the string that's going to be setted"

	progress isNil ifFalse: [progress text: aString]!

progressValue: aValue
	"Sets a value in the progress bar. The value is in percentage between 1 and 100
	
	@param aValue a number between 1 and 100"

	progress isNil ifFalse: [progress value: aValue]!

pubIcon
	^pubIcon!

pubIcon: anObject
	pubIcon := anObject!

reset
	self initializeLabels.
	packages := stClassSet := nil.
	self resetProcessors!

resetProcessors
	"Removes all the processors from the manager"

	processors := IdentitySet new!

searchClass: aName
	^self classes detect: [:cls | cls classObject name = aName] ifNone: []!

setClasses: stClasses
	"Receives a collection of classes and adds the extra classes that should be added according to the configuration for the documentation.
	The things it can be added are the superclasses and related classes to the package.
	This method replaces all the classes that are in the manager. 

	@param stClasses selected classes"

	| cls |
	cls := Set new.
	self classes: Set new.
	cls addAll: (stClasses collect: [:c | STDocClass forClass: c manager: self]).
	self classes addAll: cls.
	self config renderRelatedClasses ifTrue: [cls do: [:c | self classes addAll: c relatedClasses]].
	self config renderSuperclasses ifTrue: [cls do: [:c | self classes addAll: c superClasses]].
	stClassSet := nil!

stClassSet
	stClassSet notNil ifTrue: [^stClassSet].
	stClassSet := IdentitySet new.
	self classesDo: [:c | stClassSet add: c classObject].
	^stClassSet! !
!STDocSmalltalkManager categoriesFor: #addProcessor:!private! !
!STDocSmalltalkManager categoriesFor: #blankIcon!accessing!public! !
!STDocSmalltalkManager categoriesFor: #blankIcon:!accessing!public! !
!STDocSmalltalkManager categoriesFor: #classes!accessing!public! !
!STDocSmalltalkManager categoriesFor: #classes:!accessing!public! !
!STDocSmalltalkManager categoriesFor: #classesDo:!private! !
!STDocSmalltalkManager categoriesFor: #classFileName:!public! !
!STDocSmalltalkManager categoriesFor: #classIcon!accessing!public! !
!STDocSmalltalkManager categoriesFor: #classIcon:!accessing!public! !
!STDocSmalltalkManager categoriesFor: #config!accessing!public! !
!STDocSmalltalkManager categoriesFor: #config:!accessing!public! !
!STDocSmalltalkManager categoriesFor: #configKeys!accessing!public! !
!STDocSmalltalkManager categoriesFor: #configKeys:!accessing!public! !
!STDocSmalltalkManager categoriesFor: #createOutFileNamed:!public! !
!STDocSmalltalkManager categoriesFor: #defaultStyle!accessing!public! !
!STDocSmalltalkManager categoriesFor: #defaultStyle:!accessing!public! !
!STDocSmalltalkManager categoriesFor: #imageTag:!public! !
!STDocSmalltalkManager categoriesFor: #includesClass:!public! !
!STDocSmalltalkManager categoriesFor: #includesPackage:!public! !
!STDocSmalltalkManager categoriesFor: #inferedPackages!public! !
!STDocSmalltalkManager categoriesFor: #initialize!private! !
!STDocSmalltalkManager categoriesFor: #initializeDoc!private! !
!STDocSmalltalkManager categoriesFor: #initializeLabels!private! !
!STDocSmalltalkManager categoriesFor: #labelAt:!public! !
!STDocSmalltalkManager categoriesFor: #labelAt:put:!public! !
!STDocSmalltalkManager categoriesFor: #labelDictionary!public! !
!STDocSmalltalkManager categoriesFor: #makeDoc:!public! !
!STDocSmalltalkManager categoriesFor: #overIcon!accessing!public! !
!STDocSmalltalkManager categoriesFor: #overIcon:!accessing!public! !
!STDocSmalltalkManager categoriesFor: #packageFileName:!public! !
!STDocSmalltalkManager categoriesFor: #packages!public! !
!STDocSmalltalkManager categoriesFor: #packages:!public! !
!STDocSmalltalkManager categoriesFor: #parseTemplate!private! !
!STDocSmalltalkManager categoriesFor: #privIcon!accessing!public! !
!STDocSmalltalkManager categoriesFor: #privIcon:!accessing!public! !
!STDocSmalltalkManager categoriesFor: #processors!accessing!public! !
!STDocSmalltalkManager categoriesFor: #processors:!accessing!public! !
!STDocSmalltalkManager categoriesFor: #processorsDo:!private! !
!STDocSmalltalkManager categoriesFor: #progressText:!public! !
!STDocSmalltalkManager categoriesFor: #progressValue:!public! !
!STDocSmalltalkManager categoriesFor: #pubIcon!accessing!public! !
!STDocSmalltalkManager categoriesFor: #pubIcon:!accessing!public! !
!STDocSmalltalkManager categoriesFor: #reset!public! !
!STDocSmalltalkManager categoriesFor: #resetProcessors!public! !
!STDocSmalltalkManager categoriesFor: #searchClass:!public! !
!STDocSmalltalkManager categoriesFor: #setClasses:!public! !
!STDocSmalltalkManager categoriesFor: #stClassSet!private! !

STDocUtil guid: (GUID fromString: '{8235fd37-c1ce-4a8f-9d60-4e92ee9905f8}')!
STDocUtil comment: 'This class is used as a bag of functions to process strings and streams.
Al kinds of common functions are included here, most of them are for parsing texts'!
!STDocUtil categoriesForClass!Unclassified! !
!STDocUtil class methodsFor!

charToHTML: char
	^char == Character lf
		ifTrue: ['<br />' , String lineDelimiter]
		ifFalse: 
			[char == Character cr
				ifTrue: ['']
				ifFalse: 
					[Character tab == char
						ifTrue: 
							["'&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'"
							Character space asString]
						ifFalse: 
							[$< == char
								ifTrue: ['&lt;']
								ifFalse: [$> == char ifTrue: ['&gt;'] ifFalse: [$& == char ifTrue: ['&amp;'] ifFalse: [nil]]]]]]!

consume: aStream do: aBlock
	"Consumes the stream and executes the block for each object consumed

	@param aStream the stream to be consumed
	@param aBlock the monadic valuable to be executed"

	[aStream atEnd] whileFalse: [aBlock value: aStream next]!

firstWordOf: aStream
	"Fetches the first word of the stream, it ignores all the separator characters at the beggining.
	This method consumes the stream. When this method finished the stream is positioned after the first separater char that comes after the first word.

	@param aStream the stream that's going to be used
	@returns a string with the first word"

	| word c |
	word := String writeStream.
	aStream skipSeparators.
	[aStream atEnd not and: [(c := aStream next) isSeparator not]] whileTrue: [word nextPut: c].
	^word contents!

ignore: aStream where: aBlock
	"Ignores the objects of a stream while the block returns true

	@param aStream the stream to be used
	@param aBlock a monadic valuable, it has to return true when the object has to be skipped"

	[aStream atEnd] whileFalse: [(aBlock value: aStream peek) ifTrue: [aStream next] ifFalse: [^self]]!

ignoreFrom: aStream upTo: aBlock
	"Ignores the objects of a stream until the block returns true. The object that makes the processor stop ignoring is ignored too.

	@param aStream the stream to be used
	@param aBlock a monadic valuable, it has to return true when the processor has to stop ignoring objects"

	self consume: aStream do: [:c | (aBlock value: c) ifTrue: [^self]]!

isValidTag: tag
	| realTag |
	tag isEmpty ifTrue: [^false].
	tag first = $/ ifTrue: [realTag := tag copyFrom: 2] ifFalse: [realTag := tag].
	^##(#('b' 'pre' 'code' 'i' 'p' 'br' 'h1' 'h2' 'h3' 'h4' 'li')) includes: realTag asLowercase!

makeHTMLString: aString on: outStream
	"Replaces special HTML characters of the string and makes a valid text that does not conflicts with HTML tags. For example replaces character '<' with '&gt;'.
	The text is wrote to outStream

	@param aString with any character
	@param outStream a stream where the result text will be wrote"

	| reader c |
	reader := aString readStream.
	[reader atEnd] whileFalse: 
			[c := reader next.
			c = $<
				ifTrue: [self makeTag: reader on: outStream]
				ifFalse: 
					[(self charToHTML: c) ifNil: [outStream nextPut: c] ifNotNil: [:rpl | outStream nextPutAll: rpl]]]!

makeIDString: aString
	"Replaces all symbols from the string that are not alphanumeric with the character '_'

	@param aString the string to process
	@return a processed string only with alphanumeric characters and slashes"

	| out |
	out := String writeStream.
	self makeIDString: aString on: out.
	^out contents!

makeIDString: aString on: out
	"Replaces all symbols from the string that are not alphanumeric with the character '_'

	@param aString the string to process
	@param out the stream where the id is going to be rendered
	@return a processed string only with alphanumeric characters and slashes"

	| in replacements |
	in := aString trimBlanks readStream.
	[in atEnd] whileFalse: 
			[| c |
			c := in next.
			c isAlphaNumeric ifTrue: [out nextPut: c] ifFalse: [out nextPut: $_]]!

makeTag: reader on: outStream
	| c tag aparecioBarra |
	tag := String writeStream.
	aparecioBarra := false.
	[reader atEnd] whileFalse: 
			[c := reader next.
			(c isAlphaNumeric or: [aparecioBarra not and: [c = $/]])
				ifTrue: 
					[c = $/ ifTrue: [aparecioBarra := true].
					tag nextPut: c]
				ifFalse: 
					[(c = $> and: [self isValidTag: tag contents])
						ifTrue: 
							[outStream nextPutAll: '<' , tag contents , '>'.
							reader skipSeparators.
							^self].
					outStream
						nextPutAll: (self charToHTML: $<) , tag contents , ((self charToHTML: c) ifNil: [c asString]).
					^self]].
	outStream
		nextPutAll: (self charToHTML: $<);
		nextPutAll: tag contents!

stringFrom: aStream filterBlock: aBlock
	"Returns the string formed from the current position applying filters to each char. The filter to apply is determined using the returned value of the block passed by parameter.
	The block is evaluated for every char in the stream, and it has to return 1 if the char has to be added, 0 if it has to end the string and 2 if the evaluated char has to be skipped.

	@param aStream the stream to be used
	@param aBlock a monadic valuable that returns 1 if it has to add the char, 0 if it has to end the string and 2 if it has to skip the character
	@return the string between the current position and the end position"

	| result |
	result := String writeStream.
	self consume: aStream
		do: 
			[:c |
			| v |
			v := aBlock value: c.
			v = 0 ifTrue: [^result contents] ifFalse: [v = 1 ifTrue: [result nextPut: c]]].
	^result isEmpty ifTrue: [nil] ifFalse: [result contents]!

stringFrom: aStream from: startBlock to: endBlock
	"Returns the string that is from the position where startBlock returns true up to the position where the endBlock returns true. The startBlock object and end Block object are not included in the string but they are consumed from the string.

	@param aStream the stream to be used
	@param startBlock a monadic valuable that returns true when the string has to start
	@param endBlock a monadic valuable that returns true when the string has to end
	@return the string between the starting position, without including it, and the end position, without including it"

	self ignoreFrom: aStream upTo: startBlock.
	^self stringFrom: aStream upTo: endBlock!

stringFrom: aStream upTo: aBlock
	"Returns the string that is from the current position to the position where the block returns true

	@param aStream the stream to be used
	@param aBlock a monadic valuable that returns true when the string is finished
	@return the string between the current position and the end position"

	| result |
	result := String writeStream.
	self consume: aStream
		do: [:c | (aBlock value: c) ifTrue: [^result contents] ifFalse: [result nextPut: c]].
	^result isEmpty ifTrue: [nil] ifFalse: [result contents]! !
!STDocUtil class categoriesFor: #charToHTML:!private! !
!STDocUtil class categoriesFor: #consume:do:!public! !
!STDocUtil class categoriesFor: #firstWordOf:!public! !
!STDocUtil class categoriesFor: #ignore:where:!public! !
!STDocUtil class categoriesFor: #ignoreFrom:upTo:!public! !
!STDocUtil class categoriesFor: #isValidTag:!private! !
!STDocUtil class categoriesFor: #makeHTMLString:on:!public! !
!STDocUtil class categoriesFor: #makeIDString:!public! !
!STDocUtil class categoriesFor: #makeIDString:on:!public! !
!STDocUtil class categoriesFor: #makeTag:on:!private! !
!STDocUtil class categoriesFor: #stringFrom:filterBlock:!public! !
!STDocUtil class categoriesFor: #stringFrom:from:to:!public! !
!STDocUtil class categoriesFor: #stringFrom:upTo:!public! !

STDocVariable guid: (GUID fromString: '{b766ee9c-5dfb-479a-ba71-d12299382387}')!
STDocVariable comment: ''!
!STDocVariable categoriesForClass!Unclassified! !
!STDocVariable methodsFor!

comment
	| str |
	(self docClass docComment attributeTable at: 'var' ifAbsent: [^'']) do: 
			[:string |
			str := string readStream.
			(STDocUtil firstWordOf: str) = self name ifTrue: [^str upToEnd]].
	^''!

docClass
	^docClass!

docClass: anObject
	docClass := anObject!

firstWordOf: aStream
	"Private - Fetches the first word of the stream, it ignores all the separator characters at the beggining.
	This method consumes the stream. When this method finished the stream is positioned after the first separater char that comes after the first word.


	@param aStream the stream that's going to be used
	@returns a string with the first word"

	| word c |
	word := String writeStream.
	aStream skipSeparators.
	[aStream atEnd not and: [(c := aStream next) isSeparator not]] whileTrue: [word nextPut: c].
	^word contents!

isClass
	^self type = #class!

isClassInst
	^self type = #classInst!

isInst
	^self type = #inst!

name
	^name!

name: anObject
	name := anObject!

type
	^type!

type: anObject
	type := anObject! !
!STDocVariable categoriesFor: #comment!public! !
!STDocVariable categoriesFor: #docClass!accessing!public! !
!STDocVariable categoriesFor: #docClass:!accessing!public! !
!STDocVariable categoriesFor: #firstWordOf:!private! !
!STDocVariable categoriesFor: #isClass!public! !
!STDocVariable categoriesFor: #isClassInst!public! !
!STDocVariable categoriesFor: #isInst!public! !
!STDocVariable categoriesFor: #name!accessing!public! !
!STDocVariable categoriesFor: #name:!accessing!public! !
!STDocVariable categoriesFor: #type!accessing!public! !
!STDocVariable categoriesFor: #type:!accessing!public! !

!STDocVariable class methodsFor!

forClassInstVar: aClass name: aString
	^self
		forType: #classInst
		class: aClass
		name: aString!

forClassVar: aClass name: aString
	^self
		forType: #class
		class: aClass
		name: aString!

forInstVar: aClass name: aString
	^self
		forType: #inst
		class: aClass
		name: aString!

forType: aType class: aClass name: aString
	^(self new)
		type: aType;
		docClass: aClass;
		name: aString;
		yourself! !
!STDocVariable class categoriesFor: #forClassInstVar:name:!public! !
!STDocVariable class categoriesFor: #forClassVar:name:!public! !
!STDocVariable class categoriesFor: #forInstVar:name:!public! !
!STDocVariable class categoriesFor: #forType:class:name:!public! !

STDocAnd guid: (GUID fromString: '{c1999a98-807b-4465-8668-e6fb3b1699ef}')!
STDocAnd comment: ''!
!STDocAnd categoriesForClass!Unclassified! !
!STDocAnd methodsFor!

conditions
	^conditions!

conditions: anObject
	conditions := anObject!

value: anObject
	"@override"

	self conditions isEmpty ifTrue: [^true].
	^self conditions allSatisfy: [:cond | cond value: anObject]! !
!STDocAnd categoriesFor: #conditions!accessing!public! !
!STDocAnd categoriesFor: #conditions:!accessing!public! !
!STDocAnd categoriesFor: #value:!public! !

STDocOr guid: (GUID fromString: '{10e41e62-8400-4301-a827-fc14d205bede}')!
STDocOr comment: ''!
!STDocOr categoriesForClass!Unclassified! !
!STDocOr methodsFor!

conditions
	^conditions!

conditions: anObject
	conditions := anObject!

value: anObject
	"@override"

	self conditions isEmpty ifTrue: [^true].
	^self conditions anySatisfy: [:cond | cond value: anObject]! !
!STDocOr categoriesFor: #conditions!accessing!public! !
!STDocOr categoriesFor: #conditions:!accessing!public! !
!STDocOr categoriesFor: #value:!public! !

STDocPredefinedCond guid: (GUID fromString: '{bc28fd66-6780-4568-b1b4-aad654d01c0a}')!
STDocPredefinedCond comment: ''!
!STDocPredefinedCond categoriesForClass!Unclassified! !
!STDocPredefinedCond methodsFor!

condition
	^condition!

condition: anObject
	condition := anObject!

renderer
	^renderer!

renderer: anObject
	renderer := anObject!

value: anObject
	"@override"

	^self renderer isTrue: self condition of: anObject! !
!STDocPredefinedCond categoriesFor: #condition!accessing!public! !
!STDocPredefinedCond categoriesFor: #condition:!accessing!public! !
!STDocPredefinedCond categoriesFor: #renderer!accessing!public! !
!STDocPredefinedCond categoriesFor: #renderer:!accessing!public! !
!STDocPredefinedCond categoriesFor: #value:!public! !

!STDocPredefinedCond class methodsFor!

str: conditionString on: aRenderer
	^(self new)
		renderer: aRenderer;
		condition: conditionString;
		yourself! !
!STDocPredefinedCond class categoriesFor: #str:on:!public! !

STDocClassTemplate guid: (GUID fromString: '{6b507edb-98cb-4802-8589-29f466acab0e}')!
STDocClassTemplate comment: 'Makes an HTML page using a template file that is parsed beforehand.
The methods to render the classes are called by the DOCSmalltalkManager.

The main rendering method is #processDoc. This method tells each one of the renderers to render itself for each class.

This Documenter makes one file for each one of the classes that are going to be documented. The file name is the class name plus the html extension, if the class is Object the output file will be ''Object.html''.'!
!STDocClassTemplate categoriesForClass!Unclassified! !
!STDocClassTemplate methodsFor!

fileNameFor: anObject
	"Generates the file name that's going to be wrote for this object.

	@param anObject the object
	@returns a string with the file name"

	^self manager classFileName: anObject!

initialize
	super initialize.
	renderers := OrderedCollection new!

loadConfig: aXMLNode
	"Loads extra information that may be in the main template file.
	
	@param anXMLNode is the node that is used to instantiate the object"

	super loadConfig: aXMLNode.
	outDir := aXMLNode attributes at: 'outDir' ifAbsent: [nil]!

outDir
	^outDir!

outDir: anObject
	outDir := anObject!

parseDocument: anXMLNode
	"Initializes the state of the object with information contained in an XML file.
	Parses the renderers and adds them to the list of the template.
	
	@param anXMLNode the root node of an XML file"

	anXMLNode childNodes do: 
			[:xmlNode |
			xmlNode nodeName = 'render'
				ifTrue: 
					[renderers addLast: (STDocRenderer
								createRenderer: xmlNode
								manager: self manager
								parent: self)]
				ifFalse: [self parseError: 'Tag name "' , xmlNode nodeName , '" in template file not recognized']]!

possibleChildTypes
	"Returns a Set of classes of renderers that can be childs of this type of renderer"

	^(super possibleChildTypes)
		add: STDocClassRenderer;
		add: STDocMethodRenderer;
		add: STDocVariableRenderer;
		add: STDocTextRenderer;
		yourself!

processDoc
	"Executes the main purpose of the documenter, in general generates the output documentation files.
	Writes a file for each one of the classes selected for documentarion using the parsed template and the renderers for classes. It contains a collection of renderers that where parsed from the xml file. Those renderers are the ones that handle the output."

	| someClasses len |
	someClasses := self manager classes asOrderedCollection.
	len := someClasses size.
	self manager progressText: 'Making documentation for classes...'.
	self manager progressValue: 0.
	1 to: len
		do: 
			[:index |
			| aClass |
			aClass := someClasses at: index.
			self renderize: aClass.
			self manager progressValue: index / len * 100]!

renderers
	^renderers!

renderers: anObject
	renderers := anObject!

renderize: anObject
	"Private - Iterates the renderers and generates the output for the object.

	@param anObject any object that can be documented. It may be a class or a package"

	| str |
	str := self manager
				createOutFileNamed: (self outDir ifNil: [''] ifNotNil: [:d | File appendPathDelimiter: d])
						, (self fileNameFor: anObject).
	self renderers do: [:renderer | renderer render: anObject on: str].
	^str close! !
!STDocClassTemplate categoriesFor: #fileNameFor:!document!private! !
!STDocClassTemplate categoriesFor: #initialize!private! !
!STDocClassTemplate categoriesFor: #loadConfig:!parsing!public! !
!STDocClassTemplate categoriesFor: #outDir!accessing!private! !
!STDocClassTemplate categoriesFor: #outDir:!accessing!private! !
!STDocClassTemplate categoriesFor: #parseDocument:!parsing!public! !
!STDocClassTemplate categoriesFor: #possibleChildTypes!public! !
!STDocClassTemplate categoriesFor: #processDoc!document!public! !
!STDocClassTemplate categoriesFor: #renderers!accessing!private! !
!STDocClassTemplate categoriesFor: #renderers:!accessing!private! !
!STDocClassTemplate categoriesFor: #renderize:!document!private! !

!STDocClassTemplate class methodsFor!

processorName
	"The name of the listener used in the XML files"

	^'class'! !
!STDocClassTemplate class categoriesFor: #processorName!private! !

STDocDefaultProcessor guid: (GUID fromString: '{7a50e236-f9b3-4fd3-8e6d-cfe47bfa2387}')!
STDocDefaultProcessor comment: ''!
!STDocDefaultProcessor categoriesForClass!Unclassified! !
!STDocDefaultProcessor methodsFor!

initialize
	super initialize.
	renderers := OrderedCollection new!

loadConfig: aXMLNode
	"Loads extra information that may be in the main template file.
	
	@param anXMLNode is the node that is used to instantiate the object"

	super loadConfig: aXMLNode.
	outFile := aXMLNode attributes at: 'outFile'
				ifAbsent: [self parseError: 'The template type "default" must have and "outFile" attribute']!

parseDocument: anXMLNode
	"Initializes the state of the object with information contained in an XML file.
	Parses the renderers and adds them to the list of the template.
	
	@param anXMLNode the root node of an XML file"

	anXMLNode childNodes do: 
			[:xmlNode |
			xmlNode nodeName = 'render'
				ifTrue: 
					[renderers addLast: (STDocRenderer
								createRenderer: xmlNode
								manager: self manager
								parent: self)]
				ifFalse: [self parseError: 'Tag name "' , xmlNode nodeName , '" in template file not recognized']]!

possibleChildTypes
	"Returns a Set of classes of renderers that can be childs of this type of renderer"

	^(super possibleChildTypes)
		add: STDocTextRenderer;
		yourself!

processDoc
	"Executes the main purpose of the documenter, in general generates the output documentation files.
	Writes a file for each one of the classes selected for documentarion using the parsed template and the renderers for classes. It contains a collection of renderers that where parsed from the xml file. Those renderers are the ones that handle the output."

	| str |
	str := self manager createOutFileNamed: outFile.
	self renderers do: [:renderer | renderer render: nil on: str].
	^str close!

renderers
	^renderers!

renderers: anObject
	renderers := anObject! !
!STDocDefaultProcessor categoriesFor: #initialize!private! !
!STDocDefaultProcessor categoriesFor: #loadConfig:!parsing!public! !
!STDocDefaultProcessor categoriesFor: #parseDocument:!parsing!public! !
!STDocDefaultProcessor categoriesFor: #possibleChildTypes!public! !
!STDocDefaultProcessor categoriesFor: #processDoc!document!public! !
!STDocDefaultProcessor categoriesFor: #renderers!accessing!private! !
!STDocDefaultProcessor categoriesFor: #renderers:!accessing!private! !

!STDocDefaultProcessor class methodsFor!

processorName
	"The name of the listener used in the XML files"

	^'default'! !
!STDocDefaultProcessor class categoriesFor: #processorName!private! !

STDocIndexTemplate guid: (GUID fromString: '{7b81fd24-9ed2-4aa7-a278-317bb6e06f93}')!
STDocIndexTemplate comment: 'This Documenter makes files to have quick access to the main documentation files, such as documentation for classes.

The available index types are:
	- Hierarchy Index: The link for the classes are displayed as a hierarchy of all the classes involved.
	- Alphabetical Index: The links to the classes are displayed as a list of alphabetically ordered names.
	- Package Index: A list of documented packages and links to the pages are included.

It generates a file per index type, the name can be specified with the attribute outFile of the index tag in the index template file. If the attribute is not specified it uses a default name.

The Index layout is parsed from an xml file. The index template also tells wich of the indexes are going to be used'!
!STDocIndexTemplate categoriesForClass!Unclassified! !
!STDocIndexTemplate methodsFor!

activeIndexesLinks
	"Returns a collection of arrays of two elements of the indexes that are active and it's page.
	Each array has as first element the name of the index and as second element the page of the index"

	| result |
	result := OrderedCollection new.
	self renderers do: 
			[:r |
			| info |
			info := self indexTypeInfo at: r type.
			result add: (Array with: info first with: (r fileName ifNil: [info second]))].
	^result!

addClass: aClass to: aCollection
	"Private - Adds a class in alphabetical order and and returns the added node as parameter

	@param aClass the class object to be added
	@param aCollection the collection where is going to be added
	@return the added node, or the previous node that was in the collection"

	| nodes |
	nodes := aCollection select: [:n | n first = aClass].
	^nodes isEmpty
		ifTrue: [aCollection add: (Array with: aClass with: self sortedCollection)]
		ifFalse: [nodes first]!

addFullClass: aClass
	"Processes each class and generates output. This method is called by DOCSmalltalkManager to render each of the classes that are going to be documented.

	@param aClass the class to be processed"

	| col |
	col := hierarchy.
	((aClass classObject allSuperclasses)
		addFirst: aClass classObject;
		reverse) do: [:c | col := (self addClass: c to: col) second]!

allClasses
	| classes |
	classes := SortedCollection sortBlock: [:c1 :c2 | c1 name <= c2 name].
	self classesDo: [:c | classes add: c].
	^classes!

classesDo: aBlock
	"Iterates all the classes in the index"

	self iterateClasses: hierarchy do: [:c | aBlock value: c]!

indexTypeInfo
	^(LookupTable new)
		at: #hierarchy
			put: (Array
					with: 'Hierarchy'
					with: '_indexHierarchy.html'
					with: #renderHierarchyIndexOn:);
		at: #alphabetical
			put: (Array
					with: 'Alphabetical'
					with: '_indexAlpha.html'
					with: #renderAlphabeticalIndexOn:);
		at: #package
			put: (Array
					with: 'Packages'
					with: '_indexPackage.html'
					with: #renderPackageIndexOn:);
		yourself!

initialize
	super initialize.
	hierarchy := self sortedCollection.
	renderers := OrderedCollection new!

iterateClasses: aCollection do: aBlock
	"Iterates all the classes from the nodes and it's children

	@param aCollection a collection of nodes
	@param aBlock a monadic valuable that receives a class"

	aCollection do: 
			[:node |
			aBlock value: node first.
			self iterateClasses: node second do: aBlock]!

parseDocument: anXMLNode
	"Initializes the state of the object with information contained in an XML file.
	
	@param anXMLNode the root node of an XML file"

	(anXMLNode selectNodes: './/index') do: 
			[:node |
			self parseRenderer: node
				type: (node attributes at: 'type'
						ifAbsent: [^self parseError: 'Index tag is missing the attribute "type"']) asSymbol]!

parseRenderer: aNode type: indexType
	renderers add: ((STDocIndexRenderer forManager: self manager)
				parent: self;
				type: indexType;
				parseNode: aNode;
				yourself)!

possibleChildTypes
	"Returns a Set of classes of renderers that can be childs of this type of renderer"

	^(super possibleChildTypes)
		add: STDocIndexRenderer;
		add: STDocTextRenderer;
		yourself!

printClassLinkOf: aClass on: aStream
	"Private - Prints on the stream a class link if the class exists in the classes that are going to be rendered

	@param aClass the class that's going to be linked
	@param aStream the stream where the link is going to be rendered"

	(self manager includesClass: aClass)
		ifTrue: 
			[aStream
				nextPutAll: '<a href="javascript:browseClass(''';
				nextPutAll: (self manager classFileName: aClass);
				nextPutAll: ''');">';
				nextPutAll: aClass name asString;
				nextPutAll: '</a>']
		ifFalse: [aStream nextPutAll: aClass name asString]!

printPackageLinkOf: aPackage on: aStream
	"Private - Prints on the stream a package link if the package exists in the packages that are going to be rendered

	@param aPackage the package that's going to be linked
	@param aStream the stream where the link is going to be rendered"

	aPackage isNil
		ifTrue: 
			[aStream nextPutAll: '<i>Sin paquete</i>'.
			^self].
	(self manager includesPackage: aPackage)
		ifTrue: 
			[aStream
				nextPutAll: '<a href="javascript:browseClass(''';
				nextPutAll: (self manager packageFileName: aPackage);
				nextPutAll: ''');">';
				nextPutAll: aPackage name asString;
				nextPutAll: '</a>']
		ifFalse: [aStream nextPutAll: aPackage name asString]!

processDoc
	"Executes the main purpose of the documenter, in general generates the output documentation files.
	It organizes the tree of classes and uses the renderers to generate the index files."

	| someClasses len |
	someClasses := self manager classes asOrderedCollection.
	len := someClasses size.
	self manager progressText: 'Making indexes...'.
	self manager progressValue: 0.
	1 to: len
		do: 
			[:index |
			| aClass |
			aClass := someClasses at: index.
			self addFullClass: aClass.
			self manager progressValue: index / len * 50].
	len := renderers size.
	1 to: len
		do: 
			[:index |
			| renderer str |
			renderer := renderers at: index.
			str := FileStream write: (self manager config
								makeDocAbsolute: (renderer fileName ifNil: [(self indexTypeInfo at: renderer type) second]))
						text: true.
			renderer render: self on: str.
			str close.
			self manager progressValue: 50 + (index / len * 50)]!

renderAlphabeticalIndexOn: aStream
	"Private - Creates an index rendered as a class collection ordered by class name. The output file is hierarchyAlpha.html"

	hierarchy isEmpty ifTrue: [^self].
	(self allClasses select: [:c | self manager includesClass: c]) do: 
			[:c |
			self printClassLinkOf: c on: aStream.
			aStream
				nextPutAll: '<br />';
				cr]!

renderers
	^renderers!

renderers: anObject
	renderers := anObject!

renderHierarchyIndexOn: aStream
	"Private - Creates an index rendered as a hierarchy tree. The output file is hierarchyIndex.html"

	hierarchy isEmpty ifTrue: [^self].
	self
		renderNode: hierarchy
		sepparator: '&nbsp;&nbsp;&nbsp;'
		currentSep: ''
		on: aStream!

renderIndex: indexType on: aStream
	"Creates an index of the type specified on the stream specified.

	@param indexType a Symbol object that identifies a type of index
	@param aStream the stream where the index is going to be rendered"

	self perform: (self indexTypeInfo at: indexType) last with: aStream!

renderIndexName: indexType on: aStream
	"Renders the native name of the index.

	@param indexType a Symbol object that identifies a type of index
	@param aStream the stream where the index name is going to be rendered"

	aStream
		nextPutAll: (self indexTypeInfo at: indexType
				ifAbsent: [self parseError: 'Index type "' , indexType displayString , '" does not exist']) first!

renderNode: aCollection sepparator: sepString currentSep: curSepString on: aStream
	"Private - Renders a link for each class in the collection. Each link has the curSepString.
	When this method is called for the childs the sepString is added for the curSepString.

	@param aCollection the classes that are going to be rendered
	@param sepString the string that is added to the curSepString
	@param curSepString the string rendered before the class link
	@param aStream the stream where the index is going to be rendered"

	aCollection do: 
			[:arr |
			aStream nextPutAll: curSepString.
			self printClassLinkOf: arr first on: aStream.
			aStream
				nextPutAll: '<br />';
				cr.
			self
				renderNode: arr second
				sepparator: sepString
				currentSep: curSepString , sepString
				on: aStream]!

renderPackageIndexOn: aStream
	"Private - Creates an index for the packages."

	hierarchy isEmpty ifTrue: [^self].
	(self manager inferedPackages asSortedCollection: [:p1 :p2 | p1 name <= p2 name]) do: 
			[:package |
			self printPackageLinkOf: package on: aStream.
			aStream
				nextPutAll: '<br />';
				cr]!

sortedCollection
	^SortedCollection sortBlock: [:cArr1 :cArr2 | cArr1 first name <= cArr2 first name]! !
!STDocIndexTemplate categoriesFor: #activeIndexesLinks!public!rendering! !
!STDocIndexTemplate categoriesFor: #addClass:to:!hierarchy!private! !
!STDocIndexTemplate categoriesFor: #addFullClass:!document!public! !
!STDocIndexTemplate categoriesFor: #allClasses!hierarchy!private! !
!STDocIndexTemplate categoriesFor: #classesDo:!hierarchy!private! !
!STDocIndexTemplate categoriesFor: #indexTypeInfo!private! !
!STDocIndexTemplate categoriesFor: #initialize!private! !
!STDocIndexTemplate categoriesFor: #iterateClasses:do:!hierarchy!private! !
!STDocIndexTemplate categoriesFor: #parseDocument:!parsing!public! !
!STDocIndexTemplate categoriesFor: #parseRenderer:type:!parsing!private! !
!STDocIndexTemplate categoriesFor: #possibleChildTypes!public! !
!STDocIndexTemplate categoriesFor: #printClassLinkOf:on:!private!rendering! !
!STDocIndexTemplate categoriesFor: #printPackageLinkOf:on:!private! !
!STDocIndexTemplate categoriesFor: #processDoc!document!public!rendering! !
!STDocIndexTemplate categoriesFor: #renderAlphabeticalIndexOn:!private!rendering! !
!STDocIndexTemplate categoriesFor: #renderers!accessing!private! !
!STDocIndexTemplate categoriesFor: #renderers:!accessing!private! !
!STDocIndexTemplate categoriesFor: #renderHierarchyIndexOn:!private!rendering! !
!STDocIndexTemplate categoriesFor: #renderIndex:on:!public!rendering! !
!STDocIndexTemplate categoriesFor: #renderIndexName:on:!public! !
!STDocIndexTemplate categoriesFor: #renderNode:sepparator:currentSep:on:!private!rendering! !
!STDocIndexTemplate categoriesFor: #renderPackageIndexOn:!private!rendering! !
!STDocIndexTemplate categoriesFor: #sortedCollection!private! !

!STDocIndexTemplate class methodsFor!

processorName
	"The name of the listener used in the XML files"

	^'index'! !
!STDocIndexTemplate class categoriesFor: #processorName!private! !

STDocTemplateSpec guid: (GUID fromString: '{fff2e958-806f-4656-86f6-8f32dc98a44e}')!
STDocTemplateSpec comment: 'This class handles the main template file ''template.xml''.
It is the main Documenter, It instantiates all the other documenter based on the template file used.

It also handles the copying of extra files that can be used in the pages.'!
!STDocTemplateSpec categoriesForClass!Unclassified! !
!STDocTemplateSpec methodsFor!

addConfig: aNode
	"Sets a configuration item in the documentation manager.
	Configuration keys are PrivIcon, PubIcon, OverIcon, ClassIcon, BlankIcon, DefaultStyle.

	@param aNode the XML node with info about the configuration setting"

	| key value |
	key := (aNode attributes at: 'key'
				ifAbsent: [^self parseError: 'Missing "key" attribute in a config tag']) asUppercase.
	value := aNode attributes at: 'value'
				ifAbsent: [^self parseError: 'Missing "value" attribute in a config tag'].
	key = 'PRIVICON'
		ifTrue: 
			[self manager privIcon: value.
			^self].
	key = 'PUBICON'
		ifTrue: 
			[self manager pubIcon: value.
			^self].
	key = 'OVERICON'
		ifTrue: 
			[self manager overIcon: value.
			^self].
	key = 'DEFAULTSTYLE'
		ifTrue: 
			[self manager defaultStyle: value.
			^self].
	key = 'CLASSICON'
		ifTrue: 
			[self manager classIcon: value.
			^self].
	key = 'BLANKICON'
		ifTrue: 
			[self manager blankIcon: value.
			^self].
	self error: 'Configuration setting ' , key , ' not found.'!

addFileNode: aNode
	"Enquese a file to be copied when the template is made.

	@param aNode the XML node with info about the file"

	| src dest replace |
	src := aNode attributes at: 'src'
				ifAbsent: 
					[aNode attributes at: 'source' ifAbsent: [^self parseError: 'Missing "src" attribute in a file tag']].
	dest := aNode attributes at: 'dest'
				ifAbsent: 
					[aNode attributes at: 'destiny'
						ifAbsent: [^self parseError: 'Missing "dest" attribute in a file tag']].
	replace := (aNode attributes at: 'replace' ifAbsent: ['true']) sameAs: 'true'.
	filesToCopy add: (Array
				with: src
				with: dest
				with: replace)!

addTemplateNode: aNode
	"Adds a template to the manager that contains this object.

	@param aNode the XML node with info about the file"

	| type fName |
	type := aNode attributes at: 'type' ifAbsent: ['default'].
	fName := aNode attributes at: 'templateFile'
				ifAbsent: [^self parseError: 'Missing "templateFile" attribute in a <template> tag'].
	self manager
		addProcessor: (((STDocProcessor processorClassFor: type
				ifAbsent: [^self parseError: 'The type "' , type , '" of template does not exist'])
					forManager: self manager)
				fileName: fName;
				loadConfig: aNode;
				loadFile;
				yourself)!

filesToCopy
	^filesToCopy!

filesToCopy: anObject
	filesToCopy := anObject!

initialize
	super initialize.
	filesToCopy := OrderedCollection new!

parseDocument: anXMLNode
	"Initializes the state of the object with information contained in an XML file.
	
	@param anXMLNode the root node of an XML file"

	| childNodes len |
	childNodes := anXMLNode childNodes.
	len := childNodes size.
	self manager progressText: 'Parsing template...'.
	self manager progressValue: 0.
	1 to: len
		do: 
			[:index |
			| node nodeName |
			node := childNodes at: index.
			nodeName := node nodeName.
			nodeName = 'file'
				ifTrue: [self addFileNode: node]
				ifFalse: 
					[nodeName = 'template'
						ifTrue: [self addTemplateNode: node]
						ifFalse: 
							[nodeName = 'config'
								ifTrue: [self addConfig: node]
								ifFalse: [self parseError: 'Tag ' , nodeName , ' not recognized in the main template file']]].
			self manager progressValue: index / len * 100]!

processDoc
	"Executes the main purpose of the documenter, in general generates the output documentation files"

	| len |
	self manager progressText: 'Copying files...'.
	self manager progressValue: 0.
	len := filesToCopy size.
	1 to: len
		do: 
			[:n |
			| fCopy srcDir destDir |
			fCopy := filesToCopy at: n.
			srcDir := self manager config makeTemplateAbsolute: fCopy first.
			destDir := self manager config makeDocAbsolute: fCopy second.
			(fCopy third or: [(File exists: destDir) not]) ifTrue: [File copy: srcDir to: destDir].
			self manager progressValue: n / len * 100]! !
!STDocTemplateSpec categoriesFor: #addConfig:!parsing!public! !
!STDocTemplateSpec categoriesFor: #addFileNode:!parsing!public! !
!STDocTemplateSpec categoriesFor: #addTemplateNode:!parsing!public! !
!STDocTemplateSpec categoriesFor: #filesToCopy!accessing!private! !
!STDocTemplateSpec categoriesFor: #filesToCopy:!accessing!private! !
!STDocTemplateSpec categoriesFor: #initialize!private! !
!STDocTemplateSpec categoriesFor: #parseDocument:!parsing!public! !
!STDocTemplateSpec categoriesFor: #processDoc!document!public! !

!STDocTemplateSpec class methodsFor!

processorName
	"The name of the listener used in the XML files"

	^'templateSpec'! !
!STDocTemplateSpec class categoriesFor: #processorName!private! !

STDocPackageTemplate guid: (GUID fromString: '{3e42746e-ff22-497d-964f-2258fe110570}')!
STDocPackageTemplate comment: ''!
!STDocPackageTemplate categoriesForClass!Unclassified! !
!STDocPackageTemplate methodsFor!

fileNameFor: anObject
	"Generates the file name that's going to be wrote for this object.

	@param anObject the object
	@returns a string with the file name"

	^self manager packageFileName: anObject!

possibleChildTypes
	"Returns a Set of classes of renderers that can be childs of this type of renderer"

	^(Set new)
		add: STDocPackageRenderer;
		add: STDocTextRenderer;
		yourself!

processDoc
	"Executes the main purpose of the documenter, in general generates the output documentation files.
	Writes a file for each one of the packages selected for documentarion using the parsed template and the renderers for packages. It contains a collection of renderers that where parsed from the xml file. Those renderers are the ones that handle the output."

	| somePackages len |
	somePackages := self manager inferedPackages.
	len := somePackages size.
	self manager progressText: 'Making documentation for packages...'.
	self manager progressValue: 0.
	1 to: len
		do: 
			[:index |
			| aPackage |
			aPackage := somePackages at: index.
			self renderize: aPackage.
			self manager progressValue: index / len * 100]! !
!STDocPackageTemplate categoriesFor: #fileNameFor:!document!private! !
!STDocPackageTemplate categoriesFor: #possibleChildTypes!public! !
!STDocPackageTemplate categoriesFor: #processDoc!document!public! !

!STDocPackageTemplate class methodsFor!

processorName
	"The name of the listener used in the XML files"

	^'package'! !
!STDocPackageTemplate class categoriesFor: #processorName!private! !

STDocCondCmd guid: (GUID fromString: '{88a937f1-d937-4d1f-bf82-3afd9afdc30e}')!
STDocCondCmd comment: ''!
!STDocCondCmd categoriesForClass!Unclassified! !
!STDocCondCmd methodsFor!

condition
	^condition!

condition: anObject
	condition := anObject!

execute: obj on: aStream
	(self condition value: obj)
		ifTrue: [self trueCommands do: [:c | c execute: obj on: aStream]]
		ifFalse: [self falseCommands do: [:c | c execute: obj on: aStream]]!

falseCommands
	^falseCommands!

falseCommands: anObject
	falseCommands := anObject!

initialize
	super initialize.
	trueCommands := OrderedCollection new.
	falseCommands := OrderedCollection new!

trueCommands
	^trueCommands!

trueCommands: anObject
	trueCommands := anObject! !
!STDocCondCmd categoriesFor: #condition!accessing!public! !
!STDocCondCmd categoriesFor: #condition:!accessing!public! !
!STDocCondCmd categoriesFor: #execute:on:!public! !
!STDocCondCmd categoriesFor: #falseCommands!accessing!public! !
!STDocCondCmd categoriesFor: #falseCommands:!accessing!public! !
!STDocCondCmd categoriesFor: #initialize!private! !
!STDocCondCmd categoriesFor: #trueCommands!accessing!public! !
!STDocCondCmd categoriesFor: #trueCommands:!accessing!public! !

STDocTextCmd guid: (GUID fromString: '{7a8a3190-c10e-4536-914a-c75332cabfcd}')!
STDocTextCmd comment: ''!
!STDocTextCmd categoriesForClass!Unclassified! !
!STDocTextCmd methodsFor!

execute: obj on: aStream
	| args |
	args := Array
				with: obj
				with: self params
				with: aStream.
	self renderer perform: self name withArguments: args!

name
	^name!

name: anObject
	name := anObject!

params
	^params!

params: anObject
	params := anObject! !
!STDocTextCmd categoriesFor: #execute:on:!public! !
!STDocTextCmd categoriesFor: #name!accessing!public! !
!STDocTextCmd categoriesFor: #name:!accessing!public! !
!STDocTextCmd categoriesFor: #params!accessing!public! !
!STDocTextCmd categoriesFor: #params:!accessing!public! !

STDocClassTemplateRenderer guid: (GUID fromString: '{7e802f1c-cb2d-4127-ad5b-14f2c1d5036b}')!
STDocClassTemplateRenderer comment: ''!
!STDocClassTemplateRenderer categoriesForClass!Unclassified! !
!STDocClassTemplateRenderer methodsFor!

parseNode: aNode
	"It parses the renderer node and initializes the internal data.

	@param aNode the xml node"

	super parseNode: aNode.
	header := (aNode selectSingleNode: './header') ifNil: [''] ifNotNil: [:titleNode | titleNode text].
	footer := (aNode selectSingleNode: './footer') ifNil: [''] ifNotNil: [:titleNode | titleNode text]! !
!STDocClassTemplateRenderer categoriesFor: #parseNode:!parsing!public! !

!STDocClassTemplateRenderer class methodsFor!

rendererName
	"This is a custom name used to identify the renderer in the xml code, each renderer must have it's own name"

	^nil! !
!STDocClassTemplateRenderer class categoriesFor: #rendererName!public! !

STDocIndexRenderer guid: (GUID fromString: '{7c97bda5-ae36-4e72-8d3b-abc8f70dc07b}')!
STDocIndexRenderer comment: ''!
!STDocIndexRenderer categoriesForClass!Unclassified! !
!STDocIndexRenderer methodsFor!

availableCommands
	"The available renderers that can be used to render objects

	@returns a lookup table that has as key the command name and as value the method selector"

	^(super availableCommands)
		at: 'INDEXBAR' put: #renderIndexBar:params:on:;
		at: 'INDEXBODY' put: #renderIndexBody:params:on:;
		at: 'INDEXNAME' put: #renderIndexName:params:on:;
		yourself!

fileName
	^fileName!

fileName: anObject
	fileName := anObject!

parseNode: aNode
	"It parses the renderer node and initializes the internal data.

	@param aNode the xml node"

	super parseNode: aNode.
	fileName := aNode attributes at: 'fileName' ifAbsent: []!

printClassLinkOf: aClass on: aStream
	"Private - Prints on the stream a class link if the class exists in the classes that are going to be rendered

	@param aClass the class that's going to be linked
	@param aStream the stream where the link is going to be rendered"

	(self manager includesClass: aClass)
		ifTrue: 
			[aStream
				nextPutAll: '<a href="javascript:browseClass(''';
				nextPutAll: (self manager classFileName: aClass);
				nextPutAll: '.html'');">';
				nextPutAll: aClass name asString;
				nextPutAll: '</a>']
		ifFalse: [aStream nextPutAll: aClass name asString]!

renderIndexBar: anIndex params: params on: aStream
	"Renders the index name in the stream.
	
	@param aClass the class
	@param params parameters for the render command
	@param aStream the stream where you are going to render the result"

	| tableClass colClass |
	params size = 2
		ifTrue: 
			[tableClass := params first = '' ifFalse: [params first].
			colClass := params second = '' ifFalse: [params second]].
	self
		renderTableFor: anIndex activeIndexesLinks
		renderBlock: 
			[:str :lnk |
			str
				nextPutAll: '<a href="';
				nextPutAll: lnk second;
				nextPutAll: '">';
				nextPutAll: lnk first;
				nextPutAll: '</a>']
		tableClass: tableClass
		colClass: colClass
		columns: 2
		cellspacing: nil
		on: aStream!

renderIndexBody: anIndex params: params on: aStream
	"Renders the index body in the stream.
	
	@param anIndex the index that contains this renderer
	@param params parameters for the render command
	@param aStream the stream where you are going to render the result"

	anIndex renderIndex: type on: aStream!

renderIndexName: anIndex params: params on: aStream
	"Renders the index name in the stream.
	
	@param anIndex the index that contains this renderer
	@param params parameters for the render command
	@param aStream the stream where you are going to render the result"

	anIndex renderIndexName: type on: aStream!

type
	^type!

type: anObject
	type := anObject! !
!STDocIndexRenderer categoriesFor: #availableCommands!public! !
!STDocIndexRenderer categoriesFor: #fileName!accessing!public! !
!STDocIndexRenderer categoriesFor: #fileName:!accessing!public! !
!STDocIndexRenderer categoriesFor: #parseNode:!public! !
!STDocIndexRenderer categoriesFor: #printClassLinkOf:on:!private! !
!STDocIndexRenderer categoriesFor: #renderIndexBar:params:on:!public! !
!STDocIndexRenderer categoriesFor: #renderIndexBody:params:on:!public! !
!STDocIndexRenderer categoriesFor: #renderIndexName:params:on:!public! !
!STDocIndexRenderer categoriesFor: #type!accessing!public! !
!STDocIndexRenderer categoriesFor: #type:!accessing!public! !

!STDocIndexRenderer class methodsFor!

rendererName
	"This is a custom name used to identify the renderer in the xml code, each renderer must have it's own name"

	^'index'! !
!STDocIndexRenderer class categoriesFor: #rendererName!public! !

STDocPackageRenderer guid: (GUID fromString: '{8c9f976c-3af6-416f-ba77-55e1b7735951}')!
STDocPackageRenderer comment: ''!
!STDocPackageRenderer categoriesForClass!Unclassified! !
!STDocPackageRenderer methodsFor!

availableCommands
	"The available renderers that can be used to render objects

	@returns a lookup table that has as key the command name and as value the method selector"

	^(super availableCommands)
		at: 'PACKAGENAME' put: #renderPackageName:params:on:;
		at: 'PACKAGECOMMENT' put: #renderPackageComment:params:on:;
		at: 'PACKAGECLASSES' put: #renderPackageClasses:params:on:;
		at: 'PACKAGEDEPENDENCIES' put: #renderPackageDependencies:params:on:;
		at: 'PACKAGEDEPENDENTS' put: #renderPackageDependents:params:on:;
		yourself!

printClassLinkOf: aClass on: aStream
	"Private - Prints on the stream a class link if the class exists in the classes that are going to be rendered

	@param aClass the class that's going to be linked
	@param aStream the stream where the link is going to be rendered"

	(self manager includesClass: aClass)
		ifTrue: 
			[aStream
				nextPutAll: '<a href="';
				nextPutAll: (self manager classFileName: aClass);
				nextPutAll: '">';
				nextPutAll: aClass name asString;
				nextPutAll: '</a>']
		ifFalse: [aStream nextPutAll: aClass name asString]!

renderPackageClasses: aPackage params: params on: aStream
	"Renders package table of classes.

	@param aPackage the package that is going to be rendered
	@param params an array of parameters passed
	@param aStream the stream where the render is going to output the data"

	| tableClass colClass |
	params size = 2
		ifTrue: 
			[tableClass := params first = '' ifFalse: [params first].
			colClass := params second = '' ifFalse: [params second]].
	self
		renderTableFor: (aPackage classes asSortedCollection: [:c1 :c2 | c1 name <= c2 name])
		renderBlock: [:str :cls | self printClassLinkOf: cls on: str]
		tableClass: tableClass
		colClass: colClass
		columns: 4
		cellspacing: '0'
		on: aStream!

renderPackageComment: aPackage params: params on: aStream
	"Renders the package comment.
	
	@param aPackage the package
	@param params parameters for the render command
	@param aStream the stream where you are going to render the result"

	STDocUtil makeHTMLString: (STDocComment for: aPackage comment) fullComment on: aStream!

renderPackageDependencies: aPackage params: params on: aStream
	"Genera una tabla con todos los paquetes de los cuales el paquete indicado precisa para existir.
	En otras palabras, son las dependencias del paquete.

	@param aPackage el paquete hijo del que se imprimiran las dependencias
	@param params el estilo de la tabla de paquetes, el primer elemento es el estilo de tabla y el segundo el de celda
	@param aStream el stream donde se almacenara la tabla"

	self
		renderPackageTable: (aPackage prerequisites asSortedCollection: [:p1 :p2 | p1 name <= p2 name])
		tableClass: (params at: 1 ifPresent: [:style | style])
		colClass: (params at: 2 ifPresent: [:style | style])
		on: aStream!

renderPackageDependents: aPackage params: params on: aStream
	"Genera una tabla de paquetes que contiene todos aquellos paquetes que dependen del paquete indicado por parámetro.
	En otras los paquetes dependientes del paquete indicado.

	@param aPackage el paquete padre del que se imprimiran los dependientes
	@param params el estilo de la tabla de paquetes, el primer elemento es el estilo de tabla y el segundo el de celda
	@param aStream el stream donde se almacenara la tabla"

	self
		renderPackageTable: (aPackage dependentPackages asSortedCollection: [:p1 :p2 | p1 name <= p2 name])
		tableClass: (params at: 1 ifPresent: [:style | style])
		colClass: (params at: 2 ifPresent: [:style | style])
		on: aStream!

renderPackageName: aPackage params: params on: aStream
	"Renders package name on the stream

	@param aPackage the package that is going to be rendered
	@param params an array of parameters passed
	@param aStream the stream where the render is going to output the data"

	aStream nextPutAll: aPackage name!

renderPackageTable: somePackages tableClass: tableClass colClass: colClass on: aStream
	"Private - Renderiza una tabla con los paquetes indicados en la colección y con el estilo indicado en tableClass y colClass

	@param somePackages una colección de paquetes Smalltalk
	@param tableClass el estilo de la tabla a renderizar
	@param colClass el estilo de las celdas a renderizar
	@param aStream el stream donde se debe renderizar la tabla"

	somePackages isEmpty
		ifTrue: [aStream nextPutAll: '<span class="disabled">Ningun paquete entra en esta categoría</span>']
		ifFalse: 
			[self
				renderTableFor: somePackages
				renderBlock: [:str :pack | self printPackageLinkOf: pack on: str]
				tableClass: tableClass
				colClass: colClass
				columns: 4
				cellspacing: '0'
				on: aStream]! !
!STDocPackageRenderer categoriesFor: #availableCommands!public! !
!STDocPackageRenderer categoriesFor: #printClassLinkOf:on:!private! !
!STDocPackageRenderer categoriesFor: #renderPackageClasses:params:on:!public! !
!STDocPackageRenderer categoriesFor: #renderPackageComment:params:on:!public! !
!STDocPackageRenderer categoriesFor: #renderPackageDependencies:params:on:!public! !
!STDocPackageRenderer categoriesFor: #renderPackageDependents:params:on:!public! !
!STDocPackageRenderer categoriesFor: #renderPackageName:params:on:!public! !
!STDocPackageRenderer categoriesFor: #renderPackageTable:tableClass:colClass:on:!private! !

!STDocPackageRenderer class methodsFor!

rendererName
	"This is a custom name used to identify the renderer in the xml code, each renderer must have it's own name"

	^'package'! !
!STDocPackageRenderer class categoriesFor: #rendererName!public! !

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

STDocClassRenderer guid: (GUID fromString: '{0c4741f2-f128-4994-b58d-a58eb2befb28}')!
STDocClassRenderer comment: ''!
!STDocClassRenderer categoriesForClass!Unclassified! !
!STDocClassRenderer methodsFor!

availableCommands
	"The available renderers that can be used to render objects

	@returns a lookup table that has as key the command name and as value the method selector"

	^(super availableCommands)
		at: 'CLASSID' put: #renderClassID:params:on:;
		at: 'CLASSNAME' put: #renderClassName:params:on:;
		at: 'CLASSDEFINITION' put: #renderClassDefinition:params:on:;
		at: 'CLASSPACKAGE' put: #renderClassPackage:params:on:;
		at: 'CLASSSUPERCLASSES' put: #renderClassSuperclasses:params:on:;
		at: 'CLASSSUPERCLASS' put: #renderClassSuperclass:params:on:;
		at: 'CLASSCOMMENT' put: #renderClassComment:params:on:;
		at: 'CLASSHIERARCHY' put: #renderClassHierarchy:params:on:;
		yourself!

possibleChildTypes
	"Returns a Set of classes of renderers that can be childs of this type of renderer"

	^(super possibleChildTypes)
		add: STDocClassRenderer;
		add: STDocMethodRenderer;
		add: STDocVariableRenderer;
		yourself!

renderClassComment: aClass params: params on: aStream
	"Renders the class comment.
	
	@param aClass the class
	@param params parameters for the render command
	@param aStream the stream where you are going to render the result"

	params notEmpty ifTrue: [aStream nextPutAll: params first].
	STDocUtil makeHTMLString: aClass docComment fullComment on: aStream!

renderClassDefinition: aClass params: params on: aStream
	"Renders the class definition on the stream.
	
	@param aClass the class
	@param params parameters for the render command
	@param aStream the stream where you are going to render the class name"

	aClass classObject printDefinitionOn: aStream!

renderClassHierarchy: aClass params: params on: aStream
	"Renders a tree for tge class that shows the superclasses and the subclasses.
	
	@param aClass the class
	@param params parameters for the render command
	@param aStream the stream where you are going to render the class hierarchy"

	| spaces superclasses |
	superclasses := aClass superClasses.
	superclasses addLast: aClass.
	spaces := ''.
	superclasses do: 
			[:cls |
			aStream
				nextPutAll: spaces;
				nextPutAll: '<b>'.
			self
				printClassLink: cls
				currentClass: aClass
				on: aStream.
			aStream
				nextPutAll: '</b><br />';
				cr.
			spaces := spaces , '&nbsp;&nbsp;&nbsp;'].
	aClass subclasses do: 
			[:cls |
			(self manager includesClass: cls)
				ifTrue: 
					[aStream
						nextPutAll: spaces;
						nextPutAll: '<b>'.
					self
						printClassLink: cls
						currentClass: aClass
						on: aStream.
					aStream
						nextPutAll: '</b><br />';
						cr]]!

renderClassID: aClass params: params on: aStream
	"Renders the class name on the stream.
	
	@param aClass the class name
	@param params parameters for the render command
	@param aStream the stream where you are going to render the class name"

	aStream nextPutAll: (STDocUtil makeIDString: aClass name asString)!

renderClassName: aClass params: params on: aStream
	"Renders the class name on the stream.
	
	@param aClass the class name
	@param params parameters for the render command
	@param aStream the stream where you are going to render the class name"

	aStream nextPutAll: aClass name asString!

renderClassPackage: aClass params: params on: aStream
	"Renders the class package name on the stream.
	
	@param aClass the class
	@param params parameters for the render command
	@param aStream the stream where you are going to render the package name"

	self printPackageLinkOf: aClass package on: aStream!

renderClassSuperclass: aClass params: params on: aStream
	"Renders the inmediate superclass name.
	
	@param aClass the class
	@param params parameters for the render command
	@param aStream the stream where you are going to render the class name"

	aClass superclass
		ifNil: [aStream nextPutAll: '<code>nil</code>']
		ifNotNil: 
			[:superClass |
			aStream nextPutAll: '<b>'.
			self
				printClassLink: superClass
				currentClass: aClass
				on: aStream.
			aStream nextPutAll: '</b>']!

renderClassSuperclasses: aClass params: params on: aStream
	"Renders the superclasses tree for the class.
	
	@param aClass the class
	@param params parameters for the render command
	@param aStream the stream where you are going to render the class name"

	| spaces superclasses |
	superclasses := aClass superClasses.
	spaces := ''.
	superclasses do: 
			[:cls |
			aStream
				nextPutAll: spaces;
				nextPutAll: '<b>'.
			self
				printClassLink: cls
				currentClass: aClass
				on: aStream.
			aStream
				nextPutAll: '</b><br />';
				cr.
			spaces := spaces , '&nbsp;&nbsp;&nbsp;']! !
!STDocClassRenderer categoriesFor: #availableCommands!public! !
!STDocClassRenderer categoriesFor: #possibleChildTypes!public! !
!STDocClassRenderer categoriesFor: #renderClassComment:params:on:!public! !
!STDocClassRenderer categoriesFor: #renderClassDefinition:params:on:!public! !
!STDocClassRenderer categoriesFor: #renderClassHierarchy:params:on:!public! !
!STDocClassRenderer categoriesFor: #renderClassID:params:on:!public! !
!STDocClassRenderer categoriesFor: #renderClassName:params:on:!public! !
!STDocClassRenderer categoriesFor: #renderClassPackage:params:on:!public! !
!STDocClassRenderer categoriesFor: #renderClassSuperclass:params:on:!public! !
!STDocClassRenderer categoriesFor: #renderClassSuperclasses:params:on:!public! !

!STDocClassRenderer class methodsFor!

rendererName
	"This is a custom name used to identify the renderer in the xml code, each renderer must have it's own name"

	^'class'! !
!STDocClassRenderer class categoriesFor: #rendererName!public! !

STDocMethodRenderer guid: (GUID fromString: '{c8c1537f-e0cc-4838-a084-8b97c141cda6}')!
STDocMethodRenderer comment: ''!
!STDocMethodRenderer categoriesForClass!Unclassified! !
!STDocMethodRenderer methodsFor!

availableCommands
	"The available renderers that can be used to render objects

	@returns a lookup table that has as key the command name and as value the method selector"

	^(super availableCommands)
		at: 'METHODNAME' put: #renderMethodName:params:on:;
		at: 'METHODPARAMS' put: #renderMethodParams:params:on:;
		at: 'METHODRETURN' put: #renderMethodReturn:params:on:;
		at: 'METHODSMALLCOMMENT' put: #renderMethodSmallComment:params:on:;
		at: 'METHODLARGECOMMENT' put: #renderMethodLargeComment:params:on:;
		at: 'METHODACCESS' put: #renderMethodAccess:params:on:;
		at: 'METHODOVERRIDE' put: #renderMethodOverride:params:on:;
		at: 'METHODCLASS' put: #renderMethodClass:params:on:;
		at: 'METHODID' put: #renderMethodID:params:on:;
		at: 'METHODCATEGORIES' put: #renderMethodCategories:params:on:;
		yourself!

getObjectsOf: aDOCClass
	"Private - Returns a Collection with objects that are going to be rendered"

	| methods |
	methods := methodType = 'CLASS'
				ifTrue: [aDOCClass classMethods]
				ifFalse: 
					[methodType = 'INST'
						ifTrue: [aDOCClass methods]
						ifFalse: 
							[methodType = 'ALL'
								ifTrue: [aDOCClass classMethods , aDOCClass methods]
								ifFalse: [OrderedCollection new]]].
	methods do: [:method | self setCommentsOf: method].
	^methods!

isTrue: condString of: anObject
	(condString sameAs: 'hasMoreComment') ifTrue: [^anObject comment hasLargeComment].
	super isTrue: condString of: anObject!

methodID: aMethod on: aStream
	^aMethod definitionIDOn: aStream!

parseNode: aNode
	super parseNode: aNode.
	methodType := (aNode attributes at: 'methodType' ifAbsent: ['INST']) asUppercase!

renderMethodAccess: aMethod params: params on: aStream
	"Renders the method access image.
	
	@param aMethod the method
	@param params parameters for the render command
	@param aStream the stream where you are going to render"

	aStream
		nextPutAll: (self manager imageTag: (aMethod methodObject isPrivate
							ifTrue: [self manager privIcon]
							ifFalse: [self manager pubIcon]));
		cr!

renderMethodCategories: aMethod params: params on: aStream
	"Renders the method categories list.
	
	@param aMethod the method
	@param params parameters for the render command
	@param aStream the stream where you are going to render"

	| categoriesNames |
	categoriesNames := aMethod methodObject categories collect: [:c | c name].
	(categoriesNames notEmpty and: [params notEmpty]) ifTrue: [aStream nextPutAll: params first].
	categoriesNames do: [:cName | aStream nextPutAll: cName] separatedBy: [aStream nextPutAll: ', ']!

renderMethodClass: aMethod params: params on: aStream
	"Renders the method class image.
	
	@param aMethod the method
	@param params parameters for the render command
	@param aStream the stream where you are going to render"

	aStream
		nextPutAll: (self manager imageTag: (aMethod methodObject isClassMethod
							ifTrue: [self manager classIcon]
							ifFalse: [self manager blankIcon]));
		cr!

renderMethodID: aMethod params: params on: aStream
	"Renders an id for the method that doesn't have spaces nor ':' to use for identifying the method.
	
	@param aMethod the method
	@param params parameters for the render command
	@param aStream the stream where you are going to render"

	self methodID: aMethod on: aStream!

renderMethodLargeComment: aMethod params: params on: aStream
	"Renders the method basic description.
	
	@param aMethod the method
	@param params parameters for the render command
	@param aStream the stream where you are going to render"

	"aStream nextPutAll: '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'."

	STDocUtil makeHTMLString: aMethod comment fullComment on: aStream!

renderMethodName: aMethod params: params on: aStream
	"Renders the method name.
	
	@param aMethod the method
	@param params parameters for the render command
	@param aStream the stream where you are going to render"

	| parts methodDef |
	methodDef := aMethod definition.
	parts := methodDef subStrings: ' '.
	1 to: parts size
		do: 
			[:i |
			i odd
				ifTrue: 
					[aStream
						nextPutAll: '<span style="color: blue;font-weight:bold">';
						nextPutAll: (parts at: i) trimBlanks;
						nextPutAll: ' </span> ']
				ifFalse: 
					[aStream
						nextPutAll: (parts at: i) trimBlanks;
						nextPutAll: ' ']]!

renderMethodOverride: aMethod params: params on: aStream
	"Renders the method override image.
	
	@param aMethod the method
	@param params parameters for the render command
	@param aStream the stream where you are going to render"

	aMethod methodObject isOverride
		ifTrue: 
			[(self searchOverrideClassOf: aMethod methodObject)
				ifNotNil: 
					[:c |
					aStream nextPutAll: '(Overridden from class '.
					self
						printClassLink: c
						currentClass: aMethod methodObject methodClass
						on: aStream.
					aStream nextPutAll: ')']]!

renderMethodParams: aMethod params: params on: aStream
	"Renders the method parameter descriptions.
	
	@param aMethod the method
	@param params parameters for the render command
	@param aStream the stream where you are going to render"

	| colClass paramsSize strings colStyle |
	strings := aMethod comment attributesAt: 'param' ifAbsent: [^self].
	paramsSize := params size.
	paramsSize > 0 ifTrue: [aStream nextPutAll: params first].
	aStream nextPutAll: '<table'.
	paramsSize > 2
		ifTrue: 
			[params second ~= ''
				ifTrue: 
					[aStream
						nextPutAll: ' class="';
						nextPutAll: params second;
						nextPutAll: '"'].
			colClass := params third.
			colClass = '' ifTrue: [colClass := nil]].
	aStream nextPutAll: '>'.
	colClass notNil ifTrue: [colStyle := ' class="' , colClass , '"'] ifFalse: [colStyle := ''].
	strings do: 
			[:string |
			| name stream |
			stream := string readStream.
			name := STDocUtil firstWordOf: stream.
			aStream
				nextPutAll: '<tr><td border="0" style="width:12px"';
				nextPutAll: colStyle;
				nextPutAll: '>&nbsp;</td><td style="font-weight:bold"';
				nextPutAll: colStyle;
				nextPutAll: '>';
				nextPutAll: name;
				nextPutAll: '&nbsp;&nbsp;&nbsp;</td><td';
				nextPutAll: colStyle;
				nextPutAll: '>';
				nextPutAll: (stream atEnd ifTrue: [''] ifFalse: [stream upToEnd]);
				nextPutAll: '</td></tr>';
				cr].
	aStream
		nextPutAll: '</table>';
		cr!

renderMethodReturn: aMethod params: params on: aStream
	"Renders the method return comment.
	
	@param aMethod the method
	@param params parameters for the render command
	@param aStream the stream where you are going to render"

	| strings |
	strings := aMethod comment attributesAt: 'returns'
				ifAbsent: [aMethod comment attributesAt: 'return' ifAbsent: [^self]].
	params notEmpty ifTrue: [aStream nextPutAll: params first].
	aStream nextPutAll: '<table>'.
	strings do: 
			[:string |
			aStream
				nextPutAll: '<tr><td class="' , self manager defaultStyle
							, '" style="width:12px">&nbsp;</td><td class="' , self manager defaultStyle
							, '">';
				nextPutAll: string;
				nextPutAll: '</td></tr>';
				cr].
	aStream
		nextPutAll: '</table>';
		cr!

renderMethodSmallComment: aMethod params: params on: aStream
	"Renders the method basic description.
	
	@param aMethod the method
	@param params parameters for the render command
	@param aStream the stream where you are going to render"

	STDocUtil makeHTMLString: aMethod comment smallComment on: aStream!

searchOverrideClassOf: aMethod
	| c |
	c := aMethod methodClass superclass.
	[c notNil] whileTrue: 
			[(c compiledMethodAt: aMethod selector ifAbsent: []) ifNotNil: [:m | ^c].
			c := c superclass].
	^nil!

setCommentsOf: method
	(method comment attributesAt: 'override' ifAbsent: []) notNil
		ifTrue: 
			[(self searchOverrideClassOf: method methodObject)
				ifNotNil: 
					[:cls |
					(self manager searchClass: cls name)
						ifNotNil: 
							[:docCls |
							(docCls searchMethod: method methodObject selector)
								ifNotNil: [:met | method comment combine: met comment]]]]! !
!STDocMethodRenderer categoriesFor: #availableCommands!public! !
!STDocMethodRenderer categoriesFor: #getObjectsOf:!private! !
!STDocMethodRenderer categoriesFor: #isTrue:of:!public! !
!STDocMethodRenderer categoriesFor: #methodID:on:!private! !
!STDocMethodRenderer categoriesFor: #parseNode:!public! !
!STDocMethodRenderer categoriesFor: #renderMethodAccess:params:on:!public! !
!STDocMethodRenderer categoriesFor: #renderMethodCategories:params:on:!public! !
!STDocMethodRenderer categoriesFor: #renderMethodClass:params:on:!public! !
!STDocMethodRenderer categoriesFor: #renderMethodID:params:on:!public! !
!STDocMethodRenderer categoriesFor: #renderMethodLargeComment:params:on:!public! !
!STDocMethodRenderer categoriesFor: #renderMethodName:params:on:!public! !
!STDocMethodRenderer categoriesFor: #renderMethodOverride:params:on:!public! !
!STDocMethodRenderer categoriesFor: #renderMethodParams:params:on:!public! !
!STDocMethodRenderer categoriesFor: #renderMethodReturn:params:on:!public! !
!STDocMethodRenderer categoriesFor: #renderMethodSmallComment:params:on:!public! !
!STDocMethodRenderer categoriesFor: #searchOverrideClassOf:!private! !
!STDocMethodRenderer categoriesFor: #setCommentsOf:!private! !

!STDocMethodRenderer class methodsFor!

rendererName
	"This is a custom name used to identify the renderer in the xml code, each renderer must have it's own name"

	^'method'! !
!STDocMethodRenderer class categoriesFor: #rendererName!public! !

STDocVariableRenderer guid: (GUID fromString: '{49b74884-0f99-4fb4-9e50-3555950f3c6a}')!
STDocVariableRenderer comment: ''!
!STDocVariableRenderer categoriesForClass!Unclassified! !
!STDocVariableRenderer methodsFor!

availableCommands
	"The available renderers that can be used to render objects

	@returns a lookup table that has as key the command name and as value the method selector"

	^(super availableCommands)
		at: 'VARNAME' put: #renderVarName:params:on:;
		at: 'VARID' put: #renderVarID:params:on:;
		at: 'VARCOMMENT' put: #renderVarComment:params:on:;
		at: 'VARTYPE' put: #renderVarType:params:on:;
		yourself!

getObjectsOf: aDOCClass
	"Private - Returns a Collection with objects that are going to be rendered"

	^varType = 'INST'
		ifTrue: [aDOCClass variables]
		ifFalse: 
			[varType = 'CLASS'
				ifTrue: [aDOCClass classVariables]
				ifFalse: 
					[varType = 'INSTCLASS'
						ifTrue: [aDOCClass instanceClassVariables]
						ifFalse: 
							[varType = 'ALL'
								ifTrue: [aDOCClass variables , aDOCClass classVariables , aDOCClass instanceClassVariables]
								ifFalse: [OrderedCollection new]]]]!

parseNode: aNode
	super parseNode: aNode.
	varType := (aNode attributes at: 'varType' ifAbsent: ['ALL']) asUppercase!

renderVarComment: aVariable params: params on: aStream
	"Renders the variable associated comment of the class.
	
	@param aVariable the variable name
	@param params parameters for the render command
	@param aStream the stream where you are going to render"

	aVariable comment isEmpty
		ifTrue: [aStream nextPutAll: '&nbsp;']
		ifFalse: [aStream nextPutAll: aVariable comment displayString]!

renderVarID: aVariable params: params on: aStream
	"Renders the variable name.
	
	@param aVariable the variable name
	@param params parameters for the render command
	@param aStream the stream where you are going to render"

	aStream nextPutAll: (STDocUtil makeIDString: aVariable name displayString)!

renderVarName: aVariable params: params on: aStream
	"Renders the variable name.
	
	@param aVariable the variable name
	@param params parameters for the render command
	@param aStream the stream where you are going to render"

	aStream nextPutAll: aVariable name displayString!

renderVarType: aVariable params: params on: aStream
	"Renders the variable type. If the variable is of the instance it renders an 'I', if it is a class variable it renders 'C' and if it is an class instance variable it renders 'IC'
	
	@param aVariable the variable name
	@param params parameters for the render command
	@param aStream the stream where you are going to render"

	aStream nextPutAll: (aVariable isClass
				ifTrue: ['C']
				ifFalse: [aVariable isInst ifTrue: ['I'] ifFalse: ['IC']])! !
!STDocVariableRenderer categoriesFor: #availableCommands!public! !
!STDocVariableRenderer categoriesFor: #getObjectsOf:!private! !
!STDocVariableRenderer categoriesFor: #parseNode:!public! !
!STDocVariableRenderer categoriesFor: #renderVarComment:params:on:!public! !
!STDocVariableRenderer categoriesFor: #renderVarID:params:on:!public! !
!STDocVariableRenderer categoriesFor: #renderVarName:params:on:!public! !
!STDocVariableRenderer categoriesFor: #renderVarType:params:on:!public! !

!STDocVariableRenderer class methodsFor!

rendererName
	"This is a custom name used to identify the renderer in the xml code, each renderer must have it's own name"

	^'variable'! !
!STDocVariableRenderer class categoriesFor: #rendererName!public! !

"Binary Globals"!

