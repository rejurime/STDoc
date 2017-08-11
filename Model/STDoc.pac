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
	add: #DOCAnd;
	add: #DOCClass;
	add: #DOCClassRenderer;
	add: #DOCClassTemplate;
	add: #DOCClassTemplateRenderer;
	add: #DOCComment;
	add: #DOCCondCmd;
	add: #DOCCondition;
	add: #DOCConfig;
	add: #DOCDefaultProcessor;
	add: #DOCIndexRenderer;
	add: #DOCIndexTemplate;
	add: #DOCMethod;
	add: #DOCMethodRenderer;
	add: #DOCObject;
	add: #DOCOr;
	add: #DOCPackageRenderer;
	add: #DOCPackageTemplate;
	add: #DOCParseError;
	add: #DOCPredefinedCond;
	add: #DOCProcessor;
	add: #DOCRenderCmd;
	add: #DOCRenderer;
	add: #DOCSmalltalkManager;
	add: #DOCSUtils;
	add: #DOCTemplateSpec;
	add: #DOCTextCmd;
	add: #DOCTextRenderer;
	add: #DOCVariable;
	add: #DOCVariableRenderer;
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

Object subclass: #DOCObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCObject subclass: #ClassDiagramBuilder
	instanceVariableNames: 'subjectClass methods classMethods outFile templateFile renderSuperclasses'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCObject subclass: #DOCClass
	instanceVariableNames: 'manager classObject classMethods methods variables classVariables instanceClassVariables docComment superClasses subClasses'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCObject subclass: #DOCComment
	instanceVariableNames: 'comment attrTable fullComment smallComment combined'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCObject subclass: #DOCCondition
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCObject subclass: #DOCConfig
	instanceVariableNames: 'renderPrivate renderPublic renderClassPublic renderClassPrivate templateName listeners templateDir docPath renderSuperclasses renderRelatedClasses renderClassAccessor renderAccessors'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCObject subclass: #DOCMethod
	instanceVariableNames: 'comment methodObject definition id'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCObject subclass: #DOCProcessor
	instanceVariableNames: 'manager fileName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCObject subclass: #DOCRenderCmd
	instanceVariableNames: 'renderer'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCObject subclass: #DOCRenderer
	instanceVariableNames: 'manager headerCommands bodyCommands footerCommands childRenderers parent'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCObject subclass: #DOCSmalltalkManager
	instanceVariableNames: 'config processors configKeys progress defaultStyle privIcon pubIcon overIcon classIcon blankIcon classes stClassSet packages'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCObject subclass: #DOCSUtils
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCObject subclass: #DOCVariable
	instanceVariableNames: 'name docClass type'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCCondition subclass: #DOCAnd
	instanceVariableNames: 'conditions'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCCondition subclass: #DOCOr
	instanceVariableNames: 'conditions'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCCondition subclass: #DOCPredefinedCond
	instanceVariableNames: 'renderer condition'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCProcessor subclass: #DOCClassTemplate
	instanceVariableNames: 'renderers outDir'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCProcessor subclass: #DOCDefaultProcessor
	instanceVariableNames: 'renderers outFile'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCProcessor subclass: #DOCIndexTemplate
	instanceVariableNames: 'hierarchy renderers'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCProcessor subclass: #DOCTemplateSpec
	instanceVariableNames: 'filesToCopy'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCClassTemplate subclass: #DOCPackageTemplate
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCRenderCmd subclass: #DOCCondCmd
	instanceVariableNames: 'trueCommands falseCommands condition'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCRenderCmd subclass: #DOCTextCmd
	instanceVariableNames: 'name params'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCRenderer subclass: #DOCClassTemplateRenderer
	instanceVariableNames: 'header footer'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCRenderer subclass: #DOCIndexRenderer
	instanceVariableNames: 'type fileName'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCRenderer subclass: #DOCPackageRenderer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCRenderer subclass: #DOCTextRenderer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCClassTemplateRenderer subclass: #DOCClassRenderer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCClassTemplateRenderer subclass: #DOCMethodRenderer
	instanceVariableNames: 'methodType'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
DOCClassTemplateRenderer subclass: #DOCVariableRenderer
	instanceVariableNames: 'varType'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Error subclass: #DOCParseError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

DOCObject guid: (GUID fromString: '{592A9D63-F819-4782-A5A0-197D460FB430}')!
DOCObject comment: ''!
!DOCObject categoriesForClass!Unclassified! !
!DOCObject methodsFor!

parseError: aString
	^DOCParseError signal: aString! !
!DOCObject categoriesFor: #parseError:!private! !

!DOCObject class methodsFor!

new
	^self basicNew initialize! !
!DOCObject class categoriesFor: #new!public! !

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

DOCClass guid: (GUID fromString: '{4F92FA62-A6BC-4FF7-8FCB-0C823A5B6155}')!
DOCClass comment: ''!
!DOCClass categoriesForClass!Unclassified! !
!DOCClass methodsFor!

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
				ifTrue: [obj value isBehavior ifTrue: [col add: (DOCClass forClass: obj value manager: self manager)]]].
	^col!

classVariables
	^classVariables
		ifNil: 
			[self selectVariables.
			classVariables]!

classVariables: anObject
	classVariables := anObject!

docComment
	^docComment ifNil: [docComment := DOCComment forClass: self classObject]!

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
	classVariables := (classObject classVarNames collect: [:v | DOCVariable forClassVar: self name: v])
				asOrderedCollection.
	variables := (classObject instVarNames collect: [:v | DOCVariable forInstVar: self name: v])
				asOrderedCollection.
	instanceClassVariables := (classObject class instVarNames
				collect: [:v | DOCVariable forClassInstVar: self name: v]) asOrderedCollection!

setFor: aClass
	classObject := aClass!

sortMethods: someMethods
	^(someMethods collect: [:m | DOCMethod for: m]) asSortedCollection!

subclasses
	subClasses notNil ifTrue: [^subClasses].
	subClasses := self classObject subclasses asSortedCollection: [:c1 :c2 | c1 name <= c2 name].
	^subClasses!

superclass
	^self classObject superclass
		ifNil: []
		ifNotNil: [:sprCls | DOCClass forClass: sprCls manager: self manager]!

superClasses
	"Makes a Collection of super classes of the contained class, that are ordered by proximity to the contained class reversed.
	The first element is the farest superclass of the class, generally Object class and the last element is the inmediate superclass.

	@return an OrderedCollection with the contained class super classes"

	| supers sprClass |
	superClasses notNil ifTrue: [^superClasses].
	supers := OrderedCollection new.
	sprClass := self superclass ifNotNil: [:cls | cls classObject].
	[sprClass notNil] whileTrue: 
			[supers add: (DOCClass forClass: sprClass manager: self manager).
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
!DOCClass categoriesFor: #=!public! !
!DOCClass categoriesFor: #classMethods!accessing!public! !
!DOCClass categoriesFor: #classMethods:!accessing!public! !
!DOCClass categoriesFor: #classObject!accessing!public! !
!DOCClass categoriesFor: #classObject:!accessing!public! !
!DOCClass categoriesFor: #classReferencesFrom:!private! !
!DOCClass categoriesFor: #classVariables!accessing!public! !
!DOCClass categoriesFor: #classVariables:!accessing!public! !
!DOCClass categoriesFor: #docComment!public! !
!DOCClass categoriesFor: #hash!public! !
!DOCClass categoriesFor: #instanceClassVariables!accessing!public! !
!DOCClass categoriesFor: #instanceClassVariables:!accessing!public! !
!DOCClass categoriesFor: #manager!accessing!public! !
!DOCClass categoriesFor: #manager:!accessing!public! !
!DOCClass categoriesFor: #methods!accessing!public! !
!DOCClass categoriesFor: #methods:!accessing!public! !
!DOCClass categoriesFor: #name!public! !
!DOCClass categoriesFor: #package!public! !
!DOCClass categoriesFor: #printOn:!public! !
!DOCClass categoriesFor: #relatedClasses!public! !
!DOCClass categoriesFor: #searchMethod:!public! !
!DOCClass categoriesFor: #selectMethods!private! !
!DOCClass categoriesFor: #selectVariables!private! !
!DOCClass categoriesFor: #setFor:!public! !
!DOCClass categoriesFor: #sortMethods:!private! !
!DOCClass categoriesFor: #subclasses!public! !
!DOCClass categoriesFor: #superclass!public! !
!DOCClass categoriesFor: #superClasses!public! !
!DOCClass categoriesFor: #variables!accessing!public! !
!DOCClass categoriesFor: #variables:!accessing!public! !

!DOCClass class methodsFor!

forClass: aClass manager: aManager
	"Creates a DOCClassfor a specific class and manager

	@param aClass the real smalltalk class
	@param aManager the manager that has the configuration"

	^(self new)
		manager: aManager;
		setFor: aClass;
		yourself! !
!DOCClass class categoriesFor: #forClass:manager:!public! !

DOCComment guid: (GUID fromString: '{FDCD6495-E6E7-44C5-A2F4-34EB915BDA97}')!
DOCComment comment: ''!
!DOCComment categoriesForClass!Unclassified! !
!DOCComment methodsFor!

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
	DOCSUtils ignoreFrom: str upTo: [:c | c == $@].
	result := LookupTable new.
	[(attribute := DOCSUtils stringFrom: str upTo: [:c | c == $@]) notNil] whileTrue: 
			[| id partStream |
			partStream := attribute readStream.
			id := DOCSUtils firstWordOf: partStream.
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
			smallComment := ((DOCSUtils stringFrom: line upTo: [:c | c = $.])
						ifNil: ['']
						ifNotNil: [:s | s , '.']) trimBlanks]! !
!DOCComment categoriesFor: #attributeIncludes:!public! !
!DOCComment categoriesFor: #attributeIncludesAll:!public! !
!DOCComment categoriesFor: #attributesAt:do:!public! !
!DOCComment categoriesFor: #attributesAt:ifAbsent:!public! !
!DOCComment categoriesFor: #attributesIn:do:!public! !
!DOCComment categoriesFor: #attributeTable!public! !
!DOCComment categoriesFor: #combine:!public! !
!DOCComment categoriesFor: #comment!accessing!public! !
!DOCComment categoriesFor: #comment:!accessing!public! !
!DOCComment categoriesFor: #fullComment!public! !
!DOCComment categoriesFor: #hasLargeComment!public! !
!DOCComment categoriesFor: #initialize!private! !
!DOCComment categoriesFor: #parseMethod:!private! !
!DOCComment categoriesFor: #smallComment!public! !

!DOCComment class methodsFor!

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
!DOCComment class categoriesFor: #for:!public! !
!DOCComment class categoriesFor: #forClass:!public! !
!DOCComment class categoriesFor: #forMethod:!public! !

DOCCondition guid: (GUID fromString: '{0F0025E8-3122-42A2-A1A1-45429C02E814}')!
DOCCondition comment: ''!
!DOCCondition categoriesForClass!Unclassified! !
!DOCCondition methodsFor!

cand: aCondition
	^(DOCAnd new)
		conditions: self;
		condition2: aCondition;
		yourself!

cor: aCondition
	^(DOCOr new)
		conditions: self;
		condition2: aCondition;
		yourself!

value: anObject
	"Debe procesar lo que sea necesario y retornar el valor booleano que corresponda.

	@param anObject es lo que se está renderizando, puede ser una clase, un método o una variable
	@returns true si la condición es válida o en caso contrario false"

	^self subclassResponsibility! !
!DOCCondition categoriesFor: #cand:!public! !
!DOCCondition categoriesFor: #cor:!public! !
!DOCCondition categoriesFor: #value:!public! !

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

DOCMethod guid: (GUID fromString: '{29133369-FEFE-4CF1-942D-E5553BC40746}')!
DOCMethod comment: ''!
!DOCMethod categoriesForClass!Unclassified! !
!DOCMethod methodsFor!

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
	^comment ifNil: [comment := DOCComment forMethod: self methodObject]!

comment: anObject
	comment := anObject!

definition
	definition notNil ifTrue: [^definition].
	definition := self methodObject getSource readStream nextLine.
	^definition!

definitionIDOn: aStream
	DOCSUtils makeIDString: self definition on: aStream!

methodObject
	^methodObject!

methodObject: anObject
	methodObject := anObject! !
!DOCMethod categoriesFor: #<=!public! !
!DOCMethod categoriesFor: #comment!accessing!public! !
!DOCMethod categoriesFor: #comment:!accessing!public! !
!DOCMethod categoriesFor: #definition!public! !
!DOCMethod categoriesFor: #definitionIDOn:!public! !
!DOCMethod categoriesFor: #methodObject!accessing!public! !
!DOCMethod categoriesFor: #methodObject:!accessing!public! !

!DOCMethod class methodsFor!

for: aMethod
	"Creates an instance for the specified method.

	@param aMethod the CompiledMethod that's going to be contained"

	^(self new)
		methodObject: aMethod;
		yourself! !
!DOCMethod class categoriesFor: #for:!public! !

DOCProcessor guid: (GUID fromString: '{C9C2770E-9F5C-4EDD-AF80-819FDF7FE214}')!
DOCProcessor comment: 'The subclasses of this class are the ones that handle the documentation process and the template files.
A type of template is associated to each class.
Each class has a different purpose and output. Some handle each class and makes documentation for them, other generates index files to navigate the files, other copy files and instantiates new documenters, etc.

The class that invoques the documenters is the DOCSmalltalkManager when the documentation process starts.'!
!DOCProcessor categoriesForClass!Unclassified! !
!DOCProcessor methodsFor!

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
!DOCProcessor categoriesFor: #fileName!accessing!public! !
!DOCProcessor categoriesFor: #fileName:!accessing!public! !
!DOCProcessor categoriesFor: #loadConfig:!parsing!public! !
!DOCProcessor categoriesFor: #loadFile!parsing!public! !
!DOCProcessor categoriesFor: #manager!accessing!public! !
!DOCProcessor categoriesFor: #manager:!accessing!public! !
!DOCProcessor categoriesFor: #parseDocument:!parsing!public! !
!DOCProcessor categoriesFor: #possibleChildTypes!public! !
!DOCProcessor categoriesFor: #processDoc!document!public! !

!DOCProcessor class methodsFor!

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
!DOCProcessor class categoriesFor: #forManager:!public! !
!DOCProcessor class categoriesFor: #processorClassFor:ifAbsent:!private! !
!DOCProcessor class categoriesFor: #processorName!private! !

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

DOCRenderer guid: (GUID fromString: '{CE39D2A6-0443-46FB-8114-A0A267B389BC}')!
DOCRenderer comment: 'This class handles the rendering of the documentation. They are instantiated from the XML.
It has two parts, the first is the template, where some special tags are replaced with information that the render formats. The other part is the child templates, those same tags make the child renderers of this render to print it''s data.

@var manager the manager that contains the renderer
@var commands the commands that are replaced with information. Each renderer has different commands
@var childRenderers a collection of child renderers identified by an id
@var parent if this render is a child render it contains the renderer that has this renderer as a child, otherwise is nil'!
!DOCRenderer categoriesForClass!Unclassified! !
!DOCRenderer methodsFor!

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

	aCollection add: ((DOCTextCmd for: self)
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
	[(container := self parent) isKindOf: DOCRenderer] whileTrue.
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
			param := DOCSUtils stringFrom: str
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
				ifTrue: [cond := DOCAnd new]
				ifFalse: 
					[(op sameAs: 'or') ifTrue: [cond := DOCOr new] ifFalse: [self error: 'Operador ' , op , ' inválido']].
			cond conditions: (node childNodes collect: [:condNode | self parseCondition: condNode]).
			^cond].
	^(DOCPredefinedCond new)
		condition: predefined;
		renderer: self;
		yourself!

parseConditionFrom: node
	^Error notYetImplemented!

parseConditionFrom: node on: cmdCol
	| cmd |
	cmd := (DOCCondCmd new)
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
			childRenderer := DOCRenderer
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

	^Set with: DOCTextRenderer!

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
!DOCRenderer categoriesFor: #addCommand:params:!parsing!public! !
!DOCRenderer categoriesFor: #addCommand:params:to:!parsing!public! !
!DOCRenderer categoriesFor: #availableCommands!commands!public!rendering! !
!DOCRenderer categoriesFor: #bodyCommands!accessing!public! !
!DOCRenderer categoriesFor: #bodyCommands:!accessing!public! !
!DOCRenderer categoriesFor: #container!public! !
!DOCRenderer categoriesFor: #getObjectsOf:!private!rendering! !
!DOCRenderer categoriesFor: #initialize!private! !
!DOCRenderer categoriesFor: #isTrue:of:!public!rendering! !
!DOCRenderer categoriesFor: #manager!accessing!public! !
!DOCRenderer categoriesFor: #manager:!accessing!public! !
!DOCRenderer categoriesFor: #parent!accessing!public! !
!DOCRenderer categoriesFor: #parent:!accessing!public! !
!DOCRenderer categoriesFor: #parseCommandName:!parsing!private! !
!DOCRenderer categoriesFor: #parseCommandParams:!parsing!private! !
!DOCRenderer categoriesFor: #parseCondition:!parsing!private! !
!DOCRenderer categoriesFor: #parseConditionFrom:!parsing!private! !
!DOCRenderer categoriesFor: #parseConditionFrom:on:!parsing!private! !
!DOCRenderer categoriesFor: #parseNode:!parsing!public! !
!DOCRenderer categoriesFor: #parseNode:nodeName:commandsOn:!parsing!private! !
!DOCRenderer categoriesFor: #parseTextFrom:on:!parsing!private! !
!DOCRenderer categoriesFor: #possibleChildTypes!public! !
!DOCRenderer categoriesFor: #printClassLink:currentClass:on:!private! !
!DOCRenderer categoriesFor: #printPackageLinkOf:on:!private! !
!DOCRenderer categoriesFor: #render:on:!public!rendering! !
!DOCRenderer categoriesFor: #renderChild:params:on:!commands!public!rendering! !
!DOCRenderer categoriesFor: #renderCommands:on:for:!private!rendering! !
!DOCRenderer categoriesFor: #renderCommandsOn:for:!private!rendering! !
!DOCRenderer categoriesFor: #renderLabel:params:on:!commands!public!rendering! !
!DOCRenderer categoriesFor: #renderTableFor:renderBlock:tableClass:colClass:columns:cellspacing:on:!private!rendering! !
!DOCRenderer categoriesFor: #renderText:params:on:!commands!public!rendering! !

!DOCRenderer class methodsFor!

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
				parseError: 'Renderers of type ' , aParent class rendererName 
						, ' can''t contain renderers of type ' , rendererClass rendererName].
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
!DOCRenderer class categoriesFor: #createRenderer:manager:parent:!public! !
!DOCRenderer class categoriesFor: #forManager:!public! !
!DOCRenderer class categoriesFor: #renderClassFor:ifAbsent:!private! !
!DOCRenderer class categoriesFor: #rendererName!public! !

DOCSmalltalkManager guid: (GUID fromString: '{5AE38F9C-D577-4E86-90AD-2A90678FF390}')!
DOCSmalltalkManager comment: 'This manager handles the documentation process. It has a configuration object that specifies wich objects are going to be rendered, the template that''s going to be used, the output directory and the classes that will be documented.

It has a bunch of listeners that handles the rendering process. Those listeners are parsed from the template file.

To document you first have to set the configuration and then start the process by calling the method #makeDoc

Writer: Javier
1st Assistant: Leo Arias (A.K.A. "Capa Basica" Designer)
CoPilot: Svanlacke (A.K.A. Reprimed Hawk)'!
!DOCSmalltalkManager categoriesForClass!Unclassified! !
!DOCSmalltalkManager methodsFor!

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
	config := DOCConfig new.
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
		labelAt: 'COPYRIGHT' put: '©CIDESO 2007'!

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
	template := DOCTemplateSpec forManager: self.
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
	cls addAll: (stClasses collect: [:c | DOCClass forClass: c manager: self]).
	self classes addAll: cls.
	self config renderRelatedClasses ifTrue: [cls do: [:c | self classes addAll: c relatedClasses]].
	self config renderSuperclasses ifTrue: [cls do: [:c | self classes addAll: c superClasses]].
	stClassSet := nil!

stClassSet
	stClassSet notNil ifTrue: [^stClassSet].
	stClassSet := IdentitySet new.
	self classesDo: [:c | stClassSet add: c classObject].
	^stClassSet! !
!DOCSmalltalkManager categoriesFor: #addProcessor:!private! !
!DOCSmalltalkManager categoriesFor: #blankIcon!accessing!public! !
!DOCSmalltalkManager categoriesFor: #blankIcon:!accessing!public! !
!DOCSmalltalkManager categoriesFor: #classes!accessing!public! !
!DOCSmalltalkManager categoriesFor: #classes:!accessing!public! !
!DOCSmalltalkManager categoriesFor: #classesDo:!private! !
!DOCSmalltalkManager categoriesFor: #classFileName:!public! !
!DOCSmalltalkManager categoriesFor: #classIcon!accessing!public! !
!DOCSmalltalkManager categoriesFor: #classIcon:!accessing!public! !
!DOCSmalltalkManager categoriesFor: #config!accessing!public! !
!DOCSmalltalkManager categoriesFor: #config:!accessing!public! !
!DOCSmalltalkManager categoriesFor: #configKeys!accessing!public! !
!DOCSmalltalkManager categoriesFor: #configKeys:!accessing!public! !
!DOCSmalltalkManager categoriesFor: #createOutFileNamed:!public! !
!DOCSmalltalkManager categoriesFor: #defaultStyle!accessing!public! !
!DOCSmalltalkManager categoriesFor: #defaultStyle:!accessing!public! !
!DOCSmalltalkManager categoriesFor: #imageTag:!public! !
!DOCSmalltalkManager categoriesFor: #includesClass:!public! !
!DOCSmalltalkManager categoriesFor: #includesPackage:!public! !
!DOCSmalltalkManager categoriesFor: #inferedPackages!public! !
!DOCSmalltalkManager categoriesFor: #initialize!private! !
!DOCSmalltalkManager categoriesFor: #initializeDoc!private! !
!DOCSmalltalkManager categoriesFor: #initializeLabels!private! !
!DOCSmalltalkManager categoriesFor: #labelAt:!public! !
!DOCSmalltalkManager categoriesFor: #labelAt:put:!public! !
!DOCSmalltalkManager categoriesFor: #labelDictionary!public! !
!DOCSmalltalkManager categoriesFor: #makeDoc:!public! !
!DOCSmalltalkManager categoriesFor: #overIcon!accessing!public! !
!DOCSmalltalkManager categoriesFor: #overIcon:!accessing!public! !
!DOCSmalltalkManager categoriesFor: #packageFileName:!public! !
!DOCSmalltalkManager categoriesFor: #packages!public! !
!DOCSmalltalkManager categoriesFor: #packages:!public! !
!DOCSmalltalkManager categoriesFor: #parseTemplate!private! !
!DOCSmalltalkManager categoriesFor: #privIcon!accessing!public! !
!DOCSmalltalkManager categoriesFor: #privIcon:!accessing!public! !
!DOCSmalltalkManager categoriesFor: #processors!accessing!public! !
!DOCSmalltalkManager categoriesFor: #processors:!accessing!public! !
!DOCSmalltalkManager categoriesFor: #processorsDo:!private! !
!DOCSmalltalkManager categoriesFor: #progressText:!public! !
!DOCSmalltalkManager categoriesFor: #progressValue:!public! !
!DOCSmalltalkManager categoriesFor: #pubIcon!accessing!public! !
!DOCSmalltalkManager categoriesFor: #pubIcon:!accessing!public! !
!DOCSmalltalkManager categoriesFor: #reset!public! !
!DOCSmalltalkManager categoriesFor: #resetProcessors!public! !
!DOCSmalltalkManager categoriesFor: #searchClass:!public! !
!DOCSmalltalkManager categoriesFor: #setClasses:!public! !
!DOCSmalltalkManager categoriesFor: #stClassSet!private! !

DOCSUtils guid: (GUID fromString: '{8235FD37-C1CE-4A8F-9D60-4E92EE9905F8}')!
DOCSUtils comment: 'This class is used as a bag of functions to process strings and streams.
Al kinds of common functions are included here, most of them are for parsing texts'!
!DOCSUtils categoriesForClass!Unclassified! !
!DOCSUtils class methodsFor!

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
!DOCSUtils class categoriesFor: #charToHTML:!private! !
!DOCSUtils class categoriesFor: #consume:do:!public! !
!DOCSUtils class categoriesFor: #firstWordOf:!public! !
!DOCSUtils class categoriesFor: #ignore:where:!public! !
!DOCSUtils class categoriesFor: #ignoreFrom:upTo:!public! !
!DOCSUtils class categoriesFor: #isValidTag:!private! !
!DOCSUtils class categoriesFor: #makeHTMLString:on:!public! !
!DOCSUtils class categoriesFor: #makeIDString:!public! !
!DOCSUtils class categoriesFor: #makeIDString:on:!public! !
!DOCSUtils class categoriesFor: #makeTag:on:!private! !
!DOCSUtils class categoriesFor: #stringFrom:filterBlock:!public! !
!DOCSUtils class categoriesFor: #stringFrom:from:to:!public! !
!DOCSUtils class categoriesFor: #stringFrom:upTo:!public! !

DOCVariable guid: (GUID fromString: '{B766EE9C-5DFB-479A-BA71-D12299382387}')!
DOCVariable comment: ''!
!DOCVariable categoriesForClass!Unclassified! !
!DOCVariable methodsFor!

comment
	| str |
	(self docClass docComment attributeTable at: 'var' ifAbsent: [^'']) do: 
			[:string |
			str := string readStream.
			(DOCSUtils firstWordOf: str) = self name ifTrue: [^str upToEnd]].
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
!DOCVariable categoriesFor: #comment!public! !
!DOCVariable categoriesFor: #docClass!accessing!public! !
!DOCVariable categoriesFor: #docClass:!accessing!public! !
!DOCVariable categoriesFor: #firstWordOf:!private! !
!DOCVariable categoriesFor: #isClass!public! !
!DOCVariable categoriesFor: #isClassInst!public! !
!DOCVariable categoriesFor: #isInst!public! !
!DOCVariable categoriesFor: #name!accessing!public! !
!DOCVariable categoriesFor: #name:!accessing!public! !
!DOCVariable categoriesFor: #type!accessing!public! !
!DOCVariable categoriesFor: #type:!accessing!public! !

!DOCVariable class methodsFor!

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
!DOCVariable class categoriesFor: #forClassInstVar:name:!public! !
!DOCVariable class categoriesFor: #forClassVar:name:!public! !
!DOCVariable class categoriesFor: #forInstVar:name:!public! !
!DOCVariable class categoriesFor: #forType:class:name:!public! !

DOCAnd guid: (GUID fromString: '{C1999A98-807B-4465-8668-E6FB3B1699EF}')!
DOCAnd comment: ''!
!DOCAnd categoriesForClass!Unclassified! !
!DOCAnd methodsFor!

conditions
	^conditions!

conditions: anObject
	conditions := anObject!

value: anObject
	"@override"

	self conditions isEmpty ifTrue: [^true].
	^self conditions allSatisfy: [:cond | cond value: anObject]! !
!DOCAnd categoriesFor: #conditions!accessing!public! !
!DOCAnd categoriesFor: #conditions:!accessing!public! !
!DOCAnd categoriesFor: #value:!public! !

DOCOr guid: (GUID fromString: '{10E41E62-8400-4301-A827-FC14D205BEDE}')!
DOCOr comment: ''!
!DOCOr categoriesForClass!Unclassified! !
!DOCOr methodsFor!

conditions
	^conditions!

conditions: anObject
	conditions := anObject!

value: anObject
	"@override"

	self conditions isEmpty ifTrue: [^true].
	^self conditions anySatisfy: [:cond | cond value: anObject]! !
!DOCOr categoriesFor: #conditions!accessing!public! !
!DOCOr categoriesFor: #conditions:!accessing!public! !
!DOCOr categoriesFor: #value:!public! !

DOCPredefinedCond guid: (GUID fromString: '{BC28FD66-6780-4568-B1B4-AAD654D01C0A}')!
DOCPredefinedCond comment: ''!
!DOCPredefinedCond categoriesForClass!Unclassified! !
!DOCPredefinedCond methodsFor!

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
!DOCPredefinedCond categoriesFor: #condition!accessing!public! !
!DOCPredefinedCond categoriesFor: #condition:!accessing!public! !
!DOCPredefinedCond categoriesFor: #renderer!accessing!public! !
!DOCPredefinedCond categoriesFor: #renderer:!accessing!public! !
!DOCPredefinedCond categoriesFor: #value:!public! !

!DOCPredefinedCond class methodsFor!

str: conditionString on: aRenderer
	^(self new)
		renderer: aRenderer;
		condition: conditionString;
		yourself! !
!DOCPredefinedCond class categoriesFor: #str:on:!public! !

DOCClassTemplate guid: (GUID fromString: '{6B507EDB-98CB-4802-8589-29F466ACAB0E}')!
DOCClassTemplate comment: 'Makes an HTML page using a template file that is parsed beforehand.
The methods to render the classes are called by the DOCSmalltalkManager.

The main rendering method is #processDoc. This method tells each one of the renderers to render itself for each class.

This Documenter makes one file for each one of the classes that are going to be documented. The file name is the class name plus the html extension, if the class is Object the output file will be ''Object.html''.'!
!DOCClassTemplate categoriesForClass!Unclassified! !
!DOCClassTemplate methodsFor!

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
					[renderers addLast: (DOCRenderer
								createRenderer: xmlNode
								manager: self manager
								parent: self)]
				ifFalse: [self parseError: 'Tag name "' , xmlNode nodeName , '" in template file not recognized']]!

possibleChildTypes
	"Returns a Set of classes of renderers that can be childs of this type of renderer"

	^(super possibleChildTypes)
		add: DOCClassRenderer;
		add: DOCMethodRenderer;
		add: DOCVariableRenderer;
		add: DOCTextRenderer;
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
!DOCClassTemplate categoriesFor: #fileNameFor:!document!private! !
!DOCClassTemplate categoriesFor: #initialize!private! !
!DOCClassTemplate categoriesFor: #loadConfig:!parsing!public! !
!DOCClassTemplate categoriesFor: #outDir!accessing!private! !
!DOCClassTemplate categoriesFor: #outDir:!accessing!private! !
!DOCClassTemplate categoriesFor: #parseDocument:!parsing!public! !
!DOCClassTemplate categoriesFor: #possibleChildTypes!public! !
!DOCClassTemplate categoriesFor: #processDoc!document!public! !
!DOCClassTemplate categoriesFor: #renderers!accessing!private! !
!DOCClassTemplate categoriesFor: #renderers:!accessing!private! !
!DOCClassTemplate categoriesFor: #renderize:!document!private! !

!DOCClassTemplate class methodsFor!

processorName
	"The name of the listener used in the XML files"

	^'class'! !
!DOCClassTemplate class categoriesFor: #processorName!private! !

DOCDefaultProcessor guid: (GUID fromString: '{7A50E236-F9B3-4FD3-8E6D-CFE47BFA2387}')!
DOCDefaultProcessor comment: ''!
!DOCDefaultProcessor categoriesForClass!Unclassified! !
!DOCDefaultProcessor methodsFor!

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
					[renderers addLast: (DOCRenderer
								createRenderer: xmlNode
								manager: self manager
								parent: self)]
				ifFalse: [self parseError: 'Tag name "' , xmlNode nodeName , '" in template file not recognized']]!

possibleChildTypes
	"Returns a Set of classes of renderers that can be childs of this type of renderer"

	^(super possibleChildTypes)
		add: DOCTextRenderer;
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
!DOCDefaultProcessor categoriesFor: #initialize!private! !
!DOCDefaultProcessor categoriesFor: #loadConfig:!parsing!public! !
!DOCDefaultProcessor categoriesFor: #parseDocument:!parsing!public! !
!DOCDefaultProcessor categoriesFor: #possibleChildTypes!public! !
!DOCDefaultProcessor categoriesFor: #processDoc!document!public! !
!DOCDefaultProcessor categoriesFor: #renderers!accessing!private! !
!DOCDefaultProcessor categoriesFor: #renderers:!accessing!private! !

!DOCDefaultProcessor class methodsFor!

processorName
	"The name of the listener used in the XML files"

	^'default'! !
!DOCDefaultProcessor class categoriesFor: #processorName!private! !

DOCIndexTemplate guid: (GUID fromString: '{7B81FD24-9ED2-4AA7-A278-317BB6E06F93}')!
DOCIndexTemplate comment: 'This Documenter makes files to have quick access to the main documentation files, such as documentation for classes.

The available index types are:
	- Hierarchy Index: The link for the classes are displayed as a hierarchy of all the classes involved.
	- Alphabetical Index: The links to the classes are displayed as a list of alphabetically ordered names.
	- Package Index: A list of documented packages and links to the pages are included.

It generates a file per index type, the name can be specified with the attribute outFile of the index tag in the index template file. If the attribute is not specified it uses a default name.

The Index layout is parsed from an xml file. The index template also tells wich of the indexes are going to be used'!
!DOCIndexTemplate categoriesForClass!Unclassified! !
!DOCIndexTemplate methodsFor!

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
	renderers add: ((DOCIndexRenderer forManager: self manager)
				parent: self;
				type: indexType;
				parseNode: aNode;
				yourself)!

possibleChildTypes
	"Returns a Set of classes of renderers that can be childs of this type of renderer"

	^(super possibleChildTypes)
		add: DOCIndexRenderer;
		add: DOCTextRenderer;
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
!DOCIndexTemplate categoriesFor: #activeIndexesLinks!public!rendering! !
!DOCIndexTemplate categoriesFor: #addClass:to:!hierarchy!private! !
!DOCIndexTemplate categoriesFor: #addFullClass:!document!public! !
!DOCIndexTemplate categoriesFor: #allClasses!hierarchy!private! !
!DOCIndexTemplate categoriesFor: #classesDo:!hierarchy!private! !
!DOCIndexTemplate categoriesFor: #indexTypeInfo!private! !
!DOCIndexTemplate categoriesFor: #initialize!private! !
!DOCIndexTemplate categoriesFor: #iterateClasses:do:!hierarchy!private! !
!DOCIndexTemplate categoriesFor: #parseDocument:!parsing!public! !
!DOCIndexTemplate categoriesFor: #parseRenderer:type:!parsing!private! !
!DOCIndexTemplate categoriesFor: #possibleChildTypes!public! !
!DOCIndexTemplate categoriesFor: #printClassLinkOf:on:!private!rendering! !
!DOCIndexTemplate categoriesFor: #printPackageLinkOf:on:!private! !
!DOCIndexTemplate categoriesFor: #processDoc!document!public!rendering! !
!DOCIndexTemplate categoriesFor: #renderAlphabeticalIndexOn:!private!rendering! !
!DOCIndexTemplate categoriesFor: #renderers!accessing!private! !
!DOCIndexTemplate categoriesFor: #renderers:!accessing!private! !
!DOCIndexTemplate categoriesFor: #renderHierarchyIndexOn:!private!rendering! !
!DOCIndexTemplate categoriesFor: #renderIndex:on:!public!rendering! !
!DOCIndexTemplate categoriesFor: #renderIndexName:on:!public! !
!DOCIndexTemplate categoriesFor: #renderNode:sepparator:currentSep:on:!private!rendering! !
!DOCIndexTemplate categoriesFor: #renderPackageIndexOn:!private!rendering! !
!DOCIndexTemplate categoriesFor: #sortedCollection!private! !

!DOCIndexTemplate class methodsFor!

processorName
	"The name of the listener used in the XML files"

	^'index'! !
!DOCIndexTemplate class categoriesFor: #processorName!private! !

DOCTemplateSpec guid: (GUID fromString: '{FFF2E958-806F-4656-86F6-8F32DC98A44E}')!
DOCTemplateSpec comment: 'This class handles the main template file ''template.xml''.
It is the main Documenter, It instantiates all the other documenter based on the template file used.

It also handles the copying of extra files that can be used in the pages.'!
!DOCTemplateSpec categoriesForClass!Unclassified! !
!DOCTemplateSpec methodsFor!

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
		addProcessor: (((DOCProcessor processorClassFor: type
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
!DOCTemplateSpec categoriesFor: #addConfig:!parsing!public! !
!DOCTemplateSpec categoriesFor: #addFileNode:!parsing!public! !
!DOCTemplateSpec categoriesFor: #addTemplateNode:!parsing!public! !
!DOCTemplateSpec categoriesFor: #filesToCopy!accessing!private! !
!DOCTemplateSpec categoriesFor: #filesToCopy:!accessing!private! !
!DOCTemplateSpec categoriesFor: #initialize!private! !
!DOCTemplateSpec categoriesFor: #parseDocument:!parsing!public! !
!DOCTemplateSpec categoriesFor: #processDoc!document!public! !

!DOCTemplateSpec class methodsFor!

processorName
	"The name of the listener used in the XML files"

	^'templateSpec'! !
!DOCTemplateSpec class categoriesFor: #processorName!private! !

DOCPackageTemplate guid: (GUID fromString: '{3E42746E-FF22-497D-964F-2258FE110570}')!
DOCPackageTemplate comment: ''!
!DOCPackageTemplate categoriesForClass!Unclassified! !
!DOCPackageTemplate methodsFor!

fileNameFor: anObject
	"Generates the file name that's going to be wrote for this object.

	@param anObject the object
	@returns a string with the file name"

	^self manager packageFileName: anObject!

possibleChildTypes
	"Returns a Set of classes of renderers that can be childs of this type of renderer"

	^(Set new)
		add: DOCPackageRenderer;
		add: DOCTextRenderer;
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
!DOCPackageTemplate categoriesFor: #fileNameFor:!document!private! !
!DOCPackageTemplate categoriesFor: #possibleChildTypes!public! !
!DOCPackageTemplate categoriesFor: #processDoc!document!public! !

!DOCPackageTemplate class methodsFor!

processorName
	"The name of the listener used in the XML files"

	^'package'! !
!DOCPackageTemplate class categoriesFor: #processorName!private! !

DOCCondCmd guid: (GUID fromString: '{88A937F1-D937-4D1F-BF82-3AFD9AFDC30E}')!
DOCCondCmd comment: ''!
!DOCCondCmd categoriesForClass!Unclassified! !
!DOCCondCmd methodsFor!

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
!DOCCondCmd categoriesFor: #condition!accessing!public! !
!DOCCondCmd categoriesFor: #condition:!accessing!public! !
!DOCCondCmd categoriesFor: #execute:on:!public! !
!DOCCondCmd categoriesFor: #falseCommands!accessing!public! !
!DOCCondCmd categoriesFor: #falseCommands:!accessing!public! !
!DOCCondCmd categoriesFor: #initialize!private! !
!DOCCondCmd categoriesFor: #trueCommands!accessing!public! !
!DOCCondCmd categoriesFor: #trueCommands:!accessing!public! !

DOCTextCmd guid: (GUID fromString: '{7A8A3190-C10E-4536-914A-C75332CABFCD}')!
DOCTextCmd comment: ''!
!DOCTextCmd categoriesForClass!Unclassified! !
!DOCTextCmd methodsFor!

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
!DOCTextCmd categoriesFor: #execute:on:!public! !
!DOCTextCmd categoriesFor: #name!accessing!public! !
!DOCTextCmd categoriesFor: #name:!accessing!public! !
!DOCTextCmd categoriesFor: #params!accessing!public! !
!DOCTextCmd categoriesFor: #params:!accessing!public! !

DOCClassTemplateRenderer guid: (GUID fromString: '{7E802F1C-CB2D-4127-AD5B-14F2C1D5036B}')!
DOCClassTemplateRenderer comment: ''!
!DOCClassTemplateRenderer categoriesForClass!Unclassified! !
!DOCClassTemplateRenderer methodsFor!

parseNode: aNode
	"It parses the renderer node and initializes the internal data.

	@param aNode the xml node"

	super parseNode: aNode.
	header := (aNode selectSingleNode: './header') ifNil: [''] ifNotNil: [:titleNode | titleNode text].
	footer := (aNode selectSingleNode: './footer') ifNil: [''] ifNotNil: [:titleNode | titleNode text]! !
!DOCClassTemplateRenderer categoriesFor: #parseNode:!parsing!public! !

!DOCClassTemplateRenderer class methodsFor!

rendererName
	"This is a custom name used to identify the renderer in the xml code, each renderer must have it's own name"

	^nil! !
!DOCClassTemplateRenderer class categoriesFor: #rendererName!public! !

DOCIndexRenderer guid: (GUID fromString: '{7C97BDA5-AE36-4E72-8D3B-ABC8F70DC07B}')!
DOCIndexRenderer comment: ''!
!DOCIndexRenderer categoriesForClass!Unclassified! !
!DOCIndexRenderer methodsFor!

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
!DOCIndexRenderer categoriesFor: #availableCommands!public! !
!DOCIndexRenderer categoriesFor: #fileName!accessing!public! !
!DOCIndexRenderer categoriesFor: #fileName:!accessing!public! !
!DOCIndexRenderer categoriesFor: #parseNode:!public! !
!DOCIndexRenderer categoriesFor: #printClassLinkOf:on:!private! !
!DOCIndexRenderer categoriesFor: #renderIndexBar:params:on:!public! !
!DOCIndexRenderer categoriesFor: #renderIndexBody:params:on:!public! !
!DOCIndexRenderer categoriesFor: #renderIndexName:params:on:!public! !
!DOCIndexRenderer categoriesFor: #type!accessing!public! !
!DOCIndexRenderer categoriesFor: #type:!accessing!public! !

!DOCIndexRenderer class methodsFor!

rendererName
	"This is a custom name used to identify the renderer in the xml code, each renderer must have it's own name"

	^'index'! !
!DOCIndexRenderer class categoriesFor: #rendererName!public! !

DOCPackageRenderer guid: (GUID fromString: '{8C9F976C-3AF6-416F-BA77-55E1B7735951}')!
DOCPackageRenderer comment: ''!
!DOCPackageRenderer categoriesForClass!Unclassified! !
!DOCPackageRenderer methodsFor!

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

	DOCSUtils makeHTMLString: (DOCComment for: aPackage comment) fullComment on: aStream!

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
!DOCPackageRenderer categoriesFor: #availableCommands!public! !
!DOCPackageRenderer categoriesFor: #printClassLinkOf:on:!private! !
!DOCPackageRenderer categoriesFor: #renderPackageClasses:params:on:!public! !
!DOCPackageRenderer categoriesFor: #renderPackageComment:params:on:!public! !
!DOCPackageRenderer categoriesFor: #renderPackageDependencies:params:on:!public! !
!DOCPackageRenderer categoriesFor: #renderPackageDependents:params:on:!public! !
!DOCPackageRenderer categoriesFor: #renderPackageName:params:on:!public! !
!DOCPackageRenderer categoriesFor: #renderPackageTable:tableClass:colClass:on:!private! !

!DOCPackageRenderer class methodsFor!

rendererName
	"This is a custom name used to identify the renderer in the xml code, each renderer must have it's own name"

	^'package'! !
!DOCPackageRenderer class categoriesFor: #rendererName!public! !

DOCTextRenderer guid: (GUID fromString: '{CAD47FF8-F359-48CA-BCBD-D48F6366B016}')!
DOCTextRenderer comment: ''!
!DOCTextRenderer categoriesForClass!Unclassified! !
!DOCTextRenderer methodsFor!

possibleChildTypes
	"Returns a Set of classes of renderers that can be childs of this type of renderer"

	^(super possibleChildTypes)
		add: DOCIndexRenderer;
		add: DOCClassRenderer;
		add: DOCMethodRenderer;
		add: DOCVariableRenderer;
		yourself! !
!DOCTextRenderer categoriesFor: #possibleChildTypes!public! !

!DOCTextRenderer class methodsFor!

rendererName
	"This is a custom name used to identify the renderer in the xml code, each renderer must have it's own name"

	^'text'! !
!DOCTextRenderer class categoriesFor: #rendererName!public! !

DOCClassRenderer guid: (GUID fromString: '{0C4741F2-F128-4994-B58D-A58EB2BEFB28}')!
DOCClassRenderer comment: ''!
!DOCClassRenderer categoriesForClass!Unclassified! !
!DOCClassRenderer methodsFor!

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
		add: DOCClassRenderer;
		add: DOCMethodRenderer;
		add: DOCVariableRenderer;
		yourself!

renderClassComment: aClass params: params on: aStream
	"Renders the class comment.
	
	@param aClass the class
	@param params parameters for the render command
	@param aStream the stream where you are going to render the result"

	params notEmpty ifTrue: [aStream nextPutAll: params first].
	DOCSUtils makeHTMLString: aClass docComment fullComment on: aStream!

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

	aStream nextPutAll: (DOCSUtils makeIDString: aClass name asString)!

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
!DOCClassRenderer categoriesFor: #availableCommands!public! !
!DOCClassRenderer categoriesFor: #possibleChildTypes!public! !
!DOCClassRenderer categoriesFor: #renderClassComment:params:on:!public! !
!DOCClassRenderer categoriesFor: #renderClassDefinition:params:on:!public! !
!DOCClassRenderer categoriesFor: #renderClassHierarchy:params:on:!public! !
!DOCClassRenderer categoriesFor: #renderClassID:params:on:!public! !
!DOCClassRenderer categoriesFor: #renderClassName:params:on:!public! !
!DOCClassRenderer categoriesFor: #renderClassPackage:params:on:!public! !
!DOCClassRenderer categoriesFor: #renderClassSuperclass:params:on:!public! !
!DOCClassRenderer categoriesFor: #renderClassSuperclasses:params:on:!public! !

!DOCClassRenderer class methodsFor!

rendererName
	"This is a custom name used to identify the renderer in the xml code, each renderer must have it's own name"

	^'class'! !
!DOCClassRenderer class categoriesFor: #rendererName!public! !

DOCMethodRenderer guid: (GUID fromString: '{C8C1537F-E0CC-4838-A084-8B97C141CDA6}')!
DOCMethodRenderer comment: ''!
!DOCMethodRenderer categoriesForClass!Unclassified! !
!DOCMethodRenderer methodsFor!

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

	DOCSUtils makeHTMLString: aMethod comment fullComment on: aStream!

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
			name := DOCSUtils firstWordOf: stream.
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

	DOCSUtils makeHTMLString: aMethod comment smallComment on: aStream!

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
!DOCMethodRenderer categoriesFor: #availableCommands!public! !
!DOCMethodRenderer categoriesFor: #getObjectsOf:!private! !
!DOCMethodRenderer categoriesFor: #isTrue:of:!public! !
!DOCMethodRenderer categoriesFor: #methodID:on:!private! !
!DOCMethodRenderer categoriesFor: #parseNode:!public! !
!DOCMethodRenderer categoriesFor: #renderMethodAccess:params:on:!public! !
!DOCMethodRenderer categoriesFor: #renderMethodCategories:params:on:!public! !
!DOCMethodRenderer categoriesFor: #renderMethodClass:params:on:!public! !
!DOCMethodRenderer categoriesFor: #renderMethodID:params:on:!public! !
!DOCMethodRenderer categoriesFor: #renderMethodLargeComment:params:on:!public! !
!DOCMethodRenderer categoriesFor: #renderMethodName:params:on:!public! !
!DOCMethodRenderer categoriesFor: #renderMethodOverride:params:on:!public! !
!DOCMethodRenderer categoriesFor: #renderMethodParams:params:on:!public! !
!DOCMethodRenderer categoriesFor: #renderMethodReturn:params:on:!public! !
!DOCMethodRenderer categoriesFor: #renderMethodSmallComment:params:on:!public! !
!DOCMethodRenderer categoriesFor: #searchOverrideClassOf:!private! !
!DOCMethodRenderer categoriesFor: #setCommentsOf:!private! !

!DOCMethodRenderer class methodsFor!

rendererName
	"This is a custom name used to identify the renderer in the xml code, each renderer must have it's own name"

	^'method'! !
!DOCMethodRenderer class categoriesFor: #rendererName!public! !

DOCVariableRenderer guid: (GUID fromString: '{49B74884-0F99-4FB4-9E50-3555950F3C6A}')!
DOCVariableRenderer comment: ''!
!DOCVariableRenderer categoriesForClass!Unclassified! !
!DOCVariableRenderer methodsFor!

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

	aStream nextPutAll: (DOCSUtils makeIDString: aVariable name displayString)!

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
!DOCVariableRenderer categoriesFor: #availableCommands!public! !
!DOCVariableRenderer categoriesFor: #getObjectsOf:!private! !
!DOCVariableRenderer categoriesFor: #parseNode:!public! !
!DOCVariableRenderer categoriesFor: #renderVarComment:params:on:!public! !
!DOCVariableRenderer categoriesFor: #renderVarID:params:on:!public! !
!DOCVariableRenderer categoriesFor: #renderVarName:params:on:!public! !
!DOCVariableRenderer categoriesFor: #renderVarType:params:on:!public! !

!DOCVariableRenderer class methodsFor!

rendererName
	"This is a custom name used to identify the renderer in the xml code, each renderer must have it's own name"

	^'variable'! !
!DOCVariableRenderer class categoriesFor: #rendererName!public! !

DOCParseError guid: (GUID fromString: '{B4569EAA-ABD3-4562-B3C7-5AF2A2B3221D}')!
DOCParseError comment: ''!
!DOCParseError categoriesForClass!Unclassified! !
"Binary Globals"!

