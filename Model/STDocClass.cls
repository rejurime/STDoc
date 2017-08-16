"Filed out from Dolphin Smalltalk 7"!

STDocObject subclass: #STDocClass
	instanceVariableNames: 'manager classObject classMethods methods variables classVariables instanceClassVariables docComment superClasses subClasses'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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

