"Filed out from Dolphin Smalltalk 7"!

DOCObject subclass: #DOCClass
	instanceVariableNames: 'manager classObject classMethods methods variables classVariables instanceClassVariables docComment superClasses subClasses'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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

