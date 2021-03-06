"Filed out from Dolphin Smalltalk 7"!

STDocRenderer subclass: #STDocPackageRenderer
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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
	"Genera una tabla de paquetes que contiene todos aquellos paquetes que dependen del paquete indicado por par�metro.
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
	"Private - Renderiza una tabla con los paquetes indicados en la colecci�n y con el estilo indicado en tableClass y colClass

	@param somePackages una colecci�n de paquetes Smalltalk
	@param tableClass el estilo de la tabla a renderizar
	@param colClass el estilo de las celdas a renderizar
	@param aStream el stream donde se debe renderizar la tabla"

	somePackages isEmpty
		ifTrue: [aStream nextPutAll: '<span class="disabled">Ningun paquete entra en esta categor�a</span>']
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

