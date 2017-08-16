"Filed out from Dolphin Smalltalk 7"!

STDocObject subclass: #STDocSmalltalkManager
	instanceVariableNames: 'config processors configKeys progress defaultStyle privIcon pubIcon overIcon classIcon blankIcon classes stClassSet packages'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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

