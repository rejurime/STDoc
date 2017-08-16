"Filed out from Dolphin Smalltalk 7"!

STDocClassTemplate subclass: #STDocPackageTemplate
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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

