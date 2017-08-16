"Filed out from Dolphin Smalltalk 7"!

Object subclass: #STDocObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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

