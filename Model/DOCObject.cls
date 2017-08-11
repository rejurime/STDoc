"Filed out from Dolphin Smalltalk 7"!

Object subclass: #DOCObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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

