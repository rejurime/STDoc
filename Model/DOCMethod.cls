"Filed out from Dolphin Smalltalk 7"!

DOCObject subclass: #DOCMethod
	instanceVariableNames: 'comment methodObject definition id'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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

