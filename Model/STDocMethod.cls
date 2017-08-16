"Filed out from Dolphin Smalltalk 7"!

STDocObject subclass: #STDocMethod
	instanceVariableNames: 'comment methodObject definition id'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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

