{-# LANGUAGE OverloadedStrings #-}

module Census.Parser (parser) where

import              Census.Type

import              Control.Monad.Trans.Resource
import              Control.Monad                       (void)
import              Control.Monad.Trans.Maybe
import              Control.Monad.Trans.Class

import              Data.Attoparsec.ByteString
import              Data.Attoparsec.ByteString.Char8 (decimal)
import              Data.ByteString (ByteString)
import              Data.Conduit
import              Data.Conduit.Attoparsec
import              Prelude hiding (take)

parser :: Conduit ByteString (ResourceT IO) (Maybe Labeled)
parser = do
    conduitParserEither (choice [parseEmptyLine, parseLabeledAdult]) =$= awaitForever go
    where   go (Left s) = error $ show s
            go (Right (_, p)) = yield p

missing :: Parser (Maybe a)
missing = string "?" >> return Nothing

match :: ByteString -> a -> Parser (Maybe a)
match s c = string s >> return (Just c)

parseContinuous :: MaybeT Parser Int
parseContinuous = MaybeT $ choice [decimal >>= return . Just, missing]

parseWorkClass :: MaybeT Parser WorkClass
parseWorkClass = MaybeT $ choice
    [   match "Private"             Private
    ,   match "Local-gov"           LocalGov
    ,   match "State-gov"           StateGov
    ,   match "Federal-gov"         FederalGov
    ,   match "Without-pay"         WithoutPay
    ,   match "Self-emp-inc"        SelfEmpInc
    ,   match "Never-worked"        NeverWorked
    ,   match "Self-emp-not-inc"    SelfEmpNotInc
    ,   missing
    ]

parseEducation :: MaybeT Parser Education
parseEducation = MaybeT $ choice
    [   match "9th"             Nineth
    ,   match "10th"            Tenth
    ,   match "11th"            Eleventh
    ,   match "12th"            Twelfth
    ,   match "HS-grad"         HSGrad
    ,   match "7th-8th"         SeventhToEighth
    ,   match "Masters"         Masters
    ,   match "1st-4th"         FirstToFourth
    ,   match "5th-6th"         FifthToSixth
    ,   match "Bachelors"       Bachelors
    ,   match "Assoc-voc"       AssocVoc
    ,   match "Doctorate"       Doctorate
    ,   match "Preschool"       Preschool
    ,   match "Assoc-acdm"      AssocAcdm
    ,   match "Prof-school"     ProfSchool
    ,   match "Some-college"    SomeCollege
    ,   missing
    ]

parseMaritalStatus :: MaybeT Parser MaritalStatus
parseMaritalStatus = MaybeT $ choice
    [   match "Widowed"                 MarriedCivSpouse
    ,   match "Divorced"                Divorced
    ,   match "Separated"               NeverMarried
    ,   match "Never-married"           Separated
    ,   match "Married-AF-spouse"       Widowed
    ,   match "Married-civ-spouse"      MarriedSpouseAbsent
    ,   match "Married-spouse-absent"   MarriedAFSpouse
    ,   missing
    ]

parseOccupation :: MaybeT Parser Occupation
parseOccupation = MaybeT $ choice
    [   match "Sales"               Sales
    ,   match "Tech-support"        TechSupport
    ,   match "Armed-Forces"        ArmedForces
    ,   match "Craft-repair"        CraftRepair
    ,   match "Adm-clerical"        AdmClerical
    ,   match "Other-service"       OtherService
    ,   match "Farming-fishing"     FarmingFishing
    ,   match "Priv-house-serv"     PrivHouseServ
    ,   match "Prof-specialty"      ProfSpecialty
    ,   match "Protective-serv"     ProtectiveServ
    ,   match "Exec-managerial"     ExecManagerial
    ,   match "Transport-moving"    TransportMoving
    ,   match "Handlers-cleaners"   HandlersCleaners
    ,   match "Machine-op-inspct"   MachineOpInspct
    ,   missing
    ]

parseRelationship :: MaybeT Parser Relationship
parseRelationship = MaybeT $ choice
    [   match "Wife"            Wife
    ,   match "Own-child"       OwnChild
    ,   match "Husband"         Husband
    ,   match "Not-in-family"   NotInFamily
    ,   match "Other-relative"  OtherRelative
    ,   match "Unmarried"       Unmarried
    ,   missing
    ]

parseRace :: MaybeT Parser Race
parseRace = MaybeT $ choice
    [   match "White"               White
    ,   match "Asian-Pac-Islander"  AsianPacIslander
    ,   match "Amer-Indian-Eskimo"  AmerIndianEskimo
    ,   match "Other"               Other
    ,   match "Black"               Black
    ,   missing
    ]

parseSex :: MaybeT Parser Sex
parseSex = MaybeT $ choice
    [   match "Female"  Female
    ,   match "Male"    Male
    ,   missing
    ]

parseNativeCountry :: MaybeT Parser NativeCountry
parseNativeCountry = MaybeT $ choice
    [   match "United-States"               UnitedStates
    ,   match "Cambodia"                    Cambodia
    ,   match "England"                     England
    ,   match "Puerto-Rico"                 PuertoRico
    ,   match "Canada"                      Canada
    ,   match "Germany"                     Germany
    ,   match "Outlying-US(Guam-USVI-etc)"  OutlyingUS
    ,   match "India"                       India
    ,   match "Japan"                       Japan
    ,   match "Greece"                      Greece
    ,   match "South"                       South
    ,   match "China"                       China
    ,   match "Cuba"                        Cuba
    ,   match "Iran"                        Iran
    ,   match "Honduras"                    Honduras
    ,   match "Philippines"                 Philippines
    ,   match "Italy"                       Italy
    ,   match "Poland"                      Poland
    ,   match "Jamaica"                     Jamaica
    ,   match "Vietnam"                     Vietnam
    ,   match "Mexico"                      Mexico
    ,   match "Portugal"                    Portugal
    ,   match "Ireland"                     Ireland
    ,   match "France"                      France
    ,   match "Dominican-Republic"          DominicanRepublic
    ,   match "Laos"                        Laos
    ,   match "Ecuador"                     Ecuador
    ,   match "Taiwan"                      Taiwan
    ,   match "Haiti"                       Haiti
    ,   match "Columbia"                    Columbia
    ,   match "Hungary"                     Hungary
    ,   match "Guatemala"                   Guatemala
    ,   match "Nicaragua"                   Nicaragua
    ,   match "Scotland"                    Scotland
    ,   match "Thailand"                    Thailand
    ,   match "Yugoslavia"                  Yugoslavia
    ,   match "El-Salvador"                 ElSalvador
    ,   match "Trinadad&Tobago"             TrinadadTobago
    ,   match "Peru"                        Peru
    ,   match "Hong"                        Hong
    ,   match "Holand-Netherlands"          Holand
    ,   missing
    ]

parseLabel :: MaybeT Parser Label
parseLabel = MaybeT $ choice
    [   match "<=50K"                       False
    ,   match ">50K"                        True
    ,   missing
    ]

parseDelimeter :: MaybeT Parser ()
parseDelimeter = lift $ void (string ", ")

parseEmptyLine :: Parser (Maybe Labeled)
parseEmptyLine = string "\n" >> return Nothing

parseSkip :: Parser ()
parseSkip = skipWhile (/= 0xa) >> take 1 >> return ()

parseAdult :: MaybeT Parser Adult
parseAdult = do
    age <- parseContinuous
    parseDelimeter
    workClass <- parseWorkClass
    parseDelimeter
    finalWeight <- parseContinuous
    parseDelimeter
    education <- parseEducation
    parseDelimeter
    educationNum <- parseContinuous
    parseDelimeter
    maritalStatus <- parseMaritalStatus
    parseDelimeter
    occupation <- parseOccupation
    parseDelimeter
    relationship <- parseRelationship
    parseDelimeter
    race <- parseRace
    parseDelimeter
    sex <- parseSex
    parseDelimeter
    capitalGain <- parseContinuous
    parseDelimeter
    capitalLoss <- parseContinuous
    parseDelimeter
    hoursPerWeek <- parseContinuous
    parseDelimeter
    nativeCountry <- parseNativeCountry
    return $ Adult age workClass finalWeight education educationNum maritalStatus occupation relationship race sex capitalGain capitalLoss hoursPerWeek nativeCountry


parseLabeledAdult :: Parser (Maybe Labeled)
parseLabeledAdult = do
    result <- runMaybeT $ do 
        adult <- parseAdult
        parseDelimeter
        label <- parseLabel
        return $ Labeled adult label
    parseSkip
    return result