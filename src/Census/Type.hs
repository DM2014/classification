module Census.Type where

data WorkClass = Private | SelfEmpNotInc | SelfEmpInc 
               | FederalGov | LocalGov | StateGov 
               | WithoutPay | NeverWorked
               deriving (Show)

data Education = Bachelors | SomeCollege | Eleventh | HSGrad | ProfSchool
               | AssocAcdm | AssocVoc | Nineth | SeventhToEighth
               | Twelfth | Masters | FirstToFourth | Tenth | Doctorate
               | FifthToSixth | Preschool
               deriving (Show)

data MaritalStatus = MarriedCivSpouse | Divorced | NeverMarried | Separated
                   | Widowed | MarriedSpouseAbsent | MarriedAFSpouse 
                   deriving (Show)

data Occupation = TechSupport | CraftRepair | OtherService | Sales
                | ExecManagerial | ProfSpecialty | HandlersCleaners
                | MachineOpInspct | AdmClerical | FarmingFishing
                | TransportMoving | PrivHouseServ | ProtectiveServ
                | ArmedForces
                deriving (Show)

data Relationship = Wife | OwnChild | Husband 
                  | NotInFamily | OtherRelative | Unmarried
                  deriving (Show)

data Race = White | AsianPacIslander | AmerIndianEskimo | Other | Black
          deriving (Show)

data Sex = Female | Male deriving (Show)

data NativeCountry = UnitedStates | Cambodia | England | PuertoRico | Canada
                   | Germany | OutlyingUS | India | Japan | Greece | South
                   | China | Cuba | Iran | Honduras | Philippines | Italy
                   | Poland | Jamaica | Vietnam | Mexico | Protugal | Ireland
                   | France | DominicanRepublic | Laos | Ecuador | Taiwan 
                   | Haiti | Columbia | Hungary | Guatemala | Nicaragua 
                   | Scotland | Thailand | Yugoslavia | ElSalvador 
                   | TrinadadTobago | Peru | Hong | Holand

data Adult = Adult 
    {   adultAge :: Int
    ,   adultWorkclass :: WorkClass
    ,   adultFinalWeight :: Int
    ,   adultEducation :: Education
    ,   adultEducationNum :: Int
    ,   adultMaritalStatus :: MaritalStatus
    ,   adultOccupation :: Occupation
    ,   adultRelationship :: Relationship
    ,   adultRace :: Race
    ,   adultSex :: Sex
    ,   adultCapitalGain :: Int
    ,   adultCapitalLoss :: Int
    ,   adultHoursPerWeek :: Int
    ,   adultNativeCountry :: NativeCountry
    }