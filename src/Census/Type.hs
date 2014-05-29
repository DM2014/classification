module Census.Type where

data WorkClass = Private | SelfEmpNotInc | SelfEmpInc 
               | FederalGov | LocalGov | StateGov 
               | WithoutPay | NeverWorked
               deriving (Show, Eq)

data Education = Bachelors | SomeCollege | Eleventh | HSGrad | ProfSchool
               | AssocAcdm | AssocVoc | Nineth | SeventhToEighth
               | Twelfth | Masters | FirstToFourth | Tenth | Doctorate
               | FifthToSixth | Preschool
               deriving (Show, Eq)

data MaritalStatus = MarriedCivSpouse | Divorced | NeverMarried | Separated
                   | Widowed | MarriedSpouseAbsent | MarriedAFSpouse 
                   deriving (Show, Eq)

data Occupation = TechSupport | CraftRepair | OtherService | Sales
                | ExecManagerial | ProfSpecialty | HandlersCleaners
                | MachineOpInspct | AdmClerical | FarmingFishing
                | TransportMoving | PrivHouseServ | ProtectiveServ
                | ArmedForces
                deriving (Show, Eq)

data Relationship = Wife | OwnChild | Husband 
                  | NotInFamily | OtherRelative | Unmarried
                  deriving (Show, Eq)

data Race = White | AsianPacIslander | AmerIndianEskimo | Other | Black
          deriving (Show, Eq)

data Sex = Female | Male deriving (Show, Eq)

data NativeCountry = UnitedStates | Cambodia | England | PuertoRico | Canada
                   | Germany | OutlyingUS | India | Japan | Greece | South
                   | China | Cuba | Iran | Honduras | Philippines | Italy
                   | Poland | Jamaica | Vietnam | Mexico | Portugal | Ireland
                   | France | DominicanRepublic | Laos | Ecuador | Taiwan 
                   | Haiti | Columbia | Hungary | Guatemala | Nicaragua 
                   | Scotland | Thailand | Yugoslavia | ElSalvador 
                   | TrinadadTobago | Peru | Hong | Holand
                   deriving (Show, Eq)

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
    deriving (Show, Eq)

type Label = Bool
type DataPoint = Adult
data Labeled = Labeled DataPoint Label deriving (Show, Eq)
data Result = TruePositive
            | TrueNegative
            | FalsePositive
            | FalseNegative
            deriving (Show, Eq)