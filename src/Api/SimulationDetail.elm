module Api.SimulationDetail exposing (Simulation, get)

import Http
import Json.Decode


type alias Simulation =
    { id : Int
    }



{-
   type alias Simulation =
       { id : String
       , mdRepoId : String
       , guid : String
       , isPlaceholder : Bool
       , isRestricted : Bool
       , description : String
       , shortDescription : String
       , externalLink : Maybe String
       , includesWater : Bool
       , runCommands : Maybe String
       , waterType : String
       , waterDensity : Maybe Float
       , duration : Float
       , samplingFrequence : Float
       , integrationTimestepFs : Float
       , software : SimulationSoftware
       , ligands : List String
       , contributions : List String
       , biomolecules : List Biomolecule
       , unvalidatedBiomolecules : List Biomolecule
       , solvents : List String
       , papers : List String
       , slug : String
       , rmsdValues : List Float
       , rmsfValues : List Float
       , createdBy : CreatedBy
       , replicate : Int
       , totalReplicates : Int
       , creationDate : String
       , replicateGroup : ReplicateGroup
       , forceField : Maybe String
       , forceFieldComments : Maybe String
       , uploadedFiles : List UploadedFile
       , processedFiles : List ProcessedFile
       , displayTrajectoryFile : ProcessedFile
       , displayStructureFile : ProcessedFile
       , fastaSequence : String
       , temperature : Int
       , protonationMethod : Maybe String
       , displayTrajectoryFileNFrames : Int
       , canEdit : Bool
       }


   type alias SimulationSoftware =
       { name : String
       , version : Maybe String
       }


   type alias Biomolecule =
       { moleculeId : String
       , moleculeIdType : String
       }


   type alias CreatedBy =
       { firstName : String
       , lastName : String
       , fullName : String
       , email : String
       , institution : Maybe String
       , isSuperuser : Bool
       , isStaff : Bool
       , orcid : Maybe String
       , canContribute : Bool
       }


   type alias ReplicateGroup =
       { psfHash : String
       , simulationSet : List String
       }



   type alias UploadedFile =
       { id : String
       , primary : Bool
       , filename : String
       , fileType : String
       , description : String
       , fileSizeBytes : Int
       }


   type alias ProcessedFile =
       { id : String
       , isPrimary : Bool
       , fileType : String
       , localFilename : String
       , localFilePath : String
       , description : String
       , fileSizeBytes : Int
       , public : Bool
       }
-}


get :
    { id : String
    , onResponse : Result Http.Error Simulation -> msg
    }
    -> Cmd msg
get options =
    Http.get
        { url = "http://localhost:8000/api/v1/getSimulations/" ++ options.id
        , expect = Http.expectJson options.onResponse decoder
        }



-- JSON DECODERS


decoder : Json.Decode.Decoder Simulation
decoder =
    Json.Decode.map Simulation
        (Json.Decode.field "id" Json.Decode.int)
