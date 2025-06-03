module Types exposing (..)

import Json.Encode exposing (int)
import Time


type alias Profile =
    { firstName : String
    , lastName : String
    , fullName : String
    , email : String
    , institution : Maybe String
    , isSuperuser : Maybe Bool
    , isStaff : Maybe Bool
    , orcid : Maybe String
    , canContribute : Maybe Bool
    }


type alias Downloads =
    { count : Int
    , next : Maybe String
    , previous : Maybe String
    , results : List DownloadToken
    }


type alias DownloadToken =
    { id : Int
    , token : String
    , nSubmissions : Int
    , createdBy : Int
    , orcid : Maybe String
    , createdAt : String
    , status : String
    , ticketType : String
    , simulations : List String
    , uploadInstances : List String
    }


type alias DownloadInstance =
    { downloadInstanceId : Int }


type alias Simulation =
    { id : Int
    , mdRepoId : String
    , guid : String
    , slug : String
    , description : Maybe String
    , externalLink : Maybe String
    , createdBy : CreatedBy
    , creationDate : Time.Posix
    , replicate : Maybe Int
    , totalReplicates : Maybe Int
    , samplingFrequency : Maybe Float
    , duration : Maybe Float
    , integrationTimestepFs : Float
    , temperature : Int
    , fastaSequence : Maybe String
    , rmsdValues : List Float
    , rmsfValues : List Float
    , software : SimulationSoftware
    , biomolecules : List Biomolecule
    , unvalidatedBiomolecules : List UnvalidatedBiomolecule
    , ligands : List Ligand
    , contributions : List Contribution
    , solvents : List Solvent
    , processedFiles : List ProcessedFile
    , uploadedFiles : List UploadedFile
    , papers : List Paper
    , replicateGroup : ReplicateGroup
    }


type alias SimulationSoftware =
    { name : String
    , version : Maybe String
    }


type alias Solvent =
    { name : String
    , concentration : Float
    , concentrationUnits : String
    }


type alias Contribution =
    { name : String
    , orcid : Maybe String
    , email : Maybe String
    , institution : Maybe String
    }


type alias Biomolecule =
    { name : String
    , primaryMoleculeIdType : String
    , aminoLength : Int
    , sequence : Maybe String
    , uniprotId : Maybe String
    , pdbId : Maybe String
    }


type alias UnvalidatedBiomolecule =
    { moleculeId : String
    , moleculeIdType : Maybe String
    }


type alias Ligand =
    { name : String
    , smilesString : Maybe String
    }


type alias CreatedBy =
    { firstName : Maybe String
    , lastName : Maybe String
    , fullName : Maybe String
    , email : Maybe String
    , institution : Maybe String
    , isSuperuser : Maybe Bool
    , isStaff : Maybe Bool
    , orcid : Maybe String
    , canContribute : Maybe Bool
    }


type alias ProcessedFile =
    { id : Int
    , isPrimary : Bool
    , fileType : String
    , localFileName : String
    , localFilePath : String
    , description : Maybe String
    , fileSizeBytes : Maybe Int
    , isPublic : Bool
    }


type alias UploadedFile =
    { id : Int
    , primary : Bool
    , filename : String
    , fileType : String
    , description : Maybe String
    , fileSizeBytes : Maybe Int
    }


type alias Paper =
    { title : String
    , authors : String
    , journal : String
    , volume : Int
    , number : Maybe String
    , year : Int
    , pages : Maybe String
    , doi : String
    }


type alias ReplicateGroup =
    { psfHash : Maybe String
    , simulationSet : List String
    }


type alias UploadTicketsResult =
    { count : Int
    , next : Maybe String
    , previous : Maybe String
    , results : List UploadTicket
    }


type alias UploadTicket =
    { id : Int
    , token : String
    , nSubmissions : Int
    , createdBy : Int
    , orcid : Maybe String
    , createdAt : String
    , status : String
    , ticketType : String
    , simulations : List UploadTicketSimulation
    , uploadInstances : List UploadTicketInstance
    }


type alias UploadTicketSimulation =
    { mdRepoId : String
    , guid : String
    , description : Maybe String
    , slug : String
    , isPlaceholder : Bool
    , isDeprecated : Bool
    , creationDate : String
    , ticketType : String
    , isSuccessful : Bool
    , leadContributorOrcid : String
    , filenames : String
    , uploadedFiles : List UploadTicketFile
    }


type alias UploadTicketFile =
    { id : Int
    , isPrimary : Bool
    , fileName : String
    , fileType : String
    , description : String
    , fileSizeBytes : Int
    }


type alias UploadTicketInstance =
    { id : Int
    , user : Profile
    , ticket : Int
    , createdOn : String
    , simulation : List UploadTicketInstanceSimulation
    , statusMessages : List UploadInstanceStatusMessage
    }


type alias UploadTicketInstanceSimulation =
    { mdRepoId : String
    , guid : String
    , description : Maybe String
    , slug : String
    , isPlaceholder : Bool
    , isDeprecated : Bool
    , creationDate : String
    , uploadedFiles : List UploadTicketFile
    }


type alias UploadInstanceStatusMessage =
    { timestamp : String
    , message : String
    , isError : Bool
    , isWarning : Bool
    }
