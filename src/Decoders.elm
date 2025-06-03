module Decoders exposing (..)

import Iso8601
import Json.Decode as Decode exposing (Decoder, bool, float, int, list, nullable, string)
import Json.Decode.Pipeline exposing (optional, required)
import Types exposing (..)


profileDecoder : Decoder Profile
profileDecoder =
    Decode.succeed Profile
        |> required "first_name" string
        |> required "last_name" string
        |> required "full_name" string
        |> required "email" string
        |> optional "institution" (nullable string) Nothing
        |> optional "is_superuser" (nullable bool) Nothing
        |> optional "is_staff" (nullable bool) Nothing
        |> optional "orcid" (nullable string) Nothing
        |> optional "can_contribute" (nullable bool) Nothing


downloadsDecoder : Decoder Downloads
downloadsDecoder =
    Decode.succeed Downloads
        |> required "count" int
        |> required "next" (nullable string)
        |> required "previous" (nullable string)
        |> required "results" (list downloadTokenDecoder)


downloadTokenDecoder : Decoder DownloadToken
downloadTokenDecoder =
    Decode.succeed DownloadToken
        |> required "id" int
        |> required "token" string
        |> required "n_submissions" int
        |> required "created_by" int
        |> optional "orcid" (nullable string) Nothing
        |> required "created_at" string
        |> required "status" string
        |> required "ticket_type" string
        |> required "simulations" (list string)
        |> required "upload_instances" (list string)


simulationDecoder : Decoder Simulation
simulationDecoder =
    Decode.succeed Simulation
        |> required "id" int
        |> required "md_repo_id" string
        |> required "guid" string
        |> required "slug" string
        |> required "description" (nullable string)
        |> required "external_link" (nullable string)
        |> required "created_by" createdByDecoder
        |> required "creation_date" Iso8601.decoder
        |> required "replicate" (nullable int)
        |> required "total_replicates" (nullable int)
        |> optional "sampling_frequency" (nullable float) Nothing
        |> optional "duration" (nullable float) Nothing
        |> required "integration_timestep_fs" float
        |> required "temperature" int
        |> required "fasta_sequence" (nullable string)
        |> optional "rmsd_values" (list float) []
        |> optional "rmsf_values" (list float) []
        |> required "software" softwareDecoder
        |> required "biomolecules" (list biomoleculeDecoder)
        |> required "unvalidated_biomolecules" (list unvalidatedBiomoleculeDecoder)
        |> required "ligands" (list ligandDecoder)
        |> required "contributions" (list contributionDecoder)
        |> required "solvents" (list solventDecoder)
        |> required "processed_files" (list processedFileDecoder)
        |> required "uploaded_files" (list uploadedFileDecoder)
        |> required "papers" (list paperDecoder)
        |> required "replicate_group" replicateGroupDecoder


paperDecoder : Decoder Paper
paperDecoder =
    Decode.succeed Paper
        |> required "title" string
        |> required "authors" string
        |> required "journal" string
        |> required "volume" int
        |> required "number" (nullable string)
        |> required "year" int
        |> required "pages" (nullable string)
        |> required "doi" string


createdByDecoder : Decoder CreatedBy
createdByDecoder =
    Decode.succeed CreatedBy
        |> optional "first_name" (nullable string) Nothing
        |> optional "last_name" (nullable string) Nothing
        |> optional "full_name" (nullable string) Nothing
        |> optional "email" (nullable string) Nothing
        |> optional "institution" (nullable string) Nothing
        |> optional "is_superuser" (nullable bool) Nothing
        |> optional "is_staff" (nullable bool) Nothing
        |> optional "orcid" (nullable string) Nothing
        |> optional "can_contribute" (nullable bool) Nothing


contributionDecoder : Decoder Contribution
contributionDecoder =
    Decode.succeed Contribution
        |> required "name" string
        |> required "orcid" (nullable string)
        |> required "email" (nullable string)
        |> required "institution" (nullable string)


processedFileDecoder : Decoder ProcessedFile
processedFileDecoder =
    Decode.succeed ProcessedFile
        |> required "id" int
        |> required "is_primary" bool
        |> required "file_type" string
        |> required "local_filename" string
        |> required "local_file_path" string
        |> required "description" (nullable string)
        |> required "file_size_bytes" (nullable int)
        |> required "public" bool


uploadedFileDecoder : Decoder UploadedFile
uploadedFileDecoder =
    Decode.succeed UploadedFile
        |> required "id" int
        |> required "primary" bool
        |> required "filename" string
        |> required "file_type" string
        |> required "description" (nullable string)
        |> required "file_size_bytes" (nullable int)


solventDecoder : Decoder Solvent
solventDecoder =
    Decode.succeed Solvent
        |> required "name" string
        |> required "concentration" float
        |> required "concentration_units" string


softwareDecoder : Decoder SimulationSoftware
softwareDecoder =
    Decode.succeed SimulationSoftware
        |> required "name" string
        |> required "version" (nullable string)


ligandDecoder : Decoder Ligand
ligandDecoder =
    Decode.succeed Ligand
        |> required "name" string
        |> required "smiles_string" (nullable string)


replicateGroupDecoder : Decoder ReplicateGroup
replicateGroupDecoder =
    Decode.succeed ReplicateGroup
        |> optional "psf_hash" (nullable string) Nothing
        |> optional "simulation_set" (list string) []


biomoleculeDecoder : Decoder Biomolecule
biomoleculeDecoder =
    Decode.succeed Biomolecule
        |> required "name" string
        |> required "primary_molecule_id_type" string
        |> required "amino_length" int
        |> required "sequence" (nullable string)
        |> required "uniprot_id" (nullable string)
        |> required "pdb_id" (nullable string)


unvalidatedBiomoleculeDecoder : Decoder UnvalidatedBiomolecule
unvalidatedBiomoleculeDecoder =
    Decode.succeed UnvalidatedBiomolecule
        |> required "molecule_id" string
        |> required "molecule_id_type" (nullable string)


downloadInstanceDecoder : Decoder DownloadInstance
downloadInstanceDecoder =
    Decode.succeed DownloadInstance
        |> required "download_instance_id" int


uploadTicketsResultDecoder : Decoder UploadTicketsResult
uploadTicketsResultDecoder =
    Decode.succeed UploadTicketsResult
        |> required "count" int
        |> required "next" (nullable string)
        |> required "previous" (nullable string)
        |> required "results" (list uploadTicketDecoder)


uploadTicketDecoder : Decoder UploadTicket
uploadTicketDecoder =
    Decode.succeed UploadTicket
        |> required "id" int
        |> required "token" string
        |> required "n_submissions" int
        |> required "created_by" int
        |> optional "orcid" (nullable string) Nothing
        |> required "created_at" string
        |> required "status" string
        |> required "ticket_type" string
        |> required "simulations" (list uploadTicketSimulationDecoder)
        |> required "upload_instances" (list uploadTicketInstanceDecoder)


uploadTicketSimulationDecoder : Decoder UploadTicketSimulation
uploadTicketSimulationDecoder =
    Decode.succeed UploadTicketSimulation
        |> required "md_repo_id" string
        |> required "guid" string
        |> required "description" (nullable string)
        |> required "slug" string
        |> required "is_placeholder" bool
        |> required "is_deprecated" bool
        |> required "creationDate" string
        |> required "ticketType" string
        |> required "isSuccessful" bool
        |> required "lead_contributor_orcid" string
        |> required "filenames" string
        |> required "uploaded_files" (list uploadTicketFileDecoder)


uploadTicketFileDecoder : Decoder UploadTicketFile
uploadTicketFileDecoder =
    Decode.succeed UploadTicketFile
        |> required "id" int
        |> required "primary" bool
        |> required "fileName" string
        |> required "fileType" string
        |> required "description" string
        |> required "file_size_bytes" int


uploadTicketInstanceDecoder : Decoder UploadTicketInstance
uploadTicketInstanceDecoder =
    Decode.succeed UploadTicketInstance
        |> required "id" int
        |> required "user" profileDecoder
        |> required "ticket" int
        |> required "createdOn" string
        |> required "simulation" (list uploadTicketInstanceSimulationDecoder)
        |> required "status_messages" (list uploadInstanceStatusMessageDecoder)


uploadTicketInstanceSimulationDecoder : Decoder UploadTicketInstanceSimulation
uploadTicketInstanceSimulationDecoder =
    Decode.succeed UploadTicketInstanceSimulation
        |> required "md_repo_id" string
        |> required "guid" string
        |> required "description" (nullable string)
        |> required "slug" string
        |> required "is_placeholder" bool
        |> required "is_deprecated" bool
        |> required "creation_date" string
        |> required "uploaded_files" (list uploadTicketFileDecoder)


uploadInstanceStatusMessageDecoder : Decoder UploadInstanceStatusMessage
uploadInstanceStatusMessageDecoder =
    Decode.succeed UploadInstanceStatusMessage
        |> required "timestamp" string
        |> required "message" string
        |> required "is_error" bool
        |> required "is_warning" bool
