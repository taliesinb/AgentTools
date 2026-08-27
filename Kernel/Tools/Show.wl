(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`AgentTools`Tools`Show`" ];
Begin[ "`Private`" ];

Needs[ "Wolfram`AgentTools`"        ];
Needs[ "Wolfram`AgentTools`Common`" ];
Needs[ "Wolfram`AgentTools`Tools`"  ];

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Config*)
(* Retina resolution: twice the 72 dpi default. *)
$imageResolution = 144;
$showDirectoryName = "AgentToolsShow";

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Prompt*)
$showToolDescription = "\
Renders a Wolfram Language expression to a PNG image at Retina resolution and displays it inline in the \
session transcript.
Use this to show the user a plot, graphic, grid, typeset expression, or any visual result.

The image appears directly in the transcript: a companion extension matches this tool's JSON output and \
renders the file inline, so there is no separate window to open. The expression is evaluated, rasterized, and \
written to a timestamped file whose name ends in an @Nx retina suffix (e.g. @2x for 144 dpi). Returns a JSON \
object with the file path and image metadata.";

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Tool Definition*)
(* Add to $defaultMCPTools Association (initialized in Kernel/Tools/Tools.wl) *)
$defaultMCPTools[ "Show" ] := LLMTool @ <|
    "Name"        -> "Show",
    "DisplayName" -> "Show",
    "Description" -> $showToolDescription,
    "Function"    -> showExpression,
    "Options"     -> { },
    "Parameters"  -> {
        "expression" -> <|
            "Interpreter" -> "String",
            "Help"        -> "The Wolfram Language expression to render and display (e.g. Plot[Sin[x], {x, 0, 2 Pi}]).",
            "Required"    -> True
        |>
    }
|>;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Main Entry Point*)

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*showExpression*)
(* Runs in the server kernel (not the sandboxed evaluator kernel) so file writes
   are unrestricted. *)
showExpression // beginDefinition;

showExpression[ KeyValuePattern[ "expression" -> code_String ] ] := Enclose[
    Module[ { held, image, bytes, hash, file, dims, scale },
        held  = ConfirmMatch[ Quiet @ ToExpression[ code, InputForm, HoldComplete ], HoldComplete[ _ ], "Parse" ];
        image = ConfirmMatch[ Rasterize[ ReleaseHold @ held, ImageResolution -> $imageResolution ], _Image, "Rasterize" ];
        bytes = ConfirmMatch[ ExportByteArray[ image, "PNG" ], _ByteArray, "Export" ];
        (* Short content tag: 8 hex chars of MD5. Not for security, just a
           compact filename disambiguator. *)
        hash  = ConfirmBy[ StringTake[ Hash[ bytes, "MD5", "HexString" ], 8 ], StringQ, "Hash" ];
        file  = ConfirmBy[ writeImageFile[ hash, bytes ], StringQ, "Write" ];
        ConfirmAssert[ FileExistsQ @ file, "FileExists" ];
        dims  = ImageDimensions @ image;
        scale = $imageResolution / 72; (* 144 dpi -> 2 *)
        (* Emit a single JSON object describing the rendered image. A client
           extension can match { "type": "image", "path": ... } and display it
           inline; other clients read the metadata directly. *)
        ConfirmBy[
            Developer`WriteRawJSONString @ <|
                "type"         -> "image",
                "source"       -> "WolframShow",
                "path"         -> file,
                "format"       -> "PNG",
                (* Actual pixels in the PNG (rendered at 144 dpi, i.e. 2x). *)
                "devicePixels" -> <| "width" -> First @ dims, "height" -> Last @ dims |>,
                (* Logical size in points: devicePixels / scale. *)
                "points"       -> <| "width" -> Round[ First @ dims / scale ], "height" -> Round[ Last @ dims / scale ] |>,
                "resolution"   -> $imageResolution,
                "scale"        -> scale,
                "bytes"        -> Length @ bytes
            |>,
            StringQ,
            "JSON"
        ]
    ],
    throwInternalFailure
];

showExpression // endDefinition;

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*writeImageFile*)
writeImageFile // beginDefinition;

writeImageFile[ hash_String, bytes_ByteArray ] := Enclose[
    Module[ { dir, timestamp, scale, name, file, stream },
        dir = FileNameJoin @ { $UserBaseDirectory, $showDirectoryName };
        If[ ! DirectoryQ @ dir, ConfirmBy[ CreateDirectory[ dir, CreateIntermediateDirectories -> True ], DirectoryQ, "CreateDirectory" ] ];
        timestamp = DateString[ { "Year", "-", "Month", "-", "Day", "-", "Hour", "-", "Minute", "-", "Second" } ];
        scale = $imageResolution / 72; (* 144 dpi -> 2, i.e. @2x *)
        name = timestamp <> "-" <> hash <> "@" <> ToString @ scale <> "x.png";
        file = FileNameJoin @ { dir, name };
        If[ ! FileExistsQ @ file,
            stream = ConfirmMatch[ OpenWrite[ file, BinaryFormat -> True ], _OutputStream, "OpenWrite" ];
            WithCleanup[ BinaryWrite[ stream, bytes ], Close @ stream ]
        ];
        file
    ],
    throwInternalFailure
];

writeImageFile // endDefinition;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];
