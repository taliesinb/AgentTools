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
Renders a Wolfram Language expression to a PNG image at Retina resolution and opens it on the user's screen.
Use this to show the user a plot, graphic, grid, typeset expression, or any visual result.

The expression is evaluated, rasterized, written to a file named by the hex hash of the PNG contents, and opened \
with the system image viewer. Returns the file path and pixel dimensions.";

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
   and RunProcess["open", ...] are unrestricted. *)
showExpression // beginDefinition;

showExpression[ KeyValuePattern[ "expression" -> code_String ] ] := Enclose[
    Module[ { held, image, bytes, hash, file, dims },
        held  = ConfirmMatch[ Quiet @ ToExpression[ code, InputForm, HoldComplete ], HoldComplete[ _ ], "Parse" ];
        image = ConfirmMatch[ Rasterize[ ReleaseHold @ held, ImageResolution -> $imageResolution ], _Image, "Rasterize" ];
        bytes = ConfirmMatch[ ExportByteArray[ image, "PNG" ], _ByteArray, "Export" ];
        hash  = ConfirmBy[ Hash[ bytes, "SHA256", "HexString" ], StringQ, "Hash" ];
        file  = ConfirmBy[ writeImageFile[ hash, bytes ], StringQ, "Write" ];
        ConfirmAssert[ FileExistsQ @ file, "FileExists" ];
        ConfirmMatch[ openImageFile @ file, Except[ _Failure ], "Open" ];
        dims  = ImageDimensions @ image;
        "Displayed image (" <> ToString @ First @ dims <> "x" <> ToString @ Last @ dims <> " px): " <> file
    ],
    throwInternalFailure
];

showExpression // endDefinition;

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*writeImageFile*)
writeImageFile // beginDefinition;

writeImageFile[ hash_String, bytes_ByteArray ] := Enclose[
    Module[ { dir, file, stream },
        dir = FileNameJoin @ { $UserBaseDirectory, $showDirectoryName };
        If[ ! DirectoryQ @ dir, ConfirmBy[ CreateDirectory[ dir, CreateIntermediateDirectories -> True ], DirectoryQ, "CreateDirectory" ] ];
        file = FileNameJoin @ { dir, hash <> ".png" };
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
(* ::Subsection::Closed:: *)
(*openImageFile*)
openImageFile // beginDefinition;

openImageFile[ file_String ] :=
    If[ $OperatingSystem === "MacOSX",
        RunProcess[ { "open", file } ],
        SystemOpen @ file
    ];

openImageFile // endDefinition;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
End[ ];
EndPackage[ ];
