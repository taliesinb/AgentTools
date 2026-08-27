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
Renders a Wolfram Language expression to a PNG image at Retina resolution.
Use this to show the user a plot, graphic, grid, typeset expression, or any visual result.

The expression is evaluated, rasterized, and written to a timestamped file whose name ends in an @Nx retina \
suffix (e.g. @2x for 144 dpi) so other tools know the pixel density, and includes the hex hash of the PNG \
contents. When the open parameter is true, the file is also opened with the system image viewer so the user \
sees it. Returns the file path, pixel dimensions, resolution, byte size, and whether it was opened.";

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
        |>,
        "open" -> <|
            "Interpreter" -> "Boolean",
            "Help"        -> "Whether to open the rendered image in the system viewer so the user sees it on screen. If false, the image is written to disk but not opened.",
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

showExpression[ KeyValuePattern[ { "expression" -> code_String, "open" -> open0_ } ] ] := Enclose[
    Module[ { open, held, image, bytes, hash, file, dims },
        open  = Replace[ open0, Except[ True | False ] -> False ];
        held  = ConfirmMatch[ Quiet @ ToExpression[ code, InputForm, HoldComplete ], HoldComplete[ _ ], "Parse" ];
        image = ConfirmMatch[ Rasterize[ ReleaseHold @ held, ImageResolution -> $imageResolution ], _Image, "Rasterize" ];
        bytes = ConfirmMatch[ ExportByteArray[ image, "PNG" ], _ByteArray, "Export" ];
        hash  = ConfirmBy[ Hash[ bytes, "SHA256", "HexString" ], StringQ, "Hash" ];
        file  = ConfirmBy[ writeImageFile[ hash, bytes ], StringQ, "Write" ];
        ConfirmAssert[ FileExistsQ @ file, "FileExists" ];
        If[ open, ConfirmMatch[ openImageFile @ file, Except[ _Failure ], "Open" ] ];
        dims  = ImageDimensions @ image;
        StringRiffle[
            {
                "Rendered image:",
                "- path: " <> file,
                "- dimensions: " <> ToString @ First @ dims <> "x" <> ToString @ Last @ dims <> " px",
                "- resolution: " <> ToString @ $imageResolution <> " dpi",
                "- size: " <> ToString @ Length @ bytes <> " bytes",
                "- opened: " <> If[ open, "true", "false" ]
            },
            "\n"
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
