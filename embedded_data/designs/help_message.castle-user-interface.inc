{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "help_message.castle-user-interface" }
'{' + LineEnding +
'  "FullSize" : true,' + LineEnding +
'  "Name" : "Group1",' + LineEnding +
'  "$Children" : [' + LineEnding +
'    {' + LineEnding +
'      "FullSize" : true,' + LineEnding +
'      "Name" : "RectangleControl2",' + LineEnding +
'      "$Children" : [' + LineEnding +
'      ],' + LineEnding +
'      "$ClassName" : "TCastleRectangleControl"' + LineEnding +
'    },' + LineEnding +
'    {' + LineEnding +
'      "Height" : 6.0000000000000000E+001,' + LineEnding +
'      "Name" : "Group2",' + LineEnding +
'      "WidthFraction" : 1.0000000000000000E+000,' + LineEnding +
'      "$Children" : [' + LineEnding +
'        {' + LineEnding +
'          "HorizontalAnchorParent" : "hpRight",' + LineEnding +
'          "HorizontalAnchorSelf" : "hpRight",' + LineEnding +
'          "Name" : "HorizontalGroup1",' + LineEnding +
'          "Padding" : 1.0000000000000000E+001,' + LineEnding +
'          "Spacing" : 1.0000000000000000E+001,' + LineEnding +
'          "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'          "VerticalAnchorSelf" : "vpMiddle",' + LineEnding +
'          "$Children" : [' + LineEnding +
'            {' + LineEnding +
'              "Caption" : "Copy To Clipboard (Ctrl + C)",' + LineEnding +
'              "CustomBackgroundDisabled" : {' + LineEnding +
'                "$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "CustomBackgroundFocused" : {' + LineEnding +
'                "$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "CustomBackgroundNormal" : {' + LineEnding +
'                "$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "CustomBackgroundPressed" : {' + LineEnding +
'                "$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "CustomTextColorUse" : false,' + LineEnding +
'              "HorizontalAnchorDelta" : 1.0000000000000000E+001,' + LineEnding +
'              "Image" : {' + LineEnding +
'                "$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "Name" : "ButtonCopyClipboard",' + LineEnding +
'              "PaddingHorizontal" : 2.0000000000000000E+001,' + LineEnding +
'              "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'              "VerticalAnchorSelf" : "vpMiddle",' + LineEnding +
'              "$Children" : [' + LineEnding +
'              ],' + LineEnding +
'              "$ClassName" : "TCastleButton"' + LineEnding +
'            },' + LineEnding +
'            {' + LineEnding +
'              "Caption" : "Close",' + LineEnding +
'              "CustomBackgroundDisabled" : {' + LineEnding +
'                "$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "CustomBackgroundFocused" : {' + LineEnding +
'                "$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "CustomBackgroundNormal" : {' + LineEnding +
'                "$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "CustomBackgroundPressed" : {' + LineEnding +
'                "$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "CustomTextColorUse" : false,' + LineEnding +
'              "HorizontalAnchorDelta" : 3.4500000000000000E+002,' + LineEnding +
'              "Image" : {' + LineEnding +
'                "$ClassName" : "TCastleImagePersistent"' + LineEnding +
'              },' + LineEnding +
'              "Name" : "ButtonOK",' + LineEnding +
'              "PaddingHorizontal" : 2.0000000000000000E+001,' + LineEnding +
'              "VerticalAnchorParent" : "vpMiddle",' + LineEnding +
'              "VerticalAnchorSelf" : "vpMiddle",' + LineEnding +
'              "$Children" : [' + LineEnding +
'              ],' + LineEnding +
'              "$ClassName" : "TCastleButton"' + LineEnding +
'            }' + LineEnding +
'          ],' + LineEnding +
'          "$ClassName" : "TCastleHorizontalGroup"' + LineEnding +
'        }' + LineEnding +
'      ],' + LineEnding +
'      "$ClassName" : "TCastleUserInterface"' + LineEnding +
'    },' + LineEnding +
'    {' + LineEnding +
'      "Border" : {' + LineEnding +
'        "Bottom" : 6.0000000000000000E+001,' + LineEnding +
'        "$ClassName" : "TBorder"' + LineEnding +
'      },' + LineEnding +
'      "FullSize" : true,' + LineEnding +
'      "Name" : "ScrollView1",' + LineEnding +
'      "ScrollArea" : {' + LineEnding +
'        "AutoSizeToChildren" : true,' + LineEnding +
'        "Name" : "ScrollArea",' + LineEnding +
'        "$Children" : [' + LineEnding +
'          {' + LineEnding +
'            "Border" : {' + LineEnding +
'              "AllSides" : 1.0000000000000000E+001,' + LineEnding +
'              "$ClassName" : "TBorder"' + LineEnding +
'            },' + LineEnding +
'            "Name" : "LabelMessage",' + LineEnding +
'            "Text" : "Debug options:\r\n  --debug-log           Deprecated. We now log by default.\r\n  --debug-log-cache     Write log info, including cache.\r\n  --debug-log-shaders   Write log info, including shader source and log.\r\n  --debug-log-changes   Write log info, including VRML/X3D graph changes.\r\n  --debug-log-videos    Write log info, including videos loading and cache.\r\n  --debug-texture-memory Profile GPU texture memory usage.\r\n  \r\nDeprecated options:\r\n  --scene-change-no-normals\r\n                        Remove normals information from the loaded scene.\r\n                        Forces automatic calculation of normal vectors.\r\n                        Deprecated, doing this from command-line is not\r\n                        usually useful.\r\n  --scene-change-no-solid-objects\r\n                        Make all shapes not solid in the loaded scene.\r\n                        Disables backface culling.\r\n                        Deprecated, doing this fr' + 
'om command-line is not\r\n                        usually useful.\r\n  --scene-change-no-convex-faces\r\n                        Treat all faces as potentially concave in the loaded scene.\r\n                        Deprecated, doing this from command-line is not\r\n                        usually useful.\r\n  --write-to-vrml       Deprecated, shortcut for \"--write --write-encoding=classic\".\r\n",' + 
LineEnding +
'            "$Children" : [' + LineEnding +
'            ],' + LineEnding +
'            "$ClassName" : "TCastleLabel"' + LineEnding +
'          }' + LineEnding +
'        ],' + LineEnding +
'        "$ClassName" : "TInternalCastleScrollArea"' + LineEnding +
'      },' + LineEnding +
'      "$Children" : [' + LineEnding +
'      ],' + LineEnding +
'      "$ClassName" : "TCastleScrollView"' + LineEnding +
'    }' + LineEnding +
'  ],' + LineEnding +
'  "$ClassName" : "TCastleUserInterface"' + LineEnding +
'}' + LineEnding +
''
