// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2019/03/28 17:09
// License Copyright (c) Daniele Giardini

using DG.DeebugLib.RuntimeConsole;
using DG.DemiEditor;
using UnityEditor;
using UnityEngine;

namespace DG.DeebugLib.Editor
{
    class DeebugOptionsWindow : EditorWindow
    {
        [MenuItem("Tools/Demigiant/" + _Title)]
        static void ShowWindow() { GetWindow(typeof(DeebugOptionsWindow), false, _Title); }
        
        const string _Title = "Deebug";
        DeebugOptions _src;
        Vector2 _scrollPos;

        const string _Lab_Console_ExtraReportInfoHelp = "Adds extra custom text to the log report." +
                                                       "\n<i>You can override this with the <b>AddExtraReportInfo</b> method.</i>";
        const string _Lab_Console_AdvancedStackTraceHelp = "Activates a system that allows to get stack traces even if the <b>Development Build</b>" +
                                                           " option is not enabled in your build settings." +
                                                           "\nConsider though that if the <b>Development Build</b> option is disabled" +
                                                           " stack traces will be present but less clear and without line numbers," +
                                                           " and that the <i>System.Diagnostics</i> namespace will be included in builds if this is active.";
        const string _Lab_Console_ErrorPopupHelp = "Opens a popup with a custom message when an error happens (only if the console was closed)." +
                                                  "\n<i>You can override this with the <b>ShowPopupOnError</b> method.</i>";
        const string _Lab_Console_CanSendMailHelp = "Adds a SEND MAIL button to the log evidence (and eventual error popup)" +
                                                    " which opens the user's mail and fills it with the log data and the mail address below." +
                                                    "\nNote that if the user's system is not set to follow <b>mailto</b> links this will fail." +
                                                   "\n<i>You can override this with the <b>SetMailTo</b> method.</i>";

        #region Unity and GUI Methods

        void OnEnable()
        { Undo.undoRedoPerformed += Repaint; }

        void OnDisable()
        { Undo.undoRedoPerformed -= Repaint; }

        void OnHierarchyChange()
        { Repaint(); }

        void OnGUI()
        {
            if (_src == null) _src = DeEditorPanelUtils.ConnectToSourceAsset<DeebugOptions>(DeebugEditor.OptionsADBFilePath, true, true);
            _scrollPos = GUILayout.BeginScrollView(_scrollPos);
            Undo.RecordObject(_src, _Title);
            DeGUI.BeginGUI();
            Style.Init();
            using (new EditorGUI.DisabledScope(Application.isPlaying)) {
                // Console options
                using (new DeGUILayout.ToolbarScope(DeGUI.styles.toolbar.large)) {
                    GUILayout.Label("Runtime Console Options", DeGUI.styles.label.toolbarL);
                    _src.extraHelpInfo = DeGUILayout.ToggleButton(_src.extraHelpInfo, "?", Style.toolbarLToggle);
                }
                using (new GUILayout.VerticalScope(DeGUI.styles.box.stickyTop)) {
                    using (new GUILayout.VerticalScope()) {
                        GUILayout.Label("Tracked logs");
                        using (new GUILayout.HorizontalScope()) {
                            _src.console.trackedLogTypes[0] = DeGUILayout.ToggleButton(_src.console.trackedLogTypes[0], "Normal Logs");
                            _src.console.trackedLogTypes[1] = DeGUILayout.ToggleButton(_src.console.trackedLogTypes[1], "Warnings");
                            _src.console.trackedLogTypes[2] = DeGUILayout.ToggleButton(_src.console.trackedLogTypes[2], "Errors");
                        }
                    }
                    using (new GUILayout.VerticalScope()) {
                        GUILayout.Label("Log Visibility toggles");
                        using (new GUILayout.HorizontalScope()) {
                            using (new EditorGUI.DisabledScope(!_src.console.trackedLogTypes[0])) {
                                _src.console.logTypesVisibility[0] = DeGUILayout.ToggleButton(_src.console.logTypesVisibility[0], "Normal Logs");
                            }
                            using (new EditorGUI.DisabledScope(!_src.console.trackedLogTypes[1])) {
                                _src.console.logTypesVisibility[1] = DeGUILayout.ToggleButton(_src.console.logTypesVisibility[1], "Warnings");
                            }
                            using (new EditorGUI.DisabledScope(!_src.console.trackedLogTypes[2])) {
                                _src.console.logTypesVisibility[2] = DeGUILayout.ToggleButton(_src.console.logTypesVisibility[2], "Errors");
                            }
                        }
                    }
                    DeGUILayout.HorizontalDivider(null, 2);
                    using (new EditorGUI.DisabledScope(!_src.console.trackedLogTypes[2])) {
                        _src.console.autoOpenOnError = EditorGUILayout.Toggle("Auto-open on error", _src.console.autoOpenOnError);
                    }
                    _src.console.toggleOpenKey = (KeyCode)EditorGUILayout.EnumPopup("Toggle open key", _src.console.toggleOpenKey);
                    _src.console.toggleOpenGesture = (DeeConsole.Gesture)EditorGUILayout.EnumPopup("Toggle open gesture", _src.console.toggleOpenGesture);
                    _src.console.gestureTolerance = (DeeGesture.GestureTolerance)EditorGUILayout.EnumPopup("Gesture tolerance", _src.console.gestureTolerance);
                    _src.console.useBigFonts = EditorGUILayout.Toggle("Use big fonts", _src.console.useBigFonts);
                    _src.console.showTimeLabel = EditorGUILayout.Toggle("Show time/frame labels", _src.console.showTimeLabel);
                    _src.console.showSceneLabel = EditorGUILayout.Toggle("Show scene label", _src.console.showSceneLabel);
                    // Extra report info
                    using (new GUILayout.VerticalScope(DeGUI.styles.box.def)) {
                        GUILayout.Label("Extra report info");
                        if (_src.extraHelpInfo) GUILayout.Label(_Lab_Console_ExtraReportInfoHelp, Style.descrLabel);
                        _src.console.extraReportInfo = EditorGUILayout.TextArea(_src.console.extraReportInfo, Style.textArea);
                    }
                    // Advanced stack traces
                    using (new DeGUILayout.ToolbarScope(Style.subToolbar)) {
                        using (var check = new EditorGUI.ChangeCheckScope()) {
                            _src.console.advancedStackTrace = DeGUILayout.ToggleButton(_src.console.advancedStackTrace, "Enable", Style.toolbarToggle);
                            if (check.changed) {
                                bool change = EditorUtility.DisplayDialog("Advanced Stack Trace",
                                    "Do you want to change this option?\n\nDeebug will be reimported in order to " +
                                    (_src.console.advancedStackTrace ? "include" : "exclude") +
                                    " the required systems",
                                    "Ok", "Cancel"
                                );
                                if (change) DeebugProcessors.RefreshFileModifications();
                                else _src.console.advancedStackTrace = !_src.console.advancedStackTrace;
                            }
                        }
                        GUILayout.Label("Advanced stack trace", DeGUI.styles.label.toolbar);
                    }
                    if (_src.console.advancedStackTrace && _src.extraHelpInfo) {
                        using (new GUILayout.VerticalScope(DeGUI.styles.box.sticky)) {
                            GUILayout.Space(-5);
                            GUILayout.Label(_Lab_Console_AdvancedStackTraceHelp, Style.descrLabel);
                        }
                    }
                    // Error popup
                    using (new DeGUILayout.ToolbarScope(Style.subToolbar)) {
                        _src.console.showPopupOnError = DeGUILayout.ToggleButton(_src.console.showPopupOnError, "Enable", Style.toolbarToggle);
                        GUILayout.Label("Error popup", DeGUI.styles.label.toolbar);
                    }
                    if (_src.console.showPopupOnError) {
                        using (new GUILayout.VerticalScope(DeGUI.styles.box.sticky)) {
                            if (_src.extraHelpInfo) {
                                GUILayout.Space(-5);
                                GUILayout.Label(_Lab_Console_ErrorPopupHelp, Style.descrLabel);
                            }
                            if (_src.console.showPopupOnError) {
                                _src.console.errorPopupMessage = EditorGUILayout.TextArea(_src.console.errorPopupMessage, Style.textArea);
                            }
                        }
                    }
                    // Mail options
                    using (new DeGUILayout.ToolbarScope(Style.subToolbar))
                    {
                        _src.console.canSendMail = DeGUILayout.ToggleButton(_src.console.canSendMail, "Enable", Style.toolbarToggle);
                        GUILayout.Label("Allow Send Mail", DeGUI.styles.label.toolbar);
                    }
                    if (_src.console.canSendMail) {
                        using (new GUILayout.VerticalScope(DeGUI.styles.box.sticky)) {
                            if (_src.extraHelpInfo) {
                                GUILayout.Space(-5);
                                GUILayout.Label(_Lab_Console_CanSendMailHelp, Style.descrLabel);
                            }
                            _src.console.mailAddress = EditorGUILayout.TextField("Email address", _src.console.mailAddress);
                            _src.console.mailSubject = EditorGUILayout.TextField("Email title", _src.console.mailSubject);
                        }
                    }
                }

            }
            GUILayout.EndScrollView();
            if (GUI.changed) EditorUtility.SetDirty(_src);
        }

        #endregion

        // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████
        // ███ INTERNAL CLASSES ████████████████████████████████████████████████████████████████████████████████████████████████
        // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

        static class Style
        {
            static bool _initialized;

            public static GUIStyle descrLabel, textArea, subToolbar, toolbarToggle, toolbarLToggle;

            public static void Init()
            {
                if (_initialized) return;

                _initialized = true;

                descrLabel = new GUIStyle(GUI.skin.label).Add(Format.RichText, Format.WordWrap, 10);
                textArea = EditorStyles.textArea.Clone(Format.WordWrap);
                subToolbar = DeGUI.styles.toolbar.defNoPadding.Clone();
                toolbarToggle = DeGUI.styles.button.bBlankBorderCompact.Clone().StretchWidth(false).Height(15)
                    .Padding(7, 5, 1, 0).Margin(0, 0, 2, 0);
                toolbarLToggle = toolbarToggle.Clone().MarginTop(4);
            }
        }
    }
}