// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/01/22

using DG.DemiEditor;
using DG.DemiLib;
using DG.Tweening.Timeline.Core;
using UnityEditor;
using UnityEngine;

namespace DG.Tweening.TimelineEditor
{
    internal class TimelineSettingsUI : ABSTimelineElement
    {
        readonly GUIContent _gcMinPixelDragDistance = new GUIContent("Min Drag Activation Distance",
            "Minimum pixels the mouse must move during dragging before the drag operation really starts");
        readonly GUIContent _gcMaxSnapPixelDistance = new GUIContent("Max Snap Distance",
            "Max distance at which elements will snap to when dragging them with the ALT key pressed");
        Vector2 _scrollP;

        #region Public Methods

        public void Refresh() {}

        public override void Draw(Rect drawArea)
        {
            base.Draw(drawArea);

            editor.MarkForUndo(true, false, true);
            DOEGUI.BeginGUI();

            // Toolbar
            using (new DeGUILayout.ToolbarScope()) {
                GUILayout.Label("Settings", DOEGUI.Styles.label.toolbar, GUILayout.ExpandWidth(true));
                GUILayout.Label("v" + DOVisualSequenceSettings.Version, DOEGUI.Styles.label.toolbar, GUILayout.ExpandWidth(false));
                using (new DeGUI.ColorScope(new DeSkinColor(0.7f, 0.3f))) {
                    if (GUILayout.Button("×", DOEGUI.Styles.button.tool, GUILayout.Width(18))) {
                        DOVisualSequenceTimeline.mode = DOVisualSequenceTimeline.Mode.Default;
                    }
                }
            }

            _scrollP = GUILayout.BeginScrollView(_scrollP);

            using (new DeGUI.LabelFieldWidthScope(186)) {
                // Runtime settings
                DeGUILayout.HorizontalDivider(new Color(0.64f, 0.89f, 0.44f), 1, 0, 0);
                using (new DeGUILayout.ToolbarScope()) {
                    GUILayout.Label("Runtime Settings", DOEGUI.Styles.label.toolbar, GUILayout.ExpandWidth(true));
                }
                using (new GUILayout.VerticalScope(DOEGUI.Styles.timeline.settingsPanel)) {
                    runtimeSettings.foo_debugLogs = EditorGUILayout.Toggle("Debug Logs", runtimeSettings.foo_debugLogs, GUILayout.ExpandWidth(false));
                }


                // Editor settings
                DeGUILayout.HorizontalDivider(new Color(0.64f, 0.89f, 0.44f), 1, 0, 0);
                using (new DeGUILayout.ToolbarScope()) {
                    GUILayout.Label("Editor Settings", DOEGUI.Styles.label.toolbar, GUILayout.ExpandWidth(true));
                }
                using (new GUILayout.VerticalScope(DOEGUI.Styles.timeline.settingsPanel)) {
                    settings.actionsLayoutDuration = EditorGUILayout.Slider("Actions/Events Span", settings.actionsLayoutDuration, 0.25f, 2, Width());
                    settings.minPixelDragDistance = EditorGUILayout.IntSlider(_gcMinPixelDragDistance, settings.minPixelDragDistance, 5, 50, Width());
                    settings.maxSnapPixelDistance = EditorGUILayout.IntSlider(_gcMaxSnapPixelDistance, settings.maxSnapPixelDistance, 10, 100, Width());
                    GUILayout.Space(4);
                    using (new DeGUI.LabelFieldWidthScope(EditorGUIUtility.labelWidth - 6)) {
                        using (new DeGUILayout.ToolbarScope()) {
                            GUILayout.Label("Default settings for newly created tweens in the Timeline", DOEGUI.Styles.label.toolbar, GUILayout.ExpandWidth(true));
                        }
                        using (new GUILayout.VerticalScope(DOEGUI.Styles.box.stickyTop, GUILayout.ExpandWidth(true))) {
                            settings.defaults.duration = EditorGUILayout.Slider("Duration", settings.defaults.duration, 0, 100, Width());
                            settings.defaults.ease = (Ease)EditorGUILayout.EnumPopup("Ease", settings.defaults.ease, Width());
                            settings.defaults.loopType = (LoopType)EditorGUILayout.EnumPopup("Loop Type", settings.defaults.loopType, Width());
                        }
                        GUILayout.Space(4);
                        using (new DeGUILayout.ToolbarScope()) {
                            GUILayout.Label("DOVisualSequence Inspector settings", DOEGUI.Styles.label.toolbar, GUILayout.ExpandWidth(true));
                        }
                        using (new GUILayout.VerticalScope(DOEGUI.Styles.box.stickyTop, GUILayout.ExpandWidth(true))) {
                            settings.forceFoldoutsOpen = EditorGUILayout.Toggle("Force Inspector Foldouts Open", settings.forceFoldoutsOpen, GUILayout.ExpandWidth(false));
                        }
                        GUILayout.Space(4);
                        using (new DeGUI.ColorScope(null, null, Color.yellow)) {
                            using (new DeGUILayout.ToolbarScope()) {
                                GUILayout.Label("Experimental (use at your own risk)", DOEGUI.Styles.label.toolbar, GUILayout.ExpandWidth(true));
                            }
                            using (new GUILayout.VerticalScope(DOEGUI.Styles.box.stickyTop, GUILayout.ExpandWidth(true))) {
                                settings.experimental.enableRecordMode = EditorGUILayout.Toggle("Enable Record Mode", settings.experimental.enableRecordMode, GUILayout.ExpandWidth(false));
                            }
                        }
                    }
                }
            }

            GUILayout.EndScrollView();
            if (GUI.changed) editor.MarkDirty(true, false, true);
        }

        GUILayoutOption Width()
        {
            return GUILayout.MaxWidth(EditorGUIUtility.labelWidth + 300);
        }

        #endregion
    }
}