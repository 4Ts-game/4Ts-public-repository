// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/01/16

using System;
using DG.DemiEditor;
using DG.DemiLib;
using DG.DOTweenEditor.UI;
using DG.Tweening.TimelineEditor;
using DG.Tweening.TimelineEditor.SequencedUI;
using DG.Tweening.Timeline;
using DG.Tweening.Timeline.Core;
using DG.Tweening.TimelineEditor.PropertyDrawers;
using UnityEditor;
using UnityEngine;

namespace DG.Tweening.TimelineEditor
{
    class DOVisualSequenceTimeline : EditorWindow, IHasCustomMenu
    {
        public enum Mode
        {
            Default,
            Settings,
            Help
        }

        // ■■■ EVENTS
        public static event Action OnPrefabEditingModeChanged; // Dispatched when entering/exiting prefab editing mode (if the Timeline is open)
        public static event Action<DOVisualSequence> OnSequenceOpened;
        public static event Action<DOVisualSequence> OnSequenceChanged;
        public static event Action OnMouseDown;
        static void Dispatch_OnPrefabEditingModeChanged() { if (OnPrefabEditingModeChanged != null) OnPrefabEditingModeChanged(); }
        static void Dispatch_OnSequenceOpened(DOVisualSequence sequence) { if (OnSequenceOpened != null) OnSequenceOpened(sequence); }
        public static void Dispatch_OnSequenceChanged(DOVisualSequence sequence) { if (OnSequenceChanged != null) OnSequenceChanged(sequence); }
        static void Dispatch_OnMouseDown() { if (OnMouseDown != null) OnMouseDown(); }
        // ■■■

        [MenuItem("Tools/Demigiant/" + _Title)]
        static void ShowWindow() { ShowWindow(null, null, null); }
		
        public const int LayerHeight = 18;
        public static DOVisualSequenceTimeline editor { get; private set; }
        public static DOTimelineSettings settings { get; private set; }
        public static DOVisualSequenceSettings runtimeSettings { get; private set; }
        public static readonly TimelineLayout Layout = new TimelineLayout();
        public static SerializedProperty spSequence; // Used by SequencedEditor to display UnityEvents
        public static Component src;
        public static DOVisualSequence sequence;
        public static Mode mode = Mode.Default;
        public static bool isUndoRedoPass { get; private set; } // TRUE during the first Repaint after an undoRedo
        public static bool isRecorderOrPreviewUndoPass { get; private set; } // TRUE during the first Repaint after TimelineRecorder has stopped
        public static bool isOrWillResizeLayersPanel { get { return _isPreparingToResizeLayersPanel || _isResizingLayersPanel; } }
        public static StageMode stageMode { get; private set; }
        public PrefabEditSaveMode prefabEditingSaveMode { get; private set; }
        const string _Title = "DOTween Timeline";
        static bool _isPreparingToResizeLayersPanel;
        static bool _isResizingLayersPanel;
        Vector2 _dragStartP;
        float _layersPanelDragStartWidth;
        readonly TimelineGlobalHeader _globalHeader = new TimelineGlobalHeader();
        readonly TimelineLayersHeader _layersHeader = new TimelineLayersHeader();
        readonly TimelineLayers _layers = new TimelineLayers();
        readonly TimelineHeader _timelineHeader = new TimelineHeader();
        readonly TimelineMain _timeline = new TimelineMain();
        readonly TimelineScrubber _scrubber = new TimelineScrubber();
        readonly SequencedEditor _sequencedEditor = new SequencedEditor();
        readonly TimelineSettingsUI _settingsEditor = new TimelineSettingsUI();
        readonly TimelineHelpUI _helpEditor = new TimelineHelpUI();
        readonly GUIContent _gcDisconnected = new GUIContent("<b>Select a DOVisualSequence in the Inspector to edit it</b>" +
                                                      "\n(<i>or use the button below to select one of the existing DOVisualSequences in the Scene</i>)");
        readonly GUIContent _gcSelectSequence = new GUIContent("Select Sequence in Scene");
        readonly GUIContent _gcSettings = new GUIContent("Settings");
        readonly GUIContent _gcHelp = new GUIContent("Help");
        readonly GUIContent _gcPrefabEditingAutoSaveWarning = new GUIContent("DOTween Timeline requires <b>Prefab AutoSave</b> to be <b>disabled</b>" +
                                                                             " (<i>top-right toggle in Unity's Scene view</i>)");

        #region Unity and GUI Methods

        public void AddItemsToMenu(GenericMenu menu)
        {
            menu.AddItem(new GUIContent("DOTween Timeline > Cleanup current Sequence"), false, () => {
                if (sequence == null) {
                    EditorUtility.DisplayDialog("DOTween Timeline > Cleanup", "You must open a DOVisualSequence to apply the cleanup", "Ok");
                    return;
                }
                TimelineEditorUtils.CleanupSequence(sequence);
            });
            menu.AddItem(new GUIContent("DOTween Timeline > Developer Debug Mode/ON"), TimelineSession.isDevDebugMode, null);
            menu.AddSeparator("DOTween Timeline > Developer Debug Mode/");
            menu.AddItem(new GUIContent("DOTween Timeline > Developer Debug Mode/ACTIVATE ALL"), false, () => {
                TimelineSession.ToggleAll(true);
            });
            menu.AddItem(new GUIContent("DOTween Timeline > Developer Debug Mode/DEACTIVATE ALL"), false, () => {
                TimelineSession.ToggleAll(false);
            });
            menu.AddSeparator("DOTween Timeline > Developer Debug Mode/");
            menu.AddItem(new GUIContent("DOTween Timeline > Developer Debug Mode/Log Missing PlugDataGuid Assignment"), TimelineSession.logMissingPlugDataGuidAssignment, () => {
                TimelineSession.logMissingPlugDataGuidAssignment = !TimelineSession.logMissingPlugDataGuidAssignment;
            });
            menu.AddItem(new GUIContent("DOTween Timeline > Developer Debug Mode/Show Sequenceds PlugData[Index|Guid]"), TimelineSession.showSequencedsPlugDataIndexAndGuid, () => {
                TimelineSession.showSequencedsPlugDataIndexAndGuid = !TimelineSession.showSequencedsPlugDataIndexAndGuid;
            });
        }

        void OnEnable()
        {
            SetTitle();
            ConnectToSettings();
            Refresh();
            this.wantsMouseMove = true;
            Undo.undoRedoPerformed += OnUndoRedoPerformed;
            DOTimelineRecorder.OnStopRecording += OnRecorderOrPreviewStopped;
            DOTimelinePreviewManager.OnStopPreviewing += OnRecorderOrPreviewStopped;
        }

        void OnDisable()
        {
            Undo.undoRedoPerformed -= OnUndoRedoPerformed;
            DOTimelineRecorder.OnStopRecording -= OnRecorderOrPreviewStopped;
            DOTimelinePreviewManager.OnStopPreviewing -= OnRecorderOrPreviewStopped;
        }

        void OnHierarchyChange()
        {
            Refresh();
            Repaint();
        }

        void OnUndoRedoPerformed()
        {
            isUndoRedoPass = true;
            settings.ReapplySelected(out src, out sequence, out spSequence);
            Refresh();
            TimelineSelection.RefreshSelectionsData();
            Repaint();
        }

        void SetTitle()
        {
            this.titleContent = new GUIContent("Timeline", EditorGUIUtils.logo);
        }

        void Update()
        {
            if (Application.isPlaying) Repaint();
        }

        void OnGUI()
        {
            ConnectToSettings();
            DOEGUI.BeginGUI();
            Rect area = position.ResetXY();
            bool isPlayingOrPreviewing = Application.isPlaying || DOTimelinePreviewManager.isPlayingOrPreviewing;
            bool isRecording = DOTimelineRecorder.isRecording;
            bool isRepaint = Event.current.type == EventType.Repaint;

            using (new EditorGUI.DisabledScope(isPlayingOrPreviewing)) {
                switch (mode) {
                case Mode.Settings:
                    using (new GUILayout.AreaScope(area)) _settingsEditor.Draw(area);
                    isUndoRedoPass = isRecorderOrPreviewUndoPass = false;
                    return;
                case Mode.Help:
                    using (new GUILayout.AreaScope(area)) _helpEditor.Draw(area);
                    isUndoRedoPass = isRecorderOrPreviewUndoPass = false;
                    return;
                }
            }

            if (src == null) {
                TimelineSelection.Clear();
                isUndoRedoPass = isRecorderOrPreviewUndoPass = false;
                Rect descrR = new Rect(area.x + 10, area.y + 10, area.width - 20,
                    DOEGUI.Styles.timeline.disconnectedLabel.CalcHeight(_gcDisconnected, area.width)
                );
                Rect btSequenceR = descrR.SetY(descrR.yMax + 6).SetWidth(GUI.skin.button.CalcSize(_gcSelectSequence).x).SetHeight(22);
                Rect btSettingsR = btSequenceR.SetX(btSequenceR.xMax + 2).SetWidth(80);
                Rect btHelpR = btSettingsR.SetX(btSettingsR.xMax + 2);
                DeGUI.DrawTiledTexture(area, DeStylePalette.tileBars_slanted_alpha, 1f, new DeSkinColor(0.1f));
                EditorGUI.DropShadowLabel(descrR, _gcDisconnected, DOEGUI.Styles.timeline.disconnectedLabel);
                if (GUI.Button(btSequenceR, _gcSelectSequence)) {
                    TimelineEditorUtils.CM_SelectSequenceInScene(btSequenceR);
                }
                if (GUI.Button(btSettingsR, _gcSettings)) {
                    mode = Mode.Settings;
                }
                if (GUI.Button(btHelpR, _gcHelp)) {
                    mode = Mode.Help;
                }
                return;
            }

            if (stageMode == StageMode.PrefabEditingMode) {
                RefreshPrefabStageData();
                if (prefabEditingSaveMode == PrefabEditSaveMode.AutoSave) {
                    DeGUI.DrawTiledTexture(area, DeStylePalette.tileBars_slanted, 1f, new Color(0.11f, 0.3f, 0.48f, 0.5f));
                    float warningH = DOEGUI.Styles.global.warningLabelBox.CalcHeight(_gcPrefabEditingAutoSaveWarning, 230);
                    Rect warningR = new Rect(0, 0, 230, warningH).SetCenter(area.center.x, area.center.y);
                    using (new DeGUI.ColorScope(new Color(0.91f, 0.42f, 0f))) {
                        GUI.Label(warningR, _gcPrefabEditingAutoSaveWarning, DOEGUI.Styles.global.warningLabelBox);
                    }
                    return;
                }
            }

            MarkForUndo();
//            Debug.Log(Time.frameCount + " ED > " + Event.current.type + ", undoPass: " + isUndoRedoPass + ", recOrPreviewUndoPass: " + isRecorderOrPreviewUndoPass);

            bool hasSameTypeSelection = TimelineSelection.HasSelections(true);
            bool showSequencedEditor = hasSameTypeSelection
                                       && !TimelineSelection.isDraggingSelection
                                       && !TimelineSelection.isDraggingSequenceds
                                       && !TimelineSelection.isDraggingDuration;

            Rect headerR = area.SetHeight(EditorGUIUtility.singleLineHeight + 2);
            Rect contentR = area.ShiftYAndResize(headerR.height);
            Rect sequencedEditorR = showSequencedEditor
                ? contentR.ShiftXAndResize(contentR.width - SequencedEditor.DefaultWidth)
                : contentR.ShiftXAndResize(contentR.width);
            Rect layersR = contentR.SetWidth(settings.layersPanelWidth);
            // Validate layers panel size
            int maxLayersPanelW = (int)(area.width - sequencedEditorR.width) - 50;
            if (settings.layersPanelWidth > maxLayersPanelW) {
                settings.layersPanelWidth = Mathf.Max(TimelineLayers.MinSize, maxLayersPanelW);
                MarkDirty(true, false);
                layersR.width = settings.layersPanelWidth;
                if (layersR.width <= TimelineLayers.MinSize && layersR.width > maxLayersPanelW) {
                    // Hide layers
                    layersR.width = 18;
                }
            }
            //
            Rect layersHeaderR = layersR.SetHeight(18);
            layersR = layersR.ShiftYAndResize(layersHeaderR.height);
            Rect timelineR = contentR.ShiftXAndResize(layersR.width).Shift(0, 0, -sequencedEditorR.width, 0);
            Rect timelineHeaderR = timelineR.SetHeight(18);
            timelineR = timelineR.ShiftYAndResize(timelineHeaderR.height);
            Rect scrubberR = timelineHeaderR.Shift(0, 0, 0, timelineR.height);
            Layout.Refresh(timelineR);

            // Check for layers panel resizing
            if (!_isResizingLayersPanel) {
                float dragHalfW = 4;
                Rect dragLayersPanelR = layersR.Shift(layersR.width - dragHalfW, 0, 0, 0).SetWidth(dragHalfW * 2);
                bool wasPreparingToResizeLayersPanel = _isPreparingToResizeLayersPanel;
                _isPreparingToResizeLayersPanel = dragLayersPanelR.Contains(Event.current.mousePosition);
                if (_isPreparingToResizeLayersPanel != wasPreparingToResizeLayersPanel) Repaint();
            }
            if (isOrWillResizeLayersPanel) {
                EditorGUIUtility.AddCursorRect(area, MouseCursor.ResizeHorizontal);
                switch (Event.current.rawType) {
                case EventType.MouseUp:
                    if (_isResizingLayersPanel) StopResizingLayersPanel();
                    break;
                }
                switch (Event.current.type) {
                case EventType.MouseDown:
                    switch (Event.current.button) {
                    case 0:
                        if (_isPreparingToResizeLayersPanel) DragResizeLayersPanel(true);
                        break;
                    }
                    break;
                case EventType.MouseDrag:
                    if (_isResizingLayersPanel) DragResizeLayersPanel(false);
                    break;
                }
            }

            // Not in disabledScope so sequence selection can work at runtime
            using (new GUILayout.AreaScope(headerR)) _globalHeader.Draw(headerR.ResetXY());
            if (sequence != null) { // Sequence might have been closed by global header
                // Not in disabledScope so preview button can work while previewing
                using (new GUILayout.AreaScope(layersHeaderR)) _layersHeader.Draw(layersHeaderR.ResetXY());
                using (new EditorGUI.DisabledScope(isPlayingOrPreviewing && !isRecording)) {
                    if (hasSameTypeSelection) {
                        using (new GUILayout.AreaScope(sequencedEditorR)) _sequencedEditor.Draw(sequencedEditorR.ResetXY());
                    }
                }
                using (new EditorGUI.DisabledScope(isPlayingOrPreviewing)) {
                    using (new GUILayout.AreaScope(layersR)) _layers.Draw(layersR.ResetXY());
                    using (new GUILayout.AreaScope(timelineHeaderR)) _timelineHeader.Draw(timelineHeaderR.ResetXY());
                }
                using (new EditorGUI.DisabledScope(_isResizingLayersPanel)) {
                    using (new GUILayout.AreaScope(timelineR)) _timeline.Draw(timelineR.ResetXY());
                }
                // Not in disabledScope so scrubbing can work while in editor preview mode
                using (new GUILayout.AreaScope(scrubberR)) _scrubber.Draw(scrubberR.ResetXY());

                if (DOTimelineRecorder.isRecording) {
                    // Recording border
                    using (new DeGUI.ColorScope(null, null, Color.red)) {
                        GUI.Box(area, GUIContent.none, DOEGUI.Styles.timeline.previewBorderBox);
                    }
                } else if (DOTimelinePreviewManager.isPlayingOrPreviewing) {
                    // Preview border
                    using (new DeGUI.ColorScope(null, null, Color.cyan)) {
                        GUI.Box(area, GUIContent.none, DOEGUI.Styles.timeline.previewBorderBox);
                    }
                }
            }

            if (GUI.changed) {
                MarkDirty();
                Dispatch_OnSequenceChanged(sequence);
            }
            if (isRepaint) {
                bool wasRecorderStoppedPass = isRecorderOrPreviewUndoPass;
                isUndoRedoPass = isRecorderOrPreviewUndoPass = false;
                if (wasRecorderStoppedPass) Repaint();
            } else if (Event.current.type == EventType.MouseDown) {
                Dispatch_OnMouseDown();
            }
        }

        void DragResizeLayersPanel(bool begin)
        {
            if (begin) {
                _isResizingLayersPanel = true;
                _isPreparingToResizeLayersPanel = false;
                _dragStartP = Event.current.mousePosition;
                _layersPanelDragStartWidth = settings.layersPanelWidth;
            } else {
                using (new DOScope.UndoableSerialization(true, false)) {
                    settings.layersPanelWidth = Mathf.Max(
                        (int)(_layersPanelDragStartWidth + (Event.current.mousePosition.x - _dragStartP.x)),
                        TimelineLayers.MinSize
                    );
                }
                Repaint();
            }
        }

        void StopResizingLayersPanel()
        {
            _isPreparingToResizeLayersPanel = _isResizingLayersPanel = false;
            Repaint();
        }

        #endregion

        #region Methods

        void Refresh()
        {
            editor = this;
            if (RefreshPrefabStageData()) return;

            if (src == null) settings.ReapplySelected(out src, out sequence, out spSequence);
            else settings.RefreshSelected(src, sequence);
            if (sequence != null) {
                if (TimelineEditorUtils.ValidateAndFixSequence(sequence, spSequence)) EditorUtility.SetDirty(src);
            }
            _globalHeader.Refresh();
            _layersHeader.Refresh();
            _layers.Refresh();
            _timelineHeader.Refresh();
            _timeline.Refresh();
            _scrubber.Refresh();
            _sequencedEditor.Refresh();
            _settingsEditor.Refresh();
        }

        /// <summary>
        /// Returns TRUE if prefab editing mode changed
        /// </summary>
        bool RefreshPrefabStageData()
        {
            StageMode currStageMode = TimelineEditorUtils.IsEditingPrefab() ? StageMode.PrefabEditingMode : StageMode.Normal;
            bool prefabEditingModeChanged = stageMode != StageMode.Unset && currStageMode != stageMode;
            stageMode = currStageMode;
            prefabEditingSaveMode = TimelineEditorUtils.GetPrefabEditSaveMode();
            if (prefabEditingModeChanged) {
                DOVisualSequencePropertyDrawer.ForceClear();
                CloseCurrentSequence();
                Dispatch_OnPrefabEditingModeChanged();
                return true;
            }
            return false;
        }

        static void ConnectToSettings()
        {
            if (settings == null) settings = DOTimelineSettings.Load();
            if (runtimeSettings == null) {
                runtimeSettings = DeEditorPanelUtils.ConnectToSourceAsset<DOVisualSequenceSettings>(TimelinePaths.ADB.DOVisualSequenceSettings, true, true);
            }
        }

        #endregion

        #region Public Methods

        public static void ShowWindow(Component component, DOVisualSequence sequence, SerializedProperty spSequence)
        {
            if (spSequence == null && sequence != null) spSequence = TimelineEditorUtils.GetSerializedSequence(component, sequence.guid);
            DOVisualSequenceTimeline.src = component;
            DOVisualSequenceTimeline.sequence = sequence;
            DOVisualSequenceTimeline.spSequence = spSequence;
            ConnectToSettings();
            if (component != null) settings.RefreshSelected(component, sequence);
            TimelineSelection.Clear();
            editor = GetWindow<DOVisualSequenceTimeline>(false, _Title);
            editor.SetTitle();
            editor.Refresh();
            Dispatch_OnSequenceOpened(sequence);
        }

        public static void CloseCurrentSequence()
        {
            if (DOTimelineRecorder.isRecording) DOTimelineRecorder.ExitRecordMode(true);
            if (DOTimelinePreviewManager.isPlaying) DOTimelinePreviewManager.StopPreview();
            src = null;
            sequence = null;
            spSequence = null;
            ConnectToSettings();
            settings.RefreshSelected(null, null, false);
            if (editor != null) editor.Repaint();
        }

        public static void ShowSettingsPanel()
        {
            mode = Mode.Settings;
        }

        public static void ShowHelpPanel()
        {
            mode = Mode.Help;
        }

        public void MarkForUndo(bool markSettings = true, bool markComponent = true, bool markRuntimeSettings = false)
        {
            if (markSettings) Undo.RecordObject(settings, _Title);
            if (markComponent) {
                if (src != null) Undo.RecordObject(src, _Title);
                if (spSequence != null) {
                    try {
                        spSequence.serializedObject.Update();
                    } catch {
                        // Happens when deselecting the component that contains the Sequence: Unity clears the SO contents
                        // ► Regenerate it
                        spSequence = TimelineEditorUtils.GetSerializedSequence(src, sequence.guid);
                        spSequence.serializedObject.Update();
                    }
                }
            }
            if (markRuntimeSettings) Undo.RecordObject(runtimeSettings, _Title);
        }

        public void MarkDirty(bool markSettings = true, bool markComponent = true, bool markRuntimeSettings = false)
        {
            if (markSettings) EditorUtility.SetDirty(settings);
            if (markComponent) {
                if (spSequence != null) spSequence.serializedObject.ApplyModifiedProperties();
                if (src != null) EditorUtility.SetDirty(src);
            }
            if (markRuntimeSettings) EditorUtility.SetDirty(runtimeSettings);
        }

        #endregion

        #region Callbacks

        void OnRecorderOrPreviewStopped()
        {
            isRecorderOrPreviewUndoPass = true;
            string currSequenceGuid = sequence.guid;
            settings.ReapplySelected(out src, out sequence, out spSequence);
            if (sequence != null && sequence.guid != currSequenceGuid) {
                Event.current.Use();
                ShowWindow(src, sequence, spSequence);
            }
        }

        #endregion
    }
}