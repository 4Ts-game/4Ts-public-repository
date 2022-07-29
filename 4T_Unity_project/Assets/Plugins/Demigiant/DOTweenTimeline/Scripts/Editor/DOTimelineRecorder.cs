// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/02/10

using System;
using System.Collections.Generic;
using DG.Tweening.Timeline;
using DG.Tweening.TimelineEditor;
using UnityEditor;
using UnityEditor.SceneManagement;
using UnityEngine;

namespace DG.Tweening.TimelineEditor
{
    /// <summary>
    /// Not really a recorder but more an edit-mode controller, but recorder is shorter
    /// </summary>
    internal static class DOTimelineRecorder
    {
        // ■■■ EVENTS
        public static event Action OnStopRecording;
        static void Dispatch_OnStopRecording() { if (OnStopRecording != null) OnStopRecording(); }
        // ■■■

        public static bool isRecording { get; private set; }

        static DOVisualSequenceTimeline _editor { get { return DOVisualSequenceTimeline.editor; } }
        static DOTimelineSettings _settings { get { return DOVisualSequenceTimeline.settings; } }

        #region Public Methods

        public static void EnterRecordMode(DOVisualSequence sequence)
        {
            if (isRecording) return;
            isRecording = true;
            TimelineEditorUtils.RegisterCompleteSceneUndo("DOTimelineRecorder");
            int currUndoGroup = Undo.GetCurrentGroup();
            using (new DOScope.NonUndoableSettingsSerialization()) {
                _settings.recorderData.undoIndexBeforeRecording = currUndoGroup;
                _settings.recorderData.recordedSequences.Clear();
                AddSequence(sequence);
            }
            AddCallbacks();
        }

        public static void ExitRecordMode(bool applyRecordedChanges)
        {
            if (!isRecording) return;
            isRecording = false;
            if (applyRecordedChanges) StoreSnapshot();
            if (DOTimelinePreviewManager.isPlayingOrPreviewing) DOTimelinePreviewManager.StopPreview();
//            Debug.Log("REC ► REVERT TO " + _settings.recorderData.undoIndexBeforeRecording);
            Undo.RevertAllDownToGroup(_settings.recorderData.undoIndexBeforeRecording);
            if (applyRecordedChanges) ApplySnapshot();
            using (new DOScope.NonUndoableSettingsSerialization()) {
                _settings.recorderData.recordedSequences.Clear();
                _settings.recorderData.tmpClonedSequences.Clear();
            }
            RemoveCallbacks();
            Dispatch_OnStopRecording();
            DOVisualSequenceTimeline.editor.Repaint();
        }

        public static void StoreSnapshot()
        {
            using (new DOScope.NonUndoableSettingsSerialization()) {
                _settings.RefreshSelected(DOVisualSequenceTimeline.src, DOVisualSequenceTimeline.sequence, false);
                _settings.StoreSelectedSnapshot();
                _settings.recorderData.tmpClonedSequences.Clear();
                for (int i = 0; i < _settings.recorderData.recordedSequences.Count; ++i) {
                    _settings.recorderData.tmpClonedSequences.Add(_settings.recorderData.recordedSequences[i].Editor_Clone(false));
                }
            }
        }

        public static void ApplySnapshot()
        {
            using (new DOScope.NonUndoableSettingsSerialization()) {
                for (int i = 0; i < _settings.recorderData.recordedSequences.Count; ++i) {
                    _settings.recorderData.recordedSequences[i].Editor_AssignPropertiesFrom(_settings.recorderData.tmpClonedSequences[i], true);
                }
                _settings.ReapplySelectedSnapshot();
            }
        }

        #endregion

        #region Methods

        static void AddCallbacks()
        {
            RemoveCallbacks();
            DOVisualSequenceTimeline.OnSequenceOpened += OnSequenceOpened;
            EditorSceneManager.sceneOpening += OnSceneOpening;
            EditorApplication.playModeStateChanged += OnPlayModeStateChanged;
            AssemblyReloadEvents.beforeAssemblyReload += OnBeforeAssemblyReload;
        }

        static void RemoveCallbacks()
        {
            DOVisualSequenceTimeline.OnSequenceOpened -= OnSequenceOpened;
            EditorSceneManager.sceneOpening -= OnSceneOpening;
            EditorApplication.playModeStateChanged -= OnPlayModeStateChanged;
            AssemblyReloadEvents.beforeAssemblyReload -= OnBeforeAssemblyReload;
        }

        static void AddSequence(DOVisualSequence sequence)
        {
            if (_settings.recorderData.recordedSequences.Contains(sequence)) return;
            using (new DOScope.NonUndoableSettingsSerialization()) {
                _settings.recorderData.recordedSequences.Add(sequence);
            }
        }

        #endregion

        #region Callbacks

        static void OnSequenceOpened(DOVisualSequence sequence)
        {
            if (!isRecording) return;
            AddSequence(sequence);
        }

        static void OnPlayModeStateChanged(PlayModeStateChange stateChange)
        {
            ExitRecordMode(true);
        }

        static void OnSceneOpening(string path, OpenSceneMode mode)
        {
            ExitRecordMode(true);
        }

        static void OnBeforeAssemblyReload()
        {
            ExitRecordMode(true);
        }

        #endregion
    }
}