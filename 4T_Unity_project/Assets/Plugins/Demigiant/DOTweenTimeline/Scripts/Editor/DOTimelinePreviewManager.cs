// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/01/31

using System;
using System.Collections;
using DG.DemiEditor;
using DG.DOTweenEditor;
using DG.Tweening.Timeline;
using DG.Tweening.Timeline.Core;
using DG.Tweening.TimelineEditor;
using UnityEditor;
using UnityEditor.SceneManagement;
using UnityEngine;

namespace DG.Tweening.TimelineEditor
{
    internal static class DOTimelinePreviewManager
    {
        // ■■■ EVENTS
        public static event Action OnStopPreviewing;
        static void Dispatch_OnStopPreviewing() { if (OnStopPreviewing != null) OnStopPreviewing(); }
        // ■■■

        public static bool isPlayingOrPreviewing { get; private set; }
        public static bool isPlaying { get; private set; }
        public static bool waitingToRegenerateTween { get; private set; } // TRUE if waiting to finish editing a field before regenerating

        static DOVisualSequenceTimeline _timelineEditor;
        static DOVisualSequence _currSequence;
        static Tween _currTween;
        static float _currTimeMultiplier; // Either timeScale or multiplier based on durationOverload
        static readonly DeStopwatch _Sw = new DeStopwatch();
        static int _undoIndexBeforePreview = -1;
        static bool _isWaitingToRegenerateAfterUndoRedo;

        #region Public Methods

        /// <summary>
        /// Generates the preview tween and returns it, or NULL if none was generated
        /// </summary>
        public static Tween StartPreview(
            DOVisualSequenceTimeline timelineEditor, DOVisualSequence sequence, bool andPlay = true
        ){
            Undo.IncrementCurrentGroup();
            TimelineEditorUtils.RegisterCompleteSceneUndo("DOTimelinePreviewManager");
            if (_undoIndexBeforePreview == -1) {
                _undoIndexBeforePreview = Undo.GetCurrentGroup();
            }
            if (DOTimelineRecorder.isRecording) {
                // Move up the undo count and re-record everything,
                // otherwise if user undoes an operation while the timeline is in use all objects will be set to the tween's startup state
                // (only solution I found after scratching my head on this for a long-ass time)
                Undo.IncrementCurrentGroup();
                TimelineEditorUtils.RegisterCompleteSceneUndo("DOTimelinePreviewManager");
            }

            _currTween = sequence.GenerateTween();
            if (_currTween == null) return null;

            isPlayingOrPreviewing = true;
            _timelineEditor = timelineEditor;
            _currSequence = sequence;
            _currTimeMultiplier = _currSequence.timeMode == DOVisualSequence.TimeMode.TimeScale
                ? _currSequence.timeScale
                : TimelineUtils.GetSequenceDuration(_currSequence) / _currSequence.durationOverload;
            AddCallbacks();
            DOTweenEditorPreview.PrepareTweenForPreview(_currTween, true, true, false);
            DOTweenEditorPreview.Start();
            if (andPlay) Play(true);
            return _currTween;
        }

        /// <summary>
        /// Send the preview to the given time
        /// </summary>
        /// <param name="time"></param>
        /// <param name="andPlay"></param>
        public static void Goto(float time, bool andPlay = false)
        {
            if (time < 0) time = 0;
            float timeDescaled = time / _currTimeMultiplier;
            float currElapsed = _Sw.elapsed;
            _Sw.Goto(timeDescaled, andPlay);
            _currTween.Goto(time);
            if (!andPlay) Pause();
            else Play();
            if (Mathf.Approximately(currElapsed, timeDescaled)) return;
            _timelineEditor.Repaint();
        }

        /// <summary>
        /// Pauses the preview tween
        /// </summary>
        public static void PausePreview()
        {
            if (_currTween != null) Pause();
        }

        /// <summary>
        /// Resumes the preview tween
        /// </summary>
        public static void ResumePreview()
        {
            if (_currTween != null) Play();
        }

        /// <summary>
        /// Stops the preview tween and resets all
        /// </summary>
        public static void StopPreview()
        {
            if (_currTween != null) Stop();
        }

        #endregion

        #region Methods

        static void AddCallbacks()
        {
            EditorApplication.projectChanged += StopPreview;
            EditorSceneManager.sceneOpening += OnSceneOpening;
            EditorApplication.playModeStateChanged += OnPlayModeStateChanged;
            DOVisualSequenceTimeline.OnSequenceChanged += OnSequenceChanged;
            DOVisualSequenceTimeline.OnMouseDown += OnTimelineMouseDown;
            DOTimelineRecorder.OnStopRecording += OnRecorderStopped;
            Undo.undoRedoPerformed += OnUndoRedoPerformed;
            EditorApplication.update += OnEditorUpdate;
        }

        static void RemoveCallbacks()
        {
            EditorApplication.projectChanged -= StopPreview;
            EditorSceneManager.sceneOpening -= OnSceneOpening;
            EditorApplication.playModeStateChanged -= OnPlayModeStateChanged;
            DOVisualSequenceTimeline.OnSequenceChanged -= OnSequenceChanged;
            DOVisualSequenceTimeline.OnMouseDown -= OnTimelineMouseDown;
            DOTimelineRecorder.OnStopRecording -= OnRecorderStopped;
            Undo.undoRedoPerformed -= OnUndoRedoPerformed;
            EditorApplication.update -= OnEditorUpdate;
        }

        static void Play(bool fromBeginning = false)
        {
            isPlaying = true;
            if (fromBeginning) _Sw.Reset();
            _Sw.Start();
        }

        static void Pause()
        {
            isPlaying = false;
            _Sw.Stop();
        }

        static void Stop()
        {
            isPlayingOrPreviewing = isPlaying = waitingToRegenerateTween = false;
            RemoveCallbacks();
            bool isRecording = DOTimelineRecorder.isRecording;
            _Sw.Stop();
            DOTweenEditorPreview.Stop(true);
            _timelineEditor = null;
            _currSequence.KillTween();
            _currSequence = null;
            _currTween = null;
            if (_undoIndexBeforePreview > 0) {
                if (isRecording) DOTimelineRecorder.StoreSnapshot();
                Undo.RevertAllDownToGroup(_undoIndexBeforePreview);
                if (!DOTimelineRecorder.isRecording) _undoIndexBeforePreview = -1;
                if (isRecording) DOTimelineRecorder.ApplySnapshot();
            }
            Dispatch_OnStopPreviewing();
            if (Event.current != null) DeGUI.ExitCurrentEvent();
        }

        static void Update(bool applyTimeMultiplier)
        {
            if (_isWaitingToRegenerateAfterUndoRedo) return;
            float timeMultiplier = applyTimeMultiplier ? _currTimeMultiplier : 1;
            _currTween.Goto(_Sw.elapsed * timeMultiplier);
            _timelineEditor.Repaint();
        }

        // Assumes the Timeline is recording
        static void Regenerate(bool andPlay)
        {
            bool isRecording = DOTimelineRecorder.isRecording;
            Transform currSelection = isRecording ? Selection.activeTransform : null;
            float elapsed = _Sw.elapsed;
            waitingToRegenerateTween = false;
            if (_currTween == null) return;
            if (IsEditingField()) {
//                Debug.Log("<color=#ff0000>" + Time.frameCount + " DELAYING REGEN " + (_currTween != null) + "</color> "
//                          + GUIUtility.hotControl + "/" + GUIUtility.keyboardControl + "/" + GUIUtility.GetControlID(FocusType.Keyboard) + "/" + GUIUtility.GetControlID(FocusType.Passive));
                waitingToRegenerateTween = true;
                return;
            }
//            Debug.Log("<color=#00ff00>" + Time.frameCount + " REGENERATE " + (_currTween != null) + "</color> "
//                      + GUIUtility.hotControl + "/" + GUIUtility.keyboardControl + "/" + GUIUtility.GetControlID(FocusType.Keyboard) + "/" + GUIUtility.GetControlID(FocusType.Passive));
            Stop();
            if (isRecording && currSelection != null) Selection.activeTransform = currSelection;
            StartPreview(DOVisualSequenceTimeline.editor, DOVisualSequenceTimeline.sequence, false);
            Goto(elapsed, andPlay);
        }

        static bool IsEditingField()
        {
            return GUIUtility.hotControl == 0 && GUIUtility.keyboardControl > 0
                   && GUIUtility.GetControlID(FocusType.Keyboard) == -1
                   && GUIUtility.GetControlID(FocusType.Passive) == -1;
        }

        #endregion

        #region Callbacks

        static void OnEditorUpdate()
        {
            if (isPlaying) Update(true);
        }

        static void OnTimelineMouseDown()
        {
            if (waitingToRegenerateTween && !IsEditingField()) {
//                Debug.Log(Time.frameCount + " DELAYED REGEN");
                waitingToRegenerateTween = false;
                GUI.FocusControl(null);
                Regenerate(isPlaying);
            }
        }

        static void OnPlayModeStateChanged(PlayModeStateChange stateChange)
        {
            StopPreview();
        }

        static void OnSceneOpening(string path, OpenSceneMode mode)
        {
            StopPreview();
        }

        static void OnSequenceChanged(DOVisualSequence sequence)
        {
            if (DOTimelineRecorder.isRecording) Regenerate(isPlaying);
        }

        static void OnRecorderStopped()
        {
            _undoIndexBeforePreview = -1;
        }

        static void OnUndoRedoPerformed()
        {
            if (!DOTimelineRecorder.isRecording || !isPlayingOrPreviewing) return;
            _isWaitingToRegenerateAfterUndoRedo = true;
            DeEditorCoroutines.StartCoroutine(CO_OnUndoRedoPerformed());
        }

        static IEnumerator CO_OnUndoRedoPerformed()
        {
            // Delayed otherwise undoing while Unity undoes will throw an error
            yield return null;
            _isWaitingToRegenerateAfterUndoRedo = false;
            Regenerate(isPlaying);
        }

        #endregion
    }
}