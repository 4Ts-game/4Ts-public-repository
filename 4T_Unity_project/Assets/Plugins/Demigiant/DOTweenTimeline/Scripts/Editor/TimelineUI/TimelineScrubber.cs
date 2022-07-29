// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/01/25

using DG.DemiEditor;
using DG.Tweening.Timeline;
using DG.Tweening.Timeline.Core;
using UnityEditor;
using UnityEngine;

namespace DG.Tweening.TimelineEditor
{
    internal class TimelineScrubber : ABSTimelineElement
    {
        readonly GUIContent _gcRewind = new GUIContent("■");
        readonly GUIContent _gcPlay = new GUIContent("►");
        readonly GUIContent _gcWaitingToRegenerate = new GUIContent("Regeneration Pending");
        readonly Color _btColor = Color.red;
        readonly Color _btContentColor = Color.white;
        Rect _scrubArea;
        bool _scrubbing;
        bool _wasPreviewActive;

        #region GUI

        public void Refresh() {}

        public override void Draw(Rect drawArea)
        {
            if (Event.current.type == EventType.Layout) return;

            base.Draw(drawArea);

            if (Application.isPlaying || DOTimelinePreviewManager.isPlayingOrPreviewing) {
                DrawPlayMode();
                if (Application.isPlaying) return;
            }

            _scrubArea = area.SetHeight(18);

            switch (Event.current.rawType) {
            case EventType.MouseUp:
                if (!_scrubbing) break;
                _scrubbing = false;
                if (DOTimelineRecorder.isRecording) break;
                if (_wasPreviewActive) DOTimelinePreviewManager.ResumePreview();
                if (!_wasPreviewActive) DOTimelinePreviewManager.StopPreview();
                return;
            case EventType.MouseDrag:
                if (_scrubbing) UpdateScrubber();
                break;
            }

            switch (Event.current.type) {
            case EventType.MouseDown:
                if (_scrubArea.Contains(Event.current.mousePosition)) {
                    _wasPreviewActive = DOTimelinePreviewManager.isPlayingOrPreviewing;
                    if (!_wasPreviewActive) {
                        Tween t = DOTimelinePreviewManager.StartPreview(editor, sequence, false);
                        if (t == null) break;
                    }
                    _scrubbing = true;
                    UpdateScrubber();
                }
                break;
            }
        }

        void DrawPlayMode()
        {
            if (sequence.tween == null || !sequence.tween.IsActive()) return;

            float duration = Application.isPlaying && sequence.timeMode == DOVisualSequence.TimeMode.DurationOverload
                ? TimelineUtils.GetSequenceDuration(sequence)
                : sequence.tween.Duration(false);
            float sequenceTime = sequence.tween.ElapsedDirectionalPercentage() * duration;
            float tweenX = timelineShift.x + sequenceTime * settings.secondToPixels;
            // Delay info
            float delay = sequence.tween.Delay();
            float elapsedDelay = sequence.tween.ElapsedDelay();
            if (delay > 0 && elapsedDelay < delay) {
                Rect delayR = new Rect(tweenX + 3, area.y, 200, EditorGUIUtility.singleLineHeight);
                GUI.Label(delayR, string.Format("Waiting for delay: {0:0.00}", delay - elapsedDelay), DOEGUI.Styles.timeline.scrubberWaitLabel);
            }
            //
            DeGUI.DrawColoredSquare(new Rect(tweenX, area.y, 1, area.height), DOTimelineRecorder.isRecording ? Color.red : Color.cyan);
            if (DOTimelineRecorder.isRecording) {
                const int btSize = 14;
                Rect rewindR = new Rect(tweenX - btSize - 2, 0, btSize, btSize).SetCenterY(area.y + 8);
                Rect playR = rewindR.SetX(tweenX + 3);
                using (new DeGUI.ColorScope(_btColor, _btContentColor)) {
                    if (GUI.Button(rewindR, _gcRewind, DOEGUI.Styles.timeline.scrubberBtStop)) {
                        DOTimelinePreviewManager.StopPreview();
                    }
                    if (GUI.Button(playR, _gcPlay, DOEGUI.Styles.timeline.scrubberBtPlay)) {
                        DOTimelinePreviewManager.ResumePreview();
                        GUI.changed = false;
                    }
                }
                if (DOTimelinePreviewManager.waitingToRegenerateTween) {
                    Rect waitingR = playR.SetX(playR.xMax + 2).SetWidth(200);
                    GUI.Label(waitingR, _gcWaitingToRegenerate, DOEGUI.Styles.timeline.scrubberWarningLabel);
                }
            }
        }

        void UpdateScrubber()
        {
            float atTime = (-timelineShift.x + Event.current.mousePosition.x) / settings.secondToPixels;
            DOTimelinePreviewManager.Goto(atTime);
        }

        #endregion
    }
}