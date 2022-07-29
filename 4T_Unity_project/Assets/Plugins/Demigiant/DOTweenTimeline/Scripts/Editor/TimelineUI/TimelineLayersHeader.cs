// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/01/17

using DG.DemiEditor;
using DG.DemiLib;
using DG.Tweening.Timeline;
using UnityEditor;
using UnityEngine;

namespace DG.Tweening.TimelineEditor
{
    internal class TimelineLayersHeader : ABSTimelineElement
    {
        readonly GUIContent _gcPreview = new GUIContent("Preview ►", "Note that only Tweens (excluding Global Tweens) will be previewed");
        readonly GUIContent _gcStopPreview = new GUIContent("Stop ■");
        readonly GUIContent _gcStartRecording = new GUIContent("",
            "Sets Unity in a temporary record-mode, where all changes except to DOTween Timeline Sequences will be reset when you exit it." +
            " The Preview system will also behave differently during record-mode." +
            "\nNOTE: this button is experimental and is enabled/disabled in the Timeline's settings."
        );
        readonly GUIContent _gcStopRecording = new GUIContent("", "Exit the temporary edit-mode");
        readonly GUIContent _gcAdd = new GUIContent("+", "Add Layer");
        readonly Color _controlsBgColor = new Color(0.41f, 0.41f, 0.41f);
        readonly Color _controlsColor = new Color(0.89f, 0.89f, 0.89f);

        #region GUI

        public void Refresh() {}

        public override void Draw(Rect drawArea)
        {
            if (Event.current.type == EventType.Layout) return;

            base.Draw(drawArea);

            Rect btAddR = area.ShiftXAndResize(area.width - 18);
            Rect cntrBtsR = area.SetWidth(108).SetCenterX(btAddR.x * 0.5f);
            Rect btPreviewR = cntrBtsR.SetWidth(80);
            Rect btEditModeR = cntrBtsR.ShiftXAndResize(btPreviewR.width);

            // Background
            DeGUI.DrawColoredSquare(area, new DeSkinColor(0.02f));

            // Content
            if (area.width > 50) {
                // Preview
                bool isPlaying = DOTimelinePreviewManager.isPlayingOrPreviewing;
                using (new EditorGUI.DisabledScope(Application.isPlaying)) {
                    if (DeGUI.ActiveButton(btPreviewR, isPlaying ? _gcStopPreview : _gcPreview, _controlsBgColor, DOEGUI.Styles.timeline.hLayerControlsBt)) {
                        if (isPlaying) DOTimelinePreviewManager.StopPreview();
                        else DOTimelinePreviewManager.StartPreview(editor, sequence);
                    }
                    // Edit-mode
                    using (new EditorGUI.DisabledScope(!settings.experimental.enableRecordMode || !DOTimelineRecorder.isRecording && DOTimelinePreviewManager.isPlayingOrPreviewing)) {
                        GUIContent label = DOTimelineRecorder.isRecording ? _gcStopRecording : _gcStartRecording;
                        if (DeGUI.ActiveButton(btEditModeR, label, DOTimelineRecorder.isRecording ? Color.red : _controlsBgColor, DOEGUI.Styles.timeline.hLayerControlsBt)) {
                            if (DOTimelineRecorder.isRecording) DOTimelineRecorder.ExitRecordMode(true);
                            else DOTimelineRecorder.EnterRecordMode(sequence);
                        }
                        using (new DeGUI.ColorScope(null, null, DOEGUI.Styles.timeline.hLayerControlsBt.normal.textColor)) {
                            GUI.DrawTexture(new Rect(0, 0, 8, 8).SetCenter(btEditModeR.center.x, btEditModeR.center.y), DeStylePalette.circle);
                        }
                    }
                }
            }

            // Add layer
            using (new EditorGUI.DisabledScope(Application.isPlaying || DOTimelinePreviewManager.isPlayingOrPreviewing)) {
                if (DOEGUI.Button(btAddR, _gcAdd)) {
                    DeEditorUtils.Array.ExpandAndAdd(ref sequence.layers, new DOVisualSequence.VisualLayer("Layer " + (sequence.layers.Length + 1)));
                    GUI.changed = true;
                }
            }
        }

        #endregion
    }
}