// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/01/17

using DG.DemiEditor;
using DG.DemiLib;
using DG.Tweening.Timeline.Core;
using UnityEngine;

namespace DG.Tweening.TimelineEditor
{
    internal class TimelineHeader : ABSTimelineElement
    {
        readonly Color _labelColor = new DeSkinColor(0.45f);
        readonly Color _labelStepColor = new DeSkinColor(0.7f);
        readonly Color _fullDurationColor = new Color(0f, 1f, 1f, 0.5f);
        readonly Color _noLoopsDurationColor = Color.cyan;

        #region GUI

        public void Refresh() {}

        public override void Draw(Rect drawArea)
        {
            if (Event.current.type == EventType.Layout) return;

            base.Draw(drawArea);

            DeGUI.DrawColoredSquare(area, new DeSkinColor(0.1f));
            // Sequence duration color
            float fullDuration = TimelineUtils.GetSequenceDuration(sequence);
            float noLoopsDuration = TimelineUtils.GetSequenceDuration(sequence, false);
            float visibleFullDuration = fullDuration + timelineShift.x / (float)settings.secondToPixels;
            float visibleNoLoopsDuration = noLoopsDuration + timelineShift.x / (float)settings.secondToPixels;
            if (visibleFullDuration > 0) {
                Rect durationR = new Rect(area.x, area.y, visibleFullDuration * settings.secondToPixels, 2);
                DeGUI.DrawColoredSquare(durationR, _fullDurationColor);
            }
            if (visibleNoLoopsDuration > 0) {
                Rect durationR = new Rect(area.x, area.y, visibleNoLoopsDuration * settings.secondToPixels, 2);
                DeGUI.DrawColoredSquare(durationR, _noLoopsDurationColor);
            }
            // Vertical separators (separators)
            float fullWidth = area.width - layout.partialOffset.x;
            int totColumns = Mathf.CeilToInt(fullWidth / settings.secondToPixels);
            int firstColIndex = -timelineShift.x / settings.secondToPixels;
            int labelW = (int)(settings.secondToPixels * 0.5f) + 8;
            Rect colR = new Rect(layout.partialOffset.x - (int)(labelW * 0.5f), area.y, labelW, area.height);
            for (int i = 0; i < totColumns; ++i) {
                int seconds = firstColIndex + i;
                string label = seconds < 60 ? seconds.ToString() : string.Format("{0}:{1:00}", seconds / 60, seconds % 60);
                using (new DeGUI.ColorScope(null, seconds % 5 == 0 ? _labelStepColor : _labelColor)) {
                    GUI.Label(colR, label, settings.secondToPixels > 20 ? DOEGUI.Styles.timeline.headerTimeLabel : DOEGUI.Styles.timeline.headerTimeLabelSml);
                }
                colR = colR.Shift(settings.secondToPixels, 0, 0, 0);
            }
        }

        #endregion
    }
}