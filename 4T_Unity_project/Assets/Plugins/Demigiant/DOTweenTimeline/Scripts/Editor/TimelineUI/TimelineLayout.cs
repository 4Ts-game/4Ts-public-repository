// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/01/18

using DG.Tweening.Timeline;
using UnityEngine;

namespace DG.Tweening.TimelineEditor
{
    internal class TimelineLayout
    {
        public Vector2Int partialOffset;
        public int firstVisibleLayerIndex;
        public int totVisibleLayers;
        public int visibleTimelineHeight;
        public int visibleLayersDrawLoopLength;
        public float firstVisibleTime, lastVisibleTime;

        DOTimelineSettings _settings { get { return DOVisualSequenceTimeline.settings; } }
        Component _src { get { return DOVisualSequenceTimeline.src; } }
        DOVisualSequence _sequence { get { return DOVisualSequenceTimeline.sequence; } }
        protected Vector2Int timelineShift { get { return _sequence.editor.roundedAreaShift; } }

        public void Refresh(Rect timelineRect)
        {
            partialOffset = new Vector2Int(
                -(_sequence.editor.roundedAreaShift.x < 0 ? -(_sequence.editor.roundedAreaShift.x % _settings.secondToPixels) : 0), // can only be negative
                -(_sequence.editor.roundedAreaShift.y < 0 ? -_sequence.editor.roundedAreaShift.y % _settings.layerHeight : 0) // can only be negative
            );
            firstVisibleLayerIndex = -_sequence.editor.roundedAreaShift.y / _settings.layerHeight;
            totVisibleLayers = _sequence.layers.Length - firstVisibleLayerIndex;
            visibleLayersDrawLoopLength = totVisibleLayers + firstVisibleLayerIndex;
            visibleTimelineHeight = totVisibleLayers * _settings.layerHeight + partialOffset.y;
            firstVisibleTime = -timelineShift.x / (float)_settings.secondToPixels;
            lastVisibleTime = firstVisibleTime + timelineRect.width / _settings.secondToPixels;
        }

        /// <summary>
        /// If <see cref="clamp"/> is TRUE returns an index within the min/max range of available indexes even if the mouse is outside,
        /// otherwise returns -1 in those cases.<para/>
        /// Returns -1 with clamp if there's no layers.
        /// </summary>
        public int GetLayerIndexAtMouse(bool clamp = false)
        {
            if (_sequence.layers == null) return -1;
            int len = _sequence.layers.Length;
            if (len == 0) return -1;
            float mouseY = Event.current.mousePosition.y;
            for (int i = firstVisibleLayerIndex; i < len; ++i) {
                int layerMaxY = partialOffset.y + _settings.layerHeight * (i - firstVisibleLayerIndex) + _settings.layerHeight;
                if (mouseY < layerMaxY) return i;
            }
            return clamp ? len - 1 : -1;
        }

        public float GetSecondsAtMouse(bool applyMinSnapping = true)
        {
            float result = firstVisibleTime + Event.current.mousePosition.x / _settings.secondToPixels;
            if (applyMinSnapping) result -= result % DOTimelineSettings.MinSequencedSnapping;
            return result;
        }

        public float GetTimelineXAtTime(float time)
        {
            float relativeTime = time + timelineShift.x / (float)_settings.secondToPixels;
            return relativeTime * _settings.secondToPixels;
        }
    }
}