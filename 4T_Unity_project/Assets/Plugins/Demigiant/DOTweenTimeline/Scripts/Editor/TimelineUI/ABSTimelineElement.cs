// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/01/17

using DG.Tweening.Timeline;
using DG.Tweening.Timeline.Core;
using UnityEditor;
using UnityEngine;

namespace DG.Tweening.TimelineEditor
{
    internal class ABSTimelineElement
    {
        protected DOVisualSequenceTimeline editor { get { return DOVisualSequenceTimeline.editor; } }
        protected TimelineLayout layout { get { return DOVisualSequenceTimeline.Layout; } }
        protected DOTimelineSettings settings { get { return DOVisualSequenceTimeline.settings; } }
        protected DOVisualSequenceSettings runtimeSettings { get { return DOVisualSequenceTimeline.runtimeSettings; } }
        protected Component src { get { return DOVisualSequenceTimeline.src; } }
        protected DOVisualSequence sequence { get { return DOVisualSequenceTimeline.sequence; } }
        protected SerializedProperty spSequence { get { return DOVisualSequenceTimeline.spSequence; } }
        protected Vector2Int timelineShift {
            get { return sequence.editor.roundedAreaShift; }
            set {
                sequence.editor.roundedAreaShift = value;
                GUI.changed = true;
            }
        }
        protected bool isUndoRedoPass { get { return DOVisualSequenceTimeline.isUndoRedoPass; } }
        protected bool isRecorderStoppedPass { get { return !DOTimelineRecorder.isRecording && DOVisualSequenceTimeline.isRecorderOrPreviewUndoPass; } }
        protected Rect area { get; set; }

        public virtual void Draw(Rect drawArea)
        {
            this.area = drawArea;
        }
    }
}