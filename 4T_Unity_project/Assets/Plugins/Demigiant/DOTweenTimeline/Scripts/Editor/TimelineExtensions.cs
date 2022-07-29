// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/02/02

using System;
using DG.Tweening.Timeline;
using DG.Tweening.Timeline.Core;
using UnityEngine;

namespace DG.Tweening.TimelineEditor
{
    internal static class TimelineExtensions
    {
        #region Public Methods

        #region Sequence

        public static int Editor_GetSequencedIndex(this DOVisualSequence sequence, string sequencedGuid)
        {
            for (int i = 0; i < sequence.sequenceds.Length; ++i) {
                if (sequence.sequenceds[i].guid == sequencedGuid) return i;
            }
            return -1;
        }

        public static DOVisualSequence.VisualLayer Editor_GetSequencedLayer(this DOVisualSequence sequence, string sequencedGuid)
        {
            foreach (DOVisualSequence.VisualLayer layer in sequence.layers) {
                for (int i = 0; i < layer.sequencedGuids.Length; ++i) {
                    if (layer.sequencedGuids[i] == sequencedGuid) return layer;
                }
            }
            return null;
        }

        #endregion

        #region Sequenced

        public static float Editor_DrawDuration(this DOVisualSequenced sequenced)
        {
            switch (sequenced.type) {
            case DOVisualSequenced.Type.Event:
            case DOVisualSequenced.Type.Action: return DOVisualSequenceTimeline.settings.actionsLayoutDuration;
            default: return Mathf.Max(sequenced.duration, 0.1f);
            }
        }

        public static string Editor_GetShortName(this Type t)
        {
            string result = t.ToString();
            int index = result.LastIndexOf('.');
            return index == -1 ? result : result.Substring(index + 1);
        }

        public static bool Editor_HasDuration(this DOVisualSequenced sequenced)
        {
            switch (sequenced.type) {
            case DOVisualSequenced.Type.GlobalTween:
            case DOVisualSequenced.Type.Tween:
            case DOVisualSequenced.Type.Interval:
                return true;
            default:
                return false;
            }
        }

        public static bool Editor_HasMultipleLoops(this DOVisualSequenced sequenced)
        {
            switch (sequenced.loops) {
            case -1: return true;
            case 0: return false;
            case 1: return false;
            default: return true;
            }
        }

        public static int Editor_PositiveLoopValue(this DOVisualSequenced sequenced)
        {
            return TimelineUtils.GetSequencedPositiveLoopValue(sequenced);
        }

        #endregion

        #endregion
    }
}