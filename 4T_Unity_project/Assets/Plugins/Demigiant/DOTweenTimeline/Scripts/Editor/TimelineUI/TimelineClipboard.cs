// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/02/08

using System.Collections.Generic;
using DG.Tweening.Timeline;
using DG.Tweening.Timeline.Core;

namespace DG.Tweening.TimelineEditor
{
    internal static class TimelineClipboard
    {
        public static readonly List<SequencedCopy> SequencedCopies = new List<SequencedCopy>();

        #region Public Methods

        public static void CopySequenceds(DOVisualSequence fromSequence, List<DOVisualSequenced> sequencedsToCopy)
        {
            SequencedCopies.Clear();
            if (sequencedsToCopy == null) return;
            float earlierStartTime = float.MaxValue;
            int topLayerIndex = int.MaxValue;
            foreach (DOVisualSequenced sequenced in sequencedsToCopy) {
                SequencedCopy copy = new SequencedCopy(fromSequence, sequenced);
                SequencedCopies.Add(copy);
                if (sequenced.startTime < earlierStartTime) earlierStartTime = sequenced.startTime;
                if (copy.layerIndex < topLayerIndex) topLayerIndex = copy.layerIndex;
            }
            // Set offsetWhenPasting
            foreach (SequencedCopy sCopy in SequencedCopies) {
                sCopy.startTimeOffsetFromFirst = sCopy.sequenced.startTime - earlierStartTime;
                sCopy.layerIndexOffsetFromUpper = sCopy.layerIndex - topLayerIndex;
            }
        }

        public static bool HasMemorizedSequenceds()
        {
            return SequencedCopies.Count > 0;
        }

        public static int TotMemorizedSequenceds()
        {
            return SequencedCopies.Count;
        }

        #endregion

        // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████
        // ███ INTERNAL CLASSES ████████████████████████████████████████████████████████████████████████████████████████████████
        // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

        public class SequencedCopy
        {
            public string originalGuid;
            public int layerIndex;
            public DOVisualSequenced sequenced;
            public float startTimeOffsetFromFirst; // Set directly
            public int layerIndexOffsetFromUpper; // Set directly

            public SequencedCopy(DOVisualSequence sequence, DOVisualSequenced sequenced)
            {
                this.originalGuid = sequenced.guid;
                this.layerIndex = sequence.FindSequencedLayerIndexByGuid(sequenced.guid);
                this.sequenced = sequenced.Editor_Clone(false);
            }

            public DOVisualSequenced GenerateCopy(bool regenerateGuid)
            {
                return sequenced.Editor_Clone(regenerateGuid);
            }
        }
    }
}