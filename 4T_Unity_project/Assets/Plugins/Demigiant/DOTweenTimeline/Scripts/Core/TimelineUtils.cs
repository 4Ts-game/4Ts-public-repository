// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/04/08

namespace DG.Tweening.Timeline.Core
{
    public static class TimelineUtils
    {
        #region Public Methods

        public static float GetSequenceDuration(DOVisualSequence sequence, bool includeSequencedsLoops = true)
        {
            float endTime = 0;
            foreach (DOVisualSequenced sequenced in sequence.sequenceds) {
                float sEndTime = sequenced.startTime + sequenced.duration * (includeSequencedsLoops ? GetSequencedPositiveLoopValue(sequenced) : 1);
                if (sEndTime > endTime) endTime = sEndTime;
            }
            return endTime;
        }

        public static int GetSequencedPositiveLoopValue(DOVisualSequenced sequenced)
        {
            switch (sequenced.loops) {
            case 0: return 1;
            case -1: return int.MaxValue;
            default: return sequenced.loops;
            }
        }

        #endregion
    }
}