// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/01/19

using System.Collections.Generic;
using DG.DemiEditor;
using DG.Tweening.Timeline;
using DG.Tweening.Timeline.Core;
using UnityEngine;

namespace DG.Tweening.TimelineEditor
{
    internal static class TimelineSelection
    {
        static DOTimelineSettings _settings { get { return DOVisualSequenceTimeline.settings; } }
        static Component _src { get { return DOVisualSequenceTimeline.src; } }
        static DOVisualSequence _sequence { get { return DOVisualSequenceTimeline.sequence; } }

        public static readonly List<SelectedSequenced> Sequenceds = new List<SelectedSequenced>(); // Last selected is always first element
        public static int totSequenceds { get; private set; }
        public static bool containsSequenceds { get { return totSequenceds > 0; } }
        public static bool isDraggingSelection; // Set by TimelineMain
        public static bool isDraggingSequenceds; // Set by TimelineMain
        public static bool isDraggingDuration; // Set by TimelineMain

        #region Public Methods

        /// <summary>
        /// Clears the selection without recording an undo
        /// </summary>
        public static void Clear()
        {
            Sequenceds.Clear();
            totSequenceds = 0;
        }

        public static void Select(
            DOVisualSequenced sequenced, bool add = false, bool refreshSettings = true,
            float? forcedOriginalStartTime = null, float? forcedOriginalDuration = null, int? forcedOriginalLayerIndex = null
        ){
            if (!add) Clear();
            int existingIndex = IndexOf(sequenced);
            if (existingIndex != -1) {
                DeEditorUtils.List.Shift(Sequenceds, existingIndex, 0);
                return;
            }
            SelectedSequenced sel = new SelectedSequenced(sequenced, forcedOriginalStartTime, forcedOriginalDuration, forcedOriginalLayerIndex);
            Sequenceds.Insert(0, sel);
            totSequenceds++;
            if (refreshSettings) _settings.RefreshSelected(_src, _sequence);
        }

        public static void SelectAllIn(DOVisualSequence sequence, bool includeLockedLayers)
        {
            Clear();
            for (int i = 0; i < sequence.sequenceds.Length; ++i) {
                if (!includeLockedLayers && sequence.layers[sequence.FindSequencedLayerIndexByGuid(sequence.sequenceds[i].guid)].locked) continue;
                SelectedSequenced sel = new SelectedSequenced(sequence.sequenceds[i]);
                Sequenceds.Add(sel);
                totSequenceds++;
            }
            _settings.RefreshSelected(_src, _sequence);
        }

        public static void Deselect(DOVisualSequenced sequenced)
        {
            if (!containsSequenceds) return;
            int index = IndexOf(sequenced);
            if (index == -1) return;
            Sequenceds.RemoveAt(index);
            totSequenceds--;
            _settings.RefreshSelected(_src, _sequence);
        }

        public static void DeselectAll()
        {
            Clear();
            _settings.RefreshSelected(_src, _sequence);
        }

        public static bool Contains(DOVisualSequenced sequenced)
        {
            if (!containsSequenceds) return false;
            for (int i = 0; i < totSequenceds; ++i) {
                if (Sequenceds[i].sequenced == sequenced) return true;
            }
            return false;
        }

        public static bool HasSelections(bool ofSameType = false)
        {
            if (totSequenceds == 0) return false;
            if (!ofSameType || totSequenceds == 1) return true;
            DOVisualSequenced.Type sType = Sequenceds[0].sequenced.type;
            for (int i = 1; i < totSequenceds; ++i) {
                if (Sequenceds[i].sequenced.type != sType) return false;
            }
            return true;
        }

        public static void RefreshSelectionsData()
        {
            if (totSequenceds > 0) {
                for (int i = 0; i < totSequenceds; ++i) Sequenceds[i].Refresh();
            }
        }

        public static List<DOVisualSequenced> GetCleanSelectedSequenceds()
        {
            if (Sequenceds.Count == 0) return null;
            List<DOVisualSequenced> result = new List<DOVisualSequenced>();
            foreach (SelectedSequenced sel in Sequenceds) result.Add(sel.sequenced);
            return result;
        }

        #endregion

        #region Methods

        static int IndexOf(DOVisualSequenced sequenced)
        {
            if (!containsSequenceds) return -1;
            for (int i = 0; i < totSequenceds; ++i) {
                if (Sequenceds[i].sequenced == sequenced) return i;
            }
            return -1;
        }

        #endregion

        // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████
        // ███ INTERNAL CLASSES ████████████████████████████████████████████████████████████████████████████████████████████████
        // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

        public class SelectedSequenced
        {
            public readonly DOVisualSequenced sequenced;
            public float originalStartTime;
            public float originalDuration;
            public int originalLayerIndex;

            public SelectedSequenced(
                DOVisualSequenced sequenced,
                float? forcedOriginalStartTime = null, float? forcedOriginalDuration = null, int? forcedOriginalLayerIndex = null
            ){
                this.sequenced = sequenced;
                Refresh(forcedOriginalStartTime, forcedOriginalDuration, forcedOriginalLayerIndex);
            }

            public void Refresh(float? forcedOriginalStartTime = null, float? forcedOriginalDuration = null, int? forcedOriginalLayerIndex = null)
            {
                originalStartTime = forcedOriginalStartTime != null ? (float)forcedOriginalStartTime : sequenced.startTime;
                originalDuration = forcedOriginalDuration != null ? (float)forcedOriginalDuration : sequenced.duration;
                originalLayerIndex = forcedOriginalLayerIndex != null
                    ? (int)forcedOriginalLayerIndex
                    : _sequence.FindSequencedLayerIndexByGuid(sequenced.guid);
            }
        }
    }
}