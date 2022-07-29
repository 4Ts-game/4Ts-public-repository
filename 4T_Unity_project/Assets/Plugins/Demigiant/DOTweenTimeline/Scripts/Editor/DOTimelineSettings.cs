// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/01/17

using System;
using System.Collections.Generic;
using System.Reflection;
using DG.DemiEditor;
using DG.Tweening.Timeline;
using DG.Tweening.Timeline.Core;
using DG.Tweening.TimelineEditor;
using UnityEditor;
using UnityEngine;

namespace DG.Tweening.TimelineEditor
{
    /// <summary>
    /// Global editor settings for the visual timeline
    /// </summary>
    public class DOTimelineSettings : ScriptableObject
    {
        #region Serialized
#pragma warning disable 0649

        public int secondToPixels = 100;
        public int layerHeight = 18;
        public int layersPanelWidth = 180;
        public bool forceFoldoutsOpen = false; // Forces DOVisualSequence Inspector foldouts to stay open
        public float actionsLayoutDuration = 0.5f; // visual duration in Timeline of no-duration sequenceds (like Actions and Events)
        public int maxSnapPixelDistance = 20; // Max distance at which we'll snap to other sequenceds when dragging
        public int minPixelDragDistance = 10; // Min distance before a dragging activates
        public Defaults defaults = new Defaults();
        public Experimental experimental = new Experimental();
        public RecorderData recorderData = new RecorderData();
        //
        [SerializeField] int _lastSelectedComponentId;
        [SerializeField] string _lastSelectedSequenceGuid;
        [SerializeField] List<LastSelectedData> _lastSelectedSequencedsData = new List<LastSelectedData>();

#pragma warning restore 0649
        #endregion

        static DOTimelineSettings I;
        public const int MinSecondToPixels = 20;
        public const int MaxSecondToPixels = 1600;
        public const int MinLayerHeight = 16;
        public const int MaxLayerHeight = 36;
        public const float MinSequencedSnapping = 0.01f;

        static readonly List<DOVisualSequence> _TmpSequences = new List<DOVisualSequence>();

        #region Public Methods

        public static DOTimelineSettings Load()
        {
            if (TimelineSession.isDevDebugMode) DOLog.DebugDev("Developer Debug Mode <color=#00ff00>ACTIVE</color>");
            if (I == null) I = DeEditorPanelUtils.ConnectToSourceAsset<DOTimelineSettings>(TimelinePaths.ADB.DOTimelineSettings, true, true);
            return I;
        }

        #region Selected Methods

        public void RefreshSelected(Component component, DOVisualSequence sequence, bool undoable = true)
        {
            if (undoable) Undo.RecordObject(this, "DOTween Sequence");
            _lastSelectedComponentId = component == null ? 0 : component.GetInstanceID();
            _lastSelectedSequenceGuid = component == null ? null : sequence.guid;
            _lastSelectedSequencedsData.Clear();
            if (sequence != null) {
                int len = TimelineSelection.Sequenceds.Count;
                for (int i = 0; i < len; ++i) {
                    _lastSelectedSequencedsData.Add(new LastSelectedData(TimelineSelection.Sequenceds[i]));
                }
            }
            if (undoable) EditorUtility.SetDirty(this);
//            Debug.Log("RefreshSelected " + component + (sequence == null ? "" : " " + sequence.name)
//                      + ", totSelected: " + _lastSelectedSequencedsGuids.Count);
        }

        // NOTE: tried using ref instead or out to keep sequence/SO if they were already correct but for some reason
        // they're actually incorrect and it doesn't work
        public void ReapplySelected(out Component component, out DOVisualSequence sequence, out SerializedProperty spSequence)
        {
//            Debug.Log("ReapplySelected " + _lastSelectedComponentId + "/" + _lastSelectedSequenceGuid + "/" + _lastSelectedSequencedsData.Count);
            component = _lastSelectedComponentId == 0 ? null : EditorUtility.InstanceIDToObject(_lastSelectedComponentId) as Component;
            if (component == null && _lastSelectedComponentId != 0) {
                // Component doesn't exist anymore, clear cache
                ResetSelected(out component, out sequence, out spSequence);
                return;
            }
            if (component == null) sequence = null;
            else {
                _TmpSequences.Clear();
                TimelineEditorUtils.FindSerializedSequences(component, _TmpSequences, _lastSelectedSequenceGuid);
                sequence = _TmpSequences.Count > 0 ? _TmpSequences[0] : null;
                _TmpSequences.Clear();
            }
            TimelineSelection.Clear();
            if (sequence == null) {
                ResetSelected(out component, out sequence, out spSequence);
                return;
            }
            spSequence = TimelineEditorUtils.GetSerializedSequence(component, sequence.guid);
            foreach (LastSelectedData sel in _lastSelectedSequencedsData) {
                DOVisualSequenced sequenced = sequence.FindSequencedByGuid(sel.guid);
                if (sequenced != null) {
                    TimelineSelection.Select(sequenced, true, false, sel.originalStartTime, sel.originalDuration, sel.originalLayerIndex);
                }
            }
        }

        public void StoreSelectedSnapshot()
        {
            recorderData.lastSelectedComponentId = _lastSelectedComponentId;
            recorderData.lastSelectedSequenceGuid = _lastSelectedSequenceGuid;
            recorderData.lastSelectedSequencedsData.Clear();
            foreach (LastSelectedData sel in _lastSelectedSequencedsData) recorderData.lastSelectedSequencedsData.Add(sel);
        }

        public void ReapplySelectedSnapshot()
        {
            _lastSelectedComponentId = recorderData.lastSelectedComponentId;
            _lastSelectedSequenceGuid = recorderData.lastSelectedSequenceGuid;
            _lastSelectedSequencedsData.Clear();
            foreach (LastSelectedData sel in recorderData.lastSelectedSequencedsData) _lastSelectedSequencedsData.Add(sel);
        }

        void ResetSelected(out Component component, out DOVisualSequence sequence, out SerializedProperty spSequence)
        {
            Undo.RecordObject(this, "DOTween Sequence");
            component = null;
            sequence = null;
            spSequence = null;
            _lastSelectedComponentId = 0;
            _lastSelectedSequenceGuid = null;
            _lastSelectedSequencedsData.Clear();
            EditorUtility.SetDirty(this);
        }

        #endregion

        #endregion

        // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████
        // ███ INTERNAL CLASSES ████████████████████████████████████████████████████████████████████████████████████████████████
        // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

        [Serializable]
        public class Defaults
        {
            public float duration = 1;
            public Ease ease = Ease.OutQuad;
            public LoopType loopType = LoopType.Restart;
        }

        [Serializable]
        public class Experimental
        {
            public bool enableRecordMode = false;
        }

        [Serializable]
        public struct LastSelectedData
        {
            public string guid;
            public float originalStartTime, originalDuration;
            public int originalLayerIndex;
            internal LastSelectedData(TimelineSelection.SelectedSequenced selection)
            {
                this.guid = selection.sequenced.guid;
                this.originalStartTime = selection.originalStartTime;
                this.originalDuration = selection.originalDuration;
                this.originalLayerIndex = selection.originalLayerIndex;
            }
        }

        [Serializable]
        public class RecorderData
        {
            public List<DOVisualSequence> recordedSequences = new List<DOVisualSequence>();
            public List<DOVisualSequence> tmpClonedSequences = new List<DOVisualSequence>();
            public int undoIndexBeforeRecording;
            public int lastSelectedComponentId;
            public string lastSelectedSequenceGuid;
            public List<LastSelectedData> lastSelectedSequencedsData = new List<LastSelectedData>();
        }
    }
}