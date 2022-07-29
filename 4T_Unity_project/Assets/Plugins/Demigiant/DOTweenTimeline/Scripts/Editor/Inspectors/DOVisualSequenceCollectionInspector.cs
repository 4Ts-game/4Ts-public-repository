// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/01/16

using System;
using DG.DemiEditor;
using DG.Tweening.Timeline;
using DG.Tweening.TimelineEditor.PropertyDrawers;
using UnityEditor;
using UnityEngine;

namespace DG.Tweening.TimelineEditor.Inspectors
{
    [CustomEditor(typeof(DOVisualSequenceCollection))]
    class DOVisualSequenceCollectionInspector : Editor
    {
        DOVisualSequenceCollection _src;

        #region Unity and GUI Methods

        void OnEnable()
        {
            _src = target as DOVisualSequenceCollection;
        }

        public override void OnInspectorGUI()
        {
            Undo.RecordObject(_src, "DOVisualSequenceCollection");
            DOEGUI.BeginGUI();

            if (GUILayout.Button("Add Visual Sequence")) {
                if (_src.sequences == null) _src.sequences = new[] { new DOVisualSequence(Guid.NewGuid().ToString()) };
                DeEditorUtils.Array.ExpandAndAdd(ref _src.sequences, new DOVisualSequence(Guid.NewGuid().ToString()));
                GUI.changed = true;
            }
            if (_src.sequences == null) return;

            for (int i = 0; i < _src.sequences.Length; ++i) {
                DOVisualSequencePropertyDrawer.Internal_SequencerField(_src, _src.sequences[i], true, this, ref _src.sequences, i);
            }
        }

        #endregion
    }
}