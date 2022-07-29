// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/01/24

using DG.DemiLib;
using UnityEditor;

namespace DG.Tweening.TimelineEditor
{
    internal static class DOScope
    {
        public class UndoableSerialization : DeScope
        {
            readonly bool _markSettings, _markComponent;
            public UndoableSerialization(bool markSettings = true, bool markComponent = true)
            {
                _markSettings = markSettings;
                _markComponent = markComponent;
                DOVisualSequenceTimeline.editor.MarkForUndo(_markSettings, _markComponent);
            }
            protected override void CloseScope()
            {
                DOVisualSequenceTimeline.editor.MarkDirty(_markSettings, _markComponent);
            }
        }

        /// <summary>
        /// Only for settings, doesn't mark for undo/dirty the component
        /// </summary>
        public class NonUndoableSettingsSerialization : DeScope
        {
            public NonUndoableSettingsSerialization()
            {
                Undo.IncrementCurrentGroup();
                // This allows the undo to be nullified but also prevents it to work
                Undo.RegisterCompleteObjectUndo(DOVisualSequenceTimeline.settings, "DOTween Timeline");
            }
            protected override void CloseScope()
            {
                DOVisualSequenceTimeline.editor.MarkDirty(true, false);
                Undo.ClearUndo(DOVisualSequenceTimeline.settings);
            }
        }
    }
}