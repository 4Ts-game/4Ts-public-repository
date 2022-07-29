// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/01/26

using System;
using System.Collections.Generic;
using DG.DemiEditor;
using DG.Tweening.Timeline;
using DG.Tweening.TimelineEditor;
using UnityEditor;
using UnityEngine;
using UnityEngine.Events;

namespace DG.Tweening.TimelineEditor.PropertyDrawers
{
    [CustomPropertyDrawer(typeof(DOVisualSequence))]
    public class DOVisualSequencePropertyDrawer : PropertyDrawer
    {
        const int _LinesOffsetY = 2;
        const int _ExtraBottomOpenMargin = 3;
        const int _SubRowsOffsetX = 12;
        const int _SequencerSubRowsOffsetX = 25;
        static readonly float _LineHeight = EditorGUIUtility.singleLineHeight;
        static readonly float _SmlLineHeight = EditorGUIUtility.singleLineHeight - 4;
        static readonly GUIContent _GcDrag = new GUIContent("≡");
        static readonly GUIContent _GcEdit = new GUIContent("EDIT", "Open this VisualSequence in DOTween's Timeline");
        static readonly GUIContent _GcDelete = new GUIContent("×", "Delete this VisualSequence");
        static readonly GUIContent _GcStartupBehaviour = new GUIContent("Startup Behavior", "Behaviour to apply when to VisualSequence is generated");
        static readonly GUIContent _GcAutoplay = new GUIContent("AutoPlay", "If toggled starts playing the VisualSequence as soon as it's generated");
        static readonly GUIContent _GcAutokill = new GUIContent("Kill On Complete", "If toggled kills the VisualSequence once it completes," +
                                                                            " freeing up memory and resources but preventing it from being reused");
        static readonly GUIContent _GcIgnoreTimeScale = new GUIContent("Ignore UTimeScale", "If toggled the VisualSequence will ignore Unity's timeScale");
        static readonly GUIContent _GcDelay = new GUIContent("Startup Delay");
        static readonly GUIContent _GcTimeScale = new GUIContent("Internal TimeScale", "Sequence internal timeScale");
        static readonly GUIContent _GcOverloadDuration = new GUIContent("Overload Duration", "Sequence overall duration (in this mode the sequence elements' duration will be considered as a percentage to achieve this overall duration)");
        static readonly GUIContent _GcTimeMode_TimeScale = new GUIContent("TimeScale");
        static readonly GUIContent _GcTimeMode_OverloadDuration = new GUIContent("Duration Overload");
        static readonly GUIContent _GcLoops = new GUIContent("Loops");
        static readonly GUIContent _GcInfiniteLoops = new GUIContent("∞");
        static readonly GUIContent _GcOnComplete = new GUIContent("On Complete", "Events to call when the VisualSequence completes all its loops");
        static readonly GUIContent _GcOnStepComplete = new GUIContent("On Step", "Events to call when the VisualSequence completes each loop cycle");
        static readonly GUIContent _GcOnUpdate = new GUIContent("On Update", "Events to call at each frame the VisualSequence is updated");
        static DOTimelineSettings _settings;
        static DOVisualSequence[] _stub;
        static readonly Dictionary<string, bool> _SequenceGuidToFoldout = new Dictionary<string, bool>();
        static readonly Dictionary<string, SerializedProperty> _SequenceGuidToSoSequence = new Dictionary<string, SerializedProperty>();
        static readonly Dictionary<string, SerializedProperty> _SequenceGuidToSoOnComplete = new Dictionary<string, SerializedProperty>();
        static readonly Dictionary<string, SerializedProperty> _SequenceGuidToSoOnStepComplete = new Dictionary<string, SerializedProperty>();
        static readonly Dictionary<string, SerializedProperty> _SequenceGuidToSoOnUpdate = new Dictionary<string, SerializedProperty>();
        static readonly List<SerializedProperty> _TmpSOSequences = new List<SerializedProperty>();
        static readonly HashSet<string> _TmpSequencesGUIDs = new HashSet<string>();

        #region GUI

        public override float GetPropertyHeight(SerializedProperty property, GUIContent label)
        {
            return GetHeight(property.serializedObject.targetObject as Component, property.CastTo<DOVisualSequence>(), property, false);
        }

        static float GetHeight(Component src, DOVisualSequence sequence, SerializedProperty soSequence, bool isSequencerMode)
        {
            if (_settings == null) _settings = DOTimelineSettings.Load();
            bool isFoldout = _settings.forceFoldoutsOpen || GetFoldout(sequence.guid);
            int lines = !isFoldout
                ? 1
                : isSequencerMode
                    ? 7
                    : 6;
//            float h = (EditorGUIUtility.singleLineHeight + _LinesOffsetY) * lines + (!_settings.sequencePropertyDrawerFoldout ? 0 : _ExtraBottomOpenMargin);
            float h = !isFoldout
                ? _LineHeight + _LinesOffsetY
                : (_LineHeight + _LinesOffsetY) * (lines - 2)
                  + (_SmlLineHeight + _LinesOffsetY) * 2
                  + _ExtraBottomOpenMargin;
            if (sequence != null) {
                if (isFoldout) {
                    if (sequence.hasOnComplete) h += GetSoOnComplete(soSequence, sequence.guid).GetUnityEventHeight() + _LinesOffsetY;
                    if (sequence.hasOnStepComplete) h += GetSoOnStepComplete(soSequence, sequence.guid).GetUnityEventHeight() + _LinesOffsetY;
                    if (sequence.hasOnUpdate) h += GetSoOnUpdate(soSequence, sequence.guid).GetUnityEventHeight() + _LinesOffsetY;
                }
            }
            return h;
        }

        public override void OnGUI(Rect position, SerializedProperty property, GUIContent label)
        {
            DOVisualSequence sequence = property.CastTo<DOVisualSequence>();
            if (sequence == null) return;
            DoField(position, label, property.serializedObject.targetObject as Component, sequence, property, false, null, ref _stub);
            if (GUI.changed) {
                EditorUtility.SetDirty(property.serializedObject.targetObject);
                property.serializedObject.ApplyModifiedProperties();
            }
        }

        /// <summary>
        /// Meant to be used only by <see cref="DOVisualSequenceCollection"/> Inspector
        /// </summary>
        internal static void Internal_SequencerField(
            Component src, DOVisualSequence sequence,
            bool isSequencerMode, Editor editor, ref DOVisualSequence[] sequencesList, int index = -1
        ){
            SerializedProperty soSequence = GetSoSequence(src, sequence.guid);
            float h = GetHeight(src, sequence, soSequence, true);
            Rect r = GUILayoutUtility.GetRect(10, 10000, h, h);
            DoField(r, null, src, sequence, soSequence, isSequencerMode, editor, ref sequencesList, index);
        }

        /// <summary>
        /// INTERNAL: only to be used by DOVisualSequenceCollection Inspector
        /// </summary>
        static DOVisualSequence DoField(
            Rect r, GUIContent label, Component src, DOVisualSequence sequence, SerializedProperty soSequence,
            bool isSequencerMode, Editor editor, ref DOVisualSequence[] sequencerModeSequences, int index = -1
        ){
            if (Event.current.type == EventType.Layout) return sequence;

            const int miniButtonW = 18;
            const int editW = 40;
            if (soSequence == null) soSequence = GetSoSequence(src, sequence.guid);
            DOEGUI.BeginGUI();
            Undo.RecordObject(src, "DOVisualSequence");
            bool hasModifiedProperties = soSequence.serializedObject.hasModifiedProperties;
            soSequence.serializedObject.ApplyModifiedProperties();
            if (hasModifiedProperties) ValidateAllSequences(src, sequence, soSequence.serializedObject); // Visual Inspector List fix
            soSequence.serializedObject.Update();

            // --------------------------------
            // [content]
            // └[drag][foldout][label][subcontent]
            //                        └[isActive][name][edit][delete]
            Rect indentedR = EditorGUI.IndentedRect(r);
            float indentedOffset = r.width - indentedR.width;
            using (new DeGUI.LabelFieldWidthScope(EditorGUIUtility.labelWidth - indentedOffset)) {
                Rect contentR = indentedR.SetHeight(EditorGUIUtility.singleLineHeight); // Full line
                Rect subcontentR = isSequencerMode || label == null // Line without eventual label
                    ? contentR.ShiftXAndResize(miniButtonW * 2)
                    : contentR.ShiftXAndResize(EditorGUIUtility.labelWidth + 2);
                Rect dragR = contentR.SetWidth(isSequencerMode ? miniButtonW : 0); // Sequencer only
                Rect foldoutR = contentR.SetX(dragR.xMax)
                    .SetWidth(isSequencerMode || label == null ? miniButtonW : EditorGUIUtility.labelWidth);
                Rect labelR = contentR.SetX(foldoutR.xMax) // Non-sequencer only
                    .SetWidth(isSequencerMode || label == null ? 0 : EditorGUIUtility.labelWidth - foldoutR.width);
                Rect deleteR = subcontentR.ShiftXAndResize(subcontentR.width - miniButtonW); // Sequencer only
                Rect editR = isSequencerMode
                    ? subcontentR.SetX(deleteR.x - 2 - editW).SetWidth(editW)
                    : subcontentR.SetX(subcontentR.xMax - editW).SetWidth(editW);
                Rect isActiveR = subcontentR.SetWidth(16);
                Rect nameR = subcontentR.SetX(isActiveR.xMax)
                    .SetWidth(editR.x - isActiveR.xMax - 2);

                int currIndentLevel = EditorGUI.indentLevel;
                EditorGUI.indentLevel = 0; // Without this indented GUI draws wrongly for some weird reason (Unity bug)
                
                // Drag (Sequencer only)
                if (isSequencerMode) {
                    if (DeGUI.PressButton(dragR, _GcDrag, GUI.skin.button)) {
                        DeGUIDrag.StartDrag(editor, sequencerModeSequences, index);
                    }
                }
                // Foldout
                bool isFoldout = _settings.forceFoldoutsOpen || GetFoldout(sequence.guid);
                using (var check = new EditorGUI.ChangeCheckScope()) {
                    isFoldout = label != null
                        ? DeGUI.FoldoutLabel(foldoutR.Shift(-3, 0, 3, 0), isFoldout, label) || _settings.forceFoldoutsOpen
                        : DeGUI.FoldoutLabel(foldoutR, isFoldout, GUIContent.none) || _settings.forceFoldoutsOpen;
                    if (check.changed) SetFoldout(sequence.guid, isFoldout);
                }
                // Active
                using (new DeGUI.ColorScope(null, null, sequence.isActive ? Color.green : Color.red)) {
                    sequence.isActive = EditorGUI.Toggle(isActiveR, sequence.isActive);
                    if (!sequence.isActive) {
                        DeGUI.DrawColoredSquare(
                            new Rect(isActiveR.x + 4, isActiveR.center.y, isActiveR.width - 10, 1), DeGUI.IsProSkin ? Color.red : Color.white
                        );
                    }
                }
                // Name + Edit
                using (new EditorGUI.DisabledScope(!sequence.isActive)) {
                    sequence.name = EditorGUI.TextField(nameR, sequence.name);
                    if (GUI.Button(editR, _GcEdit)) {
                        DOVisualSequenceTimeline.ShowWindow(src, sequence, soSequence);
                    }
                }
                // Delete (Sequencer only)
                if (isSequencerMode) {
                    using (new DeGUI.ColorScope(Color.red)) {
                        if (GUI.Button(deleteR, _GcDelete)) {
                            DeEditorUtils.Array.RemoveAtIndexAndContract(ref sequencerModeSequences, index);
                            GUI.changed = true;
                        }
                    }
                }

                if (isFoldout) {
                    float subRowsOffset = isSequencerMode ? _SequencerSubRowsOffsetX : _SubRowsOffsetX;
                    using (new DeGUI.LabelFieldWidthScope(EditorGUIUtility.labelWidth - subRowsOffset)) {
                        contentR = contentR.ShiftXAndResize(subRowsOffset);
                        subcontentR = contentR.ShiftXAndResize(EditorGUIUtility.labelWidth + 2);
                        // --------------------------------
                        if (isSequencerMode) {
                            // Startup Behaviour (Sequencer only)
                            ShiftRects(_LineHeight, ref contentR, ref subcontentR, out labelR);
                            EditorGUI.PrefixLabel(labelR, _GcStartupBehaviour);
                            sequence.startupBehaviour = (DOVisualSequence.StartupBehaviour)EditorGUI.EnumPopup(subcontentR, sequence.startupBehaviour);
                        }
                        // --------------------------------
                        // Autoplay + Autokill + IgnoreTimeScale
                        ShiftRects(_SmlLineHeight, ref contentR, ref subcontentR, out labelR);
                        Rect autoplayR = contentR.SetWidth((int)(contentR.width / 3) - 1);
                        Rect autokillR = autoplayR.SetX(autoplayR.xMax + 2);
                        Rect ignoreTimeScaleR = contentR.SetX(autokillR.xMax + 2).SetWidth(contentR.xMax - autokillR.xMax - 2);
                        sequence.autoplay = DeGUI.ToggleButton(autoplayR, sequence.autoplay, _GcAutoplay, DOEGUI.Styles.global.toggleSmlLabel);
                        sequence.autokill = DeGUI.ToggleButton(autokillR, sequence.autokill, _GcAutokill, DOEGUI.Styles.global.toggleSmlLabel);
                        sequence.ignoreTimeScale = DeGUI.ToggleButton(
                            ignoreTimeScaleR, sequence.ignoreTimeScale, _GcIgnoreTimeScale, DOEGUI.Styles.global.toggleSmlLabel
                        );
                        // --------------------------------
                        // Startup Delay
                        ShiftRects(_LineHeight, ref contentR, ref subcontentR, out labelR);
                        sequence.startupDelay = EditorGUI.Slider(contentR, _GcDelay, sequence.startupDelay, 0, 1000);
                        // --------------------------------
                        // TimeScale/Duration-overload
                        ShiftRects(_LineHeight, ref contentR, ref subcontentR, out labelR);
                        float timeModeW = sequence.timeMode == DOVisualSequence.TimeMode.TimeScale ? 86 : 124;
                        Rect contentModR = contentR.Shift(0, 0, -timeModeW - 2, 0);
                        Rect timeModeR = new Rect(contentModR.xMax + 2, contentR.y, timeModeW, contentR.height);
                        switch (sequence.timeMode) {
                        case DOVisualSequence.TimeMode.TimeScale:
                            sequence.timeScale = EditorGUI.Slider(contentModR, _GcTimeScale, sequence.timeScale, 0.01f, 100);
                            break;
                        case DOVisualSequence.TimeMode.DurationOverload:
                            using (var check = new EditorGUI.ChangeCheckScope()) {
                                sequence.durationOverload = EditorGUI.FloatField(contentModR, _GcOverloadDuration, sequence.durationOverload);
                                if (check.changed) {
                                    if (sequence.durationOverload < 0) sequence.durationOverload = 0;
                                }
                            }
                            break;
                        }
                        sequence.timeMode = (DOVisualSequence.TimeMode)EditorGUI.EnumPopup(timeModeR, sequence.timeMode);
                        // --------------------------------
                        // Loops
                        ShiftRects(_LineHeight, ref contentR, ref subcontentR, out labelR);
                        Rect loopsR = contentR.SetWidth(labelR.width + 58);
                        Rect infiniteLoopsR = loopsR.SetX(loopsR.xMax + 2).SetWidth(20);
                        Rect loopTypeR = contentR.ShiftXAndResize(infiniteLoopsR.xMax - contentR.x + 2);
                        bool infiniteLoops = sequence.loops == -1;
                        using (var check = new EditorGUI.ChangeCheckScope()) {
                            infiniteLoops = DeGUI.ToggleButton(
                                infiniteLoopsR.Contract(0, 1), infiniteLoops, _GcInfiniteLoops, DOEGUI.Styles.timeline.seqInfiniteLoopToggle
                            );
                            if (check.changed) sequence.loops = infiniteLoops ? -1 : 1;
                        }
                        if (infiniteLoops) {
                            // Draw loops with label and field separate, so I can disable only the field and not the label
                            Rect loopsLabelR = loopsR.Shift(0, 0, -58, 0);
                            Rect loopsFieldR = loopsR.ShiftXAndResize(labelR.width);
                            GUI.Label(loopsLabelR, _GcLoops, DOEGUI.Styles.global.prefixLabel);
                            using (new EditorGUI.DisabledScope(true)) EditorGUI.IntField(loopsFieldR, GUIContent.none, sequence.loops);
                        } else {
                            using (new EditorGUI.DisabledScope(sequence.loops == -1)) {
                                using (var check = new EditorGUI.ChangeCheckScope()) {
                                    sequence.loops = EditorGUI.IntField(loopsR, _GcLoops, sequence.loops);
                                    if (check.changed && sequence.loops < 1) sequence.loops = 1;
                                }
                            }
                        }
                        using (new EditorGUI.DisabledScope(sequence.loops < 2 && sequence.loops > -1)) {
                            sequence.loopType = (LoopType)EditorGUI.EnumPopup(loopTypeR, sequence.loopType);
                        }
                        // --------------------------------
                        // Events
                        ShiftRects(_SmlLineHeight, ref contentR, ref subcontentR, out labelR);
                        Rect onCompleteR = contentR.SetWidth(contentR.width / 3 - 1);
                        Rect onStepCompleteR = onCompleteR.SetX(onCompleteR.xMax + 2);
                        Rect onUpdateR = onStepCompleteR.SetX(onStepCompleteR.xMax + 2);
                        DrawEventToggle(onCompleteR, _GcOnComplete, soSequence, ref sequence.hasOnComplete, ref sequence.onComplete);
                        DrawEventToggle(onStepCompleteR, _GcOnStepComplete, soSequence, ref sequence.hasOnStepComplete, ref sequence.onStepComplete);
                        DrawEventToggle(onUpdateR, _GcOnUpdate, soSequence, ref sequence.hasOnUpdate, ref sequence.onUpdate);
                        Rect eventR = contentR;
                        if (sequence.hasOnComplete) DrawEvent(ref eventR, _GcOnComplete, GetSoOnComplete(soSequence, sequence.guid));
                        if (sequence.hasOnStepComplete) DrawEvent(ref eventR, _GcOnStepComplete, GetSoOnStepComplete(soSequence, sequence.guid));
                        if (sequence.hasOnUpdate) DrawEvent(ref eventR, _GcOnUpdate, GetSoOnUpdate(soSequence, sequence.guid));
                    }
                }

                EditorGUI.indentLevel = currIndentLevel;
            }

            if (isSequencerMode) {
                if (DeGUIDrag.Drag(sequencerModeSequences, index, r).outcome == DeDragResultType.Accepted) GUI.changed = true;
            }
            if (GUI.changed) {
                soSequence.serializedObject.ApplyModifiedProperties();
                EditorUtility.SetDirty(src);
            }
            return sequence;
        }

        static void DrawEventToggle(Rect r, GUIContent label, SerializedProperty property, ref bool toggled, ref UnityEvent unityEvent)
        {
            using (var check = new EditorGUI.ChangeCheckScope()) {
                toggled = DeGUI.ToggleButton(r, toggled, label, DOEGUI.Styles.global.toggleSmlLabel);
                if (check.changed) {
                    if (!toggled) unityEvent = new UnityEvent();
                }
            }
        }

        static void DrawEvent(ref Rect r, GUIContent label, SerializedProperty soEvent)
        {
            r = r.Shift(0, r.height + _LinesOffsetY, 0, 0).SetHeight(soEvent.GetUnityEventHeight());
            EditorGUI.PropertyField(r, soEvent, label);
        }

        static void ShiftRects(float newLineHeight, ref Rect contentR, ref Rect subcontentR, out Rect labelR)
        {
            contentR = contentR.Shift(0, contentR.height + _LinesOffsetY, 0, 0).SetHeight(newLineHeight);
            subcontentR = subcontentR.SetY(contentR.y).SetHeight(newLineHeight);
            labelR = contentR.SetWidth(EditorGUIUtility.labelWidth + 2);
        }

        static bool GetFoldout(string sequenceGuid)
        {
            if (!_SequenceGuidToFoldout.ContainsKey(sequenceGuid)) {
                _SequenceGuidToFoldout.Add(sequenceGuid, false);
            }
            return _SequenceGuidToFoldout[sequenceGuid];
        }
        static void SetFoldout(string sequenceGuid, bool foldout)
        {
            if (!_SequenceGuidToFoldout.ContainsKey(sequenceGuid)) {
                _SequenceGuidToFoldout.Add(sequenceGuid, foldout);
            } else _SequenceGuidToFoldout[sequenceGuid] = foldout;
        }

        #endregion

        #region Public Methods

        // Called by DOVisualSequenceTimeline when prefab editing mode changes
        public static void ForceClear()
        {
            _SequenceGuidToSoSequence.Clear();
            _SequenceGuidToSoOnComplete.Clear();
            _SequenceGuidToSoOnStepComplete.Clear();
            _SequenceGuidToSoOnUpdate.Clear();
        }

        #endregion

        #region Methods

        static SerializedProperty GetSoSequence(Component src, string sequenceGuid)
        {
            if (!_SequenceGuidToSoSequence.ContainsKey(sequenceGuid)) {
                _SequenceGuidToSoSequence.Add(sequenceGuid, TimelineEditorUtils.GetSerializedSequence(src, sequenceGuid));
            }
            try {
                if (_SequenceGuidToSoSequence[sequenceGuid].serializedObject.targetObject == null) throw new Exception("SO Destroyed");
                bool val = _SequenceGuidToSoSequence[sequenceGuid].serializedObject.hasModifiedProperties;
            } catch {
                // SerializedObject has been disposed, regenerate
                _SequenceGuidToSoSequence.Remove(sequenceGuid);
                _SequenceGuidToSoSequence.Add(sequenceGuid, TimelineEditorUtils.GetSerializedSequence(src, sequenceGuid));
            }
            return _SequenceGuidToSoSequence[sequenceGuid];
        }

        static SerializedProperty GetSoOnComplete(SerializedProperty soSequence, string sequenceGuid)
        {
            return GetSoRelative(_SequenceGuidToSoOnComplete, soSequence, "onComplete", sequenceGuid);
        }
        static SerializedProperty GetSoOnStepComplete(SerializedProperty soSequence, string sequenceGuid)
        {
            return GetSoRelative(_SequenceGuidToSoOnStepComplete, soSequence, "onStepComplete", sequenceGuid);
        }
        static SerializedProperty GetSoOnUpdate(SerializedProperty soSequence, string sequenceGuid)
        {
            return GetSoRelative(_SequenceGuidToSoOnUpdate, soSequence, "onUpdate", sequenceGuid);
        }
        static SerializedProperty GetSoRelative(
            Dictionary<string, SerializedProperty> dict, SerializedProperty soSequence, string propertyName, string sequenceGuid
        ){
            if (!dict.ContainsKey(sequenceGuid)) {
                dict.Add(sequenceGuid, soSequence.FindPropertyRelative(propertyName));
            }
            try {
                bool val = dict[sequenceGuid].editable;
            } catch {
                // SerializedObject has been disposed, regenerate
                dict.Remove(sequenceGuid);
                dict.Add(sequenceGuid, soSequence.FindPropertyRelative(propertyName));
            }
            return dict[sequenceGuid];
        }

        /// <summary>
        /// Visual Inspector List fix: Validates all sequences on the component and if it finds one with the same GUID as another resets it
        /// (it happens when a new array element is created in the Inspector as a copy of the previous one)
        /// </summary>
        static void ValidateAllSequences(Component src, DOVisualSequence sequence, SerializedObject so)
        {
//            Debug.Log(string.Format("<color=#00ff00>VALIDATE SEQUENCES (fired from #{0} change)</color>", sequence.guid));
            _TmpSOSequences.Clear();
            _TmpSequencesGUIDs.Clear();
            int totChanged = 0;
            TimelineEditorUtils.GetAllSerializedSequencesInComponent(src, so, _TmpSOSequences);
            for (int i = 0; i < _TmpSOSequences.Count; ++i) {
                SerializedProperty p = _TmpSOSequences[i];
                DOVisualSequence s = p.CastTo<DOVisualSequence>();
                if (s == null) continue;
                if (_TmpSequencesGUIDs.Contains(s.guid)) {
                    // Change guid of element and partially reset sequence
                    totChanged++;
                    s.Editor_Reset(true, true);
                }
                _TmpSequencesGUIDs.Add(s.guid);
            }
            _TmpSOSequences.Clear();
            _TmpSequencesGUIDs.Clear();
            if (totChanged > 0) {
//                Debug.Log(string.Format("   Recreated GUIDs and partially reset {0} DOVisualSequences", totChanged));
                so.ApplyModifiedProperties();
            }
        }

        #endregion
    }
}