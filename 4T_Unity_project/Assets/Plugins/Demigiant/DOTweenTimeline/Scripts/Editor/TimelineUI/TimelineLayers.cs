// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/01/17

using System;
using DG.DemiEditor;
using DG.DemiLib;
using DG.Tweening.Timeline;
using DG.Tweening.Timeline.Core;
using UnityEditor;
using UnityEngine;

namespace DG.Tweening.TimelineEditor
{
    internal class TimelineLayers : ABSTimelineElement
    {
        public const int MinSize = 126;
        readonly Color _bgColor = new DeSkinColor(0.15f);
        readonly Color _lockedColor = new Color(0.99f, 0.76f, 0.02f);
        readonly Color _unlockedColor = new DeSkinColor(0.5f);
        readonly Color _activeColor = new Color(0.09f, 0.85f, 0.56f);
        readonly Color _inactiveColor = new Color(0.9f, 0.23f, 0.31f);
        readonly Color _nameBgColor = new DeSkinColor(0.3f);
        readonly Color _optionsDropdownColor = new DeSkinColor(0.5f);
        GUIContent _icoOptionsDropdown;
        string _lockedTooltip = "Locks/unlocks editing for this layer, has no effect at runtime";
        GUIContent _lockedContent, _unlockedContent;
        string _activeTooltip = "Activates/deactivates the layer. A deactivated layer will not create any tween/event at runtime";
        GUIContent _activeContent, _inactiveContent;

        #region GUI

        public void Refresh()
        {
            _icoOptionsDropdown = new GUIContent(DeStylePalette.ico_optionsDropdown);
            _lockedContent = new GUIContent(DeStylePalette.ico_lock, _lockedTooltip);
            _unlockedContent = new GUIContent(DeStylePalette.ico_lock_open, _lockedTooltip);
            _activeContent = new GUIContent(DeStylePalette.ico_visibility, _activeTooltip);
            _inactiveContent = new GUIContent(DeStylePalette.ico_visibility_off, _activeTooltip);
        }

        public override void Draw(Rect drawArea)
        {
            if (Event.current.type == EventType.Layout || drawArea.width < 50 || isRecorderStoppedPass) return;

            base.Draw(drawArea);

            // Background
            DeGUI.DrawColoredSquare(area, _bgColor);
            // Layers
            if (sequence.layers != null) {
                for (int i = layout.firstVisibleLayerIndex; i < layout.visibleLayersDrawLoopLength; ++i) DrawLayer(i);
            }
        }

        void DrawLayer(int index)
        {
            Rect layerR = new Rect(
                0, layout.partialOffset.y + settings.layerHeight * (index - layout.firstVisibleLayerIndex), area.width, settings.layerHeight
            );
            Rect contentR = layerR.Contract(1);
            Rect colorR = layerR.SetWidth(20).Shift(0, 1, 0, -2);
            float contentRCenterY = contentR.center.y;
            Rect optionsSize = DeStylePalette.ico_optionsDropdown.GetRect().Fit(DeStylePalette.ico_optionsDropdown.width, contentR.height);
            Rect lockSize = DeStylePalette.ico_lock.GetRect().Fit(DeStylePalette.ico_lock.width, contentR.height);
            Rect activeSize = DeStylePalette.ico_visibility.GetRect().Fit(DeStylePalette.ico_visibility.width, contentR.height);
            Rect btOptionsR = new Rect(contentR.xMax - optionsSize.width, 0, optionsSize.width, optionsSize.height)
                .SetCenterY(contentRCenterY);
            Rect btLockR = new Rect(btOptionsR.x - lockSize.width - 2, 0, lockSize.width, lockSize.height)
                .SetCenterY(contentRCenterY);
            Rect btActiveR = new Rect(btLockR.x - activeSize.width - 2, 0, activeSize.width, activeSize.height)
                .SetCenterY(contentRCenterY);
            Rect nameR = contentR.ShiftXAndResize(colorR.width).Shift(0, 0, -(contentR.width - btActiveR.x + 2), 0).SetHeight(16)
                .SetCenterY(contentRCenterY);

            sequence.layers[index].color = EditorGUI.ColorField(colorR, GUIContent.none, sequence.layers[index].color, false, false, false);
            using (new DeGUI.ColorScope(null, DOEGUI.GetVisibleContentColorOn(sequence.layers[index].color))) {
                GUI.Label(colorR, sequence.layers[index].sequencedGuids.Length.ToString(), DOEGUI.Styles.timeline.layerTotSequencedLabel);
            }
            using (new DeGUI.ColorScope(_nameBgColor)) {
                sequence.layers[index].name = DeGUI.DoubleClickDraggableTextField(
                    nameR, editor, "layerName" + index, sequence.layers[index].name, sequence.layers, index,
                    DOEGUI.Styles.timeline.layerNameField, DOEGUI.Styles.timeline.layerNameFieldSelected
                );
            }
            using (new DeGUI.ColorScope(null, null, _optionsDropdownColor)) {
                if (EditorGUI.DropdownButton(btOptionsR, _icoOptionsDropdown, FocusType.Passive, DOEGUI.Styles.timeline.layerIcoToggle)) {
                    CM_Dropdown(index);
                }
            }
            bool locked = sequence.layers[index].locked;
            bool active = sequence.layers[index].isActive;
            using (new DeGUI.ColorScope(null, null, locked ? _lockedColor : _unlockedColor)) {
                if (GUI.Button(btLockR, locked ? _lockedContent : _unlockedContent, DOEGUI.Styles.timeline.layerIcoToggle)) {
                    sequence.layers[index].locked = !locked;
                    DOVisualSequence.VisualLayer layer = sequence.layers[index];
                    for (int i = 0; i < layer.sequencedGuids.Length; ++i) {
                        TimelineSelection.Deselect(sequence.FindSequencedByGuid(layer.sequencedGuids[i]));
                    }
                    GUI.changed = true;
                }
            }
            using (new DeGUI.ColorScope(null, null, active ? _activeColor : _inactiveColor)) {
                using (var check = new EditorGUI.ChangeCheckScope()) {
                    if (GUI.Button(btActiveR, active ? _activeContent : _inactiveContent, DOEGUI.Styles.timeline.layerIcoToggle)) {
                        sequence.layers[index].isActive = !active;
                        GUI.changed = true;
                    }
                    if (check.changed) {
                        // Set active value of all tweens in layer
                        var layer = sequence.layers[index];
                        for (int i = 0; i < layer.sequencedGuids.Length; ++i) {
                            sequence.FindSequencedByGuid(layer.sequencedGuids[i]).isActive = layer.isActive;
                        }
                    }
                }
            }
            if (DeGUIDrag.Drag(sequence.layers, index, layerR).outcome == DeDragResultType.Accepted) GUI.changed = true;
        }

        #region Context Menus

        void CM_Dropdown(int layerIndex)
        {
            GenericMenu menu = new GenericMenu();
            menu.AddItem(new GUIContent("Reset Color"), false, () => {
                using (new DOScope.UndoableSerialization()) sequence.layers[layerIndex].color = DOVisualSequence.VisualLayer.DefColor;
            });
            menu.AddSeparator("");
            if (sequence.layers.Length <= 1) menu.AddDisabledItem(new GUIContent("Delete Layer"));
            else {
                menu.AddItem(new GUIContent("Delete Layer"), false, () => {
                    using (new DOScope.UndoableSerialization()) {
                        TimelineEditorUtils.RemoveLayer(sequence, layerIndex);
                        TimelineSelection.DeselectAll();
                    }
                });
            }
            menu.ShowAsContext();
        }

        #endregion

        #endregion
    }
}