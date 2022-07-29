// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/01/17

using System;
using DG.DemiEditor;
using DG.DemiLib;
using DG.Tweening.Timeline;
using DG.Tweening.Timeline.Core;
using DG.Tweening.Timeline.Core.Plugins;
using UnityEditor;
using UnityEngine;
using Object = System.Object;

namespace DG.Tweening.TimelineEditor
{
    internal class TimelineMain : ABSTimelineElement
    {
        enum LayerPass
        {
            Main, Overlay
        }

        readonly Color _bgColor = new DeSkinColor(0.1f);
        readonly Color _prefabModeBgColor = new Color(0.04f, 0.18f, 0.31f);
        readonly Color _timeSeparatorColor0 = new Color(0, 0, 0, 0.4f);
        readonly Color _timeSeparatorColor1 = new Color(1, 1, 1, 0.2f);
        readonly Color _separatorFractionColor = new Color(0, 0, 0, 0.1f);
        readonly Color _rowColor0 = new DeSkinColor(0.2f);
        readonly Color _rowColor1 = new DeSkinColor(0.25f);
        readonly Color _prefabModeRowColor0 = new Color(0.08f, 0.27f, 0.45f);
        readonly Color _prefabModeRowColor1 = new Color(0.12f, 0.34f, 0.54f);
        readonly Color _inactiveOverlayColor = new Color(0.82f, 0.02f, 0.15f, 0.6f);
        readonly Color _lockedBgColor = new Color(0.99f, 0.76f, 0.02f, 0.5f);
        readonly Color _shadowColor = new Color(0, 0, 0, 0.6f);
        readonly Color _selectionColor = new Color(0f, 0.68f, 1f);
        readonly Color _snapToSequencedColor = new Color(1f, 0f, 0.99f);
        readonly Color _pinBgColor = new Color(0.38f, 0.02f, 0.7f);
        readonly Color _pinBorderColor = new Color(0.67f, 0.94f, 0.52f);

        Vector2 _dragStartP;
        Vector2 _dragCurrMouseP; // Used by continuous shift update
        DOVisualSequenced _currMouseOverSequenced; // Refreshed on every GUI call by DrawLayer
        bool _isDraggingTimeline, _isPreparingToDragSequenceds;
        bool _isSnapDragging; // TRUE only if snapping to multiplier values, not to other sequenceds
        SnapToSequencedData _snapToSequencedData = new SnapToSequencedData();
        bool _isPreparingToDragDuration, _durationDraggingWaitingForFirstStep;
        DOVisualSequenced _isPreparingToDragDurationMainTarget;
        int _draggedMainSequencedCurrLayerIndex, _draggedSequencedsMaxLayerShift, _draggedSequencedsMinLayerShift;
        Vector2Int _timelineShiftSnapshot;
        Rect _allSelectedR;
        Rect _dragSelectionR;
        bool _keyAlt;

        #region GUI

        public void Refresh() {}

        public override void Draw(Rect drawArea)
        {
            if (Event.current.type == EventType.Layout || isRecorderStoppedPass) return;

            base.Draw(drawArea);

            _keyAlt = DeGUIKey.alt;

            // Input - PRE
            switch (Event.current.rawType) {
            case EventType.MouseDown: // Raw so it can work even when scope is disabled during playmode
                switch (Event.current.button) {
                case 2: // MMB
                    GUI.FocusControl(null);
                    DragTimeline(true);
                    break;
                }
                break;
            case EventType.MouseUp:
                switch (Event.current.button) {
                case 0: // LMB:
                    if (TimelineSelection.isDraggingSelection) StopDragSelection();
                    else if (_isPreparingToDragSequenceds || TimelineSelection.isDraggingSequenceds) StopDraggingSequenceds();
                    else if (TimelineSelection.isDraggingDuration || _isPreparingToDragDuration) StopDraggingSequencedsDuration();
                    break;
                case 2: // MMB:
                    if (_isDraggingTimeline) StopDragTimeline();
                    break;
                }
                break;
            case EventType.MouseDrag: // Raw so it can work even when scope is disabled during playmode
                if (_isDraggingTimeline) DragTimeline();
                else if (TimelineSelection.isDraggingSelection) DragSelection();
                else if (_isPreparingToDragSequenceds || TimelineSelection.isDraggingSequenceds) DragSequenceds();
                else if (TimelineSelection.isDraggingDuration) DragSequencedsDuration();
                break;
            case EventType.ScrollWheel: // Change zoom (secondToPixels or layerHeight) - Raw so it can work even when scope is disabled during playmode
                if (!area.Contains(Event.current.mousePosition)) break;
                Vector2 scroll = Event.current.delta;
                if (DeGUIKey.shift) {
                    int newLayerHeight = Mathf.Min(
                        DOTimelineSettings.MaxLayerHeight,
                        Mathf.Max(DOTimelineSettings.MinLayerHeight, (int)(settings.layerHeight - scroll.y))
                    );
                    if (newLayerHeight != settings.layerHeight) TimelineEditorUtils.UpdateLayerHeight(newLayerHeight, Event.current.mousePosition.y);
                } else {
                    int newSecondToPixels = Mathf.Min(
                        DOTimelineSettings.MaxSecondToPixels,
                        Mathf.Max(DOTimelineSettings.MinSecondToPixels, (int)(settings.secondToPixels - scroll.y * 4))
                    );
                    if (newSecondToPixels != settings.secondToPixels) TimelineEditorUtils.UpdateSecondToPixels(newSecondToPixels, Event.current.mousePosition.x);
                }
                editor.Repaint();
                break;
            }
            switch (Event.current.type) {
            case EventType.MouseDown:
                switch (Event.current.button) {
                case 0: // LMB
                    if (_isPreparingToDragDuration && area.Contains(Event.current.mousePosition)) {
                        GUI.FocusControl(null);
                        DragSequencedsDuration(true);
                        Event.current.Use();
                    }
                    break;
                }
                break;
            case EventType.DragUpdated:
                if (!IsValidDragAndDrop(DragAndDrop.objectReferences)) break;
                DragAndDrop.visualMode = DragAndDropVisualMode.Generic;
                break;
            case EventType.DragPerform:
                if (!IsValidDragAndDrop(DragAndDrop.objectReferences)) break;
                CompleteDragAndDrop(DragAndDrop.objectReferences[0]);
                DeGUI.ExitCurrentEvent();
                return;
            }
            //
            if (_isDraggingTimeline) EditorGUIUtility.AddCursorRect(area, MouseCursor.Pan);
            else if (TimelineSelection.isDraggingSequenceds) EditorGUIUtility.AddCursorRect(area, MouseCursor.MoveArrow);
            else if (_isPreparingToDragDuration || TimelineSelection.isDraggingDuration) EditorGUIUtility.AddCursorRect(area, MouseCursor.ResizeHorizontal);

            // Draw
            _currMouseOverSequenced = null;
            bool hasLayers = sequence.layers != null;
            DrawBackground(hasLayers);
            DrawTimeSeparators();
            if (hasLayers) {
                bool wasPreparingToDragDuration = _isPreparingToDragDuration;
                if (!TimelineSelection.isDraggingDuration) {
                    _isPreparingToDragDuration = false;
                    _isPreparingToDragDurationMainTarget = null;
                }
                for (int i = layout.firstVisibleLayerIndex; i < layout.visibleLayersDrawLoopLength; ++i) DrawLayer(LayerPass.Main, i);
                if (_isPreparingToDragDuration != wasPreparingToDragDuration) editor.Repaint();
            }
            if (TimelineSelection.isDraggingSelection || TimelineSelection.containsSequenceds) {
                _allSelectedR = new Rect(99999999, 0, 0, 0);
                for (int i = layout.firstVisibleLayerIndex; i < layout.visibleLayersDrawLoopLength; ++i) DrawLayer(LayerPass.Overlay, i);
            }
            DrawOverlay();

            // Input - POST
            switch (Event.current.type) {
            case EventType.MouseDown:
                switch (Event.current.button) {
                case 0: // LMB
                    GUI.FocusControl(null);
                    if (TimelineSelection.containsSequenceds) {
                        TimelineSelection.DeselectAll();
                        editor.Repaint();
                    }
                    DragSelection(true);
                    break;
                case 1: // RMB
                    if (_currMouseOverSequenced != null && !TimelineSelection.Contains(_currMouseOverSequenced)) {
                        DOVisualSequence.VisualLayer layer = sequence.Editor_GetSequencedLayer(_currMouseOverSequenced.guid);
                        if (layer.locked) editor.ShowNotification(new GUIContent(string.Format("Layer \"{0}\" is locked", layer.name)));
                        else {
                            TimelineSelection.Select(_currMouseOverSequenced);
                            editor.Repaint();
                        }
                    }
                    break;
                }
                break;
            case EventType.ContextClick:
                int layerIndex = layout.GetLayerIndexAtMouse();
                if (layerIndex != -1 && sequence.layers[layerIndex].locked) {
                    editor.ShowNotification(new GUIContent(string.Format("Layer \"{0}\" is locked", sequence.layers[layerIndex].name)));
                } else {
                    if (_currMouseOverSequenced == null) CM_EmptyArea();
                    else CM_Sequenced();
                }
                break;
            case EventType.KeyDown:
                switch (Event.current.keyCode) {
                case KeyCode.Delete:
                case KeyCode.Backspace:
                    if (GUIUtility.hotControl > 0 || GUIUtility.keyboardControl > 0) return;
                    if (!TimelineSelection.containsSequenceds) break;
                    if (_isPreparingToDragSequenceds || TimelineSelection.isDraggingSequenceds) StopDraggingSequenceds();
                    for (int i = 0; i < TimelineSelection.Sequenceds.Count; ++i) {
                        TimelineEditorUtils.RemoveSequenced(sequence, TimelineSelection.Sequenceds[i].sequenced.guid);
                    }
                    TimelineSelection.DeselectAll();
                    GUI.changed = true;
                    break;
                case KeyCode.Escape:
                    if (TimelineSelection.HasSelections()) {
                        TimelineSelection.DeselectAll();
                        editor.Repaint();
                    }
                    break;
                case KeyCode.A:
                    if (GUIUtility.hotControl > 0 || GUIUtility.keyboardControl > 0) return;
                    if (DeGUIKey.Exclusive.ctrl) {
                        TimelineSelection.SelectAllIn(sequence, false);
                        editor.Repaint();
                    }
                    break;
                case KeyCode.D:
                    if (GUIUtility.hotControl > 0 || GUIUtility.keyboardControl > 0) return;
                    if (DeGUIKey.Exclusive.ctrl) {
                        TimelineSelection.DeselectAll();
                        editor.Repaint();
                    }
                    break;
                case KeyCode.C:
                    if (DeGUIKey.Exclusive.ctrl) CopySelectedSequenceds(false);
                    break;
                case KeyCode.V:
                    if (DeGUIKey.Exclusive.ctrl || DeGUIKey.Exclusive.ctrlShift) {
                        PasteSequencedsFromClipboard(
                            layout.GetSecondsAtMouse(), layout.GetLayerIndexAtMouse(true), DeGUIKey.Exclusive.ctrlShift
                        );
                    }
                    break;
                case KeyCode.X:
                    if (DeGUIKey.Exclusive.ctrl) CopySelectedSequenceds(true);
                    break;
                }
                break;
            }
        }

        void DrawBackground(bool hasLayers)
        {
            // Main bg
            if (DOVisualSequenceTimeline.stageMode == StageMode.PrefabEditingMode) {
                DeGUI.DrawTiledTexture(area.ShiftYAndResize(layout.visibleTimelineHeight), DeStylePalette.tileBars_slanted_alpha, 1f, _prefabModeBgColor);
            } else DeGUI.DrawTiledTexture(area.ShiftYAndResize(layout.visibleTimelineHeight), DeStylePalette.tileBars_slanted, 1f, _bgColor);
            // Layers bg
            if (hasLayers) {
                for (int i = layout.firstVisibleLayerIndex; i < layout.visibleLayersDrawLoopLength; ++i) {
                    DOVisualSequence.VisualLayer layer = sequence.layers[i];
                    Rect layerR = TimelineEditorUtils.GetLayerRect(i, area.width);
                    DeGUI.DrawColoredSquare(layerR, i % 2 == 0
                        ? DOVisualSequenceTimeline.stageMode == StageMode.PrefabEditingMode ? _prefabModeRowColor0 : _rowColor0
                        : DOVisualSequenceTimeline.stageMode == StageMode.PrefabEditingMode ? _prefabModeRowColor1 : _rowColor1);
                }
            }
        }

        // Overlay pass happens only if sequenceds are selected or if dragging selection
        void DrawLayer(LayerPass pass, int index)
        {

            bool isMainPass = pass == LayerPass.Main;
            bool isOverlayPass = !isMainPass;
            Vector2 mouseP = Event.current.mousePosition;
            DOVisualSequence.VisualLayer layer = sequence.layers[index];
            Rect layerR = TimelineEditorUtils.GetLayerRect(index, area.width);
            // Sequenced
            int len = layer.sequencedGuids.Length;
            // Buttons pass
            if (isMainPass) {
                for (int i = len - 1; i > -1; --i) {
                    DOVisualSequenced sequenced = sequence.FindSequencedByGuid(layer.sequencedGuids[i]);
                    if (sequenced == null) continue;
                    bool visible = sequenced.startTime < layout.lastVisibleTime
                                   && sequenced.startTime + sequenced.Editor_DrawDuration() * sequenced.Editor_PositiveLoopValue() > layout.firstVisibleTime;
                    if (!visible) continue;
                    Rect r = TimelineEditorUtils.GetSequencedRect(sequenced, layerR);
                    if (r.Contains(mouseP) && _currMouseOverSequenced == null) _currMouseOverSequenced = sequenced;
                    // Drag-to-resize
                    if (!TimelineSelection.isDraggingDuration) {
                        float dragHalfW = Mathf.Min(r.width * 0.5f, 6);
                        Rect dragDurationR = r.Shift(r.width - dragHalfW, 0, 0, 0).SetWidth(dragHalfW * 2);
                        if (!EditorApplication.isPlaying && dragDurationR.Contains(mouseP)) {
                            _isPreparingToDragDuration = true;
                            _isPreparingToDragDurationMainTarget = sequenced;
                        }
                    }
                    // Button
                    if (EditorGUI.DropdownButton(r, GUIContent.none, FocusType.Passive, GUIStyle.none)) {
                        GUI.FocusControl(null);
                        if (layer.locked) editor.ShowNotification(new GUIContent(string.Format("Layer \"{0}\" is locked", layer.name)));
                        else {
                            if (DeGUIKey.shift) {
                                if (TimelineSelection.Contains(sequenced)) TimelineSelection.Deselect(sequenced);
                                else TimelineSelection.Select(sequenced, true);
                            } else {
                                TimelineSelection.Select(sequenced, TimelineSelection.Contains(sequenced));
                                if (!EditorApplication.isPlaying) {
                                    DragSequenceds(true);
                                    // Move sequenced to back (so it appears above other sequenceds in the same layer)
                                    string[] layerSequencedGuids = sequence.layers[TimelineSelection.Sequenceds[0].originalLayerIndex].sequencedGuids;
                                    int sIndex = Array.IndexOf(layerSequencedGuids, sequenced.guid);
                                    layerSequencedGuids.Shift(sIndex, layerSequencedGuids.Length - 1);
                                }
                            }
                        }
                    }
                }
            }
            // Graphics pass
            for (int i = 0; i < len; ++i) {
                DOVisualSequenced sequenced = sequence.FindSequencedByGuid(layer.sequencedGuids[i]);
                if (sequenced == null) continue;
                bool visible = sequenced.startTime < layout.lastVisibleTime
                               && sequenced.startTime + sequenced.Editor_DrawDuration() * sequenced.Editor_PositiveLoopValue() > layout.firstVisibleTime;
                if (!visible && isMainPass) continue; // Selections are evaluated even if sequenced is not visible, so that multi-selection looks correct
                bool isGlobal = false, isEvent = false, isAction = false, isInterval = false;
                bool requiresPlugin = true;
                switch (sequenced.type) {
                case DOVisualSequenced.Type.GlobalTween:
                    isGlobal = true;
                    break;
                case DOVisualSequenced.Type.Event:
                    isEvent = true;
                    requiresPlugin = false;
                    break;
                case DOVisualSequenced.Type.Action:
                    isAction = true;
                    // isGlobal set later when plugin is loaded
                    break;
                case DOVisualSequenced.Type.Interval:
                    isInterval = true;
                    requiresPlugin = false;
                    break;
                }
                Rect r = TimelineEditorUtils.GetSequencedRect(sequenced, layerR);
                if (isMainPass) {
                    // Main
                    bool isTweener = true;
                    bool skipDuringPreview = false;
                    Color color;
                    switch (sequenced.type) {
                    case DOVisualSequenced.Type.Event:
                        skipDuringPreview = true;
                        color = DOEGUI.Colors.timeline.sEvent;
                        break;
                    case DOVisualSequenced.Type.Action:
                        isTweener = false;
                        skipDuringPreview = true;
                        color = DOEGUI.Colors.timeline.sAction;
                        break;
                    case DOVisualSequenced.Type.Interval:
                        color = DOEGUI.Colors.timeline.sInterval;
                        break;
                    case DOVisualSequenced.Type.GlobalTween:
                        skipDuringPreview = true;
                        color = DOEGUI.Colors.timeline.sGlobalTween;
                        break;
                    default:
                        color = DOEGUI.Colors.timeline.sTween;
                        break;
                    }
                    if (isTweener && sequenced.Editor_HasMultipleLoops()) {
                        // Loops underlay
                        using (new DeGUI.ColorScope(null, null, DOEGUI.Colors.timeline.sTweenerLoop)) {
                            GUI.Box(
                                r.SetWidth(sequenced.Editor_DrawDuration() * sequenced.Editor_PositiveLoopValue() * settings.secondToPixels),
                                GUIContent.none, DOEGUI.Styles.timeline.sequencedLoop
                            );
                        }
                    }
                    // Button graphics
                    if (DOTimelinePreviewManager.isPlayingOrPreviewing && skipDuringPreview) {
                        using (new DeGUI.ColorScope(null, null, color)) {
                            GUI.Box(r, GUIContent.none, DOEGUI.Styles.timeline.btSequencedSkipped);
                        }
                    } else {
                        using (new DeGUI.ColorScope(null, null, color)) {
                            GUI.Box(r, GUIContent.none, DOEGUI.Styles.timeline.btSequenced);
                        }
                        using (new DeGUI.ColorScope(null, null, new Color(1f, 1f, 1f, 0.05f))) {
                            GUI.Box(r, GUIContent.none, DOEGUI.Styles.timeline.btSequencedOutline);
                        }
                    }
                    // Labels
                    Rect lineR = r.SetHeight(Mathf.Min(r.height, 16));
                    Rect icoR = new Rect(0, 0, 16, 16).SetCenter(lineR.x + 9, lineR.center.y);
                    Rect contentR = lineR.ShiftXAndResize(icoR.xMax + 2 - icoR.x);
                    bool hasTarget = sequenced.target != null;
                    DOVisualActionPlugin actionPlugin = null;
                    DOVisualTweenPlugin tweenPlugin = null;
                    bool hasPlugin = false;
                    if (isAction) {
                        actionPlugin = DOVisualPluginsManager.GetActionPlugin(sequenced.plugId);
                        PlugDataAction plugData = actionPlugin == null ? null : actionPlugin.GetPlugData(sequenced);
                        hasPlugin = actionPlugin != null && plugData != null;
                        isGlobal = !hasPlugin || plugData.wantsTarget == false;
                        if (hasPlugin && plugData != null && string.IsNullOrEmpty(sequenced.plugDataGuid) && !string.IsNullOrEmpty(plugData.guid)) {
                            // Legacy fix for plugData being stored by index instead of GUID:
                            // assign plugDataGuid to sequenced if missing
                            if (TimelineSession.logMissingPlugDataGuidAssignment) Debug.Log("Assign plugDataGuid ► " + plugData.guid);
                            sequenced.plugDataGuid = plugData.guid;
                            GUI.changed = true;
                        }
                    } else if (requiresPlugin) {
                        tweenPlugin = isGlobal
                            ? DOVisualPluginsManager.GetGlobalTweenPlugin(sequenced.plugId)
                            : hasTarget ? DOVisualPluginsManager.GetTweenPlugin(sequenced.target) : null;
                        hasPlugin = tweenPlugin != null;
                        if (hasPlugin && string.IsNullOrEmpty(sequenced.plugDataGuid)) {
                            // Legacy fix for plugData being stored by index instead of GUID:
                            // assign plugDataGuid to sequenced if missing
                            ITweenPluginData plugData = tweenPlugin.GetPlugData(sequenced);
                            if (plugData != null && !string.IsNullOrEmpty(plugData.guid)) {
                                if (TimelineSession.logMissingPlugDataGuidAssignment) Debug.Log("Assign plugDataGuid ► " + plugData.guid);
                                sequenced.plugDataGuid = plugData.guid;
                                GUI.changed = true;
                            }
                        }
                    }
                    bool hasMissingRequiredTargetError = hasPlugin && !isEvent && !isInterval && !isGlobal && !hasTarget;
                    bool hasError = requiresPlugin && !hasPlugin || hasMissingRequiredTargetError;
                    if (hasError) {
                        // Missing plugin or missing required target
                        using (new DeGUI.ColorScope(null, null, Color.black)) GUI.Box(r.Contract(2), GUIContent.none, DOEGUI.Styles.box.roundOutline02);
                        using (new DeGUI.ColorScope(null, null, Color.red)) GUI.Box(r, GUIContent.none, DOEGUI.Styles.box.roundOutline02);
                        GUI.DrawTexture(icoR, DeStylePalette.ico_alert, ScaleMode.ScaleToFit);
                    }
                    Rect labelR = new Rect();
                    if (isInterval) {
                        labelR = hasError ? contentR : contentR.Shift(-icoR.width + 2, 0, icoR.width - 4, 0);
                        GUI.Label(labelR, "<color=#006666><b>INTERVAL</b></color>", DOEGUI.Styles.timeline.sequencedLabel);
                    } else if (isEvent) {
                        labelR = hasError ? contentR : contentR.Shift(-icoR.width + 2, 0, icoR.width - 4, 0);
                        GUI.Label(labelR, "<color=#ffffff><b>EVENT</b></color>", DOEGUI.Styles.timeline.sequencedLabel);
                    } else if (isGlobal) {
                        labelR = hasError ? contentR : contentR.Shift(-icoR.width + 2, 0, icoR.width - 4, 0);
                        GUI.Label(labelR,
                            !hasPlugin
                                ? "<color=#fff219><b>unset</b></color>"
                                : string.Format("<b>{0}</b>", isAction
                                    ? actionPlugin.Editor_GetSequencedHeaderLabelGUIContent(sequenced, true).text
                                    : tweenPlugin.Editor_GetAnimationNameGUIContent(sequenced).text
                                ),
                            DOEGUI.Styles.timeline.sequencedLabel
                        );
                    } else {
                        if (hasTarget) {
                            if (hasPlugin) GUI.DrawTexture(icoR, AssetPreview.GetMiniThumbnail(sequenced.target), ScaleMode.ScaleToFit);
                            labelR = contentR.Shift(0, 0, -2, 0);
                            GUI.Label(labelR,
                                !hasPlugin
                                    ? string.Format(
                                        "{0}→<color=#fff219><b>{1} not supported</b></color>", sequenced.target.name, TimelineEditorUtils.GetCleanType(sequenced.target.GetType())
                                    )
                                    : string.Format(
                                        "{0}→<b>{1}</b>", sequenced.target.name,
                                        isAction
                                            ? actionPlugin.Editor_GetSequencedHeaderLabelGUIContent(sequenced, true).text
                                            : tweenPlugin.Editor_GetAnimationNameGUIContent(sequenced).text
                                    ),
                                DOEGUI.Styles.timeline.sequencedLabel
                            );
                        } else if (hasMissingRequiredTargetError) {
                            labelR = contentR.Shift(0, 0, -2, 0);
                            GUI.Label(labelR,
                                string.Format(
                                    "→<b>{0}</b>",
                                    isAction
                                        ? actionPlugin.Editor_GetSequencedHeaderLabelGUIContent(sequenced, true).text
                                        : tweenPlugin.Editor_GetAnimationNameGUIContent(sequenced).text
                                ),
                                DOEGUI.Styles.timeline.sequencedLabel
                            );
                        } else {
                            // Target missing (which caused non-action plugin to be set to NULL)
                            // Meaning this is a target tween with no target (and thus we can't determine the type)
                            GUI.Label(contentR.Shift(0, 0, -2, 0), "<color=#fff219><b>missing target</b></color>", DOEGUI.Styles.timeline.sequencedLabel);
                        }
                    }
                    // Developer Debug
                    if (TimelineSession.showSequencedsPlugDataIndexAndGuid) {
                        labelR = labelR.height > 0 ? lineR.SetY(labelR.yMax - 4).ShiftXAndResize(4) : contentR;
                        GUI.Label(labelR,
                            string.Format("{0}►{1}", sequenced.plugDataIndex, string.IsNullOrEmpty(sequenced.plugDataGuid) ? "no plugDataGuid" : sequenced.plugDataGuid),
                            DOEGUI.Styles.timeline.sequencedLabel);
                    }
                    // Eventual pin
                    if (sequenced.pin > 0) {
                        Rect pinR = new Rect(r.xMax - 14 - 3, r.y + 2, 14, 14);
                        using (new DeGUI.ColorScope(null, null, _pinBorderColor)) {
                            GUI.DrawTexture(pinR.Expand(1), DeStylePalette.circle);
                        }
                        using (new DeGUI.ColorScope(null, null, _pinBgColor)) {
                            GUI.DrawTexture(pinR, DeStylePalette.circle);
                        }
                        GUI.Label(pinR, sequenced.pin.ToString(), DOEGUI.Styles.timeline.sequencedPin);
                    }
                } else {
                    // Selection
                    if (TimelineSelection.isDraggingSelection) {
                        // If dragging selection determine if this sequenced should be selected
                        if (!layer.locked) {
                            if (_dragSelectionR.Overlaps(r)) TimelineSelection.Select(sequenced, true);
                            else TimelineSelection.Deselect(sequenced);
                        }
                    }
                    if (TimelineSelection.Contains(sequenced)) {
                        Rect selectionR = r;
                        _allSelectedR = _allSelectedR.x > 9999 ? selectionR : _allSelectedR.Add(selectionR);
                        using (new DeGUI.ColorScope(null, null, Color.black)) {
                            GUI.Box(selectionR.Contract(1), GUIContent.none, DOEGUI.Styles.timeline.singleSelection);
                            GUI.Box(selectionR.Contract(2), GUIContent.none, DOEGUI.Styles.timeline.singleSelection);
                            GUI.Box(selectionR.Expand(1), GUIContent.none, DOEGUI.Styles.timeline.singleSelection);
                        }
                        using (new DeGUI.ColorScope(null, null, _selectionColor)) {
                            GUI.Box(selectionR, GUIContent.none, DOEGUI.Styles.timeline.singleSelection);
                        }
                    }
                }
            }
            if (isMainPass) {
                // Inactive
                if (!layer.isActive) {
                    DeGUI.DrawColoredSquare(layerR, _inactiveOverlayColor);
                }
                // Locked
                if (layer.locked) {
                    DeGUI.DrawTiledTexture(layerR, DeStylePalette.tileBars_empty, 0.5f, _lockedBgColor);
                }
                // Color
                if (layer.color != DOVisualSequence.VisualLayer.DefColor) {
                    DeGUI.DrawColoredSquare(layerR.ShiftYAndResize(layerR.height - 2), layer.color);
                }
            }
        }

        void DrawOverlay()
        {
            if (TimelineSelection.isDraggingSelection) {
                // Selection drag area
                if (_dragSelectionR.width > 1 || _dragSelectionR.height > 1) {
                    using (new DeGUI.ColorScope(null, null, _selectionColor)) {
                        GUI.Box(_dragSelectionR, GUIContent.none, DOEGUI.Styles.timeline.selectionArea);
                    }
                }
            } else if (TimelineSelection.totSequenceds > 1) {
                // Multi-selection box
                using (new DeGUI.ColorScope(null, null, _selectionColor)) {
                    GUI.Box(_allSelectedR, GUIContent.none, DOEGUI.Styles.timeline.multiSelection);
                }
            }
            const int overlayH = 16;
            if (TimelineSelection.isDraggingSequenceds || TimelineSelection.isDraggingDuration) {
                // Time or duration overlay
                using (new DeGUI.ColorScope(_selectionColor)) {
                    for (int i = 0; i < TimelineSelection.totSequenceds; ++i) {
                        Rect sequencedR = TimelineEditorUtils.GetSequencedRect(TimelineSelection.Sequenceds[i].sequenced, area.width);
                        float timeVal;
                        GUIStyle draggedLabelStyle;
                        if (TimelineSelection.isDraggingSequenceds) {
                            timeVal = TimelineSelection.Sequenceds[i].sequenced.startTime;
                            draggedLabelStyle = DOEGUI.Styles.timeline.draggedTimeLabel;
                        } else {
                            timeVal = TimelineSelection.Sequenceds[i].sequenced.duration;
                            draggedLabelStyle = DOEGUI.Styles.timeline.draggedDurationLabel;
                        }
                        string label = TimelineEditorUtils.ConvertSecondsToTimeString(timeVal, true, true);
                        int w = (int)draggedLabelStyle.CalcSize(new GUIContent(label)).x;
                        Rect timeR = new Rect(sequencedR.x, sequencedR.y, Mathf.Max(w, sequencedR.width), Mathf.Max(overlayH, sequencedR.height));
                        if (TimelineSelection.isDraggingDuration) timeR.x = sequencedR.xMax - timeR.width;
                        GUI.Label(timeR, label, draggedLabelStyle);
                    }
                }
                // Snapping overlay
                if (_snapToSequencedData.isSnappingToSequenced) {
                    using (new DeGUI.ColorScope(_snapToSequencedColor)) {
                        Rect topR, bottomR;
                        if (_snapToSequencedData.selectedSequencedR.yMin < _snapToSequencedData.snapToSequencedR.yMin) {
                            topR = _snapToSequencedData.selectedSequencedR;
                            bottomR = _snapToSequencedData.snapToSequencedR;
                        } else {
                            topR = _snapToSequencedData.snapToSequencedR;
                            bottomR = _snapToSequencedData.selectedSequencedR;
                        }
                        GUI.Box(_snapToSequencedData.snapToSequencedR, GUIContent.none, DOEGUI.Styles.timeline.snapSequenced);
                        Rect lineR = new Rect(
                            (_snapToSequencedData.isSnappingToSelfDuration ? _snapToSequencedData.selectedSequencedR.xMax : _snapToSequencedData.selectedSequencedR.x)
                                + (TimelineSelection.isDraggingSequenceds ? 0 : _snapToSequencedData.selectedSequencedR.width) - 1,
                            topR.y,
                            2, Mathf.Abs(topR.y - bottomR.yMax)
                        );
                        DeGUI.DrawColoredSquare(lineR, _snapToSequencedColor);
                    }
                }
            }
        }

        void DrawTimeSeparators()
        {
            float fullWidth = area.width - layout.partialOffset.x;
            int totColumns = Mathf.CeilToInt(fullWidth / settings.secondToPixels);
            int firstColIndex = -timelineShift.x / settings.secondToPixels;
            Rect colR = new Rect(layout.partialOffset.x, area.y, 1, area.height).SetHeight(layout.visibleTimelineHeight);
            for (int i = 0; i < totColumns; ++i) {
                DeGUI.DrawColoredSquare(colR, (firstColIndex + i) % 5 == 0 ? _timeSeparatorColor1 : _timeSeparatorColor0);
                // Fractions separator
                const float fractions = 4;
                Rect fractionR = colR;
                for (int j = 0; j < fractions - 1; ++j) {
                    fractionR = fractionR.Shift(settings.secondToPixels / fractions, 0, 0, 0);
                    DeGUI.DrawColoredSquare(fractionR, _separatorFractionColor);
                }
                //
                colR = colR.Shift(settings.secondToPixels, 0, 0, 0);
            }
        }

        void DragTimeline(bool begin = false)
        {
            if (begin) {
                _isDraggingTimeline = true;
                _dragStartP = Event.current.mousePosition;
                _timelineShiftSnapshot = timelineShift;
                editor.Repaint();
            } else {
                timelineShift = new Vector2Int(
                    Mathf.Min(0, _timelineShiftSnapshot.x + (int)(Event.current.mousePosition.x - _dragStartP.x)),
                    Mathf.Min(
                        0,
                        Mathf.Max(
                            sequence.layers == null ? 0 : -sequence.layers.Length * settings.layerHeight,
                            _timelineShiftSnapshot.y + (int)(Event.current.mousePosition.y - _dragStartP.y)
                    ))
                );
            }
        }

        void StopDragTimeline()
        {
            _isDraggingTimeline = false;
            editor.Repaint();
        }

        void DragSelection(bool begin = false)
        {
            if (begin) {
                TimelineSelection.isDraggingSelection = true;
                _dragStartP = Event.current.mousePosition;
                _dragSelectionR = new Rect(_dragStartP.x, _dragStartP.y, 0, 0);
                editor.Repaint();
            } else {
                Vector2 mouseP = Event.current.mousePosition;
                if (mouseP.x < 0) mouseP.x = 0;
                else if (mouseP.x > area.width) mouseP.x = area.width;
                if (mouseP.y < 0) mouseP.y = 0;
                else if (mouseP.y > area.height) mouseP.y = area.height;
                if (mouseP.x > _dragStartP.x) _dragSelectionR.x = _dragStartP.x;
                else _dragSelectionR.x = mouseP.x;
                if (mouseP.y > _dragStartP.y) _dragSelectionR.y = _dragStartP.y;
                else _dragSelectionR.y = mouseP.y;
                _dragSelectionR.width = Mathf.Abs(mouseP.x - _dragStartP.x);
                _dragSelectionR.height = Mathf.Abs(mouseP.y - _dragStartP.y);
                editor.Repaint();
            }
        }

        void StopDragSelection()
        {
            TimelineSelection.isDraggingSelection = false;
            editor.Repaint();
        }

        void DragSequenceds(bool begin = false)
        {
            _snapToSequencedData.Reset();
            if (begin) {
                _isPreparingToDragSequenceds = true;
                _dragStartP = _dragCurrMouseP = Event.current.mousePosition;
                editor.Repaint();
            } else {
                if (_isPreparingToDragSequenceds) {
                    if (Vector2.Distance(Event.current.mousePosition, _dragStartP) > settings.minPixelDragDistance) {
                        _isPreparingToDragSequenceds = false;
                        TimelineSelection.isDraggingSequenceds = true;
                        _draggedMainSequencedCurrLayerIndex = TimelineSelection.Sequenceds[0].originalLayerIndex;
                        EditorApplication.update += CheckShiftTimelineContinuous;
                        // Set min max shift for layers
                        int layersLen = sequence.layers.Length;
                        for (int i = 0; i < TimelineSelection.Sequenceds.Count; ++i) {
                            int layerIndex = TimelineSelection.Sequenceds[i].originalLayerIndex;
                            int thisMax = layersLen - layerIndex - 1;
                            int thisMin = -layerIndex;
                            if (i == 0) {
                                _draggedSequencedsMinLayerShift = thisMin;
                                _draggedSequencedsMaxLayerShift = thisMax;
                            } else if (_draggedSequencedsMinLayerShift < thisMin) _draggedSequencedsMinLayerShift = thisMin;
                            else if (_draggedSequencedsMaxLayerShift > thisMax) _draggedSequencedsMaxLayerShift = thisMax;
                        }
                        // Refresh selection cache
                        TimelineSelection.RefreshSelectionsData();
                    }
                }
                if (TimelineSelection.isDraggingSequenceds) {
                    _dragCurrMouseP = Event.current.mousePosition;
                    _isSnapDragging = DeGUIKey.ctrl;
                    // Vertical/layer shift
                    int currMouseLayerIndex = layout.GetLayerIndexAtMouse(true);
                    if (currMouseLayerIndex != _draggedMainSequencedCurrLayerIndex) {
                        // Switch layer for all selected
                        int shift = currMouseLayerIndex - _draggedMainSequencedCurrLayerIndex;
                        if (shift > _draggedSequencedsMaxLayerShift) shift = _draggedSequencedsMaxLayerShift;
                        if (shift < _draggedSequencedsMinLayerShift) shift = _draggedSequencedsMinLayerShift;
                        if (shift != 0) {
                            for (int i = 0; i < TimelineSelection.Sequenceds.Count; ++i) {
                                TimelineSelection.SelectedSequenced sel = TimelineSelection.Sequenceds[i];
                                int shiftTo = sel.originalLayerIndex + shift;
                                TimelineEditorUtils.ShiftSequencedToLayer(sequence, sel.sequenced, sel.originalLayerIndex, shiftTo);
                                sel.originalLayerIndex = shiftTo;
                            }
                            _draggedMainSequencedCurrLayerIndex += shift;
                            _draggedSequencedsMinLayerShift -= shift;
                            _draggedSequencedsMaxLayerShift -= shift;
                        }
                    }
                    if (DeGUIKey.shift) {
                        // Lock horizontal shift
                        for (int i = 0; i < TimelineSelection.Sequenceds.Count; ++i) {
                            TimelineSelection.Sequenceds[i].sequenced.startTime = TimelineSelection.Sequenceds[i].originalStartTime;
                        }
                    } else {
                        // Horizontal shift
                        // First find max offset so that no selected tween reaches a startTime below 0
                        float timeOffset = (_dragCurrMouseP.x - _dragStartP.x) / settings.secondToPixels;
                        ShiftDraggedSequencedsTime(timeOffset);
                    }
                    GUI.changed = true;
                }
            }
        }

        void StopDraggingSequenceds()
        {
            TimelineSelection.isDraggingSequenceds = _isPreparingToDragSequenceds = _isSnapDragging = false;
            _snapToSequencedData.Reset();
            TimelineSelection.RefreshSelectionsData();
            EditorApplication.update -= CheckShiftTimelineContinuous;
            editor.Repaint();
        }

        void DragSequencedsDuration(bool begin = false)
        {
            _snapToSequencedData.Reset();
            if (begin) {
                TimelineSelection.isDraggingDuration = true;
                _durationDraggingWaitingForFirstStep = true;
                _dragStartP = _dragCurrMouseP = Event.current.mousePosition;
                if (!TimelineSelection.Contains(_isPreparingToDragDurationMainTarget)) {
                    TimelineSelection.Select(_isPreparingToDragDurationMainTarget);
                }
                TimelineSelection.RefreshSelectionsData();
                editor.Repaint();
            } else {
                if (_durationDraggingWaitingForFirstStep) {
                    if (Vector2.Distance(Event.current.mousePosition, _dragStartP) > settings.minPixelDragDistance) {
                        _durationDraggingWaitingForFirstStep = false;
                    }
                }
                if (!_durationDraggingWaitingForFirstStep) {
                    _dragCurrMouseP = Event.current.mousePosition;
                    _isSnapDragging = DeGUIKey.ctrl;
                    float timeOffset = (_dragCurrMouseP.x - _dragStartP.x) / settings.secondToPixels;
                    // Check eventual snapping with other sequenceds
                    bool isSnappingToSequenced = false;
                    if (_keyAlt) {
                        TimelineSelection.SelectedSequenced mainSel = TimelineSelection.Sequenceds[0];
                        float mainCurrTime = mainSel.sequenced.startTime + mainSel.originalDuration;
                        isSnappingToSequenced = CheckSnapToSequencedTimeOffset(mainCurrTime, ref timeOffset, -999999, ref _snapToSequencedData);
                    }
                    for (int i = 0; i < TimelineSelection.totSequenceds; ++i) {
                        TimelineSelection.SelectedSequenced sel = TimelineSelection.Sequenceds[i];
                        float toDuration = Mathf.Max(0, sel.originalDuration + timeOffset);
                        if (!isSnappingToSequenced && i == 0) {
                            // snap to 0.25 or 0.01 increments
                            float snapValue = _isSnapDragging ? 0.25f : 0.01f;
                            toDuration -= toDuration % snapValue;
                            timeOffset = toDuration - sel.originalDuration;
                        }
                        if (sel.sequenced.Editor_HasDuration()) sel.sequenced.duration = toDuration;
                        if (isSnappingToSequenced) {
                            _snapToSequencedData.selectedSequencedR = TimelineEditorUtils.GetSequencedRect(TimelineSelection.Sequenceds[0].sequenced, area.width);
                        }
                    }
                    GUI.changed = true;
                }
            }
        }

        void StopDraggingSequencedsDuration()
        {
            _isPreparingToDragDuration = _durationDraggingWaitingForFirstStep = TimelineSelection.isDraggingDuration = false;
            TimelineSelection.RefreshSelectionsData();
            editor.Repaint();
        }

        void ShiftDraggedSequencedsTime(float timeOffset, bool isTimelineShift = false)
        {
            if (timeOffset < 0.0001f && timeOffset > -0.0001f) return;
            // First find max offset so that no selected tween reaches a startTime below 0
            float minTimeOffset = -999999;
            for (int i = 0; i < TimelineSelection.totSequenceds; ++i) {
                TimelineSelection.SelectedSequenced sel = TimelineSelection.Sequenceds[i];
                if (-(isTimelineShift ? sel.sequenced.startTime : sel.originalStartTime) > minTimeOffset) {
                    minTimeOffset = -(isTimelineShift ? sel.sequenced.startTime : sel.originalStartTime);
                }
            }
            timeOffset = Mathf.Max(minTimeOffset, timeOffset);
            // Check eventual snapping with other sequenceds
            bool isSnappingToSequenced = false;
            if (_keyAlt) {
                TimelineSelection.SelectedSequenced mainSel = TimelineSelection.Sequenceds[0];
                float mainCurrTime = isTimelineShift ? mainSel.sequenced.startTime : mainSel.originalStartTime;
                isSnappingToSequenced = CheckSnapToSequencedTimeOffset(mainCurrTime, ref timeOffset, minTimeOffset, ref _snapToSequencedData);
                if (!isSnappingToSequenced) {
                    // Check sequenced end position (start time + duration) VS others
                    isSnappingToSequenced = CheckSnapToSequencedTimeOffset(mainCurrTime + mainSel.sequenced.duration, ref timeOffset, minTimeOffset, ref _snapToSequencedData);
                    if (isSnappingToSequenced) _snapToSequencedData.isSnappingToSelfDuration = true;
                };
            }
            //
            for (int i = 0; i < TimelineSelection.totSequenceds; ++i) {
                TimelineSelection.SelectedSequenced sel = TimelineSelection.Sequenceds[i];
                float toTime = (isTimelineShift ? sel.sequenced.startTime : sel.originalStartTime) + timeOffset;
                if (!isSnappingToSequenced && i == 0) { // snap to 0.25 or 0.01 increments (unless we're snapping to another sequenced)
                    float snapValue = _isSnapDragging ? 0.25f : DOTimelineSettings.MinSequencedSnapping;
                    toTime -= toTime % snapValue;
                    timeOffset = toTime - (isTimelineShift ? sel.sequenced.startTime : sel.originalStartTime);
                }
                sel.sequenced.startTime = toTime;
                if (isSnappingToSequenced) {
                    _snapToSequencedData.selectedSequencedR = TimelineEditorUtils.GetSequencedRect(TimelineSelection.Sequenceds[0].sequenced, area.width);
                }
            }
        }

        void CheckShiftTimelineContinuous()
        {
            const int borderX = 100;
            const float maxShift = 7;
            Vector2Int prevTimelineShift = timelineShift;
            float borderRight = area.xMax - borderX;
            if (_dragCurrMouseP.x < borderX) {
                int power = (int)(maxShift * (borderX - _dragCurrMouseP.x) / borderX);
                timelineShift = new Vector2Int((int)Mathf.Min(0, timelineShift.x + power), timelineShift.y);
            } else if (_dragCurrMouseP.x > borderRight) {
                int power = (int)(maxShift * (_dragCurrMouseP.x - borderRight) / borderX);
                timelineShift = new Vector2Int(timelineShift.x - power, timelineShift.y);
            }
            if (prevTimelineShift != timelineShift) {
                Vector2 diff = prevTimelineShift - timelineShift;
                _dragStartP.x -= diff.x;
                float timeOffset = diff.x / settings.secondToPixels;
                ShiftDraggedSequencedsTime(timeOffset, true);
                GUI.changed = true;
                editor.Repaint();
            }
        }

        bool IsValidDragAndDrop(UnityEngine.Object[] allDragged)
        {
            if (allDragged.Length != 1) return false;
            if (layout.GetLayerIndexAtMouse() == -1) return false;
            UnityEngine.Object dragged = allDragged[0];
            if (dragged is GameObject) return ((GameObject)dragged).scene.rootCount != 0; // Ignore prefabs
            if (dragged is Component) return ((Component)dragged).gameObject.scene.rootCount != 0; // Ignore prefabs
            return false;
        }

        void CompleteDragAndDrop(Object dragged)
        {
            int layerIndex = layout.GetLayerIndexAtMouse();
            float atTime = layout.GetSecondsAtMouse();
            TimelineEditorUtils.CM_SelectSequencedTargetFromGameObject(
                dragged is GameObject ? (GameObject)dragged : ((Component)dragged).gameObject,
                (component) => {
                    using (new DOScope.UndoableSerialization()) {
                        DOVisualSequenced sequenced = new DOVisualSequenced(Guid.NewGuid().ToString(), DOVisualSequenced.Type.Tween, atTime) {
                            target = component
                        };
                        AddSequenced(sequenced, sequence.layers[layerIndex]);
                    }
                    DOVisualSequenceTimeline.Dispatch_OnSequenceChanged(sequence);
                }
            );
        }

        #region Context Menus

        void CM_EmptyArea()
        {
            if (EditorApplication.isPlaying) return;

            int layerIndex = layout.GetLayerIndexAtMouse();
            float timePosition = layout.GetSecondsAtMouse();
            GenericMenu menu = new GenericMenu();
            menu.AddItem(new GUIContent(string.Format("Reset shift (current: {0},{1})", timelineShift.x, timelineShift.y)), false,
                () => {
                    using (new DOScope.UndoableSerialization()) timelineShift = new Vector2Int(0, 0);
                });
            menu.AddSeparator("");
            int totSequencedsInClipboard = TimelineClipboard.TotMemorizedSequenceds();
            if (totSequencedsInClipboard > 0) {
                menu.AddItem(
                    new GUIContent(string.Format("Paste {0} Clip{1} [Ctrl+V]", totSequencedsInClipboard, totSequencedsInClipboard > 1 ? "s" : "")),
                    false, ()=> PasteSequencedsFromClipboard(timePosition, layerIndex)
                );
                menu.AddItem(
                    new GUIContent(string.Format("Paste {0} Clip{1} At Original Time [Ctrl+Shift+V]", totSequencedsInClipboard, totSequencedsInClipboard > 1 ? "s" : "")),
                    false, ()=> PasteSequencedsFromClipboard(timePosition, layerIndex, true)
                );
            } else menu.AddDisabledItem(new GUIContent("Paste [Ctrl+V]"));
            menu.AddSeparator("");
            if (layerIndex != -1) {
                if (sequence.layers[layerIndex].locked) {
                    menu.AddDisabledItem(new GUIContent("[Layer is locked]"));
                } else {
                    menu.AddItem(new GUIContent("Add Tween"), false, () => {
                        AddSequenced(new DOVisualSequenced(
                            Guid.NewGuid().ToString(), DOVisualSequenced.Type.Tween, timePosition), sequence.layers[layerIndex]
                        );
                    });
                    menu.AddItem(new GUIContent("Add Global Tween"), false, () => {
                        AddSequenced(new DOVisualSequenced(
                                Guid.NewGuid().ToString(), DOVisualSequenced.Type.GlobalTween, timePosition), sequence.layers[layerIndex]
                        );
                    });
                    menu.AddItem(new GUIContent("Add Event"), false, () => {
                        AddSequenced(new DOVisualSequenced(
                                Guid.NewGuid().ToString(), DOVisualSequenced.Type.Event, timePosition), sequence.layers[layerIndex]
                        );
                    });
                    menu.AddItem(new GUIContent("Add Action"), false, () => {
                        AddSequenced(new DOVisualSequenced(
                                Guid.NewGuid().ToString(), DOVisualSequenced.Type.Action, timePosition), sequence.layers[layerIndex]
                        );
                    });
                    menu.AddItem(new GUIContent("Add Interval"), false, () => {
                        AddSequenced(new DOVisualSequenced(
                                Guid.NewGuid().ToString(), DOVisualSequenced.Type.Interval, timePosition), sequence.layers[layerIndex]
                        );
                    });
                }
            }
            menu.ShowAsContext();
            editor.Repaint();
        }

        void CM_Sequenced()
        {
            if (EditorApplication.isPlaying) return;

            GenericMenu menu = new GenericMenu();
            menu.AddItem(new GUIContent("Cut [Ctrl+X]"), false, ()=> CopySelectedSequenceds(true));
            menu.AddItem(new GUIContent("Copy [Ctrl+C]"), false, ()=> CopySelectedSequenceds(false));
            menu.AddSeparator("");
            menu.AddItem(new GUIContent("Remove Pin"), false, () => {
                using (new DOScope.UndoableSerialization()) {
                    for (int i = 0; i < TimelineSelection.totSequenceds; ++i) TimelineSelection.Sequenceds[i].sequenced.pin = -1;
                }
            });
            for (int i = 1; i < 11; ++i) {
                int pinVal = i;
                menu.AddItem(
                    new GUIContent("Pin/" + pinVal), TimelineSelection.totSequenceds == 1 && TimelineSelection.Sequenceds[0].sequenced.pin == pinVal,
                    () => {
                        using (new DOScope.UndoableSerialization()) {
                            for (int j = 0; j < TimelineSelection.totSequenceds; ++j) TimelineSelection.Sequenceds[j].sequenced.pin = pinVal;
                        }
                    }
                );
            }
            menu.ShowAsContext();
            editor.Repaint();
        }

        #endregion

        #endregion

        #region Methods

        void AddSequenced(DOVisualSequenced sequenced, DOVisualSequence.VisualLayer layer, bool isCopy = false)
        {
            using (new DOScope.UndoableSerialization()) {
                DeEditorUtils.Array.ExpandAndAdd(ref layer.sequencedGuids, sequenced.guid);
                DeEditorUtils.Array.ExpandAndAdd(ref sequence.sequenceds, sequenced);
                if (!isCopy) {
                    sequenced.isActive = layer.isActive;
                    switch (sequenced.type) {
                    case DOVisualSequenced.Type.Action:
                    case DOVisualSequenced.Type.Event:
                        sequenced.duration = 0;
                        break;
                    case DOVisualSequenced.Type.Tween:
                        sequenced.duration = settings.defaults.duration;
                        DOVisualTweenPlugin plug = DOVisualPluginsManager.GetTweenPlugin(sequenced.target);
                        if (plug != null) {
                            ITweenPluginData plugData = plug.GetPlugData(sequenced);
                            if (plugData != null) sequenced.plugDataGuid = plugData.guid;
                        }
                        break;
                    default:
                        sequenced.duration = settings.defaults.duration;
                        break;
                    }
                    sequenced.ease = settings.defaults.ease;
                    sequenced.loopType = settings.defaults.loopType;
                }
            }
            using (new DOScope.UndoableSerialization()) TimelineSelection.Select(sequenced, isCopy);
        }

        void CopySelectedSequenceds(bool cut)
        {
            TimelineClipboard.CopySequenceds(sequence, TimelineSelection.GetCleanSelectedSequenceds());
            if (cut) {
                using (new DOScope.UndoableSerialization()) {
                    for (int i = 0; i < TimelineSelection.Sequenceds.Count; ++i) {
                        TimelineEditorUtils.RemoveSequenced(sequence, TimelineSelection.Sequenceds[i].sequenced.guid);
                    }
                    TimelineSelection.DeselectAll();
                }
            }
        }

        void PasteSequencedsFromClipboard(float timePosition, int layerIndex, bool pasteAtOriginalTimePosition = false)
        {
            if (!TimelineClipboard.HasMemorizedSequenceds()) return;
            using (new DOScope.UndoableSerialization()) {
                // First determine if new layers need to be added
                int highestLayerIndex = 0;
                foreach (TimelineClipboard.SequencedCopy sCopy in TimelineClipboard.SequencedCopies) {
                    int sLayerIndex = layerIndex + sCopy.layerIndexOffsetFromUpper;
                    if (sLayerIndex > highestLayerIndex) highestLayerIndex = sLayerIndex;
                }
                if (highestLayerIndex > sequence.layers.Length - 1) {
                    int totLayersToAdd = highestLayerIndex - (sequence.layers.Length - 1);
                    bool proceed = EditorUtility.DisplayDialog("Paste Clips",
                        string.Format(
                            "{0} layer{1} need to be added in order to paste the clips correctly.\n\nProceed?",
                            totLayersToAdd, totLayersToAdd > 1 ? "s" : ""
                        ),
                        "Ok", "Cancel"
                    );
                    if (!proceed) return;
                    for (int i = 0; i < totLayersToAdd; ++i) {
                        DeEditorUtils.Array.ExpandAndAdd(ref sequence.layers, new DOVisualSequence.VisualLayer("Layer " + (sequence.layers.Length + 1)));
                    }
                }
                TimelineSelection.DeselectAll();
                foreach (TimelineClipboard.SequencedCopy sCopy in TimelineClipboard.SequencedCopies) {
                    int sLayerIndex = layerIndex + sCopy.layerIndexOffsetFromUpper;
                    DOVisualSequenced sClone = sCopy.sequenced.Editor_Clone(true);
                    if (!pasteAtOriginalTimePosition) sClone.startTime = timePosition + sCopy.startTimeOffsetFromFirst;
                    AddSequenced(sClone, sequence.layers[sLayerIndex], true);
                }
            }
            editor.Repaint();
        }

        // Returns TRUE if we should snap to another sequenced
        // Out Rects are filled correctly only if snapping is set
        bool CheckSnapToSequencedTimeOffset(
            float mainSelectedTargetTime, ref float dragTimeOffset, float minDragTimeOffset, ref SnapToSequencedData snapToSequencedData
        ){
            snapToSequencedData.Reset();
            float shortestSnapDiff = 99999999;
            float currSnapTimePos = 0;
            float mainSelectedTargetTimeWOffset = mainSelectedTargetTime + dragTimeOffset;
            DOVisualSequenced snapToSequenced = null;
            for (int i = 0; i < sequence.sequenceds.Length; ++i) { // Skip first, obviously
                DOVisualSequenced sequenced = sequence.sequenceds[i];
                if (TimelineSelection.Contains(sequenced)) continue;
                // Check VS start time
                float time = sequenced.startTime;
                float timeDiff = Mathf.Abs(time - mainSelectedTargetTimeWOffset);
                if (timeDiff < shortestSnapDiff) {
                    shortestSnapDiff = timeDiff;
                    currSnapTimePos = time;
                    snapToSequenced = sequenced;
                }
                // Check VS duration
                time = sequenced.startTime + sequenced.duration;
                timeDiff = Mathf.Abs(time - mainSelectedTargetTimeWOffset);
                if (timeDiff < shortestSnapDiff) {
                    shortestSnapDiff = timeDiff;
                    currSnapTimePos = time;
                    snapToSequenced = sequenced;
                }
            }
            if (shortestSnapDiff * settings.secondToPixels <= settings.maxSnapPixelDistance) {
                dragTimeOffset = Mathf.Max(minDragTimeOffset, currSnapTimePos - mainSelectedTargetTime);
                snapToSequencedData.isSnappingToSequenced = true;
                snapToSequencedData.snapToSequencedR = TimelineEditorUtils.GetSequencedRect(snapToSequenced, area.width);
                return true;
            }
            return false;
        }

        #endregion

        // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████
        // ███ INTERNAL CLASSES ████████████████████████████████████████████████████████████████████████████████████████████████
        // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

        struct SnapToSequencedData
        {
            public bool isSnappingToSequenced;
            public bool isSnappingToSelfDuration; // TRUE if snapping to the selected sequenced's end time (start time + duration)
            public Rect selectedSequencedR;
            public Rect snapToSequencedR;

            public void Reset()
            {
                isSnappingToSequenced = isSnappingToSelfDuration = false;
                selectedSequencedR = snapToSequencedR = new Rect();
            }
        }
    }
}