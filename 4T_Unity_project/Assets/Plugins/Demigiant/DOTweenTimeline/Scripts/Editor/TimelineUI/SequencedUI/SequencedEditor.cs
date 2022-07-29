// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/01/20

using System;
using System.Collections.Generic;
using DG.DemiEditor;
using DG.DemiLib;
using DG.Tweening.Timeline.Core;
using DG.Tweening.Timeline.Core.Plugins;
using UnityEditor;
using UnityEngine;
using UnityEngine.Events;
using Object = UnityEngine.Object;

namespace DG.Tweening.TimelineEditor.SequencedUI
{
    internal class SequencedEditor : ABSTimelineElement
    {
        public const int DefaultWidth = 250;

        readonly GUIContent _gcEvent = new GUIContent("Events");
        readonly GUIContent _gcStartTime = new GUIContent("At");
        readonly GUIContent _gcDuration = new GUIContent("Duration");
        readonly GUIContent _gcLoops = new GUIContent("Loops");
        readonly GUIContent _gcTarget = new GUIContent("Target");
        readonly GUIContent _gcEase = new GUIContent("Ease");
        readonly GUIContent _gcOvershoot = new GUIContent("Ovrsht");
        readonly GUIContent _gcAmplitude = new GUIContent("Ampl");
        readonly GUIContent _gcPeriod = new GUIContent("Period");
        readonly GUIContent _gcSnapping = new GUIContent("Snapping");
        readonly GUIContent _gcRichTextEnabled = new GUIContent("Rich-text");
        readonly GUIContent _gcScramble = new GUIContent("Scramble");
        readonly GUIContent _gcScrambleOptions = new GUIContent("Scramble Options");
        readonly GUIContent _gcIsRelative = new GUIContent("Relative");
        readonly GUIContent _gcIsNotRelative = new GUIContent("Absolute");
        readonly GUIContent _gcAlphaOnly = new GUIContent("Fade");
        readonly GUIContent _gcVibrato = new GUIContent("Vibrato");
        readonly GUIContent _gcElasticity = new GUIContent("Elasticity");
        readonly GUIContent _gcRandomness = new GUIContent("Rnd");
        readonly GUIContent _gcFadeOut = new GUIContent("Fade Out");
        readonly GUIContent _gcNumber = new GUIContent("N");
        GUIContent _gcVectorLocked, _gcVectorUnlocked;
        readonly Color _isRelativeColor = new Color(1f, 0.6f, 0f);
        Color _hDividerColor;
        const int _Padding = 2;
        float _lineHeight { get { return EditorGUIUtility.singleLineHeight; } }
        bool _initialized;
        readonly List<DOVisualSequenced> _sequenceds = new List<DOVisualSequenced>();
        readonly List<DOVisualTweenPlugin> _tweenPlugins = new List<DOVisualTweenPlugin>();
        readonly List<DOVisualActionPlugin> _actionPlugins = new List<DOVisualActionPlugin>();
        readonly List<SerializedProperty> _spSequenceds = new List<SerializedProperty>();
        readonly List<SerializedProperty> _spOnCompletes = new List<SerializedProperty>();
        readonly List<SerializedProperty> _spOnStepCompletes = new List<SerializedProperty>();
        readonly List<SerializedProperty> _spOnUpdates = new List<SerializedProperty>();
        DOVisualSequenced _mainSequenced { get { return _sequenceds[0]; } }
        DOVisualTweenPlugin _mainTweenPlugin{ get { return _tweenPlugins[0]; } }
        DOVisualActionPlugin _mainActionPlugin{ get { return _actionPlugins[0]; } }
        SerializedProperty _mainSpSequenced { get { return _spSequenceds[0]; } }
        SerializedProperty _mainSpOnComplete { get { return _spOnCompletes[0]; } }
        SerializedProperty _mainSpOnStepComplete { get { return _spOnStepCompletes[0]; } }
        SerializedProperty _mainSpOnUpdates { get { return _spOnUpdates[0]; } }
        bool _forceShowOnCompletes, _forceShowOnStepCompletes, _forceShowOnUpdates;
        bool _allTargetsSet, _allTargetsTheSame; // True even in case of global tweens/actions
        bool _allPluginsSet, _allPluginsOfSameType, _allPluginsOfSameTypeAndSubtype;
        int _totSequenceds;
        bool _isTweener, _isEvent, _isAction, _isInterval, _isGlobal;
        float _minLabelWidth = 46;
        Color _mainColor;
        AxisConstraint _lockAllAxesTo = AxisConstraint.None;
        AxisConstraint _disabledAxes = AxisConstraint.None;
        DeScrollView _scrollView;
        EaseSelectionWindow _easeSelectionWin = new EaseSelectionWindow();
        readonly List<Object> _currTargets = new List<Object>();
        Texture2D _easePreviewTex;
        EaseSnapshot _lastPreviewedEase;

        #region GUI + INIT

        // Also called each time the target changes
        void Init(bool forceRefresh = false)
        {
            if (!_initialized) {
                _initialized = true;
                _gcVectorLocked = new GUIContent(DeStylePalette.ico_lock);
                _gcVectorUnlocked = new GUIContent(DeStylePalette.ico_lock_open);
                _hDividerColor = new DeSkinColor(new Color(0, 0, 0, 0.2f), new Color(1, 1, 1, 0.1f));
            }

            // Check full refresh and apply every-time refresh
            bool refreshRequired = false;
            _totSequenceds = TimelineSelection.totSequenceds;
            int len = _sequenceds.Count;
            if (_totSequenceds != len) {
                refreshRequired = true;
                _sequenceds.Clear();
                for (int i = 0; i < _totSequenceds; ++i) _sequenceds.Add(TimelineSelection.Sequenceds[i].sequenced);
            } else {
                for (int i = 0; i < len; ++i) {
                    if (!refreshRequired && TimelineSelection.Sequenceds[i].sequenced == _sequenceds[i]) continue;
                    refreshRequired = true;
                    _sequenceds[i] = TimelineSelection.Sequenceds[i].sequenced;
                }
            }
            if (refreshRequired) _forceShowOnCompletes = _forceShowOnStepCompletes = _forceShowOnUpdates = false;
            DOVisualSequenced.Type sType = _totSequenceds > 0 ? _mainSequenced.type : DOVisualSequenced.Type.Tween;
            switch (sType) {
            case DOVisualSequenced.Type.Event:
                _isEvent = true;
                _isTweener = _isAction = _isInterval = _isGlobal = false;
                _mainColor = DOEGUI.Colors.timeline.sEvent;
                break;
            case DOVisualSequenced.Type.Action:
                // isGlobal is set after Init because it requires knowledge of the pluginData
                _isAction = true;
                _isTweener = _isEvent = _isInterval = false;
                _mainColor = DOEGUI.Colors.timeline.sAction;
                break;
            case DOVisualSequenced.Type.Interval:
                _isInterval = true;
                _isAction = _isTweener = _isGlobal = false;
                _mainColor = DOEGUI.Colors.timeline.sInterval;
                break;
            case DOVisualSequenced.Type.GlobalTween:
                _isTweener = _isGlobal = true;
                _isAction = _isEvent = _isInterval = false;
                _mainColor = DOEGUI.Colors.timeline.sGlobalTween;
                break;
            default:
                _isTweener = true;
                _isAction = _isEvent = _isInterval = _isGlobal = false;
                _mainColor = DOEGUI.Colors.timeline.sTween;
                break;
            }
            if (!refreshRequired && !forceRefresh) return;

            // Refresh -----------------------
            // Store serializedProperties
            _spSequenceds.Clear();
            _spOnCompletes.Clear();
            _spOnStepCompletes.Clear();
            _spOnUpdates.Clear();
            for (int i = 0; i < _totSequenceds; ++i) {
                SerializedProperty spSequenced = TimelineEditorUtils.GetSerializedSequenced(spSequence, _sequenceds[i].guid);
                _spSequenceds.Add(spSequenced);
                _spOnCompletes.Add(spSequenced.FindPropertyRelative("onComplete"));
                _spOnStepCompletes.Add(spSequenced.FindPropertyRelative("onStepComplete"));
                _spOnUpdates.Add(spSequenced.FindPropertyRelative("onUpdate"));
            }
            //
            _tweenPlugins.Clear();
            _actionPlugins.Clear();
            if (_isEvent) {
                // Stop here if Event
                _allTargetsSet = _allTargetsTheSame = _allPluginsSet = _allPluginsOfSameType = _allPluginsOfSameTypeAndSubtype = false;
                return;
            }
            //
            _allTargetsSet = _allTargetsTheSame = _allPluginsSet = _allPluginsOfSameType = _allPluginsOfSameTypeAndSubtype = true;
            Object defTarget = null; // Used for targeted tweens/actions
            DOVisualTweenPlugin defPlug = null; // Used for targeted tweens/actions
            string defPlugId = null; // Used for global tweens/actions
            string defPlugDataGuid = null;
            int defPlugDataIndex = -1; // Legacy
            for (int i = 0; i < _totSequenceds; ++i) {
                DOVisualSequenced sequenced = _sequenceds[i];
                if (_isAction) {
                    if (sequenced == null) {
                        _allTargetsSet = _allTargetsTheSame = _allPluginsSet = _allPluginsOfSameType = _allPluginsOfSameTypeAndSubtype = false;
                        _actionPlugins.Add(null);
                    } else {
                        DOVisualActionPlugin plug = DOVisualPluginsManager.GetActionPlugin(sequenced.plugId);
                        bool isValidPlugin = TimelineEditorUtils.ValidateSequencedPlugin(sequenced, plug);
                        if (!isValidPlugin) {
                            plug = null;
                            _allPluginsSet = _allPluginsOfSameType = _allPluginsOfSameTypeAndSubtype = false;
                        }
                        string plugDataGuid = sequenced.plugDataGuid;
                        int plugDataIndex = sequenced.plugDataIndex;
                        if (i == 0) {
                            _isGlobal = !isValidPlugin || plug != null && !plug.GetPlugData(_mainSequenced).wantsTarget;
                            defPlugId = sequenced.plugId;
                            defPlugDataGuid = plugDataGuid;
                            defPlugDataIndex = plugDataIndex;
                        } else {
                            if (_allPluginsOfSameType) {
                                if (sequenced.plugId != defPlugId) _allPluginsOfSameType = _allPluginsOfSameTypeAndSubtype = false;
                                else if (plugDataGuid != defPlugDataGuid || plugDataIndex != defPlugDataIndex) _allPluginsOfSameTypeAndSubtype = false;
                            }
                        }
                        _actionPlugins.Add(plug);
                    }
                } else if (_isTweener) {
                    if (sequenced == null || !_isGlobal && sequenced.target == null) {
                        _allTargetsSet = _allTargetsTheSame = _allPluginsSet = _allPluginsOfSameType = _allPluginsOfSameTypeAndSubtype = false;
                        _tweenPlugins.Add(null);
                    } else {
                        if (_isGlobal) {
                            DOVisualTweenPlugin plug = DOVisualPluginsManager.GetGlobalTweenPlugin(sequenced.plugId);
                            bool isValidPlugin = TimelineEditorUtils.ValidateSequencedPlugin(sequenced, plug, true);
                            if (!isValidPlugin) {
                                plug = null;
                                _allPluginsSet = _allPluginsOfSameType = _allPluginsOfSameTypeAndSubtype = false;
                            }
                            string plugDataGuid = sequenced.plugDataGuid;
                            int plugDataIndex = sequenced.plugDataIndex;
                            if (i == 0) {
                                defPlugId = sequenced.plugId;
                                defPlugDataGuid = plugDataGuid;
                                defPlugDataIndex = plugDataIndex;
                            } else {
                                if (_allPluginsOfSameType) {
                                    if (sequenced.plugId != defPlugId) _allPluginsOfSameType = _allPluginsOfSameTypeAndSubtype = false;
                                    else if (plugDataGuid != defPlugDataGuid || plugDataIndex != defPlugDataIndex) _allPluginsOfSameTypeAndSubtype = false;
                                }
                            }
                            _tweenPlugins.Add(plug);
                        } else {
                            Object target = sequenced.target;
                            DOVisualTweenPlugin plug = DOVisualPluginsManager.GetTweenPlugin(target);
                            bool isValidPlugin = TimelineEditorUtils.ValidateSequencedPlugin(sequenced, plug, false);
                            if (!isValidPlugin) {
                                plug = null;
                                _allPluginsSet = _allPluginsOfSameType = _allPluginsOfSameTypeAndSubtype = false;
                            }
                            string plugDataGuid = sequenced.plugDataGuid;
                            int plugDataIndex = sequenced.plugDataIndex;
                            if (i == 0) {
                                defTarget = target;
                                defPlug = plug;
                                defPlugDataGuid = plugDataGuid;
                                defPlugDataIndex = plugDataIndex;
                            } else {
                                if (_allTargetsTheSame && target != defTarget) _allTargetsTheSame = false;
                                if (_allPluginsOfSameType) {
                                    if (plug.targetType != defPlug.targetType) _allPluginsOfSameType = _allPluginsOfSameTypeAndSubtype = false;
                                    else if (plugDataGuid != defPlugDataGuid || plugDataIndex != defPlugDataIndex) _allPluginsOfSameTypeAndSubtype = false;
                                }
                            }
                            _tweenPlugins.Add(plug);
                        }
                    }
                }
            }
//            Debug.Log("allTargetsSet: " + LogBool(_allTargetsSet) + ", _allTargetsTheSame: " + LogBool(_allTargetsTheSame)
//                      + ", allPluginsSet: " + LogBool(_allPluginsSet) + ", _allPluginsOfSameType: " + LogBool(_allPluginsOfSameType)
//                      + ", _allPluginsOfSameTypeAndSubtype: " + LogBool(_allPluginsOfSameTypeAndSubtype));
        }

        string LogBool(bool value)
        {
            return value ? "<color=#00ff00>TRUE</color>" : "<color=#ff0000>FALSE</color>";
        }

        public void Refresh() {}

        // Modifies area as it goes on
        public override void Draw(Rect drawArea)
        {
            if (Event.current.type == EventType.Layout) return;

            base.Draw(drawArea);
            if (area.width <= 0) return;

            Init(isUndoRedoPass || isRecorderStoppedPass);
            if (_totSequenceds == 0 || isRecorderStoppedPass) return;
            if (_isAction) {
                _isGlobal = _actionPlugins.Count > 0 && _mainActionPlugin != null && !_mainActionPlugin.GetPlugData(_mainSequenced).wantsTarget;
            }

            // Background
            using (new DeGUI.ColorScope(null, null, _mainColor)) {
                GUI.Box(area, GUIContent.none, DOEGUI.Styles.timeline.sBg);
            }

            Rect scrollViewArea = area;
            if (_scrollView.fullContentArea.height > area.height) area = area.Shift(0, 0, -11, 0);
            _scrollView = DeGUI.BeginScrollView(scrollViewArea, _scrollView);
            using (new DeGUI.LabelFieldWidthScope(_minLabelWidth)) {
                _scrollView.IncreaseContentHeightBy(DrawHeaderAndTweenType());
                if (!_isEvent && !_isInterval && !_isGlobal && (!_isAction || _allPluginsOfSameTypeAndSubtype)) _scrollView.IncreaseContentHeightBy(DrawTarget());
                if (!_isEvent && !_isInterval && (!_allTargetsSet || !_allPluginsSet)) {
                    using (new DeGUI.ColorScope(DOEGUI.Colors.global.red)) {
                        GUI.Label(area.SetHeight(30),
                            _isGlobal || _isAction ? "Unset" : _allTargetsSet ? "Target is not supported" : "Target must be set",
                            DOEGUI.Styles.timeline.sMissingPluginBox
                        );
                    }
                } else if (_isEvent || _isInterval || _allTargetsSet) {
                    _scrollView.IncreaseContentHeightBy(DrawTimings());
                    if (_isTweener) {
                        if (CanHaveEase()) _scrollView.IncreaseContentHeightBy(DrawEase());
                        if (_allPluginsOfSameTypeAndSubtype) {
                            Divider();
                            _scrollView.IncreaseContentHeightBy(DrawToAndFrom());
                            _scrollView.IncreaseContentHeightBy(DrawExtraOptions());
                        }
                        Divider();
                    } else if (_isAction) {
                        Divider();
                        _scrollView.IncreaseContentHeightBy(DrawActionOptions());
                    }
                    if (_isTweener || _isEvent) _scrollView.IncreaseContentHeightBy(DrawEvents());
                }
            }
            _scrollView.IncreaseContentHeightBy(4);
            DeGUI.EndScrollView();

            // Deselect text field if mouseDown outside of it
            if (Event.current.type == EventType.MouseDown && Event.current.button == 0 && GUIUtility.keyboardControl != 0) {
                GUI.FocusControl(null);
                editor.Repaint();
            }
        }

        void Divider()
        {
            DeGUI.DrawColoredSquare(new Rect(area.x + 4, area.y + 4, area.width - 8, 1), _hDividerColor);
            area = area.ShiftYAndResize(9);
            _scrollView.IncreaseContentHeightBy(9);
        }

        float DrawHeaderAndTweenType()
        {
            Rect fullR = area.SetHeight(_lineHeight + 2);
            Rect contentR = fullR.Shift(_Padding, 1, -_Padding - 1, -2);
            Rect typeR = contentR.SetWidth(_minLabelWidth + 1);
            Rect pluginR = contentR.ShiftXAndResize(typeR.width).ShiftYAndResize(1);
            pluginR = pluginR.SetWidth(pluginR.width - 1);

            DeGUI.DrawColoredSquare(fullR, _mainColor);
            if (_isInterval) GUI.Label(typeR, "Interval", DOEGUI.Styles.timeline.sIntervalTitle);
            else {
                GUI.Label(typeR,
                    _mainSequenced.type == DOVisualSequenced.Type.Action
                        ? "Action"
                        : _mainSequenced.type == DOVisualSequenced.Type.Event
                            ? "Event"
                            : "Tween",
                    DOEGUI.Styles.timeline.sTitle
                );
            }
            if (!_isInterval) {
                bool drawSelector = false;
                GUIContent gcPlugin = null;
                float pluginFullWidth = 0;
                if (_isAction) {
                    gcPlugin = _allPluginsOfSameTypeAndSubtype
                        ? _mainActionPlugin.Editor_GetSequencedHeaderLabelGUIContent(_mainSequenced, false)
                        : new GUIContent("–");
                    pluginFullWidth = DOEGUI.Styles.timeline.sBtTargetAndPlugType.CalcSize(gcPlugin).x;
                    drawSelector = true;
                } else if (!_isEvent && (_isGlobal || _allPluginsSet)) {
                    gcPlugin = _allPluginsOfSameTypeAndSubtype
                        ? _mainTweenPlugin.Editor_GetShortTypeAndAnimationNameGUIContent(_mainSequenced)
                        : _allPluginsOfSameType
                            ? new GUIContent(_mainTweenPlugin.Editor_GetShortTypeName(true) + " –")
                            : new GUIContent("–");
                    pluginFullWidth = DOEGUI.Styles.timeline.sBtTargetAndPlugType.CalcSize(gcPlugin).x;
                    drawSelector = true;
                }
                if (drawSelector) {
                    using (new DeGUI.ColorScope(new DeSkinColor(0.1f))) {
                        if (EditorGUI.DropdownButton(pluginR, gcPlugin, FocusType.Passive,
                            pluginFullWidth > pluginR.width
                                ? DOEGUI.Styles.timeline.sBtTargetAndPlugTypeRightAligned
                                : DOEGUI.Styles.timeline.sBtTargetAndPlugType
                        )) {
                            if (_isAction) CM_ActionPlugType();
                            else if (_isGlobal) CM_GlobalPlugType();
                            else if (_allPluginsOfSameType) CM_PlugType();
                        }
                    }
                }
            }

            area = area.ShiftYAndResize(fullR.height);
            return fullR.height;
        }

        float DrawTarget()
        {
            Rect fullR = area.SetHeight(_lineHeight + _Padding * 2);
            Rect contentR = fullR.Contract(_Padding);
            Rect prefixR = contentR.SetWidth(EditorGUIUtility.labelWidth + 2);
            Rect targetR = contentR.ShiftXAndResize(prefixR.width);

            Color bgColor = _allTargetsSet ? _mainColor : Color.red;
            DeGUI.DrawColoredSquare(fullR, bgColor.SetAlpha(0.5f));
            Type forcedTargetType = null;
            if (_isAction) {
                PlugDataAction plugData = _allPluginsOfSameTypeAndSubtype ? _mainActionPlugin.GetPlugData(_mainSequenced) : null;
                if (plugData != null && plugData.targetType != null) forcedTargetType = plugData.targetType;
                GUI.Label(prefixR,
                    plugData != null && plugData.targetLabel != null ? new GUIContent(plugData.targetLabel) : _gcTarget,
                    DOEGUI.Styles.timeline.sTargetPrefixLabel
                );
            } else GUI.Label(prefixR, _gcTarget, DOEGUI.Styles.timeline.sTargetPrefixLabel);
            using (new DeGUI.ColorScope(null, null, _allTargetsSet ? Color.white : bgColor)) {
                using (var check = new EditorGUI.ChangeCheckScope()) {
                    if (forcedTargetType != null) DeGUI.MultiObjectField(targetR, GUIContent.none, "target", _sequenceds, forcedTargetType, true);
                    else {
                        CacheCurrTargets();
                        DeGUI.MultiObjectField(targetR, GUIContent.none, "target", _sequenceds, true);
                    }
                    if (check.changed) {
                        bool isGameObject = _mainSequenced.target != null && _mainSequenced.target is GameObject;
                        if (isGameObject && forcedTargetType == null) {
                            GameObject target = _mainSequenced.target as GameObject;
                            RestoreCurrTargets();
                            TimelineEditorUtils.CM_SelectSequencedTargetFromGameObject(target, AssignTarget);
                        } else {
                            AssignTarget(_mainSequenced.target);
                        }
                    }
                }
            }

            area = area.ShiftYAndResize(fullR.height);
            return fullR.height;
        }
        void AssignTarget(Object target)
        {
            using (new DOScope.UndoableSerialization()) {
                for (int i = 0; i < _totSequenceds; ++i) {
                    DOVisualSequenced sequenced = _sequenceds[i];
                    sequenced.target = target;
                    if (!_isAction) {
                        if (sequenced.target == null) {
                            _tweenPlugins[i] = null;
                            sequenced.plugDataGuid = null;
                            sequenced.plugDataIndex = 0;
                        } else {
                            DOVisualTweenPlugin prevPlugin = _tweenPlugins[i];
                            _tweenPlugins[i] = DOVisualPluginsManager.GetTweenPlugin(sequenced.target);
                            if (_tweenPlugins[i] == null || _tweenPlugins[i] != prevPlugin) {
                                sequenced.plugDataGuid = null;
                                sequenced.plugDataIndex = 0;
                            }
                        }
                    }
                }
            }
            Init(true);
            DOVisualSequenceTimeline.Dispatch_OnSequenceChanged(sequence);
        }

        float DrawTimings()
        {
            int rows = _isTweener ? 2 : 1;
            Rect fullR = area.SetHeight(_lineHeight * rows + _Padding * (rows + 1));
            Rect contentR = fullR.Contract(_Padding);
            Rect timeRowR = contentR.SetHeight(_lineHeight);
            Rect timeR = timeRowR.SetWidth(_minLabelWidth + 64);
            Rect durationR = timeRowR.ShiftXAndResize(timeR.width + 16);
            Rect loopsRowR = timeRowR.Shift(0, _lineHeight + _Padding, 0, 0);
            Rect loopsR = timeR.SetY(loopsRowR.y);
            Rect loopTypeR = loopsRowR.ShiftXAndResize(loopsR.width + _Padding);

            if (_isTweener || _isInterval) { // Time
                DeGUI.MultiFloatField(timeR, _gcStartTime, "startTime", _sequenceds, 0, null);
                using (new DeGUI.LabelFieldWidthScope(GUI.skin.label.CalcSize(_gcDuration).x)) {
                    DeGUI.MultiFloatField(durationR, _gcDuration, "duration", _sequenceds, 0);
                }
            } else { // Duration
                DeGUI.MultiFloatField(timeR, _gcStartTime, "startTime", _sequenceds, 0, null);
            }
            if (_isTweener) { // Loops
                DeGUI.MultiIntField(loopsR, _gcLoops, "loops", _sequenceds, 1, null);
                using (new EditorGUI.DisabledScope(!_mainSequenced.Editor_HasMultipleLoops())) {
                    DeGUI.MultiEnumPopup<LoopType>(loopTypeR, GUIContent.none, "loopType", _sequenceds);
                }
            }

            area = area.ShiftYAndResize(fullR.height);
            return fullR.height;
        }

        float DrawEase()
        {
            Rect fullR = area.SetHeight(_lineHeight + _Padding * 2); // Increased below if necessary
            Rect contentR = fullR.Contract(_Padding);
            Rect easeRowR = contentR;
            Rect easePreviewR = contentR.Shift(0, _lineHeight + _Padding, 0, 0).SetHeight(_lineHeight * 2);
            Rect extraContentR = easePreviewR.Shift(0, easePreviewR.height + _Padding, 0, 0).SetHeight(_lineHeight);

            // Ease + eventual Snapping
            Rect snappingR = easeRowR.ShiftXAndResize(easeRowR.width + _Padding);
            if (_allPluginsOfSameTypeAndSubtype) {
                switch (_mainTweenPlugin.GetPlugData(_mainSequenced).propertyType) {
                case DOVisualSequenced.PropertyType.Float:
                case DOVisualSequenced.PropertyType.Vector2:
                case DOVisualSequenced.PropertyType.Vector3:
                case DOVisualSequenced.PropertyType.Vector4:
                case DOVisualSequenced.PropertyType.Rect:
                    snappingR = easeRowR.ShiftXAndResize(easeRowR.width - 64).Shift(0, 1, 0, 0);
                    DeGUI.MultiToggleButton(snappingR, _gcSnapping, "boolOption0", _sequenceds, DOEGUI.Styles.timeline.sToggle);
                    break;
                }
            }
            Rect easeR = easeRowR.Shift(0, 0, -snappingR.width - _Padding, 0);
            if (GUI.Button(easeR, GUIContent.none, GUIStyle.none)) {
                _easeSelectionWin.Prepare(_sequenceds);
                PopupWindow.Show(easeR, _easeSelectionWin);
            }
            bool mixedEases = DOEGUI.MultiFilteredEasePopup(easeR, _gcEase, "ease", _sequenceds);
            if (!mixedEases) {
                // Ease preview
                EaseSnapshot currEaseSnapshot = new EaseSnapshot(_mainSequenced.ease, _mainSequenced.overshootOrAmplitude, _mainSequenced.period);
                if (currEaseSnapshot.ease == Ease.INTERNAL_Custom) {
                    extraContentR = extraContentR.ShiftY(-easePreviewR.height - _Padding);
                } else {
                    fullR = fullR.Shift(0, 0, 0, easePreviewR.height + _Padding);
                    easePreviewR = easePreviewR.ShiftXAndResize(EditorGUIUtility.labelWidth + 2);
                    if (GUI.Button(easePreviewR, GUIContent.none, GUIStyle.none)) {
                        _easeSelectionWin.Prepare(_sequenceds);
                        PopupWindow.Show(easeR, _easeSelectionWin);
                    }
                    if (currEaseSnapshot != _lastPreviewedEase || _easePreviewTex == null) {
                        _lastPreviewedEase = currEaseSnapshot;
                        if (_easePreviewTex == null) _easePreviewTex = new Texture2D((int)easePreviewR.width, (int)easePreviewR.height);
                        TimelineEditorUtils.GenerateEaseTextureIn(
                            _easePreviewTex, currEaseSnapshot.ease, currEaseSnapshot.overshootOrAmplitude, currEaseSnapshot.period
                        );
                    }
                    GUI.DrawTexture(easePreviewR, _easePreviewTex);
                }
                // Extra ease options
                Rect overR;
                bool isBack = false, isElastic = false, isFlash = false, isCustom = false;
                switch (_mainSequenced.ease) {
                case Ease.InBack: case Ease.OutBack: case Ease.InOutBack:
                    isBack = true;
                    break;
                case Ease.InElastic: case Ease.OutElastic: case Ease.InOutElastic:
                    isElastic = true;
                    break;
                case Ease.Flash: case Ease.InFlash: case Ease.OutFlash: case Ease.InOutFlash:
                    isFlash = true;
                    break;
                case Ease.INTERNAL_Custom:
                    isCustom = true;
                    break;
                }
                if (isBack) {
                    fullR = fullR.Shift(0, 0, 0, _lineHeight + _Padding);
                    extraContentR = extraContentR.ShiftXAndResize(EditorGUIUtility.labelWidth + 2);
                    overR = extraContentR.SetWidth(_minLabelWidth + 64);
                    using (new DeGUI.LabelFieldWidthScope(EditorStyles.textField.CalcSize(_gcOvershoot).x)) {
                        DeGUI.MultiFloatField(overR, _gcOvershoot, "overshootOrAmplitude", _sequenceds, 0);
                    }
                } else if (isElastic || isFlash) {
                    fullR = fullR.Shift(0, 0, 0, _lineHeight + _Padding);
                    extraContentR = extraContentR.ShiftXAndResize(EditorGUIUtility.labelWidth + 2);
                    overR = extraContentR.SetWidth(_minLabelWidth + 40);
                    Rect periodR = extraContentR.ShiftXAndResize(overR.width + 16);
                    using (new DeGUI.LabelFieldWidthScope(EditorStyles.textField.CalcSize(_gcAmplitude).x)) {
                        if (isFlash) DeGUI.MultiIntField(overR, _gcAmplitude, "overshootOrAmplitude", _sequenceds, 1);
                        else DeGUI.MultiFloatField(overR, _gcAmplitude, "overshootOrAmplitude", _sequenceds, 1, 10);
                    }
                    using (new DeGUI.LabelFieldWidthScope(EditorStyles.textField.CalcSize(_gcPeriod).x)) {
                        if (isFlash) DeGUI.MultiIntField(periodR, _gcPeriod, "period", _sequenceds, -1, 1);
                        else DeGUI.MultiFloatField(periodR, _gcPeriod, "period", _sequenceds, 0.05f, 1f);
                    }
                } else if (isCustom) {
                    fullR = fullR.Shift(0, 0, 0, _lineHeight + _Padding);
                    DeGUI.MultiCurveField(extraContentR.ShiftXAndResize(_minLabelWidth + 2), GUIContent.none, "easeCurve", _sequenceds);
                }
            }

            area = area.ShiftYAndResize(fullR.height);
            return fullR.height;
        }

        float DrawToAndFrom()
        {
            if (!_allPluginsOfSameTypeAndSubtype) return 0;

            Rect fullR = area.SetHeight(_lineHeight + _Padding * 2); // Increased below if necessary
            Rect contentR = fullR.Contract(_Padding);
            Rect innerContentR = contentR;

            PluginTweenType tweenType = _mainTweenPlugin.GetPlugData(_mainSequenced).tweenType;
            bool canHaveFrom = CanHaveFrom(_mainSequenced, _mainTweenPlugin);
            // From
            if (canHaveFrom) {
                // Flip options
                Rect prefixR = innerContentR.SetWidth(_minLabelWidth);
                Rect flipR = prefixR.ShiftXAndResize(prefixR.width - DOEGUI.Styles.timeline.sBtFlip.fixedWidth + 2);
                innerContentR = innerContentR.ShiftXAndResize(flipR.xMax - innerContentR.x + 2);
                GUI.Label(prefixR, "From");
                using (new DeGUI.ColorScope(new DeSkinColor(0.2f, 1f))) {
                    if (GUI.Button(flipR, DeStylePalette.ico_flipV, DOEGUI.Styles.timeline.sBtFlip)) {
                        for (int i = 0; i < _sequenceds.Count; ++i) {
                            SwitchToFrom(_sequenceds[i], _tweenPlugins[i]);
                        }
                        GUI.changed = true;
                    }
                }
                if (GUI.Button(prefixR, GUIContent.none, GUIStyle.none)) {
                    if (Event.current.button == 1) CM_CopyValuesFromHierarchyTarget(true);
                }
                //
                innerContentR = OptionalAlphaToggle(innerContentR);
                innerContentR = OptionalAxisConstraint(innerContentR, _mainSequenced.fromType);
                innerContentR = OptionalRelativeToggle(innerContentR, _mainSequenced.fromType);
                using (var check = new EditorGUI.ChangeCheckScope()) {
                    DeGUI.MultiEnumPopup<DOVisualSequenced.ToFromType>(innerContentR, GUIContent.none, "fromType", _sequenceds);
                    if (check.changed && _mainSequenced.fromType == DOVisualSequenced.ToFromType.Dynamic) {
                        for (int i = 0; i < _sequenceds.Count; ++i) _sequenceds[i].toType = DOVisualSequenced.ToFromType.Direct;
                    }
                }
                if (_mainSequenced.fromType == DOVisualSequenced.ToFromType.Direct) {
                    contentR = innerContentR = contentR.Shift(0, contentR.height + _Padding, 0, 0);
                    innerContentR = innerContentR.ShiftXAndResize(_minLabelWidth + 2);
                    innerContentR = ToFromPropertyValue(innerContentR, false);
                    contentR = contentR.SetHeight(innerContentR.height);
                    fullR = fullR.Shift(0, 0, 0, contentR.height + _Padding);
                }
            }
            // To
            if (canHaveFrom) {
                contentR = innerContentR = contentR.Shift(0, contentR.height + _Padding, 0, 0).SetHeight(_lineHeight);
                fullR = fullR.Shift(0, 0, 0, contentR.height + _Padding);
                Rect prefixR = innerContentR.SetWidth(_minLabelWidth);
                innerContentR = innerContentR.ShiftXAndResize(prefixR.width + 2);
                innerContentR = OptionalAlphaToggle(innerContentR);
                innerContentR = OptionalAxisConstraint(innerContentR, _mainSequenced.toType);
                innerContentR = OptionalRelativeToggle(innerContentR, _mainSequenced.toType);
                GUI.Label(prefixR, "To");
                if (GUI.Button(prefixR, GUIContent.none, GUIStyle.none)) {
                    if (Event.current.button == 1) CM_CopyValuesFromHierarchyTarget(false);
                }
                using (var check = new EditorGUI.ChangeCheckScope()) {
                    DeGUI.MultiEnumPopup<DOVisualSequenced.ToFromType>(innerContentR, GUIContent.none, "toType", _sequenceds);
                    if (check.changed && _mainSequenced.toType == DOVisualSequenced.ToFromType.Dynamic) {
                        for (int i = 0; i < _sequenceds.Count; ++i) _sequenceds[i].fromType = DOVisualSequenced.ToFromType.Direct;
                    }
                }
            }
            if (_mainSequenced.toType == DOVisualSequenced.ToFromType.Direct) {
                switch (tweenType) {
                case PluginTweenType.Punch:
                case PluginTweenType.Shake:
                    innerContentR = contentR;
                    Rect prefixR = innerContentR.SetWidth(_minLabelWidth + 2);
                    GUI.Label(prefixR, "Strn", DOEGUI.Styles.timeline.sPrefixLabel);
                    break;
                default:
                    contentR = innerContentR = contentR.Shift(0, contentR.height + _Padding, 0, 0);
                    break;
                }
                innerContentR = innerContentR.ShiftXAndResize(_minLabelWidth + 2);
                innerContentR = ToFromPropertyValue(innerContentR, true);
                contentR = contentR.SetHeight(innerContentR.height);
                if (canHaveFrom) fullR = fullR.Shift(0, 0, 0, contentR.height + _Padding);
            }

            area = area.ShiftYAndResize(fullR.height);
            return fullR.height;
        }

        float DrawActionOptions()
        {
            Rect fullR = area.SetHeight(0); // Increased below if necessary
            Rect contentR = new Rect(area.x + _Padding, area.y - _lineHeight, area.width - _Padding * 2, _lineHeight);
            Rect innerContentR = contentR;
            Rect prefixR = innerContentR.SetWidth(_minLabelWidth);
            PlugDataAction plugData = _mainActionPlugin.GetPlugData(_mainSequenced);
            using (new DeGUI.LabelFieldWidthScope(EditorGUIUtility.labelWidth + 20)) {
                if (!string.IsNullOrEmpty(plugData.boolOptionLabel)) {
                    ShiftExtraOptionsRectsBy(ref fullR, ref contentR, ref innerContentR, ref prefixR, _lineHeight + _Padding);
                    DeGUI.MultiToggleButton(contentR, new GUIContent(plugData.boolOptionLabel), "boolOption0", _sequenceds);
                }
                if (!string.IsNullOrEmpty(plugData.stringOptionLabel)) {
                    ShiftExtraOptionsRectsBy(ref fullR, ref contentR, ref innerContentR, ref prefixR, _lineHeight + _Padding);
                    DeGUI.MultiTextField(contentR, new GUIContent(plugData.stringOptionLabel), "toStringVal", _sequenceds);
                }
                if (!string.IsNullOrEmpty(plugData.intOptionLabel)) {
                    ShiftExtraOptionsRectsBy(ref fullR, ref contentR, ref innerContentR, ref prefixR, _lineHeight + _Padding);
                    if (plugData.intOptionAsEnumType == null) {
                        contentR = contentR.SetWidth(EditorGUIUtility.labelWidth + 64);
                        DeGUI.MultiIntField(contentR, new GUIContent(plugData.intOptionLabel), "toIntVal", _sequenceds);
                    } else { // int as enum popup
                        DeGUI.MultiEnumPopup(contentR,
                            new GUIContent(plugData.intOptionLabel), plugData.intOptionAsEnumType, "toIntVal", _sequenceds
                        );
                    }
                }
                if (!string.IsNullOrEmpty(plugData.float0OptionLabel)) {
                    ShiftExtraOptionsRectsBy(ref fullR, ref contentR, ref innerContentR, ref prefixR, _lineHeight + _Padding);
                    contentR = contentR.SetWidth(EditorGUIUtility.labelWidth + 64);
                    DeGUI.MultiFloatField(contentR, new GUIContent(plugData.float0OptionLabel), "toFloatVal", _sequenceds);
                }
                if (!string.IsNullOrEmpty(plugData.float1OptionLabel)) {
                    ShiftExtraOptionsRectsBy(ref fullR, ref contentR, ref innerContentR, ref prefixR, _lineHeight + _Padding);
                    contentR = contentR.SetWidth(EditorGUIUtility.labelWidth + 64);
                    DeGUI.MultiFloatField(contentR, new GUIContent(plugData.float1OptionLabel), "fromFloatVal", _sequenceds);
                }
                if (!string.IsNullOrEmpty(plugData.float2OptionLabel)) {
                    ShiftExtraOptionsRectsBy(ref fullR, ref contentR, ref innerContentR, ref prefixR, _lineHeight + _Padding);
                    contentR = contentR.SetWidth(EditorGUIUtility.labelWidth + 64);
                    DeGUI.MultiFloatField(contentR, new GUIContent(plugData.float2OptionLabel), "floatOption0", _sequenceds);
                }
                if (!string.IsNullOrEmpty(plugData.float3OptionLabel)) {
                    ShiftExtraOptionsRectsBy(ref fullR, ref contentR, ref innerContentR, ref prefixR, _lineHeight + _Padding);
                    contentR = contentR.SetWidth(EditorGUIUtility.labelWidth + 64);
                    DeGUI.MultiFloatField(contentR, new GUIContent(plugData.float3OptionLabel), "floatOption1", _sequenceds);
                }
            }

            area = area.ShiftYAndResize(fullR.height);
            return fullR.height;
        }

        float DrawExtraOptions()
        {
            Rect fullR = area.SetHeight(0); // Increased below if necessary
            Rect contentR = new Rect(area.x + _Padding, area.y - _lineHeight, area.width - _Padding * 2, _lineHeight);
            Rect innerContentR = contentR;
            Rect prefixR = innerContentR.SetWidth(_minLabelWidth);
            Rect r0, r1;

            ITweenPluginData plugData = _mainTweenPlugin.GetPlugData(_mainSequenced);
            switch (plugData.propertyType) {
            case DOVisualSequenced.PropertyType.String:
                ShiftExtraOptionsRectsBy(ref fullR, ref contentR, ref innerContentR, ref prefixR, _lineHeight + _Padding);
                r0 = innerContentR.SetWidth(innerContentR.width * 0.5f - _Padding * 0.5f).Shift(0, 1, 0, 0);
                r1 = r0.Shift(r0.width + _Padding, 0, 0, 0);
                GUI.Label(prefixR, "Options", DOEGUI.Styles.timeline.sPrefixLabel);
                DeGUI.MultiToggleButton(r0, _gcRichTextEnabled, "boolOption0", _sequenceds, DOEGUI.Styles.timeline.sToggle);
                using (var check = new EditorGUI.ChangeCheckScope()) {
                    DeGUI.MultiToggleButton(r1, _gcScramble, "intOption0", _sequenceds, DOEGUI.Styles.timeline.sToggle);
                    if (check.changed) {
                        for (int i = 0; i < _sequenceds.Count; ++i) {
                            _sequenceds[i].intOption1 = _sequenceds[i].intOption0;
                            _sequenceds[i].stringOption0 = null;
                        }
                    }
                }
                if (_mainSequenced.intOption0 == 1) {
                    ShiftExtraOptionsRectsBy(ref fullR, ref contentR, ref innerContentR, ref prefixR, _lineHeight + _Padding);
                    GUI.Label(innerContentR, _gcScrambleOptions);
                    ShiftExtraOptionsRectsBy(ref fullR, ref contentR, ref innerContentR, ref prefixR, _lineHeight + _Padding);
                    using (var check = new EditorGUI.ChangeCheckScope()) {
                        DeGUI.MultiEnumPopup<ScrambleMode>(innerContentR, GUIContent.none, "intOption1", _sequenceds);
                        if (check.changed) {
                            ScrambleMode scrambleMode = (ScrambleMode)_mainSequenced.intOption1;
                            if (scrambleMode == ScrambleMode.None) {
                                for (int i = 0; i < _sequenceds.Count; ++i) _sequenceds[i].intOption0 = 0;
                            } else if (scrambleMode != ScrambleMode.Custom) {
                                for (int i = 0; i < _sequenceds.Count; ++i) _sequenceds[i].stringOption0 = null;
                            }
                        }
                    }
                    if ((ScrambleMode)_mainSequenced.intOption1 == ScrambleMode.Custom) {
                        ShiftExtraOptionsRectsBy(ref fullR, ref contentR, ref innerContentR, ref prefixR, _lineHeight + _Padding);
                        DeGUI.MultiTextField(innerContentR, GUIContent.none, "stringOption0", _sequenceds);
                    }
                }
                break;
            case DOVisualSequenced.PropertyType.Vector3:
                switch (plugData.tweenType) {
                case PluginTweenType.Punch:
                    ShiftExtraOptionsRectsBy(ref fullR, ref contentR, ref innerContentR, ref prefixR, _lineHeight + _Padding);
                    r0 = prefixR.SetWidth(prefixR.width + 64);
                    r1 = new Rect(r0.xMax + 16, r0.y, 0, r0.height);
                    r1.width = innerContentR.xMax - r1.x;
                    float elasticitySize = GUI.skin.label.CalcSize(_gcElasticity).x + 6;
                    DeGUI.MultiIntField(r0, _gcVibrato, "intOption1", _sequenceds, 0, null);
                    using (new DeGUI.LabelFieldWidthScope(elasticitySize)) {
                        DeGUI.MultiFloatField(r1, _gcElasticity, "floatOption0", _sequenceds, 0, 1);
                    }
                    break;
                case PluginTweenType.Shake:
                    ShiftExtraOptionsRectsBy(ref fullR, ref contentR, ref innerContentR, ref prefixR, _lineHeight + _Padding);
                    r0 = prefixR.SetWidth(prefixR.width + 64);
                    r1 = new Rect(r0.xMax + 16, r0.y, 0, r0.height);
                    r1.width = innerContentR.xMax - r1.x;
                    float randomnessSize = GUI.skin.label.CalcSize(_gcRandomness).x;
                    DeGUI.MultiIntField(r0, _gcVibrato, "intOption1", _sequenceds, 0, null);
                    using (new DeGUI.LabelFieldWidthScope(randomnessSize)) {
                        DeGUI.MultiFloatField(r1, _gcRandomness, "floatOption0", _sequenceds, 0, 180);
                    }
                    ShiftExtraOptionsRectsBy(ref fullR, ref contentR, ref innerContentR, ref prefixR, _lineHeight + _Padding);
                    innerContentR = innerContentR.Shift(0, 1, 0, 0);
                    DeGUI.MultiToggleButton(innerContentR, _gcFadeOut, "intOption0", _sequenceds, DOEGUI.Styles.timeline.sToggle);
                    break;
                }
                break;
            case DOVisualSequenced.PropertyType.Quaternion:
                ShiftExtraOptionsRectsBy(ref fullR, ref contentR, ref innerContentR, ref prefixR, _lineHeight + _Padding);
                GUI.Label(prefixR, "Mode", DOEGUI.Styles.timeline.sPrefixLabel);
                DeGUI.MultiEnumPopup<RotateMode>(innerContentR, GUIContent.none, "intOption0", _sequenceds);
                break;
            }
            switch (plugData.tweenType) {
            case PluginTweenType.StringOption:
                ShiftExtraOptionsRectsBy(ref fullR, ref contentR, ref innerContentR, ref prefixR, _lineHeight + _Padding);
                DeGUI.MultiTextField(contentR, new GUIContent(plugData.stringOptionLabel), "stringOption0", _sequenceds);
                break;
            case PluginTweenType.IntOption:
                ShiftExtraOptionsRectsBy(ref fullR, ref contentR, ref innerContentR, ref prefixR, _lineHeight + _Padding);
                contentR = contentR.SetWidth(_minLabelWidth + 64);
                DeGUI.MultiIntField(contentR, new GUIContent(plugData.intOptionLabel), "intOption1", _sequenceds);
                break;
            }

            area = area.ShiftYAndResize(fullR.height);
            return fullR.height;
        }

        void ShiftExtraOptionsRectsBy(ref Rect fullR, ref Rect contentR, ref Rect innerContentR, ref Rect prefixR, float y)
        {
            if (fullR.height < 0) fullR = fullR.SetHeight(_lineHeight + _Padding * 2);
            fullR = fullR.Shift(0, 0, 0, y);
            contentR = innerContentR = contentR.Shift(0, y, 0, 0);
            innerContentR = innerContentR.ShiftXAndResize(prefixR.width + 2);
            prefixR = contentR.SetWidth(_minLabelWidth);
        }

        float DrawEvents()
        {
            Rect fullR = area.SetHeight(_lineHeight + _Padding * 2); // Increased below if necessary
            Rect contentR = fullR.Contract(_Padding);
            bool hasOnCompletes = _forceShowOnCompletes || _mainSequenced.onComplete.GetPersistentEventCount() > 0;
            bool hasOnStepCompletes = _forceShowOnStepCompletes || _mainSequenced.onStepComplete.GetPersistentEventCount() > 0;
            bool hasOnUpdates = _forceShowOnUpdates || _mainSequenced.onUpdate.GetPersistentEventCount() > 0;
            using (new EditorGUI.DisabledScope(_sequenceds.Count > 1)) {
                if (_isEvent) {
                    fullR = fullR.Shift(0, 0, 0, -_lineHeight + _Padding);
                    contentR = contentR.SetHeight(-_Padding);
                } else {
                    // Toggles
                    Rect btOnCompletesR = contentR.SetWidth(contentR.width * 0.3f - _Padding);
                    Rect btOnStepCompletesR = btOnCompletesR.Shift(btOnCompletesR.width + _Padding, 0, 0, 0).SetWidth(contentR.width * 0.4f);
                    Rect btOnUpdatesR = btOnStepCompletesR.Shift(btOnStepCompletesR.width + _Padding, 0, 0, 0).SetWidth(btOnCompletesR.width);
                    using (var check = new EditorGUI.ChangeCheckScope()) {
                        DeGUI.MultiToggleButton(btOnCompletesR,
                            hasOnCompletes,
                            new GUIContent("OnComplete"), "onComplete", _sequenceds, null, null, null, null, DOEGUI.Styles.global.toggleSmlLabel
                        );
                        if (check.changed) {
                            if (hasOnCompletes) {
                                // Remove
                                _forceShowOnCompletes = false;
                                for (int i = 0; i < _sequenceds.Count; ++i) _sequenceds[i].onComplete = new UnityEvent();
                            } else {
                                // Show
                                _forceShowOnCompletes = true;
                            }
                            Init(true);
                        }
                    }
                    using (var check = new EditorGUI.ChangeCheckScope()) {
                        DeGUI.MultiToggleButton(btOnStepCompletesR,
                            hasOnStepCompletes,
                            new GUIContent("OnStepComplete"), "onStepComplete", _sequenceds, null, null, null, null, DOEGUI.Styles.global.toggleSmlLabel
                        );
                        if (check.changed) {
                            if (hasOnStepCompletes) {
                                // Remove
                                _forceShowOnStepCompletes = false;
                                for (int i = 0; i < _sequenceds.Count; ++i) _sequenceds[i].onStepComplete = new UnityEvent();
                            } else {
                                // Show
                                _forceShowOnStepCompletes = true;
                            }
                            Init(true);
                        }
                    }
                    using (var check = new EditorGUI.ChangeCheckScope()) {
                        DeGUI.MultiToggleButton(btOnUpdatesR,
                            hasOnUpdates,
                            new GUIContent("OnUpdate"), "onUpdate", _sequenceds, null, null, null, null, DOEGUI.Styles.global.toggleSmlLabel
                        );
                        if (check.changed) {
                            if (hasOnUpdates) {
                                // Remove
                                _forceShowOnUpdates = false;
                                for (int i = 0; i < _sequenceds.Count; ++i) _sequenceds[i].onUpdate = new UnityEvent();
                            } else {
                                // Show
                                _forceShowOnUpdates = true;
                            }
                            Init(true);
                        }
                    }
                }
                // Events
                if (_sequenceds.Count == 1) {
                    if (_isEvent || hasOnCompletes) {
                        float h = _mainSpOnComplete.GetUnityEventHeight();
                        fullR = fullR.Shift(0, 0, 0, h + _Padding);
                        contentR = contentR.Shift(0, contentR.height + _Padding, 0, 0).SetHeight(h);
                        DeGUI.MultiUnityEvent(contentR, _isEvent ? _gcEvent : GUIContent.none, "onComplete", _sequenceds, _spOnCompletes);
                    }
                    if (!_isEvent && hasOnStepCompletes) {
                        float h = _mainSpOnStepComplete.GetUnityEventHeight();
                        fullR = fullR.Shift(0, 0, 0, h + _Padding);
                        contentR = contentR.Shift(0, contentR.height + _Padding, 0, 0).SetHeight(h);
                        DeGUI.MultiUnityEvent(contentR, GUIContent.none, "onStepComplete", _sequenceds, _spOnStepCompletes);
                    }
                    if (!_isEvent && hasOnUpdates) {
                        float h = _mainSpOnUpdates.GetUnityEventHeight();
                        fullR = fullR.Shift(0, 0, 0, h + _Padding);
                        contentR = contentR.Shift(0, contentR.height + _Padding, 0, 0).SetHeight(h);
                        DeGUI.MultiUnityEvent(contentR, GUIContent.none, "onUpdate", _sequenceds, _spOnUpdates);
                    }
                }
            }
            return fullR.height;
        }

        Rect OptionalAxisConstraint(Rect contentR, DOVisualSequenced.ToFromType toFromType)
        {
            if (toFromType != DOVisualSequenced.ToFromType.Direct) return contentR;
            ITweenPluginData plugData = _mainTweenPlugin.GetPlugData(_mainSequenced);
            if (plugData.tweenType != PluginTweenType.SelfDetermined) return contentR;
            if (plugData.propertyType == DOVisualSequenced.PropertyType.Quaternion) return contentR;
            const int width = 20;
            bool hasAxes = false, hasZ = false, hasW = false;
            switch (plugData.propertyType) {
            case DOVisualSequenced.PropertyType.Vector2:
                hasAxes = true;
                break;
            case DOVisualSequenced.PropertyType.Vector3:
                hasAxes = hasZ = true;
                break;
            case DOVisualSequenced.PropertyType.Vector4:
                hasAxes = hasZ = hasW = true;
                break;
            }
            if (!hasAxes) return contentR;
            Rect axisR = new Rect(contentR.xMax, contentR.y + 1, width, contentR.height);
            if (hasW) {
                using (var check = new EditorGUI.ChangeCheckScope()) {
                    axisR = axisR.Shift(-width, 0, 0, 0);
                    DeGUI.MultiToggleButton(axisR,
                        _mainSequenced.axisConstraint == AxisConstraint.W || _mainSequenced.axisConstraint == AxisConstraint.None,
                        new GUIContent("W"), "axisConstraint", _sequenceds, null, null, null, null, DOEGUI.Styles.timeline.sAxisToggle
                    );
                    if (check.changed) {
                        for (int i = 0; i < _sequenceds.Count; ++i) {
                            _sequenceds[i].axisConstraint = _sequenceds[i].axisConstraint != AxisConstraint.W ? AxisConstraint.W : AxisConstraint.None;
                        }
                    }
                }
            }
            if (hasZ) {
                using (var check = new EditorGUI.ChangeCheckScope()) {
                    axisR = axisR.Shift(-width, 0, 0, 0);
                    DeGUI.MultiToggleButton(axisR,
                        _mainSequenced.axisConstraint == AxisConstraint.Z || _mainSequenced.axisConstraint == AxisConstraint.None,
                        new GUIContent("Z"), "axisConstraint", _sequenceds, null, null, null, null, DOEGUI.Styles.timeline.sAxisToggle
                    );
                    if (check.changed) {
                        for (int i = 0; i < _sequenceds.Count; ++i) {
                            _sequenceds[i].axisConstraint = _sequenceds[i].axisConstraint != AxisConstraint.Z ? AxisConstraint.Z : AxisConstraint.None;
                        }
                    }
                }
            }
            using (var check = new EditorGUI.ChangeCheckScope()) {
                axisR = axisR.Shift(-width, 0, 0, 0);
                DeGUI.MultiToggleButton(axisR,
                    _mainSequenced.axisConstraint == AxisConstraint.Y || _mainSequenced.axisConstraint == AxisConstraint.None,
                    new GUIContent("Y"), "axisConstraint", _sequenceds, null, null, null, null, DOEGUI.Styles.timeline.sAxisToggle
                );
                if (check.changed) {
                    for (int i = 0; i < _sequenceds.Count; ++i) {
                        _sequenceds[i].axisConstraint = _sequenceds[i].axisConstraint != AxisConstraint.Y ? AxisConstraint.Y : AxisConstraint.None;
                    }
                }
            }
            using (var check = new EditorGUI.ChangeCheckScope()) {
                axisR = axisR.Shift(-width, 0, 0, 0);
                DeGUI.MultiToggleButton(axisR,
                    _mainSequenced.axisConstraint == AxisConstraint.X || _mainSequenced.axisConstraint == AxisConstraint.None,
                    new GUIContent("X"), "axisConstraint", _sequenceds, null, null, null, null, DOEGUI.Styles.timeline.sAxisToggle
                );
                if (check.changed) {
                    for (int i = 0; i < _sequenceds.Count; ++i) {
                        _sequenceds[i].axisConstraint = _sequenceds[i].axisConstraint != AxisConstraint.X ? AxisConstraint.X : AxisConstraint.None;
                    }
                }
            }
            return contentR.SetWidth(axisR.x - contentR.x - _Padding);
        }

        Rect OptionalRelativeToggle(Rect contentR, DOVisualSequenced.ToFromType toFromType)
        {
            if (toFromType != DOVisualSequenced.ToFromType.Direct) return contentR;
            if (_mainTweenPlugin.GetPlugData(_mainSequenced).tweenType != PluginTweenType.SelfDetermined) return contentR;
            Rect r = new Rect(contentR.xMax - 64, contentR.y + 1, 64, contentR.height);
            DeGUI.MultiToggleButton(r, _mainSequenced.isRelative ? _gcIsRelative : _gcIsNotRelative, "isRelative", _sequenceds,
                DOEGUI.Colors.bg.toggleOn, _isRelativeColor, DOEGUI.Colors.content.toggleOn, Color.white,
                DOEGUI.Styles.timeline.sToggle
            );
            return contentR.SetWidth(r.x - contentR.x - _Padding);
        }

        Rect OptionalAlphaToggle(Rect contentR)
        {
            switch (_mainTweenPlugin.GetPlugData(_mainSequenced).propertyType) {
            case DOVisualSequenced.PropertyType.Color:
                Rect r = new Rect(contentR.xMax - 64, contentR.y + 1, 64, contentR.height);
                contentR = contentR.SetWidth(contentR.width - r.width - _Padding);
                DeGUI.MultiToggleButton(r, _gcAlphaOnly, "boolOption0", _sequenceds, DOEGUI.Styles.timeline.sToggle);
                break;
            }
            return contentR;
        }

        Rect ToFromPropertyValue(Rect contentR, bool isTo)
        {
            // Editing fields
            switch (_mainTweenPlugin.GetPlugData(_mainSequenced).propertyType) {
            case DOVisualSequenced.PropertyType.Float:
                using (new DeGUI.LabelFieldWidthScope(12)) {
                    DeGUI.MultiFloatField(contentR.SetWidth(132), _gcNumber, isTo ? "toFloatVal" : "fromFloatVal", _sequenceds);
                }
                break;
            case DOVisualSequenced.PropertyType.Int:
                using (new DeGUI.LabelFieldWidthScope(12)) {
                    DeGUI.MultiIntField(contentR.SetWidth(132), _gcNumber, isTo ? "toIntVal" : "fromIntVal", _sequenceds);
                }
                break;
            case DOVisualSequenced.PropertyType.Uint:
                using (new DeGUI.LabelFieldWidthScope(12)) {
                    DeGUI.MultiIntField(contentR.SetWidth(132), _gcNumber, isTo ? "toUintVal" : "fromUintVal", _sequenceds);
                }
                break;
            case DOVisualSequenced.PropertyType.String:
                contentR = contentR.SetHeight(Mathf.Max(contentR.height,
                    EditorStyles.textArea.CalcHeight(new GUIContent(isTo ? _mainSequenced.toStringVal : _mainSequenced.fromStringVal), contentR.width))
                );
                DeGUI.MultiTextArea(contentR, isTo ? "toStringVal" : "fromStringVal", _sequenceds);
                break;
            case DOVisualSequenced.PropertyType.Vector2:
                RefreshVectorLockedToAndDisabledAxes();
                VectorLock(contentR, true);
                DeGUI.MultiVector2FieldAdvanced(contentR, GUIContent.none, isTo ? "toVector2Val" : "fromVector2Val", _sequenceds,
                    (_disabledAxes & AxisConstraint.X) == 0, (_disabledAxes & AxisConstraint.Y) == 0,
                    (_lockAllAxesTo & AxisConstraint.X) != 0, (_lockAllAxesTo & AxisConstraint.Y) != 0
                );
                break;
            case DOVisualSequenced.PropertyType.Vector3:
                RefreshVectorLockedToAndDisabledAxes();
                VectorLock(contentR, false, true);
                DeGUI.MultiVector3FieldAdvanced(contentR, GUIContent.none, isTo ? "toVector3Val" : "fromVector3Val", _sequenceds,
                    (_disabledAxes & AxisConstraint.X) == 0, (_disabledAxes & AxisConstraint.Y) == 0, (_disabledAxes & AxisConstraint.Z) == 0,
                    (_lockAllAxesTo & AxisConstraint.X) != 0, (_lockAllAxesTo & AxisConstraint.Y) != 0, (_lockAllAxesTo & AxisConstraint.Z) != 0
                );
                break;
            case DOVisualSequenced.PropertyType.Vector4:
                RefreshVectorLockedToAndDisabledAxes();
                VectorLock(contentR, false, false, true);
                DeGUI.MultiVector4FieldAdvanced(contentR, GUIContent.none, isTo ? "toVector4Val" : "fromVector4Val", _sequenceds,
                    (_disabledAxes & AxisConstraint.X) == 0, (_disabledAxes & AxisConstraint.Y) == 0,
                    (_disabledAxes & AxisConstraint.Z) == 0, (_disabledAxes & AxisConstraint.W) == 0,
                    (_lockAllAxesTo & AxisConstraint.X) != 0, (_lockAllAxesTo & AxisConstraint.Y) != 0,
                    (_lockAllAxesTo & AxisConstraint.Z) != 0, (_lockAllAxesTo & AxisConstraint.W) != 0
                );
                break;
            case DOVisualSequenced.PropertyType.Quaternion:
                DeGUI.MultiVector3Field(contentR, GUIContent.none, isTo ? "toVector3Val" : "fromVector3Val", _sequenceds);
                break;
            case DOVisualSequenced.PropertyType.Color:
                DeGUI.MultiColorFieldAdvanced(contentR, GUIContent.none, isTo ? "toColorVal" : "fromColorVal", _sequenceds, _mainSequenced.boolOption0);
                break;
            case DOVisualSequenced.PropertyType.Rect:
                contentR = contentR.SetHeight(36);
                DeGUI.MultiRectField(contentR, GUIContent.none, isTo ? "toRectVal" : "fromRectVal", _sequenceds);
                break;
            }
            return contentR;
        }

        void VectorLock(Rect contentR, bool isVector2, bool isVector3 = false, bool isVector4 = false)
        {
            using (var check = new EditorGUI.ChangeCheckScope()) {
                Rect r = contentR.SetX(contentR.x - 20).SetWidth(20);
                _mainSequenced.editor_lockVector = DeGUI.ToggleButton(r,
                    _mainSequenced.editor_lockVector, _mainSequenced.editor_lockVector ? _gcVectorLocked : _gcVectorUnlocked,
                    DOEGUI.Styles.timeline.sLockToggle
                );
                if (check.changed && _mainSequenced.editor_lockVector) {
                    RefreshVectorLockedToAndDisabledAxes();
                    float value;
                    if (isVector2) {
                        for (int i = 0; i < _sequenceds.Count; ++i) {
                            value = (_lockAllAxesTo & AxisConstraint.X) != 0 ? _sequenceds[i].toVector2Val.x : _sequenceds[i].toVector2Val.y;
                            _sequenceds[i].toVector2Val = new Vector2(value, value);
                            value = (_lockAllAxesTo & AxisConstraint.X) != 0 ? _sequenceds[i].fromVector2Val.x : _sequenceds[i].fromVector2Val.y;
                            _sequenceds[i].fromVector2Val = new Vector2(value, value);
                        }
                    } else if (isVector3) {
                        for (int i = 0; i < _sequenceds.Count; ++i) {
                            value = (_lockAllAxesTo & AxisConstraint.X) != 0 ? _sequenceds[i].toVector3Val.x
                                : (_lockAllAxesTo & AxisConstraint.Y) != 0 ? _sequenceds[i].toVector3Val.y : _sequenceds[i].toVector3Val.z;
                            _sequenceds[i].toVector3Val = new Vector3(value, value, value);
                            value = (_lockAllAxesTo & AxisConstraint.X) != 0 ? _sequenceds[i].fromVector3Val.x
                                : (_lockAllAxesTo & AxisConstraint.Y) != 0 ? _sequenceds[i].fromVector3Val.y : _sequenceds[i].fromVector3Val.z;
                            _sequenceds[i].fromVector3Val = new Vector3(value, value, value);
                        }
                    } else if (isVector4) {
                        for (int i = 0; i < _sequenceds.Count; ++i) {
                            value = (_lockAllAxesTo & AxisConstraint.X) != 0 ? _sequenceds[i].toVector4Val.x
                                : (_lockAllAxesTo & AxisConstraint.Y) != 0 ? _sequenceds[i].toVector4Val.y
                                : (_lockAllAxesTo & AxisConstraint.Z) != 0 ? _sequenceds[i].toVector4Val.z : _sequenceds[i].toVector4Val.w;
                            _sequenceds[i].toVector4Val = new Vector4(value, value, value, value);
                            value = (_lockAllAxesTo & AxisConstraint.X) != 0 ? _sequenceds[i].fromVector4Val.x
                                : (_lockAllAxesTo & AxisConstraint.Y) != 0 ? _sequenceds[i].fromVector4Val.y
                                : (_lockAllAxesTo & AxisConstraint.Z) != 0 ? _sequenceds[i].fromVector4Val.z : _sequenceds[i].fromVector4Val.w;
                            _sequenceds[i].fromVector4Val = new Vector4(value, value, value, value);
                        }
                    }
                }
            }
        }

        void RefreshVectorLockedToAndDisabledAxes()
        {
            DOVisualSequenced.PropertyType propertyType = _mainTweenPlugin.GetPlugData(_mainSequenced).propertyType;
            _lockAllAxesTo = AxisConstraint.None;
            _disabledAxes = AxisConstraint.None;
            bool useX, useY, useZ, useW;
            useX = _mainSequenced.axisConstraint == AxisConstraint.None || (_mainSequenced.axisConstraint & AxisConstraint.X) != 0;
            useY = _mainSequenced.axisConstraint == AxisConstraint.None || (_mainSequenced.axisConstraint & AxisConstraint.Y) != 0;
            useZ = _mainSequenced.axisConstraint == AxisConstraint.None || (_mainSequenced.axisConstraint & AxisConstraint.Z) != 0;
            useW = _mainSequenced.axisConstraint == AxisConstraint.None || (_mainSequenced.axisConstraint & AxisConstraint.W) != 0;
            if (!useX) _disabledAxes |= AxisConstraint.X;
            if (!useY) _disabledAxes |= AxisConstraint.Y;
            if (!useZ) _disabledAxes |= AxisConstraint.Z;
            if (!useW) _disabledAxes |= AxisConstraint.W;
            if (_mainSequenced.editor_lockVector) {
                switch (propertyType) {
                case DOVisualSequenced.PropertyType.Vector2:
                    if (useX) {
                        _lockAllAxesTo |= AxisConstraint.X;
                        _disabledAxes = ~AxisConstraint.X;
                    } else if (useY) {
                        _lockAllAxesTo |= AxisConstraint.Y;
                        _disabledAxes = ~AxisConstraint.Y;
                    }
                    break;
                case DOVisualSequenced.PropertyType.Vector3:
                    if (useX) {
                        _lockAllAxesTo |= AxisConstraint.X;
                        _disabledAxes = ~AxisConstraint.X;
                    } else if (useY) {
                        _lockAllAxesTo |= AxisConstraint.Y;
                        _disabledAxes = ~AxisConstraint.Y;
                    } else if (useZ) {
                        _lockAllAxesTo |= AxisConstraint.Z;
                        _disabledAxes = ~AxisConstraint.Z;
                    }
                    break;
                case DOVisualSequenced.PropertyType.Vector4:
                    if (useX) {
                        _lockAllAxesTo |= AxisConstraint.X;
                        _disabledAxes = ~AxisConstraint.X;
                    } else if (useY) {
                        _lockAllAxesTo |= AxisConstraint.Y;
                        _disabledAxes = ~AxisConstraint.Y;
                    } else if (useZ) {
                        _lockAllAxesTo |= AxisConstraint.Z;
                        _disabledAxes = ~AxisConstraint.Z;
                    } else if (useW) {
                        _lockAllAxesTo |= AxisConstraint.W;
                        _disabledAxes = ~AxisConstraint.W;
                    }
                    break;
                }
            }
        }

        void SwitchToFrom(DOVisualSequenced sequenced, DOVisualTweenPlugin plugin)
        {
            var toType = sequenced.toType;
            sequenced.toType = sequenced.fromType;
            sequenced.fromType = toType;
            switch (plugin.GetPlugData(sequenced).propertyType) {
            case DOVisualSequenced.PropertyType.Float:
                float toFloat = sequenced.toFloatVal;
                sequenced.toFloatVal = sequenced.fromFloatVal;
                sequenced.fromFloatVal = toFloat;
                break;
            case DOVisualSequenced.PropertyType.Int:
                int toInt = sequenced.toIntVal;
                sequenced.toIntVal = sequenced.fromIntVal;
                sequenced.fromIntVal = toInt;
                break;
            case DOVisualSequenced.PropertyType.Uint:
                uint toUint = sequenced.toUintVal;
                sequenced.toUintVal = sequenced.fromUintVal;
                sequenced.fromUintVal = toUint;
                break;
            case DOVisualSequenced.PropertyType.String:
                string toString = sequenced.toStringVal;
                sequenced.toStringVal = sequenced.fromStringVal;
                sequenced.fromStringVal = toString;
                break;
            case DOVisualSequenced.PropertyType.Vector2:
                Vector2 toVector2 = sequenced.toVector2Val;
                sequenced.toVector2Val = sequenced.fromVector2Val;
                sequenced.fromVector2Val = toVector2;
                break;
            case DOVisualSequenced.PropertyType.Vector3:
                Vector3 toVector3 = sequenced.toVector3Val;
                sequenced.toVector3Val = sequenced.fromVector3Val;
                sequenced.fromVector3Val = toVector3;
                break;
            case DOVisualSequenced.PropertyType.Vector4:
                Vector4 toVector4 = sequenced.toVector4Val;
                sequenced.toVector4Val = sequenced.fromVector4Val;
                sequenced.fromVector4Val = toVector4;
                break;
            case DOVisualSequenced.PropertyType.Color:
                Color toColor = sequenced.toColorVal;
                sequenced.toColorVal = sequenced.fromColorVal;
                sequenced.fromColorVal = toColor;
                break;
            case DOVisualSequenced.PropertyType.Rect:
                Rect toRect = sequenced.toRectVal;
                sequenced.toRectVal = sequenced.fromRectVal;
                sequenced.fromRectVal = toRect;
                break;
            }
            GUI.changed = true;
        }

        #region Context Menus

        void CM_PlugType()
        {
            GenericMenu menu = new GenericMenu();
            DOVisualTweenPlugin plugin = _mainTweenPlugin;
            if (plugin == null) return;
            SortedPlugData[] sortedPlugDatas = new SortedPlugData[plugin.totPluginDatas];
            for (int i = 0; i < plugin.totPluginDatas; ++i) {
                sortedPlugDatas[i] = new SortedPlugData(plugin.pluginDatas[i], i);
            }
//            Array.Sort(sortedPlugDatas, (a, b) => string.Compare(a.data.label, b.data.label, StringComparison.OrdinalIgnoreCase));
            Array.Sort(sortedPlugDatas, (a, b) => TimelineEditorUtils.SortPlugDataLabels(a.data.label, b.data.label));
            for (int i = 0; i < sortedPlugDatas.Length; ++i) {
                SortedPlugData sortedPData = sortedPlugDatas[i];
                menu.AddItem(
                    new GUIContent(plugin.Editor_GetShortTypeName() + " > " + sortedPData.data.label),
                    false, () => {
                        using (new DOScope.UndoableSerialization()) {
                            for (int j = 0; j < _totSequenceds; ++j) {
                                DOVisualSequenced sequenced = _sequenceds[j];
                                DOVisualTweenPlugin prevPlugin = _tweenPlugins[j];
                                string prevPlugDataGuid = sequenced.plugDataGuid;
                                int prevPlugDataIndex = sequenced.plugDataIndex;
                                sequenced.plugDataGuid = sortedPData.data.guid;
                                sequenced.plugDataIndex = sortedPData.originalIndex;
                                ConditionalResetSequenced(sequenced, plugin, prevPlugin, prevPlugDataGuid, prevPlugDataIndex);
                            }
                        }
                        Init(true);
                        DOVisualSequenceTimeline.Dispatch_OnSequenceChanged(sequence);
                    }
                );
            }
            menu.ShowAsContext();
        }

        void CM_GlobalPlugType()
        {
            GenericMenu menu = new GenericMenu();
            List<string> allPluginsIds = DOVisualPluginsManager.GlobalTweenPluginsIds;
//            allPluginsIds.Sort(StringComparer.OrdinalIgnoreCase);
            allPluginsIds.Sort(TimelineEditorUtils.SortPlugDataLabels);
            List<DOVisualTweenPlugin> plugins = new List<DOVisualTweenPlugin>();
            for (int i = 0; i < allPluginsIds.Count; ++i) {
                plugins.Add(DOVisualPluginsManager.GetGlobalTweenPlugin(allPluginsIds[i]));
            }
            for (int i = 0; i < plugins.Count; ++i) {
                DOVisualTweenPlugin plugin = plugins[i];
                string plugId = allPluginsIds[i];
                if (plugin == null) continue;
                SortedPlugData[] sortedPlugDatas = new SortedPlugData[plugin.totPluginDatas];
                for (int j = 0; j < plugin.totPluginDatas; ++j) {
                    sortedPlugDatas[j] = new SortedPlugData(plugin.pluginDatas[j], j);
                }
//                Array.Sort(sortedPlugDatas, (a, b) => string.Compare(a.data.label, b.data.label, StringComparison.OrdinalIgnoreCase));
                Array.Sort(sortedPlugDatas, (a, b) => TimelineEditorUtils.SortPlugDataLabels(a.data.label, b.data.label));
                for (int j = 0; j < sortedPlugDatas.Length; ++j) {
                    SortedPlugData sortedPData = sortedPlugDatas[j];
                    menu.AddItem(
                        new GUIContent(sortedPData.data.label),
                        false, () => {
                            using (new DOScope.UndoableSerialization()) {
                                for (int k = 0; k < _totSequenceds; ++k) {
                                    DOVisualSequenced sequenced = _sequenceds[k];
                                    sequenced.plugId = plugId;
                                    DOVisualTweenPlugin prevPlugin = _tweenPlugins[k];
                                    string prevPlugDataGuid = sequenced.plugDataGuid;
                                    int prevPlugDataIndex = sequenced.plugDataIndex;
                                    sequenced.plugDataGuid = sortedPData.data.guid;
                                    sequenced.plugDataIndex = sortedPData.originalIndex;
                                    ConditionalResetSequenced(sequenced, plugin, prevPlugin, prevPlugDataGuid, prevPlugDataIndex, true);
                                }
                            }
                            Init(true);
                        }
                    );
                }
            }
            menu.ShowAsContext();
        }

        void CM_ActionPlugType()
        {
            GenericMenu menu = new GenericMenu();
            List<string> allPluginsIds = DOVisualPluginsManager.ActionPluginsIds;
//            allPluginsIds.Sort(StringComparer.OrdinalIgnoreCase);
            allPluginsIds.Sort(TimelineEditorUtils.SortPlugDataLabels);
            List<DOVisualActionPlugin> plugins = new List<DOVisualActionPlugin>();
            for (int i = 0; i < allPluginsIds.Count; ++i) {
                plugins.Add(DOVisualPluginsManager.GetActionPlugin(allPluginsIds[i]));
            }
            for (int i = 0; i < plugins.Count; ++i) {
                DOVisualActionPlugin plugin = plugins[i];
                string plugId = allPluginsIds[i];
                if (plugin == null) continue;
                SortedPlugData[] sortedPlugDatas = new SortedPlugData[plugin.totPluginDatas];
                for (int j = 0; j < plugin.totPluginDatas; ++j) {
                    sortedPlugDatas[j] = new SortedPlugData(plugin.pluginDatas[j], j);
                }
//                Array.Sort(sortedPlugDatas, (a, b) => string.Compare(a.data.label, b.data.label, StringComparison.OrdinalIgnoreCase));
                Array.Sort(sortedPlugDatas, (a, b) => TimelineEditorUtils.SortPlugDataLabels(a.data.label, b.data.label));
                for (int j = 0; j < sortedPlugDatas.Length; ++j) {
                    SortedPlugData sortedPData = sortedPlugDatas[j];
                    menu.AddItem(
                        new GUIContent(sortedPData.data.label),
                        false, () => {
                            using (new DOScope.UndoableSerialization()) {
                                for (int k = 0; k < _totSequenceds; ++k) {
                                    DOVisualSequenced sequenced = _sequenceds[k];
                                    sequenced.plugId = plugId;
                                    DOVisualActionPlugin prevPlugin = _actionPlugins[k];
                                    string prevPlugDataGuid = sequenced.plugDataGuid;
                                    int prevPlugDataIndex = sequenced.plugDataIndex;
                                    sequenced.plugDataGuid = sortedPData.data.guid;
                                    sequenced.plugDataIndex = sortedPData.originalIndex;
                                    ConditionalResetSequenced(sequenced, plugin, prevPlugin, prevPlugDataGuid, prevPlugDataIndex);
                                }
                            }
                            Init(true);
                        }
                    );
                }
            }
            menu.ShowAsContext();
        }

        void CM_CopyValuesFromHierarchyTarget(bool isFrom)
        {
            GenericMenu menu = new GenericMenu();
            bool canCopyValuesFromTarget = CanCopyValuesFromSelectedTarget();
            PlugDataTween plugData = _mainTweenPlugin.GetPlugData(_mainSequenced) as PlugDataTween;
            if (canCopyValuesFromTarget) {
                menu.AddItem(new GUIContent("Assign from Selected GameObject"), false, () => {
                    bool set = false;
                    using (new DOScope.UndoableSerialization()) {
                        foreach (DOVisualSequenced s in _sequenceds) {
                            switch (plugData.label) {
                            case "AnchoredPosition":
                                set = true;
                                if (isFrom) s.fromVector2Val = Selection.activeTransform.GetComponent<RectTransform>().anchoredPosition;
                                else s.toVector2Val = Selection.activeTransform.GetComponent<RectTransform>().anchoredPosition;
                                break;
                            case "AnchoredPosition 3D":
                                set = true;
                                if (isFrom) s.fromVector3Val = Selection.activeTransform.GetComponent<RectTransform>().anchoredPosition3D;
                                else s.toVector3Val = Selection.activeTransform.GetComponent<RectTransform>().anchoredPosition3D;
                                break;
                            case "Position":
                                set = true;
                                if (isFrom) s.fromVector3Val = Selection.activeTransform.position;
                                else s.toVector3Val = Selection.activeTransform.position;
                                break;
                            case "Local Position":
                                set = true;
                                if (isFrom) s.fromVector3Val = Selection.activeTransform.localPosition;
                                else s.toVector3Val = Selection.activeTransform.localPosition;
                                break;
                            case "Rotation":
                                set = true;
                                if (isFrom) s.fromVector3Val = Selection.activeTransform.eulerAngles;
                                else s.toVector3Val = Selection.activeTransform.eulerAngles;
                                break;
                            case "Local Rotation":
                                set = true;
                                if (isFrom) s.fromVector3Val = Selection.activeTransform.localEulerAngles;
                                else s.toVector3Val = Selection.activeTransform.localEulerAngles;
                                break;
                            case "Scale":
                                set = true;
                                if (isFrom) s.fromVector3Val = Selection.activeTransform.localScale;
                                else s.toVector3Val = Selection.activeTransform.localScale;
                                break;
                            }
                            if (set) {
                                if (isFrom && s.fromType != DOVisualSequenced.ToFromType.Direct) s.fromType = DOVisualSequenced.ToFromType.Direct;
                                else if (!isFrom && s.toType != DOVisualSequenced.ToFromType.Direct) s.toType = DOVisualSequenced.ToFromType.Direct;
                            } 
                        }
                        if (!set) Debug.LogWarning("Nothing could be set: this shouldn't happen");
                    }
                    DOVisualSequenceTimeline.Dispatch_OnSequenceChanged(sequence);
                });
            } else {
                menu.AddDisabledItem(new GUIContent("Assign from Selected GameObject (nothing valid selected)"));
            }
            menu.ShowAsContext();
        }

        #endregion

        #endregion

        #region Methods

        void CacheCurrTargets()
        {
            _currTargets.Clear();
            for (int i = 0; i < _totSequenceds; ++i) _currTargets.Add(_sequenceds[i].target);
        }

        void RestoreCurrTargets()
        {
            for (int i = 0; i < _totSequenceds; ++i) _sequenceds[i].target = _currTargets[i];
        }

        bool CanHaveEase()
        {
            switch (_mainTweenPlugin.GetPlugData(_mainSequenced).tweenType) {
            case PluginTweenType.Punch:
            case PluginTweenType.Shake:
                return false;
            default: return true;
            }
        }

        bool CanHaveFrom(DOVisualSequenced sequenced, DOVisualTweenPlugin plugin)
        {
            // switch (_mainTweenPlugin.GetPlugData(_mainSequenced).tweenType) {
            switch (plugin.GetPlugData(sequenced).tweenType) {
            case PluginTweenType.Punch:
            case PluginTweenType.Shake:
                return false;
            default: return true;
            }
        }

        // Assumes all sequenceds have same-type plugins
        bool CanCopyValuesFromSelectedTarget()
        {
            if (!_isTweener || _isGlobal || Selection.activeTransform == null) return false;
            Type pluginTargetType = _mainTweenPlugin.targetType;
            if (pluginTargetType == typeof(RectTransform)) {
                return !_mainTweenPlugin.GetPlugData(_mainSequenced).label.Contains("AnchoredPosition")
                       || Selection.activeTransform.GetComponent<RectTransform>() != null;
            } else if (pluginTargetType == typeof(Transform)) return true;
            return false;
        }

        void ConditionalResetSequenced(
            DOVisualSequenced sequenced, DOVisualTweenPlugin plugin, DOVisualTweenPlugin prevPlugin,
            string prevPlugDataGuid, int prevPlugDataIndex, bool forceReset = false
        ){
            sequenced.toStringVal = sequenced.fromStringVal = null; // Reset strings to free up memory
            sequenced.axisConstraint = AxisConstraint.None; // Reset axis constraint (rotation doesn't use them)
            if (!CanHaveFrom(sequenced, plugin)) {
                sequenced.toType = DOVisualSequenced.ToFromType.Direct;
                sequenced.fromType = DOVisualSequenced.ToFromType.Dynamic;
            }
            PluginTweenType tweenType = plugin.GetPlugData(sequenced).tweenType;
            if (forceReset || prevPlugin != null && tweenType != prevPlugin.GetPlugData(prevPlugDataGuid, prevPlugDataIndex).tweenType) {
                switch (tweenType) {
                case PluginTweenType.Punch:
                case PluginTweenType.Shake:
                    sequenced.isRelative = false;
                    sequenced.boolOption0 = false;
                    sequenced.intOption0 = 0;
                    sequenced.intOption1 = 10;
                    sequenced.floatOption0 = tweenType == PluginTweenType.Shake ? 90 : 1;
                    sequenced.ease = Ease.Linear;
                    break;
                }
                sequenced.stringOption0 = null;
            }
        }
        void ConditionalResetSequenced(
            DOVisualSequenced sequenced, DOVisualActionPlugin plugin, DOVisualActionPlugin prevPlugin, string prevPlugDataGuid, int prevPlugDataIndex
        ){
            PlugDataAction plugData = plugin.GetPlugData(sequenced);
            bool resetTarget;
            if (prevPlugin == null) resetTarget = true;
            else {
                PlugDataAction prevPlugData = prevPlugin.GetPlugData(prevPlugDataGuid, prevPlugDataIndex);
                resetTarget = prevPlugData == null
                              || prevPlugData.wantsTarget != plugData.wantsTarget || prevPlugData.targetType != plugData.targetType;
            }
            if (resetTarget) sequenced.target = null;
            sequenced.boolOption0 = plugData.defBoolValue;
            sequenced.toStringVal = plugData.defStringValue;
            sequenced.toFloatVal = plugData.defFloat0Value;
            sequenced.fromFloatVal = plugData.defFloat1Value;
            sequenced.floatOption0 = plugData.defFloat2Value;
            sequenced.floatOption1 = plugData.defFloat3Value;
            sequenced.toIntVal = plugData.defIntValue;
        }

        #endregion

        // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████
        // ███ INTERNAL CLASSES ████████████████████████████████████████████████████████████████████████████████████████████████
        // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

        public struct EaseSnapshot
        {
            public Ease ease;
            public float overshootOrAmplitude, period;

            public EaseSnapshot(Ease ease, float overshootOrAmplitude, float period)
            {
                this.ease = ease;
                this.overshootOrAmplitude = overshootOrAmplitude;
                this.period = period;
            }

            public static bool operator ==(EaseSnapshot a, EaseSnapshot b)
            {
                return a.ease == b.ease
                       && Mathf.Approximately(a.overshootOrAmplitude, b.overshootOrAmplitude)
                       && Mathf.Approximately(a.period, b.period);
            }
            public static bool operator !=(EaseSnapshot a, EaseSnapshot b)
            {
                return a.ease != b.ease
                       || !Mathf.Approximately(a.overshootOrAmplitude, b.overshootOrAmplitude)
                       || !Mathf.Approximately(a.period, b.period);
            }

            public override bool Equals(object obj)
            { return base.Equals(obj); }
            public override int GetHashCode()
            { return base.GetHashCode(); }
        }

        // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

        class SortedPlugData
        {
            public IPluginData data;
            public int originalIndex;
            public SortedPlugData(IPluginData pluginData, int originalIndex)
            {
                this.data = pluginData;
                this.originalIndex = originalIndex;
            }
        }
    }
}