// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/01/21

using System;
using System.Collections.Generic;
using DG.Tweening.Core;
using DG.Tweening.Plugins.Options;
using UnityEngine;

namespace DG.Tweening.Timeline.Core.Plugins
{
    /// <summary>
    /// Contains/executes all possible tween types based on its componentType
    /// </summary>
    public class DOVisualTweenPlugin
    {
        public readonly Type targetType;
        public readonly ITweenPluginData[] pluginDatas; // Always sorted by label in editor windows
        public readonly int totPluginDatas;

        readonly Dictionary<string, ITweenPluginData> _guidToPlugData = new Dictionary<string, ITweenPluginData>();

        public DOVisualTweenPlugin(Type targetType, params ITweenPluginData[] pluginDatas)
        {
            this.targetType = targetType;
            this.pluginDatas = pluginDatas;
            totPluginDatas = this.pluginDatas.Length;

            for (int i = 0; i < totPluginDatas; ++i) {
                string plugDataGuid = this.pluginDatas[i].guid ?? "INVALID:" + Guid.NewGuid(); // Missing GUID, assign a unique one but marked as invalid
                if (_guidToPlugData.ContainsKey(plugDataGuid)) {
                    DOLog.Error(string.Format("Another ITweenPluginData with the same guid for \"{0}\" already exists (guid: \"{1}\")", targetType, plugDataGuid));
                } else _guidToPlugData.Add(plugDataGuid, this.pluginDatas[i]);
            }
        }

        #region Public Methods

        /// <summary>
        /// Gets a <see cref="ITweenPluginData"/> by the given sequenced plugDataGuid, and if it doesn't find it falls back on the sequenced plugDataIndex.
        /// Returns NULL if both retrieval methods fail.
        /// </summary>
        public ITweenPluginData GetPlugData(DOVisualSequenced sequenced)
        {
            return GetPlugData(sequenced.plugDataGuid, sequenced.plugDataIndex);
        }
        /// <summary>
        /// Gets a <see cref="ITweenPluginData"/> by the given sequenced plugDataGuid, and if it doesn't find it falls back on the sequenced plugDataIndex.
        /// Returns NULL if both retrieval methods fail.
        /// </summary>
        public ITweenPluginData GetPlugData(string plugDataGuid, int plugDataIndex)
        {
            return plugDataGuid != null && _guidToPlugData.TryGetValue(plugDataGuid, out ITweenPluginData plugData)
                ? plugData
                : string.IsNullOrEmpty(plugDataGuid) && plugDataIndex < totPluginDatas ? pluginDatas[plugDataIndex] : null;
        }

        public bool HasPlugData(DOVisualSequenced sequenced)
        {
            if (!string.IsNullOrEmpty(sequenced.plugDataGuid)) return _guidToPlugData.ContainsKey(sequenced.plugDataGuid);
            return sequenced.plugDataIndex < totPluginDatas;
        }

        // Assumes target is not NULL. Returns NULL if plugData wasn't found
        public Tweener CreateTween(DOVisualSequenced sequenced, float timeMultiplier)
        {
            ITweenPluginData plugData = GetPlugData(sequenced);
            if (plugData == null) return null; // Missing plugData
            Tweener t = null;
            float duration = sequenced.duration * timeMultiplier;
            switch (plugData.propertyType) {
            case DOVisualSequenced.PropertyType.Float: //---------------------------------------------------------
                TweenerCore<float, float, FloatOptions> floatT;
                DOGetter<float> floatGetter = plugData.FloatGetter(sequenced.target, sequenced.stringOption0, sequenced.intOption1);
                switch (sequenced.toType) {
                case DOVisualSequenced.ToFromType.Dynamic:
                    floatT = DOTween.To(
                        floatGetter, plugData.FloatSetter(sequenced.target, sequenced.stringOption0, sequenced.intOption1), floatGetter(), duration
                    );
                    break;
                default:
                    floatT = DOTween.To(
                        floatGetter, plugData.FloatSetter(sequenced.target, sequenced.stringOption0, sequenced.intOption1),
                        sequenced.isRelative ? floatGetter() + sequenced.toFloatVal : sequenced.toFloatVal, duration
                    );
                    break;
                }
                floatT.SetOptions(sequenced.boolOption0);
                if (sequenced.fromType == DOVisualSequenced.ToFromType.Direct) {
                    floatT.From(sequenced.isRelative ? floatGetter() + sequenced.fromFloatVal : sequenced.fromFloatVal);
                }
                t = floatT;
                break;
            case DOVisualSequenced.PropertyType.Int: //---------------------------------------------------------
                TweenerCore<int, int, NoOptions> intT;
                DOGetter<int> intGetter = plugData.IntGetter(sequenced.target, sequenced.stringOption0, sequenced.intOption1);
                switch (sequenced.toType) {
                case DOVisualSequenced.ToFromType.Dynamic:
                    intT = DOTween.To(
                        intGetter, plugData.IntSetter(sequenced.target, sequenced.stringOption0, sequenced.intOption1), intGetter(), duration
                        );
                    break;
                default:
                    intT = DOTween.To(
                        intGetter, plugData.IntSetter(sequenced.target, sequenced.stringOption0, sequenced.intOption1),
                        sequenced.isRelative ? intGetter() + sequenced.toIntVal : sequenced.toIntVal, duration
                    );
                    break;
                }
                if (sequenced.fromType == DOVisualSequenced.ToFromType.Direct) {
                    intT.From(sequenced.isRelative ? intGetter() + sequenced.fromIntVal : sequenced.fromIntVal);
                }
                t = intT;
                break;
            case DOVisualSequenced.PropertyType.Uint: //---------------------------------------------------------
                TweenerCore<uint, uint, UintOptions> uintT;
                DOGetter<uint> uintGetter = plugData.UintGetter(sequenced.target, sequenced.stringOption0, sequenced.intOption1);
                switch (sequenced.toType) {
                case DOVisualSequenced.ToFromType.Dynamic:
                    uintT = DOTween.To(
                        uintGetter, plugData.UintSetter(sequenced.target, sequenced.stringOption0, sequenced.intOption1), uintGetter(), duration
                        );
                    break;
                default:
                    uintT = DOTween.To(
                        uintGetter, plugData.UintSetter(sequenced.target, sequenced.stringOption0, sequenced.intOption1),
                        sequenced.isRelative ? uintGetter() + sequenced.toUintVal : sequenced.toUintVal, duration
                    );
                    break;
                }
                if (sequenced.fromType == DOVisualSequenced.ToFromType.Direct) {
                    uintT.From(sequenced.isRelative ? uintGetter() + sequenced.fromUintVal : sequenced.fromUintVal);
                }
                t = uintT;
                break;
            case DOVisualSequenced.PropertyType.String: //---------------------------------------------------------
                TweenerCore<string, string, StringOptions> stringT;
                DOGetter<string> stringGetter = plugData.StringGetter(sequenced.target, sequenced.stringOption0, sequenced.intOption1);
                switch (sequenced.toType) {
                case DOVisualSequenced.ToFromType.Dynamic:
                    stringT = DOTween.To(
                        stringGetter, plugData.StringSetter(sequenced.target, sequenced.stringOption0, sequenced.intOption1),
                        stringGetter(), duration
                    );
                    break;
                default:
                    stringT = DOTween.To(
                        stringGetter, plugData.StringSetter(sequenced.target, sequenced.stringOption0, sequenced.intOption1),
                        sequenced.isRelative ? stringGetter() + sequenced.toStringVal : sequenced.toStringVal, duration
                    );
                    break;
                }
                stringT.SetOptions(sequenced.boolOption0, (ScrambleMode)sequenced.intOption1, sequenced.stringOption0);
                if (sequenced.fromType == DOVisualSequenced.ToFromType.Direct) {
                    stringT.From(sequenced.isRelative ? stringGetter() + sequenced.fromStringVal : sequenced.fromStringVal);
                }
                t = stringT;
                break;
            case DOVisualSequenced.PropertyType.Vector2: //---------------------------------------------------------
                TweenerCore<Vector2, Vector2, VectorOptions> vector2T;
                DOGetter<Vector2> vector2Getter = plugData.Vector2Getter(sequenced.target, sequenced.stringOption0, sequenced.intOption1);
                switch (sequenced.toType) {
                case DOVisualSequenced.ToFromType.Dynamic:
                    vector2T = DOTween.To(
                        vector2Getter, plugData.Vector2Setter(sequenced.target, sequenced.stringOption0, sequenced.intOption1),
                        vector2Getter(), duration
                    );
                    break;
                default:
                    vector2T = DOTween.To(
                        vector2Getter, plugData.Vector2Setter(sequenced.target, sequenced.stringOption0, sequenced.intOption1),
                        sequenced.isRelative ? vector2Getter() + sequenced.toVector2Val : sequenced.toVector2Val, duration
                    );
                    break;
                }
                vector2T.SetOptions(sequenced.axisConstraint, sequenced.boolOption0);
                if (sequenced.fromType == DOVisualSequenced.ToFromType.Direct) {
                    bool noAxisConstraint = sequenced.axisConstraint == AxisConstraint.None;
                    Vector2 fromVector2ValFiltered = new Vector2(
                        noAxisConstraint || sequenced.axisConstraint == AxisConstraint.X ? sequenced.fromVector2Val.x : 0,
                        noAxisConstraint || sequenced.axisConstraint == AxisConstraint.Y ? sequenced.fromVector2Val.y : 0
                    );
                    vector2T.From(sequenced.isRelative ? vector2Getter() + fromVector2ValFiltered : fromVector2ValFiltered);
                }
                t = vector2T;
                break;
            case DOVisualSequenced.PropertyType.Vector3: //---------------------------------------------------------
                switch (plugData.tweenType) {
                case PluginTweenType.Punch: //---------------
                    TweenerCore<Vector3, Vector3[], Vector3ArrayOptions> punchT;
                    punchT = DOTween.Punch(
                        plugData.Vector3Getter(sequenced.target, sequenced.stringOption0, sequenced.intOption1),
                        plugData.Vector3Setter(sequenced.target, sequenced.stringOption0, sequenced.intOption1),
                        sequenced.toVector3Val, duration, sequenced.intOption1, sequenced.floatOption0
                    );
                    punchT.SetOptions(sequenced.axisConstraint, sequenced.boolOption0);
                    t = punchT;
                    break;
                case PluginTweenType.Shake: //---------------
                    TweenerCore<Vector3, Vector3[], Vector3ArrayOptions> shakeT;
                    shakeT = DOTween.Shake(
                        plugData.Vector3Getter(sequenced.target, sequenced.stringOption0, sequenced.intOption1),
                        plugData.Vector3Setter(sequenced.target, sequenced.stringOption0, sequenced.intOption1),
                        duration, sequenced.toVector3Val, sequenced.intOption1, sequenced.floatOption0, sequenced.intOption0 == 1
                    );
                    shakeT.SetOptions(sequenced.axisConstraint, sequenced.boolOption0);
                    t = shakeT;
                    break;
                default: //---------------
                    TweenerCore<Vector3, Vector3, VectorOptions> vector3T;
                    DOGetter<Vector3> vector3Getter = plugData.Vector3Getter(sequenced.target, sequenced.stringOption0, sequenced.intOption1);
                    switch (sequenced.toType) {
                    case DOVisualSequenced.ToFromType.Dynamic:
                        vector3T = DOTween.To(
                            vector3Getter, plugData.Vector3Setter(sequenced.target, sequenced.stringOption0, sequenced.intOption1),
                            vector3Getter(), duration
                        );
                        break;
                    default:
                        vector3T = DOTween.To(
                            vector3Getter, plugData.Vector3Setter(sequenced.target, sequenced.stringOption0, sequenced.intOption1),
                            sequenced.isRelative ? vector3Getter() + sequenced.toVector3Val : sequenced.toVector3Val, duration
                        );
                        break;
                    }
                    vector3T.SetOptions(sequenced.axisConstraint, sequenced.boolOption0);
                    if (sequenced.fromType == DOVisualSequenced.ToFromType.Direct) {
                        bool noAxisConstraint = sequenced.axisConstraint == AxisConstraint.None;
                        Vector3 fromVector3ValFiltered = new Vector3(
                            noAxisConstraint || sequenced.axisConstraint == AxisConstraint.X ? sequenced.fromVector3Val.x : 0,
                            noAxisConstraint || sequenced.axisConstraint == AxisConstraint.Y ? sequenced.fromVector3Val.y : 0,
                            noAxisConstraint || sequenced.axisConstraint == AxisConstraint.Z ? sequenced.fromVector3Val.z : 0
                        );
                        vector3T.From(sequenced.isRelative ? vector3Getter() + fromVector3ValFiltered : fromVector3ValFiltered);
                    }
                    t = vector3T;
                    break;
                }
                break;
            case DOVisualSequenced.PropertyType.Vector4: //---------------------------------------------------------
                TweenerCore<Vector4, Vector4, VectorOptions> vector4T;
                DOGetter<Vector4> vector4Getter = plugData.Vector4Getter(sequenced.target, sequenced.stringOption0, sequenced.intOption1);
                switch (sequenced.toType) {
                case DOVisualSequenced.ToFromType.Dynamic:
                    vector4T = DOTween.To(
                        vector4Getter, plugData.Vector4Setter(sequenced.target, sequenced.stringOption0, sequenced.intOption1),
                        vector4Getter(), duration
                    );
                    break;
                default:
                    vector4T = DOTween.To(
                        vector4Getter, plugData.Vector4Setter(sequenced.target, sequenced.stringOption0, sequenced.intOption1),
                        sequenced.isRelative ? vector4Getter() + sequenced.toVector4Val : sequenced.toVector4Val, duration
                    );
                    break;
                }
                vector4T.SetOptions(sequenced.axisConstraint, sequenced.boolOption0);
                if (sequenced.fromType == DOVisualSequenced.ToFromType.Direct) {
                    bool noAxisConstraint = sequenced.axisConstraint == AxisConstraint.None;
                    Vector4 fromVector4ValFiltered = new Vector4(
                        noAxisConstraint || sequenced.axisConstraint == AxisConstraint.X ? sequenced.fromVector4Val.x : 0,
                        noAxisConstraint || sequenced.axisConstraint == AxisConstraint.Y ? sequenced.fromVector4Val.y : 0,
                        noAxisConstraint || sequenced.axisConstraint == AxisConstraint.Z ? sequenced.fromVector4Val.z : 0,
                        noAxisConstraint || sequenced.axisConstraint == AxisConstraint.W ? sequenced.fromVector4Val.w : 0
                    );
                    vector4T.From(sequenced.isRelative ? vector4Getter() + fromVector4ValFiltered : fromVector4ValFiltered);
                }
                t = vector4T;
                break;
            case DOVisualSequenced.PropertyType.Quaternion: //---------------------------------------------------------
                TweenerCore<Quaternion, Vector3, QuaternionOptions> quaternionT;
                DOGetter<Quaternion> quaternionGetter = plugData.QuaternionGetter(sequenced.target, sequenced.stringOption0, sequenced.intOption1);
                switch (sequenced.toType) {
                case DOVisualSequenced.ToFromType.Dynamic:
                    quaternionT = DOTween.To(
                        quaternionGetter, plugData.QuaternionSetter(sequenced.target, sequenced.stringOption0, sequenced.intOption1),
                        quaternionGetter().eulerAngles, duration
                    );
                    break;
                default:
                    quaternionT = DOTween.To(
                        quaternionGetter, plugData.QuaternionSetter(sequenced.target, sequenced.stringOption0, sequenced.intOption1),
                        sequenced.isRelative ? quaternionGetter().eulerAngles + sequenced.toVector3Val : sequenced.toVector3Val, duration
                    );
                    break;
                }
                if (sequenced.fromType == DOVisualSequenced.ToFromType.Direct) {
                    quaternionT.From(sequenced.isRelative ? quaternionGetter().eulerAngles + sequenced.fromVector3Val : sequenced.fromVector3Val);
                }
                quaternionT.plugOptions.rotateMode = (RotateMode)sequenced.intOption0;
                t = quaternionT;
                break;
            case DOVisualSequenced.PropertyType.Color: //---------------------------------------------------------
                TweenerCore<Color, Color, ColorOptions> colorT;
                DOGetter<Color> colorGetter = plugData.ColorGetter(sequenced.target, sequenced.stringOption0, sequenced.intOption1);
                switch (sequenced.toType) {
                case DOVisualSequenced.ToFromType.Dynamic:
                    colorT = sequenced.boolOption0
                        ? DOTween.ToAlpha(
                            colorGetter, plugData.ColorSetter(sequenced.target, sequenced.stringOption0, sequenced.intOption1),
                            colorGetter().a, duration
                        )
                        : DOTween.To(
                            colorGetter, plugData.ColorSetter(sequenced.target, sequenced.stringOption0, sequenced.intOption1),
                            colorGetter(), duration
                        );
                    break;
                default:
                    colorT = sequenced.boolOption0
                        ? DOTween.ToAlpha(
                            colorGetter, plugData.ColorSetter(sequenced.target, sequenced.stringOption0, sequenced.intOption1),
                            sequenced.isRelative ? colorGetter().a + sequenced.toColorVal.a : sequenced.toColorVal.a, duration
                        )
                        : DOTween.To(
                            colorGetter, plugData.ColorSetter(sequenced.target, sequenced.stringOption0, sequenced.intOption1),
                            sequenced.isRelative ? colorGetter() + sequenced.toColorVal : sequenced.toColorVal, duration
                        );
                    break;
                }
                if (sequenced.fromType == DOVisualSequenced.ToFromType.Direct) {
                    if (sequenced.boolOption0) colorT.From(sequenced.isRelative ? colorGetter().a + sequenced.fromColorVal.a : sequenced.fromColorVal.a);
                    else colorT.From(sequenced.isRelative ? colorGetter() + sequenced.fromColorVal : sequenced.fromColorVal);
                }
                t = colorT;
                break;
            case DOVisualSequenced.PropertyType.Rect: //---------------------------------------------------------
                TweenerCore<Rect, Rect, RectOptions> rectT;
                DOGetter<Rect> rectGetter = plugData.RectGetter(sequenced.target, sequenced.stringOption0, sequenced.intOption1);
                switch (sequenced.toType) {
                case DOVisualSequenced.ToFromType.Dynamic:
                    rectT = DOTween.To(
                        rectGetter, plugData.RectSetter(sequenced.target, sequenced.stringOption0, sequenced.intOption1), rectGetter(), duration
                    );
                    break;
                default:
                    rectT = DOTween.To(
                            rectGetter, plugData.RectSetter(sequenced.target, sequenced.stringOption0, sequenced.intOption1),
                            sequenced.isRelative ? Add(rectGetter(), sequenced.toRectVal) : sequenced.toRectVal, duration
                        );
                    break;
                }
                if (sequenced.fromType == DOVisualSequenced.ToFromType.Direct) {
                    rectT.From(sequenced.isRelative ? Add(rectGetter(), sequenced.fromRectVal) : sequenced.fromRectVal);
                }
                t = rectT;
                break;
            }
            if (t == null) return null;
            if (sequenced.ease == Ease.INTERNAL_Custom) t.SetEase(sequenced.easeCurve);
            else t.SetEase(sequenced.ease, sequenced.overshootOrAmplitude, sequenced.period);
            t.SetLoops(sequenced.loops, sequenced.loopType);
            if (DOVisualSequenceSettings.isApplicationPlaying) {
                if (sequenced.onComplete.GetPersistentEventCount() > 0) t.OnComplete(sequenced.onComplete.Invoke);
                if (sequenced.onStepComplete.GetPersistentEventCount() > 0) t.OnStepComplete(sequenced.onStepComplete.Invoke);
                if (sequenced.onUpdate.GetPersistentEventCount() > 0) t.OnUpdate(sequenced.onUpdate.Invoke);
            }
            return t;
        }

#if UNITY_EDITOR
        Dictionary<string, GUIContent> _editor_guidToGc; // Used to cache GUIContent values
        Dictionary<string, GUIContent> _editor_guidToGc_timeline; // Used to cache GUIContent values
        GUIContent[] _editor_gcs; // Used to cache GUIContent values - fallback for legacy plugin index usage
        GUIContent[] _editor_gcs_timeline; // Used to cache GUIContent values - fallback for legacy plugin index usage
        readonly GUIContent _editor_missingPlugin = new GUIContent("<color=#ff0000>UNSUPPORTED</color>");

        public GUIContent Editor_GetShortTypeAndAnimationNameGUIContent(DOVisualSequenced sequenced)
        {
            if (_editor_guidToGc == null) {
                _editor_guidToGc = new Dictionary<string, GUIContent>();
                _editor_gcs = new GUIContent[totPluginDatas];
            }
            GUIContent gc = GetGuiContent(sequenced, _editor_guidToGc, _editor_gcs);
            if (gc == null) {
                ITweenPluginData plugData = GetPlugData(sequenced);
                if (plugData == null) return _editor_missingPlugin; // This can happen if sequenced was created with a now disabled module in legacy mode
                gc = new GUIContent(Editor_GetShortTypeAndAnimationName(plugData, false));
                if (!string.IsNullOrEmpty(plugData.guid)) _editor_guidToGc.Add(plugData.guid, gc);
                _editor_gcs[sequenced.plugDataIndex] = gc;
            }
            return gc;
        }

        public GUIContent Editor_GetAnimationNameGUIContent(DOVisualSequenced sequenced)
        {
            if (_editor_gcs_timeline == null) {
                _editor_guidToGc_timeline = new Dictionary<string, GUIContent>();
                _editor_gcs_timeline = new GUIContent[totPluginDatas];
            }
            GUIContent gc = GetGuiContent(sequenced, _editor_guidToGc_timeline, _editor_gcs_timeline);
            if (gc == null) {
                ITweenPluginData plugData = GetPlugData(sequenced);
                if (plugData == null) return _editor_missingPlugin; // This can happen if sequenced was created with a now disabled module in legacy mode
                gc = new GUIContent(Editor_GetAnimationName(plugData, true));
                if (!string.IsNullOrEmpty(plugData.guid)) _editor_guidToGc_timeline.Add(plugData.guid, gc);
                _editor_gcs_timeline[sequenced.plugDataIndex] = gc;
            }
            return gc;
        }

        public string Editor_GetShortTypeName(bool richText = false)
        {
            string s = targetType.FullName;
            int dotIndex = s.LastIndexOf('.');
            if (dotIndex != -1) s = s.Substring(dotIndex + 1);
            if (richText) s = string.Format("<color=#68b3e2>{0}</color>", s);
            return s;
        }

        GUIContent GetGuiContent(DOVisualSequenced sequenced, Dictionary<string, GUIContent> guidToGcDict, GUIContent[] gcList)
        {
            return sequenced.plugDataGuid != null && guidToGcDict.TryGetValue(sequenced.plugDataGuid, out GUIContent gc)
                ? gc
                : sequenced.plugDataIndex < totPluginDatas ? gcList[sequenced.plugDataIndex] : null;
        }

        string Editor_GetShortTypeAndAnimationName(ITweenPluginData plugData, bool forTimeline)
        {
            return targetType == null
                ? string.Format("<color=#68b3e2>Global</color> → <color=#ffa047>{0}</color>", Editor_GetAnimationName(plugData, forTimeline))
                : string.Format("<color=#68b3e2>{0}</color> → <color=#ffa047>{1}</color>", Editor_GetShortTypeName(), Editor_GetAnimationName(plugData, forTimeline));
        }

        string Editor_GetAnimationName(ITweenPluginData plugData, bool forTimeline)
        {
            string label = plugData.label;
            int lastSlashIndex = label.LastIndexOf('/');
            if (lastSlashIndex == -1)
                return string.Format(" <color=#ffa047>{0}</color>", label);
            return forTimeline
                ? string.Format("<color=#2e0020>{0}</color>→<color=#ffa047>{1}</color>",
                    label.Substring(0, lastSlashIndex), label.Substring(lastSlashIndex + 1))
                : string.Format("<color=#a8a5ff>{0}</color>→<color=#ffa047>{1}</color>",
                    label.Substring(0, lastSlashIndex), label.Substring(lastSlashIndex + 1));
        }
#endif

        #endregion

        #region Methods

        /// <summary>Adds one rect into another, and returns the resulting a</summary>
        static Rect Add(Rect a, Rect b)
        {
            if (b.xMin < a.xMin) a.xMin = b.xMin;
            if (b.xMax > a.xMax) a.xMax = b.xMax;
            if (b.yMin < a.yMin) a.yMin = b.yMin;
            if (b.yMax > a.yMax) a.yMax = b.yMax;
            return a;
        }

        #endregion
    }
}