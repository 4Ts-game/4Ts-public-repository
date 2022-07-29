// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/01/31

using System;
using DG.Tweening.Core;
using UnityEngine;

namespace DG.Tweening.Timeline.Core.Plugins
{
    public class PlugDataGlobalTween : ITweenPluginData
    {
        public bool wantsTarget { get { return false; } }
        public string guid { get; private set; }
        public string label { get; private set; }
        public string targetLabel { get { return "Target"; } }
        public string stringOptionLabel { get; private set; }
        public string intOptionLabel { get; private set; }
        public DOVisualSequenced.PropertyType propertyType { get; private set; }
        public PluginTweenType tweenType { get; private set; }

        readonly DOGetter<float> _floatGetter;
        readonly DOSetter<float> _floatSetter;
        readonly DOGetter<int> _intGetter;
        readonly DOSetter<int> _intSetter;
        readonly DOGetter<uint> _uintGetter;
        readonly DOSetter<uint> _uintSetter;
        readonly DOGetter<string> _stringGetter;
        readonly DOSetter<string> _stringSetter;
        readonly DOGetter<Vector2> _vector2Getter;
        readonly DOSetter<Vector2> _vector2Setter;
        readonly DOGetter<Vector3> _vector3Getter;
        readonly DOSetter<Vector3> _vector3Setter;
        readonly DOGetter<Vector4> _vector4Getter;
        readonly DOSetter<Vector4> _vector4Setter;
        readonly DOGetter<Quaternion> _quaternionGetter;
        readonly DOSetter<Quaternion> _quaternionSetter;
        readonly DOGetter<Color> _colorGetter;
        readonly DOSetter<Color> _colorSetter;
        readonly DOGetter<Rect> _rectGetter;
        readonly DOSetter<Rect> _rectSetter;

        public PlugDataGlobalTween(string guid, string label, DOGetter<float> getter, DOSetter<float> setter, PluginTweenType tweenType = PluginTweenType.SelfDetermined)
        {
            propertyType = DOVisualSequenced.PropertyType.Float;
            this.guid = guid;
            this.label = label;
            this.tweenType = tweenType;
            this._floatGetter = getter;
            this._floatSetter = setter;
        }
        public PlugDataGlobalTween(string guid, string label, DOGetter<int> getter, DOSetter<int> setter, PluginTweenType tweenType = PluginTweenType.SelfDetermined)
        {
            propertyType = DOVisualSequenced.PropertyType.Int;
            this.guid = guid;
            this.label = label;
            this.tweenType = tweenType;
            this._intGetter = getter;
            this._intSetter = setter;
        }
        public PlugDataGlobalTween(string guid, string label, DOGetter<uint> getter, DOSetter<uint> setter, PluginTweenType tweenType = PluginTweenType.SelfDetermined)
        {
            propertyType = DOVisualSequenced.PropertyType.Uint;
            this.guid = guid;
            this.label = label;
            this.tweenType = tweenType;
            this._uintGetter = getter;
            this._uintSetter = setter;
        }
        public PlugDataGlobalTween(string guid, string label, DOGetter<string> getter, DOSetter<string> setter, PluginTweenType tweenType = PluginTweenType.SelfDetermined)
        {
            propertyType = DOVisualSequenced.PropertyType.String;
            this.guid = guid;
            this.label = label;
            this.tweenType = tweenType;
            this._stringGetter = getter;
            this._stringSetter = setter;
        }
        public PlugDataGlobalTween(string guid, string label, DOGetter<Vector2> getter, DOSetter<Vector2> setter, PluginTweenType tweenType = PluginTweenType.SelfDetermined)
        {
            propertyType = DOVisualSequenced.PropertyType.Vector2;
            this.guid = guid;
            this.label = label;
            this.tweenType = tweenType;
            this._vector2Getter = getter;
            this._vector2Setter = setter;
        }
        public PlugDataGlobalTween(string guid, string label, DOGetter<Vector3> getter, DOSetter<Vector3> setter, PluginTweenType tweenType = PluginTweenType.SelfDetermined)
        {
            propertyType = DOVisualSequenced.PropertyType.Vector3;
            this.guid = guid;
            this.label = label;
            this.tweenType = tweenType;
            this._vector3Getter = getter;
            this._vector3Setter = setter;
        }
        public PlugDataGlobalTween(string guid, string label, DOGetter<Vector4> getter, DOSetter<Vector4> setter, PluginTweenType tweenType = PluginTweenType.SelfDetermined)
        {
            propertyType = DOVisualSequenced.PropertyType.Vector4;
            this.guid = guid;
            this.label = label;
            this.tweenType = tweenType;
            this._vector4Getter = getter;
            this._vector4Setter = setter;
        }
        public PlugDataGlobalTween(string guid, string label, DOGetter<Quaternion> getter, DOSetter<Quaternion> setter, PluginTweenType tweenType = PluginTweenType.SelfDetermined)
        {
            propertyType = DOVisualSequenced.PropertyType.Quaternion;
            this.guid = guid;
            this.label = label;
            this.tweenType = tweenType;
            this._quaternionGetter = getter;
            this._quaternionSetter = setter;
        }
        public PlugDataGlobalTween(string guid, string label, DOGetter<Color> getter, DOSetter<Color> setter, PluginTweenType tweenType = PluginTweenType.SelfDetermined)
        {
            propertyType = DOVisualSequenced.PropertyType.Color;
            this.guid = guid;
            this.label = label;
            this.tweenType = tweenType;
            this._colorGetter = getter;
            this._colorSetter = setter;
        }
        public PlugDataGlobalTween(string guid, string label, DOGetter<Rect> getter, DOSetter<Rect> setter, PluginTweenType tweenType = PluginTweenType.SelfDetermined)
        {
            propertyType = DOVisualSequenced.PropertyType.Rect;
            this.guid = guid;
            this.label = label;
            this.tweenType = tweenType;
            this._rectGetter = getter;
            this._rectSetter = setter;
        }

        public DOGetter<float> FloatGetter(object target = null, string strVal = null, int intVal = 0)
        { return _floatGetter; }
        public DOSetter<float> FloatSetter(object target = null, string strVal = null, int intVal = 0)
        { return _floatSetter; }
        public DOGetter<int> IntGetter(object target = null, string strVal = null, int intVal = 0)
        { return _intGetter; }
        public DOSetter<int> IntSetter(object target = null, string strVal = null, int intVal = 0)
        { return _intSetter; }
        public DOGetter<uint> UintGetter(object target = null, string strVal = null, int intVal = 0)
        { return _uintGetter; }
        public DOSetter<uint> UintSetter(object target = null, string strVal = null, int intVal = 0)
        { return _uintSetter; }
        public DOGetter<string> StringGetter(object target = null, string strVal = null, int intVal = 0)
        { return _stringGetter; }
        public DOSetter<string> StringSetter(object target = null, string strVal = null, int intVal = 0)
        { return _stringSetter; }
        public DOGetter<Vector2> Vector2Getter(object target = null, string strVal = null, int intVal = 0)
        { return _vector2Getter; }
        public DOSetter<Vector2> Vector2Setter(object target = null, string strVal = null, int intVal = 0)
        { return _vector2Setter; }
        public DOGetter<Vector3> Vector3Getter(object target = null, string strVal = null, int intVal = 0)
        { return _vector3Getter; }
        public DOSetter<Vector3> Vector3Setter(object target = null, string strVal = null, int intVal = 0)
        { return _vector3Setter; }
        public DOGetter<Vector4> Vector4Getter(object target = null, string strVal = null, int intVal = 0)
        { return _vector4Getter; }
        public DOSetter<Vector4> Vector4Setter(object target = null, string strVal = null, int intVal = 0)
        { return _vector4Setter; }
        public DOGetter<Quaternion> QuaternionGetter(object target = null, string strVal = null, int intVal = 0)
        { return _quaternionGetter; }
        public DOSetter<Quaternion> QuaternionSetter(object target = null, string strVal = null, int intVal = 0)
        { return _quaternionSetter; }
        public DOGetter<Color> ColorGetter(object target = null, string strVal = null, int intVal = 0)
        { return _colorGetter; }
        public DOSetter<Color> ColorSetter(object target = null, string strVal = null, int intVal = 0)
        { return _colorSetter; }
        public DOGetter<Rect> RectGetter(object target = null, string strVal = null, int intVal = 0)
        { return _rectGetter; }
        public DOSetter<Rect> RectSetter(object target = null, string strVal = null, int intVal = 0)
        { return _rectSetter; }
    }
}