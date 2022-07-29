// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/02/01

using System;
using DG.Tweening.Core;
using UnityEngine;

namespace DG.Tweening.Timeline.Core.Plugins
{
    public class PlugDataTween : ITweenPluginData
    {
        public bool wantsTarget { get { return true; } }
        public string guid { get; private set; }
        public string label { get; private set; }
        public string targetLabel { get { return "Target"; } }
        public string stringOptionLabel { get; private set; }
        public string intOptionLabel { get; private set; }
        public DOVisualSequenced.PropertyType propertyType { get; private set; }
        public PluginTweenType tweenType { get; private set; }

        readonly Func<object, string, int, DOGetter<float>> _floatGetterGen;
        readonly Func<object, string, int, DOSetter<float>> _floatSetterGen;
        readonly Func<object, string, int, DOGetter<int>> _intGetterGen;
        readonly Func<object, string, int, DOSetter<int>> _intSetterGen;
        readonly Func<object, string, int, DOGetter<uint>> _uintGetterGen;
        readonly Func<object, string, int, DOSetter<uint>> _uintSetterGen;
        readonly Func<object, string, int, DOGetter<string>> _stringGetterGen;
        readonly Func<object, string, int, DOSetter<string>> _stringSetterGen;
        readonly Func<object, string, int, DOGetter<Vector2>> _vector2GetterGen;
        readonly Func<object, string, int, DOSetter<Vector2>> _vector2SetterGen;
        readonly Func<object, string, int, DOGetter<Vector3>> _vector3GetterGen;
        readonly Func<object, string, int, DOSetter<Vector3>> _vector3SetterGen;
        readonly Func<object, string, int, DOGetter<Vector4>> _vector4GetterGen;
        readonly Func<object, string, int, DOSetter<Vector4>> _vector4SetterGen;
        readonly Func<object, string, int, DOGetter<Quaternion>> _quaternionGetterGen;
        readonly Func<object, string, int, DOSetter<Quaternion>> _quaternionSetterGen;
        readonly Func<object, string, int, DOGetter<Color>> _colorGetterGen;
        readonly Func<object, string, int, DOSetter<Color>> _colorSetterGen;
        readonly Func<object, string, int, DOGetter<Rect>> _rectGetterGen;
        readonly Func<object, string, int, DOSetter<Rect>> _rectSetterGen;

        public PlugDataTween(
            string guid, string label, Func<object, string, int, DOGetter<float>> getterGen, Func<object, string, int, DOSetter<float>> setterGen,
            PluginTweenType tweenType = PluginTweenType.SelfDetermined, string stringOptionLabel = null, string intOptionLabel = null
        ){
            propertyType = DOVisualSequenced.PropertyType.Float;
            this.guid = guid;
            this.label = label;
            this.stringOptionLabel = stringOptionLabel;
            this.intOptionLabel = intOptionLabel;
            this.tweenType = tweenType;
            this._floatGetterGen = getterGen;
            this._floatSetterGen = setterGen;
        }
        public PlugDataTween(
            string guid, string label, Func<object, string, int, DOGetter<int>> getterGen, Func<object, string, int, DOSetter<int>> setterGen,
            PluginTweenType tweenType = PluginTweenType.SelfDetermined, string stringOptionLabel = null, string intOptionLabel = null
        ){
            propertyType = DOVisualSequenced.PropertyType.Int;
            this.guid = guid;
            this.label = label;
            this.stringOptionLabel = stringOptionLabel;
            this.intOptionLabel = intOptionLabel;
            this.tweenType = tweenType;
            this._intGetterGen = getterGen;
            this._intSetterGen = setterGen;
        }
        public PlugDataTween(
            string guid, string label, Func<object, string, int, DOGetter<uint>> getterGen, Func<object, string, int, DOSetter<uint>> setterGen,
            PluginTweenType tweenType = PluginTweenType.SelfDetermined, string stringOptionLabel = null, string intOptionLabel = null
        ){
            propertyType = DOVisualSequenced.PropertyType.Uint;
            this.guid = guid;
            this.label = label;
            this.stringOptionLabel = stringOptionLabel;
            this.intOptionLabel = intOptionLabel;
            this.tweenType = tweenType;
            this._uintGetterGen = getterGen;
            this._uintSetterGen = setterGen;
        }
        public PlugDataTween(
            string guid, string label, Func<object, string, int, DOGetter<string>> getterGen, Func<object, string, int, DOSetter<string>> setterGen,
            PluginTweenType tweenType = PluginTweenType.SelfDetermined, string stringOptionLabel = null, string intOptionLabel = null
        ){
            propertyType = DOVisualSequenced.PropertyType.String;
            this.guid = guid;
            this.label = label;
            this.stringOptionLabel = stringOptionLabel;
            this.intOptionLabel = intOptionLabel;
            this.tweenType = tweenType;
            this._stringGetterGen = getterGen;
            this._stringSetterGen = setterGen;
        }
        public PlugDataTween(
            string guid, string label, Func<object, string, int, DOGetter<Vector2>> getterGen, Func<object, string, int, DOSetter<Vector2>> setterGen,
            PluginTweenType tweenType = PluginTweenType.SelfDetermined, string stringOptionLabel = null, string intOptionLabel = null
        ){
            propertyType = DOVisualSequenced.PropertyType.Vector2;
            this.guid = guid;
            this.label = label;
            this.stringOptionLabel = stringOptionLabel;
            this.intOptionLabel = intOptionLabel;
            this.tweenType = tweenType;
            this._vector2GetterGen = getterGen;
            this._vector2SetterGen = setterGen;
        }
        public PlugDataTween(
            string guid, string label, Func<object, string, int, DOGetter<Vector3>> getterGen, Func<object, string, int, DOSetter<Vector3>> setterGen,
            PluginTweenType tweenType = PluginTweenType.SelfDetermined, string stringOptionLabel = null, string intOptionLabel = null
        ){
            propertyType = DOVisualSequenced.PropertyType.Vector3;
            this.guid = guid;
            this.label = label;
            this.stringOptionLabel = stringOptionLabel;
            this.intOptionLabel = intOptionLabel;
            this.tweenType = tweenType;
            this._vector3GetterGen = getterGen;
            this._vector3SetterGen = setterGen;
        }
        public PlugDataTween(
            string guid, string label, Func<object, string, int, DOGetter<Vector4>> getterGen, Func<object, string, int, DOSetter<Vector4>> setterGen,
            PluginTweenType tweenType = PluginTweenType.SelfDetermined, string stringOptionLabel = null, string intOptionLabel = null
        ){
            propertyType = DOVisualSequenced.PropertyType.Vector4;
            this.guid = guid;
            this.label = label;
            this.stringOptionLabel = stringOptionLabel;
            this.intOptionLabel = intOptionLabel;
            this.tweenType = tweenType;
            this._vector4GetterGen = getterGen;
            this._vector4SetterGen = setterGen;
        }
        public PlugDataTween(
            string guid, string label, Func<object, string, int, DOGetter<Quaternion>> getterGen, Func<object, string, int, DOSetter<Quaternion>> setterGen,
            PluginTweenType tweenType = PluginTweenType.SelfDetermined, string stringOptionLabel = null, string intOptionLabel = null
        ){
            propertyType = DOVisualSequenced.PropertyType.Quaternion;
            this.guid = guid;
            this.label = label;
            this.stringOptionLabel = stringOptionLabel;
            this.intOptionLabel = intOptionLabel;
            this.tweenType = tweenType;
            this._quaternionGetterGen = getterGen;
            this._quaternionSetterGen = setterGen;
        }
        public PlugDataTween(
            string guid, string label, Func<object, string, int, DOGetter<Color>> getterGen, Func<object, string, int, DOSetter<Color>> setterGen,
            PluginTweenType tweenType = PluginTweenType.SelfDetermined, string stringOptionLabel = null, string intOptionLabel = null
        ){
            propertyType = DOVisualSequenced.PropertyType.Color;
            this.guid = guid;
            this.label = label;
            this.stringOptionLabel = stringOptionLabel;
            this.intOptionLabel = intOptionLabel;
            this.tweenType = tweenType;
            this._colorGetterGen = getterGen;
            this._colorSetterGen = setterGen;
        }
        public PlugDataTween(
            string guid, string label, Func<object, string, int, DOGetter<Rect>> getterGen, Func<object, string, int, DOSetter<Rect>> setterGen,
            PluginTweenType tweenType = PluginTweenType.SelfDetermined, string stringOptionLabel = null, string intOptionLabel = null
        ){
            propertyType = DOVisualSequenced.PropertyType.Rect;
            this.guid = guid;
            this.label = label;
            this.stringOptionLabel = stringOptionLabel;
            this.intOptionLabel = intOptionLabel;
            this.tweenType = tweenType;
            this._rectGetterGen = getterGen;
            this._rectSetterGen = setterGen;
        }

        public DOGetter<float> FloatGetter(object target = null, string strVal = null, int intVal = 0)
        { return _floatGetterGen(target, strVal, intVal); }
        public DOSetter<float> FloatSetter(object target = null, string strVal = null, int intVal = 0)
        { return _floatSetterGen(target, strVal, intVal); }
        public DOGetter<int> IntGetter(object target = null, string strVal = null, int intVal = 0)
        { return _intGetterGen(target, strVal, intVal); }
        public DOSetter<int> IntSetter(object target = null, string strVal = null, int intVal = 0)
        { return _intSetterGen(target, strVal, intVal); }
        public DOGetter<uint> UintGetter(object target = null, string strVal = null, int intVal = 0)
        { return _uintGetterGen(target, strVal, intVal); }
        public DOSetter<uint> UintSetter(object target = null, string strVal = null, int intVal = 0)
        { return _uintSetterGen(target, strVal, intVal); }
        public DOGetter<string> StringGetter(object target = null, string strVal = null, int intVal = 0)
        { return _stringGetterGen(target, strVal, intVal); }
        public DOSetter<string> StringSetter(object target = null, string strVal = null, int intVal = 0)
        { return _stringSetterGen(target, strVal, intVal); }
        public DOGetter<Vector2> Vector2Getter(object target = null, string strVal = null, int intVal = 0)
        { return _vector2GetterGen(target, strVal, intVal); }
        public DOSetter<Vector2> Vector2Setter(object target = null, string strVal = null, int intVal = 0)
        { return _vector2SetterGen(target, strVal, intVal); }
        public DOGetter<Vector3> Vector3Getter(object target = null, string strVal = null, int intVal = 0)
        { return _vector3GetterGen(target, strVal, intVal); }
        public DOSetter<Vector3> Vector3Setter(object target = null, string strVal = null, int intVal = 0)
        { return _vector3SetterGen(target, strVal, intVal); }
        public DOGetter<Vector4> Vector4Getter(object target = null, string strVal = null, int intVal = 0)
        { return _vector4GetterGen(target, strVal, intVal); }
        public DOSetter<Vector4> Vector4Setter(object target = null, string strVal = null, int intVal = 0)
        { return _vector4SetterGen(target, strVal, intVal); }
        public DOGetter<Quaternion> QuaternionGetter(object target = null, string strVal = null, int intVal = 0)
        { return _quaternionGetterGen(target, strVal, intVal); }
        public DOSetter<Quaternion> QuaternionSetter(object target = null, string strVal = null, int intVal = 0)
        { return _quaternionSetterGen(target, strVal, intVal); }
        public DOGetter<Color> ColorGetter(object target = null, string strVal = null, int intVal = 0)
        { return _colorGetterGen(target, strVal, intVal); }
        public DOSetter<Color> ColorSetter(object target = null, string strVal = null, int intVal = 0)
        { return _colorSetterGen(target, strVal, intVal); }
        public DOGetter<Rect> RectGetter(object target = null, string strVal = null, int intVal = 0)
        { return _rectGetterGen(target, strVal, intVal); }
        public DOSetter<Rect> RectSetter(object target = null, string strVal = null, int intVal = 0)
        { return _rectSetterGen(target, strVal, intVal); }
    }
}