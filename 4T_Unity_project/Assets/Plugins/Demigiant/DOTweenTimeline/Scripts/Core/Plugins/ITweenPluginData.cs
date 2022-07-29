// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/01/31

using DG.Tweening.Core;
using UnityEngine;

namespace DG.Tweening.Timeline.Core.Plugins
{
    public interface ITweenPluginData : IPluginData
    {
        PluginTweenType tweenType { get; }

        DOGetter<float> FloatGetter(object target = null, string strVal = null, int intVal = 0);
        DOSetter<float> FloatSetter(object target = null, string strVal = null, int intVal = 0);
        DOGetter<int> IntGetter(object target = null, string strVal = null, int intVal = 0);
        DOSetter<int> IntSetter(object target = null, string strVal = null, int intVal = 0);
        DOGetter<uint> UintGetter(object target = null, string strVal = null, int intVal = 0);
        DOSetter<uint> UintSetter(object target = null, string strVal = null, int intVal = 0);
        DOGetter<string> StringGetter(object target = null, string strVal = null, int intVal = 0);
        DOSetter<string> StringSetter(object target = null, string strVal = null, int intVal = 0);
        DOGetter<Vector2> Vector2Getter(object target = null, string strVal = null, int intVal = 0);
        DOSetter<Vector2> Vector2Setter(object target = null, string strVal = null, int intVal = 0);
        DOGetter<Vector3> Vector3Getter(object target = null, string strVal = null, int intVal = 0);
        DOSetter<Vector3> Vector3Setter(object target = null, string strVal = null, int intVal = 0);
        DOGetter<Vector4> Vector4Getter(object target = null, string strVal = null, int intVal = 0);
        DOSetter<Vector4> Vector4Setter(object target = null, string strVal = null, int intVal = 0);
        DOGetter<Quaternion> QuaternionGetter(object target = null, string strVal = null, int intVal = 0);
        DOSetter<Quaternion> QuaternionSetter(object target = null, string strVal = null, int intVal = 0);
        DOGetter<Color> ColorGetter(object target = null, string strVal = null, int intVal = 0);
        DOSetter<Color> ColorSetter(object target = null, string strVal = null, int intVal = 0);
        DOGetter<Rect> RectGetter(object target = null, string strVal = null, int intVal = 0);
        DOSetter<Rect> RectSetter(object target = null, string strVal = null, int intVal = 0);
    }
}