// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/01/16

using System;
using DG.Tweening.Timeline.Core.Plugins;
using UnityEngine;
using UnityEngine.Events;
using Object = UnityEngine.Object;

namespace DG.Tweening.Timeline.Core
{
    /// <summary>
    /// This represents a <see cref="DOVisualSequence"/> internal tween/action.<para/>
    /// If you're trying to add a Timeline sequence to your Component this is not the right class, it's <see cref="DOVisualSequence"/>
    /// </summary>
    [Serializable]
    public class DOVisualSequenced
    {
        public enum Type
        {
            Tween,
            GlobalTween, // Tweens that act on static properties like Time.timeScale
            Event,
            [Obsolete("Here for later usage but can't be used right now", true)]
            Sequence, // Not implemented but here for future usage, maybe
            Action, // Can be global or with target
            Interval
        }
        public enum ToFromType
        {
            Dynamic, Direct
        }
        public enum PropertyType
        {
            Unset, Float, Int, Uint, String, Vector2, Vector3, Vector4, Quaternion, Color, Rect
        }

        #region Serialized
#pragma warning disable 0649

        public Type type;
        [SerializeField] string _guid;
        public int pin; // If <=0 considers it unpinned
        public bool isActive = true; // Set via editor when the sequenced's layer is disabled
        public float startTime;
        public float duration = 1;
        public int loops = 1; // can't have infinite loops
        public LoopType loopType = LoopType.Restart;
        public Object target;
        public Ease ease = Ease.OutQuad;
        public AnimationCurve easeCurve = new AnimationCurve(new Keyframe(0, 0), new Keyframe(1, 1));
        public float overshootOrAmplitude = DOTween.defaultEaseOvershootOrAmplitude;
        public float period = DOTween.defaultEasePeriod;
        public ToFromType toType = ToFromType.Direct;
        public ToFromType fromType = ToFromType.Dynamic;
        public string plugId; // Used by global tweens/actions to store their subcategory 
        public int plugDataIndex = 0; // Legacy
        public string plugDataGuid = null;
        public bool isRelative = false;
        public AxisConstraint axisConstraint = AxisConstraint.None;
        /// <summary>Used for snapping for float/Vector, richTextEnabled for String, alpha-only for Color</summary>
        public bool boolOption0 = false;
        /// <summary>Used for scramble option for String, RotateMode for Rotations, fadeOut for Shake</summary>
        public int intOption0 = 0;
        /// <summary>Used as ScrambleMode for String, vibrato for Punch/Shake and by <see cref="PluginTweenType.IntOption"/> plugins</summary>
        public int intOption1 = 10;
        /// <summary>Used for elasticity for Punch, randomness for Shake tweenType</summary>
        public float floatOption0 = 1;
        /// <summary>Used only by Actions</summary>
        public float floatOption1 = 1;
        /// <summary>Used as scramble chars for String and by <see cref="PluginTweenType.StringOption"/> plugins</summary>
        public string stringOption0;
        public float toFloatVal = 0; public float fromFloatVal = 0;
        public int toIntVal = 0; public int fromIntVal = 0;
        public uint fromUintVal = 0; public uint toUintVal = 0;
        public string toStringVal, fromStringVal;
        public Vector2 fromVector2Val = Vector2.zero, toVector2Val = Vector2.zero;
        public Vector3 fromVector3Val = Vector3.zero, toVector3Val = Vector3.zero;
        public Vector4 fromVector4Val = Vector4.zero, toVector4Val = Vector4.zero;
        public Color fromColorVal = Color.white, toColorVal = Color.white;
        public Rect fromRectVal, toRectVal;
        // Events
        public UnityEvent onComplete, onStepComplete, onUpdate;
        // Editor-only
        public bool editor_lockVector = false; // Locks the vector editing to a single axis in the editor

#pragma warning restore 0649
        #endregion

        public string guid { get { return _guid; } }

        #region CONSTRUCTOR

        /// <summary>Use this constructor when creating a new sequenced object, so that you can pass a unique GUID</summary>
        public DOVisualSequenced(string guid, Type type, float startTime)
        {
            _guid = guid;
            this.type = type;
            this.startTime = startTime;
        }

        #endregion

#if UNITY_EDITOR
        #region Editor-Only

        public void Editor_RegenerateGuid()
        {
            _guid = Guid.NewGuid().ToString();
        }

        /// <summary>
        /// Editor-only. Clones this sequenced
        /// </summary>
        public DOVisualSequenced Editor_Clone(bool regenerateGuid)
        {
            return new DOVisualSequenced(
                regenerateGuid ? Guid.NewGuid().ToString() : this.guid,
                this.type, this.startTime
            ) {
                isActive = this.isActive,
                duration = this.duration,
                loops = this.loops,
                loopType = this.loopType,
                target = this.target,
                ease = this.ease,
                easeCurve = this.easeCurve,
                overshootOrAmplitude = this.overshootOrAmplitude,
                period = this.period,
                toType = this.toType,
                fromType = this.fromType,
                plugId = this.plugId,
                plugDataGuid =  this.plugDataGuid,
                plugDataIndex = this.plugDataIndex,
                isRelative = this.isRelative,
                axisConstraint = this.axisConstraint,
                boolOption0 = this.boolOption0,
                intOption0 = this.intOption0,
                intOption1 = this.intOption1,
                floatOption0 = this.floatOption0,
                floatOption1 = this.floatOption1,
                stringOption0 = this.stringOption0,
                toFloatVal = this.toFloatVal,
                fromFloatVal = this.fromFloatVal,
                toIntVal = this.toIntVal,
                fromIntVal = this.fromIntVal,
                toUintVal = this.toUintVal,
                fromUintVal = this.fromUintVal,
                toStringVal = this.toStringVal,
                fromStringVal = this.fromStringVal,
                toVector2Val = this.toVector2Val,
                fromVector2Val = this.fromVector2Val,
                toVector3Val = this.toVector3Val,
                fromVector3Val = this.fromVector3Val,
                toVector4Val = this.toVector4Val,
                fromVector4Val = this.fromVector4Val,
                toColorVal = this.toColorVal,
                fromColorVal = this.fromColorVal,
                toRectVal = this.toRectVal,
                fromRectVal = this.fromRectVal,
                onComplete = EditorConnector.Request.Dispatch_OnCloneUnityEvent(this.onComplete),
                onStepComplete = EditorConnector.Request.Dispatch_OnCloneUnityEvent(this.onStepComplete),
                onUpdate = EditorConnector.Request.Dispatch_OnCloneUnityEvent(this.onUpdate),
                editor_lockVector = this.editor_lockVector
            };
        }

        #endregion
#endif
    }
}