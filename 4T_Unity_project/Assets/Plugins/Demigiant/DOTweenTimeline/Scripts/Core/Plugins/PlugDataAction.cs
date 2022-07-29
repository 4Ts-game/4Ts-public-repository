// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/02/01

using System;
using Object = UnityEngine.Object;

namespace DG.Tweening.Timeline.Core.Plugins
{
    public class PlugDataAction : IPluginData
    {
        public bool wantsTarget { get; private set; }
        public string guid { get; private set; }
        public string label { get; private set; }
        public string targetLabel { get; private set; }
        public string boolOptionLabel { get; private set; }
        public string stringOptionLabel { get; private set; }
        public string float0OptionLabel { get; private set; }
        public string float1OptionLabel { get; private set; }
        public string float2OptionLabel { get; private set; }
        public string float3OptionLabel { get; private set; }
        public string intOptionLabel { get; private set; }
        public DOVisualSequenced.PropertyType propertyType { get; private set; }

        public Type targetType { get; private set; } // NULL for actions that don't require a target
        public Type intOptionAsEnumType { get; private set; } // Set to eventually interpret intOption as enum
        public bool defBoolValue { get; private set; }
        public string defStringValue { get; private set; }
        public float defFloat0Value { get; private set; }
        public float defFloat1Value { get; private set; }
        public float defFloat2Value { get; private set; }
        public float defFloat3Value { get; private set; }
        public int defIntValue { get; private set; }
        public readonly Action<object, bool, string, float, float, float, float, int> action;
        public readonly Action<object, bool, string, float, float, float, float, int> onCreation;

        public PlugDataAction(
            string guid, string label, Type targetType,
            Action<object, bool, string, float, float, float, float, int> action,
            Action<object, bool, string, float, float, float, float, int> onCreation = null,
            string targetLabel = null,
            bool defBoolValue = false, string boolOptionLabel = null,
            string defStringValue = null, string stringOptionLabel = null,
            float defFloat0Value = 0, string float0OptionLabel = null,
            float defFloat1Value = 0, string float1OptionLabel = null,
            float defFloat2Value = 0, string float2OptionLabel = null,
            float defFloat3Value = 0, string float3OptionLabel = null,
            int defIntValue = 0, string intOptionLabel = null,
            Type intOptionAsEnumType = null
        ){
            this.guid = guid;
            this.targetType = targetType;
            this.wantsTarget = targetType != null;
            this.propertyType = DOVisualSequenced.PropertyType.Unset; // Unused
            this.label = label;
            this.targetLabel = targetLabel;
            this.defBoolValue = defBoolValue;
            this.boolOptionLabel = boolOptionLabel;
            this.defStringValue = defStringValue;
            this.stringOptionLabel = stringOptionLabel;
            this.defFloat0Value = defFloat0Value;
            this.float0OptionLabel = float0OptionLabel;
            this.defFloat1Value = defFloat1Value;
            this.float1OptionLabel = float1OptionLabel;
            this.defFloat2Value = defFloat2Value;
            this.float2OptionLabel = float2OptionLabel;
            this.defFloat3Value = defFloat3Value;
            this.float3OptionLabel = float3OptionLabel;
            this.defIntValue = defIntValue;
            this.intOptionLabel = intOptionLabel;
            this.intOptionAsEnumType = intOptionAsEnumType;
            this.action = action;
            this.onCreation = onCreation;
        }
    }
}