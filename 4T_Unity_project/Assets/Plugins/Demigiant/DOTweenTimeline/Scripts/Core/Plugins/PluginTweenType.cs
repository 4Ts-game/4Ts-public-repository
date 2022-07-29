// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/01/31

namespace DG.Tweening.Timeline.Core.Plugins
{
    public enum PluginTweenType
    {
        /// <summary>Default applied</summary>
        SelfDetermined,
        /// <summary>Only works with Vector3 values</summary>
        Punch,
        /// <summary>Only works with Vector3 values</summary>
        Shake,
        /// <summary>Displays the extra <see cref="DOVisualSequenced.stringOption0"/> property and assign it to running plugins</summary>
        StringOption,
        /// <summary>Displays the extra <see cref="DOVisualSequenced.intOption1"/> property and assign it to running plugins</summary>
        IntOption
    }
}