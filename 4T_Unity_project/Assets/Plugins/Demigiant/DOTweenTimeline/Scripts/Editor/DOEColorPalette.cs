// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/01/20

using DG.DemiLib;
using UnityEngine;

namespace DG.Tweening.TimelineEditor
{
    public class DOEColorPalette : DeColorPalette
    {
        public TimelineColors timeline = new TimelineColors();

        public class TimelineColors : DeColorPalette
        {
            public Color sTween, sGlobalTween, sTweenerLoop, sEvent, sAction, sInterval;

            public TimelineColors()
            {
                sTween = new Color(0.05f, 0.65f, 0.41f);
                sGlobalTween = new Color(0f, 0.56f, 1f);
                sTweenerLoop = new Color(0.05f, 0.65f, 0.41f, 0.4f);
                sEvent = new Color(0.55f, 0.55f, 0.42f);
                sAction = new Color(0.84f, 0.19f, 0.85f);
                sInterval = Color.cyan;
            }
        }
    }
}