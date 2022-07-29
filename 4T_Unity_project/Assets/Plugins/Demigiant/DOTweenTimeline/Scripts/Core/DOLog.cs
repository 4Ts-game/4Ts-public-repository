// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/01/22

using UnityEngine;

namespace DG.Tweening.Timeline.Core
{
    public static class DOLog
    {
        public static void Normal(object message, Object context = null)
        {
            Debug.Log(message, context);
        }

        public static void Warning(object message, Object context = null)
        {
            Debug.LogWarning(message, context);
        }

        public static void Error(object message, Object context = null)
        {
            Debug.LogError(message, context);
        }

        public static void DebugDev(object message, Object context = null)
        {
            Debug.Log(string.Format("<color=#f68000>DOTweenTimeline DEV-DEBUG ►</color> {0}", message), context);
        }
    }
}