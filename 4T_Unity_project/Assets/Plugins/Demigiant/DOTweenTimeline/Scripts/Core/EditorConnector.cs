// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/02/08

#if UNITY_EDITOR
using System;
using UnityEngine.Events;

namespace DG.Tweening.Timeline.Core
{
    public static class EditorConnector
    {
        // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████
        // ███ INTERNAL CLASSES ████████████████████████████████████████████████████████████████████████████████████████████████
        // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

        public static class Request
        {
            public static event Func<UnityEvent, UnityEvent> CloneUnityEvent;

            public static UnityEvent Dispatch_OnCloneUnityEvent(UnityEvent unityEvent)
            { return CloneUnityEvent != null ? CloneUnityEvent(unityEvent) : null; }
        }
    }
}
#endif