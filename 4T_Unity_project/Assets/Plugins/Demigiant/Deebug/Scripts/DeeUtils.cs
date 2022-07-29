// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2019/03/22 20:52
// License Copyright (c) Daniele Giardini

using UnityEngine;

namespace DG.DeebugLib
{
    internal static class DeeUtils
    {
        #region Public Methods

        /// <summary>
        /// Returns the gameView scale if playing inside the editor, otherwise 1
        /// </summary>
        public static float GetGameViewScale()
        {
#if UNITY_EDITOR
            System.Reflection.Assembly assembly = typeof(UnityEditor.EditorWindow).Assembly;
            System.Type type = assembly.GetType("UnityEditor.GameView");
            UnityEditor.EditorWindow gameViewWin = UnityEditor.EditorWindow.GetWindow(type);
            var zoomArea = type.GetField("m_ZoomArea", System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic);
            var zoomAreaObj = zoomArea.GetValue(gameViewWin);
            var scaleField = zoomAreaObj.GetType().GetField("m_Scale", System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic);
            return ((Vector2)scaleField.GetValue(zoomAreaObj)).x;
#else
            return 1;
#endif
        }

        /// <summary>
        /// Calculates and returns DPI more correctly than Unity's Scree.dpi
        /// </summary>
        public static float GetDPI()
        {
#if PLATFORM_ANDROID
            if (Application.isEditor) return Screen.dpi;
            // Unity suggested best method for Android
            AndroidJavaClass activityClass = new AndroidJavaClass("com.unity3d.player.UnityPlayer");
            AndroidJavaObject activity = activityClass.GetStatic<AndroidJavaObject>("currentActivity");
            AndroidJavaObject metrics = new AndroidJavaObject("android.util.DisplayMetrics");
            activity.Call<AndroidJavaObject>("getWindowManager").Call<AndroidJavaObject>("getDefaultDisplay").Call("getMetrics", metrics);
            return (metrics.Get<float>("xdpi") + metrics.Get<float>("ydpi")) * 0.5f;
#else
            return Screen.dpi;
#endif
        }

        #endregion
    }
}