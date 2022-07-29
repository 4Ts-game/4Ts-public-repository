// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/01/22

using UnityEngine;

namespace DG.Tweening.Timeline.Core
{
    public class DOVisualSequenceSettings : ScriptableObject
    {
        public const string Version = "0.9.406";
        public const string ResourcePath = "DOVisualSequenceSettings";

        #region Serialized
#pragma warning disable 0649

        public bool foo_debugLogs;

#pragma warning restore 0649
        #endregion

#if UNITY_EDITOR
        static DOVisualSequenceSettings()
        {
            UnityEditor.EditorApplication.playModeStateChanged += Editor_OnPlayModeStateChanged;
        }
        static void Editor_OnPlayModeStateChanged(UnityEditor.PlayModeStateChange obj)
        {
            _foo_isApplicationPlayingSet = false;
        }
#endif

        public static DOVisualSequenceSettings I { get {
            if (_instance == null) _instance = Resources.Load<DOVisualSequenceSettings>(ResourcePath);
            return _instance;
        }}
        static DOVisualSequenceSettings _instance;
        public bool debugLogs { get { return foo_debugLogs && isApplicationPlaying; } }
        public static bool isApplicationPlaying {
            get {
                if (!_foo_isApplicationPlayingSet) {
                    _foo_isApplicationPlaying = Application.isPlaying;
                    _foo_isApplicationPlayingSet = true;
                }
                return _foo_isApplicationPlaying;
            }
        }
        static bool _foo_isApplicationPlaying;
        static bool _foo_isApplicationPlayingSet;
    }
}