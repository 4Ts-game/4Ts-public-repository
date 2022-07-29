// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/08/07

using DG.Tweening.Timeline.Core;
using UnityEditor;

namespace DG.Tweening.TimelineEditor
{
    /// <summary>
    /// Session-only editor data
    /// </summary>
    [InitializeOnLoad]
    internal static class TimelineSession
    {
        public static bool isDevDebugMode {
            get { return _showSequencedsPlugDataIndexAndGuid || _logMissingPlugDataGuidAssignment; }
        }

        public static bool showSequencedsPlugDataIndexAndGuid {
            get { return _showSequencedsPlugDataIndexAndGuid; }
            set { SetDebugModeSessionVar("ShowSequencedsPlugDataIndexAndGuid", value, ref _showSequencedsPlugDataIndexAndGuid); }
        }
        static bool _showSequencedsPlugDataIndexAndGuid;

        public static bool logMissingPlugDataGuidAssignment {
            get { return _logMissingPlugDataGuidAssignment; }
            set { SetDebugModeSessionVar("LogMissingPlugDataGuidAssignment", value, ref _logMissingPlugDataGuidAssignment); }
        }
        static bool _logMissingPlugDataGuidAssignment;

        const string _Prefix = "DOTweenTimelineDebugMode";

        static TimelineSession()
        {
            _showSequencedsPlugDataIndexAndGuid = SessionState.GetBool(_Prefix + "ShowSequencedsPlugDataIndexAndGuid", false);
            _logMissingPlugDataGuidAssignment = SessionState.GetBool(_Prefix + "LogMissingPlugDataGuidAssignment", false);
        }

        #region Public Methods

        public static void ToggleAll(bool toggleOn)
        {
            showSequencedsPlugDataIndexAndGuid = toggleOn;
            logMissingPlugDataGuidAssignment = toggleOn;
        }

        #endregion

        #region Methods

        static void SetDebugModeSessionVar(string id, bool value, ref bool var)
        {
            if (value == var) return;
            bool wasDevDebugModeActive = isDevDebugMode;
            SessionState.SetBool(_Prefix + id, value);
            var = value;
            if (wasDevDebugModeActive != isDevDebugMode) {
                DOLog.DebugDev("Developer Debug Mode ► " + (isDevDebugMode ? "<color=#00ff00>ACTIVATED</color>" : "<color=#ff0000>DEACTIVATED</color>"));
            }
        }

        #endregion

    }
}