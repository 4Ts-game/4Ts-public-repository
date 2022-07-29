// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2019/03/25 21:53
// License Copyright (c) Daniele Giardini

using DG.DeebugLib.GUISystems;
using DG.DeebugLib.RuntimeConsole;
using UnityEngine;

namespace DG.DeebugLib
{
    public static class Deebug
    {
        public const string Version = "1.0.163";
        public static bool isConsoleActive { get { return DeeConsole.isActive; } }
        public static bool isConsoleOpen { get { return DeeConsole.isOpen; } }

        internal static DeebugOptions options { get; private set; }
        static bool _initialized;

        #region Constructor + INIT

        static Deebug()
        {
            Init();
        }

        internal static void Init()
        {
            if (_initialized) return;

            _initialized = true;
            if (Application.isPlaying) {
                options = DeebugOptions.Init();
                DeeGUI.RefreshGUIScale();
            }
        }

        #endregion
        
        #region Public Methods

        /// <summary>
        /// Activates both the console and the log tracking.
        /// If optional parameters are not passed the values set in Deebug's editor panel will be used<para/>
        /// You can use chaining to append these extra options:<para/>
        /// - <code>AddExtraReportInfo(info)</code>: adds the given info after the log report when copying it to clipboard<para/>
        /// - <code>SetMailTo(options)</code>: choose whether to activate the mail button and its data<para/>
        /// - <code>SetViewOptions(options)</code>: sets the default view options<para/>
        /// - <code>ShowPopupOnError(options)</code>: choose whether to open a popup on error<para/>
        /// </summary>
        /// <param name="toggleOpenKey"><code>KeyCode</code> to use to toggle the console window opened/closed.
        /// Pass it a value of <see cref="KeyCode.None"/> to disable key-based toggling</param>
        /// <param name="toggleOpenGesture">Eventual mouse/touch gesture that can be drawn to toggle the console window opened/closed</param>
        /// <param name="gestureTolerance">Tolerance of eventual gesture drawing</param>
        /// <param name="autoOpenOnError">If TRUE the console opens automatically when there's an error</param>
        public static DeeConsole EnableConsole(
            KeyCode? toggleOpenKey = null, DeeConsole.Gesture? toggleOpenGesture = null,
            DeeGesture.GestureTolerance? gestureTolerance = null,
            bool? autoOpenOnError = null
        ){
            return DeeConsole.Activate(toggleOpenKey, toggleOpenGesture, gestureTolerance, autoOpenOnError);
        }

        #endregion
    }
}