// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2019/03/28 16:48
// License Copyright (c) Daniele Giardini

using System;
using DG.DeebugLib.RuntimeConsole;
using UnityEngine;

namespace DG.DeebugLib
{
    public class DeebugOptions : ScriptableObject
    {
        #region Serialized
#pragma warning disable 0649

        public ConsoleOptions console = new ConsoleOptions();
        public bool extraHelpInfo = true;

#pragma warning restore 0649
        #endregion

        static DeebugOptions I;
        public const string ResourceId = "DeebugOptions";
        static bool _initialized;

        #region INIT

        internal static DeebugOptions Init()
        {
            if (_initialized) return I;

            _initialized = true;
            I = Resources.Load<DeebugOptions>(ResourceId);
            if (I == null) {
                // No options setup. No biggie: continue without defaults
                return null;
            }
            return I;
        }

        #endregion

        // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████
        // ███ INTERNAL CLASSES ████████████████████████████████████████████████████████████████████████████████████████████████
        // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

        [Serializable]
        public class ConsoleOptions
        {
            // indexes are: 0:normal, 1:warnings, 2:errors
            public bool[] trackedLogTypes = new[] { true, true, true };
            // indexes are: 0:normal, 1:warnings, 2:errors
            public bool[] logTypesVisibility = new[] { true, true, true };
            public bool useBigFonts = false;
            public bool showTimeLabel = true;
            public bool showSceneLabel = false;
            public bool advancedStackTrace = true;
            public string extraReportInfo = "";
            public bool showPopupOnError = false;
            public string errorPopupMessage = "Oops, <b>an error happened</b>!!!" +
                                              "\n\nPlease send the log to the developers";
            public bool canSendMail;
            public string mailAddress = "";
            public string mailSubject = "Game log report";

            // Basic
            public KeyCode toggleOpenKey = KeyCode.F8;
            public DeeConsole.Gesture toggleOpenGesture = DeeConsole.Gesture.Circle;
            public DeeGesture.GestureTolerance gestureTolerance = DeeGesture.GestureTolerance.Tolerant;
            public bool autoOpenOnError = true;
        }
    }
}