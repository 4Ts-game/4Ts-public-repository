// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2019/03/22 16:30
// License Copyright (c) Daniele Giardini

using System;
using DG.DeebugLib.GUISystems;
using UnityEngine;
using UnityEngine.EventSystems;
using UnityEngine.Networking;

namespace DG.DeebugLib.RuntimeConsole
{
    public class DeeConsole : MonoBehaviour
    {
        public enum Gesture
        {
            None,
            Circle
        }

        internal static bool isActive { get { return _component != null; } }
        internal static bool isOpen { get; private set; }
        internal static bool isAnyPopupOpen { get { return DeeConsolePopup.isOpen || DeeConsoleCopyPopup.isOpen; } }

        internal readonly LogDB logDB = new LogDB();
        internal bool showNormalLogs = true;
        internal bool showWarningLogs = true;
        internal bool showErrorLogs = true;
        internal bool showTimeLabels = true;
        internal bool showSceneLabel = false;
        internal bool showPopupOnError = false;
        internal string errorPopupMessage = "";
        internal bool canSendMail;
        internal string mailAddress = "";
        internal string mailSubject = "";

        // indexes are: 0:normal, 1:warnings, 2:errors
        internal bool[] trackedLogTypes = new[] { true, true, true };

        static DeeConsole _component;
        readonly DeeConsoleGUI _gui = new DeeConsoleGUI();
        bool _autoOpenOnError = true;
        KeyCode _toggleOpenKey = KeyCode.F8;
        Gesture _toggleOpenGesture = Gesture.Circle;
        DeeGesture.GestureTolerance _gestureTolerance;
        bool _hasToggleOpenKey, _hasToggleOpenGesture;
        bool _isCapturingGesture;
        float _timeScaleBeforeOpen;
        EventSystem _eventSysBeforeOpen;

        #region Constructor + INIT

        static DeeConsole()
        {
            Deebug.Init();
        }

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
        public static DeeConsole Activate(
            KeyCode? toggleOpenKey = null, Gesture? toggleOpenGesture = null,
            DeeGesture.GestureTolerance? gestureTolerance = null,
            bool? autoOpenOnError = null
        ){
            if (_component != null) {
                Debug.LogWarning("DeeConsole.Activate ► DeeConsole has already been activated");
                return _component;
            }

            // Create component
            _component = new GameObject("-[ DeeConsole ]-").AddComponent<DeeConsole>();
            // Store correct basic options
            KeyCode eval_toggleOpenKey = toggleOpenKey == null
                ? Deebug.options == null ? _component._toggleOpenKey : Deebug.options.console.toggleOpenKey
                : (KeyCode)toggleOpenKey;
            Gesture eval_toggleOpenGesture = toggleOpenGesture == null
                ? Deebug.options == null ? _component._toggleOpenGesture : Deebug.options.console.toggleOpenGesture
                : (Gesture)toggleOpenGesture;
            DeeGesture.GestureTolerance eval_gestureTolerance = gestureTolerance == null
                ? Deebug.options == null ? _component._gestureTolerance : Deebug.options.console.gestureTolerance
                : (DeeGesture.GestureTolerance)gestureTolerance;
            bool eval_autoOpenOnError = autoOpenOnError == null
                ? Deebug.options == null ? _component._autoOpenOnError : Deebug.options.console.autoOpenOnError
                : (bool)autoOpenOnError;
            // Set control methods
            _component.SetControlMethods(eval_toggleOpenKey, eval_toggleOpenGesture, eval_gestureTolerance, eval_autoOpenOnError);
            DontDestroyOnLoad(_component.gameObject);
            // Setup extra options
            if (Deebug.options != null) {
                DeebugOptions.ConsoleOptions options = Deebug.options.console;
                _component.showNormalLogs = options.logTypesVisibility[0];
                _component.showWarningLogs = options.logTypesVisibility[1];
                _component.showErrorLogs = options.logTypesVisibility[2];
                _component._autoOpenOnError = options.autoOpenOnError;
                _component.showTimeLabels = options.showTimeLabel;
                _component.showSceneLabel = options.showSceneLabel;
                Style.useBigFonts = options.useBigFonts;
                _component.trackedLogTypes = options.trackedLogTypes;
                LogData.extraReportInfo = options.extraReportInfo;
                _component.showPopupOnError = options.showPopupOnError;
                _component.errorPopupMessage = options.errorPopupMessage;
                _component.canSendMail = options.canSendMail;
                _component.mailAddress = options.mailAddress;
                _component.mailSubject = options.mailSubject;
                //
                _component.logDB.ChangeFilters(_component.showNormalLogs, _component.showWarningLogs, _component.showErrorLogs);
            }
            //
            return _component;
        }

        #endregion

        #region Unity

        void Awake()
        {
            isOpen = false;
            _gui.Init(this);
            Application.logMessageReceivedThreaded += OnLogMessageReceived;
        }

        void OnDestroy()
        {
            if (_component == this) _component = null;
            Application.logMessageReceivedThreaded -= OnLogMessageReceived;
        }

        void Update()
        {
            // Gestures
            if (_hasToggleOpenGesture) {
                if (_isCapturingGesture) {
                    if (Input.GetMouseButtonUp(0)) {
                        // Stop capturing gesture and evaluate
                        _isCapturingGesture = false;
                        if (DeeGesture.Circle.StopAndValidate(Input.mousePosition)) {
                            if (isOpen) Close();
                            else Open();
                        }
                    } else {
                        // Update capturing gesture
                        DeeGesture.Circle.Update(Input.mousePosition);
                    }
                } else if (!isAnyPopupOpen && Input.GetMouseButtonDown(0)) {
                    // Start capturing opening gesture
                    _isCapturingGesture = true;
                    DeeGesture.Circle.StartCapture(Input.mousePosition, _gestureTolerance);
                }
            }
            // Open/close toggle key
            if (_hasToggleOpenKey && !isAnyPopupOpen && Input.GetKeyDown(_toggleOpenKey)) {
                if (isOpen) Close();
                else Open();
            }
        }

        #region GUI

        void OnGUI()
        {
            if (!isOpen && _gui.closeComplete || Event.current.type == EventType.Layout) return;

            if (isAnyPopupOpen) GUI.enabled = false;
            _gui.BeginGUI();
            _gui.Draw_Toolbar();
            _gui.Draw_LogsList();
            if (isAnyPopupOpen) GUI.enabled = true;
            if (DeeConsoleCopyPopup.isOpen) DeeConsoleCopyPopup.Draw();
            if (DeeConsolePopup.isOpen) DeeConsolePopup.Draw();
        }
        
        #endregion

        #endregion

        #region Public Methods

        #region Chaining Setup Methods

        /// <summary>
        /// Adds the given info after the log report when copying it to clipboard
        /// </summary>
        /// <param name="info">Extra info to add</param>
        public DeeConsole AddExtraReportInfo(string info)
        {
            LogData.extraReportInfo = info;
            return this;
        }

        /// <summary>
        /// Sets the view options of the console
        /// </summary>
        /// <param name="showTimeAndFrame">If TRUE shows the time and frame label for each log, otherwise only for the selected one</param>
        /// <param name="showScene">If TRUE shows the scene label for each log, otherwise only for the selected one</param>
        /// <param name="bigFonts">If TRUE uses bigger fonts</param>
        public DeeConsole SetViewOptions(bool showTimeAndFrame = true, bool showScene = false, bool bigFonts = false)
        {
            showTimeLabels = showTimeAndFrame;
            showSceneLabel = showScene;
            Style.useBigFonts = bigFonts;
            return this;
        }

        /// <summary>
        /// Chooses whether to open a popup on error or not, and with what message
        /// </summary>
        /// <param name="open">If TRUE shows a popup otherwise not</param>
        /// <param name="message">Popup message</param>
        public DeeConsole ShowPopupOnError(bool open, string message)
        {
            showPopupOnError = open;
            errorPopupMessage = message;
            return this;
        }

        /// <summary>
        /// Activates the Mail button for evidenced logs.
        /// </summary>
        /// <param name="activate">If TRUE activates the mailto button, otherwise doesn't</param>
        /// <param name="address">Email address where the mail should be sent</param>
        /// <param name="subject">Email title</param>
        /// <returns></returns>
        public DeeConsole SetMailTo(bool activate, string address = null, string subject = null)
        {
            canSendMail = activate;
            mailAddress = address;
            mailSubject = subject;
            return this;
        }

        #endregion

        #endregion

        #region Methods

        internal void CopyToClipboard(LogData log)
        {
            DeeConsoleCopyPopup.Open(log);
        }

        internal void SendMail(LogData log)
        {
            string address = mailAddress;
            string subject = UnityWebRequest.EscapeURL(mailSubject);
            string body = UnityWebRequest.EscapeURL(log.ConverToMailFormat());
            Application.OpenURL(string.Format("mailto:{0}?subject={1}&body={2}", address, subject, body));
        }

        void Open()
        {
            if (isOpen) return;

            _isCapturingGesture = false;
            _timeScaleBeforeOpen = Time.timeScale;
            _eventSysBeforeOpen = EventSystem.current;
            Time.timeScale = 0;
            if (_eventSysBeforeOpen != null) _eventSysBeforeOpen.enabled = false;
            _gui.Open();
            isOpen = true;
        }

        internal void Close()
        {
            if (!isOpen) return;

            if (Mathf.Approximately(Time.timeScale, 0)) Time.timeScale = _timeScaleBeforeOpen;
            if (_eventSysBeforeOpen != null) _eventSysBeforeOpen.enabled = true;
            _gui.Close();
            isOpen = false;
        }

        void SetControlMethods(KeyCode toggleOpenKey, Gesture openGesture, DeeGesture.GestureTolerance gestureTolerance, bool autoOpenOnError)
        {
            _toggleOpenKey = toggleOpenKey;
            _hasToggleOpenKey = _toggleOpenKey != KeyCode.None;
            _toggleOpenGesture = openGesture;
            _gestureTolerance = gestureTolerance;
            _hasToggleOpenGesture = _toggleOpenGesture != Gesture.None;
            _autoOpenOnError = autoOpenOnError;
        }

        #endregion

        #region Callbacks

        void OnLogMessageReceived(string condition, string stacktrace, LogType type)
        {
            bool add;
            bool open = false;
            bool showErrorPopup = false;

            switch (type) {
            case LogType.Log:
                add = trackedLogTypes[0];
                break;
            case LogType.Warning:
                add = trackedLogTypes[1];
                break;
            default:
                add = trackedLogTypes[2];
                if (_autoOpenOnError && add && !isOpen) {
                    open = true;
                    if (showPopupOnError) showErrorPopup = true;
                }
                break;
            }

            if (add) {
                LogData addedLog = logDB.AddLog(type, condition, stacktrace);
                if (open && !logDB.isPaused) Open();
                if (showErrorPopup && !logDB.isPaused) {
                    if (canSendMail) DeeConsolePopup.Open(errorPopupMessage, DeeConsolePopup.PopupType.ErrorWithMailTo, ()=> SendMail(addedLog));
                    else DeeConsolePopup.Open(errorPopupMessage, DeeConsolePopup.PopupType.Error);
                }
            }
        }

        #endregion
    }
}