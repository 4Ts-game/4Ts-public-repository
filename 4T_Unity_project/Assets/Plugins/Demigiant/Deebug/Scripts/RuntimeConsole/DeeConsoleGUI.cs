// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2019/03/22 17:05
// License Copyright (c) Daniele Giardini

using DG.DeebugLib.Extensions;
using DG.DeebugLib.GUISystems;
using DG.DeExtensions;
using DG.DemiLib;
using UnityEngine;

namespace DG.DeebugLib.RuntimeConsole
{
    internal class DeeConsoleGUI
    {
        public bool closeComplete { get; private set; }

        public static readonly Color EvidenceColor = new Color(0.11f, 0.38f, 0.52f);
        const int _ToolbarH = 40;
        const int _ToolbarToggleW = 44;
        const int _ToolbarSecondaryToggleW = 32;
        const int _LogRowH = 28;
        static readonly Color _TimeColor = new Color(0.11f, 0.38f, 0.52f);
        static readonly Color _FrameColor = new Color(0.41f, 0.3f, 0.12f);
        static readonly Color _SceneColor = new Color(0.44f, 0.22f, 0.45f);

        const float _MaxClickInterval = 0.45f;

        bool _initialized;
        DeeConsole _console;
        Rect _screenSafeArea, _toolbarArea, _logsListArea, _logEvidenceArea;
        DeeGUI.ScrollDragArea _logsScrollView, _logEvidenceScrollView;
        bool _hasLogEvidence;
        LogData _currLogEvidence;
        string _currLogEvidenceStackTrace;
        float _currLogEvidenceScrollY; // List position to go when clicking on evidence
        bool _waitingToCheckLogClick; // Used to capture clicks on logs
        LogData _pressedLog; // Used to capture clicks on logs
        float _pressTime;
        Vector2 _pressMouseP;
        string _versionStr;
        DeeGUI.TweenUnit _openTween;

        #region Public Methods

        public void Init(DeeConsole console)
        {
            if (_initialized) return;

            _initialized = true;
            _console = console;
            _logsScrollView = new DeeGUI.ScrollDragArea(true, false);
            _logEvidenceScrollView = new DeeGUI.ScrollDragArea(true, false);
            _openTween = new DeeGUI.TweenUnit(0.3f);
            closeComplete = true;
            _versionStr = "v" + Deebug.Version;

            // Listeners
            LogDB.OnNewFilteredLogAdded += OnNewFilteredLogAdded;
        }

        public void Open()
        {
            _openTween.Rewind();
            _openTween.PlayForward();
            closeComplete = false;
        }

        public void Close()
        {
            _hasLogEvidence = false;
            _waitingToCheckLogClick = false;
            _openTween.PlayBackwards(() => closeComplete = true);
        }

        #endregion

        #region GUI

        public void BeginGUI()
        {
            DeeGUI.BeginGUI();
            _screenSafeArea = Screen.safeArea;
            _toolbarArea = _screenSafeArea.SetHeight(_ToolbarH.ScaledInt());
            _logsListArea = _screenSafeArea.Shift(0, _toolbarArea.height, 0, -_toolbarArea.height);
            _logsListArea.height = (int)Mathf.Min(_logsListArea.height * 0.5f, _console.logDB.filteredLogs.Count * _LogRowH.ScaledInt());
            _logEvidenceArea = _screenSafeArea.ShiftYAndResize(_toolbarArea.height + _logsListArea.height);
            // Screen blocked
            GUI.Box(new Rect(0, 0, Screen.width, Screen.height), "");
            // Tweens
            if (_openTween.value < 1) {
                float viewH = _screenSafeArea.y + _toolbarArea.height + _logsListArea.height;
                float offset = viewH - viewH * _openTween.value;
                _toolbarArea.y -= offset;
                _logsListArea.y -= offset;
            }
        }

        public void Draw_Toolbar()
        {
            int btSize = 32.ScaledInt();
            int toggleW = _ToolbarToggleW.ScaledInt();
            int secondaryToggleW = _ToolbarSecondaryToggleW.ScaledInt();
            int dist = 6.ScaledInt();
            int togglesDist = 1.ScaledInt();
            Rect btCloseR = new Rect(_toolbarArea.x + dist, _toolbarArea.center.y - btSize * 0.5f, btSize, btSize);
            Rect btPauseR = new Rect(btCloseR.xMax + 8.ScaledInt(), btCloseR.y, btSize, btSize);
            Rect btDeleteR = new Rect(_toolbarArea.xMax - btSize - dist, _toolbarArea.center.y - btSize * 0.5f, btSize, btSize);
            Rect btErrorsR = new Rect(btDeleteR.x - toggleW - dist, _toolbarArea.y, toggleW, _toolbarArea.height - 1);
            if (!_console.trackedLogTypes[2]) btErrorsR = btErrorsR.SetWidth(0).Shift(toggleW + togglesDist, 0, 0, 0);
            Rect btWarningsR = btErrorsR.Shift(-toggleW - togglesDist, 0, 0, 0);
            if (!_console.trackedLogTypes[1]) btWarningsR = btWarningsR.SetWidth(0).Shift(toggleW + togglesDist, 0, 0, 0);
            Rect btNormalR = btWarningsR.Shift(-toggleW - togglesDist, 0, 0, 0);
            if (!_console.trackedLogTypes[0]) btNormalR = btNormalR.SetWidth(0).Shift(toggleW + togglesDist, 0, 0, 0);
            Rect btFontUpscaleR = btNormalR.Shift(-secondaryToggleW - 4.ScaledInt(), 0, 0, 0).SetWidth(secondaryToggleW);
            Rect btTimeR = btFontUpscaleR.Shift(-secondaryToggleW - togglesDist, 0, 0, 0);
            Rect btSceneR = btTimeR.Shift(-secondaryToggleW - togglesDist, 0, 0, 0);
            float versionW = 80.ScaledInt();
            Rect versionR = new Rect(btSceneR.x - versionW - dist, _toolbarArea.y, versionW, _toolbarArea.height);

            // Background
            Rect bgArea = _screenSafeArea.x > 0 || _screenSafeArea.y > 0
                ? new Rect(_screenSafeArea.x, 0, _screenSafeArea.width, _toolbarArea.yMax)
                : _toolbarArea;
            GUI.Box(bgArea, "", Style.Toolbar.bg);
            // Version (only if it fits on screen)
            if (_screenSafeArea.width >= 408.Scaled()) {
                GUI.Label(versionR, _versionStr, Style.Toolbar.versionLabel);
            }
            // Close button
            if (GUI.Button(btCloseR, "", Style.Toolbar.btClose)) {
                _console.Close();
                return;
            }
            // Pause button
            if (GUI.Button(btPauseR, "", _console.logDB.isPaused ? Style.Toolbar.btResume : Style.Toolbar.btPause)) {
                if (_console.logDB.isPaused) _console.logDB.Resume();
                else _console.logDB.Pause();
            }
            // Toggles - Secondary
            ToolbarImgToggle(btTimeR, ImgDB.IcoTime, ref _console.showTimeLabels);
            if (ToolbarImgToggle(btFontUpscaleR, ImgDB.IcoFontUpscale, ref Style.useBigFonts)) {
                // Refresh all styles with new font scaling
                Style.Init();
            }
            ToolbarImgToggle(btSceneR, ImgDB.IcoScene, ref _console.showSceneLabel);
            // Toggles - Primary
            ToolbarLogToggle(btNormalR, LogType.Log, ref _console.showNormalLogs);
            ToolbarLogToggle(btWarningsR, LogType.Warning, ref _console.showWarningLogs);
            ToolbarLogToggle(btErrorsR, LogType.Error, ref _console.showErrorLogs);
            // Clear logs button
            if (GUI.Button(btDeleteR, "", Style.Toolbar.btDelete)) {
                _hasLogEvidence = false;
                _console.logDB.Clear();
            }
        }

        public void Draw_LogsList()
        {
            int totLogs = _console.logDB.filteredLogs.Count;
            int logRowH = _LogRowH.ScaledInt();
            int logsFullH = totLogs * logRowH;
            Rect fullArea = _logsScrollView.BeginScrollView(_logsListArea, new Rect(0, 0, _logsListArea.width, logsFullH));

            bool isMouseDown = false, isMouseUp = false;
            switch (Event.current.type) {
            case EventType.MouseDown:
                isMouseDown = true; break;
            case EventType.MouseUp:
                isMouseUp = true; break;
            case EventType.MouseDrag:
                if (_waitingToCheckLogClick) {
                    if (Vector2.Distance(Event.current.mousePosition - _logsScrollView.scrollPosition, _pressMouseP) > 16.Scaled()) _waitingToCheckLogClick = false;
                }
                break;
            }

            float icoSize = 18.ScaledInt();
            int dist = 6.ScaledInt();
            int labelsDist = 2.ScaledInt();
            int labelsH = 16.ScaledInt();
            int maxSceneLabelW = 250.ScaledInt();
            Rect rowR = new Rect(0, -logRowH, fullArea.width, logRowH);
            Rect icoR = new Rect(rowR.x + dist, rowR.center.y - icoSize * 0.5f, icoSize, icoSize);
            Rect timeR, frameR, sceneR;
            if (_console.showTimeLabels) {
                timeR = rowR.ShiftXAndResize(icoR.xMax - rowR.x + dist).SetWidth(72.ScaledInt()).SetHeight(labelsH).SetCenterY(rowR.center.y);
                frameR = timeR.Shift(timeR.width + labelsDist, 0, 0, 0).SetWidth(50.ScaledInt());
            } else {
                timeR = frameR = new Rect(icoR.xMax, rowR.y, 0, 0);
            }
            for (int i = 0; i < totLogs; ++i) {
                rowR.y += logRowH;
                icoR.y += logRowH;
                timeR.y += logRowH;
                frameR.y += logRowH;
                bool isVisible = rowR.y > _logsScrollView.scrollPosition.y - logRowH && rowR.y < _logsScrollView.scrollPosition.y + _logsListArea.height;
                if (!isVisible) continue;
                //
                LogData log = _console.logDB.filteredLogs[i];
                bool isSelected = _hasLogEvidence && _currLogEvidence == log;
                // Missing rects
                if (_console.showSceneLabel) {
                    sceneR = new Rect(
                        frameR.xMax + labelsDist, 0,
                        Mathf.Min(Style.Log.sceneLabel.CalcSize(new GUIContent(log.scene)).x, maxSceneLabelW),
                        labelsH
                    ).SetCenterY(rowR.center.y);
                } else sceneR = frameR;
                Rect logR = rowR.ShiftXAndResize(sceneR.xMax - rowR.x + dist);
                // Background
                GUI.backgroundColor = isSelected
                    ? Color.black
                    : (Color)(i % 2 == 0
                        ? new DeSkinColor(0.14f)
                        : new DeSkinColor(0.10f));
                GUI.Box(rowR, "", Style.Log.bg);
                GUI.backgroundColor = Color.white;
                // Icon
                GUI.DrawTexture(icoR, ImgDB.GetLogTypeIcon(log.type), ScaleMode.StretchToFill);
                // Time + Frame
                if (_console.showTimeLabels) {
                    GUI.backgroundColor = _TimeColor;
                    GUI.Label(timeR, log.timeStr, Style.Log.timeLabel);
                    GUI.backgroundColor = _FrameColor;
                    GUI.Label(frameR, log.frameStr, Style.Log.frameLabel);
                    GUI.backgroundColor = Color.white;
                }
                // Scene
                if (_console.showSceneLabel) {
                    GUI.backgroundColor = _SceneColor;
                    GUI.Label(sceneR, log.scene, Style.Log.sceneLabel);
                    GUI.backgroundColor = Color.white;
                }
                // Log
                GUI.Label(logR, log.message, Style.Log.logLabel);
                // Evidence row if it's selected log
                if (isSelected) {
                    GUI.backgroundColor = EvidenceColor;
                    GUI.Box(rowR.Shift(1.ScaledInt(), 0, -2.ScaledInt(), 0), "", Style.Log.rowEvidence);
                    GUI.backgroundColor = Color.white;
                }
                // Check mouse click on log
                if (isMouseDown) {
                    if (!_waitingToCheckLogClick && rowR.Contains(Event.current.mousePosition)) {
                        _waitingToCheckLogClick = true;
                        _pressedLog = log;
                        _pressTime = Time.realtimeSinceStartup;
                        _pressMouseP = Event.current.mousePosition - _logsScrollView.scrollPosition;
                    }
                } else if (isMouseUp && _waitingToCheckLogClick) {
                    if (Time.realtimeSinceStartup - _pressTime > _MaxClickInterval) _waitingToCheckLogClick = false;
                    else if (rowR.Contains(Event.current.mousePosition) && log == _pressedLog) {
                        // Click
                        _waitingToCheckLogClick = false;
                        if (_hasLogEvidence && _currLogEvidence == log) _hasLogEvidence = false;
                        else {
                            _hasLogEvidence = true;
                            _currLogEvidence = log;
                            _currLogEvidenceStackTrace = log.GetFormattedStackTrace();
                            _currLogEvidenceScrollY = rowR.y - _logsListArea.height + logRowH;
                        }
                    }
                }
            }

            _logsScrollView.EndScrollView();

            if (_hasLogEvidence) Draw_LogEvidence(_currLogEvidence);
        }

        void Draw_LogEvidence(LogData log)
        {
            Rect toolbarArea = _logEvidenceArea.SetHeight(28.ScaledInt());
            Rect evidenceArea = _logEvidenceArea.ShiftYAndResize(toolbarArea.height);

            float logH = Style.LogEvidence.logLabel.CalcHeight(new GUIContent(log.message), evidenceArea.width);
            float stackH = _currLogEvidenceStackTrace == null
                ? 0
                : Style.LogEvidence.stackLabel.CalcHeight(new GUIContent(_currLogEvidenceStackTrace), evidenceArea.width);
            Rect logR = new Rect(0, 0, evidenceArea.width, logH);
            Rect stackR = new Rect(0, logR.yMax, evidenceArea.width, stackH);
            float fullH = stackR.yMax;

            evidenceArea = _logEvidenceScrollView.BeginScrollView(evidenceArea, new Rect(0, 0, evidenceArea.width, fullH));

            GUI.backgroundColor = new DeSkinColor(0.05f);
            GUI.Box(evidenceArea, "", Style.LogEvidence.bg);
            GUI.backgroundColor = Color.white;
            GUI.Label(logR, log.message, Style.LogEvidence.logLabel);
            GUI.Label(stackR, _currLogEvidenceStackTrace, Style.LogEvidence.stackLabel);

            _logEvidenceScrollView.EndScrollView();

            Draw_LogEvidenceToolbar(toolbarArea, log);
        }

        void Draw_LogEvidenceToolbar(Rect area, LogData log)
        {
            float icoSize = 18.ScaledInt();
            int btH = (ImgDB.LogCopyButton.img.height * 0.42f).ScaledInt();
            int btCopyW = (ImgDB.LogCopyButton.img.width * 0.42f).ScaledInt();
            int btMailW = (ImgDB.LogMailButton.img.width * 0.42f).ScaledInt();
            int dist = 6.ScaledInt();
            Rect borderTopR = area.SetHeight(2.ScaledInt());
            Rect toolbarR = area.ShiftYAndResize(borderTopR.height);
            Rect icoR = new Rect(toolbarR.x + dist, toolbarR.center.y - icoSize * 0.5f, icoSize, icoSize);
            Rect timeR = toolbarR.ShiftXAndResize(icoR.xMax - toolbarR.x + dist).SetWidth(72.ScaledInt()).SetHeight(16.ScaledInt()).SetCenterY(toolbarR.center.y);
            Rect frameR = timeR.Shift(timeR.width + 2.ScaledInt(), 0, 0, 0).SetWidth(50.ScaledInt());
            Rect sceneR = frameR.Shift(frameR.width + 2.ScaledInt(), 0, 0, 0).SetWidth(Style.Log.sceneLabel.CalcSize(new GUIContent(log.scene)).x);
            Rect btCopyR = new Rect(toolbarR.xMax - 2.ScaledInt() - btCopyW, 0, btCopyW, btH).SetCenterY(toolbarR.center.y);
            Rect btMailR = new Rect(btCopyR.x - 2.ScaledInt() - btMailW, 0, btMailW, btH).SetCenterY(toolbarR.center.y);

            GUI.color = new DeSkinColor(0.1f);
            GUI.DrawTexture(borderTopR, ImgDB.WhiteSquare, ScaleMode.StretchToFill);
            GUI.color = EvidenceColor;
            GUI.DrawTexture(toolbarR, ImgDB.WhiteSquare, ScaleMode.StretchToFill);
            GUI.color = GUI.backgroundColor = Color.white;

            GUI.DrawTexture(icoR, ImgDB.GetLogTypeIcon(log.type), ScaleMode.StretchToFill);
            GUI.backgroundColor = _TimeColor;
            GUI.Label(timeR, log.timeStr, Style.Log.timeLabel);
            GUI.backgroundColor = _FrameColor;
            GUI.Label(frameR, log.frameStr, Style.Log.frameLabel);
            GUI.backgroundColor = _SceneColor;
            GUI.Label(sceneR, log.scene, Style.Log.sceneLabel);
            GUI.backgroundColor = Color.white;

            // Copy button
            if (GUI.Button(btCopyR, "", Style.LogEvidence.btCopy)) _console.CopyToClipboard(log);
            // Mail button
            if (_console.canSendMail) {
                if (GUI.Button(btMailR, "", Style.LogEvidence.btMail)) _console.SendMail(log);
            }

            // Click on toolbar (send scroll to selected log)
            if (Event.current.type == EventType.MouseDown && area.Contains(Event.current.mousePosition)) {
                _logsScrollView.SendToY(_currLogEvidenceScrollY);
            }
        }

        #region GUI Helpers

        void ToolbarLogToggle(Rect r, LogType logType, ref bool value)
        {
            if (r.width < 1) return;

            Texture2D img;
            string totlabel;
            bool hasLogs;
            switch (logType) {
            case LogType.Warning:
                img = ImgDB.IcoWarning;
                totlabel = _console.logDB.totWarningLogsStr;
                hasLogs = _console.logDB.totWarningLogs > 0;
                break;
            case LogType.Error:
                img = ImgDB.IcoError;
                totlabel = _console.logDB.totErrorLogsStr;
                hasLogs = _console.logDB.totErrorLogs > 0;
                break;
            default:
                img = ImgDB.IcoLog;
                totlabel = _console.logDB.totNormalLogsStr;
                hasLogs = _console.logDB.totNormalLogs > 0;
                break;
            }
            // Button
            GUI.contentColor = value ? Color.white : new Color(1f, 1f, 1f, 0.3f);
            if (GUI.Button(r, totlabel, value ? Style.Toolbar.logToggleOn : Style.Toolbar.logToggleOff)) {
                value = !value;
                _console.logDB.ChangeFilter(logType, value);
                if (!value && _hasLogEvidence && _currLogEvidence.type == logType) _hasLogEvidence = false;
                _logsScrollView.SendToBottom();
            }
            GUI.contentColor = Color.white;
            // Icon
            float icoSize = 18.ScaledInt();
            float icoHalf = icoSize * 0.5f;
            Rect icoR = new Rect(r.center.x - icoHalf, r.center.y - icoHalf - 6.ScaledInt(), icoSize, icoSize);
            GUI.color = hasLogs ? Color.white : new Color(1f, 1f, 1f, 0.3f);
            GUI.DrawTexture(icoR, img, ScaleMode.StretchToFill);
            GUI.color = Color.white;
        }

        // Returns TRUE if it changed
        bool ToolbarImgToggle(Rect r, Texture2D img, ref bool value)
        {
            bool changed = false;
            // Button
            GUI.contentColor = value ? Color.white : new Color(1f, 1f, 1f, 0.3f);
            if (GUI.Button(r, "", value ? Style.Toolbar.logToggleOn : Style.Toolbar.logToggleOff)) {
                changed = true;
                value = !value;
            }
            GUI.contentColor = Color.white;
            // Icon
            int fitTo = 22.ScaledInt();
            Rect icoR = new Rect(0, 0, img.width, img.height).Fit(fitTo, fitTo, false).SetCenter(r.center.x, r.center.y);
            GUI.DrawTexture(icoR, img, ScaleMode.StretchToFill);
            return changed;
        }

        #endregion

        #endregion

        #region Callbacks

        void OnNewFilteredLogAdded()
        {
            _logsScrollView.SendToBottom();
        }

        #endregion
    }
}