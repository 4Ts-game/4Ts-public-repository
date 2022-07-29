// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2019/03/22 16:35
// License Copyright (c) Daniele Giardini

using System;
using System.Collections.Generic;
using UnityEngine;

namespace DG.DeebugLib.RuntimeConsole
{
    internal class LogDB
    {
        #region EVENTS

        public static event Action OnNewFilteredLogAdded;
        static void Dispatch_OnNewFilteredLogAdded() { if (OnNewFilteredLogAdded != null) OnNewFilteredLogAdded(); }

        #endregion

        public bool isPaused { get; private set; }
        public readonly List<LogData> allLogs = new List<LogData>(1000);
        public readonly List<LogData> filteredLogs = new List<LogData>(1000);
        public int totNormalLogs, totWarningLogs, totErrorLogs;
        public string totNormalLogsStr = "0", totWarningLogsStr = "0", totErrorLogsStr = "0";

        // Index is Normal, Warning, Error
        bool[] _currFilters = new[] { true, true, true };
        int _pausedCount = -1; // -1 if there's no pause

        #region Public Methods

        public void Clear()
        {
            allLogs.Clear();
            filteredLogs.Clear();
            totNormalLogs = totWarningLogs = totErrorLogs = 0;
            totNormalLogsStr = totWarningLogsStr = totErrorLogsStr = "0";
            if (isPaused) _pausedCount = 0;
        }

        // Stops adding logs to filtered logs
        public void Pause()
        {
            if (isPaused) return;

            isPaused = true;
            _pausedCount = allLogs.Count;
        }

        public void Resume()
        {
            if (!isPaused) return;

            isPaused = false;
            _pausedCount = -1;
            RefreshFiltered();
        }

        public LogData AddLog(LogType logType, string message, string stacktrace)
        {
            bool filter = false;
            switch (logType) {
            case LogType.Log:
                totNormalLogs++;
                totNormalLogsStr = totNormalLogs.ToString("N0");
                filter = _currFilters[0];
                break;
            case LogType.Warning:
                totWarningLogs++;
                totWarningLogsStr = totWarningLogs.ToString("N0");
                filter = _currFilters[1];
                break;
            case LogType.Error:
            case LogType.Assert:
            case LogType.Exception:
                logType = LogType.Error;
                totErrorLogs++;
                totErrorLogsStr = totErrorLogs.ToString("N0");
                filter = _currFilters[2];
                break;
            }
            LogData logData = new LogData(allLogs.Count, logType, message, stacktrace);
            allLogs.Add(logData);
            if (filter && !isPaused) {
                filteredLogs.Add(logData);
                Dispatch_OnNewFilteredLogAdded();
            }
            return logData;
        }

        public void ChangeFilter(LogType logType, bool add)
        {
            switch (logType) {
            case LogType.Log:
                _currFilters[0] = add;
                break;
            case LogType.Warning:
                _currFilters[1] = add;
                break;
            case LogType.Error:
            case LogType.Assert:
            case LogType.Exception:
                _currFilters[2] = add;
                break;
            }
            RefreshFiltered();
        }

        public void ChangeFilters(bool showLogs, bool showWarnings, bool showErrors)
        {
            _currFilters[0] = showLogs;
            _currFilters[1] = showWarnings;
            _currFilters[2] = showErrors;
            RefreshFiltered();
        }

        #endregion

        #region Methods

        void RefreshFiltered()
        {
            filteredLogs.Clear();
            int tot = isPaused ? _pausedCount : allLogs.Count;
            for (int i = 0; i < tot; ++i) {
                if (LogHasValidFilter(allLogs[i])) filteredLogs.Add(allLogs[i]);
            }
        }

        #endregion

        #region Helpers

        bool LogHasValidFilter(LogData log)
        {
            switch (log.type) {
            case LogType.Log:
                return _currFilters[0];
            case LogType.Warning:
                return _currFilters[1];
            case LogType.Error:
            case LogType.Assert:
            case LogType.Exception:
                return _currFilters[2];
            }
            return false;
        }

        #endregion
    }
}