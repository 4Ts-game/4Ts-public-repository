// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2019/03/22 16:38
// License Copyright (c) Daniele Giardini

using System;
using System.Text;
using DG.DeExtensions;
using UnityEngine;
using UnityEngine.SceneManagement;

namespace DG.DeebugLib.RuntimeConsole
{
    internal struct LogData
    {
        public static string extraReportInfo;
        public readonly int id;
        public readonly LogType type;
        public readonly string scene;
        public readonly string message;
        public readonly string stackTrace;
        public readonly float time;
        public readonly string timeStr;
        public readonly int frame;
        public readonly string frameStr;

        static StringBuilder _Strb;

        public LogData(int id, LogType type, string message, string stackTrace)
        {
            this.id = id;
            this.type = type;
            this.message = message;
            this.stackTrace = stackTrace;
            time = Time.realtimeSinceStartup;
            timeStr = ConvertToTimeString((int)(time * 1000));
            frame = Time.frameCount;
            frameStr = frame.ToString("N0");
            scene = SceneManager.GetActiveScene().name;
#if true // MARKER_ADVANCEDSTACKTRACE
            if (this.stackTrace.IsNullOrEmpty()) {
                // Force retrieve stack trace
                System.Diagnostics.StackTrace trace = new System.Diagnostics.StackTrace();
                this.stackTrace = trace.ToString();
                // Remove rows that concern log process
                int index = this.stackTrace.IndexOf("at UnityEngine.Debug.", StringComparison.Ordinal);
                if (index != -1) index = this.stackTrace.IndexOf(System.Environment.NewLine, index + 3, StringComparison.Ordinal);
                if (index != -1) this.stackTrace = "  " + this.stackTrace.Substring(index + 1).Trim();
            }
#endif
        }

        public string ConverToMailFormat()
        {
            if (_Strb == null) _Strb = new StringBuilder();
            // Log + Stack trace
            _Strb
                .Append("::: LOG (").Append(type).Append(")")
                .Append("\ntime: ").Append(timeStr).Append(", frame: ").Append(frameStr)
                .Append("\nscene: ").Append(scene);
                if (!stackTrace.IsNullOrEmpty()) {
                    _Strb.Append("\n\n").Append(message)
                    .Append("\n\n").Append(stackTrace.TrimEnd());
                }
            // Custom extra report info
            if (!extraReportInfo.IsNullOrEmpty()) {
                _Strb.Append("\n\n").Append(extraReportInfo);
            }
            // Additional info
            _Strb.Append("\n\nProduct name: ").Append(Application.productName)
                .Append("\nApp identifier: ").Append(Application.identifier)
                .Append("\nApp version: ").Append(Application.version)
                .Append("\nUnity version: ").Append(Application.unityVersion)
                .Append("\nOperating system: ").Append(SystemInfo.operatingSystem)
                .Append("\nOperating system family: ").Append(SystemInfo.operatingSystemFamily)
                .Append("\nSystem language: ").Append(Application.systemLanguage)
                .Append("\nGraphics system memory size: ").Append(SystemInfo.systemMemorySize.ToString("N0"))
                .Append("\nProcessor type: ").Append(SystemInfo.processorType)
                .Append("\nProcessor count: ").Append(SystemInfo.processorCount)
                .Append("\nProcessor frequency: ").Append(SystemInfo.processorFrequency.ToString("N0"))
                .Append("\nDevice type: ").Append(SystemInfo.deviceType)
                .Append("\nDevice model: ").Append(SystemInfo.deviceModel)
                .Append("\nGraphics device type: ").Append(SystemInfo.graphicsDeviceType)
                .Append("\nGraphics device ID: ").Append(SystemInfo.graphicsDeviceID)
                .Append("\nGraphics device vendor: ").Append(SystemInfo.graphicsDeviceVendor)
                .Append("\nGraphics device version: ").Append(SystemInfo.graphicsDeviceVersion)
                .Append("\nGraphics memory size: ").Append(SystemInfo.graphicsMemorySize.ToString("N0"))
                .Append("\nGraphics shader level: ").Append(SystemInfo.graphicsShaderLevel)
                .Append("\nScreen resolution: ").Append(Screen.currentResolution)
                .Append("\nScreen DPI (Unity/self-determined): ").Append(Screen.dpi).Append('/').Append(DeeUtils.GetDPI())
                .Append("\nScreen orientation: ").Append(Screen.orientation)
                .Append("\nInternet reachability: ").Append(Application.internetReachability);
            string result = _Strb.ToString();
            _Strb.Length = 0;
            return result;
        }

        public string GetFormattedStackTrace()
        {
            if (stackTrace.IsNullOrEmpty()) return null;
            if (_Strb == null) _Strb = new StringBuilder();
            string s = stackTrace.TrimEnd();
            if (s.StartsWith("UnityEngine.Debug")) {
                // Remove first line (where it just mentions Unity's log call) and final line-break
                s = s.Substring(s.IndexOf('\n') + 1);
            }
            string[] lines = s.Split('\n');
            for (int i = 0; i < lines.Length; ++i) {
                if (i > 0) _Strb.Append('\n');
                _Strb.Append("<color=#f3aa0e>●</color> ");
                string l = lines[i];
                int csExtIndex = l.LastIndexOf(".cs", StringComparison.Ordinal);
                if (csExtIndex == -1) {
                    _Strb.Append(l);
                    continue;
                }
                int classNameStartIndex = l.LastIndexOf('\\', csExtIndex - 1);
                if (classNameStartIndex == -1) classNameStartIndex = l.LastIndexOf('/', csExtIndex - 1);
                if (classNameStartIndex != -1) classNameStartIndex++;
                else {
                    classNameStartIndex = l.LastIndexOf("at ", csExtIndex - 1, StringComparison.Ordinal);
                    if (classNameStartIndex != -1) classNameStartIndex += 3;
                }
                if (classNameStartIndex == -1) {
                    _Strb.Append(l);
                    continue;
                }
                _Strb
                    .Append(l.Substring(0, classNameStartIndex))
                    .Append("<color=#f3aa0e>")
                    .Append(l.Substring(classNameStartIndex, csExtIndex - classNameStartIndex + 3))
                    .Append("</color>")
                    .Append(l.Substring(csExtIndex + 3));
            }
            s = _Strb.ToString();
            _Strb.Length = 0;
            return s;
        }

        static string ConvertToTimeString(int millisecs)
        {
            int hours = millisecs / 3600000;
            int mins = (millisecs % 3600000) / 60000;
            return string.Format("{0:D2}:{1:D2}:{2:D2}.{3:D3}", hours, mins, millisecs % 60000 / 1000, millisecs % 1000);
        }

        public static bool operator ==(LogData a, LogData b)
        {
            return a.id == b.id;
        }
        public static bool operator !=(LogData a, LogData b) 
        {
            return a.id != b.id;
        }
        public override int GetHashCode()
        {
            return base.GetHashCode();
        }
        public override bool Equals(object obj)
        {
            return obj is LogData && this == (LogData)obj;
        }
    }
}
