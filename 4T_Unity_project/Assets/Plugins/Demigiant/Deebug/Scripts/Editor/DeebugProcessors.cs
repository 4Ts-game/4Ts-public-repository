// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2019/04/04

using System.Collections.Generic;
using System.IO;
using DG.DemiEditor;
using UnityEditor;
using UnityEngine;

namespace DG.DeebugLib.Editor
{
    class DeebugProcessors : AssetPostprocessor
    {
        const string _LogDataClassName = "LogData.cs";
        const string _Marker_AdvancedStackTrace = "MARKER_ADVANCEDSTACKTRACE";

        static readonly List<int> _LinesToChange = new List<int>();

        #region Processors

        static void OnPostprocessAllAssets(string[] importedAssets, string[] deletedAssets, string[] movedAssets, string[] movedFromAssetPaths)
        {
            bool logDataClassImported = false;
            for (int i = 0; i < importedAssets.Length; ++i) {
                if (!importedAssets[i].EndsWith(_LogDataClassName) || !importedAssets[i].Contains("Deebug")) continue;
                logDataClassImported = true;
                break;
            }
            if (logDataClassImported) {
                DeEditorUtils.DelayedCall(0.5f, RefreshFileModifications);
            }
        }

        #endregion

        #region Methods

        public static void RefreshFileModifications()
        {
            DeebugOptions options = DeEditorPanelUtils.ConnectToSourceAsset<DeebugOptions>(DeebugEditor.OptionsADBFilePath);
            if (options == null) return;

            // Modify #ifs if necessary
            // ► LogData.cs
            string filePath = string.Format("{1}{0}Scripts{0}RuntimeConsole{0}{2}", DeEditorFileUtils.PathSlash, DeebugEditor.DeebugDir, _LogDataClassName);
            RefreshFileMarkerModification(filePath, _Marker_AdvancedStackTrace, options.console.advancedStackTrace);
        }

        static void RefreshFileMarkerModification(string filePath, string marker, bool enable)
        {
            if (!File.Exists(filePath)) return;

            _LinesToChange.Clear();
            string[] lines = File.ReadAllLines(filePath);
            for (int i = 0; i < lines.Length; ++i) {
                string s = lines[i];
                if (s.EndsWith(marker) && s.StartsWith("#if") && (enable && s.Contains("false") || !enable && s.Contains("true"))) {
                    _LinesToChange.Add(i);
                }
            }
            if (_LinesToChange.Count <= 0) return;

            using (StreamWriter sw = new StreamWriter(filePath)) {
                for (int i = 0; i < lines.Length; ++i) {
                    string s = lines[i];
                    if (_LinesToChange.Contains(i)) {
                        s = enable ? s.Replace("false", "true") : s.Replace("true", "false");
                    }
                    sw.WriteLine(s);
                }
            }
            AssetDatabase.ImportAsset(DeEditorFileUtils.FullPathToADBPath(filePath), ImportAssetOptions.Default);
        }

        #endregion
    }
}