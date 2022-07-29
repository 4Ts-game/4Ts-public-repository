// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2019/03/30 13:11
// License Copyright (c) Daniele Giardini

using System.IO;
using DG.DemiEditor;
using UnityEditor;
using UnityEngine;

namespace DG.DeebugLib.Editor
{
    [InitializeOnLoad]
    static class DeebugEditor
    {
        /// <summary>Without final slash</summary>
        public static readonly string DeebugDir;
        public const string OptionsADBFilePath = "Assets/Resources/" + DeebugOptions.ResourceId + ".asset";

        const int _DeebugConsoleExecutionOrder = 1996;

        static DeebugEditor()
        {
            // Store DeebugDir
            string lookup = DeEditorFileUtils.assetsPath;
            string[] dirs = Directory.GetDirectories(lookup, "Deebug", SearchOption.AllDirectories);
            if (dirs.Length == 0) {
                Debug.LogError("Deebug ► Deebug is installed but Deebug folder couldn't be found");
                return;
            } else if (dirs.Length > 1) {
                Debug.LogError("Deebug ► Multiple installations of Deebug found: you should only have a single folder for it");
                return;
            }

            DeebugDir = dirs[0];

            // Find DeebugConsole and set its script execution order
            string deebugConsoleFilePath = string.Format(
                "{1}{0}Scripts{0}RuntimeConsole{0}DeeConsole.cs",
                DeEditorFileUtils.PathSlash, DeebugDir
            );
            if (!File.Exists(deebugConsoleFilePath)) {
                Debug.LogError("Deebug ► Couldn't find DeebugConsole.cs file at " + deebugConsoleFilePath);
                return;
            }
            //
            string adbDeebugConsoleFilePath = DeEditorFileUtils.FullPathToADBPath(deebugConsoleFilePath);
            MonoImporter deebugConsoleMonoImporter = AssetImporter.GetAtPath(adbDeebugConsoleFilePath) as MonoImporter;
            if (deebugConsoleMonoImporter == null) {
                Debug.LogError("Deebug ► Couldn't find DeebugConsole.cs MonoImporter");
                return;
            }
            //
            MonoScript ms = deebugConsoleMonoImporter.GetScript();
            if (MonoImporter.GetExecutionOrder(ms) != _DeebugConsoleExecutionOrder) {
                MonoImporter.SetExecutionOrder(ms, _DeebugConsoleExecutionOrder);
                Debug.Log("Deebug ► Set execution order of DeebugConsole.cs to " + _DeebugConsoleExecutionOrder);
            }
        }
    }
}