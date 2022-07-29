// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/02/06

using System;
using System.Collections;
using System.IO;
using System.Text;
using DG.DemiEditor;
using UnityEditor;
using UnityEngine;

namespace Demigiant.DemiTools.Core.Editor
{
    public static class UnityPackageGenerator
    {
        static readonly StringBuilder _Strb = new StringBuilder();

        #region Public Methods

        /// <summary>
        /// Creates a UnityPackage out of the given paths and exports it to the given directory.
        /// </summary>
        /// <param name="packageName">Name of the package (without extension)</param>
        /// <param name="exportDirName">Name of directory where the UnityPackage will be exported
        /// (the directory must exist in the parent folder of the Unity project)</param>
        /// <param name="relativeContentDirsPaths">Name of the directories to include in the package, relative to the Assets folder
        /// (but without including it)</param>
        public static void Generate(string packageName, string exportDirName, params string[] relativeContentDirsPaths)
        {
            DeEditorCoroutines.StartCoroutine(CO_Generate(packageName, exportDirName, relativeContentDirsPaths));
        }
        static IEnumerator CO_Generate(string packageName, string exportDirName, string[] relativeContentDirsPaths)
        {
            string title = "Exporting " + packageName;
            int totPaths = relativeContentDirsPaths.Length;
            string[] adbDirs = new string[totPaths];
            for (int i = 0; i < totPaths; ++i) {
                string fullPath = DeEditorFileUtils.assetsPath + DeEditorFileUtils.PathSlash 
                    + relativeContentDirsPaths[i].Replace(DeEditorFileUtils.PathSlashToReplace, DeEditorFileUtils.PathSlash);
                adbDirs[i] = DeEditorFileUtils.FullPathToADBPath(fullPath);
                if (!Directory.Exists(fullPath)) {
                    EditorUtility.DisplayDialog(title, string.Format("Couldn't find \"{0}\" directory", fullPath), "Ok");
                    yield break;
                }
            }
            string exportToDir = DeEditorFileUtils.projectPath.Substring(
                0, DeEditorFileUtils.projectPath.LastIndexOf(DeEditorFileUtils.PathSlash, StringComparison.Ordinal)
            ) + DeEditorFileUtils.PathSlash + exportDirName;
            if (!Directory.Exists(exportToDir)) {
                EditorUtility.DisplayDialog(title, "Couldn't find export folder:\n\n" + exportToDir, "Ok");
                yield break;
            }

            string exportToFilePath = string.Format("{0}{1}{2}.unitypackage", exportToDir, DeEditorFileUtils.PathSlash, packageName);
            EditorUtility.DisplayProgressBar(title, "Exporting...", 0.5f);
            yield return null;
            AssetDatabase.ExportPackage(adbDirs, exportToFilePath, ExportPackageOptions.Recurse);
            EditorUtility.ClearProgressBar();
            yield return null;

            // Exported - log results
            int ind = exportToFilePath.LastIndexOf(DeEditorFileUtils.PathSlash, StringComparison.Ordinal);
            ind = exportToFilePath.LastIndexOf(DeEditorFileUtils.PathSlash, ind - 1, StringComparison.Ordinal);
            string exportLogPath = exportToFilePath.Substring(ind + 1);
            _Strb.Append("The following packages were exported to\n\"").Append(exportLogPath).Append("\"\n");
            foreach (string adbDir in adbDirs) {
                _Strb.Append("\n   - ").Append(adbDir);
            }
            Debug.Log(_Strb.ToString());
            yield return null;
            yield return null;
            _Strb.Clear();
            EditorUtility.DisplayDialog(title, "Package exported to\n\n" + exportLogPath, "Ok");
        }

        #endregion
    }
}