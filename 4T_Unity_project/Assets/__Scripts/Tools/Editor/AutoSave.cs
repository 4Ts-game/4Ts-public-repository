// Author: Pietro Polsinelli - http://designAGame.eu
// Twitter https://twitter.com/ppolsinelli
// All free as in free beer :-)

using System;
using System.IO;
using DG.Debugging;
using TMPro;
using UnityEditor;
using UnityEditor.SceneManagement;
using UnityEngine;
using UnityEngine.SceneManagement;

namespace OL
{
    [InitializeOnLoad]
    public class AutoSaveOnCompile
    {
        static AutoSaveOnCompile _instance = null;
        static bool savedSceneForThisCompile;

        static AutoSaveOnCompile()
        {
            Unused(_instance);
            _instance = new AutoSaveOnCompile();
        }

        AutoSaveOnCompile()
        {
            EditorApplication.update += OnEditorUpdate;
        }

        ~AutoSaveOnCompile()
        {
            EditorApplication.update -= OnEditorUpdate;
        }

        static void OnEditorUpdate()
        {
            if (!EditorApplication.isPlaying && !savedSceneForThisCompile && !EditorApplication.isCompiling)
            {
                savedSceneForThisCompile = true;
                EditorSceneManager.SaveScene(SceneManager.GetActiveScene());
                var n = "0";
                try
                {
                    n = PlayerSettings.iOS.buildNumber;
                }
                catch (Exception)
                {
                    Delogger.Log("AutoSaveOnCompile", "No IOS build number");
                }
                SimpleGameManager.Build = $"{n}.{DateTime.Now.ToString("yyyy-MM-dd")}";
                var asset = Resources.Load("ol.build") as TextAsset;
                if (asset != null && SimpleGameManager.Build != asset.text)
                {
                    File.WriteAllText("Assets/Resources/ol.build.txt", SimpleGameManager.Build);
                    AssetDatabase.SaveAssets();
                    Delogger.Log("AutoSaveOnCompile", "Saved new build");
                }
            }

            if (EditorApplication.isCompiling && savedSceneForThisCompile)
            {
                savedSceneForThisCompile = false;
            }
        }

        static void Unused<T>(T unusedVariable)
        {
        }

    }
}