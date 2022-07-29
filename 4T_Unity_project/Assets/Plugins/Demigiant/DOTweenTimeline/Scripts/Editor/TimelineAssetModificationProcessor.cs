// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/03/15

using System;
using System.IO;
using DG.DemiEditor;
using UnityEngine;

namespace DG.Tweening.TimelineEditor
{
    public class TimelineAssetModificationProcessor : UnityEditor.AssetModificationProcessor
    {
        static string[] OnWillSaveAssets(string[] paths)
        {
            // Exit eventual recording or preview
            if (DOTimelineRecorder.isRecording) DOTimelineRecorder.ExitRecordMode(true);
            else if (DOTimelinePreviewManager.isPlayingOrPreviewing) DOTimelinePreviewManager.StopPreview();

            return paths;
        }
    }

    class TimelineAssetPostProcessor : UnityEditor.AssetPostprocessor
    {
        static bool IgnoreFileListProcessing()
        {
            return File.Exists(TimelinePaths.Sys.IgnoreFileListProcessing);
        }

        static void OnPostprocessAllAssets(string[] importedAssets, string[] deletedAssets, string[] movedAssets, string[] movedFromAssetPaths)
        {
            bool importedDOTweenTimeline = false;
            for (int i = 0; i < importedAssets.Length; ++i) {
                if (!importedAssets[i].Contains(TimelinePaths.Name.DOTweenTimelineDir)) continue;
                if (!importedAssets[i].StartsWith(TimelinePaths.ADB.DOTweenTimelineDir)) continue;
                importedDOTweenTimeline = true;
                break;
            }
            if (!importedDOTweenTimeline) return;

            // Remove extra files
            bool simulate = IgnoreFileListProcessing();
            DeEditorPackageManager.ParseListAndRemoveExtraFiles("DOTweenTimeline File Parser",
                TimelinePaths.ADB.DOTweenTimelineFileList, TimelinePaths.ADB.DOTweenTimelineDir, true, simulate
            );
        }
    }
}