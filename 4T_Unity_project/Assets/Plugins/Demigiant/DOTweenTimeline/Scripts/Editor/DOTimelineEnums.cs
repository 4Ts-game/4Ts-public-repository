// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/07/31

using DG.DemiEditor;
using DG.DOTweenEditor;

// ReSharper disable InconsistentNaming
namespace DG.Tweening.TimelineEditor
{
    internal enum PrefabEditSaveMode
    {
        Undetermined,
        AutoSave,
        ManualSave
    }

    internal enum StageMode
    {
        Unset,
        Normal,
        PrefabEditingMode
    }

    internal static class TimelinePaths
    {
        /// <summary>Names of files and directories without path</summary>
        public static class Name
        {
            public const string DOTweenTimelineDir = "DOTweenTimeline";
            public const string DOTweenTimelineFileList_wExt = "REQUIRED_DOTTFileList.txt";
        }

        /// <summary>Asset-Database formatted paths (Assets/...)</summary>
        public static class ADB // Full system paths
        {
            static string _DOTweenTimelineDir;
            static string _DOTweenTimelineFileList;
            // TODO allow different Resources folder/subfolder
            public const string DOVisualSequenceSettings = "Assets/Resources/" + Timeline.Core.DOVisualSequenceSettings.ResourcePath + ".asset";
            public const string DOTimelineSettings = "Assets/-DOTimelineSettings.asset";
            // If this file is present the file list processing will be ignored
            public const string IgnoreFileListProcessing = "Assets/-DOTweenTimelinePostProcessingSkipper.txt";

            /// <summary>Without final slash</summary>
            public static string DOTweenTimelineDir {  get {
                if (_DOTweenTimelineDir == null) _DOTweenTimelineDir = DeEditorFileUtils.FullPathToADBPath(Sys.DOTweenTimelineDir);
                return _DOTweenTimelineDir;
            }}
            public static string DOTweenTimelineFileList {  get {
                if (_DOTweenTimelineFileList == null) _DOTweenTimelineFileList = DOTweenTimelineDir + "/" + Name.DOTweenTimelineFileList_wExt;
                return _DOTweenTimelineFileList;
            }}
        }

        /// <summary>Full system paths</summary>
        public static class Sys // Full system paths
        {
            static string _DOTweenTimelineDir;
            static string _DOVisualSequenceCollectionMeta;
            static string _IgnoreFileListProcessing;

            /// <summary>Without final slash</summary>
            public static string DOTweenTimelineDir {  get {
                if (_DOTweenTimelineDir == null) {
                    _DOTweenTimelineDir = EditorUtils.dotweenTimelineDir;
                    _DOTweenTimelineDir = _DOTweenTimelineDir.TrimEnd('/', '\\');
                }
                return _DOTweenTimelineDir;
            }}
            public static string DOVisualSequenceCollectionMeta { get {
                if (_DOVisualSequenceCollectionMeta == null)
                    _DOVisualSequenceCollectionMeta = string.Format("{1}Scripts{0}DOVisualSequenceCollection.cs.meta", DeEditorFileUtils.PathSlash, DOTweenTimelineDir);
                return _DOVisualSequenceCollectionMeta;
            }}
            public static string IgnoreFileListProcessing { get {
                if (_IgnoreFileListProcessing == null)
                    _IgnoreFileListProcessing = DeEditorFileUtils.ADBPathToFullPath(ADB.IgnoreFileListProcessing);
                return _IgnoreFileListProcessing;
            }}
        }
    }
}