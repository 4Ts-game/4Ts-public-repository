// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/01/18

using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Text;
using DG.DemiEditor;
using DG.Tweening.Timeline;
using DG.Tweening.Timeline.Core;
using DG.Tweening.Timeline.Core.Plugins;
using UnityEditor;
using UnityEditor.Experimental.SceneManagement;
using UnityEditor.SceneManagement;
using UnityEngine;
using UnityEngine.Events;

namespace DG.Tweening.TimelineEditor
{
    [InitializeOnLoad]
    internal static class TimelineEditorUtils
    {
        public static TransformSnapshot transformSnapshot { get; private set; }

        static DOTimelineSettings _settings { get { return DOVisualSequenceTimeline.settings; } }
        static TimelineLayout _layout { get { return DOVisualSequenceTimeline.Layout; } }
        static DOVisualSequence _sequence { get { return DOVisualSequenceTimeline.sequence; } }
        static readonly StringBuilder _Strb = new StringBuilder();
        static readonly Color _EaseColor = new Color(1f, 0f, 0.74f);
        static readonly Color _EaseLimiterColor = new Color(0.39f, 0.12f, 0.51f);
        static Color32[] _easeTextureFloodFill;
        static Vector2Int _easeTextureLastSize;
        static PropertyInfo _piPrefabAutoSave;
        static readonly List<FieldInfo> _TmpFInfos = new List<FieldInfo>();
        static readonly List<GameObject> _TmpGos = new List<GameObject>();
        static readonly List<DOVisualSequence> _TmpSequences = new List<DOVisualSequence>();
        static readonly List<SelectionSequence> _TmpSelectionSequences = new List<SelectionSequence>();

        static TimelineEditorUtils()
        {
            EditorConnector.Request.CloneUnityEvent += CloneUnityEvent;
        }

        #region Public Methods

        /// <summary>
        /// Uses undo system and displays dialog at the end
        /// </summary>
        public static void CleanupSequence(DOVisualSequence sequence)
        {
            using (new DOScope.UndoableSerialization()) {
                // Check that all sequenceds are in layers
                // (if not it's because of bug solved in v0.9.155, where deleting a layer didn't delete the sequenceds)
                int totUnlayeredSequenceds = 0;
                for (int i = sequence.sequenceds.Length - 1; i > -1; i--) {
                    string guid = sequence.sequenceds[i].guid;
                    bool found = false;
                    for (int j = 0; j < sequence.layers.Length; ++j) {
                        if (Array.IndexOf(sequence.layers[j].sequencedGuids, guid) == -1) continue;
                        found = true;
                        break;
                    }
                    if (!found) {
                        totUnlayeredSequenceds++;
                        DeEditorUtils.Array.RemoveAtIndexAndContract(ref sequence.sequenceds, i);
                    }
                }
                EditorUtility.DisplayDialog("Cleanup",
                    string.Format("Elements removed because they had no layer: {0}", totUnlayeredSequenceds),
                "Ok");
            }
        }

        public static UnityEvent CloneUnityEvent(UnityEvent unityEvent)
        {
            return unityEvent.Clone();
        }

        public static string ConvertSecondsToTimeString(float seconds, bool showMilliseconds = true, bool ignoreMinutesIfLessThanOne = false)
        {
            int mins = (int)(seconds / 60);
            float secs = seconds % 60;
            float millisecs = (secs - (int)secs) * 1000;
            return !ignoreMinutesIfLessThanOne || mins > 0
                ? showMilliseconds
                    ? string.Format("{0:00}:{1:00}:{2:0000}", mins, (int)secs, millisecs)
                    : string.Format("{0:00}:{1:00}", mins, (int)secs)
                : showMilliseconds
                    ? string.Format("{0:00}:{1:0000}", (int)secs, millisecs)
                    : string.Format("{0:00}", (int)secs);
        }

        // /// <summary>
        // /// Finds and returns DOTweenTimeline's AssetDatabase directory path ("Assets/...") , or NULL if it can't be found
        // /// </summary>
        // public static string FindADBDOTweenTimelineDirPath()
        // {
        //     string[] fullDottDirs = Directory.GetDirectories(
        //         DeEditorFileUtils.assetsPath, DOTimelineSettings.DOTweenTimelineDirName, SearchOption.AllDirectories
        //     );
        //     if (fullDottDirs.Length == 0) {
        //         Debug.LogWarning(string.Format("Couldn't find any directory named \"{0}\"", DOTimelineSettings.DOTweenTimelineDirName));
        //         return null;
        //     }
        //     string fullDottValidatedDir = null;
        //     foreach (string d in fullDottDirs) {
        //         string validationFile = d + DeEditorFileUtils.PathSlash + "Scripts" + DeEditorFileUtils.PathSlash + "DOVisualSequence.cs";
        //         if (!File.Exists(validationFile)) continue;
        //         if (fullDottValidatedDir != null) {
        //             Debug.LogWarning(string.Format("More than one valid \"{0}\" directory found", DOTimelineSettings.DOTweenTimelineDirName));
        //             break;
        //         }
        //         fullDottValidatedDir = d;
        //     }
        //     if (fullDottValidatedDir == null) {
        //         Debug.LogWarning("No valid DOTween Timeline directory could be found");
        //         return null;
        //     }
        //     return DeEditorFileUtils.FullPathToADBPath(fullDottValidatedDir);
        // }

        /// <summary>
        /// Fills the given list with all the GameObjects in the scene,
        /// or the prefab root gameObject if we're in prefab editing mode
        /// </summary>
        public static void FindAllGameObjectsInScene(List<GameObject> fillList, bool rootGameObjectsOnly = false, bool ignoreHidden = true)
        {
            fillList.Clear();
            bool isPrefabStage = IsEditingPrefab();
            GameObject[] allGos = Resources.FindObjectsOfTypeAll<GameObject>();
            if (isPrefabStage) {
                GameObject prefabRootGo = PrefabStageUtility.GetCurrentPrefabStage().prefabContentsRoot;
                fillList.Add(prefabRootGo);
            } else {
                foreach (GameObject go in allGos) {
                    Transform root = go.transform.root;
                    bool isSceneGo = !EditorUtility.IsPersistent(root.gameObject)
                                     && !((go.hideFlags & HideFlags.NotEditable) == HideFlags.NotEditable || (go.hideFlags & HideFlags.HideAndDontSave) == HideFlags.HideAndDontSave);
                    bool isValid = isSceneGo
                                   && (!rootGameObjectsOnly || root == go.transform)
                                   && (!ignoreHidden || (go.hideFlags & HideFlags.HideInHierarchy) != HideFlags.HideInHierarchy);
                    if (!isValid) continue;
                    fillList.Add(go);
                }
            }
        }

        /// <summary>
        /// Uses Reflection to find the sequence/s in the given object, eventually looking for the given GUID.<para/>
        /// Looking inside all nested objects (even nested of nested) caused a crash before (specifically when entering a TextMeshPro object),
        /// but it's now solved by ignoring nested objects that derive from UnityEngine.Object.<para/>
        /// </summary>
        /// <param name="withinObj">The object inside which to look for Sequences</param>
        /// <param name="appendToList">List to which to append the results (can't be NULL)</param>
        /// <param name="guid">If NULL is ignored, if not NULL only one Sequence (the one with that GUID) will be returned</param>
        /// <param name="lookInNestedObjs">If TRUE also search nested objects (referenced instances of classes)</param>
        public static void FindSerializedSequences(object withinObj, List<DOVisualSequence> appendToList, string guid, bool lookInNestedObjs = true)
        {
            if (withinObj == null) return;
            bool findByGuid = guid != null;
            List<object> nestedObjs = null;
            Type sequenceT = typeof(DOVisualSequence);
            Type sequenceArrayT = typeof(DOVisualSequence[]);
            Type sequenceListT = typeof(List<DOVisualSequence>);
            Type serializeFieldAttribute = typeof(SerializeField);
            Type nonSerializedAttribute = typeof(NonSerializedAttribute);
            Type unityObject = typeof(UnityEngine.Object);
            // Find all fields also inside base classes
            _TmpFInfos.Clear();
            Type t = withinObj.GetType();
            FieldInfo[] fInfos = t.GetFields(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance);
            for (int i = 0; i < fInfos.Length; ++i) _TmpFInfos.Add(fInfos[i]);
            t = t.BaseType;
            while (t != null) {
                fInfos = t.GetFields(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance);
                for (int i = 0; i < fInfos.Length; ++i) {
                    bool add = true;
                    for (int j = 0; j < _TmpFInfos.Count; ++j) {
                        if (_TmpFInfos[j].Name != fInfos[i].Name) continue;
                        add = false;
                        break;
                    }
                    if (add) _TmpFInfos.Add(fInfos[i]);
                }
                t = t.BaseType;
            }
            for (int i = 0; i < _TmpFInfos.Count; ++i) {
                FieldInfo fInfo = _TmpFInfos[i];
                bool isSerialized = fInfo.IsPublic && !Attribute.IsDefined(fInfo, nonSerializedAttribute)
                                    || !fInfo.IsPublic && Attribute.IsDefined(fInfo, serializeFieldAttribute);
                if (!isSerialized) continue;
                if (fInfo.FieldType == sequenceT) {
                    DOVisualSequence s = _TmpFInfos[i].GetValue(withinObj) as DOVisualSequence;
                    if (s == null || findByGuid && s.guid != guid) continue;
                    appendToList.Add(s);
                    if (findByGuid) {
                        _TmpFInfos.Clear();
                        return;
                    }
                } else if (fInfo.FieldType == sequenceArrayT || fInfo.FieldType == sequenceListT) {
                    IList<DOVisualSequence> listS = fInfo.GetValue(withinObj) as IList<DOVisualSequence>;
                    if (listS != null) {
                        foreach (DOVisualSequence s in listS) {
                            if (s == null || findByGuid && s.guid != guid) continue;
                            appendToList.Add(s);
                            if (findByGuid) {
                                _TmpFInfos.Clear();
                                return;
                            }
                        }
                    }
                } else if (lookInNestedObjs) {
                    // Non-sequence object: check if it's serialized in which case we'll look inside that too
                    object nestedObj = fInfo.GetValue(withinObj);
                    if (nestedObj != null && nestedObj != withinObj && !fInfo.FieldType.IsSubclassOf(unityObject)) {
                        if (nestedObjs == null) nestedObjs = new List<object>();
                        nestedObjs.Add(nestedObj);
                    }
                }
            }
            // Look inside nested serialized classes
            if (lookInNestedObjs && nestedObjs != null) {
                for (int i = 0; i < nestedObjs.Count; ++i) {
                    FindSerializedSequences(nestedObjs[i], appendToList, guid, true);
                    if (findByGuid && appendToList.Count > 0) {
                        _TmpFInfos.Clear();
                        return;
                    }
                }
            }
            _TmpFInfos.Clear();
        }

        /// <summary>
        /// Generates a flat black texture in case of Custom ease
        /// </summary>
        public static void GenerateEaseTextureIn(Texture2D texture, Ease ease, float overshootOrAmplitude = 1.70158f, float period = 0)
        {
            Vector2Int size = new Vector2Int(texture.width, texture.height);
            int totPixels = size.x * size.y;
            int easeH = (int)(size.y * 0.35f);
            int easeBaseY = (int)((size.y - easeH) * 0.5f);
            int easeTopY = easeBaseY + easeH;
            // Flood fill bg
            if (size != _easeTextureLastSize) {
                _easeTextureLastSize = size;
                _easeTextureFloodFill = new Color32[totPixels];
                for (int i = 0; i < totPixels; ++i) _easeTextureFloodFill[i] = Color.black;
            }
            texture.SetPixels32(_easeTextureFloodFill);
            //
            if (ease != Ease.INTERNAL_Custom) {
                for (int i = 0; i < size.x; ++i) {
                    int x = i;
                    int y = (int)DOVirtual.EasedValue(easeBaseY, easeTopY, (float)i / size.x, ease, overshootOrAmplitude, period);
                    texture.SetPixel(x, easeBaseY, _EaseLimiterColor);
                    texture.SetPixel(x, easeTopY, _EaseLimiterColor);
                    if (y >= size.y || y <= 0) continue;
                    texture.SetPixel(x, y, _EaseColor);
                }
            }
            texture.Apply();
        }

        public static string GetCleanType(Type type)
        {
            string s = type.ToString();
            int index = s.LastIndexOf('.');
            return index == -1 ? s : s.Substring(index + 1);
        }

        public static Rect GetLayerRect(int layerIndex, float timelineAreaWidth)
        {
            return new Rect(
                0, _layout.partialOffset.y + _settings.layerHeight * (layerIndex - _layout.firstVisibleLayerIndex),
                timelineAreaWidth, _settings.layerHeight
            );
        }

        // Returns -1 if the sequenced isn't linked to any layer (can happen when creating a new sequence from an array in the Inspector)
        public static int GetSequencedLayerIndex(DOVisualSequence sequence, string sequencedGuid)
        {
            int lLen = sequence.layers.Length;
            for (int i = 0; i < lLen; ++i) {
                DOVisualSequence.VisualLayer layer = sequence.layers[i];
                int gLen = layer.sequencedGuids.Length;
                for (int j = 0; j < gLen; ++j) {
                    if (layer.sequencedGuids[j] == sequencedGuid) return i;
                }
            }
            return -1;
        }

        public static Rect GetSequencedRect(DOVisualSequenced sequenced, float timelineAreaWidth)
        {
            int layerIndex = _sequence.FindSequencedLayerIndexByGuid(sequenced.guid);
            return GetSequencedRect(sequenced, GetLayerRect(layerIndex, timelineAreaWidth));
        }
        public static Rect GetSequencedRect(DOVisualSequenced sequenced, Rect layerRect)
        {
            return new Rect(
                (int)_layout.GetTimelineXAtTime(sequenced.startTime), layerRect.y + 1,
                (int)(sequenced.Editor_DrawDuration() * _settings.secondToPixels), layerRect.height - 2
            );
        }

        public static SerializedProperty GetSerializedSequence(Component fromSrc, string sequenceGuid)
        {
//            Debug.Log("GetSerializedSequence " + fromSrc.name);
            SerializedObject so = new SerializedObject(fromSrc);
            SerializedProperty iterator = fromSrc.GetType() == typeof(DOVisualSequenceCollection)
                ? so.FindProperty("sequences")
                : so.GetIterator();
            while (iterator.Next(true)) {
                if (iterator.type != "DOVisualSequence") continue;
//                Debug.Log("   found A DOVisualSequence: " + iterator.type + ", " + iterator.propertyType);
                if (iterator.isArray) {
                    for (int i = 0; i < iterator.arraySize; ++i) {
                        SerializedProperty iteratorMember = iterator.GetArrayElementAtIndex(i);
                        if (iteratorMember.FindPropertyRelative("_guid").stringValue == sequenceGuid) return iteratorMember;
                    }
                }
                if (iterator.FindPropertyRelative("_guid").stringValue != sequenceGuid) continue;
//                Debug.Log("   found CORRECT DOVisualSequence: " + iterator.type + ", " + iterator.propertyType);
                return iterator;
            }
            return null;
        }

        /// <summary>
        /// Adds to the given list all the <see cref="DOVisualSequence"/> serializedProperties in the component
        /// </summary>
        public static void GetAllSerializedSequencesInComponent(Component fromSrc, SerializedObject so, List<SerializedProperty> addToList)
        {
            SerializedProperty iterator = fromSrc.GetType() == typeof(DOVisualSequenceCollection)
                ? so.FindProperty("sequences")
                : so.GetIterator();
            while (iterator.Next(true)) {
                if (iterator.type != "DOVisualSequence") continue;
                if (iterator.isArray) {
                    for (int i = 0; i < iterator.arraySize; ++i) {
                        SerializedProperty iteratorMember = iterator.GetArrayElementAtIndex(i);
                        addToList.Add(iteratorMember);
                    }
                } else addToList.Add(iterator);
            }
        }

        public static SerializedProperty GetSerializedSequenced(SerializedProperty spSequence, string sequencedGuid)
        {
            SerializedProperty sequenceds = spSequence.FindPropertyRelative("sequenceds");
//            Debug.Log("GetSerializedSequenced (tot sequenceds: " + sequenceds.arraySize + ")");
            for (int i = 0; i < sequenceds.arraySize; ++i) {
                SerializedProperty spSequenced = sequenceds.GetArrayElementAtIndex(i);
                if (spSequenced.FindPropertyRelative("_guid").stringValue != sequencedGuid) continue;
//                Debug.Log("   found CORRECT DOVisualSequenced: " + spSequenced.type + ", " + spSequenced.propertyType);
                return spSequenced;
            }
            return null;
        }

        public static bool IsEditingPrefab()
        {
            // HACK uses experimental PrefabSceneUtility (should've been non-experimental in Unity 2019 but still is :|)
//            return EditorSceneManager.previewSceneCount > 1 && PrefabStageUtility.GetCurrentPrefabStage() != null;
            return PrefabStageUtility.GetCurrentPrefabStage() != null;
        }

        // Returns TRUE if we're in prefab editing mode and prefab editing mode has autoSave active
        public static PrefabEditSaveMode GetPrefabEditSaveMode()
        {
            if (!IsEditingPrefab()) return PrefabEditSaveMode.Undetermined;
            if (_piPrefabAutoSave == null) {
                _piPrefabAutoSave = typeof(PrefabStage).GetProperty("autoSave", BindingFlags.NonPublic | BindingFlags.Public | BindingFlags.Instance);
            }
            if (_piPrefabAutoSave == null) return PrefabEditSaveMode.Undetermined;
            bool autoSaveActive = (bool)_piPrefabAutoSave.GetValue(PrefabStageUtility.GetCurrentPrefabStage());
            return autoSaveActive ? PrefabEditSaveMode.AutoSave : PrefabEditSaveMode.ManualSave;
        }

        public static void RegisterCompleteSceneUndo(string undoLabel)
        {
            FindAllGameObjectsInScene(_TmpGos, true);
            foreach (GameObject go in _TmpGos) {
                Undo.RegisterFullObjectHierarchyUndo(go, undoLabel);
            }
            _TmpGos.Clear();
        }

        public static void RemoveLayer(DOVisualSequence fromSequence, int layerIndex)
        {
            DOVisualSequence.VisualLayer layer = fromSequence.layers[layerIndex];
            foreach (string guid in layer.sequencedGuids) {
                int sequencedIndex = fromSequence.Editor_GetSequencedIndex(guid);
                if (sequencedIndex == -1) continue;
                DeEditorUtils.Array.RemoveAtIndexAndContract(ref fromSequence.sequenceds, sequencedIndex);
            }
            DeEditorUtils.Array.RemoveAtIndexAndContract(ref fromSequence.layers, layerIndex);
        }

        public static void RemoveSequenced(DOVisualSequence fromSequence, string sequencedGuid)
        {
            for (int i = 0; i < fromSequence.sequenceds.Length; ++i) {
                if (fromSequence.sequenceds[i].guid != sequencedGuid) continue;
                DeEditorUtils.Array.RemoveAtIndexAndContract(ref fromSequence.sequenceds, i);
                break;
            }
            for (int i = 0; i < fromSequence.layers.Length; ++i) {
                DOVisualSequence.VisualLayer layer = fromSequence.layers[i];
                for (int j = 0; j < layer.sequencedGuids.Length; ++j) {
                    if (layer.sequencedGuids[j] != sequencedGuid) continue;
                    DeEditorUtils.Array.RemoveAtIndexAndContract(ref layer.sequencedGuids, j);
                    break;
                }
            }
        }

        public static void ShiftSequencedToLayer(DOVisualSequence sequence, DOVisualSequenced sequenced, int fromLayerIndex, int toLayerIndex)
        {
            DOVisualSequence.VisualLayer fromLayer = sequence.layers[fromLayerIndex];
            DOVisualSequence.VisualLayer toLayer = sequence.layers[toLayerIndex];
            int fromIndex = Array.IndexOf(fromLayer.sequencedGuids, sequenced.guid);
            if (fromIndex == -1) return;
            DeEditorUtils.Array.RemoveAtIndexAndContract(ref fromLayer.sequencedGuids, fromIndex);
            DeEditorUtils.Array.ExpandAndAdd(ref toLayer.sequencedGuids, sequenced.guid);
        }

        public static int SortPlugDataLabels(string a, string b)
        {
            const char divider = '/';
            int aDividers = 0, bDividers = 0;
            foreach (char c in a) {
                if (c == divider) aDividers++;
            }
            foreach (char c in b) {
                if (c == divider) bDividers++;
            }
            if (aDividers > bDividers) return -1;
            if (aDividers < bDividers) return 1;
            return string.Compare(a, b, StringComparison.OrdinalIgnoreCase);
        }

        public static void StoreTransform(Transform t)
        {
            transformSnapshot = new TransformSnapshot(t);
        }

        public static void UpdateSecondToPixels(int newValue, float? mouseOffsetX = null)
        {
            float offsetX = mouseOffsetX == null ? 0 : (float)mouseOffsetX;
            float scaleFactor = (float)newValue / _settings.secondToPixels;
            float newShiftX = _sequence.editor.areaShift.x * scaleFactor + (offsetX - offsetX * scaleFactor);
            _sequence.editor.areaShift = new Vector2(Mathf.Min(0, newShiftX), _sequence.editor.areaShift.y);
            _settings.secondToPixels = newValue;
        }

        public static void UpdateLayerHeight(int newValue, float? mouseOffsetY = null)
        {
            float offsetY = mouseOffsetY == null ? 0 : (float)mouseOffsetY;
            float scaleFactor = (float)newValue / _settings.layerHeight;
            float newShifty = _sequence.editor.areaShift.y * scaleFactor + (offsetY - offsetY * scaleFactor);
            _sequence.editor.areaShift = new Vector2(_sequence.editor.areaShift.x, Mathf.Min(0, newShifty));
            _settings.layerHeight = newValue;
        }

        /// <summary>
        /// Validates the sequence by:<para/>
        /// - looking for sequenceds that have no GUID assigned
        /// - looking for layers that refer to non-existing sequenceds GUIDs, in which case automatically removes them<para/>
        /// - looking for sequenced that aren't linked to any layer, in which case automatically adds a new layer and links it<para/>
        /// Returns TRUE if fixes were applied and the Component needs to be saved
        /// </summary>
        public static bool ValidateAndFixSequence(DOVisualSequence sequence, SerializedProperty spSequence)
        {
            _Strb.Length = 0;
            bool hasAppliedFixes = false;
            int totNullSequencedGuids = 0, totNullLayerSequencedGuids = 0, totUnusedGuids = 0, totUnlinkedSequenceds = 0;
            // Look for sequenced with empty GUID and layers that store and empty sequenced GUID
            foreach (DOVisualSequenced sequenced in sequence.sequenceds) {
                if (!string.IsNullOrEmpty(sequenced.guid)) continue;
                // Assign guid to sequenced (will be assigned to new layer later below) and activate them
                hasAppliedFixes = true;
                totNullSequencedGuids++;
                sequenced.isActive = true;
                sequenced.Editor_RegenerateGuid();
            }
            // Look for layers that refer to non-existent or empty sequenced GUIDs
            foreach (DOVisualSequence.VisualLayer layer in sequence.layers) {
                for (int i = layer.sequencedGuids.Length - 1; i > -1; --i) {
                    string guid = layer.sequencedGuids[i];
                    if (string.IsNullOrEmpty(guid)) {
                        // Remove guid since it's empty
                        hasAppliedFixes = true;
                        totNullLayerSequencedGuids++;
                        DeEditorUtils.Array.RemoveAtIndexAndContract(ref layer.sequencedGuids, i);
                    } else {
                        bool guidIsValid = false;
                        foreach (DOVisualSequenced sequenced in sequence.sequenceds) {
                            if (sequenced.guid != guid) continue;
                            guidIsValid = true;
                            break;
                        }
                        if (guidIsValid) continue;
                        // Remove guid since there's no sequenced that uses it
                        hasAppliedFixes = true;
                        totUnusedGuids++;
                        DeEditorUtils.Array.RemoveAtIndexAndContract(ref layer.sequencedGuids, i);
                    }
                }
            }
            // Look for sequenceds that are not referred by any layer
            int sLen = sequence.sequenceds.Length;
            for (int i = 0; i < sLen; ++i) {
                if (GetSequencedLayerIndex(sequence, sequence.sequenceds[i].guid) != -1) continue;
                // Missing layer for sequenced. Add new layer and add sequenced to it
                hasAppliedFixes = true;
                totUnlinkedSequenceds++;
                DOVisualSequence.VisualLayer newLayer = new DOVisualSequence.VisualLayer("Layer " + (sequence.layers.Length + 1));
                DeEditorUtils.Array.ExpandAndAdd(ref newLayer.sequencedGuids, sequence.sequenceds[i].guid);
                DeEditorUtils.Array.ExpandAndAdd(ref sequence.layers, newLayer);
            }
            if (hasAppliedFixes) {
                _Strb.Append("Some errors were found and fixed in this DOVisualSequence:");
                if (totNullSequencedGuids > 0) {
                    _Strb.Append("\n- ").Append(totNullSequencedGuids).Append(" sequenced GUIDs were regenerated because they were unset");
                }
                if (totNullLayerSequencedGuids > 0) {
                    _Strb.Append("\n- ").Append(totNullLayerSequencedGuids).Append(" null/empty sequenced GUIDs removed from layers");
                }
                if (totUnusedGuids > 0) {
                    _Strb.Append("\n- ").Append(totUnusedGuids).Append(" unused sequenced GUIDs were removed from layers");
                }
                if (totUnlinkedSequenceds > 0) {
                    _Strb.Append("\n- ").Append(totUnlinkedSequenceds)
                        .Append(" sequenced not referred by any layer were re-added by adding new layers");
                }
                EditorUtility.DisplayDialog("DOVisualSequence Problems", _Strb.ToString(), "Ok");
                _Strb.Length = 0;
            }
            return hasAppliedFixes;
        }

        /// <summary>
        /// Returns FALSE if the plugin used is invalid (happens if the plugins were changed and the sequenced refers to an older version) or NULL.
        /// </summary>
        public static bool ValidateSequencedPlugin(DOVisualSequenced sequenced, DOVisualTweenPlugin plugin, bool isGlobal)
        {
            if (plugin == null) return false;
            if (!plugin.HasPlugData(sequenced)) return false;
            if (!isGlobal && sequenced.target != null) {
                Type targetType = sequenced.target.GetType();
                if (targetType != plugin.targetType && !targetType.IsSubclassOf(plugin.targetType)) return false;
            }
            return true;
        }

        /// <summary>
        /// Returns FALSE if the plugin used is invalid (happens if the plugins were changed and the sequenced refers to an older version).
        /// </summary>
        public static bool ValidateSequencedPlugin(DOVisualSequenced sequenced, DOVisualActionPlugin plugin)
        {
            if (plugin == null) return false;
            PlugDataAction plugData = plugin.GetPlugData(sequenced);
            if (plugData == null) return false;
            if (plugData.wantsTarget && sequenced.target != null) {
                Type targetType = sequenced.target.GetType();
                if (targetType != plugData.targetType && !targetType.IsSubclassOf(plugData.targetType)) return false;
            }
            return true;
        }

        #region Context Menus

        public static void CM_SelectSequencedTargetFromGameObject(GameObject go, Action<Component> onSelect)
        {
            GenericMenu menu = new GenericMenu();
            Component[] components = go.GetComponents<Component>();
            Array.Sort(components, (a, b) => {
                bool aSupported = DOVisualPluginsManager.GetTweenPlugin(a) != null;
                bool bSupported = DOVisualPluginsManager.GetTweenPlugin(b) != null;
                if (aSupported && !bSupported) return -1;
                if (!aSupported && bSupported) return 1;
                return 0;
            });
            for (int i = 0; i < components.Length; ++i) {
                Component component = components[i];
                bool supported = DOVisualPluginsManager.GetTweenPlugin(component) != null;
                if (supported) {
                    menu.AddItem(new GUIContent(component.GetType().Editor_GetShortName()), false, ()=> onSelect(component));
                } else {
                    menu.AddDisabledItem(new GUIContent("- (Not Supported) " + component.GetType().Editor_GetShortName()));
                }
            }
            menu.DropDown(new Rect(Event.current.mousePosition.x, Event.current.mousePosition.y, 0, 0));
//            menu.ShowAsContext();
        }

        public static void CM_SelectSequenceInScene(Rect buttonR)
        {
            GenericMenu menu = new GenericMenu();
            if (IsEditingPrefab()) {
                menu.AddDisabledItem(new GUIContent("× Selection disabled in Prefab editing mode"));
            } else {
                FindAllGameObjectsInScene(_TmpGos, true); // Only root gameObjects (will find components in children)
                if (_TmpGos.Count == 0) {
                    menu.AddDisabledItem(new GUIContent("No DOVisualSequences in Scene"));
                } else {
                    _TmpSelectionSequences.Clear();
                    foreach (GameObject go in _TmpGos) {
                        Component[] components = go.GetComponentsInChildren<Component>(true);
                        foreach (Component c in components) {
                            _TmpSequences.Clear();
                            FindSerializedSequences(c, _TmpSequences, null, true);
                            foreach (DOVisualSequence s in _TmpSequences) _TmpSelectionSequences.Add(new SelectionSequence(c, s));
                        }
                    }
                    if (_TmpSelectionSequences.Count == 0) {
                        menu.AddDisabledItem(new GUIContent("No DOVisualSequences in Scene"));
                    } else {
                        // Sort and display
                        _TmpSelectionSequences.Sort((a, b) => string.Compare(a.label.text, b.label.text, StringComparison.OrdinalIgnoreCase));
                        foreach (SelectionSequence sel in _TmpSelectionSequences) {
                            bool selected = DOVisualSequenceTimeline.sequence != null && DOVisualSequenceTimeline.sequence == sel.sequence;
                            menu.AddItem(sel.label, selected, () => {
                                DOVisualSequenceTimeline.ShowWindow(sel.component, sel.sequence, null);
                            });
                        }
                    }
                    _TmpSelectionSequences.Clear();
                    _TmpSequences.Clear();
                    _TmpGos.Clear();
                }
            }
            menu.DropDown(buttonR.SetX(buttonR.xMax).SetY(buttonR.y - buttonR.height));
        }

        #endregion

        #endregion

        // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████
        // ███ INTERNAL CLASSES ████████████████████████████████████████████████████████████████████████████████████████████████
        // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

        class SelectionSequence
        {
            public Component component;
            public DOVisualSequence sequence;
            public GUIContent label;
            public SelectionSequence(Component component, DOVisualSequence sequence)
            {
                this.component = component;
                this.sequence = sequence;
                label = new GUIContent(string.Format("{0}/{1}{2}",
                    component.name, sequence.name.IsNullOrEmpty() ? string.Format("[unnamed - {0}]", sequence.guid) : sequence.name,
                    sequence.isActive ? "" : " (inactive)"
                ));
            }
        }

        public struct TransformSnapshot
        {
            public Vector3 position, localPosition, eulerAngles, localEulerAngles, localScale;
            public Quaternion rotation, localRotation;
            public TransformSnapshot(Transform t)
            {
                this.position = t.position;
                this.localPosition = t.localPosition;
                this.eulerAngles = t.eulerAngles;
                this.localEulerAngles = t.localEulerAngles;
                this.localScale = t.localScale;
                this.rotation = t.rotation;
                this.localRotation = t.localRotation;
            }
        }
    }
}