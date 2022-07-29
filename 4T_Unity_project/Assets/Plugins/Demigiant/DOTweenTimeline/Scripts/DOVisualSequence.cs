// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/01/16

using System;
using System.Collections.Generic;
using DG.Tweening.Timeline.Core;
using DG.Tweening.Timeline.Core.Plugins;
using UnityEngine;
using UnityEngine.Events;

namespace DG.Tweening.Timeline
{
    /// <summary>
    /// Add a serialized instance of this class to any MonoBehaviour/Behaviour/Component to have the Inspector show all its options.<para/>
    /// Note that a <see cref="DOVisualSequence"/> doesn't automatically generate its tween,
    /// you'll need to call <see cref="GenerateTween"/> to do that.
    /// </summary>
    [Serializable]
#if UNITY_EDITOR
    public class DOVisualSequence : ISerializationCallbackReceiver
#else
    public class DOVisualSequence
#endif
    {
        public enum StartupBehaviour
        {
            /// <summary>Does not create the tween</summary>
            DoNothing,
            /// <summary>Creates the tween without initializing it</summary>
            Create,
            /// <summary>Creates the tween and initializes the startup values of its targets</summary>
            ForceInitialization,
            /// <summary>Creates the tween and immediately completes it but without firing any internal callback</summary>
            Complete,
            /// <summary>Creates the tween and immediately completes it while also firing internal callbacks</summary>
            CompleteWithInternalCallbacks,
        }

        public enum TimeMode
        {
            TimeScale,
            DurationOverload
        }

        #region Serialized
#pragma warning disable 0649

        [SerializeField] string _guid = Guid.NewGuid().ToString();
        /// <summary>If FALSE <see cref="GenerateTween"/> will have no effect and will not generate this Sequence</summary>
        public bool isActive = true;
        /// <summary>Will be set as the string ID for the Sequence, so it can be used with DOTween's static id-based methods</summary>
        public string name = "";
        public StartupBehaviour startupBehaviour = StartupBehaviour.Create;
        public float startupDelay = 0;
        public bool autoplay = true;
        public bool autokill = true;
        public bool ignoreTimeScale = false;
        public TimeMode timeMode = TimeMode.TimeScale;
        public float timeScale = 1; // Tween's timeScale: used in case of TimeMode.TimeScale
        public float durationOverload = 1; // Tween's overall duration: used in case of TimeMode.DurationOverload
        public int loops = 1;
        public LoopType loopType = LoopType.Yoyo;
        public bool hasOnComplete, hasOnStepComplete, hasOnUpdate;
        public UnityEvent onComplete, onStepComplete, onUpdate;
        public DOVisualSequenced[] sequenceds = new DOVisualSequenced[0];
        public VisualLayer[] layers = new []{ new VisualLayer("Layer 1") };

        // Editor-only
        /// <summary>INTERNAL USE ONLY</summary>
        public EditorData editor = new EditorData();

#pragma warning restore 0649
        #endregion

        public string guid { get { return _guid; } }
        /// <summary>Contains the generated tween.
        /// Returns NULL if the tween hasn't yet been generated or if creation was skipped</summary>
        public Sequence tween { get; private set; }

        static readonly List<DOVisualSequenced> _TmpSequenceds = new List<DOVisualSequenced>(); // Used by FindSequencedsByPinNoAlloc

        /// <summary>
        /// INTERNAL USE ONLY: Parameterless constructor that uses the GUID already assigned (for sequences not created via <see cref="DOVisualSequenceCollection"/>)
        /// </summary>
        public DOVisualSequence() {}
        /// <summary>INTERNAL USE ONLY: Use this constructor when creating a new sequence object, so that you can pass a unique GUID</summary>
        public DOVisualSequence(string guid)
        {
            _guid = guid;
        }

        #region Unity

#if UNITY_EDITOR

        public void OnBeforeSerialize() {}

        public void OnAfterDeserialize()
        {
            // Visual Inspector List fix: detect if new DOVisualSequence was just created as first array element
            // (in which case it's set with all default type values ignoring the ones set by myself, and I need to reset it)
            if (!string.IsNullOrEmpty(_guid)) return;
            Editor_Reset(true);
//            Debug.Log("<color=#ff0000>AFTER DESERIALIZE ► Sequence was reset because GUID was empty (meaning it the newly created element in a visual list):</color> " + _guid);
        }

#endif

        #endregion

        #region Public Methods

        /// <summary>
        /// Generates the Sequence's tween and returns it
        /// (unless <see cref="isActive"/> is set to FALSE/unchecked or the Sequence is immediately killed by a Complete with autoKill active).<para/>
        /// If the tween was already generated does nothing and returns NULL
        /// (use <see cref="ForceGenerateTween"/>) if you want to force the tween to be regenerated.<para/>
        /// </summary>
        /// <param name="behaviour">Leave as NULL to use Startup Behaviour set in the Inspector</param>
        /// <param name="andPlay">Leave as NULL to use Autoplay option set in the Inspector</param>
        public Sequence GenerateTween(StartupBehaviour? behaviour = null, bool? andPlay = null)
        {
            bool isApplicationPlaying = DOVisualSequenceSettings.isApplicationPlaying;
            if (isApplicationPlaying && !isActive) return null;
            if (HasTween()) {
                DOLog.Warning(string.Format("GenerateTween ► DOVisualSequence <color=#d568e3>\"{0}\"</color> has already been generated. Kill the tween first or Use ForceGenerateTween instead.", this.name));
                return null;
            }
            Sequence s = DoGenerateTween(isApplicationPlaying, behaviour, andPlay);
            if (s == null) {
                DOLog.Warning(string.Format("GenerateTween ► DOVisualSequence <color=#d568e3>\"{0}\"</color> was not generated.", this.name));
                return null;
            }
            return s;
        }

        /// <summary>
        /// Regenerates the Sequence's tween and returns it
        /// (unless <see cref="isActive"/> is set to FALSE/unchecked or the Sequence is immediately killed by a Complete with autoKill active).<para/>
        /// If the tween had already been generated it rewinds+kills it or simply kills it, depending on the <see cref="rewindIfExists"/> parameter.<para/>
        /// </summary>
        /// <param name="rewindIfExists">If TRUE and a tween had already been generated rewinds it before killing it, otherwise just kills it</param>
        /// <param name="behaviour">Leave as NULL to use Startup Behaviour set in the Inspector</param>
        /// <param name="andPlay">Leave as NULL to use Autoplay option set in the Inspector</param>
        public Sequence ForceGenerateTween(bool rewindIfExists = true, StartupBehaviour? behaviour = null, bool? andPlay = null)
        {
            bool isApplicationPlaying = DOVisualSequenceSettings.isApplicationPlaying;
            if (isApplicationPlaying && !isActive) return null;
            if (HasTween()) {
                if (rewindIfExists) tween.Rewind();
                tween.Kill();
            }
            Sequence s = DoGenerateTween(isApplicationPlaying, behaviour, andPlay);
            if (s == null) {
                DOLog.Warning(string.Format("ForceGenerateTween ► DOVisualSequence <color=#d568e3>{0}</color> was not generated.", this.name));
                return null;
            }
            return s;
        }

        // Assumes the tween doesn't exist
        Sequence DoGenerateTween(bool isApplicationPlaying, StartupBehaviour? behaviour = null, bool? andPlay = null)
        {
            StartupBehaviour assignedBehaviour = behaviour == null ? startupBehaviour : (StartupBehaviour)behaviour;
            if (isApplicationPlaying && assignedBehaviour == StartupBehaviour.DoNothing) return null;
            bool assignedAndPlay = andPlay == null ? autoplay : (bool)andPlay;
            Sequence s = null;
            float timeMultiplier = 1; // Ignored in editor preview, where duration overload is simulated via timeScale (easier to manage)
            float finalTimeScale = 1;
            switch (timeMode) {
            case TimeMode.TimeScale:
                finalTimeScale = timeScale;
                break;
            case TimeMode.DurationOverload:
                if (isApplicationPlaying) timeMultiplier = durationOverload / TimelineUtils.GetSequenceDuration(this);
                else finalTimeScale = TimelineUtils.GetSequenceDuration(this) / durationOverload;
                break;
            }
            int len = sequenceds.Length;
            for (int i = 0; i < len; ++i) {
                DOVisualSequenced sequenced = sequenceds[i];
                if (!sequenced.isActive) continue;
                switch (sequenced.type) {
                case DOVisualSequenced.Type.Event:
                    if (!isApplicationPlaying) break; // Events are not previewed in editor
                    InsertEvent(ref s, this, sequenced, timeMultiplier);
                    break;
                case DOVisualSequenced.Type.Action:
                    if (!isApplicationPlaying) break; // Global actions are not previewed in editor
                    InsertAction(ref s, this, sequenced, timeMultiplier);
                    break;
                case DOVisualSequenced.Type.Interval:
                    InsertInterval(ref s, this, sequenced, timeMultiplier);
                    break;
                case DOVisualSequenced.Type.GlobalTween:
                    if (!isApplicationPlaying) break; // Global tweens are not previewed in editor
                    InsertSequentiableTween(ref s, this, sequenced, true, timeMultiplier);
                    break;
                default:
                    InsertSequentiableTween(ref s, this, sequenced, false, timeMultiplier);
                    break;
                }
            }
            if (s == null) return null;
            if (isApplicationPlaying) {
                if (startupDelay > 0) s.SetDelay(startupDelay, false);
                bool wasKilled = false;
                s.SetAutoKill(autokill);
                switch (assignedBehaviour) {
                case StartupBehaviour.ForceInitialization:
                    s.ForceInit();
                    if (!assignedAndPlay) s.Pause();
                    break;
                case StartupBehaviour.Complete:
                    s.Complete(false);
                    wasKilled = autokill;
                    break;
                case StartupBehaviour.CompleteWithInternalCallbacks:
                    s.Complete(true);
                    wasKilled = autokill;
                    break;
                default:
                    if (!assignedAndPlay) s.Pause();
                    break;
                }
                if (wasKilled) return null;
                if (onComplete.GetPersistentEventCount() > 0) s.OnComplete(onComplete.Invoke);
                if (onStepComplete.GetPersistentEventCount() > 0) s.OnStepComplete(onStepComplete.Invoke);
                if (onUpdate.GetPersistentEventCount() > 0) s.OnUpdate(onUpdate.Invoke);
            }
            if (!string.IsNullOrEmpty(name)) s.SetId(name);
            s.SetUpdate(ignoreTimeScale);
            s.SetLoops(loops, loopType);
            s.timeScale = finalTimeScale;
            tween = s;
            return s;
        }

        /// <summary>
        /// Returns the <see cref="DOVisualSequenced"/> with the given GUID in this sequence, or NULL if it can't be found
        /// </summary>
        public DOVisualSequenced FindSequencedByGuid(string sequencedGuid)
        {
            int len = sequenceds.Length;
            for (int i = 0; i < len; ++i) {
                if (sequenceds[i].guid == sequencedGuid) return sequenceds[i];
            }
            return null;
        }

        /// <summary>
        /// Returns all of this sequence's <see cref="DOVisualSequenced"/> elements that have the given pin, or an empty list if none was found.<para/>
        /// <code>IMPORTANT:</code> to avoid allocations this method always uses an internal list for the result.
        /// This means you shouldn't modify the resulting list, only its items.
        /// </summary>
        /// <param name="pin">Pin Id (set by right-clicking on a sequenced in the Timeline)</param>
        public List<DOVisualSequenced> FindSequencedsByPinNoAlloc(int pin)
        {
            _TmpSequenceds.Clear();
            int len = sequenceds.Length;
            for (int i = 0; i < len; ++i) {
                if (sequenceds[i].pin == pin) _TmpSequenceds.Add(sequenceds[i]);
            }
            return _TmpSequenceds;
        }

        /// <summary>
        /// Returns the index of the <see cref="VisualLayer"/> containing the given sequenced, or -1 if it can't be found
        /// </summary>
        public int FindSequencedLayerIndexByGuid(string sequencedGuid)
        {
            int len = layers.Length;
            for (int i = 0; i < len; ++i) {
                int sublen = layers[i].sequencedGuids.Length;
                for (int j = 0; j < sublen; ++j) {
                    if (layers[i].sequencedGuids[j] == sequencedGuid) return i;
                }
            }
            return -1;
        }

        #region Control Methods

        /// <summary>
        /// Kills the eventual tween generated and NULLs the <see cref="tween"/> reference
        /// </summary>
        /// <param name="complete">If TRUE completes the tween before killing it</param>
        public void KillTween(bool complete = false)
        {
            if (!HasTween()) return;
            tween.Kill(complete);
            tween = null;
        }

        /// <summary>
        /// Stops and rewinds the tween. Does nothing if the tween hasn't been generated yet or has been killed.
        /// </summary>
        /// <param name="includeDelay">If TRUE includes the eventual delay set on the <see cref="DOVisualSequence"/></param>
        public void RewindTween(bool includeDelay = true)
        {
            if (!HasTween()) {
//                Debug.LogWarning("DOVisualSequence.Rewind ► The tween hasn't been generated or was killed. You must call GenerateTween to create it first.");
                return;
            }
            tween.Rewind(includeDelay);
        }

        /// <summary>
        /// Completes the tween. Does nothing if the tween hasn't been generated yet or has been killed.
        /// </summary>
        /// <param name="withCallbacks">If TRUE will fire all internal callbacks up from the current position to the end, otherwise will ignore them</param>
        public void CompleteTween(bool withCallbacks = false)
        {
            if (!HasTween()) {
                Debug.LogWarning("DOVisualSequence.Complete ► The tween hasn't been generated or was killed. You must call GenerateTween to create it first.");
                return;
            }
            tween.Complete(withCallbacks);
        }

        /// <summary>
        /// Restarts the tween from the beginning. Does nothing if the tween hasn't been generated yet or has been killed.
        /// </summary>
        /// <param name="includeDelay">If TRUE includes the eventual delay set on the <see cref="DOVisualSequence"/></param>
        /// <param name="changeDelayTo">If <see cref="includeDelay"/> is true and this value is bigger than 0 assigns it as the new delay</param>
        public void RestartTween(bool includeDelay = true, float changeDelayTo = -1f)
        {
            if (!HasTween()) {
                Debug.LogWarning("DOVisualSequence.RestartTween ► The tween hasn't been generated or was killed. You must call GenerateTween to create it first.");
                return;
            }
            tween.Restart(includeDelay, changeDelayTo);
        }

        /// <summary>
        /// Regenerates the tween and restarts it from the current targets state (meaning all dynamic elements values will be re-evaluated).
        /// </summary>
        /// <param name="includeDelay">If TRUE includes the eventual delay set on the <see cref="DOVisualSequence"/></param>
        /// <param name="changeDelayTo">If <see cref="includeDelay"/> is true and this value is bigger than 0 assigns it as the new delay</param>
        public void RestartTweenFromHere(bool includeDelay = true, float changeDelayTo = -1f)
        {
            if (HasTween()) tween.Kill();
            ForceGenerateTween(false, StartupBehaviour.Create, false);
            tween.Restart(includeDelay, changeDelayTo);
        }

        /// <summary>
        /// Pauses the tween. Does nothing if the tween hasn't been generated yet or has been killed.
        /// </summary>
        public void PauseTween()
        {
            if (!HasTween()) {
                Debug.LogWarning("DOVisualSequence.PauseTween ► The tween hasn't been generated or was killed. You must call GenerateTween to create it first.");
                return;
            }
            tween.Pause();
        }

        /// <summary>
        /// Resumes the tween. Does nothing if the tween hasn't been generated yet or has been killed.
        /// </summary>
        public void PlayTween()
        {
            if (!HasTween()) {
//                Debug.LogWarning("DOVisualSequence.PlayTween ► The tween hasn't been generated or was killed. You must call GenerateTween to create it first.");
                return;
            }
            tween.Play();
        }

        /// <summary>
        /// Resumes the tween and plays it backwards. Does nothing if the tween hasn't been generated yet or has been killed.
        /// </summary>
        public void PlayTweenBackwards()
        {
            if (!HasTween()) {
       //         Debug.LogWarning("DOVisualSequence.PlayTweenBackwards ► The tween hasn't been generated or was killed. You must call GenerateTween to create it first.");
                return;
            }
            tween.PlayBackwards();
        }

        /// <summary>
        /// Resumes the tween and plays it forward. Does nothing if the tween hasn't been generated yet or has been killed.
        /// </summary>
        public void PlayTweenForward()
        {
            if (!HasTween()) {
          //      Debug.LogWarning("DOVisualSequence.PlayTweenForward ► The tween hasn't been generated or was killed. You must call GenerateTween to create it first.");
                return;
            }
            tween.PlayForward();
        }

        #endregion

        #region Info Methods

        /// <summary>Returns TRUE if the tween was generated and hasn't been killed</summary>
        public bool HasTween()
        {
            return tween != null && tween.active;
        }

        /// <summary>Returns TRUE if the tween is complete
        /// (silently fails and returns FALSE if the tween has been killed)</summary>
        public bool IsTweenComplete()
        {
            return tween != null && tween.active && tween.IsComplete();
        }

        /// <summary>Returns TRUE if the tween has been generated and is playing</summary>
        public bool IsTweenPlaying()
        {
            return tween != null && tween.active && tween.IsPlaying();
        }

        #endregion

#if UNITY_EDITOR
        #region Editor-Only

        /// <summary>
        /// Editor-only. Clones the sequence
        /// </summary>
        public DOVisualSequence Editor_Clone(bool regenerateGuid)
        {
            DOVisualSequence result = new DOVisualSequence(regenerateGuid ? Guid.NewGuid().ToString() : this.guid);
            result.Editor_AssignPropertiesFrom(this, true);
            return result;
        }
        DOVisualSequenced[] Editor_CloneDOVisualSequenceds()
        {
            DOVisualSequenced[] result = new DOVisualSequenced[sequenceds.Length];
            for (int i = 0; i < sequenceds.Length; ++i) result[i] = sequenceds[i].Editor_Clone(false);
            return result;
        }
        VisualLayer[] Editor_CloneVisualLayers()
        {
            VisualLayer[] result = new VisualLayer[layers.Length];
            for (int i = 0; i < layers.Length; ++i) result[i] = layers[i].Editor_Clone();
            return result;
        }

        public void Editor_AssignPropertiesFrom(DOVisualSequence sequence, bool cloneProperties)
        {
            isActive = sequence.isActive;
            name = sequence.name;
            startupBehaviour = sequence.startupBehaviour;
            startupDelay = sequence.startupDelay;
            autoplay = sequence.autoplay;
            autokill = sequence.autokill;
            ignoreTimeScale = sequence.ignoreTimeScale;
            timeMode = sequence.timeMode;
            timeScale = sequence.timeScale;
            durationOverload = sequence.durationOverload;
            loops = sequence.loops;
            loopType = sequence.loopType;
            hasOnComplete = sequence.hasOnComplete;
            hasOnStepComplete = sequence.hasOnStepComplete;
            hasOnUpdate = sequence.hasOnUpdate;
            onComplete = cloneProperties ? EditorConnector.Request.Dispatch_OnCloneUnityEvent(sequence.onComplete) : sequence.onComplete;
            onStepComplete = cloneProperties ? EditorConnector.Request.Dispatch_OnCloneUnityEvent(sequence.onStepComplete) : sequence.onStepComplete;
            onUpdate = cloneProperties ? EditorConnector.Request.Dispatch_OnCloneUnityEvent(sequence.onUpdate) : sequence.onUpdate;
            sequenceds = cloneProperties ? sequence.Editor_CloneDOVisualSequenceds() : sequence.sequenceds;
            layers = cloneProperties ? sequence.Editor_CloneVisualLayers() : sequence.layers;
            editor = sequence.editor;
        }

        /// <summary>
        /// Editor-only. Resets the sequence and eventually generates a new GUID
        /// </summary>
        /// <param name="regenerateGuid">If TRUE regenerates the GUID</param>
        /// <param name="partial">If TRUE resets important properties but leaves other unchanged
        /// (useful to keep some data when it's copied from the previous array element in the Inspector)</param>
        public void Editor_Reset(bool regenerateGuid, bool partial = false)
        {
            if (regenerateGuid) _guid = Guid.NewGuid().ToString();
            isActive = true;
            name = "";
            if (!partial) {
                startupBehaviour = StartupBehaviour.Create;
                startupDelay = 0;
                autoplay = true;
                autokill = true;
                ignoreTimeScale = false;
                timeMode = TimeMode.TimeScale;
                timeScale = 1;
                durationOverload = 1;
                loops = 1;
                loopType = LoopType.Yoyo;
            }
            hasOnComplete = false;
            hasOnStepComplete = false;
            hasOnUpdate = false;
            onComplete = null;
            onStepComplete = null;
            onUpdate = null;
            sequenceds = new DOVisualSequenced[0];
            layers = new []{ new VisualLayer("Layer 1") };
            editor = new EditorData();
        }

        #endregion
#endif

        #endregion

        #region Methods

        static void InsertSequentiableTween(ref Sequence s, DOVisualSequence sequence, DOVisualSequenced sequenced, bool isGlobal, float timeMultiplier)
        {
            DOVisualTweenPlugin plug;
            if (isGlobal) {
                plug = DOVisualPluginsManager.GetGlobalTweenPlugin(sequenced.plugId);
            } else {
                if (sequenced.target == null) return;
                plug = DOVisualPluginsManager.GetTweenPlugin(sequenced.target);
            }
            if (plug == null) return; // Missing plugin
            Tweener t = plug.CreateTween(sequenced, timeMultiplier);
            if (t == null) return; // Missing plugData or tween creation error
            if (DOVisualSequenceSettings.I.debugLogs) {
                string plugStr = plug.GetPlugData(sequenced).label;
                if (isGlobal) {
                    DOLog.Normal(
                        string.Format("DOVisualSequence <color=#d568e3>{0}</color> : Insert global <color=#68b3e2>{1}</color> → <color=#ffa047>{2}</color> tween at {3}\"→{4}\"",
                            string.IsNullOrEmpty(sequence.name) ? "[unnamed]" : sequence.name, sequenced.plugId, plugStr, sequenced.startTime * timeMultiplier, sequenced.duration * timeMultiplier
                    ));
                } else {
                    DOLog.Normal(
                        string.Format("DOVisualSequence <color=#d568e3>{0}</color> : Insert <color=#68b3e2>{1}</color> → <color=#ffa047>{2}</color> tween at {3}\"→{4}\"",
                            string.IsNullOrEmpty(sequence.name) ? "[unnamed]" : sequence.name, sequenced.target.name, plugStr, sequenced.startTime * timeMultiplier, sequenced.duration * timeMultiplier
                    ));
                }
            }
            if (s == null) s = DOTween.Sequence();
            s.Insert(sequenced.startTime * timeMultiplier, t);
        }

        static void InsertEvent(ref Sequence s, DOVisualSequence sequence, DOVisualSequenced sequenced, float timeMultiplier)
        {
            int totEvents = sequenced.onComplete.GetPersistentEventCount();
            if (totEvents <= 0) return;
            if (DOVisualSequenceSettings.I.debugLogs) {
                DOLog.Normal(
                    string.Format("DOVisualSequence <color=#d568e3>{0}</color> : Insert <color=#68b3e2>{1} UnityEvent{2}</color> at {3}\"",
                        string.IsNullOrEmpty(sequence.name) ? "[unnamed]" : sequence.name, totEvents, totEvents < 2 ? "" : "s", sequenced.startTime * timeMultiplier
                ));
            }
            if (s == null) s = DOTween.Sequence();
            s.InsertCallback(sequenced.startTime * timeMultiplier, sequenced.onComplete.Invoke);
        }

        static void InsertAction(ref Sequence s, DOVisualSequence sequence, DOVisualSequenced sequenced, float timeMultiplier)
        {
            DOVisualActionPlugin plug = DOVisualPluginsManager.GetActionPlugin(sequenced.plugId);
            if (plug == null) return; // Missing plugin
            PlugDataAction plugDataAction = plug.GetPlugData(sequenced);
            if (plugDataAction == null) return; // Missing plugData
            if (plugDataAction.wantsTarget && sequenced.target == null) return;
            if (DOVisualSequenceSettings.I.debugLogs) {
                DOLog.Normal(
                    string.Format("DOVisualSequence <color=#d568e3>{0}</color> : Insert action <color=#68b3e2>{1}</color> → <color=#ffa047>{2}{3}</color> at {4}\"",
                        string.IsNullOrEmpty(sequence.name) ? "[unnamed]" : sequence.name, sequenced.plugId, plugDataAction.label,
                        plugDataAction.wantsTarget ? ("(" + sequenced.target.name + ")") : "", sequenced.startTime * timeMultiplier
                ));
            }
            if (s == null) s = DOTween.Sequence();
            s.InsertCallback(sequenced.startTime * timeMultiplier,
                ()=> plugDataAction.action(
                    sequenced.target, sequenced.boolOption0, sequenced.toStringVal, sequenced.toFloatVal, sequenced.fromFloatVal,
                    sequenced.floatOption0, sequenced.floatOption1, sequenced.toIntVal
                )
            );
            if (plugDataAction.onCreation != null) {
                plugDataAction.onCreation(
                    sequenced.target, sequenced.boolOption0, sequenced.toStringVal, sequenced.toFloatVal, sequenced.fromFloatVal,
                    sequenced.floatOption0, sequenced.floatOption1, sequenced.toIntVal
                );
            }
        }

        static void InsertInterval(ref Sequence s, DOVisualSequence sequence, DOVisualSequenced sequenced, float timeMultiplier)
        {
            if (DOVisualSequenceSettings.I.debugLogs) {
                DOLog.Normal(
                    string.Format("DOVisualSequence <color=#d568e3>{0}</color> : Insert interval at {1}\"→{2}\"",
                        string.IsNullOrEmpty(sequence.name) ? "[unnamed]" : sequence.name, sequenced.startTime * timeMultiplier, sequenced.duration * timeMultiplier
                    ));
            }
            if (s == null) s = DOTween.Sequence();
            float sCurrDuration = s.Duration(false);
            float requiredDuration = (sequenced.startTime + sequenced.duration) * timeMultiplier;
            if (sCurrDuration < requiredDuration) s.AppendInterval(requiredDuration - sCurrDuration);
        }

        #endregion
        

        // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████
        // ███ INTERNAL CLASSES ████████████████████████████████████████████████████████████████████████████████████████████████
        // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

        [Serializable]
        public struct EditorData
        {
#pragma warning disable 0649
            /// <summary>Use directly only if you are moving the timeline, otherwise use roundedAreaShift</summary>
            public Vector2 areaShift;
#pragma warning restore 0649
            public Vector2Int roundedAreaShift {
                get { return new Vector2Int((int)areaShift.x, (int)areaShift.y); }
                set { areaShift = value; }
            }
        }

        // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

        [Serializable]
        public class VisualLayer
        {
#pragma warning disable 0649
            public bool isActive = true;
            public string[] sequencedGuids = new string[0];
            // Editor-only
            public string name; // Doesn't need to be univocal
            public bool locked;
            public Color color = DefColor;
#pragma warning restore 0649

            public static readonly Color DefColor = new Color(0.2f, 0.2f, 0.2f);

            public VisualLayer(string name)
            {
                this.name = name;
            }

#if UNITY_EDITOR
            public VisualLayer Editor_Clone()
            {
                return new VisualLayer(this.name) {
                    isActive = this.isActive,
                    sequencedGuids = this.Editor_CloneSequencedGuids(),
                    name = this.name,
                    locked = this.locked,
                    color = this.color
                };
            }

            string[] Editor_CloneSequencedGuids()
            {
                string[] result = new string[sequencedGuids.Length];
                for (int i = 0; i < sequencedGuids.Length; ++i) result[i] = sequencedGuids[i];
                return result;
            }
#endif
        }
    }
}