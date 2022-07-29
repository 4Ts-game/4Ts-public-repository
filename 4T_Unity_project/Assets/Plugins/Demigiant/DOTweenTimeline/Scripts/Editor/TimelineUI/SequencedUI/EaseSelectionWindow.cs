// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/02/04

using System;
using System.Collections.Generic;
using DG.DemiEditor;
using DG.DOTweenEditor.UI;
using DG.Tweening.Timeline.Core;
using UnityEditor;
using UnityEngine;

namespace DG.Tweening.TimelineEditor.SequencedUI
{
    class EaseSelectionWindow : PopupWindowContent
    {
        DOVisualSequenceTimeline _editor { get { return DOVisualSequenceTimeline.editor; } }

        const int _Padding = 4;
        const int _Offset = _Padding * 2;
        const int _TotEasesXRow = 6;
        static readonly Vector2Int _EaseSize = new Vector2Int(80, 60);
        readonly Color _selectionColor = new Color(0f, 0.98f, 0.28f);
        bool _initialized;
        string[] _filteredEases;
        GUIContent[] _gcEases;
        Texture2D[] _easeImgs;
        string[] _selectedEases;
        List<DOVisualSequenced> _sequencedsRefs;
        int _totEases;

        void Init()
        {
            if (_initialized) return;

            _initialized = true;

            _totEases = EditorGUIUtils.FilteredEaseTypes.Length;
            _gcEases = new GUIContent[_totEases];
            // Sort eases and place Linear at the end before custom
            _filteredEases = new string[_totEases];
            for (int i = 0; i < _totEases; ++i) {
                _filteredEases[i] = EditorGUIUtils.FilteredEaseTypes[i];
                _gcEases[i] = new GUIContent(i == _totEases - 1 ? "Custom\nCurve" : _filteredEases[i]);
            }
            _filteredEases.Shift(0, _totEases - 2);
            _gcEases.Shift(0, _totEases - 2);
            //
            _easeImgs = new Texture2D[_totEases];
            for (int i = 0; i < _totEases; ++i) {
                Texture2D tex = new Texture2D(_EaseSize.x, _EaseSize.y);
                Ease ease = FilteredEaseToEase(i);
                float overshootOrAmplitude = 1.70158f;
                bool isBack = ease == Ease.InBack || ease == Ease.OutBack || ease == Ease.InOutBack;
                bool isFlash = !isBack && (ease == Ease.Flash || ease == Ease.InFlash || ease == Ease.OutFlash || ease == Ease.InOutFlash);
                if (isFlash) overshootOrAmplitude = 8;
                else if (isBack) overshootOrAmplitude = 3;
                TimelineEditorUtils.GenerateEaseTextureIn(tex, ease, overshootOrAmplitude);
                _easeImgs[i] = tex;
            }
        }

        #region Unity + UI

        public override void OnOpen()
        {
            Init();
        }

        public override void OnClose()
        {
            base.OnClose();
        }

        public override Vector2 GetWindowSize()
        {
            int totRows = (int)(_filteredEases.Length / (float)_TotEasesXRow);
            return new Vector2(
                _EaseSize.x * _TotEasesXRow + _Offset * (_TotEasesXRow - 1) + _Padding * 2,
                _EaseSize.y * totRows + _Offset * (totRows - 1) + _Padding * 2
            );
        }

        public override void OnGUI(Rect rect)
        {
            DOEGUI.BeginGUI();
            Rect btR = new Rect(rect.x + _Padding, rect.y + _Padding, _EaseSize.x, _EaseSize.y);
            for (int i = 0; i < _totEases; ++i) {
                if (i % _TotEasesXRow == 0) {
                    if (i > 0) {
                        btR.x = rect.x + _Padding;
                        btR.y += _EaseSize.y + _Offset;
                    }
                } else btR.x += _EaseSize.x + _Offset;
                if (GUI.Button(btR.Expand(_Padding), GUIContent.none, DOEGUI.Styles.easePopup.btEase)) {
                    using (new DOScope.UndoableSerialization(false, true)) {
                        Ease mainEase = _sequencedsRefs[0].ease;
                        Ease newEase = FilteredEaseToEase(i);
                        if (EaseMainTypeIsDifferent(mainEase, newEase)) {
                            float overshootOrAmplitude, period;
                            // Reset overshoot, amplitude and period
                            switch (newEase) {
                            case Ease.Flash:
                            case Ease.InFlash:
                            case Ease.OutFlash:
                            case Ease.InOutFlash:
                                overshootOrAmplitude = 16;
                                period = 0;
                                break;
                            default:
                                overshootOrAmplitude = 1.70158f;
                                period = 0.05f;
                                break;
                            }
                            foreach (DOVisualSequenced sequenced in _sequencedsRefs) {
                                sequenced.overshootOrAmplitude = overshootOrAmplitude;
                                sequenced.period = period;
                            }
                        }
                        foreach (DOVisualSequenced sequenced in _sequencedsRefs) {
                            sequenced.ease = newEase;
                        }
                    }
                    this.editorWindow.Close();
                    DOVisualSequenceTimeline.Dispatch_OnSequenceChanged(DOVisualSequenceTimeline.sequence);
                }
                GUI.DrawTexture(btR, _easeImgs[i]);
                EditorGUI.DropShadowLabel(
                    btR, _gcEases[i], i == _totEases - 1 ? DOEGUI.Styles.easePopup.easeLabelCustom : DOEGUI.Styles.easePopup.easeLabel
                );
                // Mark selected eases
                for (int j = 0; j < _selectedEases.Length; ++j) {
                    if (_selectedEases[j] != _filteredEases[i]) continue;
                    if (!DeGUI.IsProSkin) { // Add black border
                        using (new DeGUI.ColorScope(null, null, Color.black)) {
                            GUI.Box(btR.Expand(1), GUIContent.none, DOEGUI.Styles.easePopup.selectionBox);
                        }
                    }
                    using (new DeGUI.ColorScope(null, null, _selectionColor)) {
                        GUI.Box(btR, GUIContent.none, DOEGUI.Styles.easePopup.selectionBox);
                    }
                    break;
                }
            }
        }

        #endregion

        #region Public Methods

        public void Prepare(List<DOVisualSequenced> sequenceds)
        {
            _sequencedsRefs = sequenceds;
            _selectedEases = new string[sequenceds.Count];
            for (int i = 0; i < _selectedEases.Length; ++i) {
                _selectedEases[i] = sequenceds[i].ease.ToString();
            }
        }

        #endregion

        #region Methods

        bool EaseMainTypeIsDifferent(Ease a, Ease b)
        {
            return GetEaseMainTypeStr(a) != GetEaseMainTypeStr(b);
        }

        string GetEaseMainTypeStr(Ease ease)
        {
            string result = ease.ToString();
            return result.StartsWith("InOut")
                ? result.Substring(5)
                : result.StartsWith("In")
                    ? result.Substring(2)
                    : result.StartsWith("Out")
                        ? result.Substring(3)
                        : result;
        }

        Ease FilteredEaseToEase(int filteredIndex)
        {
            if (filteredIndex == _filteredEases.Length - 1) return Ease.INTERNAL_Custom;
            return (Ease)Enum.Parse(typeof(Ease), _filteredEases[filteredIndex]);
        }

        #endregion
    }
}