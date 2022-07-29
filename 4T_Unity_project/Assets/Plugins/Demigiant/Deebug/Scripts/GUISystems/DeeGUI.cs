// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2019/03/23 11:55
// License Copyright (c) Daniele Giardini

using System;
using System.Collections.Generic;
using DG.DeebugLib.Extensions;
using DG.DeExtensions;
using UnityEngine;

namespace DG.DeebugLib.GUISystems
{
    internal static class DeeGUI
    {
        public static float guiScale { get; private set; }

        static readonly List<TweenUnit> _Tweens = new List<TweenUnit>();
        static int _currScreenW, _currScreenH;

        #region Public Methods

        public static void BeginGUI()
        {
            for (int i = 0; i < _Tweens.Count; ++i) _Tweens[i].Update();
            DoRefreshGUIScale(true);
        }

        public static void RefreshGUIScale()
        {
            DoRefreshGUIScale(false);
        }

        #endregion

        #region Methods

        static void DoRefreshGUIScale(bool fullGUIProcess)
        {
            if (_currScreenW == Screen.width && _currScreenH == Screen.height) return;

            // Screen size changed or first call: set gui scale, screen area and regenerate styles
            float dpi = DeeUtils.GetDPI();
            if (dpi < 150) guiScale = 1; // PC-based DPI
            else guiScale = DeeUtils.GetDPI() / 150; // Should be 96 but that's only because DPI resolution is false on PC
            guiScale /= DeeUtils.GetGameViewScale();
            if (fullGUIProcess) {
                _currScreenW = Screen.width;
                _currScreenH = Screen.height;
                Style.Init();
            }
        }

        #endregion

        // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████
        // ███ INTERNAL CLASSES ████████████████████████████████████████████████████████████████████████████████████████████████
        // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

        /// <summary>
        /// Tweens a unit value from 0 to 1 or viceversa
        /// </summary>
        public class TweenUnit
        {
            public float value { get; private set; }
            readonly float _duration;
            float _elapsed;
            float _time;
            bool _isPlaying;
            bool _isBackwards;
            Action _onPlayForwardComplete, _onPlayBackwardsComplete;

            public TweenUnit(float duration)
            {
                _duration = duration;
                _Tweens.Add(this);
            }

            public void PlayForward(Action onComplete = null)
            {
                _isPlaying = true;
                _isBackwards = false;
                _onPlayForwardComplete = onComplete;
                _time = Time.realtimeSinceStartup;
            }

            public void PlayBackwards(Action onComplete = null)
            {
                _isPlaying = true;
                _isBackwards = true;
                _onPlayBackwardsComplete = onComplete;
                _time = Time.realtimeSinceStartup;
            }

            public void Rewind()
            {
                _isPlaying = false;
                _elapsed = value = 0;
            }

            // Called by DeeGUI
            public void Update()
            {
                if (!_isPlaying) return;

                bool completed = false;
                float realtime = Time.realtimeSinceStartup;
                float currElapsed = realtime - _time;
                _time = realtime;
                if (_isBackwards) {
                    _elapsed -= currElapsed;
                    if (_elapsed < 0) {
                        completed = true;
                        _elapsed = 0;
                    }
                } else {
                    _elapsed += currElapsed;
                    if (_elapsed > _duration) {
                        completed = true;
                        _elapsed = _duration;
                    }
                }
                float time = _elapsed;
                value = -(time /= _duration) * (time - 2); // outQuad ease
                if (completed) {
                    _isPlaying = false;
                    Action action;
                    if (_isBackwards) {
                        value = 0;
                        action = _onPlayBackwardsComplete;
                        _onPlayBackwardsComplete = null;
                    } else {
                        value = 1;
                        action = _onPlayForwardComplete;
                        _onPlayForwardComplete = null;
                    }
                    if (action != null) action();
                }
            }
        }

        public class ScrollDragArea
        {
            public Vector2 scrollPosition { get { return _scrollPosition; } }
            Vector2 _scrollPosition;
            readonly bool _dragHorizontally, _dragVertically;
            bool _sendToBottomRequested;
            bool _isDragging;
            Rect _position;
            Rect _fullViewRect;
            Vector2 _currDragMouseP;

            public ScrollDragArea(bool dragVertically = true, bool dragHorizontally = true)
            {
                _dragVertically = dragVertically;
                _dragHorizontally = dragHorizontally;
            }

            /// <summary>
            /// Returns the actual full view area (with X and Y set to 0), meaning the position Rect without eventual scrollbars and full height
            /// </summary>
            public Rect BeginScrollView(Rect position, Rect fullViewRect)
            {
                _position = position;
                _fullViewRect = fullViewRect;
                GUI.skin.verticalScrollbarThumb = Style.Global.vScrollbarThumb;
                GUI.skin.horizontalScrollbarThumb = Style.Global.hScrollbarThumb;
                if (_sendToBottomRequested) {
                    _sendToBottomRequested = false;
                    DoSendToBottom();
                }
                bool vSCrollbarVisible = _dragVertically && fullViewRect.height > position.height;
                bool hScrollbarVisible = _dragHorizontally && fullViewRect.width > position.width;
                if (vSCrollbarVisible && !_dragHorizontally) {
                    fullViewRect.width -= Style.Global.vScrollbar.fixedWidth + 1;
                }
                if (hScrollbarVisible && !_dragVertically) {
                    fullViewRect.height -= Style.Global.hScrollbar.fixedHeight + 1;
                }

                if (vSCrollbarVisible) {
                    // To top/bottom buttons
                    int size = (int)Style.Global.vScrollbarBt.fixedWidth;
                    Rect btTopR = new Rect(_position.xMax - size, _position.y, size, size);
                    Rect btBottomR = btTopR.SetY(_position.yMax - size);
                    if (GUI.Button(btTopR, "", Style.Global.vScrollbarBt)) {
                        _scrollPosition.y = 0;
                    }
                    if (GUI.Button(btBottomR, "", Style.Global.vScrollbarBt)) {
                        DoSendToBottom();
                    }
                    // Icons
                    GUI.DrawTexture(btTopR.Contract(10.ScaledInt()), ImgDB.IcoUpFast, ScaleMode.ScaleToFit);
                    GUI.DrawTexture(btBottomR.Contract(10.ScaledInt()), ImgDB.IcoDownFast, ScaleMode.ScaleToFit);
                }

                _scrollPosition = GUI.BeginScrollView(position, _scrollPosition, fullViewRect, Style.Global.hScrollbar, Style.Global.vScrollbar);

                return fullViewRect;
            }

            public void EndScrollView()
            {
                GUI.EndScrollView();
                GUI.skin.verticalScrollbarThumb = Style.Global.defVScrollbarThumb;
                GUI.skin.horizontalScrollbarThumb = Style.Global.defHScrollbarThumb;

                // Manage drag-scroll
                if (_isDragging) {
                    switch (Event.current.type) {
                    case EventType.MouseDrag:
                        Vector2 offset = Event.current.mousePosition - _currDragMouseP;
                        _currDragMouseP = Event.current.mousePosition;
                        if (_dragHorizontally) _scrollPosition.x -= offset.x;
                        if (_dragVertically) _scrollPosition.y -= offset.y;
                        ClampScrollPos();
                        break;
                    case EventType.MouseUp:
                        _isDragging = false;
                        break;
                    }
                } else {
                    if (Event.current.type == EventType.MouseDown && _position.Contains(Event.current.mousePosition)) {
                        _isDragging = true;
                        _currDragMouseP = Event.current.mousePosition;
                    }
                }
            }

            public void SendToX(float scrollX)
            { DoSendTo(new Vector2(scrollX, _scrollPosition.y)); }
            public void SendToY(float scrollY)
            { DoSendTo(new Vector2(_scrollPosition.x, scrollY)); }
            public void SendTo(Vector2 scrollP)
            { DoSendTo(scrollP); }
            public void DoSendTo(Vector2 scrollP)
            {
                _scrollPosition = scrollP;
                ClampScrollPos();
            }

            public void SendToTop()
            {
                _scrollPosition.y = 0;
            }
            public void SendToBottom()
            {
                _sendToBottomRequested = true;
            }
            void DoSendToBottom()
            {
                _scrollPosition.y = _fullViewRect.height - _position.height;
            }

            void ClampScrollPos()
            {
                if (_scrollPosition.y < 0) _scrollPosition.y = 0;
                else if (_scrollPosition.y > _fullViewRect.height - _position.height) _scrollPosition.y = _fullViewRect.height - _position.height;
                if (_scrollPosition.x > 0) _scrollPosition.x = 0;
                else if (_scrollPosition.x > _fullViewRect.width - _position.width) _scrollPosition.x = _fullViewRect.width - _position.width;
            }
        }
    }
}