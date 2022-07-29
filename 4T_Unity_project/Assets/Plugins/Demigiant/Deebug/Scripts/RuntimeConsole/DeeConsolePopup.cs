// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2019/03/30 13:49
// License Copyright (c) Daniele Giardini

using System;
using DG.DeebugLib.Extensions;
using DG.DeebugLib.GUISystems;
using DG.DeExtensions;
using UnityEngine;

namespace DG.DeebugLib.RuntimeConsole
{
#if UNITY_2018_1_OR_NEWER
    [UnityEngine.Scripting.Preserve] // Otherwise iOS erroneously tries to remove this class
#endif
    internal static class DeeConsolePopup
    {
        internal enum PopupType
        {
            Normal,
            Error,
            ErrorWithMailTo
        }

        public static bool isOpen { get; private set; }

        static PopupType _popupType;
        static GUIContent _message;
        static bool _hasCancelButton;
        static Action _onOk;
        static Rect _winRect;
        static Rect _contentRect;
        static Rect _footerRect;

        #region GUI

        public static void Draw()
        {
            int border = 60.ScaledInt();
            int btsBorder = 8.ScaledInt();
            int btsW = (ImgDB.PopupButton.img.width * 0.5f).ScaledInt();
            int btsH = (ImgDB.PopupButton.img.height * 0.5f).ScaledInt();
            int footerH = btsH + btsBorder;
            float winW = Mathf.Min(400.ScaledInt(), Screen.width - border * 2);
            float contentH = Style.Popup.contentLabel.CalcHeight(_message, winW);
            float winH = Mathf.Min(contentH + footerH, Screen.height - border * 2);
            _winRect = new Rect((int)(Screen.width * 0.5f - winW * 0.5f), (int)(Screen.height * 0.5f - winH * 0.5f), winW, winH);
            _footerRect = new Rect(_winRect.x, _winRect.yMax - footerH, _winRect.width, footerH);
            _contentRect = new Rect(_winRect.x, _winRect.y, _winRect.width, _winRect.height - _footerRect.height);

            // Hiding background
            GUI.color = new Color(0, 0, 0, 0.3f);
            GUI.DrawTexture(new Rect(0, 0, Screen.width, Screen.height), ImgDB.WhiteSquare);
            // Border
            GUI.color = _popupType == PopupType.Normal ? Color.black : Color.white;
            GUI.DrawTexture(_winRect.Expand(2.ScaledInt()), ImgDB.WhiteSquare);
            GUI.color = Color.white;

            DrawWindow(btsBorder, btsW, btsH);
        }

        static void DrawWindow(int btsBorder, int btW, int btH)
        {
            int btsDist = 5.ScaledInt();
            Rect btCancelR = new Rect(_footerRect.xMax - btW - btsBorder, _footerRect.yMax - btH - btsBorder, btW, btH);
            Rect btOkR = _hasCancelButton ? btCancelR.Shift(-btW - btsDist, 0, 0, 0) : btCancelR;

            // Window + content
            switch (_popupType) {
            case PopupType.Error:
            case PopupType.ErrorWithMailTo:
                GUI.color = new Color(0.93f, 0.09f, 0.05f);
                GUI.contentColor = Color.white;
                break;
            default:
                GUI.color = Color.white;
                GUI.contentColor = Color.black;
                break;
            }
            GUI.DrawTexture(_winRect, ImgDB.WhiteSquare);
            GUI.color = Color.white;
            GUI.Label(_contentRect, _message, Style.Popup.contentLabel);
            GUI.contentColor = Color.white;

            // Footer
            if (PopupButton(btOkR, _popupType == PopupType.ErrorWithMailTo ? ImgDB.IcoMailTo : _hasCancelButton ? ImgDB.IcoOk : ImgDB.IcoCancel)) {
                Action action = _onOk;
                _onOk = null;
                Event.current.Use();
                Close();
                if (action != null) action();
            }
            if (_hasCancelButton) {
                if (PopupButton(btCancelR, ImgDB.IcoCancel)) {
                    Event.current.Use();
                    Close();
                }
            }
        }

        #region GUI Helpers (also used by DeeConsoleCopyPopup)

        public static bool PopupButton(Rect r, Texture2D img, Color? color = null)
        {
            Color c = color == null ? new Color(0.18f, 0.18f, 0.18f) : (Color)color;
            GUI.backgroundColor = c;
            bool clicked = GUI.Button(r, "", Style.Popup.bt);
            GUI.backgroundColor = Color.white;
            Rect imgR = r.Contract(r.width * 0.18f);
            GUI.DrawTexture(imgR, img, ScaleMode.ScaleToFit);
            return clicked;
        }

        #endregion

        #endregion

        #region Public Methods

        public static void Open(string message, PopupType type = PopupType.Normal, Action onOk = null)
        {
            isOpen = true;
            _popupType = type;
            _message = new GUIContent(message);
            _onOk = onOk;
            _hasCancelButton = _onOk != null;
        }

        #endregion

        #region Methods

        static void Close()
        {
            isOpen = false;
        }

        #endregion
    }
}