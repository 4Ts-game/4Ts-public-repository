// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2019/03/25 18:09
// License Copyright (c) Daniele Giardini

using DG.DeebugLib.Extensions;
using DG.DeebugLib.GUISystems;
using DG.DeExtensions;
using DG.DemiLib;
using UnityEngine;

namespace DG.DeebugLib.RuntimeConsole
{
    internal static class DeeConsoleCopyPopup
    {
        public static bool isOpen { get; private set; }

        static DeeGUI.ScrollDragArea _scrollArea;
        static GUIContent _content;
        static string _copyStr;
        static Rect _winRect;
        static DeeGUI.TweenUnit _openTween;

        #region GUI

        public static void Draw()
        {
            int border = 40.ScaledInt();
            float winW = Mathf.Min(500.ScaledInt(), Screen.width - border * 2);
            float winH = Mathf.Min(700.ScaledInt(), Screen.height - border * 2);
            _winRect = new Rect((int)(Screen.width * 0.5f - winW * 0.5f), (int)(Screen.height * 0.5f - winH * 0.5f), winW, winH);

            // Tween
            if (_openTween.value < 1) {
                float offset = -_winRect.center.y - _winRect.height * 0.5f;
                _winRect.y += offset * (1 - _openTween.value);
            }

            // Hiding background
            GUI.color = new Color(0, 0, 0, 0.3f * _openTween.value);
            GUI.DrawTexture(new Rect(0, 0, Screen.width, Screen.height), ImgDB.WhiteSquare);
            // Border
            GUI.color = Color.white;
            GUI.DrawTexture(_winRect.Expand(2.ScaledInt()), ImgDB.WhiteSquare);

            DrawWindow();
        }

        static void DrawWindow()
        {
            const string headerStr = "Copy the following information to the clipboard?";

            int btW = (ImgDB.PopupButton.img.width * 0.5f).ScaledInt();
            int btH = (ImgDB.PopupButton.img.height * 0.5f).ScaledInt();
            float btsDist = 3.ScaledInt();
            float btsBorder = 1.ScaledInt();
            Rect area = _winRect;
            float headerH = Style.CopyPopup.headerLabel.CalcHeight(new GUIContent(headerStr), area.width);
            Rect headerR = new Rect(area.x, area.y, area.width, headerH);
            Rect footerR = new Rect(area.x, area.yMax - 40.ScaledInt(), area.width, 40.ScaledInt());
            Rect contentR = area.ShiftYAndResize(headerR.height).Shift(0, 0, 0, -footerR.height);
            Rect contentTextR = contentR.SetWidth(contentR.width - Style.Global.vScrollbar.fixedWidth + 1);
            float contentH = Style.CopyPopup.contentLabel.CalcHeight(_content, contentTextR.width);
            Rect btCancelR = new Rect(footerR.xMax - btW - btsBorder, footerR.yMax - btH - btsBorder, btW, btH);
            Rect btOkR = btCancelR.Shift(-btW - btsDist, 0, 0, 0);

            // Header
            GUI.color = DeeConsoleGUI.EvidenceColor;
            GUI.DrawTexture(headerR, ImgDB.WhiteSquare);
            GUI.color = Color.white;
            GUI.Label(headerR, headerStr, Style.CopyPopup.headerLabel);

            // Scroll area
            GUI.color = new DeSkinColor(0.1f);
            GUI.DrawTexture(contentR, ImgDB.WhiteSquare);
            GUI.color = Color.white;
            _scrollArea.BeginScrollView(contentR, new Rect(contentR.x, contentR.y, contentR.width, contentH));
            GUI.Label(contentTextR, _content, Style.CopyPopup.contentLabel);
            _scrollArea.EndScrollView();

            // Footer
            if (DeeConsolePopup.PopupButton(btOkR, ImgDB.IcoOk)) {
                UniClipboard.SetText(_copyStr);
                Event.current.Use();
                Close();
            }
            if (DeeConsolePopup.PopupButton(btCancelR, ImgDB.IcoCancel)) {
                Event.current.Use();
                Close();
            }
        }

        #endregion

        #region Public Methods

        public static void Open(LogData log)
        {
            isOpen = true;
            _copyStr = log.ConverToMailFormat();
            _content = new GUIContent(_copyStr);
            if (_scrollArea == null) _scrollArea = new DeeGUI.ScrollDragArea(true, false);
            else _scrollArea.SendToTop();
            if (_openTween == null) _openTween = new DeeGUI.TweenUnit(0.16f);
            _openTween.Rewind();
            _openTween.PlayForward();
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