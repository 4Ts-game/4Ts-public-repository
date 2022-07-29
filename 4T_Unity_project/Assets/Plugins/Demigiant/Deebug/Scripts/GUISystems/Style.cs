// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2019/03/22 16:51
// License Copyright (c) Daniele Giardini

using DG.DeebugLib.Extensions;
using DG.DeebugLib.RuntimeConsole;
using DG.DemiLib;
using UnityEngine;

namespace DG.DeebugLib.GUISystems
{
    internal static class Style
    {
        public static bool useBigFonts = false;

        static float _fontSizeMultiplier = 1;

        public static void Init()
        {
            _fontSizeMultiplier = useBigFonts ? 1.25f : 1f;
            ImgDB.Load();
            Global.Init();
            Toolbar.Init();
            Log.Init();
            LogEvidence.Init();
            Popup.Init();
            CopyPopup.Init();
        }

        // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████
        // ███ STYLES ██████████████████████████████████████████████████████████████████████████████████████████████████████████
        // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

        internal static class Global
        {
            public static GUIStyle defVScrollbarThumb, defHScrollbarThumb,
                                   vScrollbar, hScrollbar, vScrollbarThumb, hScrollbarThumb,
                                   vScrollbarBt;

            public static void Init()
            {
                defVScrollbarThumb = GUI.skin.verticalScrollbarThumb;
                defHScrollbarThumb = GUI.skin.horizontalScrollbarThumb;

                int border = 6;
                int size = 30.ScaledInt();
                vScrollbar = new GUIStyle(GUI.skin.verticalScrollbar).Width(size).Padding(0).Margin(0)
                    .Background(ImgDB.ScrollbarBg).Border(border).Padding(0, 0, size, size).Overflow(0, 0, -size, -size);
                hScrollbar = new GUIStyle(GUI.skin.horizontalScrollbar).Height(size).Padding(0).Margin(0)
                    .Background(ImgDB.ScrollbarBg).Border(border);
                vScrollbarThumb = new GUIStyle(GUI.skin.verticalScrollbarThumb).Width(vScrollbar.fixedWidth)
                    .Background(ImgDB.ScrollbarThumb).Border(border);
                hScrollbarThumb = new GUIStyle(GUI.skin.horizontalScrollbarThumb).Height(hScrollbar.fixedHeight)
                    .Background(ImgDB.ScrollbarThumb).Border(border);
                vScrollbarBt = new GUIStyle().Width(vScrollbar.fixedWidth).Height(vScrollbar.fixedWidth)
                    .Background(ImgDB.ScrollbarThumb).Border(border);
            }
        }

        internal static class Toolbar
        {
            public static GUIStyle bg, logToggleOn, logToggleOff, btClose, btPause, btResume, btDelete, versionLabel;

            public static void Init()
            {
                bg = new GUIStyle().Background(ImgDB.ToolbarBg);
                logToggleOn = new GUIStyle().Add(Color.white, 10.ScaledInt(), TextAnchor.LowerCenter)
                    .Background(ImgDB.ToolbarToggleOn, ImgDB.ToolbarToggleOnHover).Border(6)
                    .Padding(4.ScaledInt(), 4.ScaledInt(), 0, 4.ScaledInt());
                logToggleOff = logToggleOn.Clone().Background(ImgDB.ToolbarToggleOff, ImgDB.ToolbarToggleOffHover);
                btClose = new GUIStyle().Background(ImgDB.ToolbarCloseButton, ImgDB.ToolbarCloseButtonHover);
                btPause = new GUIStyle().Background(ImgDB.ToolbarPauseButton, ImgDB.ToolbarPauseButtonHover);
                btResume = new GUIStyle().Background(ImgDB.ToolbarPlayButton, ImgDB.ToolbarPlayButtonHover);
                btDelete = new GUIStyle().Background(ImgDB.ToolbarDeleteButton, ImgDB.ToolbarDeleteButtonHover);
                versionLabel = new GUIStyle().Add(11.ScaledInt(), new DeSkinColor(0.3f), TextAnchor.LowerRight, Format.NoRichText, Format.NoWordWrap)
                    .PaddingBottom(3.ScaledInt());
            }
        }

        internal static class Log
        {
            public static GUIStyle bg, logLabel, timeLabel, frameLabel, sceneLabel, rowEvidence;

            public static void Init()
            {
                bg = new GUIStyle().Background(ImgDB.LogBg).Border(2);
                logLabel = new GUIStyle().Add((12 * _fontSizeMultiplier).ScaledInt(), TextAnchor.UpperLeft, Color.white).Clipping(TextClipping.Clip)
                    .Padding(0, 1, (7 / _fontSizeMultiplier).ScaledInt(), 1.ScaledInt());
                timeLabel = new GUIStyle().Add(10.ScaledInt(), TextAnchor.MiddleCenter, new DeSkinColor(0.8f)).Clipping(TextClipping.Clip)
                    .Background(ImgDB.WhiteSquareCurved).Border(6).ContentOffsetX(2);
                frameLabel = timeLabel.Clone().ContentOffsetX(1);
                sceneLabel = frameLabel.Clone(11.ScaledInt(), TextAnchor.MiddleLeft).Padding(4.ScaledInt(), 4.ScaledInt(), 0, 0);
                rowEvidence = DeeGUI.guiScale >= 2
                    ? new GUIStyle().Background(ImgDB.LogSelectedBorderHD).Border(16)
                    : new GUIStyle().Background(ImgDB.LogSelectedBorder).Border(8);
            }
        }

        internal static class LogEvidence
        {
            public static GUIStyle bg, logLabel, stackLabel, btCopy, btMail;

            public static void Init()
            {
                bg = new GUIStyle().Background(ImgDB.LogBg).Border(2);
                logLabel = new GUIStyle().Add((12 * _fontSizeMultiplier).ScaledInt(), TextAnchor.UpperLeft, Color.white, Format.WordWrap, Format.RichText)
                    .Padding(6.ScaledInt(), 36.ScaledInt(), 8.ScaledInt(), 6.ScaledInt());
                stackLabel = logLabel.Clone((12 * _fontSizeMultiplier).ScaledInt(), new DeSkinColor(0.75f)).PaddingTop(0);
                btCopy = new GUIStyle().Background(ImgDB.LogCopyButton, ImgDB.LogCopyButtonHover);
                btMail = new GUIStyle().Background(ImgDB.LogMailButton, ImgDB.LogMailButtonHover);
            }
        }

        internal static class Popup
        {
            public static GUIStyle contentLabel, bt;

            public static void Init()
            {
                contentLabel = new GUIStyle().Add((14 * _fontSizeMultiplier).ScaledInt(), new DeSkinColor(1f), TextAnchor.MiddleCenter, Format.WordWrap, Format.RichText)
                    .Padding(20.ScaledInt(), 20.ScaledInt(), 20.ScaledInt(), 24.ScaledInt());
                bt = new GUIStyle().Background(ImgDB.PopupButton, ImgDB.PopupButtonHover);
            }
        }

        internal static class CopyPopup
        {
            public static GUIStyle headerLabel, contentLabel, bt;

            public static void Init()
            {
                headerLabel = new GUIStyle().Add(15.ScaledInt(), new DeSkinColor(1f), TextAnchor.MiddleCenter, Format.WordWrap, Format.RichText)
                    .Padding(10.ScaledInt());
                contentLabel = new GUIStyle().Add((12 * _fontSizeMultiplier).ScaledInt(), new DeSkinColor(0.9f), TextAnchor.UpperLeft, Format.WordWrap, Format.NoRichText)
                    .Padding(10.ScaledInt());
                bt = Popup.bt.Clone();
            }
        }
    }
}