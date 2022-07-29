// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2019/03/22 18:57
// License Copyright (c) Daniele Giardini

using System;
using UnityEngine;

namespace DG.DeebugLib.GUISystems
{
    internal static class ImgDB
    {
        public static readonly ImgRef WhiteSquare = new ImgRef("Square_white");
        public static readonly ImgRef WhiteSquareCurved = new ImgRef("Square_white_curved");
        public static readonly ImgRef WhiteSquareEmptyCurved2Px = new ImgRef("Square_white_emptyCurved_2px");

        public static readonly ImgRef ScrollbarBg = new ImgRef("Scrollbar_Bg");
        public static readonly ImgRef ScrollbarThumb = new ImgRef("Scrollbar_Thumb");

        public static readonly ImgRef ToolbarBg = new ImgRef("Toolbar_Bg");
        public static readonly ImgRef ToolbarToggleOn = new ImgRef("Toolbar_Toggle_on");
        public static readonly ImgRef ToolbarToggleOnHover = new ImgRef("Toolbar_Toggle_on_hover");
        public static readonly ImgRef ToolbarToggleOff = new ImgRef("Toolbar_Toggle_off");
        public static readonly ImgRef ToolbarToggleOffHover = new ImgRef("Toolbar_Toggle_off_hover");
        public static readonly ImgRef ToolbarCloseButton = new ImgRef("Toolbar_Button_Close");
        public static readonly ImgRef ToolbarCloseButtonHover = new ImgRef("Toolbar_Button_Close_hover");
        public static readonly ImgRef ToolbarPauseButton = new ImgRef("Toolbar_Button_Pause");
        public static readonly ImgRef ToolbarPauseButtonHover = new ImgRef("Toolbar_Button_Pause_hover");
        public static readonly ImgRef ToolbarPlayButton = new ImgRef("Toolbar_Button_Play");
        public static readonly ImgRef ToolbarPlayButtonHover = new ImgRef("Toolbar_Button_Play_hover");
        public static readonly ImgRef ToolbarDeleteButton = new ImgRef("Toolbar_Button_Delete");
        public static readonly ImgRef ToolbarDeleteButtonHover = new ImgRef("Toolbar_Button_Delete_hover");

        public static readonly ImgRef LogCopyButton = new ImgRef("Log_Button_Copy");
        public static readonly ImgRef LogCopyButtonHover = new ImgRef("Log_Button_Copy_hover");
        public static readonly ImgRef LogMailButton = new ImgRef("Log_Button_Mail");
        public static readonly ImgRef LogMailButtonHover = new ImgRef("Log_Button_Mail_hover");

        public static readonly ImgRef LogBg = new ImgRef("Log_Bg");
        public static readonly ImgRef LogSelectedBorder = new ImgRef("Log_Selected_Border");
        public static readonly ImgRef LogSelectedBorderHD = new ImgRef("Log_Selected_Border_HD");

        public static readonly ImgRef PopupButton = new ImgRef("Popup_Button");
        public static readonly ImgRef PopupButtonHover = new ImgRef("Popup_Button_hover");

        public static readonly ImgRef IcoLog = new ImgRef("Ico_Log");
        public static readonly ImgRef IcoWarning = new ImgRef("Ico_Warning");
        public static readonly ImgRef IcoError = new ImgRef("Ico_Error");
        public static readonly ImgRef IcoTime = new ImgRef("Ico_Time");
        public static readonly ImgRef IcoFontUpscale = new ImgRef("Ico_Font_Upscale");
        public static readonly ImgRef IcoScene = new ImgRef("Ico_Scene");
        public static readonly ImgRef IcoUpFast = new ImgRef("Ico_Up_Fast");
        public static readonly ImgRef IcoDownFast = new ImgRef("Ico_Down_Fast");
        public static readonly ImgRef IcoOk = new ImgRef("Ico_Ok");
        public static readonly ImgRef IcoCancel = new ImgRef("Ico_Cancel");
        public static readonly ImgRef IcoMailTo = new ImgRef("Ico_MailTo");

        static event Action OnRequestLoad;

        public static void Load()
        {
            if (OnRequestLoad != null) OnRequestLoad();
        }

        public static Texture2D GetLogTypeIcon(LogType logType)
        {
            switch (logType) {
            case LogType.Log: return IcoLog.img;
            case LogType.Warning: return IcoWarning.img;
            default: return IcoError.img;
            }
        }

        // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████
        // ███ INTERNAL CLASSES ████████████████████████████████████████████████████████████████████████████████████████████████
        // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

        internal class ImgRef
        {
            public Texture2D img;
            public string resourceId;

            const string _ResourcePath = "DeebugGUI/";

            public ImgRef(string resourceId)
            {
                this.resourceId = _ResourcePath + resourceId;
                OnRequestLoad += LoadImg;
            }

            void LoadImg()
            {
                img = Resources.Load<Texture2D>(resourceId);
                OnRequestLoad -= LoadImg;
            }

            public static implicit operator Texture2D(ImgRef v) { return v.img; }
        }
    }
}