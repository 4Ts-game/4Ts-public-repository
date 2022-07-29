// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2019/03/22 20:36
// License Copyright (c) Daniele Giardini

using DG.DeebugLib.GUISystems;

namespace DG.DeebugLib.Extensions
{
    public static class GUIScaleExtensions
    {
        #region Public Methods

        public static int ScaledInt(this float n)
        {
            return (int)(n * DeeGUI.guiScale);
        }

        public static int ScaledInt(this int n)
        {
            return (int)(n * DeeGUI.guiScale);
        }

        public static float Scaled(this float n)
        {
            return n * DeeGUI.guiScale;
        }

        public static float Scaled(this int n)
        {
            return n * DeeGUI.guiScale;
        }

        #endregion
    }
}