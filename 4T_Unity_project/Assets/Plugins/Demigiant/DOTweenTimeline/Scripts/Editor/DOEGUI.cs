// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/01/17

using System;
using System.Collections;
using DG.DemiEditor;
using DG.DOTweenEditor.UI;
using UnityEditor;
using UnityEngine;

namespace DG.Tweening.TimelineEditor
{
    internal static class DOEGUI
    {
        public static readonly DOEStylePalette Styles = new DOEStylePalette();
        public static readonly DOEColorPalette Colors = new DOEColorPalette();

        #region GUI

        public static void BeginGUI()
        {
            DeGUI.BeginGUI(Colors, Styles);
        }

        public static bool Button(Rect rect, string label, GUIStyle style = null)
        { return Button(rect, new GUIContent(label), style); }
        public static bool Button(Rect rect, GUIContent label, GUIStyle style = null)
        {
            return GUI.Button(rect, label, style == null ? Styles.global.bt : style);
        }
        public static bool Button(Rect rect, Texture2D img, GUIStyle style = null)
        {
            return GUI.Button(rect, img, style == null ? Styles.global.bt : style);
        }

        #endregion

        #region Public Methods

        public static Color GetVisibleContentColorOn(Color32 bgColor)
        {
            int lum = (int)Math.Sqrt(
                bgColor.r * bgColor.r * .299 +
                bgColor.g * bgColor.g * .587 +
                bgColor.b * bgColor.b * .114
            );
            return lum > 130 ? Color.black : Color.white;
//            // Old faster but obviously not as good method
//            return bgColor.r + bgColor.g + bgColor.b > 1.5f ? Color.black : Color.white;
        }

        #region MultiEditing

        public static bool MultiFilteredEasePopup(Rect rect, GUIContent label, string fieldName, IList targets)
        {
            using (var mScope = new DeGUI.MultiPropertyScope(fieldName, targets)) {
                mScope.value = EditorGUIUtils.FilteredEasePopup(rect, label.text, (Ease)mScope.fieldInfo.GetValue(targets[0]));
                return mScope.hasMixedValue;
            }
        }

        #endregion

        #endregion
    }
}