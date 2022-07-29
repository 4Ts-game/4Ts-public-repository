// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/01/17

using DG.DemiEditor;
using DG.DemiLib;
using UnityEditor;
using UnityEngine;

namespace DG.Tweening.TimelineEditor
{
    internal class DOEStylePalette : DeStylePalette
    {
        public readonly GlobalStyles global = new GlobalStyles();
        public readonly TimelineStyles timeline = new TimelineStyles();
        public readonly EasePopupStyles easePopup = new EasePopupStyles();
    }

    internal class GlobalStyles : DeStyleSubPalette
    {
        public GUIStyle bt, btInvisible, warningLabelBox, prefixLabel, toggleSmlLabel, errorBoxLabel;

        public override void Init()
        {
            bt = new GUIStyle(GUI.skin.button).Add(TextAnchor.MiddleCenter).Padding(2, 2, 0, 0).ContentOffset(0, -1);
            btInvisible = new GUIStyle();
            warningLabelBox = new GUIStyle(GUI.skin.label).Add(Color.white, TextAnchor.MiddleCenter, Format.RichText, Format.WordWrap)
                .Background(DeStylePalette.squareBorderCurved02_darkBorders).Border(6, 6, 6, 6).Padding(10);
            prefixLabel = new GUIStyle(GUI.skin.label).PaddingLeft(1).PaddingRight(0);
            toggleSmlLabel = DeGUI.styles.button.bBlankBorder.Clone(10);
            errorBoxLabel = new GUIStyle(GUI.skin.label).Add(Color.white, Format.RichText, Format.WordWrap, TextAnchor.UpperLeft)
                .Padding(5, 5, 2, 0).Background(DeStylePalette.whiteSquare);
        }
    }

    internal class EasePopupStyles : DeStyleSubPalette
    {
        public GUIStyle btEase, easeLabel, easeLabelCustom, selectionBox;

        public override void Init()
        {
            btEase = DeGUI.styles.button.def.Clone().Padding(0);
            easeLabel = new GUIStyle(GUI.skin.label).Add(12, new Color(0.91f, 0.9f, 0.95f), TextAnchor.LowerCenter).PaddingBottom(3);
            easeLabelCustom = easeLabel.Clone(TextAnchor.MiddleCenter).Padding(0);
            selectionBox = DeGUI.styles.box.outline02.Clone().Padding(0);
        }
    }

    internal class TimelineStyles : DeStyleSubPalette
    {
        public GUIStyle disconnectedLabel,
                        gHeaderCrumb, gHeaderCrumbSequence, gHeaderCrumbsMidLabel, gHeaderSequenceLabel, gHeaderTimeScaleLabel,
                        headerTimeLabel, headerTimeLabelSml,
                        scrubberBtPlay, scrubberBtStop, scrubberWarningLabel, scrubberWaitLabel,
                        hLayerControlsBt,
                        layerTotSequencedLabel, layerIcoToggle, layerNameField, layerNameFieldSelected,
                        selectionArea, singleSelection, multiSelection, draggedTimeLabel, draggedDurationLabel, snapSequenced,
                        btSequenced, btSequencedOutline, btSequencedSkipped, sequencedLoop, sequencedLabel, sequencedPin,
                        sBg, sTitle, sIntervalTitle, sBtTargetAndPlugType, sBtTargetAndPlugTypeRightAligned, sTargetPrefixLabel, sBtFlip,
                        sToggle, sAxisToggle, sLockToggle, sPrefixLabel,
                        sMissingPluginBox,
                        previewBorderBox,
                        settingsPanel,
                        helpContent,
                        seqInfiniteLoopToggle;

        public override void Init()
        {
            disconnectedLabel = EditorStyles.label.Clone(13, Color.white, TextAnchor.UpperLeft, Format.WordWrap, Format.RichText).Padding(0);
            gHeaderCrumb = new GUIStyle(GUI.skin.label).Add(11, Color.white, TextAnchor.MiddleCenter).Padding(18, 2, 0, 0)
                .Background(DeStylePalette.squareBorderCurved).Border(2, 2, 2, 2);
            gHeaderCrumbSequence = gHeaderCrumb.Clone().PaddingLeft(15);
            gHeaderCrumbsMidLabel = new GUIStyle(GUI.skin.label).Add(TextAnchor.MiddleCenter).Padding(0);
            gHeaderSequenceLabel = gHeaderCrumbsMidLabel.Clone(Color.white);
            gHeaderTimeScaleLabel = gHeaderSequenceLabel.Clone(new DeSkinColor(0.95f), 10).Padding(3, 3, 0, 0)
                .Background(DeStylePalette.squareBorderCurvedEmpty).Border(4, 4, 4, 4);
            headerTimeLabel = new GUIStyle(GUI.skin.label).Add(Color.white, 10, TextAnchor.MiddleCenter).Padding(0);
            headerTimeLabelSml = headerTimeLabel.Clone(8);
            scrubberBtPlay = new GUIStyle(GUI.skin.button).Clone(Color.white, 10, TextAnchor.MiddleCenter).Padding(0)
                .Background(DeStylePalette.squareBorderCurved).Border(4, 4, 4, 4).ContentOffset(1, 1);
            scrubberBtStop = scrubberBtPlay.Clone(15).ContentOffset(1, -1);
            scrubberWarningLabel = new GUIStyle(GUI.skin.label).Add(new Color(1f, 0.55f, 0f), 11, TextAnchor.MiddleLeft).Padding(0).ContentOffset(0, 0);
            scrubberWaitLabel = scrubberWarningLabel.Clone(Color.cyan);
            hLayerControlsBt = DeGUI.styles.button.bBlankBorder.Clone(Color.white);
            layerTotSequencedLabel = new GUIStyle(GUI.skin.label).Add(Color.white, 10, TextAnchor.MiddleCenter).Padding(0);
            layerIcoToggle = new GUIStyle(GUI.skin.button).Padding(0).Background(null);
            layerNameFieldSelected = EditorStyles.toolbarTextField.Clone(new DeSkinColor(0.7f), TextAnchor.MiddleLeft, 11).Padding(2, 2, 0, 0)
                .ContentOffsetY(0);
            layerNameField = layerNameFieldSelected.Clone();
            selectionArea = DeGUI.styles.box.outline01.Clone().Padding(0).Margin(0);
            singleSelection = DeGUI.styles.box.roundOutline01.Clone().Padding(0).Margin(0);
            multiSelection = DeGUI.styles.box.roundOutline01.Clone().Padding(0).Margin(0);
            draggedTimeLabel = DeGUI.styles.box.roundOutline01.Clone(11, new DeSkinColor(1f), TextAnchor.MiddleLeft, Format.NoWordWrap)
                .Padding(2, 2, 0, 0).Background(DeStylePalette.squareBorderCurved);
            draggedDurationLabel = draggedTimeLabel.Clone(TextAnchor.MiddleRight);
            snapSequenced = DeGUI.styles.box.outline01.Clone().Padding(0).Margin(0);
            btSequenced = DeGUI.styles.button.bBlankBorderCompact.Clone().Padding(0).Margin(0).Overflow(0);
            btSequencedOutline = btSequenced.Clone().Background(DeStylePalette.squareBorderCurvedEmpty);
            btSequencedSkipped = btSequenced.Clone().Background(DeStylePalette.squareBorderCurvedEmptyThick);
            sequencedLoop = btSequenced.Clone();
            sequencedLabel = new GUIStyle(GUI.skin.label).Add(10, Color.white, Format.RichText, TextAnchor.MiddleLeft).Padding(0);
            sequencedPin = new GUIStyle(GUI.skin.label).Add(10, Color.white, FontStyle.Bold, TextAnchor.MiddleCenter).Padding(0).ContentOffset(0, 0);
            sTitle = new GUIStyle(GUI.skin.label).Add(10, Color.white, FontStyle.Bold).ContentOffset(-1, -1).MarginLeft(0).MarginRight(0);
            sIntervalTitle = sTitle.Clone(new Color(0f, 0.27f, 0.27f));
            sBg = DeGUI.styles.box.sticky.Clone().Padding(4).Margin(0).Background(DeStylePalette.whiteSquareAlpha15);
            sBtTargetAndPlugType = DeGUI.styles.button.bBlankBorderCompact
                .Clone(11, FontStyle.Bold, new DeSkinColor(0.3f, 1f), TextAnchor.MiddleLeft, Format.RichText)
                .StretchWidth().Height(16).Background(DeStylePalette.squareBorderCurved)
                .Border(4, 4, 4, 4).Padding(4, 3, 0, 0).Margin(0, 0, 1, 0).ContentOffset(0, 0);
            sBtTargetAndPlugTypeRightAligned = sBtTargetAndPlugType.Clone(TextAnchor.MiddleRight);
            sTargetPrefixLabel = EditorStyles.label.Clone(TextAnchor.MiddleLeft, 10).PaddingRight(0).PaddingTop(0);
            sBtFlip = new GUIStyle(GUI.skin.button).Padding(0).Width(18).Height(18).Margin(0, 0, 2, 0);
            sToggle = DeGUI.styles.button.bBlankBorder.Clone().Height(16).ContentOffsetX(0);
            sAxisToggle = sToggle.Clone().MarginLeft(2).MarginRight(2);
            sLockToggle = new GUIStyle(GUI.skin.button).Width(16).Padding(0, 0, 2, 0).Background(null);
            sPrefixLabel = EditorStyles.label.Clone(TextAnchor.MiddleLeft).PaddingRight(0).PaddingTop(0);
            sMissingPluginBox = DeGUI.styles.box.roundOutline01.Clone(Color.white, TextAnchor.MiddleCenter, FontStyle.Bold).Padding(0)
                .ContentOffset(0, -1).Background(DeStylePalette.squareBorderCurved_darkBorders);
            previewBorderBox = DeGUI.styles.box.outline02.Clone();
            settingsPanel = DeGUI.styles.box.stickyTop.Clone().StretchWidth();
            helpContent = new GUIStyle(GUI.skin.label).Add(13, new DeSkinColor(0.9f), Format.RichText, Format.WordWrap).Padding(3);
            seqInfiniteLoopToggle = DeGUI.styles.button.bBlankBorder.Clone(14).Padding(0).ContentOffset(1, -1);
        }
    }
}