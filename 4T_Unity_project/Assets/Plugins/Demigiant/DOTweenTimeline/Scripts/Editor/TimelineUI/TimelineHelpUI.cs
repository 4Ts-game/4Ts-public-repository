// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/03/02

using System.Text;
using DG.DemiEditor;
using DG.DemiLib;
using UnityEditor;
using UnityEngine;

namespace DG.Tweening.TimelineEditor
{
    internal class TimelineHelpUI : ABSTimelineElement
    {
        static readonly Color _BgColor = new DeSkinColor(0.15f);
        static readonly HelpDoc _GeneralHelpDoc = new HelpDoc();
        static readonly HelpDoc _KeyboardCommandsDoc = new HelpDoc();
        static readonly GUIContent[] _GcMode = new[] {new GUIContent("General Help"), new GUIContent("Keyboard Commands")};
        static int _mode;
        Vector2 _scrollP;

        #region Public Methods

        public void Refresh() {}

        public override void Draw(Rect drawArea)
        {
            base.Draw(drawArea);

            DOEGUI.BeginGUI();
            if (!_KeyboardCommandsDoc.HasContent()) GenerateHelp();

            DeGUI.DrawColoredSquare(drawArea, _BgColor);

            // Toolbar
            using (new DeGUILayout.ToolbarScope()) {
                using (var check = new EditorGUI.ChangeCheckScope()) {
                    _mode = GUILayout.SelectionGrid(_mode, _GcMode, 2, DOEGUI.Styles.button.tool, GUILayout.Width(250));
                    if (check.changed) _scrollP = Vector2.zero;
                }
                GUILayout.FlexibleSpace();
                using (new DeGUI.ColorScope(new DeSkinColor(0.7f, 0.3f))) {
                    if (GUILayout.Button("×", DOEGUI.Styles.button.tool, GUILayout.Width(18))) {
                        DOVisualSequenceTimeline.mode = DOVisualSequenceTimeline.Mode.Default;
                    }
                }
            }
            switch (_mode) {
            case 1: // Keyboard Commands
                _scrollP = GUILayout.BeginScrollView(_scrollP);
                GUILayout.Label(_KeyboardCommandsDoc.content, DOEGUI.Styles.timeline.helpContent);
                GUILayout.EndScrollView();
                break;
            default: // General Help
                _scrollP = GUILayout.BeginScrollView(_scrollP);
                GUILayout.Label(_GeneralHelpDoc.content, DOEGUI.Styles.timeline.helpContent);
                GUILayout.EndScrollView();
                break;
            }
        }

        #endregion

        #region Methods

        void GenerateHelp()
        {
            // General Help

            _GeneralHelpDoc.AddTitle("Logic");
            _GeneralHelpDoc.AddParagraph("DOTween Timeline uses [C]DOVisualSequence[/C] objects which, at runtime, " +
                                         "will generate a regular DOTween [C]Tween[/C] (actually, a [C]Sequence[/C]).");

            _GeneralHelpDoc.AddTitle("Editing Setup");
            _GeneralHelpDoc.AddParagraph("To create a visual sequence in the editor, add a serialized [C]DOVisualSequence[/C] field " +
                                         "to a Component. The Inspector will then show you buttons to edit it and other options. " +
                                         "\nAlternatively, you can add a [C]DOVisualSequenceCollection[/C] Component to a GameObject, " +
                                         "which gives other options to create sequences.");

            _GeneralHelpDoc.AddTitle("Runtime Usage");
            _GeneralHelpDoc.AddSubtitle("Basic Usage");
            _GeneralHelpDoc.AddParagraph("A [C]DOVisualSequence[/C] won't generate its [C]Sequence[/C] automatically " +
                                         "(unless it's inside a [C]DOVisualSequenceCollection[/C] set to <b>AutoPlay</b>). You need to call " +
                                         "[C]myDOVisualSequenceInstance.GenerateTween()[/C] (which will also return the [C]Sequence[/C] itself) in order to do that.");
            _GeneralHelpDoc.AddSubtitle("Extras");
            _GeneralHelpDoc.AddParagraph("Before creating the [C]Tween[/C] with [C]GenerateTween[/C] you can edit individual [C]DOVisualSequence[/C] " +
                                         "elements by pinning them in the visual Timeline (RMB on an element) and then retrieving them at runtime " +
                                         "via [C]myDOVisualSequenceInstance.FindSequencedByPinNoAlloc()[/C]");

            _GeneralHelpDoc.FinalizeContent();

            // Keyboard commands
            
            _KeyboardCommandsDoc.AddTitle("General");
            _KeyboardCommandsDoc.AddShortcut("RMB", "on Timeline's title", "Open Timeline menu to access DOTween Timeline Settings and this Help");

            _KeyboardCommandsDoc.AddTitle("Keys + Shortcuts");
            _KeyboardCommandsDoc.AddSubtitle("Timeline - General")
                .AddShortcut("Drag GameObject", "to Timeline", "Add new Tween element")
                .AddShortcut("LMB + Drag", "on empty space", "Create selection area")
                .AddShortcut("LMB", "on element", "Add element to selection")
                .AddShortcut("RMB", "on empty space", "Add new element with options to choose type (Tween/Action/Event/etc)")
                .AddShortcut("CTRL+A", "Select all elements")
                .AddShortcut("CTRL+D / ESC", "Deselect all elements")
                .AddShortcut("CTRL+C", "Copy selected elements")
                .AddShortcut("CTRL+X", "Cut selected elements")
                .AddShortcut("CTRL+V", "Paste copied elements")
                .AddShortcut("CTRL+SHIFT+V", "Paste copied elements in place (at the same time position they were when copied)")
                .AddShortcut("DEL / Backspace", "Delete selected elements");
            _KeyboardCommandsDoc.AddSubtitle("Timeline - When dragging elements")
                .AddShortcut("CTRL + Drag", "Snap to 0.25 seconds interval")
                .AddShortcut("ALT + Drag", "Snap to other elements' beginning/end (if nearby)")
                .AddShortcut("SHIFT + Drag", "Drag only vertically (between layers) without changing the time position");
            _KeyboardCommandsDoc.AddSubtitle("Layers")
                .AddParagraph("Layers are irrelevant at runtime except for deactivated ones (eye icon), whose elements will be ignored.")
                .AddShortcut("LMB + Drag", "on layer name", "Reorder layer")
                .AddShortcut("Double Click", "on layer name", "Rename layer")
                .AddShortcut("LMB", "left of layer name", "Set layer color");

            _KeyboardCommandsDoc.FinalizeContent();
        }

        #endregion

        // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████
        // ███ INTERNAL CLASSES ████████████████████████████████████████████████████████████████████████████████████████████████
        // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

        class HelpDoc
        {
            public GUIContent content = new GUIContent();
            readonly StringBuilder _strb = new StringBuilder();

            public bool HasContent()
            {
                return !string.IsNullOrEmpty(content.text);
            }

            public void FinalizeContent()
            {
                content.text = _strb.ToString();
                _strb.Length = 0;
            }

            public HelpDoc AddTitle(string title)
            {
                if (_strb.Length > 0) _strb.Append("\n\n");
                _strb.Append(Tag.Title_Open).Append(title).Append(Tag.Title_Close);
                return this;
            }

            public HelpDoc AddSubtitle(string subtitle)
            {
                _strb.Append('\n').Append(Tag.Subtitle_Open).Append(subtitle).Append(Tag.Subtitle_Close);
                return this;
            }

            public HelpDoc AddParagraph(string text)
            {
                string parsedText = text.Replace("[C]", Tag.CodeTag_Open).Replace("[/C]", Tag.CodeTag_Close);
                _strb.Append('\n').Append(Tag.Paragraph_Open).Append(parsedText).Append(Tag.Paragraph_Close);
                return this;
            }

            public HelpDoc AddShortcut(string shortcut, string description)
            {
                AddShortcut(shortcut, null, description);
                return this;
            }

            public HelpDoc AddShortcut(string shortcut, string shortcutExtra, string description)
            {
                _strb.Append('\n').Append(Tag.ShortcutDefinitionTerm_Open).Append(shortcut).Append(Tag.ShortcutDefinitionTerm_Close);
                if (shortcutExtra != null) {
                    _strb.Append(' ').Append(Tag.ShortcutDefinitionTerm_Extra_Open).Append(shortcutExtra).Append(Tag.ShortcutDefinitionTerm_Extra_Close);
                }
                _strb.Append(" : ").Append(Tag.ShortcutDefinition_Open).Append(description).Append(Tag.ShortcutDefinition_Close);
                return this;
            }

            // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████
            // ███ INTERNAL CLASSES ████████████████████████████████████████████████████████████████████████████████████████████████
            // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

            static class Tag
            {
                public const string Title_Open = "<color=#94de59><size=18>";
                public const string Title_Close = "</size></color>";
                public const string Paragraph_Open = "";
                public const string Paragraph_Close = "";
                public const string Subtitle_Open = "<color=#ebd400><size=15>";
                public const string Subtitle_Close = "</size></color>";
                public const string ShortcutDefinitionTerm_Open = "<color=#db89ff>";
                public const string ShortcutDefinitionTerm_Close = "</color>";
                public const string ShortcutDefinitionTerm_Extra_Open = "<color=#9f89ff><i>";
                public const string ShortcutDefinitionTerm_Extra_Close = "</i></color>";
                public const string ShortcutDefinition_Open = "<color=#aaaaaa>";
                public const string ShortcutDefinition_Close = "</color>";
                public const string CodeTag_Open = "<color=#6fc5ff>";
                public const string CodeTag_Close = "</color>";
            }
        }
    }
}