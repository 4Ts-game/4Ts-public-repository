// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/01/22

using DG.Tweening.Timeline;
using DG.Tweening.TimelineEditor;
using UnityEditor;
using UnityEngine;

namespace DG.Tweening.TimelineEditor
{
    static class DOEMenuItems
    {
        [MenuItem("GameObject/DOTween/Timeline → DOVisualSequenceCollection", false, 20)]
        static void CreateDOVisualSequenceCollection(MenuCommand menuCommand)
        {
            GameObject go = new GameObject("DOVisualSequenceCollection");
            go.AddComponent<DOVisualSequenceCollection>();
            GameObjectUtility.SetParentAndAlign(go, menuCommand.context as GameObject);
            Undo.RegisterCreatedObjectUndo(go, "Create " + go.name);
            Selection.activeObject = go;
        }

        [MenuItem("CONTEXT/Transform/DOTweenTimeline → Copy Properties")]
        static void CopyTransformValues(MenuCommand command)
        {
            // TODO Paste Transform values
            TimelineEditorUtils.StoreTransform((Transform)command.context);
        }
    }
}