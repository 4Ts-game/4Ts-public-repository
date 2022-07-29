using UnityEngine;
using System.Collections;
using FourT;
using UnityEditor;

[CustomEditor(typeof(UIButtonDefault))]
public class UIButtonEditor : Editor
{
    public override void OnInspectorGUI()
    {
        base.OnInspectorGUI();
        UIButtonDefault t = (UIButtonDefault)target;
    }
}

