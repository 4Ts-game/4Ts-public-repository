using Holoville.HOTools;
using UnityEditor;
public class HOToolsWindowMenuItem
{
   [MenuItem ("Tools/Demigiant/HOTools")]
   static void ShowWindow() {
       EditorWindow.GetWindow(typeof(HOToolsWindow), false, "HOTools");
   }
}