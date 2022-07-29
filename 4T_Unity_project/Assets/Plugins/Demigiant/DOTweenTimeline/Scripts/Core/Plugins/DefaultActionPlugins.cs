// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/02/01

using UnityEngine;

#pragma warning disable CS1522 // Empty switch block (can be caused by disabling of DOTween modules
namespace DG.Tweening.Timeline.Core.Plugins
{
#if UNITY_EDITOR
    [UnityEditor.InitializeOnLoad]
#endif
    static class DefaultActionPlugins
    {
#if UNITY_EDITOR
        static DefaultActionPlugins()
        {
            // Used only to register plugins to be displayed in editor's timeline (runtime uses Register method directly)
            if (!UnityEditor.EditorApplication.isPlayingOrWillChangePlaymode) Register();
        }
#endif

        [RuntimeInitializeOnLoadMethod(RuntimeInitializeLoadType.BeforeSceneLoad)]
        static void Register()
        {
            DOVisualPluginsManager.RegisterActionPlugins(GetActionPlugin, "DOTweenDefaultActions");
        }

        static DOVisualActionPlugin GetActionPlugin(string id)
        {
            switch (id) {
            case "DOTweenDefaultActions":
                return DOVisualPluginsManager.CacheAndReturnAction(id,
                    new PlugDataAction("20fcef9c-105e-4eff-a423-b16fa8d56bfc", "GameObject/Destroy", typeof(GameObject),
                        (t,b,s,f0,f1,f2,f3,i) => Object.Destroy((GameObject)t),
                        null, "GameObject"),
                    new PlugDataAction("39422273-8414-4354-b309-bb28d6d6b0e7", "GameObject/Set Active", typeof(GameObject),
                        (t,b,s,f0,f1,f2,f3,i) => ((GameObject)t).SetActive(b),
                        null, "GameObject", boolOptionLabel:"Activate", defBoolValue:true),
                    new PlugDataAction("e79e07aa-f95c-485e-b0d4-d418cf2ab6c1", "Debug/Log", null,
                        (t,b,s,f0,f1,f2,f3,i) => Debug.Log(s),
                        stringOptionLabel:"Message")
                );
            }
            return null;
        }
    }
}