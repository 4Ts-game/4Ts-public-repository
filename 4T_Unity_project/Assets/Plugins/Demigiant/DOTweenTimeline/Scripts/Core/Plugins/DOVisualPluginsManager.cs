// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/01/21

using System;
using System.Collections.Generic;
using UnityEngine;

namespace DG.Tweening.Timeline.Core.Plugins
{
    public static class DOVisualPluginsManager
    {
        public static readonly List<string> GlobalTweenPluginsIds = new List<string>();
        public static readonly List<string> ActionPluginsIds = new List<string>();

        static readonly Dictionary<string, DOVisualTweenPlugin> _IdToGlobalTweenPlugin = new Dictionary<string, DOVisualTweenPlugin>();
        static readonly Dictionary<Type, DOVisualTweenPlugin> _TypeToTweenPlugin = new Dictionary<Type, DOVisualTweenPlugin>();
        static readonly List<Func<string, DOVisualTweenPlugin>> _GlobalTweenPluginsGenerators = new List<Func<string, DOVisualTweenPlugin>>();
        static readonly List<Func<Type, string, DOVisualTweenPlugin>> _TweenPluginsGenerators = new List<Func<Type, string, DOVisualTweenPlugin>>();
        static readonly Dictionary<string, DOVisualActionPlugin> _IdToActionPlugin = new Dictionary<string, DOVisualActionPlugin>();
        static readonly List<Func<string, DOVisualActionPlugin>> _ActionPluginsGenerators = new List<Func<string, DOVisualActionPlugin>>();

        #region Public Methods

        // Order of internal PluginData index is important and can't be changed (stored in sequenced to determine right plugin data)
        public static DOVisualTweenPlugin GetGlobalTweenPlugin(string id)
        {
            if (id == null) return null;
            if (_IdToGlobalTweenPlugin.ContainsKey(id)) return _IdToGlobalTweenPlugin[id];

            int len = _GlobalTweenPluginsGenerators.Count;
            for (int i = 0; i < len; ++i) {
                DOVisualTweenPlugin plugin = _GlobalTweenPluginsGenerators[i](id);
                if (plugin != null) return plugin;
            }
            return null;
        }
        // Order of internal PluginData index is important and can't be changed (stored in sequenced to determine right plugin data)
        public static DOVisualTweenPlugin GetTweenPlugin(object target)
        {
            Type t = target.GetType();
            if (_TypeToTweenPlugin.ContainsKey(t)) return _TypeToTweenPlugin[t];

            string tFullName = t.FullName;
            int len = _TweenPluginsGenerators.Count;
            for (int i = 0; i < len; ++i) {
                DOVisualTweenPlugin plugin = _TweenPluginsGenerators[i](t, tFullName);
                if (plugin != null) return plugin;
            }
            return null;
        }
        // Order of internal PluginData index is important and can't be changed (stored in sequenced to determine right plugin data)
        public static DOVisualActionPlugin GetActionPlugin(string id)
        {
            if (id == null) return null;
            if (_IdToActionPlugin.ContainsKey(id)) return _IdToActionPlugin[id];

            int len = _ActionPluginsGenerators.Count;
            for (int i = 0; i < len; ++i) {
                DOVisualActionPlugin plugin = _ActionPluginsGenerators[i](id);
                if (plugin != null) return plugin;
            }
            return null;
        }

        public static void RegisterGlobalTweenPlugins(Func<string, DOVisualTweenPlugin> customPluginsGenerator, params string[] ids)
        {
            _GlobalTweenPluginsGenerators.Add(customPluginsGenerator);
            for (int i = 0; i < ids.Length; ++i) {
                if (GlobalTweenPluginsIds.Contains(ids[i])) continue;
                GlobalTweenPluginsIds.Add(ids[i]);
            }
        }
        public static void RegisterTweenPlugins(Func<Type, string, DOVisualTweenPlugin> customPluginsGenerator)
        {
            _TweenPluginsGenerators.Add(customPluginsGenerator);
        }
        public static void RegisterActionPlugins(Func<string, DOVisualActionPlugin> customPluginsGenerator, params string[] ids)
        {
            _ActionPluginsGenerators.Add(customPluginsGenerator);
            for (int i = 0; i < ids.Length; ++i) {
                if (ActionPluginsIds.Contains(ids[i])) continue;
                ActionPluginsIds.Add(ids[i]);
            }
        }

        public static DOVisualTweenPlugin CacheAndReturnGlobal(string id, params ITweenPluginData[] plugDatas)
        {
            DOVisualTweenPlugin plugin = new DOVisualTweenPlugin(null, plugDatas);
            _IdToGlobalTweenPlugin.Add(id, plugin);
            return plugin;
        }
        public static DOVisualTweenPlugin CacheAndReturn(Type type, params ITweenPluginData[] plugDatas)
        {
            DOVisualTweenPlugin plugin = new DOVisualTweenPlugin(type, plugDatas);
            _TypeToTweenPlugin.Add(type, plugin);
            return plugin;
        }
        public static DOVisualActionPlugin CacheAndReturnAction(string id, params PlugDataAction[] plugDatas)
        {
            DOVisualActionPlugin plugin = new DOVisualActionPlugin(plugDatas);
            _IdToActionPlugin.Add(id, plugin);
            return plugin;
        }

        #endregion
    }
}