// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2020/01/31

using System;
using UnityEngine;
#if true // TEXTMESHPRO_MARKER
using TMPro;
#endif
#if true // UI_MARKER
using UnityEngine.UI;
#endif

#pragma warning disable CS1522 // Empty switch block (can be caused by disabling of DOTween modules
namespace DG.Tweening.Timeline.Core.Plugins
{
#if UNITY_EDITOR
    [UnityEditor.InitializeOnLoad]
#endif
    static class DefaultTweenPlugins
    {
#if UNITY_EDITOR
        static DefaultTweenPlugins()
        {
            // Used only to register plugins to be displayed in editor's timeline (runtime uses Register method directly)
            if (!UnityEditor.EditorApplication.isPlayingOrWillChangePlaymode) Register();
        }
#endif

        [RuntimeInitializeOnLoadMethod(RuntimeInitializeLoadType.BeforeSceneLoad)]
        static void Register()
        {
            DOVisualPluginsManager.RegisterGlobalTweenPlugins(GetGlobalTweenPlugin, "DOTweenDefaults");
            DOVisualPluginsManager.RegisterTweenPlugins(GetTweenPlugin);
        }

        static DOVisualTweenPlugin GetGlobalTweenPlugin(string id)
        {
            switch (id) {
            case "DOTweenDefaults":
                return DOVisualPluginsManager.CacheAndReturnGlobal(id,
                    new PlugDataGlobalTween("8e164fc5-ea6c-4b55-8201-a9bcc57fac3c", "Time/Time Scale", ()=> Time.timeScale, x => Time.timeScale = x),
                    new PlugDataGlobalTween("91a3e986-73bb-4127-8bab-32ff9b8b7c50", "Time/Fixed Delta Time", ()=> Time.fixedDeltaTime, x => Time.fixedDeltaTime = x)
                );
            }
            return null;
        }

        static DOVisualTweenPlugin GetTweenPlugin(Type targetType, string targetTypeFullName)
        {
            switch (targetTypeFullName) {
#if true // AUDIO_MARKER
            case "UnityEngine.AudioSource":
                return DOVisualPluginsManager.CacheAndReturn(targetType,
                    new PlugDataTween("949e0935-b5f3-4020-ab77-d926cfbc7212", "Volume", (c,s,i) => ()=> ((AudioSource)c).volume, (c,s,i) => x => ((AudioSource)c).volume = x),
                    new PlugDataTween("5c0bb6c7-64e3-4165-8465-9d9beeeb1e58", "Pitch", (c,s,i) => ()=> ((AudioSource)c).pitch, (c,s,i) => x => ((AudioSource)c).pitch = x),
                    new PlugDataTween("33a540e1-bc18-4be9-b277-8655058d2e39", "Doppler Level", (c,s,i) => ()=> ((AudioSource)c).dopplerLevel, (c,s,i) => x => ((AudioSource)c).dopplerLevel = x),
                    new PlugDataTween("39397a70-6d5d-4f46-bc2f-a6e417367bac", "Time", (c,s,i) => ()=> ((AudioSource)c).time, (c,s,i) => x => ((AudioSource)c).time = x)
                );
#endif
            case "UnityEngine.Camera":
                return DOVisualPluginsManager.CacheAndReturn(targetType,
                    new PlugDataTween("42d146c8-ee24-43a7-82ff-9f6cf2fd953c", "Background Color", (c,s,i) => ()=> ((Camera)c).backgroundColor, (c,s,i) => x => ((Camera)c).backgroundColor = x),
                    new PlugDataTween("4d254be0-e281-4093-b858-c980913af6f3", "Field of View", (c,s,i) => ()=> ((Camera)c).fieldOfView, (c,s,i) => x => ((Camera)c).fieldOfView = x),
                    new PlugDataTween("e613e5b9-8143-4bd4-8be7-a792de04fbd5", "Rect", (c,s,i) => ()=> ((Camera)c).rect, (c,s,i) => x => ((Camera)c).rect = x),
                    new PlugDataTween("b8022f2c-6a49-411e-bda3-cbf1e7930ea0", "Pixel Rect", (c,s,i) => ()=> ((Camera)c).pixelRect, (c,s,i) => x => ((Camera)c).pixelRect = x),
                    new PlugDataTween("d14a4f40-9824-4e7c-b2cf-db697e52ce79", "Far Clip Plane", (c,s,i) => ()=> ((Camera)c).farClipPlane, (c,s,i) => x => ((Camera)c).farClipPlane = x),
                    new PlugDataTween("bf059f13-67dd-4424-8d1d-36645d00d3b0", "Near Clip Plane", (c,s,i) => ()=> ((Camera)c).nearClipPlane, (c,s,i) => x => ((Camera)c).nearClipPlane = x),
                    new PlugDataTween("059137d9-2959-47b4-9f5f-21835fc9a0f8", "Orthographic Size", (c,s,i) => ()=> ((Camera)c).orthographicSize, (c,s,i) => x => ((Camera)c).orthographicSize = x),
                    new PlugDataTween("e0880958-ef4e-4c8d-80b6-4d190c10d398", "Focal Length", (c,s,i) => ()=> ((Camera)c).focalLength, (c,s,i) => x => ((Camera)c).focalLength = x),
                    new PlugDataTween("7a010918-875a-4e1e-a2ad-d292f80d5872", "Lens Shift", (c,s,i) => ()=> ((Camera)c).lensShift, (c,s,i) => x => ((Camera)c).lensShift = x)
                );
            case "UnityEngine.Light":
                return DOVisualPluginsManager.CacheAndReturn(targetType,
                    new PlugDataTween("a829dd4d-3c6a-41b0-ba20-c14b5b02a2b1", "Color", (c,s,i) => ()=> ((Light)c).color, (c,s,i) => x => ((Light)c).color = x),
                    new PlugDataTween("f69ca130-45aa-4160-ad83-c0b3e00f0b00", "Color Temperature", (c,s,i) => ()=> ((Light)c).colorTemperature, (c,s,i) => x => ((Light)c).colorTemperature = x),
                    new PlugDataTween("17976b2c-65a2-40d8-a38e-4587434750cb", "Intensity", (c,s,i) => ()=> ((Light)c).intensity, (c,s,i) => x => ((Light)c).intensity = x),
                    new PlugDataTween("0e7cfac5-55c0-4782-bbae-768b75b18fe3", "Range", (c,s,i) => ()=> ((Light)c).range, (c,s,i) => x => ((Light)c).range = x),
                    new PlugDataTween("8099744f-84d8-42c6-b2b8-da2d85ba9519", "Spot Angle", (c,s,i) => ()=> ((Light)c).spotAngle, (c,s,i) => x => ((Light)c).spotAngle = x),
                    new PlugDataTween("93b7ab1f-bdac-480b-a19c-9328e3b21703", "Shadow Strength", (c,s,i) => ()=> ((Light)c).shadowStrength, (c,s,i) => x => ((Light)c).shadowStrength = x)
                );
            case "UnityEngine.MeshRenderer":
                return DOVisualPluginsManager.CacheAndReturn(targetType,
                    new PlugDataTween("66bfa16c-2106-467b-a5ba-c299a77aa430", "Instance/Main Color", (c,s,i) => ()=> ((MeshRenderer)c).material.color, (c,s,i) => x => ((MeshRenderer)c).material.color = x),
                    new PlugDataTween("801b3410-ac42-48ee-978c-dedf0acfea45", "Instance/Color Property (string ID)",
                        (c,s,i) => ()=> ((MeshRenderer)c).material.GetColor(s),
                        (c,s,i) => x => ((MeshRenderer)c).material.SetColor(s, x), PluginTweenType.StringOption, "ID"),
                    new PlugDataTween("24095181-fb70-4a4f-847d-f88476921473", "Instance/Color Property (int ID)",
                        (c,s,i) => ()=> ((MeshRenderer)c).material.GetColor(i),
                        (c,s,i) => x => ((MeshRenderer)c).material.SetColor(i, x), PluginTweenType.IntOption, null, "ID"),
                    new PlugDataTween("44637302-5b84-4d80-a7dd-4d297e84529e", "Instance/Main Offset", (c,s,i) => ()=> ((MeshRenderer)c).material.mainTextureOffset, (c,s,i) => x => ((MeshRenderer)c).material.mainTextureOffset = x),
                    new PlugDataTween("f4e82f6f-6b9e-4236-abe1-4ad63101eefe", "Instance/Offset Property (string ID)",
                        (c,s,i) => ()=> ((MeshRenderer)c).material.GetTextureOffset(s),
                        (c,s,i) => x => ((MeshRenderer)c).material.SetTextureOffset(s, x), PluginTweenType.StringOption, "ID"),
                    new PlugDataTween("77a5aa6d-1a9b-487d-a6bf-96dd74106845", "Instance/Offset Property (int ID)",
                        (c,s,i) => ()=> ((MeshRenderer)c).material.GetTextureOffset(i),
                        (c,s,i) => x => ((MeshRenderer)c).material.SetTextureOffset(i, x), PluginTweenType.IntOption, null, "ID"),
                    new PlugDataTween("f4e6c9a2-277f-491a-b5c3-7b7f5cc3db1e", "Instance/Main Tiling", (c,s,i) => ()=> ((MeshRenderer)c).material.mainTextureScale, (c,s,i) => x => ((MeshRenderer)c).material.mainTextureScale = x),
                    new PlugDataTween("e89d4c8b-5cac-470c-9d76-bea6ada1cf41", "Instance/Tiling Property (string ID)",
                        (c,s,i) => ()=> ((MeshRenderer)c).material.GetTextureScale(s),
                        (c,s,i) => x => ((MeshRenderer)c).material.SetTextureScale(s, x), PluginTweenType.StringOption, "ID"),
                    new PlugDataTween("ad5b2ef9-52c8-465d-91f0-cc3f392cc237", "Instance/Tiling Property (int ID)",
                        (c,s,i) => ()=> ((MeshRenderer)c).material.GetTextureScale(i),
                        (c,s,i) => x => ((MeshRenderer)c).material.SetTextureScale(i, x), PluginTweenType.IntOption, null, "ID"),
                    new PlugDataTween("87977118-5ce7-4593-ac4e-5eb6d73f4bcf", "Instance/Float Property (string ID)",
                        (c,s,i) => ()=> ((MeshRenderer)c).material.GetFloat(s),
                        (c,s,i) => x => ((MeshRenderer)c).material.SetFloat(s, x), PluginTweenType.StringOption, "ID"),
                    new PlugDataTween("3dfcf270-8761-4065-889e-8297ebd544be", "Instance/Float Property (int ID)",
                        (c,s,i) => ()=> ((MeshRenderer)c).material.GetFloat(i),
                        (c,s,i) => x => ((MeshRenderer)c).material.SetFloat(i, x), PluginTweenType.IntOption, null, "ID"),
                    new PlugDataTween("89a21f52-de3d-4e4a-b73c-13f367f138ce", "Instance/Vector Property (string ID)",
                        (c,s,i) => ()=> ((MeshRenderer)c).material.GetVector(s),
                        (c,s,i) => x => ((MeshRenderer)c).material.SetVector(s, x), PluginTweenType.StringOption, "ID"),
                    new PlugDataTween("ace614aa-9ac8-4ee7-9a6b-777f7894096c", "Instance/Vector Property (int ID)",
                        (c,s,i) => ()=> ((MeshRenderer)c).material.GetVector(i),
                        (c,s,i) => x => ((MeshRenderer)c).material.SetVector(i, x), PluginTweenType.IntOption, null, "ID"),
                    new PlugDataTween("81237923-d770-4a9e-8591-579316ac3587", "Shared/Main Color", (c,s,i) => ()=> ((MeshRenderer)c).sharedMaterial.color, (c,s,i) => x => ((MeshRenderer)c).sharedMaterial.color = x),
                    new PlugDataTween("923b8bf2-cbb6-460e-8033-a169d42aceec", "Shared/Color Property (string ID)",
                        (c,s,i) => ()=> ((MeshRenderer)c).sharedMaterial.GetColor(s),
                        (c,s,i) => x => ((MeshRenderer)c).sharedMaterial.SetColor(s, x), PluginTweenType.StringOption, "ID"),
                    new PlugDataTween("3c749003-851f-43e3-90db-27fa7462d03e", "Shared/Color Property (int ID)",
                        (c,s,i) => ()=> ((MeshRenderer)c).sharedMaterial.GetColor(i),
                        (c,s,i) => x => ((MeshRenderer)c).sharedMaterial.SetColor(i, x), PluginTweenType.IntOption, null, "ID"),
                    new PlugDataTween("aeb24506-c294-4111-86e6-bab3b2824a17", "Shared/Main Offset", (c,s,i) => ()=> ((MeshRenderer)c).sharedMaterial.mainTextureOffset, (c,s,i) => x => ((MeshRenderer)c).sharedMaterial.mainTextureOffset = x),
                    new PlugDataTween("64b18c98-31d7-4c6c-ae38-5a6b943eba5c", "Shared/Offset Property (string ID)",
                        (c,s,i) => ()=> ((MeshRenderer)c).sharedMaterial.GetTextureOffset(s),
                        (c,s,i) => x => ((MeshRenderer)c).sharedMaterial.SetTextureOffset(s, x), PluginTweenType.StringOption, "ID"),
                    new PlugDataTween("2a4c1217-8412-455c-8cb2-11ae75385afe", "Shared/Offset Property (int ID)",
                        (c,s,i) => ()=> ((MeshRenderer)c).sharedMaterial.GetTextureOffset(i),
                        (c,s,i) => x => ((MeshRenderer)c).sharedMaterial.SetTextureOffset(i, x), PluginTweenType.IntOption, null, "ID"),
                    new PlugDataTween("78d34e5a-eeef-460e-9ae5-c0d5e41b7c75", "Shared/Main Tiling", (c,s,i) => ()=> ((MeshRenderer)c).sharedMaterial.mainTextureScale, (c,s,i) => x => ((MeshRenderer)c).sharedMaterial.mainTextureScale = x),
                    new PlugDataTween("51765c41-3972-4828-ae10-73562b5d716f", "Shared/Tiling Property (string ID)",
                        (c,s,i) => ()=> ((MeshRenderer)c).sharedMaterial.GetTextureScale(s),
                        (c,s,i) => x => ((MeshRenderer)c).sharedMaterial.SetTextureScale(s, x), PluginTweenType.StringOption, "ID"),
                    new PlugDataTween("2bef77dd-ed96-4864-83b4-07dac2e083c3", "Shared/Tiling Property (int ID)",
                        (c,s,i) => ()=> ((MeshRenderer)c).sharedMaterial.GetTextureScale(i),
                        (c,s,i) => x => ((MeshRenderer)c).sharedMaterial.SetTextureScale(i, x), PluginTweenType.IntOption, null, "ID"),
                    new PlugDataTween("c9a0342a-a150-4dc7-b666-9d4923bb4b24", "Shared/Float Property (string ID)",
                        (c,s,i) => ()=> ((MeshRenderer)c).sharedMaterial.GetFloat(s),
                        (c,s,i) => x => ((MeshRenderer)c).sharedMaterial.SetFloat(s, x), PluginTweenType.StringOption, "ID"),
                    new PlugDataTween("e6226e0d-4a25-489c-87bf-4d6dda51acbd", "Shared/Float Property (int ID)",
                        (c,s,i) => ()=> ((MeshRenderer)c).sharedMaterial.GetFloat(i),
                        (c,s,i) => x => ((MeshRenderer)c).sharedMaterial.SetFloat(i, x), PluginTweenType.IntOption, null, "ID"),
                    new PlugDataTween("842437e8-91c5-488f-9f54-fc4f75c49039", "Shared/Vector Property (string ID)",
                        (c,s,i) => ()=> ((MeshRenderer)c).sharedMaterial.GetVector(s),
                        (c,s,i) => x => ((MeshRenderer)c).sharedMaterial.SetVector(s, x), PluginTweenType.StringOption, "ID"),
                    new PlugDataTween("f84e77c7-3b7b-4f1a-92cb-df9c02bf8c4d", "Shared/Vector Property (int ID)",
                        (c,s,i) => ()=> ((MeshRenderer)c).sharedMaterial.GetVector(i),
                        (c,s,i) => x => ((MeshRenderer)c).sharedMaterial.SetVector(i, x), PluginTweenType.IntOption, null, "ID")
                );
            case "UnityEngine.ParticleSystem":
                return DOVisualPluginsManager.CacheAndReturn(targetType,
                    new PlugDataTween("c5d59146-fb93-4862-bb7d-e32feab4e91b", "Time", (c,s,i) => ()=> ((ParticleSystem)c).time, (c,s,i) => x => ((ParticleSystem)c).time = x),
                    new PlugDataTween("76ddcbf0-3e2d-4cb0-9d1f-39ef7f2ddd36", "Main/Simulation Speed",
                        (c,s,i) => ()=> { ParticleSystem ps = (ParticleSystem)c; var module = ps.main; return module.simulationSpeed; },
                        (c,s,i) => x => { ParticleSystem ps = (ParticleSystem)c; var module = ps.main; module.simulationSpeed = x; }),
                    new PlugDataTween("dae1e212-50cf-41b7-ba10-ed5707855da7", "Main/Color",
                        (c,s,i) => ()=> { ParticleSystem ps = (ParticleSystem)c; var module = ps.main; return module.startColor.color; },
                        (c,s,i) => x => { ParticleSystem ps = (ParticleSystem)c; var module = ps.main; module.startColor = x; })
                );
#if true // UI_MARKER
            case "UnityEngine.RectTransform":
                return DOVisualPluginsManager.CacheAndReturn(targetType,
                    new PlugDataTween("e0432885-64f4-438c-8af1-1c6045924ae7", "AnchoredPosition", (c,s,i) => ()=> ((RectTransform)c).anchoredPosition, (c,s,i) => x => ((RectTransform)c).anchoredPosition = x),
                    new PlugDataTween("c5c56091-a239-44b9-865c-119fc75e0eaa", "AnchoredPosition 3D", (c,s,i) => ()=> ((RectTransform)c).anchoredPosition3D, (c,s,i) => x => ((RectTransform)c).anchoredPosition3D = x),
                    new PlugDataTween("45e52372-c35c-480c-b36a-6ef134c15a3a", "SizeDelta", (c,s,i) => ()=> ((RectTransform)c).sizeDelta, (c,s,i) => x => ((RectTransform)c).sizeDelta = x),
                    new PlugDataTween("e7178eb7-47a7-44d5-bd77-e509ed6a160e", "AnchorMax", (c,s,i) => ()=> ((RectTransform)c).anchorMax, (c,s,i) => x => ((RectTransform)c).anchorMax = x),
                    new PlugDataTween("8ba80182-1ba8-4c12-ab91-aa001b2f1071", "AnchorMin", (c,s,i) => ()=> ((RectTransform)c).anchorMin, (c,s,i) => x => ((RectTransform)c).anchorMin = x),
                    new PlugDataTween("3775c686-33cd-4bcc-af52-04dd86fccea2", "Pivot", (c,s,i) => ()=> ((RectTransform)c).pivot, (c,s,i) => x => ((RectTransform)c).pivot = x),
                    new PlugDataTween("e73ebb0e-2ba3-4418-af9a-7faf15d822d0", "Punch/AnchoredPosition", (c,s,i) => ()=> ((RectTransform)c).anchoredPosition, (c,s,i) => x => ((RectTransform)c).anchoredPosition = x, PluginTweenType.Punch),
                    new PlugDataTween("cc503371-a964-4745-9e9a-33a413a15801", "Punch/AnchoredPosition 3D", (c,s,i) => ()=> ((RectTransform)c).anchoredPosition3D, (c,s,i) => x => ((RectTransform)c).anchoredPosition3D = x, PluginTweenType.Punch),
                    new PlugDataTween("82e13ecf-1e1f-408d-a70f-9eefd37ab37c", "Shake/AnchoredPosition", (c,s,i) => ()=> ((RectTransform)c).anchoredPosition, (c,s,i) => x => ((RectTransform)c).anchoredPosition = x, PluginTweenType.Shake),
                    new PlugDataTween("8de5e370-045b-480a-a48b-539a93ba903a", "Shake/AnchoredPosition 3D", (c,s,i) => ()=> ((RectTransform)c).anchoredPosition3D, (c,s,i) => x => ((RectTransform)c).anchoredPosition3D = x, PluginTweenType.Shake),
                    // Transform-based
                    new PlugDataTween("970633a3-7361-4396-a008-2a8bf4ad8504", "Rotation", (c,s,i) => ()=> ((Transform)c).rotation, (c,s,i) => x => ((Transform)c).rotation = x),
                    new PlugDataTween("73861f30-0f54-4a24-8a2b-d0bdbe450dcc", "Local Rotation", (c,s,i) => ()=> ((Transform)c).localRotation, (c,s,i) => x => ((Transform)c).localRotation = x),
                    new PlugDataTween("8beb6a3d-66a4-4d02-8d9d-6b09b5a013dc", "Scale", (c,s,i) => ()=> ((Transform)c).localScale, (c,s,i) => x => ((Transform)c).localScale = x),
                    new PlugDataTween("5fa36e6a-a460-471d-ac59-d4d8d724a3f2", "Punch/Rotation", (c,s,i) => ()=> ((Transform)c).eulerAngles, (c,s,i) => x => ((Transform)c).eulerAngles = x, PluginTweenType.Punch),
                    new PlugDataTween("b4433613-ebbd-442d-821e-167e4685ce68", "Punch/Local Rotation", (c,s,i) => ()=> ((Transform)c).localEulerAngles, (c,s,i) => x => ((Transform)c).localEulerAngles = x, PluginTweenType.Punch),
                    new PlugDataTween("7ace0704-183e-4265-9a8a-8d6d7d9d5541", "Punch/Scale", (c,s,i) => ()=> ((Transform)c).localScale, (c,s,i) => x => ((Transform)c).localScale = x, PluginTweenType.Punch),
                    new PlugDataTween("4b1b8b2a-203e-4523-9a2a-6c840640944a", "Shake/Rotation", (c,s,i) => ()=> ((Transform)c).eulerAngles, (c,s,i) => x => ((Transform)c).eulerAngles = x, PluginTweenType.Shake),
                    new PlugDataTween("c712c95e-3ac7-4402-a2dd-fab035fcbe5e", "Shake/Local Rotation", (c,s,i) => ()=> ((Transform)c).localEulerAngles, (c,s,i) => x => ((Transform)c).localEulerAngles = x, PluginTweenType.Shake),
                    new PlugDataTween("3e273628-99bd-4a9f-8611-c07f989afe49", "Shake/Scale", (c,s,i) => ()=> ((Transform)c).localScale, (c,s,i) => x => ((Transform)c).localScale = x, PluginTweenType.Shake)
                );
#endif
            case "UnityEngine.TrailRenderer":
                return DOVisualPluginsManager.CacheAndReturn(targetType,
                    new PlugDataTween("31e87613-af05-406e-8dba-501c60b4e60d", "Time", (c,s,i) => ()=> ((TrailRenderer)c).time, (c,s,i) => x => ((TrailRenderer)c).time = x)
                );
            case "UnityEngine.Transform":
                return DOVisualPluginsManager.CacheAndReturn(targetType,
                    new PlugDataTween("40076f47-8c0a-4d8c-bf18-ee2f1cc73856", "Position", (c,s,i) => ()=> ((Transform)c).position, (c,s,i) => x => ((Transform)c).position = x),
                    new PlugDataTween("218a5bf9-1202-4aa2-b959-9bce0dcb68ef", "Local Position", (c,s,i) => ()=> ((Transform)c).localPosition, (c,s,i) => x => ((Transform)c).localPosition = x),
                    new PlugDataTween("a08c22fd-85a9-4eb9-bf46-76a9212d3163", "Rotation", (c,s,i) => ()=> ((Transform)c).rotation, (c,s,i) => x => ((Transform)c).rotation = x),
                    new PlugDataTween("77a536b2-f1ea-4a43-9fc8-b0939e9d6f9a", "Local Rotation", (c,s,i) => ()=> ((Transform)c).localRotation, (c,s,i) => x => ((Transform)c).localRotation = x),
                    new PlugDataTween("766861c1-a51c-4323-b3e4-acd9e3d5c521", "Scale", (c,s,i) => ()=> ((Transform)c).localScale, (c,s,i) => x => ((Transform)c).localScale = x),
                    new PlugDataTween("cbee60a3-d1f9-46a3-89e8-ba493413e80f", "Punch/Position", (c,s,i) => ()=> ((Transform)c).position, (c,s,i) => x => ((Transform)c).position = x, PluginTweenType.Punch),
                    new PlugDataTween("595ff002-21f8-428d-9864-a11ce7aafaed", "Punch/Local Position", (c,s,i) => ()=> ((Transform)c).localPosition, (c,s,i) => x => ((Transform)c).localPosition = x, PluginTweenType.Punch),
                    new PlugDataTween("cbafde2c-ad28-4f2d-90f4-635d9e404247", "Punch/Rotation", (c,s,i) => ()=> ((Transform)c).eulerAngles, (c,s,i) => x => ((Transform)c).eulerAngles = x, PluginTweenType.Punch),
                    new PlugDataTween("5d5a8c7d-8155-4e6d-add0-b7a1492402b5", "Punch/Local Rotation", (c,s,i) => ()=> ((Transform)c).localEulerAngles, (c,s,i) => x => ((Transform)c).localEulerAngles = x, PluginTweenType.Punch),
                    new PlugDataTween("2dbe9c8e-3e30-4165-8c3a-d7bb2efbf86b", "Punch/Scale", (c,s,i) => ()=> ((Transform)c).localScale, (c,s,i) => x => ((Transform)c).localScale = x, PluginTweenType.Punch),
                    new PlugDataTween("532aff0b-105f-4a04-840e-492330af0053", "Shake/Position", (c,s,i) => ()=> ((Transform)c).position, (c,s,i) => x => ((Transform)c).position = x, PluginTweenType.Shake),
                    new PlugDataTween("fcbc9103-108e-4d9c-b3d3-a70645a14fa3", "Shake/Local Position", (c,s,i) => ()=> ((Transform)c).localPosition, (c,s,i) => x => ((Transform)c).localPosition = x, PluginTweenType.Shake),
                    new PlugDataTween("96c35fb1-472a-41f9-8994-73fa09707098", "Shake/Rotation", (c,s,i) => ()=> ((Transform)c).eulerAngles, (c,s,i) => x => ((Transform)c).eulerAngles = x, PluginTweenType.Shake),
                    new PlugDataTween("991c6337-15a4-43e7-b00f-b6ce0f6a2c9a", "Shake/Local Rotation", (c,s,i) => ()=> ((Transform)c).localEulerAngles, (c,s,i) => x => ((Transform)c).localEulerAngles = x, PluginTweenType.Shake),
                    new PlugDataTween("2f3c9ac5-e410-472f-8191-aee0fdbe69c4", "Shake/Scale", (c,s,i) => ()=> ((Transform)c).localScale, (c,s,i) => x => ((Transform)c).localScale = x, PluginTweenType.Shake)
                );
#if true // PHYSICS_MARKER
            // Physics ------------------------------
            case "UnityEngine.Rigidbody":
                return DOVisualPluginsManager.CacheAndReturn(targetType,
                    new PlugDataTween("d1741b1f-91d6-4f87-8f26-10460ba354de", "Position", (c,s,i) => ()=> ((Rigidbody)c).position, (c,s,i) => x => ((Rigidbody)c).MovePosition(x)),
                    new PlugDataTween("cd929037-bd9c-4fc3-b944-a25e589a995d", "Rotation", (c,s,i) => ()=> ((Rigidbody)c).rotation, (c,s,i) => x => ((Rigidbody)c).MoveRotation(x))
                );
#endif
#if true // PHYSICS2D_MARKER
            // Physics2D ----------------------------
            case "UnityEngine.Rigidbody2D":
                return DOVisualPluginsManager.CacheAndReturn(targetType,
                    new PlugDataTween("6fa770db-dbd2-41da-b2e6-0aa4f63e6d95", "Position", (c,s,i) => ()=> ((Rigidbody2D)c).position, (c,s,i) => x => ((Rigidbody2D)c).MovePosition(x)),
                    new PlugDataTween("90596fff-d41b-495b-ba35-980aa5274ef5", "Rotation", (c,s,i) => ()=> ((Rigidbody2D)c).rotation, (c,s,i) => x => ((Rigidbody2D)c).MoveRotation(x))
                );
#endif
#if true // SPRITE_MARKER
            // Sprite -------------------------------
            case "UnityEngine.SpriteRenderer":
                return DOVisualPluginsManager.CacheAndReturn(targetType,
                    new PlugDataTween("628d3c0b-b2c4-44c8-b8c0-211ba2af1837", "Color", (c,s,i) => ()=> ((SpriteRenderer)c).color, (c,s,i) => x => ((SpriteRenderer)c).color = x),
                    new PlugDataTween("3ecdc523-3fad-4629-b396-4e001852e330", "Size", (c,s,i) => ()=> ((SpriteRenderer)c).size, (c,s,i) => x => ((SpriteRenderer)c).size = x)
                );
#endif
#if true // UI_MARKER
            // UI -----------------------------------
            case "UnityEngine.CanvasGroup":
                return DOVisualPluginsManager.CacheAndReturn(targetType,
                    new PlugDataTween("b44eccec-f6f1-4bff-8d71-a3c5e60f675c", "Alpha", (c,s,i) => ()=> ((CanvasGroup)c).alpha, (c,s,i) => x => ((CanvasGroup)c).alpha = x)
                );
            case "UnityEngine.UI.Image":
                return DOVisualPluginsManager.CacheAndReturn(targetType,
                    new PlugDataTween("c9b92866-07ab-41d5-98c6-cecd65151c70", "Color", (c,s,i) => ()=> ((Image)c).color, (c,s,i) => x => ((Image)c).color = x),
                    new PlugDataTween("0154e7e6-77d1-4edc-bfd8-e9c26b2a34f8", "Fill Amount", (c,s,i) => ()=> ((Image)c).fillAmount, (c,s,i) => x => ((Image)c).fillAmount = x)
                );
            case "UnityEngine.UI.LayoutElement":
                return DOVisualPluginsManager.CacheAndReturn(targetType,
                    new PlugDataTween("573ac920-a896-464f-a014-d039140628ec", "Flexible Width", (c,s,i) => ()=> ((LayoutElement)c).flexibleWidth, (c,s,i) => x => ((LayoutElement)c).flexibleWidth = x),
                    new PlugDataTween("1ffa70d0-6f05-4a01-96ae-d850f6d30f24", "Flexible Height", (c,s,i) => ()=> ((LayoutElement)c).flexibleHeight, (c,s,i) => x => ((LayoutElement)c).flexibleHeight = x),
                    new PlugDataTween("063edfa9-575d-41e4-8fa0-664703a43b40", "Min Width", (c,s,i) => ()=> ((LayoutElement)c).minWidth, (c,s,i) => x => ((LayoutElement)c).minWidth = x),
                    new PlugDataTween("6b832408-ffda-4ec6-8850-ee3c66af370a", "Min Height", (c,s,i) => ()=> ((LayoutElement)c).minHeight, (c,s,i) => x => ((LayoutElement)c).minHeight = x),
                    new PlugDataTween("cca74fac-018b-4711-ae74-c1cec137fa71", "Preferred Width", (c,s,i) => ()=> ((LayoutElement)c).preferredWidth, (c,s,i) => x => ((LayoutElement)c).preferredWidth = x),
                    new PlugDataTween("b3b69387-9345-4a36-9840-7317a11763b1", "Preferred Height", (c,s,i) => ()=> ((LayoutElement)c).preferredHeight, (c,s,i) => x => ((LayoutElement)c).preferredHeight = x)
                );
            case "UnityEngine.UI.Slider":
                return DOVisualPluginsManager.CacheAndReturn(targetType,
                    new PlugDataTween("783dee3f-90c9-44c3-808f-5b06a4510a8a", "Value", (c,s,i) => ()=> ((Slider)c).value, (c,s,i) => x => ((Slider)c).value = x)
                );
            case "UnityEngine.UI.Text":
                return DOVisualPluginsManager.CacheAndReturn(targetType,
                    new PlugDataTween("6b66dcc1-2a4f-48b2-9581-09c01959fb9d", "Color", (c,s,i) => ()=> ((Text)c).color, (c,s,i) => x => ((Text)c).color = x),
                    new PlugDataTween("631f202c-1c47-40d4-b54d-61cd2d4b674b", "Text", (c,s,i) => ()=> ((Text)c).text, (c,s,i) => x => ((Text)c).text = x),
                    new PlugDataTween("78d4c45f-af7f-4437-842c-b5849ae18e20", "Font Size", (c,s,i) => ()=> ((Text)c).fontSize, (c,s,i) => x => ((Text)c).fontSize = x)
                );
#endif
#if true // TEXTMESHPRO_MARKER
            // EXTRA - TextMesh Pro -----------------
            case "TMPro.TMP_Text":
            case "TMPro.TextMeshPro":
            case "TMPro.TextMeshProUGUI":
                return DOVisualPluginsManager.CacheAndReturn(targetType,
                    new PlugDataTween("002a6b63-4bc5-40e1-9957-c86fb6d3f686", "Color", (c,s,i) => ()=> ((TMP_Text)c).color, (c,s,i) => x => ((TMP_Text)c).color = x),
                    new PlugDataTween("bbd5bf2c-fa54-4b54-a39f-a3494633c170", "Face Color", (c,s,i) => ()=> ((TMP_Text)c).faceColor, (c,s,i) => x => ((TMP_Text)c).faceColor = x),
                    new PlugDataTween("8d9fee08-2fdf-4df3-9835-f2605491dffd", "Outline Color", (c,s,i) => ()=> ((TMP_Text)c).outlineColor, (c,s,i) => x => ((TMP_Text)c).outlineColor = x),
                    new PlugDataTween("1c3cb4ef-91ce-40e5-85f6-786eb563f445", "Text", (c,s,i) => ()=> ((TMP_Text)c).text, (c,s,i) => x => ((TMP_Text)c).text = x),
                    new PlugDataTween("f8c431c7-6da0-46f2-9b9e-4c923ba948f9", "Font Size", (c,s,i) => ()=> ((TMP_Text)c).fontSize, (c,s,i) => x => ((TMP_Text)c).fontSize = x),
                    new PlugDataTween("264c77b6-7e6e-4ecd-b672-f76c04d9cdc2", "Max Visible Chars", (c,s,i) => ()=> ((TMP_Text)c).maxVisibleCharacters, (c,s,i) => x => ((TMP_Text)c).maxVisibleCharacters = x),
                    new PlugDataTween("01da2259-5125-46a6-be08-45129184bc4b", "Max Visible Words", (c,s,i) => ()=> ((TMP_Text)c).maxVisibleWords, (c,s,i) => x => ((TMP_Text)c).maxVisibleWords = x)
                );
#endif
            }
            // Special cases where we want to look also by parent class
            if (targetType.IsSubclassOf(typeof(Graphic))) {
                return DOVisualPluginsManager.CacheAndReturn(targetType,
                    new PlugDataTween("f7e3b8ab-7a2e-450b-9934-8c48f9e5d0bd", "Color", (c,s,i) => ()=> ((Graphic)c).color, (c,s,i) => x => ((Graphic)c).color = x)
                );
            }
            return null;
        }
    }
}
