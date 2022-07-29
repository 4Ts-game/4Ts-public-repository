// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2019/10/13

using System;
using System.Reflection;
using UnityEngine;

namespace Demigiant.DemiTools.DeUnityExtended
{
    public static class ParticleSystemUtils
    {
        #region Public Methods

        /// <summary>
        /// Copies data from one <see cref="ParticleSystem"/> into another.
        /// Note that this method uses Reflection (without caching) every time for each module
        /// </summary>
        /// <param name="fromSys">ParticleSystem to copy from</param>
        /// <param name="toSys">ParticleSystem to copy to</param>
        /// <param name="skipDuration">If TRUE doesn't copy over the eventual duration</param>
        public static void CopyDataBetweenSystems(ParticleSystem fromSys, ParticleSystem toSys, bool skipDuration = true)
        {
            CopyDataBetweenModules(fromSys.main, toSys.main, skipDuration);
            CopyDataBetweenModules(fromSys.emission, toSys.emission);
            CopyDataBetweenModules(fromSys.shape, toSys.shape);
            CopyDataBetweenModules(fromSys.velocityOverLifetime, toSys.velocityOverLifetime);
            CopyDataBetweenModules(fromSys.limitVelocityOverLifetime, toSys.limitVelocityOverLifetime);
            CopyDataBetweenModules(fromSys.inheritVelocity, toSys.inheritVelocity);
            CopyDataBetweenModules(fromSys.forceOverLifetime, toSys.forceOverLifetime);
            CopyDataBetweenModules(fromSys.colorOverLifetime, toSys.colorOverLifetime);
            CopyDataBetweenModules(fromSys.colorBySpeed, toSys.colorBySpeed);
            CopyDataBetweenModules(fromSys.sizeOverLifetime, toSys.sizeOverLifetime);
            CopyDataBetweenModules(fromSys.sizeBySpeed, toSys.sizeBySpeed);
            CopyDataBetweenModules(fromSys.rotationOverLifetime, toSys.rotationOverLifetime);
            CopyDataBetweenModules(fromSys.rotationBySpeed, toSys.rotationBySpeed);
            CopyDataBetweenModules(fromSys.externalForces, toSys.externalForces);
            CopyDataBetweenModules(fromSys.noise, toSys.noise);
            CopyDataBetweenModules(fromSys.collision, toSys.collision);
            CopyDataBetweenModules(fromSys.trigger, toSys.trigger);
            CopyDataBetweenModules(fromSys.subEmitters, toSys.subEmitters);
            CopyDataBetweenModules(fromSys.textureSheetAnimation, toSys.textureSheetAnimation);
            CopyDataBetweenModules(fromSys.lights, toSys.lights);
            CopyDataBetweenModules(fromSys.trails, toSys.trails);
            CopyDataBetweenModules(fromSys.customData, toSys.customData);
            CopyDataBetweenModules(fromSys.GetComponent<ParticleSystemRenderer>(), toSys.GetComponent<ParticleSystemRenderer>());
        }

        /// <summary>
        /// Copies data from one <see cref="ParticleSystem"/> module to another.
        /// Note that this method uses Reflection (without caching) every time for each module
        /// </summary>
        /// <param name="fromModule">Module to copy from</param>
        /// <param name="toModule">Module to copy to</param>
        /// <param name="skipDuration">If TRUE doesn't copy over the eventual duration</param>
        public static void CopyDataBetweenModules<TModule>(TModule fromModule, TModule toModule, bool skipDuration = false)
        {
            PropertyInfo[] pInfos = fromModule.GetType().GetProperties(BindingFlags.Instance | BindingFlags.Public);
            FieldInfo[] fInfos = fromModule.GetType().GetFields(BindingFlags.Instance | BindingFlags.Public);
            for (int i = 0; i < pInfos.Length; ++i) {
                PropertyInfo info = pInfos[i];
                if (!info.CanWrite) continue;
                if (CopyModulesShouldSkipThis(info.Name, skipDuration)) continue;
                info.SetValue(toModule, info.GetValue(fromModule));
            }
            for (int i = 0; i < fInfos.Length; ++i) {
                FieldInfo info = fInfos[i];
                if (CopyModulesShouldSkipThis(info.Name, skipDuration)) continue;
                info.SetValue(toModule, info.GetValue(fromModule));
            }
        }

        #endregion

        #region Methods

        static bool CopyModulesShouldSkipThis(string accessorName, bool skipDuration)
        {
            switch (accessorName) {
            case "duration":
                return skipDuration;
            case "name":
            case "material":
            case "materials":
                return true;
            default: return false;
            }
        }

        #endregion
    }
}