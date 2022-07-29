// Author: Pietro Polsinelli - http://designAGame.eu
// Twitter https://twitter.com/ppolsinelli
// License: WTFPL, all free as in free beer :-)
// Tested with Unity 5.6.1
// Created: 06 02 2017

using System;
using DG.Debugging;
using DG.DeebugLib;
using DG.DeebugLib.RuntimeConsole;
using DG.DeInspektor.Attributes;
using DG.Tweening;
using UnityEngine;

namespace OL
{
    [RequireComponent(typeof(I18n))]
    [RequireComponent(typeof(SimpleGameTimeManager))]
    public abstract class SimpleGameManager : MonoBehaviour
    {
        [DeHeader("I18n", "FFFFFFFF", "4C7353FF")]
        public bool UseDefaultI18n;
        public string I18nSpreadsheetId;
        public string I18nTabId;
        protected I18n I18N;

        [DeHeader("Audio", "FFFFFFFF", "4C7353FF")]
        public float ReferenceMasterAudioVolume;

        [DeHeader("Computed", "000000FF", "C7B299FF")]
        public State FrameworkAndPlayState;

        [DeHeader("Configuration", "FFFFFFFF", "4C7353FF")]
        //[DeColoredLabel("000000FFF", "EDD55AFF")]
        [DeToggleButton()]
        public bool Production;
        [DeComment("This when active will also turn on DOTween safe mode.")]
        [DeComment("When not active press F in editor to speed up and G to slow down.")]


        //[DeColoredLabel("000000FFF", "EDD55AFF")]
        [SerializeField]
        [DeToggleButton()]
        bool DebugSpecific;
        [DeComment("This is used for local hacks and should have no usages.")]

        //[DeColoredLabel("000000FFF", "EDD55AFF")]
        [DeToggleButton()]
        [SerializeField]
        bool NoDownloadData;

        //[DeColoredLabel("000000FFF", "EDD55AFF")]
        [DeToggleButton()]
        [SerializeField]
        bool DownloadDataAlsoOutsideEditor;
        [DeComment("NoDownloadData wins over this.")]

        //other
        public static string Build;

        public static SimpleGameManager InjectedReferrableInstance;
        public abstract void SetConcreteInstance();

        void Awake()
        {
            if (InjectedReferrableInstance == null)
            {
                Screen.sleepTimeout = SleepTimeout.NeverSleep;

                if (!DOTween.useSafeMode)
                    DOTween.useSafeMode = Production;

                var asset = Resources.Load("ol.build") as TextAsset;
                if (asset != null)
                    Build = $"{Application.version}.{asset.text}";
                else
                    Build = $"{Application.version}.no-build-info";

                if (!Production)
                {
                    Deebug.EnableConsole().
                        AddExtraReportInfo("BUILD: " + Build);
                }

                SetConcreteInstance();

                FrameworkAndPlayState = State.NewInstance();
                FrameworkAndPlayState.Setupping();

                DontDestroyOnLoad(gameObject);
            }
            else
            {
                Destroy(gameObject);
            }
        }

        public void Start()
        {
            if (UseDefaultI18n)
            {
                I18n.I.Setup = Setuppable.NewInstance();
                if (string.IsNullOrEmpty(I18nSpreadsheetId))
                    throw new Exception("UseDefaultI18n active without I18nSpreadsheetId");
                I18N = GetComponent<I18n>();
                I18N.SetupI18n(I18nSpreadsheetId, I18nTabId);
            }

            if (!Production)
            {
                Debug.Log("Development release");
            }
            OnStart();
        }

        public abstract void OnStart();

        public abstract void SetPlayStateToRunAndFirstStartTime();

        public static bool NotReadyYet()
        {
            return InjectedReferrableInstance == null ||
                   !InjectedReferrableInstance.FrameworkAndPlayState.IsRunning();
        }

        public static bool IsDebuggingSpecific()
        {
            return !InjectedReferrableInstance.Production &&
                   InjectedReferrableInstance.DebugSpecific;
        }

        public bool DownloadData()
        {
            return true;
            //return !InjectedReferrableInstance.Production &&

            //       (Application.isEditor ||
            //        InjectedReferrableInstance.DownloadDataAlsoOutsideEditor
            //       ) &&

            //       !InjectedReferrableInstance.NoDownloadData; 
           
        }
    }
}