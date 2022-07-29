// Author: Pietro Polsinelli - http://designAGame.eu
// Twitter https://twitter.com/ppolsinelli
// License: WTFPL, all free as in free beer :-)
// Tested with Unity 5.6.1
// Created: 22 06 2017

using System;
using System.Collections.Generic;
using DG.DeAudio;
using DG.DeInspektor.Attributes;
using DG.Tweening;
using UnityEngine;
using UnityEngine.SceneManagement;

namespace OL
{
#pragma warning disable 0649
    public class SceneChanger : MonoBehaviour
    {
        [DeHeader("UI", "FFFFFFFF", "4C7353FF")]
        [DeEmptyAlert]
        [SerializeField]
        CanvasGroup Cover;

        [SerializeField]
        float AudioFadeInTime = 1;

        Setuppable Setup;

        public static SceneChanger I;

        void Awake()
        {
            if (I == null)
            {
                I = this;
                Cover.alpha = 1;
                Cover.gameObject.SetActive(true);
                Setup = Setuppable.NewInstance();
            }
        }

        void Update()
        {
            if (!SimpleGameManager.InjectedReferrableInstance.FrameworkAndPlayState.IsRunning())
                return;

            if (Setup.IsToBeSetupped())
            {
                Setup.SetupCompleted();
                Cover.DOFade(0, .5f).OnComplete(() =>
                {
                    Cover.gameObject.SetActive(false);
                });

                DeAudioManager.GetAudioGroup(DeAudioGroupId.Ambient).volume = 1f;
                DeAudioManager.GetAudioGroup(DeAudioGroupId.Custom0).volume = 1f;
                DeAudioManager.GetAudioGroup(DeAudioGroupId.Custom1).volume = 1f;
                DOTween.To(() => DeAudioManager.globalVolume, x => DeAudioManager.globalVolume = x,
                    SimpleGameManager.InjectedReferrableInstance.ReferenceMasterAudioVolume, AudioFadeInTime);
            }
        }

        public static void ChangeScene(string sceneName)
        {
            DOTween.KillAll();

            DeAudioManager.FadeOut(0.5f);

            I.Cover.alpha = 0;
            I.Cover.gameObject.SetActive(true);
            I.Cover.DOFade(1, .7f).OnComplete(() =>
            {
               SceneManager.LoadScene(sceneName,LoadSceneMode.Single);                
            });
        }
    }
}