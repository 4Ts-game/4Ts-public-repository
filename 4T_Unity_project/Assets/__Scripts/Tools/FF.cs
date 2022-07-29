// Author: Pietro Polsinelli - http://designAGame.eu
// Twitter https://twitter.com/ppolsinelli
// License: WTFPL, all free as in free beer :-)
// Tested with Unity 5.6.1
// Created: 09 06 2017

using System;
using DG.Tweening;
using UnityEngine;
using UnityEngine.UI;

namespace OL
{
    public static class FF
    {
        public static void DoBlurPanel(
            RectTransform Blur,
            Image BlurColorFilter,
            float maxBlur,
            float fadeTo,
            float timeToBlur,
            Action callback = null)
        {
            var blurMaterial = Blur.GetComponent<Image>().material;
            BlurColorFilter.DOFade(fadeTo, timeToBlur * .9f);
            blurMaterial.SetFloat("_Size", 0);
            Blur.gameObject.SetActive(true);
            DOTween.To(() => blurMaterial.GetFloat("_Size"), x => blurMaterial.SetFloat("_Size", x), maxBlur,
                    timeToBlur)
                .OnComplete(
                    () =>
                    {
                        if (callback != null)
                            callback();
                    });
        }

        public static void EndBlurPanel(RectTransform Blur, Image BlurColorFilter, float timeToUnBlur,
            Action callback = null)
        {
            var blurMaterial = Blur.GetComponent<Image>().material;
            BlurColorFilter.DOFade(0, timeToUnBlur * .9f);
            DOTween.To(() => blurMaterial.GetFloat("_Size"), x => blurMaterial.SetFloat("_Size", x), 0, timeToUnBlur)
                .OnComplete(() =>
                {
                    Blur.gameObject.SetActive(false);
                    if (callback != null)
                        callback();
                });
        }
    }
}