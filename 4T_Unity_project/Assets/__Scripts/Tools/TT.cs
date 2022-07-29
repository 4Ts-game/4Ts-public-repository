// Author: Pietro Polsinelli - http://designAGame.eu
// Twitter https://twitter.com/ppolsinelli
// License: WTFPL, all free as in free beer :-)
// Tested with Unity 2018.2
// Created: 07 07 2016

using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using DG.DeAudio;
using DG.Debugging;
using DG.DeExtensions;
using DG.Tweening;
using TMPro;
using UnityEngine;
using UnityEngine.EventSystems;
using UnityEngine.Networking;
using UnityEngine.UI;
using Random = UnityEngine.Random;

namespace OL
{
    /// <summary>
    ///     Also using those in DemiLib
    ///     http://demigiant.github.io/apis/demilib/html/namespace_d_g_1_1_de_extensions.html
    /// </summary>
    public static class TT
    {
        public static string DEFAULT_TWEEN_ID = "OL_TWEEN";

        //------------------------------------------------------------------------------------------------
        // Default tagging
        public static readonly string FirstText = "Generic/FirstText";
        public static readonly string SecondText = "Generic/SecondText";
        public static readonly string ThirdText = "Generic/ThirdText";

        public static readonly string FirstImage = "Generic/FirstImage";
        public static readonly string SecondImage = "Generic/SecondImage";
        public static readonly string ThirdImage = "Generic/ThirdImage";
        public static readonly string FourthImage = "Generic/FourthImage";
        public static readonly string FifthImage = "Generic/FifthImage";
        
        //------------------------------------------------------------------------------------------------
        // Finite Numbers       

        public enum FinNum
        {
            Zero = 0,
            One = 1,
            Two = 2,
            Three = 3
        }

        public enum RelativeFinNum
        {
            MinusThree = -3,
            MinusTwo = -2,
            MinusOne = -1,
            Zero = 0,
            One = 1,
            Two = 2,
            Three = 3
        }

        public static FinNum RandomPosFinNum()
        {
            return (FinNum)Random.Range(1, 4);
        }

        public static FinNum RandomFinNum()
        {
            return (FinNum)Random.Range(0, 4);
        }

        public static float FinNumPercDistrib(FinNum fn)
        {
            return 0.25f + (0.25f * (int)fn);
        }


        public static FinNum DecreaseFinNum(this FinNum fn)
        {
            if (fn == FinNum.Zero)
                return fn;
            var decreaseFinNum = (FinNum)((int)fn - 1);
            return decreaseFinNum;
        }

        public static FinNum IncreaseFinNum(this FinNum fn)
        {
            if (fn == FinNum.Three)
                return fn;
            return (FinNum)((int)fn + 1);
        }

        public static FinNum ModifyBy(this FinNum fn, int delta)
        {
            return ModifyFinNum(fn, delta);
        }

        public static FinNum ModifyBy(this FinNum fn, FinNum delta)
        {
            return ModifyFinNum(fn, (int)delta);
        }

        public static FinNum ModifyBy(this FinNum fn, RelativeFinNum delta)
        {
            return ModifyFinNum(fn, (int)delta);
        }

        public static FinNum ModifyFinNum(FinNum num, int delta)
        {
            if (delta == 0)
                return num;
            if (delta > 0)
                for (var x = 0; x < delta; x++)
                    num = num.IncreaseFinNum();
            else if (delta < 0)
                for (var x = 0; x > delta; x--)
                    num = num.DecreaseFinNum();
            return num;
        }

        public static RelativeFinNum DecreaseRelativeFinNum(this RelativeFinNum fn)
        {
            if (fn == RelativeFinNum.MinusThree)
                return fn;
            var decreaseFinNum = (RelativeFinNum)((int)fn - 1);
            return decreaseFinNum;
        }

        public static RelativeFinNum IncreaseRelativeFinNum(this RelativeFinNum fn)
        {
            if (fn == RelativeFinNum.Three)
                return fn;
            return (RelativeFinNum)((int)fn + 1);
        }

        public static RelativeFinNum ModifyRelativeFinNum(RelativeFinNum num, int delta)
        {
            if (delta == 0)
                return num;
            if (delta > 0)
                for (var x = 0; x < delta; x++)
                    num = num.IncreaseRelativeFinNum();
            else if (delta < 0)
                for (var x = 0; x > delta; x--)
                    num = num.DecreaseRelativeFinNum();
            return num;
        }

        public static RelativeFinNum GetFromSheet(string code)
        {
            if (string.IsNullOrEmpty(code))
                return RelativeFinNum.Zero;

            if (code[0] == 'R')
            {
                int min = int.Parse(code.Substring(1, 2));
                int max = int.Parse(code.Substring(3, 2));
                int rnd = Random.Range(min, max + 1);
                return (RelativeFinNum)rnd;
            }
            else if (code[0] == 'M')
                return (RelativeFinNum)(-int.Parse(code[1] + ""));
            else
                return (RelativeFinNum)(int.Parse(code[1] + ""));
        }


        //------------------------------------------------------------------------------------------------
        // Randomness

        public static T GetRandomWeightObject<T>(Dictionary<T, float> spectrumOfChancesForEach)
        {
            // the highest number we want to roll
            float totalChance = 0;

            Dictionary<T, float> objectBoundaries = new Dictionary<T, float>();

            foreach (var value in spectrumOfChancesForEach)
            {
                totalChance += value.Value;
                objectBoundaries[value.Key] = totalChance;
            }

            float res = Random.Range(0, totalChance);

            T result = default(T);

            float currentBoundary = float.MaxValue;

            foreach (var objectBoundary in objectBoundaries)
            {
                if (objectBoundary.Value > res && objectBoundary.Value < currentBoundary)
                {
                    currentBoundary = objectBoundary.Value;
                    result = objectBoundary.Key;
                }
            }

            return result;
        }

        public static T OneRndExcluding<T>(this IList<T> list, T excluded)
        {
            var listToClone = list.Where(item => !item.Equals(excluded)).ToList();
            return listToClone[Random.Range(0, listToClone.Count)];
        }

        public static T OneRnd<T>(this IList<T> list)
        {
            return list[Random.Range(0, list.Count)];
        }

        public static int PosOrNegRND()
        {
            return Random.Range(0f, 1f) > .5 ? 1 : -1;
        }

        public static bool BoolRND()
        {
            return PosOrNegRND() > 0;
        }

        private static System.Random rng = new System.Random();
        public static void Shuffle<T>(this IList<T> list)
        {
            int n = list.Count;
            while (n > 1)
            {
                n--;
                int k = rng.Next(n + 1);
                T value = list[k];
                list[k] = list[n];
                list[n] = value;
            }
        }

        //------------------------------------------------------------------------------------------------
        // Extensions

        public static bool Between(this int num, int lower, int upper, bool inclusive = true)
        {
            return Betweenf(num, lower, upper, inclusive);
        }

        public static bool Betweenf(this float num, float lower, float upper, bool inclusive = true)
        {
            return inclusive
                ? lower <= num && num <= upper
                : lower < num && num < upper;
        }

        public static bool IsEven(this int x)
        {
            return x % 2 == 0;
        }

        public static string ConstPrettify(this string s)
        {
            var prettify = s.Replace("_", " ");
            return prettify;
        }

        public static string AddNewParagraph(this string s, string par)
        {
            if (IsNullOrWhitespace(s))
                return par;
            else
                return s + "\n\n" + par;
        }

        public static void AddOrIncrement<T>(this IDictionary<T,int> dictionary, T key)
        {
            if (dictionary.ContainsKey(key))
                dictionary[key] = dictionary[key] + 1;
            else
                dictionary[key] = 1;
        }

        //------------------------------------------------------------------------------------------------
        // Shortcuts

        public static bool Ex(params string[] args)
        {
            var ex = true;
            foreach (var s in args)
            {
                ex = ex && s != null && s.Trim().Length > 0;
                if (!ex)
                    break;
            }
            return ex;
        }

        public static bool Ex<T>(IEnumerable<T> o)
        {
            var result = false;
            if (o != null)
                result = o.Any();
            return result;
        }

        public static IEnumerable<T> EnumValues<T>()
        {
            return Enum.GetValues(typeof(T)).Cast<T>();
        }


        //------------------------------------------------------------------------------------------------
        //Colors

        public static string ColorToHex(Color32 color)
        {
            var hex = color.r.ToString("X2") + color.g.ToString("X2") + color.b.ToString("X2");
            return hex;
        }

        public static Color HexToColor(string hex)
        {
            var r = byte.Parse(hex.Substring(0, 2), NumberStyles.HexNumber);
            var g = byte.Parse(hex.Substring(2, 2), NumberStyles.HexNumber);
            var b = byte.Parse(hex.Substring(4, 2), NumberStyles.HexNumber);
            return new Color32(r, g, b, 255);
        }


        //http://gamedev.stackexchange.com/questions/38536/given-a-rgb-color-x-how-to-find-the-most-contrasting-color-y
        public static Color ContrastColor(Color c)
        {
            var ret = Color.white;

            var Y = 0.2126f * c.r + 0.7152f * c.g + 0.0722f * c.b;

            //float S = (Mathf.Max(c.r, c.g, c.b) - Mathf.Min(c.r, c.g, c.b)) / Mathf.Max(c.r, c.g, c.b);

            if (Y > .5f)
                ret = Color.black;
            return ret;
        }

        public static Color BrightenColor(Color c)
        {
            var hsbColor = new HSBColor(c);
            if (hsbColor.s < .5f)
                hsbColor.s = .8f;
            return hsbColor.ToColor();
        }


        //------------------------------------------------------------------------------------------------
        // Search

        public static Transform Search(this Transform target, string name, bool alsoInactive = true)
        {
            if (target.name == name)
                return target;

            var componentsInChildren = target.GetComponentsInChildren<Transform>(alsoInactive);

            foreach (var componentInChild in componentsInChildren)
            {
                if (target == componentInChild)
                    continue;

                var result = Search(componentInChild, name);

                if (result != null)
                    return result;
            }

            return null;
        }

        public static List<Transform> Searches(this Transform target, string name, bool alsoInactive = true)
        {
            List<Transform> results = new List<Transform>();
            DoSearches(target, name, alsoInactive, results);
            return results;
        }

        static void DoSearches(Transform target, string name, bool alsoInactive, List<Transform> results)
        {
            if (target.name == name)
            {
                results.Add(target);
            }
                
            var componentsInChildren = target.GetComponentsInChildren<Transform>(alsoInactive);

            foreach (var componentInChild in componentsInChildren)
            {
                if (target == componentInChild)
                    continue;

                DoSearches(componentInChild, name, alsoInactive, results);
            }
        }


        public static GameObject FindGameObjectChildWithTag(GameObject parent, string tag)
        {
            var ch = parent.GetComponentsInChildren<Transform>(true);
            foreach (var c in ch)
                if (c.tag == tag)
                    return c.gameObject;
            return null;
        }

        public static List<GameObject> FindGameObjectsChildrenWithTag(GameObject parent, string tag)
        {
            var results = new List<GameObject>();
            var ch = parent.GetComponentsInChildren<Transform>(true);
            foreach (var c in ch)
                if (c.tag == tag)
                    results.Add(c.gameObject);
            return results;
        }

        public static Transform GetParentWithTag(Transform Element, string Tag)
        {
            if (Element == null)
                return null;

            Transform t = Element;
            if (t.tag != Tag)
                return TT.GetParentWithTag(t.parent, Tag);
            else
                return t;
        }

        //------------------------------------------------------------------------------------------------
        // UI

        public static bool IsPointerOverUIObject()
        {
            return IsPointerOverUIObject(null);
        }

        public static bool IsPointerOverUIObject(string filteringTag)
        {
            var isPointerOverGameObject = false;

            var eventDataCurrentPosition = new PointerEventData(EventSystem.current);
            eventDataCurrentPosition.position = new Vector2(Input.mousePosition.x, Input.mousePosition.y);
            var results = new List<RaycastResult>();
            EventSystem.current.RaycastAll(eventDataCurrentPosition, results);
            int goodResults = 0;

            foreach (RaycastResult result in results)
            {
                if (!string.IsNullOrEmpty(filteringTag))
                {
                    if (result.gameObject.tag == filteringTag)
                        goodResults++;
                }
                else
                {
                    goodResults++;
                }
            }
            isPointerOverGameObject = results.Count > 0;
            return isPointerOverGameObject;
        }

        public static void FadeInCallback(
            CanvasGroup canvasGroup,
            float fadeInDuration,
            Action callback = null,
            string tweenId=null)
        {
            FadeIn(canvasGroup, fadeInDuration, false, 2f, 0.5f, 1, callback, tweenId);
        }

        public static void FadeIn(
            CanvasGroup canvasGroup,
            float fadeInDuration,
            bool alsoFadeOut = false,
            float timeBeforeFadeOut = 2f,
            float fadeOutDuration = .5f,
            float fadeToValue = 1,
            Action callback = null,
            string tweenId = null)
        {
            FadeConfiguration fc = new FadeConfiguration(alsoFadeOut ? FadeConfiguration.FadeType.IN_OUT : FadeConfiguration.FadeType.IN);
            fc.Canvas = canvasGroup;
            fc.FadeInDuration = fadeInDuration;
            fc.TimeBeforeFadeOut = timeBeforeFadeOut;
            fc.FadeOutDuration = fadeOutDuration;
            fc.FadeToValue = fadeToValue;
            fc.Callback = callback;
            fc.TweendId = tweenId;
            FadeInOut(fc);
        }

        public static void FadeIn(
            Image image,
            float fadeInDuration,
            bool alsoFadeOut = false,
            float timeBeforeFadeOut = 2f,
            float fadeOutDuration = .5f,
            float fadeToValue = 1,
            Action callback = null,
            string tweenId = null)
        {
            FadeConfiguration fc = new FadeConfiguration(alsoFadeOut ? FadeConfiguration.FadeType.IN_OUT : FadeConfiguration.FadeType.IN);
            fc.ImageUI = image;
            fc.FadeInDuration = fadeInDuration;
            fc.TimeBeforeFadeOut = timeBeforeFadeOut;
            fc.FadeOutDuration = fadeOutDuration;
            fc.FadeToValue = fadeToValue;
            fc.Callback = callback;
            fc.TweendId = tweenId;
            FadeInOut(fc);
        }

        public static void FadeIn(
            TextMeshProUGUI text,
            float fadeInDuration,
            bool alsoFadeOut = false,
            float timeBeforeFadeOut = 2f,
            float fadeOutDuration = .5f,
            float fadeToValue = 1,
            Action callback = null,
            string tweenId = null)
        {
            FadeConfiguration fc = new FadeConfiguration(alsoFadeOut ? FadeConfiguration.FadeType.IN_OUT : FadeConfiguration.FadeType.IN);
            fc.Text = text;
            fc.FadeInDuration = fadeInDuration;
            fc.TimeBeforeFadeOut = timeBeforeFadeOut;
            fc.FadeOutDuration = fadeOutDuration;
            fc.FadeToValue = fadeToValue;
            fc.Callback = callback;
            fc.TweendId = tweenId;
            FadeInOut(fc);
        }

        public static void FadeIn(
            Transform treeOfSpritesAndText,
            float fadeInDuration,
            bool alsoFadeOut = false,
            float timeBeforeFadeOut = 2f,
            float fadeOutDuration = .5f,
            float fadeToValue = 1,
            Action callback = null,
            string tweenId = null)
        {
            FadeConfiguration fc = new FadeConfiguration(alsoFadeOut ? FadeConfiguration.FadeType.IN_OUT : FadeConfiguration.FadeType.IN);
            fc.TreeOfSpritesAndText = treeOfSpritesAndText;
            fc.FadeInDuration = fadeInDuration;
            fc.TimeBeforeFadeOut = timeBeforeFadeOut;
            fc.FadeOutDuration = fadeOutDuration;
            fc.FadeToValue = fadeToValue;
            fc.Callback = callback;
            fc.TweendId = tweenId;


            FadeInOut(fc);
        }

        //todo move for all cases
        public static void FadeInOut(FadeConfiguration fc)
        {
            if (IsNullOrWhitespace(fc.TweendId))
                fc.TweendId = DEFAULT_TWEEN_ID;

            if (fc.Type == FadeConfiguration.FadeType.IN || fc.Type == FadeConfiguration.FadeType.IN_OUT)
            {
                //sprites
                if (fc.TreeOfSpritesAndText != null)
                {
                    var spriteRenderers = fc.TreeOfSpritesAndText.GetComponentsInChildren<SpriteRenderer>();
                    foreach (var spriteRenderer in spriteRenderers)
                    {
                        spriteRenderer.DOFade(fc.FadeToValue, fc.FadeInDuration).SetId(fc.TweendId);
                    }
                    var tmps = fc.TreeOfSpritesAndText.GetComponentsInChildren<TextMeshPro>();
                    foreach (var tmp in tmps)
                    {
                        tmp.DOFade(fc.FadeToValue, fc.FadeInDuration).SetId(fc.TweendId);
                    }

                    if (fc.Callback != null && fc.Type == FadeConfiguration.FadeType.IN)
                        fc.Callback();
                }

                //text UI
                else if (fc.Text != null)
                {
                    fc.Text.SetAlpha(0);
                    fc.Text.gameObject.SetActive(true);

                    fc.Text.DOFade(fc.FadeToValue, fc.FadeInDuration).OnComplete(() =>
                    {
                        if (fc.Type == FadeConfiguration.FadeType.IN_OUT)
                        {
                            DOVirtual.DelayedCall(fc.TimeBeforeFadeOut, () => { FadeOutAndMove(fc); }).SetId(fc.TweendId); ;
                        }
                        else
                        {
                            if (fc.Callback != null && fc.Type == FadeConfiguration.FadeType.IN)
                                fc.Callback();
                        }
                    });
                }

                //image
                else if (fc.ImageUI != null)
                {
                    fc.ImageUI.SetAlpha(0);
                    fc.ImageUI.gameObject.SetActive(true);

                    fc.ImageUI.DOFade(fc.FadeToValue, fc.FadeInDuration).SetId(fc.TweendId).OnComplete(() =>
                    {
                        if (fc.Type == FadeConfiguration.FadeType.IN_OUT)
                        {
                            DOVirtual.DelayedCall(fc.TimeBeforeFadeOut, () => { FadeOutAndMove(fc); }).SetId(fc.TweendId);
                        }
                        else
                        {
                            if (fc.Callback != null)
                                fc.Callback();
                        }
                    });                   
                }

                // canvas
                else if (fc.Canvas != null)
                {
                    fc.Canvas.alpha = 0;
                    fc.Canvas.gameObject.SetActive(true);

                    if (fc.DeltaMovingIn != null)
                    {
                        var originalPosition = fc.Canvas.transform.localPosition;
                        var rectTransform = ((RectTransform)fc.Canvas.transform);

                        Tweener doAnchorPos =
                            rectTransform.DOAnchorPos((Vector3)(originalPosition - fc.DeltaMovingIn), 0).SetId(fc.TweendId);
                        doAnchorPos.OnComplete(() =>
                        {
                            var move = rectTransform.DOAnchorPos(originalPosition, fc.FadeInDuration).SetId(fc.TweendId);
                            if (fc.MovingInEaseCurve != null)
                            {
                                move.SetEase(fc.MovingInEaseCurve);
                            }
                            else
                            {
                                move.SetEase(fc.MovingInEase);
                            }
                        });
                    }

                    fc.Canvas.DOFade(fc.FadeToValue, fc.FadeInDuration).SetId(fc.TweendId).OnComplete(() =>
                    {
                        if (fc.Type == FadeConfiguration.FadeType.IN_OUT)
                        {
                            DOVirtual.DelayedCall(fc.TimeBeforeFadeOut, () => { FadeOutAndMove(fc); }).SetId(fc.TweendId);
                        }
                        else
                        {
                            if (fc.Callback != null)
                                fc.Callback();
                        }
                    });
                }
                else if (fc.Type == FadeConfiguration.FadeType.OUT)
                {
                    FadeOutAndMove(fc);
                }
            }
        }

        static void FadeOutAndMove(FadeConfiguration fc)
        {
            //sprites
            if (fc.TreeOfSpritesAndText != null)
            {
                var spriteRenderers = fc.TreeOfSpritesAndText.GetComponentsInChildren<SpriteRenderer>();
                foreach (var spriteRenderer in spriteRenderers)
                {
                    spriteRenderer.DOFade(0, fc.FadeOutDuration).SetId(fc.TweendId);
                }
                var tmps = fc.TreeOfSpritesAndText.GetComponentsInChildren<TextMeshPro>();
                foreach (var tmp in tmps)
                {
                    tmp.DOFade(0, fc.FadeOutDuration).SetId(fc.TweendId);
                }

                if (fc.Callback != null)
                    fc.Callback();
            }
            
            //text
            else if (fc.Text != null)
            {
                fc.Text.DOFade(0, fc.FadeOutDuration).SetId(fc.TweendId).OnComplete(() =>
                {
                    fc.Text.gameObject.SetActive(false);
                    if (fc.Callback != null)
                        fc.Callback();
                });
            }

            //image
            else if (fc.ImageUI != null)
            {
                fc.ImageUI.DOFade(0, fc.FadeOutDuration).SetId(fc.TweendId).OnComplete(() =>
                {
                    fc.ImageUI.gameObject.SetActive(false);
                    if (fc.Callback != null)
                        fc.Callback();
                });
            }

            // canvas
            else if (fc.Canvas != null)
            {
                fc.Canvas.DOFade(0, fc.FadeOutDuration).SetId(fc.TweendId).OnComplete(() =>
                {
                    fc.Canvas.gameObject.SetActive(false);
                    if (fc.Callback != null)
                        fc.Callback();
                });

                if (fc.DeltaMovingOut != null)
                {
                    var rectTransform = ((RectTransform)fc.Canvas.transform);

                    Tweener doAnchorPos =
                        rectTransform.DOAnchorPos((Vector3)(fc.Canvas.transform.position + fc.DeltaMovingOut),
                            fc.FadeOutDuration).SetId(fc.TweendId);

                    if (fc.MovingOutEaseCurve != null)
                    {
                        doAnchorPos.SetEase(fc.MovingOutEaseCurve);
                    }
                    else
                    {
                        doAnchorPos.SetEase(fc.MovingOutEase);
                    }
                }
            }
        }

        public class FadeConfiguration
        {
            public enum FadeType
            {
                IN,
                OUT,
                IN_OUT
            }

            public CanvasGroup Canvas;
            public Image ImageUI;
            public TextMeshProUGUI Text;
            public Transform TreeOfSpritesAndText;

            public readonly FadeType Type;
            public float FadeInDuration;
            public float TimeBeforeFadeOut = 2f;
            public float FadeOutDuration = .5f;
            public float FadeToValue = 1;
            public Action Callback = null;
            public Vector3? DeltaMovingIn;
            public Ease MovingInEase = Ease.Linear;
            public AnimationCurve MovingInEaseCurve;
            public Vector3? DeltaMovingOut;
            public Ease MovingOutEase = Ease.Linear;
            public AnimationCurve MovingOutEaseCurve;
            public string TweendId;

            public FadeConfiguration(FadeType type)
            {
                Type = type;
            }

            
        }

        public static void FadeOut(
            CanvasGroup canvasGroup,
            float fadeOutDuration = .5f,
            Action callback = null)
        {
            canvasGroup.DOFade(0, fadeOutDuration).OnComplete(() =>
            {
                canvasGroup.gameObject.SetActive(false);
                if (callback != null)
                    callback();
            });
        }

        public static void MakeButtonSafeForOneSec()
        {
            MakeButtonSafe(1);
        }

        public static void MakeButtonSafe(float deadTime)
        {
            var clicked = new PointerEventData(EventSystem.current).selectedObject;
            if (clicked != null)
            {
                var button = clicked.GetComponent<Button>();
                if (button != null)
                {
                    var enableStateBeforeClick = button.enabled;
                    button.enabled = !enableStateBeforeClick;
                    //Debug.Log("MakeButtonSafe temp disabled");
                    DOVirtual.DelayedCall(deadTime, () =>
                    {
                        if (button != null)
                        {
                            button.enabled = enableStateBeforeClick;
                            //Debug.Log("MakeButtonSafe re-enabled");
                        }
                    });
                }
                else
                {
                    Debug.Log("MakeButtonSafe no button");
                }
            }
            else
            {
                Debug.Log("MakeButtonSafe no object");
            }
        }


        //------------------------------------------------------------------------------------------------
        // Effects

        /// <summary>
        ///     Assumes parent of textComponent to be disabled at text show end
        /// </summary>
        /// <param name="textComponent"></param>
        /// <param name="text"></param>
        /*public static void RevealCharacters(RevealCharactersConfig cf, int startFromChar = 0)
        {
            cf.TextComponent.maxVisibleCharacters = startFromChar;
            cf.TextComponent.text = cf.Text;
            cf.TextComponent.transform.parent.gameObject.SetActive(true);
            var totalVisibleCharacters = startFromChar + cf.Text.Length;
            DoReveal(totalVisibleCharacters, cf);
        }*/

        public class RevealCharactersConfig
        {
            public TMP_Text TextComponent;
            public string Text;
            public float DelayBetweenCharReveal;
            public float TimePerCharWaitWhenFinished;
            public float AdditionalDelayOnCompletionCallback;
            public bool DisableParentOnEnd;
            public Action CallbackOnCompletionAfterPause;
            public Action CallbackOnTypeEnded;
            public Func<bool> VerifyEndTypeNow;
            public AudioClip Feedback;
            public Ease TypeEase = Ease.OutQuad;

            public float FitMarginX;
            public float FitMarginY;
            public Image FitThisImage;


            public RevealCharactersConfig(
             TMP_Text textComponent,
             string text,
             float delayBetweenCharReveal,
             float timePerCharWaitWhenFinished)
            {
                TextComponent = textComponent;
                Text = text;
                DelayBetweenCharReveal = delayBetweenCharReveal;
                TimePerCharWaitWhenFinished = timePerCharWaitWhenFinished;
            }

        }

        /*static void DoReveal(
             int totalVisibleCharacters,
             RevealCharactersConfig cf
            )
        {
            if (cf.Feedback != null)
                DeAudioManager.Play(cf.Feedback);

            cf.TextComponent
                .DOMaxVisibleCharacters(totalVisibleCharacters, cf.DelayBetweenCharReveal * totalVisibleCharacters).SetEase(cf.TypeEase)

                .OnStart(() =>
                {
                    if (cf.FitThisImage != null)
                    {
                        FitImageToText(cf.TextComponent,cf.Text,cf.FitMarginX,cf.FitMarginY,cf.FitThisImage);
                    }
                })
                
                .OnComplete(() =>
                {
                    if (cf.Feedback != null)
                        DeAudioManager.Stop(cf.Feedback);

                    if (cf.CallbackOnTypeEnded != null)
                        cf.CallbackOnTypeEnded();

                    DOVirtual.DelayedCall(
                        cf.TimePerCharWaitWhenFinished * cf.TextComponent.text.Length +
                        cf.AdditionalDelayOnCompletionCallback, () =>
                        {
                            if (cf.DisableParentOnEnd)
                            {
                                cf.TextComponent.text = "";
                                cf.TextComponent.transform.parent.gameObject.SetActive(false);
                            }

                            if (cf.CallbackOnCompletionAfterPause != null)
                                cf.CallbackOnCompletionAfterPause();
                        });
                });


        }*/

        public class RollToScoreConfig
        {
            public TMP_Text textComponent;
            public int start;
            public int end;
            public int step;
            public Action callback;
            public string numberpostfix;
            public AudioClip feedback;
        }

        public static void RollToScore(TMP_Text textComponent, int start, int end, int step, Action callback = null, string numberpostfix = null)
        {
            RollToScoreConfig rtsc = new RollToScoreConfig();
            rtsc.textComponent = textComponent;
            rtsc.start = start;
            rtsc.end = end;
            rtsc.step = step;
            rtsc.callback = callback;
            rtsc.numberpostfix = numberpostfix;

            RollToScore(rtsc);
        }

        public static void RollToScore(RollToScoreConfig rtsc)
        {
            rtsc.textComponent.text = rtsc.start + (string.IsNullOrEmpty(rtsc.numberpostfix) ? "" : rtsc.numberpostfix);
            RollToStep(rtsc);
        }

        static void RollToStep(RollToScoreConfig rtsc)
        {
            int score = rtsc.start;
            DOTween.To(() => score, x => score = x, rtsc.end, .1f * (rtsc.end - rtsc.start)).OnUpdate(() =>
                    {
                        var currenttext = score + (string.IsNullOrEmpty(rtsc.numberpostfix) ? "" : rtsc.numberpostfix);
                        if (currenttext != rtsc.textComponent.text)
                        {
                            rtsc.textComponent.text =
                                currenttext;
                            if (rtsc.feedback != null)
                                DeAudioManager.Play(rtsc.feedback);
                        }
                    }
                    )
                .SetEase(Ease.Linear).OnComplete(() =>
                {
                    if (rtsc.callback != null)
                        rtsc.callback();
                });
        }

        //------------------------------------------------------------------------------------------------
        // Others

        static readonly DateTime Jan1St1970 = new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc);
        /// <summary>Get extra long current timestamp</summary>
        public static long CurrentUnixTimeMillis { get { return (long)((DateTime.UtcNow - Jan1St1970).TotalMilliseconds); } }

        public static Rect ScreenRect
        {
            get { return new Rect(-Screen.width / 2f, -Screen.height / 2f, Screen.width, Screen.height); }
        }


        public static IEnumerator DownloadCSVCoroutine(string docId, Action<string> callback, bool saveAsset = false, string assetName = null, string sheetTabId = null, Action errorCallback = null)
        {
            saveAsset = saveAsset && Application.isEditor;

            string url = "https://docs.google.com/spreadsheets/d/" + docId + "/export?format=csv";

            if (!string.IsNullOrEmpty(sheetTabId))
                url += "&gid=" + sheetTabId;

            Debug.Log("sheetTabId: " + sheetTabId);
            Debug.Log("URL: " + url);

            WWWForm form = new WWWForm();
            UnityWebRequest uwr = UnityWebRequest.Post(url, form);

            //WWW download = new WWW(url, form);

            yield return uwr.SendWebRequest();

            if (!string.IsNullOrEmpty(uwr.error))
            {
                Debug.Log("URL: " + url);
                Debug.Log("Error downloading: " + uwr.error);
                Debug.Log(errorCallback);

                if (errorCallback != null)
                    errorCallback();
            }
            else
            {
                var downloadHandlerText = uwr.downloadHandler.text;

                //File.WriteAllText("Assets/Resources/" + assetName + ".csv", downloadHandlerText);

                Delogger.Log("-----------------------------------------------",null);
                Delogger.Log("url", url);
                Delogger.Log("CSV", downloadHandlerText);


                callback(downloadHandlerText);
                if (Application.isEditor && saveAsset)
                {
                    if (!string.IsNullOrEmpty(assetName))
                        File.WriteAllText("Assets/Resources/" + assetName + ".csv", downloadHandlerText);
                    else
                    {
                        throw new Exception("assetName is null");
                    }
                }
            }
        }

        //CSV reader from https://bravenewmethod.com/2014/09/13/lightweight-csv-reader-for-unity/

        public static readonly string SPLIT_RE = @",(?=(?:[^""]*""[^""]*"")*(?![^""]*""))";
        public static readonly string LINE_SPLIT_RE = @"\r\n|\n\r|\n|\r";
        public static readonly char[] TRIM_CHARS = { '\"' };

        public static List<List<string>> ReadCSV(string file)
        {
            var data = Resources.Load(file) as TextAsset;
            return ParseCSV(data.text);
        }

        public static List<List<string>> ParseCSV(string text)
        {
            text = CleanReturnInCsvTexts(text);

            var list = new List<List<string>>();
            var lines = Regex.Split(text, LINE_SPLIT_RE);

            if (lines.Length <= 1) return list;

            var header = Regex.Split(lines[0], SPLIT_RE);

            bool jumpedFirst = false;

            foreach (var line in lines)
            {
                if (!jumpedFirst)
                {
                    jumpedFirst = true;
                    continue;
                }
                var values = Regex.Split(line, SPLIT_RE);

                var entry = new List<string>();
                for (var j = 0; j < header.Length && j < values.Length; j++)
                {
                    var value = values[j];
                    value = DecodeSpecialCharsFromCSV(value);
                    entry.Add(value);
                }
                list.Add(entry);
            }
            return list;
        }

        public static string DecodeSpecialCharsFromCSV(string value)
        {
            value = value.TrimStart(TRIM_CHARS).TrimEnd(TRIM_CHARS).Replace("\\", "").Replace("<br>", "\n").Replace("<c>", ",");
            return value;
        }

        public static void PlayNTimes(this AudioSource audioSource, int times, float volume)
        {
            audioSource.PlayOneShot(audioSource.clip, 1);
            times--;
            if (times > 0)
                DOVirtual.DelayedCall(audioSource.clip.length, () => { PlayNTimes(audioSource, times, volume); });
        }


        public static string RemoveSpecialCharacter(string str, char special)
        {
            var sb = new StringBuilder();
            foreach (var c in str)
                if (c != special)
                    sb.Append(c);
            return sb.ToString();
        }

        public static string InsertParams(string translation, params string[] args)
        {
            if (!string.IsNullOrEmpty(translation))
                translation = string.Format(translation, args);
            return translation;
        }

        public static string CleanReturnInCsvTexts(string text)
        {
            text = text.Replace("\"\"", "'");

            if (text.IndexOf("\"") > -1)
            {
                string clean = "";
                bool insideQuote = false;
                for (int j = 0; j < text.Length; j++)
                {
                    if (!insideQuote && text[j] == '\"')
                    {
                        insideQuote = true;
                    }
                    else if (insideQuote && text[j] == '\"')
                    {
                        insideQuote = false;
                    }
                    else if (insideQuote)
                    {
                        if (text[j] == '\n')
                            clean += "<br>";
                        else if (text[j] == ',')
                            clean += "<c>";
                        else
                            clean += text[j];
                    }
                    else
                    {
                        clean += text[j];
                    }
                }
                text = clean;
            }
            return text;
        }


        static Vector3[] worldCorners;

        public static bool IsOutOfScreen(RectTransform uiElement)
        {
            var screenRect = new Rect(0f, 0f, Screen.width, Screen.height);
            if (worldCorners == null)
                worldCorners = new Vector3[4];
            uiElement.GetWorldCorners(worldCorners);
            foreach (var corner in worldCorners)
                if (!screenRect.Contains(corner))
                    return true;
            return false;
        }

        public static Vector3 RotatePointAroundPivot(Vector3 pointTowards, Vector3 startPivot, Vector3 angles)
        {
            Vector3 dir = pointTowards - startPivot; // get point direction relative to pivot
            dir = Quaternion.Euler(angles) * dir; // rotate it
            pointTowards = dir + startPivot; // calculate rotated point
            return pointTowards; // return it
        }

        public static bool IsNullOrWhitespace(string finalFeedback)
        {
            if (finalFeedback == null)
                return true;
            return string.IsNullOrEmpty(finalFeedback.Trim());
        }

        public static string DeltaToString(int delta)
        {
            if (delta > 0)
            {
                return "+" + delta;
            }
            else if (delta < 0)
            {
                return delta.ToString();
            }
            else
            {
                return "-";
            }
        }

        public static void FitImageToText(TMP_Text content, string text, float marginX, float marginY, Image backgroundImage)
        {
            content.text = text;
            content.ForceMeshUpdate();
            var textBounds = content.textBounds;
            var rectTransformSizeDelta =
                new Vector2(
                    textBounds.size.x + marginX,
                    textBounds.size.y + marginY
                );
            backgroundImage.rectTransform.sizeDelta = rectTransformSizeDelta;
        }
    }
}