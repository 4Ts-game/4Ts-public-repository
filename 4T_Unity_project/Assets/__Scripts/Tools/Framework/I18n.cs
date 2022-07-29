using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Text.RegularExpressions;
using TMPro;
using UnityEngine;
using Match = System.Text.RegularExpressions.Match;

namespace OL
{
    public class I18n : MonoBehaviour
    {
        public static Dictionary<string, Dictionary<string, string>> LangKeyValue;
        public Setuppable Setup;
        public static string CurrentLanguage;

        public static I18n I;

        void Awake()
        {
            if (I == null)
            {
                I = this;
                DontDestroyOnLoad(gameObject);
            }
            else
            {
                Destroy(gameObject);
            }
        }

        public static string T(params string[] keys)
        {
            if (!I.Setup.IsSetupped())
                throw new Exception("I18n not setupped");

            if (keys == null || keys.Length == 0)
            {
                Debug.Log("Calling T with null/empty argument");
                return "NULL";
            }

            string result = AttemptTranslate(CurrentLanguage, keys);

            if (string.IsNullOrEmpty(result) && "EN" != CurrentLanguage)
            {
                Debug.Log("Missing translation in " + CurrentLanguage + " for " + keys[0]);
                result = AttemptTranslate("EN", keys);
            }

            if (string.IsNullOrEmpty(result))
            {
                Debug.Log("Missing any translation for " + keys[0]);
                result = keys[0];
            }

            return result;
        }

        static string AttemptTranslate(string aLanguage, string[] keys)
        {
            string result = null;
            if (LangKeyValue[aLanguage].ContainsKey(keys[0]))
            {
                if (keys.Length == 1)
                    result = LangKeyValue[aLanguage][keys[0]];
                else
                {
                    var valueWithParams = LangKeyValue[aLanguage][keys[0]];

                    int atKey = 1;
                    while (atKey < keys.Length)
                    {
                        valueWithParams = ReplaceNthOccurance(valueWithParams, "%%", keys[atKey], 1);
                        atKey++;
                    }
                    result = valueWithParams;
                }
            }
            return result;
        }

        public static string ReplaceNthOccurance(string obj, string find, string replace, int nthOccurance)
        {
            if (nthOccurance > 0)
            {
                MatchCollection matchCollection = Regex.Matches(obj, Regex.Escape(find));
                if (matchCollection.Count >= nthOccurance)
                {
                    Match match = matchCollection[nthOccurance - 1];
                    return obj.Remove(match.Index, match.Length).Insert(match.Index, replace);
                }
            }
            return obj;
        }

        public static void T(params TextMeshProUGUI[] labels)
        {
            foreach (var label in labels)
            {
                if (label == null)
                    Debug.Log("I18n.T null TextMeshProUGUI label");
                else
                    label.text = T(label.text);
            }
        }

        public void SetupI18n(string spreadSheetId, string tabId)
        {
            if (Setup.IsSetupping() || Setup.IsSetupped())
                return;

            Setup.Setupping();

            if (SimpleGameManager.InjectedReferrableInstance.DownloadData())
            {
                Action<string> commCallback = (csv) =>
                {
                    LoadCSVText(csv);
                };
                StartCoroutine(TT.DownloadCSVCoroutine(spreadSheetId, commCallback, true, "I18n", tabId));
            }
            else
            {
                var data = Resources.Load("I18n") as TextAsset;
                LoadCSVText(data.text);
            }
        }

        static void LoadCSVText(string text)
        {
            text = TT.CleanReturnInCsvTexts(text);

            LangKeyValue = new Dictionary<string, Dictionary<string, string>>();
            var lines = Regex.Split(text, TT.LINE_SPLIT_RE);

            var header = Regex.Split(lines[0], TT.SPLIT_RE);
            int columns = header.Length;
            bool keyJumped = false;
            foreach (var col in header)
            {
                if (!keyJumped)
                {
                    keyJumped = true;
                    continue;
                }

                var list = new Dictionary<string, string>();
                LangKeyValue[col] = list;
            }

            foreach (var line in lines)
            {
                var aLine = Regex.Split(line, TT.SPLIT_RE);
                string key = aLine[0];
                if (!TT.IsNullOrWhitespace(key))
                {
                    for (var j = 1; j < header.Length && j < aLine.Length; j++)
                    {
                        var lang = header[j];
                        Dictionary<string, string> dictForLang = LangKeyValue[lang];
                        var value = aLine[j];
                        value = TT.DecodeSpecialCharsFromCSV(value);
                        if (dictForLang.ContainsKey(key))
                            throw new Exception($"Duplicated key {key}");
                        dictForLang[key] = value;
                    }
                }
            }
            I.Setup.SetupCompleted();
        }
    }
}