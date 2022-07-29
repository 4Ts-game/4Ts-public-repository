using System;
using System.Collections;
using System.Collections.Generic;
using System.Net;
using DG.Debugging;
using UnityEngine;
using UnityEngine.Networking;
using UnityEngine.SceneManagement;

namespace FourT
{
    public class ServerJob : MonoBehaviour
    {
        public static ServerJob I;
        public enum COMMAND
        {
            CONSISTENCY,
            SUGGESTIONS,
            COMPLETENESS
        }

        void Awake()
        {
            I = this;

        }

        public void ConsistencyCheck(Board game, bool checkCompleteness, Action callback)
        {

            if (FourTManager.I().Game.Level == 3)
            {
                //callback();
                return;
            }


            if (!CheckForInternetConnection())
            {
                FourTManager.I().Game.HasConnection = false;

                AlertManager.I.ShowAlert("<br>The game requires an internet connection. Please connect to a network and restart the game.", () => {
                    SceneManager.LoadScene("Intro");
                }, null, false);
                return;
            }


            KeyValuePair<string, string> additionalParam = new KeyValuePair<string, string>();

            if (checkCompleteness)
            {
                additionalParam = new KeyValuePair<string, string>("checkCompleteness", "true");
            }

            StartCoroutine(SemanticCheckUnity(game, additionalParam, callback));
        }

        static IEnumerator SemanticCheckUnity(Board game, KeyValuePair<string, string> additionalParam, Action callback)
        {

            var FM = FourTManager.I();

            WWWForm form = new WWWForm();

            //http://g4t.itd.cnr.it:8000/semantic_check_Unity?context=...&objectives=...&content=...&
            //C1-CSW-TECHNIQUE=22&C2-CSW-TECHNIQUE=37&C3-CSW-TECHNIQUE=&C4-CSW-TECHNIQUE=&C1-ABA-CSS-TASK=32&
            //C1-ABA-CSS-TEAM=58&C2-ABA-CSS-TASK=&C2-ABA-CSS-TEAM=59&C3-ABA-CSS-TASK=&C3-ABA-CSS-TEAM=&C4-ABA-CSS-TASK=&
            //C4-ABA-CSS-TEAM=&C1-ABA-CSS-TEC1=74&C1-ABA-CSS-TEC2=75&C2-ABA-CSS-TEC1=&C2-ABA-CSS-TEC2=&C3-ABA-CSS-TEC1=&
            //C3-ABA-CSS-TEC2=&C4-ABA-CSS-TEC1=&C4-ABA-CSS-TEC2=&C1-ABB-CSS-TASK=114&C1-ABB-CSS-TEAM=&C2-ABB-CSS-TASK=&
            //C2-ABB-CSS-TEAM=&C3-ABB-CSS-TASK=&C3-ABB-CSS-TEAM=&C4-ABB-CSS-TASK=&C4-ABB-CSS-TEAM=&C1-ABB-CSS-TEC1=&
            //C1-ABB-CSS-TEC2=&C2-ABB-CSS-TEC1=&C2-ABB-CSS-TEC2=&C3-ABB-CSS-TEC1=&C3-ABB-CSS-TEC2=&C4-ABB-CSS-TEC1=&
            //C4-ABB-CSS-TEC2=&semantic_check=Semantic+check

            string paramsurl = "";

            foreach (var pair in game.LocationAndCard)
            {
                form.AddField($"{pair.Key}", pair.Value);
                paramsurl += $"{pair.Key}={pair.Value}&";

            }

            if (additionalParam.Key != null)
            {
                form.AddField(additionalParam.Key, additionalParam.Value);
                paramsurl += $"{additionalParam.Key}={additionalParam.Value}&";
            }

            form.AddField($"semantic_check", "Semantic check");

            string RequestUrl = $"{FM.ServerURL}semantic_check_Unity?" + paramsurl + "semantic_check=Semantic check";

            //level=advanced
            if (FourTManager.I().Game.Level == 2)
            {
                RequestUrl += "&level=advanced";
            }

            Debug.Log("semantic_check_Unity:: " + RequestUrl);
            UnityWebRequest uwr = UnityWebRequest.Get(RequestUrl);

            yield return uwr.SendWebRequest();

            if (!string.IsNullOrEmpty(uwr.error))
            {
                Debug.Log("Error downloading: " + uwr.error);

            }
            else
            {
                var downloadHandlerText = uwr.downloadHandler.text;
                Delogger.Log("Board ConsistencyCheck answer", downloadHandlerText);
                FourTManager.I().Game.HasConnection = true;

                FM.LatestKbOut = KbOut.Load(downloadHandlerText);

                if (callback != null)
                    callback();

                if(BoardManager.I != null)
                    BoardManager.I.CheckCompleteness.interactable = FM.LatestKbOut.InconsistentPositions.Count == 0 && FM.Game.LocationAndCard.Count > 0;
            }
        }

        public void SuggestionCheckCall(Board game, string location, Action callback)
        {
            StartCoroutine(SemanticCheckUnity(game, new KeyValuePair<string, string>(location, "1000"), callback));
        }

        public void CompletenessCheckCall()
        {
            I.ConsistencyCheck(FourTManager.I().Game,true, () =>
            {
               BoardManager.I.DisplayErrors(null,FourTManager.I().LatestKbOut.MissingCards);
            });
        }


        public static bool CheckForInternetConnection()
        {
            Debug.Log(Application.internetReachability);
            try
            {
                using (var client = new WebClient())
                using (client.OpenRead("https://www.cnr.it"))
                    return true;
            }
            catch(Exception e)
            {
                Debug.Log(e.Message);
                return false;
            }
        }
    }
}
