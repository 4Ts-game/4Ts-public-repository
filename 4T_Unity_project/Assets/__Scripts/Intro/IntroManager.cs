using System.Collections;
using System.Collections.Generic;
using DG.DeInspektor.Attributes;
using DG.Tweening;
using OL;
using TMPro;
using UnityEngine;
using UnityEngine.SceneManagement;

namespace FourT
{

    public class IntroManager : MonoBehaviour
    {
        Setuppable BoardSetup;

        [DeHeader("Intro", "FFFFFFFF", "9F9F9FFF")]

        [SerializeField]
        [DeEmptyAlert]
        [DeToggleButton]
        RectTransform IntroScreen;

        public TMP_Text Version;

        public void Start()
        {

            //Adjust window size
            Screen.fullScreen = false;

            int margin = (Screen.currentResolution.width * 7) / 100;
            int w = Screen.currentResolution.width - margin;
            int h = (w * 9) / 16;

            Screen.SetResolution(w, h, false);
            Debug.Log(Screen.currentResolution.width + "x" + Screen.currentResolution.height);

            string version = $"App version {Application.version}<br>Build {SimpleGameManager.Build}<br>Knowledge Base version {FourTManager.I().Config.KnowledgeBaseVersion}";
            Version.text = version;


            //if (!ServerJob.CheckForInternetConnection())
            //{
            //    FourTManager.I().Game.HasConnection = false;

            //    AlertManager.I.ShowAlert("<br>No connection available. The game will be closed. ", () => {
            //        //SceneManager.LoadScene("Intro");
            //    }, null, false);
            //    return;
            //}

        }

        public void Awake()
        {
            BoardSetup = Setuppable.NewInstance();
        }

        public void Update()
        {

            if (FourTManager.NotReadyYet())
                return;


            if (BoardSetup.NotSetuppedNorSetupping())
            {
                BoardSetup.Setupping();

                CanvasGroup Intro = IntroScreen.GetComponent<CanvasGroup>();
                Intro.alpha = 0;
                IntroScreen.gameObject.SetActive(true);
                Intro.DOFade(1f, 4f).OnComplete(()=> {
                    SceneManager.LoadScene("LoadGames");
                });

            }
        }
    }
}