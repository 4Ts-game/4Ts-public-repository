using System;
using System.Collections;
using System.Collections.Generic;
using DG.Debugging;
using DG.DeInspektor.Attributes;
using DG.Tweening;
using OL;
using UnityEngine;
using UnityEngine.SceneManagement;
using static FourT.LoadGamesManager;

namespace FourT
{
    public class FourTManager : SimpleGameManager
    {
        [DeEmptyAlert]
        public FourTConfiguration Config;

        [DeEmptyAlert]
        public string ServerURL;

        //other

        public Setuppable CardsSetup;
        public Board Game;
        public GameData GameData;
        public KbOut LatestKbOut;

        public Persistence Persistence;

        float lastTimeUpdatedTimeSpeeder;
        bool beingSpeeduppedByKeyboard;

        public GoogleSheetElement Sheet;

        public override void SetConcreteInstance()
        {
            InjectedReferrableInstance = this;

            CardsSetup = Setuppable.NewInstance();

            //very first start
            ReferenceMasterAudioVolume = 1;
        }

        public static FourTManager I()
        {
            return (FourTManager)InjectedReferrableInstance;
        }

        public override void OnStart()
        {
            Debug.Log("FourTManager Start");

            Persistence = new Persistence();
            Sheet = new GoogleSheetElement();

        }

        public void Update()
        {
            if (!Production && Time.time - lastTimeUpdatedTimeSpeeder > 1)
            {
                lastTimeUpdatedTimeSpeeder = Time.time;
                if (Input.GetKey("f"))
                {
                    beingSpeeduppedByKeyboard = true;
                    Time.timeScale = 4f;
                    DOTween.timeScale = 4f;
                    //Delogger.Log("Time.timeScale", Time.timeScale);
                }
                else if (Input.GetKey("g"))
                {
                    beingSpeeduppedByKeyboard = true;
                    Time.timeScale = .25f;
                    DOTween.timeScale = .25f;
                    //Delogger.Log("Time.timeScale", Time.timeScale);
                }
                else if (beingSpeeduppedByKeyboard)
                {
                    Time.timeScale = 1;
                    DOTween.timeScale = 1;
                    beingSpeeduppedByKeyboard = false;
                }
            }

            if (SceneManager.GetActiveScene().name != "Intro")
            {
                if (CardsSetup.IsToBeSetupped())
                {
                    CardsSetup.Setupping();
                    CardsDoSetup();

                    return;
                }

                if (!CardsSetup.IsSetupped())
                    return;

            }

            if (!FrameworkAndPlayState.IsSetupped())
            {
                Game = new Board();
                Game.Setup();
                FrameworkAndPlayState.SetupCompleted();
            }

            if (FrameworkAndPlayState.IsSetupped() &&
                !FrameworkAndPlayState.IsPaused() &&
                !FrameworkAndPlayState.IsRunning())
            {
                SetPlayStateToRunAndFirstStartTime();

                //load Stored Games
                Persistence.LoadPersistedData();
            }
        }

        public void ErrorMessagesDoSetup(Action callback = null, string sheetId = null) {

            Sheet = LoadGamesManager.I.GetSheetByLabel("ENGLISH");
            if (sheetId != null)
                Sheet = LoadGamesManager.I.GetSheetByID(sheetId);

            //if (I().Game.SheetErrorTabId == null)
            //    return;

            StartCoroutine(TT.DownloadCSVCoroutine(Sheet.Id,
                (csv) =>
                {

                    if (I().Game.SheetErrorTabId == null || I().Game.SheetErrorTabId == "0")
                        return;

                    //todo: parse Error messages

                    Card.FillUpErrorMessages(csv);

                    if (callback != null)
                        callback();

                },
                true, "Data/ErrorMessages", I().Game.SheetErrorTabId, () => {
                    AlertManager.I.ShowAlert($"Unable to load the error description file.<size=100%><br>The Data file is wrong or no connection is available.</size>", () => {
                        Application.Quit();

                    }, null, false, "", "QUIT");

                }));
        }

        public void CardsDoSetup(Action callback = null, string sheetId = null)
        {
            if (DownloadData())
            {
                Sheet = LoadGamesManager.I.GetSheetByLabel("ENGLISH");

                if(sheetId != null)
                 Sheet = LoadGamesManager.I.GetSheetByID(sheetId);


                StartCoroutine(TT.DownloadCSVCoroutine(Sheet.Id,
                    (csv) =>
                        {
                        Card.FillUpCards(csv);
                        CardsSetup.SetupCompleted();

                        if (callback!= null)
                            callback();

                    },
                    true, "Data/Cards", "0", ()=> {
                        //<br><b>{sheetId}</b><br>
                        AlertManager.I.ShowAlert($"Unable to load cards.<size=100%><br>The Data file is wrong or no connection is available.</size>", ()=> {
                            Application.Quit();

                        }, null, false, "", "QUIT");

                    }));

                ErrorMessagesDoSetup(null, Sheet.Id);
            }
            else
            {
                var data = Resources.Load("Data/Cards") as TextAsset;
                Card.FillUpCards(data.text);
                CardsSetup.SetupCompleted();

                if (callback != null)
                    callback();

            }
        }

        public override void SetPlayStateToRunAndFirstStartTime()
        {
            //_4TManager.I().StartCounting(0);
            FrameworkAndPlayState.Run();
        }

    }
}

