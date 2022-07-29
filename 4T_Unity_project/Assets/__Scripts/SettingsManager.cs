using System.Collections;
using System.Collections.Generic;
using DG.Debugging;
using Newtonsoft.Json;
using TMPro;
using UnityEngine;
using UnityEngine.SceneManagement;
using UnityEngine.UI;

namespace FourT
{
    public class SettingsManager : MonoBehaviour
    {
        public static SettingsManager I;

        public Transform SettingsPanel;
        public Transform SettingsCanvas;

        public Transform HybridSettings;
        public Transform NoWebcamAvailable;
        public Transform HybridNotAvailable;
        public Transform CamSettings;

        public Transform QuitPanel;
        public Transform ResetBoard;


        public Transform CamWindow;
        public Transform DebugWindow;

        WebCamDevice[] Cam_devices;

        public Transform CamList;
        public GameObject ChooseCamButtonPrefab;

        public WebCamDevice Webcam;

        public bool SettingsIsOpen;

        void Awake()
        {
            I = this;
            if (WebCamTexture.devices.Length > 0)
                Webcam = WebCamTexture.devices[0];
        }

        // Start is called before the first frame update
        void Start()
        {
            NoWebcamAvailable.gameObject.SetActive(false);
            HybridNotAvailable.gameObject.SetActive(false);
            ResetBoard.gameObject.SetActive(true);
            //If no webcam are available turn off Hybrid modality
            if (WebCamTexture.devices.Length == 0 || FourTMarkersManager.I.WebcamUnavailable)
            {
                NoWebcamAvailable.gameObject.SetActive(true);
                HybridSettings.Find("TurnOnHybrid").GetComponent<Button>().interactable = false;

                AlertManager.I.ShowAlert("You need a webcam to play the Hybrid game");
                ResetBoard.gameObject.SetActive(false);
            }

            //If The game has already cards played then turn off hybrid modality
            else if (FourTManager.I().Game.CardsPlayed().Count > 0)
            {

                HybridNotAvailable.gameObject.SetActive(true);
                HybridSettings.Find("TurnOnHybrid").GetComponent<Button>().interactable = false;

            }

        }

        // Update is called once per frame
        void Update()
        {

        }

        public void WebcamList()
        {

            foreach (Transform o in CamList.transform.Find("CameraChooser"))
            {
                Destroy(o.gameObject);
            }

            Cam_devices = WebCamTexture.devices;
            // for debugging purposes, prints available devices to the console
            for (int i = 0; i < Cam_devices.Length; i++)
            {
                Debug.Log($"Webcam available: {Cam_devices[i].name}");

                GameObject camButton = Instantiate(ChooseCamButtonPrefab, CamList.transform.Find("CameraChooser"));
                camButton.name = Cam_devices[i].name;
                camButton.GetComponentInChildren<TMP_Text>().text = Cam_devices[i].name;

                if (Webcam.name == Cam_devices[i].name)
                    camButton.transform.Find("Selected").gameObject.SetActive(true);

                int idx = i;
                camButton.GetComponent<Button>().onClick.RemoveAllListeners();
                camButton.GetComponent<Button>().onClick.AddListener(() =>
                {
                    SetWebcam(Cam_devices[idx]);

                    foreach(Transform t in CamList.transform.Find("CameraChooser"))
                    {
                        t.Find("Selected").gameObject.SetActive(false);

                        if(t.name == Cam_devices[idx].name)
                            t.Find("Selected").gameObject.SetActive(true);
                    }
                });
            }
        }

        public void SetWebcam(WebCamDevice cam)
        {
            Webcam = cam;
            FourTMarkersManager.I.SetWebcam(Webcam.name);
            Debug.Log(Webcam.name);
        }

        public void disableAllWebcams()
        {
            FourTMarkersManager.I.ClearAllWebcams();
        }

        public void TurnOnOffHybrid()
        {
            if (WebCamTexture.devices.Length == 0 || FourTMarkersManager.I.WebcamUnavailable)
            {
                FourTMarkersManager.I.HybridIsActive = false;
                ResetBoard.gameObject.SetActive(false);
                return;
            }

            FourTMarkersManager.I.HybridIsActive = !FourTMarkersManager.I.HybridIsActive;
            disableAllWebcams();

            if (FourTMarkersManager.I.HybridIsActive && Cam_devices.Length > 0)
            {

                FourTMarkersManager.I.ClearBoard();

                HybridSettings.GetComponentInChildren<Button>().GetComponentInChildren<TMP_Text>().text = "Off";
                HybridSettings.GetComponentInChildren<Image>().color = new Color32(134, 195, 18, 255);
                HybridSettings.transform.Find("Label").GetComponent<TMP_Text>().text = "Hybrid Mode is ON";
                CamSettings.gameObject.SetActive(true);
                CamList.gameObject.SetActive(true);
                ResetBoard.gameObject.SetActive(true);
                if (Webcam.name != null)
                     SetWebcam(Webcam);
            }
            else
            {
                HybridSettings.GetComponentInChildren<Button>().GetComponentInChildren<TMP_Text>().text = "On";
                HybridSettings.GetComponentInChildren<Image>().color = new Color32(100, 100, 100, 255);
                HybridSettings.transform.Find("Label").GetComponent<TMP_Text>().text = "Hybrid Mode is OFF";
                CamSettings.gameObject.SetActive(false);
                CamList.gameObject.SetActive(false);
                ResetBoard.gameObject.SetActive(false);
                FourTMarkersManager.I.ClearWebcam();
                TempTexturesManager.Clear();

            }

        }

        public void ShowHideCam()
        {
            if (CamWindow.gameObject.activeSelf)
            {
                CamWindow.gameObject.SetActive(false);
                //OpenCloseSettingsPanel();
                SettingsCanvas.gameObject.SetActive(true);
            }
            else
            {
                CamWindow.gameObject.SetActive(true);
                //OpenCloseSettingsPanel(true);
                SettingsCanvas.gameObject.SetActive(false);
            }

        }

        public void ShowHideDebugger()
        {
            if (DebugWindow.gameObject.activeSelf)
            {
                DebugWindow.gameObject.SetActive(false);
            } else
            {
                DebugWindow.gameObject.SetActive(true);
            }

        }

        public void OpenCloseSettingsPanel(bool forceClose = false)
        {
            if (forceClose)
            {
                SettingsIsOpen = false;
                SettingsPanel.gameObject.SetActive(false);
                return;
            }

            if (SettingsPanel.gameObject.activeSelf)
            {
                SettingsIsOpen = false;
                SettingsPanel.gameObject.SetActive(false);
            } else
            {
                SettingsIsOpen = true;

                WebcamList();


                if (FourTManager.I().Game.CardsPlayed().Count > 0)
                {
                    HybridNotAvailable.gameObject.SetActive(true);
                    HybridSettings.Find("TurnOnHybrid").GetComponent<Button>().interactable = false;

                } else
                {
                    HybridNotAvailable.gameObject.SetActive(false);
                    HybridSettings.Find("TurnOnHybrid").GetComponent<Button>().interactable = true;

                }

                if (FourTMarkersManager.I.HybridIsActive)
                {
                    HybridSettings.GetComponentInChildren<Button>().GetComponentInChildren<TMP_Text>().text = "Off";
                    HybridSettings.transform.Find("Label").GetComponent<TMP_Text>().text = "Hybrid Mode is ON";
                    CamSettings.gameObject.SetActive(true);
                    CamList.gameObject.SetActive(true);
                }
                else
                {

                    HybridSettings.GetComponentInChildren<Button>().GetComponentInChildren<TMP_Text>().text = "On";
                    HybridSettings.transform.Find("Label").GetComponent<TMP_Text>().text = "Hybrid Mode is OFF";
                    CamSettings.gameObject.SetActive(false);
                    CamList.gameObject.SetActive(false);
                }

                SettingsPanel.gameObject.SetActive(true);
            }
        }

        public void Save()
        {
            FourTManager.I().Persistence.SaveGame();
        }


        public void Restart(bool force = false)
        {

            if (force || FourTManager.I().Game.Level == 3)
            {

                if (FourTMarkersManager.I.HybridIsActive)
                    TurnOnOffHybrid();

                FourTManager.I().Persistence.SaveGame();

                SceneManager.LoadScene("Intro");
                return;

            } else
            {
                ServerJob.I.ConsistencyCheck(FourTManager.I().Game, true, () =>
                {
                    Delogger.Log("MissingCards", FourTManager.I().LatestKbOut.MissingCards.Count);

                    if (
                    FourTManager.I().LatestKbOut.MissingCards.Count == 0
                    &&
                    (FourTManager.I().Game.Level != 1 || (FourTManager.I().Game.Level == 1 && BoardManager.I.CheckIfTechniqueExist(FourTManager.I().Game)))
                    )
                    {
                        if (FourTMarkersManager.I.HybridIsActive)
                            TurnOnOffHybrid();

                        SceneManager.LoadScene("Intro");
                    }
                    else
                    {
                        OpenCloseSettingsPanel();
                        BoardManager.I.ShowConfirmQuit(() =>
                        {
                            BoardManager.I.HideConfirmQuit();
                            Restart(true);
                        }, "Are you sure you want to exit this game? There are missing cards.", "Exit");
                    }
                });
            }


        }

        public void Quit()
        {

            if(FourTManager.I().Game.Level == 3)
            {
                QuitPanel.gameObject.SetActive(true);
                return;
            }


            ServerJob.I.ConsistencyCheck(FourTManager.I().Game, true, () =>
            {
                Delogger.Log("MissingCards", FourTManager.I().LatestKbOut.MissingCards.Count);

                if (
                FourTManager.I().LatestKbOut.MissingCards.Count == 0
                &&
                (FourTManager.I().Game.Level != 1 || (FourTManager.I().Game.Level == 1 && BoardManager.I.CheckIfTechniqueExist(FourTManager.I().Game)))
                )
                {
                    QuitPanel.gameObject.SetActive(true);
                }
                else {

                    OpenCloseSettingsPanel();

                    BoardManager.I.ShowConfirmQuit(() =>
                    {
                        BoardManager.I.ConfirmQuit.gameObject.SetActive(false);
                        //QuitPanel.gameObject.SetActive(true);
                        BoardManager.I.Quit();

                        // QuitPanel.gameObject.SetActive(true);
                    }, "Are you sure you want to quit? There are missing cards.", "Quit");
                }
            });
        }

    }


}