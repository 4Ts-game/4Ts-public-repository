using System.Collections;
using System.Collections.Generic;
using DG.Debugging;
using DG.Tweening;
using TMPro;
using UnityEngine;
using UnityEngine.SceneManagement;
using UnityEngine.UI;
using OL;
using System.Threading;
using UnityEngine.EventSystems;
using System;

namespace FourT
{

    public class LoadGamesManager : MonoBehaviour
    {
        public static LoadGamesManager I;
        public List<GameData> SavedData;


        [Space]
        public TMP_Text Version;

        [Space]
        public GameObject SavedGamesBox;
        public GameObject NewGameBox;

        public Transform NameAlert;

        public Button[] Levels;
        public Button[] GameTypes;

        public Button[] SheetButtons;


        public List<GoogleSheetElement> GoogleSheets;

        [Serializable]
        public class GoogleSheetElement
        {
            public string Label;
            public string Id;
            public string TabId;
        }



        public GoogleSheetElement GetSheetByLabel(string label)
        {
            GoogleSheetElement sheet = null;

            foreach (GoogleSheetElement s in GoogleSheets)
            {
                if (s.Label == label)
                    sheet = s;
            }

            return sheet;
        }

        public GoogleSheetElement GetSheetByID(string id)
        {
            GoogleSheetElement sheet = null;

            foreach (GoogleSheetElement s in GoogleSheets)
            {
                if (s.Id == id)
                    sheet = s;
            }

            return sheet;
        }


        [Space]
        public GameObject SavedGameListBox;
        public GameObject SavedGameListElementPrefab;
        public GameObject Sipario;
        public GameObject Loading;

        [Space]
        public GameObject LoadGameFromFile;

        [Space]
        public TMP_InputField NewGameName;
        public TMP_InputField SheetID;
        public TMP_InputField SheetErrorTabID;

        public int Level = 1;
        public int GameType = 1;

        void Awake()
        {
            I = this;
        }


        // Start is called before the first frame update
        void Start()
        {
            Sipario.GetComponent<Transform>().DOScale(2, 2f);
            Sipario.GetComponent<CanvasGroup>().DOFade(0f, 2f).OnComplete(() =>
            {
                Sipario.gameObject.SetActive(false);
            });

            GoogleSheetElement sheet = GetSheetByLabel("ENGLISH");

            SheetID.text = sheet.Id;
            SheetErrorTabID.text = sheet.TabId;

            DrawGameList(); 

            if (FourTManager.I().Config.ManageFiles)
                LoadGameFromFile.SetActive(true);

            string version = $"App version {Application.version}<br>Build {SimpleGameManager.Build}<br>Knowledge Base version {FourTManager.I().Config.KnowledgeBaseVersion}";
            Version.text = version;

        }


        // Update is called once per frame
        void Update()
        {

        }

        /**
         * 
         * Language     Sheet ID                                        Error Messages Tab ID
         * 
         * ENGLISH      11wEMb8UJZQkI92caK7ts8Gi4tQIyF7uVLKZwiuKZ_74    1304083090
         * ITALIAN      1A46tpqm7rP24SZpSakSmB9c8RGi9jU-66ZGv_GCrYVs    1827283722
         * GREEK        1muXXluXDopua8chs8LjAO1-l5txbw6z3Vb0Lw6HsFVU    1575317255
         * BULGARIAN    1o6psb1HE4F5Vd2Cd_eH-o53kZph1wF5JtRfTnU1wKLg    1919357250
         * 
         * */

        public void SetSheetId(string label)
        {
            //Set the google sheet ID and the tab ID for Cards and Error messages
            GoogleSheetElement sheet = GetSheetByLabel(label);
            SheetID.text = sheet.Id;
            SheetErrorTabID.text = sheet.TabId;

            foreach (Button bs in SheetButtons)
            {
                bs.interactable = true;
            }

            EventSystem.current.currentSelectedGameObject.GetComponent<Button>().interactable = false;

        }

        public void ChangeLevel(int l)
        {
            foreach (Button bl in Levels)
            {
                bl.interactable = true;
            }

            Levels[l - 1].interactable = false;
            Level = l;
        }


        public void ChangeGameType(int l)
        {
            foreach (Button bl in GameTypes)
            {
                bl.interactable = true;
            }

            GameTypes[l - 1].interactable = false; 
            GameType = l;
        }

        public bool NameExist(string name)
        {
            bool nameAlreadyExist = false;

            foreach (GameData gD in FourTManager.I().Persistence.Games)
            {
                if (gD.Name == name)
                    nameAlreadyExist = true;
            }
                return nameAlreadyExist;
        }

        public void DrawGameList()
        {
            foreach (Transform o in SavedGameListBox.transform)
            {
                Destroy(o.gameObject);
            }

            FourTManager.I().Persistence.LoadPersistedData();

            if (FourTManager.I().Persistence.Games.Count > 0)
            {
                SavedGamesBox.SetActive(true);

                foreach (GameData gD in FourTManager.I().Persistence.Games)
                {

                    GameObject instance = Instantiate(SavedGameListElementPrefab, SavedGameListBox.transform);
                    instance.transform.Find("Label").GetComponent<TMP_Text>().text = gD.Name + " (L" + gD.Data.Level + ")";

                    instance.transform.Find("Play").GetComponent<Button>().onClick.AddListener(() => {
                        LoadGame(gD.Name);
                    });

                    instance.transform.Find("Del").GetComponent<Button>().onClick.AddListener(() => {
                        RemoveSavedGame(gD.Name);
                        DrawGameList();
                    });

                    if (FourTManager.I().Config.ManageFiles)
                    {
                        instance.transform.Find("Download").gameObject.SetActive(true);

                        instance.transform.Find("Download").GetComponent<Button>().onClick.AddListener(() =>
                        {
                            DownloadGameData(gD.Name);
                            DrawGameList();
                        });
                    }
                }
            }
            else
            {
                SavedGamesBox.SetActive(false);
            }

        }

        public IEnumerator NameExistAlert()
        {
            Delogger.Log("Name", NewGameName.text);
            NameAlert.gameObject.SetActive(true);
            yield return new WaitForSeconds(4);
            NameAlert.gameObject.SetActive(false);
            Delogger.Log("Name", NewGameName.text);
        }

        public void NewGame()
        {
            if (!string.IsNullOrEmpty(NewGameName.text))
            {
                string name = NewGameName.text;
                if (NameExist(NewGameName.text))
                {
                    StartCoroutine(NameExistAlert());
                    return;
                } 

                Loading.SetActive(true);

                FourTManager.I ().Game = new Board ();
                FourTManager.I ().Game.Setup ();


                FourTManager.I().Game.Name = name;

                FourTManager.I().Game.SheetId = SheetID.text;
                FourTManager.I().Game.SheetErrorTabId = SheetErrorTabID.text;


                FourTManager.I().Game.Level = Level;
                FourTManager.I().Game.GameType = GameType;

                FourTManager.I().CardsDoSetup(()=> {
                    SceneManager.LoadScene("Game");
                }, FourTManager.I ().Game.SheetId);
            }
            else
            {
                AlertManager.I.ShowAlert("A name is needed to start a new game.",()=>{
                    NewGameName.Select();
                    NewGameName.ActivateInputField();
                },null,false);
            }
        }

        public void LoadGame(string name)
        {
            Loading.SetActive(true);

            NameAlert.gameObject.SetActive(false);

            FourTManager.I().Persistence.LoadGame(name);

            FourTManager.I().CardsDoSetup( ()=> {
                SceneManager.LoadScene("Game");
            }, FourTManager.I ().Game.SheetId);
        }

        public void RemoveSavedGame(string name)
        {
            AlertManager.I.ShowAlert($"Do you really want to delete <br>the <b>{name}</b> gameplay?", () => {

                FourTManager.I().Persistence.DeleteGame(name);
                Delogger.Log("Deleting Game", name);
                DrawGameList();
            });
        }

        public void DownloadGameData(string name)
        {
            FourTManager.I().Persistence.SaveGameToFile(name);
        }

        public void LoadGameDataFromFile()
        {
            FourTManager.I().Persistence.LoadGameFromFile(()=> {
                Debug.Log("LoadGameDataFromFile");
                DrawGameList();
            });
        }

        public void Quit ()
        {
            Application.Quit ();
        }

    }
}
