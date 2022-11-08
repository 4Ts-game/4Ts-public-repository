using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using DG.Debugging;
using DG.Tweening;
using Newtonsoft.Json;
using SFB;
using UnityEngine;

namespace FourT
{

    public class Persistence
    {
        public List<GameData> Games = new List<GameData>();
        public GameData CurrentPersistedGame = new GameData();

        public void LoadPersistedData()
        {
            Board game = FourTManager.I().Game;
            string data = PlayerPrefs.GetString("Games");

            //Delogger.Log("Persistence", data);

            if (!string.IsNullOrEmpty(data))
            {
                FourTManager.I().Persistence = JsonConvert.DeserializeObject<Persistence>(data);
                CurrentPersistedGame = GetGameDataByName(game.Name);
            }
            else
            {
                CurrentPersistedGame = null;
            }
        }

        public string SaveGame()
        {

            LoadPersistedData();
            Board game = FourTManager.I().Game;
            CurrentPersistedGame = GetGameDataByName(game.Name);

            if (CurrentPersistedGame == null)
            {
                CurrentPersistedGame = new GameData();
                CurrentPersistedGame.Name = game.Name;
                CurrentPersistedGame.Data = game;
                FourTManager.I().Persistence.Games.Add(CurrentPersistedGame);
            }
            else
            {
                CurrentPersistedGame.Data = game;
            }

            string persisted = JsonConvert.SerializeObject(FourTManager.I().Persistence);

            Debug.Log("persisted:: " + persisted);

            if (FourTManager.I().Game.HasConnection)
                PlayerPrefs.SetString("Games", persisted);

            return persisted;
        }

        public void LoadGame(string name)
        {

            CurrentPersistedGame = GetGameDataByName(name);
            FourTManager.I().Game = CurrentPersistedGame.Data;

            if (FourTManager.I().Game.Level == 0)
                FourTManager.I().Game.Level = 3;

            if (FourTManager.I().Game.GameType == 2)
                FourTManager.I().Game.GameType = 1; 

        }

        public void DeleteGame(string name)
        {
            foreach (GameData g in FourTManager.I().Persistence.Games)
            {
                if (g.Name == name)
                {
                    FourTManager.I().Persistence.Games.Remove(g);
                    string persisted = JsonConvert.SerializeObject(FourTManager.I().Persistence);
                    PlayerPrefs.SetString("Games", persisted);
                    return;
                }
            }

        }

        public string GetNameFromPath(string path)
        {
            string[] splitPath = path.Split(Path.DirectorySeparatorChar);
            string gameName = splitPath[splitPath.Length - 1].Replace(".i4t", "");

            return gameName;

        }

        public void SaveGameToFile(string name)
        {

            foreach (GameData g in FourTManager.I().Persistence.Games)
            {
                if (g.Name == name)
                {
                    StandaloneFileBrowser.SaveFilePanelAsync("Save File", "", name, "i4t", (string path) => {

                        if (string.IsNullOrEmpty(path))
                            return;

                        string gameName = GetNameFromPath(path);

                        GameData gd = new GameData();
                        gd.Name = gameName;
                        gd.Data = g.Data;
                        gd.Data.Name = gameName;


                        string persisted = JsonConvert.SerializeObject(gd);

                        StreamWriter writer = new StreamWriter(path, false);
                        writer.WriteLine(persisted);
                        writer.Close();
                    });

                    return;
                }
            }

        }

        public void LoadGameFromFile(Action callback = null)
        {
            StandaloneFileBrowser.OpenFilePanelAsync("Open File", "", "i4t", false, (string[] paths) => {

                try
                {
                    if (string.IsNullOrEmpty(paths[0]))
                    return;

                    StreamReader reader = new StreamReader(paths[0]);
                    string game = reader.ReadToEnd();
                    reader.Close();

                    GameData gameData = JsonConvert.DeserializeObject<GameData>(game);

                    bool gameWithSameNameExist = GetGameDataByName(gameData.Name) != null;

                    if (gameWithSameNameExist)
                    {
                        AlertManager.I.ShowAlert($"A game with the name '{gameData.Name}' already exists.<br>Do you want to replace it or add it as new?", () =>
                        {
                            DeleteGame(gameData.Name);
                            AddGameFromFile(gameData);

                        }, () =>
                        {
                            string name = RenameGameData(gameData.Name);
                            gameData.Name = name;
                            gameData.Data.Name = name;
                            AddGameFromFile(gameData);

                        }, true, "Add as new", "Replace it");
                    }
                    else
                    {
                        AddGameFromFile(gameData);
                    }
                }
                catch (SystemException) {
                };

            });

        }

        public void AddGameFromFile(GameData d)
        {
            Delogger.Log("AddGameFromFile", JsonConvert.SerializeObject(d));

            FourTManager.I().Persistence.Games.Add(d);
            string persisted = JsonConvert.SerializeObject(FourTManager.I().Persistence);

            PlayerPrefs.SetString("Games", persisted);
            LoadGamesManager.I.DrawGameList();
        }

        public string RenameGameData(string name)
        {
            int nameNumber = 1;
            string newName = name +"_"+ nameNumber;

            while (GetGameDataByName(newName) != null)
            {
                newName = name + "_" + nameNumber++;
                Delogger.Log(nameNumber, newName);
            }

            return newName;
        }

        public GameData GetGameDataByName(string name)
        {
            GameData gameData = null;

            if (FourTManager.I().Persistence != null)
            {
                foreach (GameData g in FourTManager.I().Persistence.Games)
                {
                    if (g.Name == name)
                        gameData = g;
                }
            }
            return gameData;
        }

    }


    public class GameData
    {
        public string Name;
        public DateTime Date = DateTime.Now;
        public Board Data = new Board();

    }

}
