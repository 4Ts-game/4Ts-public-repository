using System.Collections;
using System.Collections.Generic;
using DG.DeInspektor.Attributes;
using OL;
using UnityEngine;

namespace OL
{
    public class MemoryTest : MonoBehaviour
    {
        void Start()
        {

        }

        [DeMethodButton("Test Ageing")]
        void Test()
        {
            Memory EpisodesMemory = new Memory(0.01f);

            EpisodesMemory.AddEpisode("old 1");
            EpisodesMemory.AddEpisode("old 2");
            EpisodesMemory.AddEpisode("old 3");
            EpisodesMemory.AddEpisode("old 4");
            EpisodesMemory.AddEpisode("old 2");
            EpisodesMemory.AddEpisode("old 5");

            Debug.Log("2 3 7 -> " +
                EpisodesMemory.GetMostFresh(new List<string>() { "old 2", "old 3", "old 7" },true));
            Debug.Log("2 3 4 -> " +
            EpisodesMemory.GetMostFresh(new List<string>() { "old 2", "old 3", "old 4" }, true));
            Debug.Log("2 4 -> " +
            EpisodesMemory.GetMostFresh(new List<string>() { "old 2", "old 4" }, true));
        }

    }


}
