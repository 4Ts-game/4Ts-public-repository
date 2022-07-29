using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using UnityEngine;

namespace OL
{
    public class Memory
    {
        [SerializeField]
        float decay;
        [SerializeField]
        Dictionary<string, float> episodes = new Dictionary<string, float>();

        public Memory()
        {
            //this is only for persistence
        }

        public Memory(float decay)
        {
            this.decay = decay;
        }

        public int Count()
        {
            return episodes.Count;
        }

        public List<string> AllEpisodes()
        {
            return episodes.Keys.ToList();
        }

        public float EpisodeCurrentMemory(object episode)
        {
            return EpisodeCurrentMemory(episode.ToString());
        }

        public float EpisodeCurrentMemory(string episode)
        {
            if (!episodes.Keys.Contains(episode))
                return 0;
            return episodes[episode];
        }

        public void AddEpisode(string episodeLabel)
        {
            Dictionary<string, float> episodesRefreshed = new Dictionary<string, float>();
            foreach (var episodeInMem in episodes.Keys)
            {
                float currentMemory = episodes[episodeInMem] - decay;
                if (currentMemory > 0)
                {
                    episodesRefreshed[episodeInMem] = currentMemory;
                }
            }
            episodesRefreshed[episodeLabel] = 1;

            episodes = episodesRefreshed;
        }

        public string GetMostFresh(List<string> possibleEpisodes, bool addChosenAsEpisode)
        {
            if (possibleEpisodes == null || possibleEpisodes.Count == 0)
                return null;

            float minRequiredToJoin = 1;
            foreach (var possibleEpisode in possibleEpisodes)
            {
                if (!episodes.ContainsKey(possibleEpisode))
                {
                    minRequiredToJoin = 0;
                    break;
                }
                minRequiredToJoin = Mathf.Min(minRequiredToJoin, episodes[possibleEpisode]);
            }

            List<string> candidates = new List<string>();
            foreach (var possibleEpisode in possibleEpisodes)
            {
                if (!episodes.ContainsKey(possibleEpisode))
                {
                    candidates.Add(possibleEpisode);
                }
                else
                {
                    if (episodes[possibleEpisode] <= minRequiredToJoin)
                    {
                        candidates.Add(possibleEpisode);
                    }
                }
            }

            candidates.Shuffle();

            var mostFresh = candidates[0];
            if (addChosenAsEpisode)
            {
                AddEpisode(mostFresh);
            }
            return mostFresh;
        }
    }
}
