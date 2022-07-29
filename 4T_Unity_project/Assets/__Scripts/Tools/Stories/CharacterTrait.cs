// Author: Pietro Polsinelli - https://www.open-lab.com/games/
// Twitter https://twitter.com/ppolsinelli
// License: WTFPL, all free as in free beer :-)
// Tested with Unity 5.6.1 to 2018
// Created: 17 05 2017

using System;
using System.Collections.Generic;

namespace OL
{
    /// <summary>
    ///     The rationale behind this class is presented here:
    ///     https://designagame.eu/2017/05/on-jon-ingolds-how-unhappy-is-unhappy
    /// </summary>
    public class CharacterTrait<T>
    {
        public float OldEpisodesDecay = 1;

        public List<Boundary<T>> Boundaries;

        public List<CharacterEpisode> AllEpisodes = new List<CharacterEpisode>();

        /// <param name="stateBoundariesOnPositiveEpisodes">
        ///     A list of % of happyness and resulting character trait state label
        /// </param>
        /// <param name="oldEpisodesDecay">
        ///     The % of decay to be applied to all older episodes once a new one happens.
        ///     Should be something like 0.97
        /// </param>
        public CharacterTrait(Dictionary<float, T> stateBoundariesOnPositiveEpisodes,
            float oldEpisodesDecay = 1)
        {
            Boundaries = new List<Boundary<T>>();
            foreach (var pair in stateBoundariesOnPositiveEpisodes)
            {
                if (!pair.Key.Betweenf(0, 1))
                {
                    throw new Exception("Must use % as boundaries");
                }
                Boundaries.Add(new Boundary<T> { BoundaryValue = pair.Key, Label = pair.Value });
            }
            Boundaries.Sort((a, b) => b.BoundaryValue.CompareTo(a.BoundaryValue));

            this.OldEpisodesDecay = oldEpisodesDecay;
        }

        public CharacterTrait<T> PositiveEpisode(float impact = 1, string log = null)
        {
            AddLog(true, log, impact);
            return this;
        }

        public CharacterTrait<T> NegativeEpisode(float impact = 1, string log = null)
        {
            AddLog(false, log, impact);
            return this;
        }

        void AddLog(bool positive, string log, float impact)
        {
            foreach (var episode in new List<CharacterEpisode>(AllEpisodes))
                episode.DecayedImpact *= OldEpisodesDecay;
            AllEpisodes.Add(new CharacterEpisode
            {
                Positive = positive,
                Log = log,
                Impact = impact,
                DecayedImpact = impact
            });
        }

        public float HowMuchPositivityInPerc()
        {
            float positiveEpisodesTotal = 0;
            float negativeEpisodesTotal = 0;
            foreach (var episode in AllEpisodes)
                if (episode.Positive)
                {
                    positiveEpisodesTotal += episode.DecayedImpact;
                }
                else
                {
                    negativeEpisodesTotal += episode.DecayedImpact;
                }
            return positiveEpisodesTotal / (positiveEpisodesTotal + negativeEpisodesTotal);
        }

        public float HowMuchNegativityInPerc()
        {
            float positiveEpisodesTotal = 0;
            float negativeEpisodesTotal = 0;
            foreach (var episode in AllEpisodes)
            {
                if (episode.Positive)
                {
                    positiveEpisodesTotal += episode.DecayedImpact;
                }
                else
                {
                    negativeEpisodesTotal += episode.DecayedImpact;
                }
            }
            return negativeEpisodesTotal / (positiveEpisodesTotal + negativeEpisodesTotal);
        }

        //sample code to save the trait
        /*public void Save(string id)
        {
            ES2.Save(OldEpisodesDecay, id + "_oldEpisodesDecay");

            List<string> boundSer = new List<string>();
            foreach (var boundary in Boundaries)
            {
                boundSer.Add(boundary.Label + "___" + boundary.BoundaryValue);
            }
            ES2.Save(boundSer, id + "_boundary");

            List<string> allEpisodesSer = new List<string>();
            foreach (var episode in AllEpisodes)
            {
                allEpisodesSer.Add(
                    episode.DecayedImpact + "___" +
                    episode.Impact + "___" +
                    episode.Log + "___" +
                    episode.Positive + "___"
                    );
            }
            ES2.Save(allEpisodesSer, id + "_allEpisodes");
        }*/
        
        public T State()
        {
            var pPerc = HowMuchPositivityInPerc();
            foreach (var boundary in Boundaries)
            {
                if (pPerc >= boundary.BoundaryValue)
                    return boundary.Label;
            }
            return default(T);
        }

        public struct Boundary<V>
        {
            public float BoundaryValue;
            public V Label;
        }

        public class CharacterEpisode
        {
            public bool Positive;
            public string Log;
            public float Impact; // original value
            public float DecayedImpact;
        }
    }
}