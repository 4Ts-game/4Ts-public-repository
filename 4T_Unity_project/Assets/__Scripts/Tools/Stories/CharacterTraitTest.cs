// Author: Pietro Polsinelli - http://designAGame.eu
// Twitter https://twitter.com/ppolsinelli
// License: WTFPL, all free as in free beer :-)
// Tested with Unity 5.6.1
// Created: 19 05 2017

using System.Collections.Generic;
using UnityEngine;

namespace OL
{
    public class CharacterTraitTest : MonoBehaviour
    {
        void Start()
        {
            //Test without decay

            var sampleCharacterTrait =
                DefaultCharacterTrait();
            sampleCharacterTrait.PositiveEpisode();
            sampleCharacterTrait.PositiveEpisode();
            sampleCharacterTrait.NegativeEpisode();
            Debug.Log("1 " + sampleCharacterTrait.State());

            sampleCharacterTrait.PositiveEpisode();
            sampleCharacterTrait.PositiveEpisode();
            Debug.Log("2 " + sampleCharacterTrait.State());

            sampleCharacterTrait.NegativeEpisode();
            sampleCharacterTrait.NegativeEpisode();
            sampleCharacterTrait.NegativeEpisode();
            sampleCharacterTrait.NegativeEpisode();
            sampleCharacterTrait.NegativeEpisode();
            sampleCharacterTrait.NegativeEpisode();
            Debug.Log("3 " + sampleCharacterTrait.State());


            //Same test with decay

            var sampleCharTraitWithDecay =
                DefaultCharacterTrait(0.8f);
            sampleCharTraitWithDecay.PositiveEpisode();
            sampleCharTraitWithDecay.PositiveEpisode();
            sampleCharTraitWithDecay.NegativeEpisode();
            Debug.Log("1 " + sampleCharTraitWithDecay.State());

            sampleCharTraitWithDecay.PositiveEpisode();
            sampleCharTraitWithDecay.PositiveEpisode();
            Debug.Log("2 " + sampleCharTraitWithDecay.State());

            sampleCharTraitWithDecay.NegativeEpisode();
            sampleCharTraitWithDecay.NegativeEpisode();
            sampleCharTraitWithDecay.NegativeEpisode();
            sampleCharTraitWithDecay.NegativeEpisode();
            sampleCharTraitWithDecay.NegativeEpisode();
            sampleCharTraitWithDecay.NegativeEpisode();
            Debug.Log("3 " + sampleCharTraitWithDecay.State());
        }

        public static CharacterTrait<string> DefaultCharacterTrait(float oldEpisodesDecay = 1)
        {
            var sample = new Dictionary<float, string>();
            sample[.8f] = "BEST_FRIENDS";
            sample[.5f] = "FRIENDLY";
            sample[.2f] = "SO_SO";
            sample[.0f] = "BAD";

            return new CharacterTrait<string>(sample, oldEpisodesDecay);
        }
    }
}