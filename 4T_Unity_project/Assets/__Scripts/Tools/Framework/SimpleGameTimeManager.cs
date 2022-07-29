// Author: Pietro Polsinelli - http://designAGame.eu
// Twitter https://twitter.com/ppolsinelli
// License: WTFPL, all free as in free beer :-)
// Tested with Unity 5.6.1 - 2018.2.5
// Created: 06 02 2017

using System;
using UnityEngine;

namespace OL
{
    public abstract class SimpleGameTimeManager : MonoBehaviour
    {
        public string GameTimeManagerDebug;

        GamePlayTimeState GameStateForTime;
        bool didSetup;

        [Header("Other - computed")]
        public static float GameElapsedTime;

        protected static SimpleGameTimeManager InjectedReferrableInstance;

        enum GamePlayTimeState
        {
            RUNNING,
            PAUSED
        }

        public void Update()
        {
            if (!didSetup)
                return;

            if (GameStateForTime == GamePlayTimeState.RUNNING)
                GameElapsedTime += Time.deltaTime;

            GameTimeManagerDebug = GameElapsedTime.ToString();

            OnUpdate();
        }

        public abstract void OnUpdate();

        public void StartCounting(float startFrom)
        {
            didSetup = true;
            CheckSetup();

            GameElapsedTime = startFrom;
            GameStateForTime = GamePlayTimeState.RUNNING;
        }

        public void PauseCounting()
        {
            CheckSetup();

            GameStateForTime = GamePlayTimeState.PAUSED;
        }

        public void ResumeCounting()
        {
            CheckSetup();

            GameStateForTime = GamePlayTimeState.RUNNING;
        }

        void CheckSetup()
        {
           if (!didSetup)
                throw new Exception("SimpleGameTimeManager must call StartCounting before usage");
        }
    }
}