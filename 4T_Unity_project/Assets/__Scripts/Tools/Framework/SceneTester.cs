using System;
using System.Collections;
using System.Collections.Generic;
using DG.DeInspektor.Attributes;
using UnityEngine;

namespace OL
{
    public abstract class SceneTester : MonoBehaviour
    {
        [DeToggleButton("Passed", false, "ff0000", "00ff00")]
        public bool Passed;

        protected Dictionary<string, Action> allTests;

        int totalFailed;
        int totalPassed;

        void Awake()
        {
            Passed = true;
        }

        [DeMethodButton]
        public void RunAllSceneTests()
        {
            allTests = new Dictionary<string, Action>();
            AddTests();

            totalFailed = 0;
            totalPassed = 0;
            Passed = true;

            foreach (var idAction in allTests)
            {
                Debug.Log("\n\nTEST "+ idAction .Key+ ":\n");
                idAction.Value();                
            }

            Debug.Log("\nTotal passed: " + totalPassed);
            Debug.Log("\nTotal failed: " + totalFailed);
        }

        protected void LogTest(bool result, string name)
        {
            Passed = Passed && result;
            if (result)
            {
                totalPassed++;
                name = "<color=#00ff00>PASSED</color> " + name;
            }
            else
            {
                totalFailed++;
                name = "<color=#ff0000>FAILED</color> " + name;
            }
            Debug.Log(name);
        }

        public abstract void AddTests();

    }
}
