// Author: Pietro Polsinelli - http://designAGame.eu
// Twitter https://twitter.com/ppolsinelli
// License: WTFPL, all free as in free beer :-)
// Tested with Unity 5.6.1
// Created: 22 06 2017

using UnityEngine;

namespace OL
{
    public class CircularFill : MonoBehaviour
    {
        float revealOffset;

        void Start()
        {
            revealOffset = 1;
        }

        void Update()
        {
            revealOffset -= .005f;

            if (revealOffset < 0.01)
                revealOffset = 1;

            gameObject.GetComponent<Renderer>().material.SetFloat("_Cutoff", revealOffset);
            Debug.Log("revealOffset " + revealOffset);
        }
    }
}