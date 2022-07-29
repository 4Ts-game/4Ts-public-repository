// Author: Pietro Polsinelli - http://designAGame.eu
// Twitter https://twitter.com/ppolsinelli
// License: WTFPL, all free as in free beer :-)
// // Created: 01 08 2016


using UnityEngine;

namespace OL
{
    public class TheScene : MonoBehaviour
    {
        public GameObject[] activateOnStartup, deactivateOnStartup;

        void Start()
        {
            foreach (var go in activateOnStartup)
                if (go != null)
                {
                    go.SetActive(true);
                    if (go.GetComponent<CanvasGroup>() != null)
                        go.GetComponent<CanvasGroup>().alpha = 1;
                }
            foreach (var go in deactivateOnStartup)
                if (go != null)
                {
                    go.SetActive(false);
                    if (go.GetComponent<CanvasGroup>() != null)
                        go.GetComponent<CanvasGroup>().alpha = 0;
                }            
        }
    }
}