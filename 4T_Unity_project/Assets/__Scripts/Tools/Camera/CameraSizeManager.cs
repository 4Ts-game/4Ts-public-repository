// Author: Pietro Polsinelli - http://designAGame.eu
// Twitter https://twitter.com/ppolsinelli
// License: WTFPL, all free as in free beer :-)
// Tested with Unity 5.6.1
// Created: 22 06 2017

using UnityEngine;

namespace OL
{
    /// <summary>
    ///     Fits the camera size in order to show about the same portion of the world
    ///     independently of aspect ratio.
    /// </summary>
    public class CameraSizeManager : MonoBehaviour
    {
        [Header("Camera size configuration")]
        public float ReferenceWidth;
        public float ReferenceHeight;

        public float UpdateInterval;
        
        public float ZoomFactor;

        public bool AdaptToAspect;

        Camera main;
        float baseSizeValue;
        float lastWritten;
        float referenceScale;

        void Start()
        {
            main = Camera.main;
            baseSizeValue = main.orthographicSize;

            if (ReferenceWidth > ReferenceHeight)
            {
                referenceScale = ReferenceHeight / ReferenceWidth;
            }
            else
            {
                referenceScale = ReferenceWidth / ReferenceHeight;
            }
            SetCameraSize();
        }

        void SetCameraSize()
        {
            if (AdaptToAspect)
            {
                float currentScale;
                if (main.pixelWidth > main.pixelHeight)
                {
                    currentScale = (float)main.pixelHeight / (float)main.pixelWidth;
                }
                else
                {
                    currentScale = (float)main.pixelWidth / (float)main.pixelHeight;
                }
                
                main.orthographicSize = (baseSizeValue *(1-Mathf.Abs(referenceScale- currentScale))) * ZoomFactor;
                Debug.Log("currentScale " + currentScale);
                Debug.Log("referenceScale " + referenceScale);
            }
            else
                main.orthographicSize = baseSizeValue * ZoomFactor;
        }

        void Update()
        {
            if (Time.time - lastWritten > UpdateInterval)
            {
                lastWritten = Time.time;
                SetCameraSize();
            }
        }
    }
}