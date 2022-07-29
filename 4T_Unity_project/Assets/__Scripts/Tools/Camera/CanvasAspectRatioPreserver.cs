using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

namespace OL
{
    [RequireComponent(typeof(CanvasScaler))]
    public class CanvasAspectRatioPreserver : MonoBehaviour
    {
        [Header("CanvasAspectRatioPreserver update interval")]
        public float UpdateInterval;

        CanvasScaler scaler;
        Camera main;
        float lastWritten;

        void Start()
        {
            main = Camera.main;
            scaler = GetComponent<CanvasScaler>();
            Preserve();
        }

        void Update()
        {
            if (!SimpleGameManager.InjectedReferrableInstance.Production && UpdateInterval>0 && Time.time - lastWritten > UpdateInterval)
            {
                Preserve();
            }
        }

        void Preserve()
        {
            lastWritten = Time.time;
            var referenceRatio = scaler.referenceResolution.x / scaler.referenceResolution.y;
           
            if (main.aspect > referenceRatio)
            {
                scaler.matchWidthOrHeight = 1;
            }
            else
            {
                scaler.matchWidthOrHeight = 0;
            }
        }
    }
}
