using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

namespace OL
{
    public class CanvasMatchWidthHeightFix : MonoBehaviour
    {
        void Start()
        {
            CanvasScaler cs = GetComponent<CanvasScaler>();

            float ratio = ((float)Screen.width) / Screen.height;

            if (ratio > 0.5)
            {
                cs.matchWidthOrHeight = 1;
            }
            else
            {
                cs.matchWidthOrHeight = 0;
            }
        }
    }
}
