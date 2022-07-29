using System.Collections;
using System.Collections.Generic;
using DG.DeInspektor.Attributes;
using TMPro;
using UnityEngine;

namespace OL
{
    public class VirtualGridTile : MonoBehaviour
    {
        public int X;
        public int Y;

        public TextMeshPro Label;
        public SpriteRenderer Square;

        void Start()
        {

        }

        [DeMethodButton("Test")]
        public void Test()
        {
            var point = VirtualGrid.I.FindPointAtPosition(transform.position);
            Label.text = X + " " + Y + " (" + point + ")";
        }
    }
}

