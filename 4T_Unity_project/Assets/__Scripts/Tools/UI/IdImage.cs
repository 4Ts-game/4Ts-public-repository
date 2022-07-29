using System;
using System.Collections;
using System.Collections.Generic;
using DG.DeInspektor.Attributes;
using UnityEngine;

namespace OL
{
    [Serializable]
    public class IdImage
    {
        public string Name;

        [DeImagePreview]
        public Sprite Image;
    }
}
