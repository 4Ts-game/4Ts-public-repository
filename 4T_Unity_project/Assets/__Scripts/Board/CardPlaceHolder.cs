using System.Collections;
using System.Collections.Generic;
using DG.Tweening;
using UnityEngine;

namespace FourT
{
    public class CardPlaceHolder : MonoBehaviour
    {
        public int Col;
        public int Row;
        public bool HasBeenDrawed;
        public int CardId;
        public float CardSetAtTime;

        public Transform DigitalCol;

        void Start()
        {
            DOVirtual.DelayedCall(.5f, () =>
            {
                BoardManager.I.CardPlaceHolders.Add(this);
            });
        }

        void Update()
        {
        }

        public static CardPlaceHolder GetPlaceHolderByRowCol(int row, int col)
        {
            CardPlaceHolder placeHolder = null;
            foreach(CardPlaceHolder ph in BoardManager.I.CardPlaceHolders)
            {
                if (row == ph.Row && col == ph.Col)
                    placeHolder = ph;
            }

            return placeHolder;

        }
    }
}