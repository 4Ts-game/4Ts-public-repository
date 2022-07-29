using System.Collections;
using System.Collections.Generic;
using DG.Debugging;
using DG.DeInspektor.Attributes;
using DG.Tweening;
using OL;
using UnityEngine;
using UnityEngine.UI;

namespace FourT
{
    public class UICardRoller : MonoBehaviour
    {
        [DeEmptyAlert]
        public Button Left;
        [DeEmptyAlert]
        public Button Right;

        //other
        float oneCardDistanceInWorldUnit;
        float oneCardWidthInPixels;
        float oneCardPaddingInPixels;
        public float CurrentDistance;

        int focusedCard;
        int lastCard;

        bool dragging;
        public int dragToPos = 0;

        bool rolling;
        float startingMousePosX;
        float resetPositionOnInvalidDrag;

        //tmp for dragging
        float latestMousePosX;

        float xOnDragStart;

        void SetDistances()
        {
            //todo one card handling
            var child0 = transform.GetChild(0);
            oneCardWidthInPixels = ((RectTransform)child0).rect.width;
            oneCardPaddingInPixels = GetComponent<HorizontalLayoutGroup>().spacing;
        }

        public void Setup( bool doShake = false, bool startAtOne = false)
        {
            rolling = false;
            dragging = false;

            int totalCards = transform.childCount;

            if (totalCards == 0)
                return;

            if (totalCards > 1 )
            {
                if(!startAtOne)
                    Left.gameObject.SetActive(true);
                else
                    Left.gameObject.SetActive(false);

                Right.gameObject.SetActive(true);
            }
            else
            {
                Left.gameObject.SetActive(false);
                Right.gameObject.SetActive(false);
            }

            SetDistances();
            lastCard = totalCards - 1;

            focusedCard = startAtOne ? 0 : totalCards / 2;

            var rectTransform = ((RectTransform)transform);

            rectTransform.sizeDelta = new Vector2(
                oneCardWidthInPixels * totalCards + (oneCardPaddingInPixels * lastCard),
                rectTransform.sizeDelta.y);

            //rectTransform.anchoredPosition = Vector2.zero;
            rectTransform.anchoredPosition = new Vector2(
                (oneCardWidthInPixels * lastCard + (oneCardPaddingInPixels * totalCards)) /2,
                0);


            if (totalCards > 1)
            {
                var child0 = transform.GetChild(0);
                var child1 = transform.GetChild(1);
                DOVirtual.DelayedCall(.1f, () =>
                {
                    oneCardDistanceInWorldUnit = Mathf.Abs(child0.transform.position.x - child1.transform.position.x);

                    if (TT.IsEven(totalCards) && !startAtOne)
                    {
                        transform.DOMoveX(transform.position.x - (oneCardDistanceInWorldUnit / 2), doShake ? .3f : .1f).OnComplete(() =>
                        {
                            if (doShake)
                                IntroShake();
                        });
                    }
                });
            }
        }

        void Update()
        {
            if (!rolling && dragToPos != 0)
            {
                if (dragToPos > 0)
                {
                    dragToPos--;
                    DoMove(true, transform.position.x);
                }
                else
                {
                    dragToPos++;
                    DoMove(false, transform.position.x);
                }
            }
            else if (!rolling && dragging)
            {
                float mouseNow = Camera.main.ScreenToWorldPoint(Input.mousePosition).x;
                float deltaX = latestMousePosX - mouseNow;
                transform.position = new Vector3(transform.position.x - deltaX, transform.position.y, transform.position.z);
                latestMousePosX = mouseNow;
            }
        }

        public void OnDragStart()
        {
            //if (focusedCard==0 || focusedCard==lastCard)
            //    return;

            if (dragging || rolling)
                return;
            
            CurrentDistance = 0;
            dragging = true;
            startingMousePosX = Camera.main.ScreenToWorldPoint(Input.mousePosition).x;
            latestMousePosX = startingMousePosX;

            resetPositionOnInvalidDrag = transform.position.x;

            Delogger.Log("OnDragStart", startingMousePosX);

        }

        public void OnDragEnd()
        {
            if (rolling)
                return;

            if (!dragging)
                return;
            dragging = false;

            //var mouseX = Camera.main.ScreenToWorldPoint(Input.mousePosition).x;
            CurrentDistance = startingMousePosX - latestMousePosX;
            Delogger.Log("OnDragEnd CurrentDistance ", CurrentDistance);

            bool invalidDrag = (CurrentDistance < 0 && focusedCard == 0) || (CurrentDistance > 0 && focusedCard == lastCard);

            if (invalidDrag)
            {
                Delogger.Log("OnDragEnd invalidDrag ", invalidDrag);

                rolling = true;
                dragging = true;

                transform.DOMoveX(resetPositionOnInvalidDrag, .4f).OnComplete(() =>
                {
                    rolling = false;
                    dragging = false;
                    CurrentDistance = 0;
                    dragToPos = 0;
                });
            }
            else
            {
                if (Mathf.Abs(CurrentDistance) > 0)
                {
                    if (CurrentDistance < 0)
                        dragToPos++;
                    else
                        dragToPos--;
                    //Delogger.Log("changed dragToPos", dragToPos.ToString());
                }
            }
        }

        public void IntroShake()
        {
            var rectTransform = ((RectTransform)transform);
            rolling = true;
            dragging = true;

            Vector3 start = transform.position;

            rectTransform.DOMoveX(start.x + .2f, .1f).OnComplete(() =>
            {
                transform.DOMoveX(start.x - .2f, .1f).OnComplete(() =>
                   {
                       transform.DOMoveX(start.x, .4f).OnComplete(() =>
                       {
                           rolling = false;
                           dragging = false;
                       });
                   });
            });
        }

        void DoMove(bool left, float start)
        {
            if (rolling)
                return;

            rolling = true;
            dragging = false;

            float xtarget = start;
            if (left)
            {
                if (focusedCard > 0)
                {
                    xtarget += oneCardDistanceInWorldUnit;
                    focusedCard--;
                }
            }
            else
            {
                if (focusedCard < lastCard)
                {
                    xtarget -= oneCardDistanceInWorldUnit;
                    focusedCard++;
                }
            }

            if (!Mathf.Approximately(Mathf.Abs(xtarget - transform.position.x), 0))
            {
                transform.DOMoveX(xtarget + CurrentDistance, .5f).OnComplete(() =>
                {
                    if (focusedCard == lastCard)
                    {
                        Right.gameObject.SetActive(false);
                        Left.gameObject.SetActive(true);
                    }
                    else if (focusedCard == 0)
                    {
                        Left.gameObject.SetActive(false);
                        Right.gameObject.SetActive(true);
                    }
                    else if (lastCard > 0)
                    {
                        Left.gameObject.SetActive(true);
                        Right.gameObject.SetActive(true);
                    }

                    rolling = false;
                });
            }
            else
            {
                rolling = false;
            }

            CurrentDistance = 0;
        }

        public void MoveLeft()
        {
            if (dragging)
                return;
            dragToPos++;
        }

        public void MoveRight()
        {
            if (dragging)
                return;
            dragToPos--;
        }
    }
}
