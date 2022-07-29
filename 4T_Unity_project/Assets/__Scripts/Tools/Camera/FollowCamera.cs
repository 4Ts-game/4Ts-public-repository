// Author: Pietro Polsinelli - http://designAGame.eu
// Twitter https://twitter.com/ppolsinelli
// License: WTFPL, all free as in free beer :-)
// Tested with Unity 5.6.1
// Created: 22 06 2017

using DG.Tweening;
using UnityEngine;

namespace OL
{
    public class FollowCamera : MonoBehaviour
    {
        public float interpVelocity;
        public float minDistance;
        public float followDistance;

        public bool FollowDrag;
        public float DragMultiplier;
        public float LerpPercentile;
        bool panning;
        Vector3 mouseOrigin;
        bool xtweening;
        bool ytweening;

        public float ZoomChangeFactor;

        public GameObject TargetToFollow;
        public Vector3 Offset;
        Vector3 targetPos;

        public Transform EastBoundary;
        Renderer ebR;
        public Transform WestBoundary;
        Renderer wbR;
        public Transform NorthBoundary;
        Renderer nbR;
        public Transform SouthBoundary;
        Renderer sbR;

        Camera aCamera;

        //debug
        /*bool eOldVis;
        bool wOldVis;
        bool nOldVis;
        bool sOldVis;*/


        // Use this for initialization
        void Start()
        {
            aCamera = GetComponent<Camera>();

            targetPos = transform.position;
            ebR = EastBoundary.GetComponent<Renderer>();
            wbR = WestBoundary.GetComponent<Renderer>();
            nbR = NorthBoundary.GetComponent<Renderer>();
            sbR = SouthBoundary.GetComponent<Renderer>();
        }

        // Update is called once per frame
        void Update()
        {
            if (TargetToFollow != null)
            {
                /*if (ebR.IsVisibleFrom(aCamera) != eOldVis)
                    Debug.Log("east visible: " + ebR.IsVisibleFrom(aCamera));
                if (wbR.IsVisibleFrom(aCamera) != wOldVis)
                    Debug.Log("west visible: " + wbR.IsVisibleFrom(aCamera));
                if (nbR.IsVisibleFrom(aCamera) != nOldVis)
                    Debug.Log("north visible: " + nbR.IsVisibleFrom(aCamera));
                if (sbR.IsVisibleFrom(aCamera) != sOldVis)
                    Debug.Log("south visible: " + sbR.IsVisibleFrom(aCamera));*/

                var towardsPosition = TargetToFollow.transform.position;

                TowardsPosition(towardsPosition);
            }

            if (FollowDrag)
            {
                if (!panning && Input.GetMouseButtonDown(0))
                {
                    panning = true;
                    mouseOrigin = Input.mousePosition;
                }
                else if (panning && !Input.GetMouseButton(0))
                {
                    panning = false;
                }

                if (panning)
                {
                    var pos = Camera.main.ScreenToViewportPoint(mouseOrigin - Input.mousePosition) * DragMultiplier;
                    //Debug.Log(pos);
                    FollowPosition(pos);
                    mouseOrigin = Input.mousePosition;
                }
            }
        }

        void FollowPosition(Vector3 towardsPosition)
        {
            var xtweenbounce = false;
            var ytweenbounce = false;
            float xdelta = 0;
            float ydelta = 0;
            var delta = .1f;

            //Vector3 tpo = new Vector3(towardsPosition.x, towardsPosition.y, transform.position.z);
            var tpo = transform.position + new Vector3(towardsPosition.x, towardsPosition.y, 0);

            if (ebR.IsVisibleFrom(aCamera) && tpo.x > transform.position.x)
            {
                tpo = new Vector3(transform.position.x, tpo.y, tpo.z);
                xtweenbounce = true;
                xdelta = delta;
            }
            else if (wbR.IsVisibleFrom(aCamera) && tpo.x < transform.position.x)
            {
                tpo = new Vector3(transform.position.x, tpo.y, tpo.z);
                xtweenbounce = true;
                xdelta = -delta;
            }

            if (!xtweening && xtweenbounce)
            {
                xtweening = true;
                transform.DOMoveX(transform.position.x + xdelta, .1f).OnComplete(() =>
                {
                    transform.DOMoveX(transform.position.x - xdelta, .1f).OnComplete(() => { xtweening = false; });
                });
            }

            if (nbR.IsVisibleFrom(aCamera) && tpo.y > transform.position.y)
            {
                tpo = new Vector3(tpo.x, transform.position.y, tpo.z);
                ytweenbounce = true;
                ydelta = delta;
            }

            else if (sbR.IsVisibleFrom(aCamera) && tpo.y < transform.position.y)
            {
                tpo = new Vector3(tpo.x, transform.position.y, tpo.z);
                ytweenbounce = true;
                ydelta = -delta;
            }

            if (!ytweening && ytweenbounce)
            {
                ytweening = true;
                transform.DOMoveY(transform.position.y + ydelta, .1f).OnComplete(() =>
                {
                    transform.DOMoveY(transform.position.y - ydelta, .1f).OnComplete(() => { ytweening = false; });
                });
            }

            transform.position = tpo; //Vector3.Lerp(transform.position, tpo, LerpPercentile);

            /*eOldVis = ebR.IsVisibleFrom(aCamera);
            wOldVis = wbR.IsVisibleFrom(aCamera);
            nOldVis = nbR.IsVisibleFrom(aCamera);
            sOldVis = sbR.IsVisibleFrom(aCamera);*/
        }

        void TowardsPosition(Vector3 towardsPosition)
        {
            var posNoZ = transform.position;
            posNoZ.z = towardsPosition.z;

            var targetDirection = towardsPosition - posNoZ;

            interpVelocity = targetDirection.magnitude * 5f;

            targetPos = transform.position + targetDirection.normalized * interpVelocity * Time.deltaTime;
            var tpo = targetPos + Offset;

            if (ebR.IsVisibleFrom(aCamera) && tpo.x > transform.position.x)
                tpo = new Vector3(transform.position.x, tpo.y, tpo.z);
            else if (wbR.IsVisibleFrom(aCamera) && tpo.x < transform.position.x)
                tpo = new Vector3(transform.position.x, tpo.y, tpo.z);

            if (nbR.IsVisibleFrom(aCamera) && tpo.y > transform.position.y)
                tpo = new Vector3(tpo.x, transform.position.y, tpo.z);
            else if (sbR.IsVisibleFrom(aCamera) && tpo.y < transform.position.y)
                tpo = new Vector3(tpo.x, transform.position.y, tpo.z);

            transform.position = Vector3.Lerp(transform.position, tpo, 0.25f);

            /*eOldVis = ebR.IsVisibleFrom(aCamera);
            wOldVis = wbR.IsVisibleFrom(aCamera);
            nOldVis = nbR.IsVisibleFrom(aCamera);
            sOldVis = sbR.IsVisibleFrom(aCamera);*/
        }

        public void ZoomPlus()
        {
            Camera.main.orthographicSize = Camera.main.orthographicSize * (1 + ZoomChangeFactor);
        }

        public void ZoomMinus()
        {
            Camera.main.orthographicSize = Camera.main.orthographicSize * (1 - ZoomChangeFactor);
        }

        public void SetLerp(string afloat)
        {
            LerpPercentile = float.Parse(afloat);
        }

        public void SetDragMultiplier(string afloat)
        {
            DragMultiplier = float.Parse(afloat);
        }
    }

    public static class RendererExtensions
    {
        public static bool IsVisibleFrom(this Renderer renderer, Camera camera)
        {
            var planes = GeometryUtility.CalculateFrustumPlanes(camera);
            return GeometryUtility.TestPlanesAABB(planes, renderer.bounds);
        }
    }
}