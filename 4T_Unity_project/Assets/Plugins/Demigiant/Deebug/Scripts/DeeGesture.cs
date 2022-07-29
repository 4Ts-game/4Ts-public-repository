// Author: Daniele Giardini - http://www.demigiant.com
// Created: 2019/03/25 20:44
// License Copyright (c) Daniele Giardini

using System.Collections.Generic;
using DG.DeebugLib.Extensions;
using UnityEngine;

namespace DG.DeebugLib
{
    public static class DeeGesture
    {
        public enum GestureTolerance
        {
            Precise,
            Normal,
            Tolerant,
            VeryTolerant
        }

        // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████
        // ███ INTERNAL CLASSES ████████████████████████████████████████████████████████████████████████████████████████████████
        // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

        public static class Circle
        {
            static readonly List<Vector2> _Points = new List<Vector2>(500);
            static Vector2 _lastP;
            static float _minDrawDistance;
            static float _errorFactor;

            #region Public Methods

            public static void StartCapture(Vector2 mouseP, GestureTolerance tolerance)
            {
                _minDrawDistance = 20.Scaled();
                switch (tolerance) {
                case GestureTolerance.Precise:
                    _errorFactor = 10.Scaled();
                    break;
                case GestureTolerance.Normal:
                    _errorFactor = 30.Scaled();
                    break;
                case GestureTolerance.Tolerant:
                    _errorFactor = 60.Scaled();
                    break;
                default: // Very tolerant
                    _errorFactor = 110.Scaled();
                    break;
                }
                _Points.Add(mouseP);
                _lastP = mouseP;
            }

            public static void Update(Vector2 mouseP)
            {
                if (Vector2.Distance(_lastP, mouseP) < _minDrawDistance) return;
                _Points.Add(mouseP);
                _lastP = mouseP;
            }

            /// <summary>
            /// Returns TRUE if a circle gesture is recognized since the last <see cref="StartCapture"/>
            /// </summary>
            public static bool StopAndValidate(Vector2 mouseP)
            {
                Update(mouseP);

                int totPs = _Points.Count;
                if (totPs < 4) return CleanUpAndReportSuccess(false);

                Vector2 tot = Vector2.zero;
                for (int i = 0; i < totPs; ++i) tot += _Points[i];
                Vector2 center = new Vector2(tot.x / totPs, tot.y / totPs);
                float approxRadius = Vector2.Distance(center, _Points[0]);
                float errorFactor = Mathf.Min(_errorFactor, approxRadius * 0.5f);
//                Debug.Log("tot points: " + totPs + ", center: " + center + ", approxRadius: " + approxRadius);

                float minDist = approxRadius - errorFactor;
                float maxDist = approxRadius + errorFactor;
                for (int i = 0; i < totPs; ++i) {
                    float dist = Vector2.Distance(_Points[i], center);
                    if (dist < minDist || dist > maxDist) return CleanUpAndReportSuccess(false);
                }

                return CleanUpAndReportSuccess(true);
            }

            #endregion

            #region Methods

            static bool CleanUpAndReportSuccess(bool success)
            {
                _Points.Clear();
                return success;
            }

            #endregion
        }
    }
}