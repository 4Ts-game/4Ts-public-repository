using UnityEngine;

namespace RD
{
    public class CameraHDRatio : MonoBehaviour
    {
        #region metody

        #region rescale camera

        void RescaleCamera()
        {
            if (Screen.width == ScreenSizeX && Screen.height == ScreenSizeY) return;

            var targetaspect = 16.0f / 9.0f;
            var windowaspect = Screen.width / (float) Screen.height;
            var scaleheight = windowaspect / targetaspect;
            var camera = GetComponent<Camera>();

            if (scaleheight < 1.0f)
            {
                var rect = camera.rect;

                rect.width = 1.0f;
                rect.height = scaleheight;
                rect.x = 0;
                rect.y = (1.0f - scaleheight) / 2.0f;

                camera.rect = rect;
            }
            else // add pillarbox
            {
                var scalewidth = 1.0f / scaleheight;

                var rect = camera.rect;

                rect.width = scalewidth;
                rect.height = 1.0f;
                rect.x = (1.0f - scalewidth) / 2.0f;
                rect.y = 0;

                camera.rect = rect;
            }

            ScreenSizeX = Screen.width;
            ScreenSizeY = Screen.height;
        }

        #endregion

        #endregion

        #region Pola

        int ScreenSizeX;
        int ScreenSizeY;

        #endregion

        #region metody unity

        void OnPreCull()
        {
            if (Application.isEditor)
                return;
            var wp = Camera.main.rect;
            var nr = new Rect(0, 0, 1, 1);
            //Camera.main.rect = nr;
            Camera.main.rect = wp;
        }

        // Use this for initialization
        void Start()
        {
            RescaleCamera();
        }

        bool didScaleOnUpdate;

        void Update()
        {
            if (didScaleOnUpdate )
            {
                RescaleCamera();
                didScaleOnUpdate = true;
            }
        }

        #endregion
    }
}