namespace OpenCvSharp.Demo {

	using UnityEngine;
	using System.Collections;
	using UnityEngine.UI;
	using Aruco;

	public class MarkerDetector : MonoBehaviour {

		public Texture2D texture;
		public RawImage rawimage;

		void Start () {

			WebCamDevice[] cam_devices = WebCamTexture.devices;
			// for debugging purposes, prints available devices to the console
			for (int i = 0; i < cam_devices.Length; i++)
			{
				Debug.Log("Webcam available: " + cam_devices[i].name);
			}

			WebCamTexture webcamTexture = new WebCamTexture();//cam_devices[0].name, 1920, 1080,  25
			rawimage.texture = webcamTexture;
			rawimage.material.mainTexture = webcamTexture;
			webcamTexture.Play();

			// Create default parameres for detection
			DetectorParameters detectorParameters = DetectorParameters.Create();

			// Dictionary holds set of all available markers
			Dictionary dictionary = CvAruco.GetPredefinedDictionary (PredefinedDictionaryName.Dict6X6_1000);
			//Dictionary dictionary = CvAruco.GetPredefinedDictionary (PredefinedDictionaryName.Dict6X6_250);

			// Variables to hold results
			Point2f[][] corners;
			int[] ids;
			Point2f[][] rejectedImgPoints;

			// Create Opencv image from unity texture
			Mat mat = Unity.TextureToMat (this.texture);

			// Convert image to grasyscale
			Mat grayMat = new Mat ();
			Cv2.CvtColor (mat, grayMat, ColorConversionCodes.BGR2GRAY); 

			// Detect and draw markers
			CvAruco.DetectMarkers (grayMat, dictionary, out corners, out ids, detectorParameters, out rejectedImgPoints);
			CvAruco.DrawDetectedMarkers (mat, corners, ids);

			Debug.Log(string.Join(", ", ids));


			foreach(Point2f[] p in corners)
            {
				Debug.Log(string.Join(", ", p));
			}

			// Create Unity output texture with detected markers
			Texture2D outputTexture = Unity.MatToTexture (mat);

			// Set texture to see the result
			RawImage rawImage = gameObject.GetComponent<RawImage> ();
			rawImage.texture = outputTexture;
		}
		
	}
}