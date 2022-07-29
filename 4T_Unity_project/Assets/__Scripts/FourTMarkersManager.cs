
using UnityEngine;
using System;
using System.Collections;
using UnityEngine.UI;
using OpenCvSharp.Aruco;
using OpenCvSharp;
using System.Collections.Generic;
using System.Linq;
using DG.Tweening;
using TMPro;
using DG.Debugging;

namespace FourT
{

	public class FourTMarkersManager : MonoBehaviour
	{

		public static FourTMarkersManager I;

		public bool MissingTechnique = true;

		public bool HybridIsActive;

		[Header("Functional Markers:")]
		[Space]
		//Calibration Markers (0-9)

		//Action Markers (10-20)
		public int ActivationMarker;

		[Space]
		public int CheckCompletenessMarker;
		public bool CheckCompletenessIsActive;

		[Space]
		public int SuggestionsMarker;
		private bool SuggestionsIsActive;

		[Space]
		public int Level1Marker;
		public int Level2Marker;
		public int Level3Marker;

		[Space]
		public int FirstCardMarkerId;
		[Space]

		[Header("UI Elements:")]
		[Space]
		//public Texture2D texture;
		public RawImage VideoSurface;
		public RawImage ScreenShotSurface;
		public Transform Viewer;
		public Transform Debugger;
		public TMP_Text DebugBox;

		public PhysicBoard PhysicBoard;
		public List<Marker> Markers;

		[Header("Settings:")]
		[Space]
		public float Cols;
		public float Rows;
		[Space]
		public int CalibrationMaxFaild = 0;
		private int CalibrationFailds = 0;
		public float Delay;
		public float OnErrorDelay = 0f;

		public WebCamTexture WebcamTexture;

		public bool WebcamUnavailable;

		Dictionary<string, WebCamTexture> AllWebCamTextures;
		int techniqueIsMissingCounter;

		// Variables to hold results
		Point2f[][] Corners;
		int[] Ids;
		Point2f[][] RejectedImgPoints;

		List<Marker> CalibrationMarkers;

		Point2f TopLeftCorner;
		Point2f TopRightCorner;
		Point2f BottomLeftCorner;

		public bool IsAnalyzing;
		public bool Error;

		//Time intervall in seconds for the Board Analysis
		//public int Delay;
		private float nextActionTime = 3f;


		private void Awake()
		{
			I = this;
			AllWebCamTextures = new Dictionary<string, WebCamTexture>();
		}

		void Update()
		{

			if (!HybridIsActive)
			{
				DebugBox.text = "The Hybrid game is disabled";
				return;
			}

			if (AllWebCamTextures.Count == 0)
			{
				DebugBox.text = "No webcam available on this computer";
				return;
			}

			float d = Error ? OnErrorDelay : Delay;
			if (Time.time > nextActionTime)
			{
				nextActionTime += d;
				Analize();
			}
		}

		void Start()
		{

			HybridIsActive = false;
			SuggestionsIsActive = false;
			techniqueIsMissingCounter = 0;

			ScreenShotSurface.gameObject.SetActive(false);

			WebCamDevice[] cam_devices = WebCamTexture.devices;

			// for debugging purposes, prints available devices to the console
			for (int i = 0; i < cam_devices.Length; i++)
			{
				DebugBox.text += $"Webcam: {cam_devices[i].name}<br>";
				Debug.Log($"Webcam available: {cam_devices[i].name}");
			}

			//if(cam_devices.Length>0)
			//	SetWebcam(SettingsManager.I.Webcam.name);

			PhysicBoard = new PhysicBoard();

		}

		public void SetWebcam(string webcamName)
		{

			ClearAllWebcams();

			if (AllWebCamTextures.ContainsKey(webcamName))
			{
				WebcamTexture = AllWebCamTextures[webcamName];
				try
				{
					WebcamTexture.Play();
					WebcamUnavailable = false;
				}
				catch (Exception e)
				{
					Debug.Log("Andrea... Allora... This webcam can not play: " + e);
					AlertManager.I.ShowAlert("Andrea... Allora... This webcam can not play: ");
					WebcamUnavailable = true;
				}

				return;
			}

			WebcamTexture = new WebCamTexture(webcamName, 2500, 2000, 25);//cam_devices[0].name, 1920, 1080,  25
			VideoSurface.texture = WebcamTexture;

			try
			{
				WebcamTexture.Play();
				WebcamUnavailable = false;

			}
			catch (Exception e)
			{
				Debug.Log("Andrea... Allora... This webcam can not play");
				AlertManager.I.ShowAlert("Andrea... Allora... This webcam can not play: ");
				WebcamUnavailable = true;
			}

			AllWebCamTextures[webcamName] = WebcamTexture;
		}

		public void ClearWebcam()
		{
			if (WebcamTexture != null)
			{
				WebcamTexture.Stop();
				WebcamTexture = null;
			}
		}


		public void ClearAllWebcams()
		{
			foreach (KeyValuePair<string, WebCamTexture> texture in AllWebCamTextures)
			{
				texture.Value.Stop();
			}
		}

		public void ClearBoard()
        {
			foreach (CardPlaceHolder cPh in BoardManager.I.CardPlaceHolders)
			{

				BoardManager.I.ActualBoardCard = cPh.transform.GetChild(0).transform;
				BoardManager.I.ActualCard = Card.CardByID(cPh.CardId.ToString());

				if(BoardManager.I.ActualBoardCard.name != "ErrorIcon")
					Destroy(BoardManager.I.ActualBoardCard.gameObject);

				// Remove card from board
				if (BoardManager.I.ActualCard != null)
				{
					BoardManager.I.RemoveCardFromBoard();
					cPh.HasBeenDrawed = false;
				}
			}
		}


		bool canShowAlert = false;
		int cardsCountAtError = 0;

		public void Reset()
		{

			foreach (CardPlaceHolder cPh in BoardManager.I.CardPlaceHolders)
			{
				cPh.HasBeenDrawed = false;
				BoardManager.I.ActualCard = Card.CardByID(cPh.CardId.ToString());

				if(BoardManager.I.ActualCard!= null && BoardManager.I.ActualCard.BoardLocation != null)
					FourTManager.I().Game.LocationAndCard.Remove(BoardManager.I.ActualCard.BoardLocation);

				cPh.CardId = 0;

			}

			ClearBoard();
			KbOut.Error = false;
			Markers = new List<Marker>();
			FourTManager.I().Game.LocationAndCard = new Dictionary<string, int>();
			//BoardManager.I.RemoveAllCardsFromBoard();

		}


		void Analize()
		{
			if (AllWebCamTextures.Count == 0 || WebcamUnavailable)
			{
				DebugBox.text = "No webcam available on this computer";
				return;
			}

			DebugBox.text = "";
			GetMarkers();


			//Check Completeness Card
			if (GetMarkerById(CheckCompletenessMarker) != null && !CheckCompletenessIsActive)
			{
				CheckCompletenessIsActive = true;
				BoardManager.I.CompletenessCheck();
			}
			else if (GetMarkerById(CheckCompletenessMarker) == null)
			{
				CheckCompletenessIsActive = false;
			}

			//if (GetMarkerById(ActivationMarker) == null) //IsAnalyzing ||
			//{
			//	DebugBox.text = $"Place the 'Activation Card' to start analyzing the board";
			//	return;
			//}

			Marker m0 = GetMarkerById(0);
			Marker m1 = GetMarkerById(1);
			Marker m2 = GetMarkerById(2);
			Marker m3 = GetMarkerById(3);

			CalibrationMarkers = new List<Marker>();
			if (m0 != null)
				CalibrationMarkers.Add(m0);
			if (m1 != null)
				CalibrationMarkers.Add(m1);
			if (m2 != null)
				CalibrationMarkers.Add(m2);
			if (m3 != null)
				CalibrationMarkers.Add(m3);

			//if (CalibrationMarkers.Count < 4)
			//	return;

			if (!SetBoardCorners() && CalibrationFailds == CalibrationMaxFaild)
			{
				//CalibrationFailds = 0;
				return;
			}

			List<int> markerIds = new List<int>();
			MissingTechnique = true;

			foreach (Marker m in Markers)
			{
				// Markers from 0 to 3 are for registry

				markerIds.Add(m.Id);

				// Get Card markers
				if (m.Id >= FirstCardMarkerId)
				{
					setPosInGrid(m);

					Card card = Card.CardByID(m.Id.ToString());

					if (card != null && card.Type == Card.CardType.Technique)
					{
						MissingTechnique = false;

					} 
				}
			}

			if (MissingTechnique)
				techniqueIsMissingCounter++;

			//If the Suggestions card has been removed from the board

			Marker suggestionM = GetMarkerById(SuggestionsMarker);

			if (suggestionM == null)
			{
				SuggestionsIsActive = false;
				BoardManager.I.HideCardChooser();
			}

			//if(KbOut.Error)
			//	FourTManager.I().Game.LocationAndCard = new Dictionary<string, int>();

			foreach (CardPlaceHolder cPh in BoardManager.I.CardPlaceHolders)
			{

				if(cPh.CardId >= 0)
					Debug.Log("cPh.CardId::: " + cPh.CardId);

				if (cPh.HasBeenDrawed && cPh.CardId >= 0 && cPh.transform.childCount > 0)
				{
					if (!CardIsStillThere(cPh) && !Error) 
					{
						if (Time.time - cPh.CardSetAtTime > 6)
						{
							BoardManager.I.ActualBoardCard = cPh.transform.GetChild(0).transform;
							BoardManager.I.ActualCard = Card.CardByID(cPh.CardId.ToString());

							// Remove card from board
							BoardManager.I.RemoveCardFromBoard();
							cPh.HasBeenDrawed = false;
						}
					}
				}
			}

			/*****************************************************/
			// Syncronize Fisical board with digital board
			/*****************************************************/


			//If playing at level 1 and there's any Technique card on the board throw an error

			//Debug.Log("Cards on board :: " + FourTManager.I().Game.LocationAndCard.Count());

			if (KbOut.Error)
			{
				Debug.Log("ERROR");

				//Reset();
				return;
			}


			if (MissingTechnique && !SettingsManager.I.SettingsIsOpen && FourTManager.I().Game.LocationAndCard.Count() >= 1 && techniqueIsMissingCounter >= 6 && FourTManager.I().Game.Level == 1)
			{
				BoardManager.I.ShowErrorInfo(4);
				return;
			}
			else
			{
				BoardManager.I.HideErrorInfo();
			}



			//FourTManager.I().Game.LocationAndCard = new Dictionary<string, int>();
			//BoardManager.I.RemoveAllCardsFromBoard();

			int cardsCount = FourTManager.I().Game.LocationAndCard.Count();
			if (BoardManager.I.ErrorOnBoard && !SettingsManager.I.SettingsIsOpen)
			{

				if (cardsCountAtError > 0 && cardsCountAtError < cardsCount)
                {
					BoardManager.I.ShowErrorInfo(3);
					Delogger.Log("Error On Board", BoardManager.I.ErrorOnBoard);
					Delogger.Log("cardsCount", cardsCount);

					return;

				} 

				cardsCountAtError = cardsCount;

			} else
            {
				cardsCountAtError = 0;
				BoardManager.I.HideErrorInfo();

			}

			//FourTManager.I().Game.LocationAndCard = new Dictionary<string, int>();

			foreach (Marker m in Markers)
			{

				if (m.Id >= FirstCardMarkerId && !(m.Col==0 && m.Row==0))
				{
					DebugBox.text += $"<br>---------------------------------<br>";
					DebugBox.text += $"CARD ID:: <b>{m.Id}<br>";
					DebugBox.text += $"COL: " + m.Col + " ROW: " + m.Row + "</b>";

					CardPlaceHolder cPh = CardPlaceHolder.GetPlaceHolderByRowCol((int)m.Row, (int)m.Col);

					if (cPh == null)
						continue;


					//If the Suggestions card is on the table
					if (m.Id == SuggestionsMarker && !SuggestionsIsActive)
					{

						SuggestionsIsActive = true;
						//Debug.Log($"SuggestionsMarker is Active {m.Id} COL: {m.Col} ROW {m.Row}");

						cPh.CardId = m.Id;
						BoardManager.I.SetLocationAndColumnFromCardPlaceHolder(cPh.transform, cPh.DigitalCol);

						ServerJob.I.SuggestionCheckCall(FourTManager.I().Game, $"C{BoardManager.I.ActualColumn.ToString().ToUpper()}-{BoardManager.I.ActualPlace.name.ToUpper()}",
						() => {
							Card.CardType cardType = BoardManager.I.GetcardTypeByPlaceHolderName(BoardManager.I.ActualPlace.name);
							BoardManager.I.ShowCardChooser(cardType, FourTManager.I().LatestKbOut.Alternatives, false, false);
						});

						continue;
					}

					//If the SuggestionsMarker has been removed close the panel
					else if (SuggestionsIsActive && !markerIds.Contains(SuggestionsMarker))
					{
						BoardManager.I.HideCardChooser();
						SuggestionsIsActive = false;
					}

					Card card = Card.CardByID(m.Id.ToString());


					if (card == null || cPh == null)
						continue;


					DebugBox.text += "</b>" + $"Digital Placeholder: " + cPh.name;


					if (!cPh.HasBeenDrawed)
					{
						cPh.CardId = m.Id;

						cPh.CardSetAtTime = Time.time;

						//Insert card on board
						BoardManager.I.SetLocationAndColumnFromCardPlaceHolder(cPh.transform, cPh.DigitalCol);

						//Debug.Log("canDrawCard::: " + !BoardManager.I.ErrorOnBoard);

						BoardManager.I.PlaceCardOnBoard(card, false, !BoardManager.I.ErrorOnBoard);
						cPh.HasBeenDrawed = true;
						
						
					}
				}
			}

			Error = false;

			//WebcamTexture.Pause();
		}

		public bool CardIsStillThere(CardPlaceHolder cPh)
		{

			bool cardIsStillThere = false;

			foreach (Marker m in Markers)
			{
				if (cPh.CardId == m.Id && cPh.Col == m.Col && cPh.Row == m.Row)
					cardIsStillThere = true;
			}

			return cardIsStillThere;
		}


		public void GetMarkers()
		{
			if (WebcamTexture == null)
				return;

			if (!WebcamTexture.isPlaying)
				return;

			Markers = new List<Marker>();

			// Create default parameres for detection
			DetectorParameters detectorParameters = DetectorParameters.Create();

			// Dictionary holds set of all available markers
			Dictionary dictionary = CvAruco.GetPredefinedDictionary(PredefinedDictionaryName.Dict4X4_250);

			Texture2D t = TempTexturesManager.CreateTexture2D(WebcamTexture.width, WebcamTexture.height);


			t.SetPixels(WebcamTexture.GetPixels());
			t.Apply();

			//t = rotateTexture(t, -90);

			t.hideFlags = HideFlags.HideAndDontSave;


			Mat mat = OpenCvSharp.Unity.TextureToMat(t);

			if (SettingsManager.I.CamWindow.gameObject.activeSelf)
			{
				ScreenShotSurface.gameObject.SetActive(true);
				ScreenShotSurface.texture = t;
			}

			// Convert image to grasyscale
			Mat grayMat = new Mat();
			Cv2.CvtColor(mat, grayMat, ColorConversionCodes.BGR2GRAY);

			// Detect and draw markers


			CvAruco.DetectMarkers(grayMat, dictionary, out Corners, out Ids, detectorParameters, out RejectedImgPoints);
			CvAruco.DrawDetectedMarkers(mat, Corners, Ids);

			if (SettingsManager.I.CamWindow.gameObject.activeSelf)
			{
				// Create Unity output texture with detected markers
				Texture2D outputTexture = OpenCvSharp.Unity.MatToTexture(mat);
				outputTexture.hideFlags = HideFlags.HideAndDontSave;
				TempTexturesManager.AddToQueue(outputTexture);

				// Set texture to see the result
				ScreenShotSurface.texture = outputTexture;
			}


			//Create Markers Objects
			int x = 0;
			foreach (int id in Ids)
			{

				Marker m = GetMarkerById(Ids[x]);

				if (m == null)
				{
					m = new Marker();
					m.Id = Ids[x];
					Markers.Add(m);
				}

				m.Corners = Corners[x];
				x++;
			}

			// Order Markers list
			Markers = Markers.OrderBy(o => o.Id).ToList();
		}

		bool SetBoardCorners()
		{

			if (CalibrationMarkers.Count() < 4)
			{
				CalibrationFailds++;
				int lostMarker = -1;

				for (int i = 0; 0 < CalibrationMarkers.Count(); i++)
				{
					if (GetMarkerById(i) == null)
					{
						lostMarker = i;
						break;
					}

				}
				//				Debug.Log("Not all the corners of the board are visible");
				DebugBox.text = $"The {lostMarker} corner is not visible.";
				Error = true;
				return false;
			}

			CalibrationFailds = 0;
			//detect which has the min x and the min y position
			float maxX = float.MinValue,
				minX = float.MaxValue,
				maxY = float.MinValue,
				minY = float.MaxValue;


			foreach (Marker m in CalibrationMarkers)
			{
				if (m.Corners[0].X >= maxX)
					maxX = m.Corners[0].X;

				if (m.Corners[0].X <= minX)
					minX = m.Corners[0].X;

				if (m.Corners[0].Y >= maxY)
					maxY = m.Corners[0].Y;

				if (m.Corners[0].Y <= minY)
					minY = m.Corners[0].Y;
			}

			PhysicBoard.TopLeft = null;
			PhysicBoard.TopRight = null;
			PhysicBoard.BottomLeft = null;
			PhysicBoard.BottomRight = null;

			float minDistTopLeft = float.MaxValue;
			float minDistTopRight = float.MaxValue;
			float minDistBottomLeft = float.MaxValue;
			float minDistBottomRight = float.MaxValue;


			foreach (Marker m in CalibrationMarkers)
			{
				float distTL = Math.Abs(minX - m.Corners[0].X) + Math.Abs(minY - m.Corners[0].Y);

				if (distTL < minDistTopLeft)
				{
					minDistTopLeft = distTL;
					PhysicBoard.TopLeft = m;
				}

				float distTR = Math.Abs(maxX - m.Corners[0].X) + Math.Abs(minY - m.Corners[0].Y);
				if (distTR < minDistTopRight)
				{
					minDistTopRight = distTR;
					PhysicBoard.TopRight = m;
				}

				float distBL = Math.Abs(minX - m.Corners[0].X) + Math.Abs(maxY - m.Corners[0].Y);
				if (distBL < minDistBottomLeft)
				{
					minDistBottomLeft = distBL;
					PhysicBoard.BottomLeft = m;
				}

				float distBR = Math.Abs(maxX - m.Corners[0].X) + Math.Abs(maxY - m.Corners[0].Y);
				if (distBR < minDistBottomRight)
				{
					minDistBottomRight = distBR;
					PhysicBoard.BottomRight = m;
				}

			}

			//Check if the board is correctly aligned
			int diff = (int)(PhysicBoard.TopRight.Corners[0].Y - PhysicBoard.TopLeft.Corners[0].Y);
			if (diff > 20 || diff < -20)
			{
				DebugBox.text = $"Board is not correctly Aligned: the difference between the two Xs is <b>{diff}px</b>";
				Error = true;

				//todo: open an alert "The board is not aligned correctly"

				return false;
			}

			//Debug.Log("-------------------------------------");
			//Debug.Log("-------------------------------------");
			//Debug.Log("Rows :: " + Rows);
			//Debug.Log("Cols :: " + Cols);


			//Debug.Log($"TL MARKER:: {PhysicBoard.TopLeft.Id}");
			//Debug.Log($"TR MARKER:: {PhysicBoard.TopRight.Id}");
			//Debug.Log($"BL MARKER:: {PhysicBoard.BottomLeft.Id}");
			//Debug.Log($"BR MARKER:: {PhysicBoard.BottomRight.Id}");
			//Debug.Log("-------------------------------------");

			if (PhysicBoard.TopLeft.Id != 0)
			{
				DebugBox.text = $"Board is not correctly positioned; top left corner Marker should be the '0' and not the '{PhysicBoard.TopLeft.Id}'";
				Error = true;
				return false;
			}

			Error = false;
			return true;
		}

		/** START ROTATE
		* **/


		Texture2D rotateTexture(Texture2D tex, float angle)
		{
			Debug.Log("rotating");
			Texture2D rotImage = new Texture2D(tex.width, tex.height);
			int x, y;
			float x1, y1, x2, y2;

			int w = tex.width;
			int h = tex.height;
			float x0 = rot_x(angle, -w / 2.0f, -h / 2.0f) + w / 2.0f;
			float y0 = rot_y(angle, -w / 2.0f, -h / 2.0f) + h / 2.0f;

			float dx_x = rot_x(angle, 1.0f, 0.0f);
			float dx_y = rot_y(angle, 1.0f, 0.0f);
			float dy_x = rot_x(angle, 0.0f, 1.0f);
			float dy_y = rot_y(angle, 0.0f, 1.0f);


			x1 = x0;
			y1 = y0;

			for (x = 0; x < tex.width; x++)
			{
				x2 = x1;
				y2 = y1;
				for (y = 0; y < tex.height; y++)
				{
					//rotImage.SetPixel (x1, y1, Color.clear);          

					x2 += dx_x;//rot_x(angle, x1, y1);
					y2 += dx_y;//rot_y(angle, x1, y1);
					rotImage.SetPixel((int)Mathf.Floor(x), (int)Mathf.Floor(y), getPixel(tex, x2, y2));
				}

				x1 += dy_x;
				y1 += dy_y;

			}

			rotImage.Apply();
			return rotImage;
		}

		private Color getPixel(Texture2D tex, float x, float y)
		{
			Color pix;
			int x1 = (int)Mathf.Floor(x);
			int y1 = (int)Mathf.Floor(y);

			if (x1 > tex.width || x1 < 0 ||
			   y1 > tex.height || y1 < 0)
			{
				pix = Color.clear;
			}
			else
			{
				pix = tex.GetPixel(x1, y1);
			}

			return pix;
		}

		private float rot_x(float angle, float x, float y)
		{
			float cos = Mathf.Cos(angle / 180.0f * Mathf.PI);
			float sin = Mathf.Sin(angle / 180.0f * Mathf.PI);
			return (x * cos + y * (-sin));
		}
		private float rot_y(float angle, float x, float y)
		{
			float cos = Mathf.Cos(angle / 180.0f * Mathf.PI);
			float sin = Mathf.Sin(angle / 180.0f * Mathf.PI);
			return (x * sin + y * cos);
		}

/** END ROTATE
 * **/

		void setPosInGrid(Marker m)
		{

			if (m.Id < SuggestionsMarker)
				return;

			if (CalibrationMarkers.Count < 4)
				return;

			TopLeftCorner = PhysicBoard.TopLeft.Corners[2];
			TopRightCorner = PhysicBoard.TopRight.Corners[3];
			BottomLeftCorner = PhysicBoard.BottomLeft.Corners[1];

			float boardWidth = TopRightCorner.X - TopLeftCorner.X;
			float boardHeight = BottomLeftCorner.Y - TopLeftCorner.Y;

			float modulX = (boardWidth / Cols);
			float modulY = boardHeight / Rows;

			float markerRelativeX = m.Corners[0].X + 10 - TopLeftCorner.X;
			float markerRelativeY = m.Corners[0].Y + 10 - TopLeftCorner.Y;
			int remainder;

			//Delogger.Log("markerRelativeX", markerRelativeX);
			//Delogger.Log("markerRelativeY", markerRelativeY);


			m.Col = 1 + (Math.DivRem((int)markerRelativeX, (int)modulX, out remainder));// Cols - 
			m.Row = 1 + (Math.DivRem((int)markerRelativeY, (int)modulY, out remainder));  // Rows - 

		}

		public Marker GetMarkerById(int id)
		{
			Marker m = null;
			foreach (Marker marker in Markers)
			{

				//Debug.Log(marker.Id);

				if (marker.Id == id)
					m = marker;
			}

			return m;
		}

		public void ShowHideViewer()
		{
			if (Viewer.gameObject.activeSelf)
				Viewer.gameObject.SetActive(false);
			else
				Viewer.gameObject.SetActive(true);
		}

		public void ShowHideDebugger()
		{
			if (Debugger.gameObject.activeSelf)
				Debugger.gameObject.SetActive(false);
			else
				Debugger.gameObject.SetActive(true);
		}

	}


	public class Marker
	{
		public int Id;
		public string Type;
		public Point2f[] Corners;
		public float Col;
		public float Row;


	}

	public class PhysicBoard
	{
		public Marker TopLeft;
		public Marker TopRight;
		public Marker BottomLeft;
		public Marker BottomRight;

	}

	/**
	 * Manage the Texture2D queue destroying the unused Objects
	*/
	public static class TempTexturesManager
	{
		static int tempNumber = 40;
		static List<UnityEngine.Object> tempObjects;
		public static void OnDisable()
		{
			Clear();
		}

		public static void Clear()
		{

			// Destroy all temp objects in the manager
			for (int i = 0; i < tempObjects.Count; i++)
			{
				if (tempObjects[i] == null)
					continue;

				UnityEngine.Object.Destroy(tempObjects[i]);
			}

			tempObjects.Clear(); // clear the list

			DOVirtual.DelayedCall(1f, () =>
			{
				Resources.UnloadUnusedAssets();
			});

			DOVirtual.DelayedCall(1.6f, () =>
			{
				GC.Collect();
			});
		}

		public static void AddToTempList(UnityEngine.Object obj)
		{
			if (obj == null) return;

			if (tempObjects.Contains(obj)) return;

			tempObjects.Add(obj); // add to list

			if (tempObjects.Count >= tempNumber)
			{
				Clear();
			}

		}

		public static void AddToQueue(UnityEngine.Object obj)
		{
			if (tempObjects == null)
				tempObjects = new List<UnityEngine.Object>();
			AddToTempList(obj);
		}

		public static Texture2D CreateTexture2D(int width, int height)
		{
			Texture2D tex = new Texture2D(width, height);
			tex.hideFlags = HideFlags.HideAndDontSave;
			if (tempObjects == null)
				tempObjects = new List<UnityEngine.Object>();
			AddToTempList(tex);
			return tex;
		}

		public static Texture2D rotateTexture(Texture2D originalTexture, bool clockwise)
		{
			Color32[] original = originalTexture.GetPixels32();
			Color32[] rotated = new Color32[original.Length];
			int w = originalTexture.width;
			int h = originalTexture.height;

			int iRotated, iOriginal;

			for (int j = 0; j < h; ++j)
			{
				for (int i = 0; i < w; ++i)
				{
					iRotated = (i + 1) * h - j - 1;
					iOriginal = clockwise ? original.Length - 1 - (j * w + i) : j * w + i;
					rotated[iRotated] = original[iOriginal];
				}
			}

			Texture2D rotatedTexture = new Texture2D(h, w);
			rotatedTexture.SetPixels32(rotated);
			rotatedTexture.Apply();
			return rotatedTexture;
		}
	}

}