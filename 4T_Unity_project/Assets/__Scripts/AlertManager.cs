using System;
using System.Collections;
using System.Collections.Generic;
using TMPro;
using UnityEngine;
using UnityEngine.UI;

namespace FourT
{
    public class AlertManager : MonoBehaviour
    {
        public static AlertManager I;

        public Transform AlertElement;
        public TMP_Text AlertTextPlaceHolder;

        public Transform OKButton;
        public Transform CancelButton;

        public AudioClip ErrorAudio;

        void Awake()
        {
            I = this;
        }

        // Start is called before the first frame update
        void Start()
        {
            CancelButton.gameObject.SetActive(false);

        }

        // Update is called once per frame
        void Update()
        {

        }

        float lastErrorPlayedOn;

        public void ShowAlert(string text, Action callback = null, Action cancelCallback = null, bool allowCancel = true, string cancelLabel = "Cancel", string okLabel = "Ok")
        {

            //if (FourTMarkersManager.I != null && FourTMarkersManager.I.HybridIsActive)
            //    FourTMarkersManager.I.Reset();

            if (Time.time - lastErrorPlayedOn > 10)
            {
                lastErrorPlayedOn = Time.time;

                AudioSource source = GetComponent<AudioSource>();
                source.clip = ErrorAudio;
                source.Play();
            }

            AlertElement.gameObject.SetActive(true);

            AlertTextPlaceHolder.text = text;

            OKButton.GetComponentInChildren<TMP_Text>().text = okLabel;
            CancelButton.GetComponentInChildren<TMP_Text>().text = cancelLabel;

            if(callback != null)
            {
                if (allowCancel)
                {
                    CancelButton.gameObject.SetActive(true);
                    CancelButton.GetComponent<Button>().onClick.AddListener(() => {

                        if (cancelCallback != null)
                            cancelCallback();

                        HideAlert();
                    });
                }

                OKButton.GetComponent<Button>().onClick.AddListener(() => {
                    callback();
                    HideAlert();
                });
            } else
            {
                OKButton.GetComponent<Button>().onClick.AddListener(() => {
                    HideAlert();
                });
            }

            if (FourTMarkersManager.I != null && FourTMarkersManager.I.HybridIsActive)
            {
                OKButton.gameObject.SetActive(false);
            } else
            {
                OKButton.gameObject.SetActive(true);
            }

        }

        public void HideAlert()
        {
            if (AlertElement.gameObject.activeSelf)
            {
                if (FourTMarkersManager.I != null && FourTMarkersManager.I.HybridIsActive)
                    FourTMarkersManager.I.Reset();

                AlertTextPlaceHolder.text = "";
                AlertElement.gameObject.SetActive(false);
            }

        }

        public ErrorMessage GetErrorMessageById(string id)
        {
            ErrorMessage error = null;

            Debug.Log("Number of errors:  " + FourTManager.I().Game.ErrorMessageList.Count);

            foreach(ErrorMessage e in FourTManager.I().Game.ErrorMessageList)
            {
                Debug.Log("Error:: " + e.ID + "     " + e.Message);
                
                if (e.ID == id)
                    error = e;
            }

            return error;
        }


    }

    public class ErrorMessage
    {
        public string ID;
        public string Message;
    }

}