using System.Collections;
using System.Collections.Generic;
using TMPro;
using UnityEngine;

namespace OL
{
    public abstract class DebugManager : MonoBehaviour
    {
        public Setuppable SetupState;

        public Canvas DebugCanvas;
        public RectTransform DebugPanel;
        protected TextMeshProUGUI DebugText;

        void Start()
        {
            SetupState = Setuppable.NewInstance();
            if (SimpleGameManager.InjectedReferrableInstance.Production)
            {
                DebugCanvas.gameObject.SetActive(false);
            }
            else
            {
                DebugText = DebugPanel.GetComponentInChildren<TextMeshProUGUI>();
            }

            OnStart();

        }

        protected abstract void OnStart();

        void Update()
        {
            if (SetupState.IsToBeSetupped())
            {
                if (SimpleGameManager.InjectedReferrableInstance.Production)
                    DebugCanvas.gameObject.SetActive(false);
                SetupState.SetupCompleted();
            }            
        }

        public void ChangeTimeScale()
        {
            if (SimpleGameManager.InjectedReferrableInstance.Production || !DebugPanel.gameObject.activeSelf)
            {
                return;
            }

            if (Input.GetKeyDown(KeyCode.Alpha1))
            {
                Time.timeScale = .1f;
            }
            else if (Input.GetKeyDown(KeyCode.Alpha2))
            {
                Time.timeScale = .5f;
            }
            else if (Input.GetKeyDown(KeyCode.Alpha3))
            {
                Time.timeScale = 1f;
            }
            else if (Input.GetKeyDown(KeyCode.Alpha4))
            {
                Time.timeScale = 2f;
            }
            else if (Input.GetKeyDown(KeyCode.Alpha5))
            {
                Time.timeScale = 5f;
            }
        }

        public void OpenCloseDebug()
        {
            DebugPanel.gameObject.SetActive(!DebugPanel.gameObject.activeSelf);
        }

        
        
        /*void InfoWindow(int id)
        {
            var style = new GUIStyle("label");
            style.fontSize = 40;
            GUILayout.Label("TimeScale " + Time.timeScale + " (press 1:.1 2:.5 3:1 4:2 5:5)", style);
            GUILayout.Label("FrameworkDebug " + GameManager.InjectedReferrableInstance.FrameworkDebug + " (press 6 7)", style);
            GUILayout.Label("(F1 to close)", style);
        }


        public void TestTranslate()
        {
            InputField Translate = GameObject.Find("DebugTranslate").GetComponent<InputField>();
            TextMeshProUGUI results = GameObject.Find("DebugResults").GetComponent<TextMeshProUGUI>();
            results.text = I18n.T(Translate.text);
        }*/
    }
}

