using DG.DeAudio;
using DG.Tweening;
using TMPro;
using UnityEngine;
using UnityEngine.UI;
using UnityEngine.EventSystems;

namespace FourT
{
    public class UIButtonDefault : Button, IPointerDownHandler
    {
        public bool LightImpact;
        public DeAudioClipData CustomSound;

        public bool RarelyClick;
        bool clickedOnce;
        Button button;
        float lastClickedOn;


        protected override void DoStateTransition(SelectionState state, bool instant)
        {
            if (clickedOnce && RarelyClick && Time.time - lastClickedOn < 5)
            {
                return;
            }

            base.DoStateTransition(state, instant);

            if (state == SelectionState.Disabled)
            {
                TextMeshProUGUI text = GetComponentInChildren<TextMeshProUGUI>();
                if (text != null)
                    text.alpha = 0.3f;
            }
            else if (state == SelectionState.Normal)
            {
                TextMeshProUGUI text = GetComponentInChildren<TextMeshProUGUI>();
                if (text != null)
                    text.alpha = 1;
            }

            else if (state == SelectionState.Pressed)
            {
                DOTween.Kill("ButtonPressed");
                button.transform.localScale = new Vector3(1, 1, 1);

                button.transform.DOPunchScale(new Vector3(-0.05f, -0.05f), 0.2f, 1, 0.5f).SetId("ButtonPressed");
            }
        }

        new void Start()
        {
            base.Start();

            if (FourTManager.I() == null)
                return;

            button = GetComponent<Button>();
            //todo a simple click
            //var click = ;

            button.onClick.AddListener(() =>
            {
                if (clickedOnce && RarelyClick && Time.time - lastClickedOn < 5)
                {
                    return;
                }

                clickedOnce = true;

                lastClickedOn = Time.time;
                /*if (LightImpact)
                    PlatformHapticManager.I.TriggerImpactMedium();
                else
                    PlatformHapticManager.I.TriggerImpactHeavy();*/

                if (CustomSound.clip != null)
                {
                    CustomSound.Play();
                }
                else
                {
                    //click.Play();
                }
            });

        }
    }
}
