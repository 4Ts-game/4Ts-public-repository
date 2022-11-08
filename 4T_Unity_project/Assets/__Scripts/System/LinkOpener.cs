using DG.Debugging;
using TMPro;
using UnityEngine;
using UnityEngine.EventSystems;

namespace SE
{
    public class LinkOpener : MonoBehaviour, IPointerClickHandler
    {
        public Camera Camera;

        public void OnPointerClick(PointerEventData eventData)
        {
            TMP_Text pTextMeshPro = GetComponent<TMP_Text>();
            int linkIndex = TMP_TextUtilities.FindIntersectingLink(pTextMeshPro, eventData.position, Camera);  
            if (linkIndex != -1)
            { // was a link clicked?
                TMP_LinkInfo linkInfo = pTextMeshPro.textInfo.linkInfo[linkIndex];

                Delogger.Log("LINK", linkInfo.GetLinkID());

                Application.OpenURL(linkInfo.GetLinkID());
            }
        }

    }
}