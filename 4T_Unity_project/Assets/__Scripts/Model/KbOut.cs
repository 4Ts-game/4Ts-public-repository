using UnityEngine;
using System.Collections.Generic;
using System.Xml;
using System.Xml.Serialization;
using System.IO;
using System;
using System.Runtime.Serialization.Formatters.Binary;
using System.Xml.Linq;
using System.Xml.XPath;
using DG.Debugging;

/*
<!ELEMENT kb-response (inconsistent-slots, missing-cards, suggested-cards)>

<!ELEMENT inconsistent-slots position*, rationale?>

<!ELEMENT missing-cards cards>

<!ELEMENT suggested-cards cards>

<!ELEMENT cards ((ground-card | alternative)*, rationale?)>
<!ELEMENT ground-card ((technique-card | task-card | team-card | technology-card), position?)>
<!ELEMENT technique-card (technique-name, technique-phase)>
<!ELEMENT technique-name (#PCDATA)>
<!ELEMENT technique-phase (#PCDATA)>
<!ELEMENT task-card (#PCDATA)>
<!ELEMENT team-card (#PCDATA)>
<!ELEMENT technology-card (#PCDATA)>
<!ELEMENT alternative ((technique-card | task-card | team-card | technology-card)*, position?)>
<!ELEMENT position (#PCDATA)>
<!ELEMENT rationale (#PCDATA)>       
*/

namespace FourT
{
    public class KbOut
    {
        public List<string> InconsistentPositions;
        public List<string> MissingCards;
        public List<string> Alternatives;
        public static bool Error = false;

        public static KbOut Load(string answer)
        {
//            Delogger.Log("KbOut Load", answer);
            /*TextAsset asset = Resources.Load("Data/" + sample) as TextAsset;
            Stream s = new MemoryStream(asset.bytes);*/
            XElement kbAnswer = XElement.Parse(answer);

            KbOut kbOut = new KbOut();
            kbOut.InconsistentPositions = new List<string>();
            kbOut.MissingCards = new List<string>();
            kbOut.Alternatives = new List<string>();

            var children = kbAnswer.Elements();
            foreach (var xElement in children)
            {
                switch (xElement.Name.ToString())
                {
                    case "inconsistent-slots":
                        {
                            var pos = xElement.Elements();
                            foreach (var ps in pos)
                            {
                                kbOut.InconsistentPositions.Add(ps.Value);
                            }
                        }
                        break;

                    case "inconsistent-details":
                        var errorCode = xElement.Element("code");

                        if (errorCode != null && errorCode.Value != "")
                        {

                            Debug.Log("errorCode.Value:   " + errorCode.Value);

                            ErrorMessage errorMessage = AlertManager.I.GetErrorMessageById(errorCode.Value);

                            if (errorMessage == null)
                            {
                                string id = "0";
                                errorMessage = AlertManager.I.GetErrorMessageById(id);

                            }

                            if (errorMessage != null)
                                AlertManager.I.ShowAlert(errorMessage.Message);

                            Error = true;

                        }
                        else {
                            AlertManager.I.HideAlert();
                            Error = false;
                        }


                        break;

                    case "missing-cards":
                        {
                            var cards = xElement.Elements();
                            if (cards != null)
                            {
                                foreach (var card in cards)
                                {

                                    var pos = card.Element("slot");
                                    kbOut.MissingCards.Add(pos.Value);

                                    Debug.Log(pos.Value);
                                }
                            }

                            /*
                        var cards = xElement.Element("cards");
                        if (cards != null)
                        {
                            var alts = cards.Elements();
                            foreach (var alt in alts)
                            {
                                kbOut.MissingCards.Add(alt.Value);
                            }
                        }
                        */
                        }
                        break;

                    case "suggested-cards":
                        {
                            var pos = xElement.Elements();
                            foreach (var ps in pos)
                            {
                                kbOut.Alternatives.Add(ps.Value);
                            }
                        }
                        break;

                        //todo: Add care error message
                    case "error-message":
                        {
                            //todo get erorror Message ID and  show popup
                        }
                        break;

                }
            }

            //Delogger.Log("kbOut", kbOut);

            return kbOut;
        }

        public struct SuggestedAlternative
        {
            public string Position;

            public string Name;

            //type, desc
            public List<KbCard> Alternatives;
        }

        public struct KbCard
        {
            public string Type;

            public string Feature;

            //0 undefined
            public int Phase;

            public static KbCard GetKbCard(XElement ser)
            {
                KbCard kbc = new KbCard();

                string type;
                if (ser.FirstNode != null)
                {
                    type = ser.FirstNode.Parent.Name.ToString();
                }
                else
                {
                    type = ser.Name.ToString();
                }

                kbc.Type = type;

                /*if (ser.NodeType != XmlNodeType.Text)
                {
                    var xElem = XElement.Load(ser.FirstNode.CreateNavigator().ReadSubtree());
                    kbc.Type = xElem.Name.ToString();
                }
                else
                {
                    kbc.Type = ser.Name.ToString();
                }*/

                if (ser.Element("technique-phase") != null)
                {
                    kbc.Type = "technique-card"; //ser.Element("technique-phase").FirstNode.Parent.Name.ToString();
                    kbc.Phase = int.Parse(ser.Element("technique-phase").Value);
                    kbc.Feature = ser.Element("technique-name").Value;
                }
                else
                {
                    kbc.Feature = ser.Value;
                }

                Delogger.Log("kbc.Type", kbc.Type);
                return kbc;
            }
        }

    }
}