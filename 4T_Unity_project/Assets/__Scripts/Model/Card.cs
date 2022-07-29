using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using DG.Debugging;
using OL;
using UnityEngine;

namespace FourT
{
    public class Card
    {
        public int ID;
        public CardType Type;
        public string Title;
        public string Description;
        public string InclusionTips;
        public bool IsJolly;
        //public string Indications;

        public string BoardLocation;

        public Dictionary<string, string> TableIndications;
        public static Dictionary<int, Card> Cards = new Dictionary<int, Card>();

        public enum CardType
        {
            Technique,
            Team,
            Technology,
            Task,
            Action,
            Wildcard,
            QuestionMark
        }

        public enum Location
        {
            CSW_TECHNIQUE,

            ABA_CSS_TASK,
            ABA_CSS_TEAM,
            ABA_CSS_TEC1,
            ABA_CSS_TEC2,

            ABB_CSS_TASK,
            ABB_CSS_TEAM,
            ABB_CSS_TEC1,
            ABB_CSS_TEC2,

        }

        public static string ColumnAndLocationToString(int column, Location l)
        {
            return $"C{column}_{l.ToString().ToUpper().Replace("_", "-")}";
        }

        public static Location GetLocationFromName(string name)
        {
            return (Location)Enum.Parse(typeof(Location), name.ToUpper().Replace("-", "_"));
        }


        public string CardContentByPosition(string positionNoColumn)
        {
            Location l = (Location)Enum.Parse(typeof(Location), positionNoColumn.ToUpper().Replace("-", "_"));
            return CardContentByPosition(l);
        }

        public string CardContentByPosition(Location positionNoColumn)
        {
            switch (positionNoColumn)
            {
                case Location.CSW_TECHNIQUE:
                    return Title;

                //ABA
                case Location.ABA_CSS_TASK:
                    if (TableIndications.ContainsKey("Task1"))
                        return TableIndications["Task1"];
                    else
                        return "";

                case Location.ABA_CSS_TEAM:
                    if (TableIndications.ContainsKey("Team1"))
                        return TableIndications["Team1"];
                    else
                        return "";

                case Location.ABA_CSS_TEC1:
                    if (TableIndications.ContainsKey("Technology1"))
                        return TableIndications["Technology1"];
                    else
                        return "";

                case Location.ABA_CSS_TEC2:
                    if (TableIndications.ContainsKey("Technology1"))
                        return TableIndications["Technology1"];
                    else
                        return "";


                //ABB
                case Location.ABB_CSS_TASK:
                    if (TableIndications.ContainsKey("Task2"))
                        return TableIndications["Task2"];
                    else
                        return "";

                case Location.ABB_CSS_TEAM:
                    if (TableIndications.ContainsKey("Team2"))
                        return TableIndications["Team2"];
                    else
                        return "";

                case Location.ABB_CSS_TEC1:
                    if (TableIndications.ContainsKey("Technology1"))
                        return TableIndications["Technology1"];
                    else
                        return "";

                case Location.ABB_CSS_TEC2:
                    if (TableIndications.ContainsKey("Technology1"))
                        return TableIndications["Technology1"];
                    else
                        return "";
            }

            throw new Exception("Position unknown " + positionNoColumn);
        }

        public static List<int> CardsByType(CardType type)
        {

            List<int> cardsOfType = new List<int>();
            foreach (var card in Cards.Values)
            {
                bool OmitJollyCards = card.IsJolly && FourTManager.I().Game.Level != 3;

                if (card.Type != type || OmitJollyCards)
                    continue;

                cardsOfType.Add(card.ID);
            }

            return cardsOfType;
        }


        public static Card CardByID(string ID)
        {
            foreach (var card in Cards.Values)
            {
                if (card.ID.ToString() == ID)
                    return card;
            }

            Debug.Log($"No card with ID {ID}");
            return null;
        }

        public static List<Card> CardsAvailableByTitle(string title)
        {
            List<Card> cards = new List<Card>();
            var idOfCardsPlayed = FourTManager.I().Game.CardsPlayed();

            foreach (var card in Cards.Values)
            {
                bool OmitJollyCards = card.IsJolly && FourTManager.I().Game.Level != 3;

                if (card.Title == title && !idOfCardsPlayed.Contains(card.ID) && !OmitJollyCards)
                {
                    Delogger.Log("Card Available:", card.ID);
                    cards.Add(card);
                }
            }
            return cards;
        }

        public static void FillUpErrorMessages(string csv)
        {
            List<List<string>> rawData = TT.ParseCSV(csv);
            FourTManager.I().Game.ErrorMessageList = new List<ErrorMessage>();

            foreach (var alist in rawData)
            {
                ErrorMessage errorMessage = new ErrorMessage();

                errorMessage.ID = alist[0];
                errorMessage.Message = alist[1];

                Debug.Log("Error ID: " + errorMessage.ID);
                Debug.Log("Error Message: " + errorMessage.Message);

                FourTManager.I().Game.ErrorMessageList.Add(errorMessage);

            }
        }


        public static void FillUpCards(string csv)
        {

            List<List<string>> rawData = TT.ParseCSV(csv);

            Card lastTypeCreated = null;


            //Type
            //Title
            //Description -- to be cutoff at a certain point towards the back of card
            //Inclusion_tips

            // No more ----------------------------------------------
            //Indications (plain text) (for non-technique cards)
            //Table Indications Task1
            //Table Indications Team1
            //Table indications Technology1
            //Table indications Time1
            //Table Indications Task2
            //Table Indications Team2

            //Table indications Technology2
            //Table indications Time2
            //Table Indications Task3
            //Table Indications Team3
            //Table indications Technology3
            //Table indications Time3
            // End No more ----------------------------------------------


            //Card 1 ID Card 2 ID   Card 3 ID   Card 4 ID   Card 5 ID   Card 6 ID
            //Card 7 ID Card 8 ID   Card 9 ID   Card 10 ID  Card 11 ID

            int xx = 0;
            foreach (var alist in rawData)
            {
                if (alist.Count < 3)
                    continue;


                List<int> ids = new List<int>();

                int i = 0;

                //for (int ii = 0; ii < 12; ii++)
                //{
                //    Delogger.Log($"Valori {xx} ->", alist[ii]);
                //}

                xx++;
                var guid = Guid.NewGuid();
                var type = alist[i];
                var title = alist[++i];


                if (TT.IsNullOrWhitespace(type.Trim())) //|| TT.IsNullOrWhitespace(title.Trim())
                    continue;

                /* todo: remove */
                if (type == "Wildcard")
                    continue;

                if (type == "Question Mark")
                    continue;

                if (type == "Action")
                    continue;

                //Delogger.Log("CARD", title.Trim());

                var description = alist[++i];
                //var indications = alist[++i];

                var inclusionTips = alist[++i];

                /* Table is no more displayed
                var indications = alist[++i];

                //table indications
                var Task1 = alist[++i];
                var Team1 = alist[++i];
                var Technology1 = alist[++i];
                var Time1 = alist[++i];

                var Task2 = alist[++i];
                var Team2 = alist[++i];
                var Technology2 = alist[++i];
                var Time2 = alist[++i];

                var Task3 = alist[++i];
                var Team3 = alist[++i];
                var Technology3 = alist[++i];
                var Time3 = alist[++i];
                */

                var Card1 = alist[++i];
                var Card2 = alist[++i];
                var Card3 = alist[++i];
                var Card4 = alist[++i];
                var Card5 = alist[++i];
                var Card6 = alist[++i];
                var Card7 = alist[++i];
                var Card8 = alist[++i];
                var Card9 = alist[++i];
                var Card10 = alist[++i];
                var Card11 = alist[++i];

                AddId(Card1, ids);
                AddId(Card2, ids);
                AddId(Card3, ids);
                AddId(Card4, ids);
                AddId(Card5, ids);
                AddId(Card6, ids);
                AddId(Card7, ids);
                AddId(Card8, ids);
                AddId(Card9, ids);
                AddId(Card10, ids);
                AddId(Card11, ids);

                if (ids.Count == 0)
                    continue;

                CardType ty = (CardType)Enum.Parse(typeof(CardType), type);

                foreach (var id in ids)
                {

                    bool isJolly = String.IsNullOrEmpty(title);

                    var t = new Card
                    {
                        ID = id,
                        Title = title,
                        Type = ty,
                        Description = description,
                        InclusionTips = inclusionTips,
                        IsJolly = isJolly
                        //Indications = indications
                    };

                    /* Table is no more displayed
                    t.TableIndications = new Dictionary<string, string>();
                    t.TableIndications["Task1"] = !TT.IsNullOrWhitespace(Task1.Trim()) ? Task1.Trim() : "";
                    t.TableIndications["Team1"] = !TT.IsNullOrWhitespace(Team1.Trim()) ? Team1.Trim() : "";
                    t.TableIndications["Technology1"] = !TT.IsNullOrWhitespace(Technology1.Trim()) ? Technology1.Trim() : "";
                    t.TableIndications["Time1"] = !TT.IsNullOrWhitespace(Time1.Trim()) ? Time1.Trim() : "";
                    t.TableIndications["Task2"] = !TT.IsNullOrWhitespace(Task2.Trim()) ? Task2.Trim() : "";
                    t.TableIndications["Team2"] = !TT.IsNullOrWhitespace(Team2.Trim()) ? Team2.Trim() : "";
                    t.TableIndications["Technology2"] = !TT.IsNullOrWhitespace(Technology2.Trim()) ? Technology2.Trim() : "";
                    t.TableIndications["Time2"] = !TT.IsNullOrWhitespace(Time2.Trim()) ? Time2.Trim() : "";
                    t.TableIndications["Task3"] = !TT.IsNullOrWhitespace(Task3.Trim()) ? Task3.Trim() : "";
                    t.TableIndications["Team3"] = !TT.IsNullOrWhitespace(Team3.Trim()) ? Team3.Trim() : "";
                    t.TableIndications["Technology3"] = !TT.IsNullOrWhitespace(Technology3.Trim()) ? Technology3.Trim() : "";
                    t.TableIndications["Time3"] = !TT.IsNullOrWhitespace(Time3.Trim()) ? Time3.Trim() : "";
                    */

                    Cards[t.ID] = t;

                    Delogger.Log("card", title + " type: " + ty + " is jolly: " + isJolly);

                }
            }

        }

        static void AddId(string Card, List<int> ids)
        {
            if (!TT.IsNullOrWhitespace(Card.Trim()))
            {
                ids.Add(int.Parse(Card));
            }
        }
    }
}
