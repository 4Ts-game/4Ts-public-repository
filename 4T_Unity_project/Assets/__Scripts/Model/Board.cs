using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Xml.Linq;
using DG.Debugging;
using FourT;
using Newtonsoft.Json;
using UnityEngine;

/*
<!ELEMENT board (context, goals, content, columns)>
<!ELEMENT context (#PCDATA)>
<!ELEMENT goals (goal*)>
<!ELEMENT goal (#PCDATA)>
<!ELEMENT content (#PCDATA)>
<!ELEMENT columns (column+)>
<!ELEMENT column ((csw-technique, aba-css-task, aba-css-team, aba-css-tec1, aba-css-tec2, 
                                  abb-css-task, abb-css-team, abb-css-tec1, abb-css-tec2) | EMPTY)> 
<!ELEMENT csw-technique (call-for-suggestion | (technique-name, technique-phase))>
<!ELEMENT technique-name (#PCDATA)>
<!ELEMENT technique-phase (#PCDATA)>
<!ELEMENT aba-css-task (call-for-suggestion | #PCDATA | EMPTY)>
<!ELEMENT aba-css-team (call-for-suggestion | #PCDATA | EMPTY)>
<!ELEMENT aba-css-tec1 (call-for-suggestion | #PCDATA | EMPTY)>
<!ELEMENT aba-css-tec2 (call-for-suggestion | #PCDATA | EMPTY)>
<!ELEMENT abb-css-task (call-for-suggestion | #PCDATA | EMPTY)>
<!ELEMENT abb-css-team (call-for-suggestion | #PCDATA | EMPTY)>
<!ELEMENT abb-css-tec1 (call-for-suggestion | #PCDATA | EMPTY)>
<!ELEMENT abb-css-tec2 (call-for-suggestion | #PCDATA | EMPTY)>
<!ELEMENT call-for-suggestion EMPTY>
<!ATTLIST column id CDATA #REQUIRED>
<!ATTLIST column hasStar (true|false) "false">
 */

namespace FourT
{
    public class Board
    {

        public bool HasConnection = true;
        public string Name;

        public string SheetId;
        public string SheetErrorTabId;

        public int Level;
        public int GameType;

        public string Context;
        public string Goals;
        public string Content;

        public List<ErrorMessage> ErrorMessageList = new List<ErrorMessage>();

        public Dictionary<string, int> LocationAndCard;
        //public List<Card> Cards;

        public void Setup()
        {
            LocationAndCard = new Dictionary<string, int>();
        }

        public class Column
        {
            public int ID;
            public Card Technique;
            public int Phase;

            public Cycle Technique1;
            public Cycle Technique2;

            public Column(int id)
            {
                ID = id;
                Technique1 = new Cycle();
                Technique2 = new Cycle();
            }

            public List<Card> CardsPlayed()
            {
                List<Card> cardsOfType = new List<Card>();
                if (Technique != null)
                    cardsOfType.Add(Technique);

                cardsOfType.AddRange(Technique1.CardsPlayed());
                cardsOfType.AddRange(Technique2.CardsPlayed());
                return cardsOfType;
            }

        }

        public class Cycle
        {
            public Card Task1;
            public Card Team1;
            public Card Tech1;
            public Card Tech2;

            public List<Card> CardsPlayed()
            {
                List<Card> cardsOfType = new List<Card>();
                if (Task1 != null)
                    cardsOfType.Add(Task1);
                if (Team1 != null)
                    cardsOfType.Add(Team1);
                if (Tech1 != null)
                    cardsOfType.Add(Tech1);
                if (Tech2 != null)
                    cardsOfType.Add(Tech2);
                return cardsOfType;
            }

        }

        public List<int> CardsPlayed()
        {
            return LocationAndCard.Values.ToList();
        }

        // Returns the first ID of the card with same title available, and the number of not in play occurrences
        public Dictionary<int, int> CardsAvailable(Card.CardType type)
        {
            var cardsByType = Card.CardsByType(type);
            var idOfCardsPlayed = CardsPlayed();

            Dictionary<KeyValuePair<string, int>, int> cardsAndOccurrences = new Dictionary<KeyValuePair<string, int>, int>();

            foreach (var cardId in cardsByType)
            {
                if (!idOfCardsPlayed.Contains(cardId))
                {
                    Card c = Card.CardByID(cardId.ToString());

                    //does dict contain this title?
                    KeyValuePair<string, int> containsTitle = new KeyValuePair<string, int>(null, 0);

                    foreach (var cardsAndOccurrence in cardsAndOccurrences.Keys)
                    {
                        if (cardsAndOccurrence.Key == c.Title)
                        {
                            containsTitle = cardsAndOccurrence;
                        }
                    }

                    if (containsTitle.Key != null)
                    {
                        cardsAndOccurrences[containsTitle] = cardsAndOccurrences[containsTitle] + 1;
                    }
                    else
                    {
                        cardsAndOccurrences[new KeyValuePair<string, int>(c.Title, c.ID)] = 1;
                    }
                }
            }

            Dictionary<int, int> available = new Dictionary<int, int>();
            foreach (KeyValuePair<string, int> cardsAndOccurrence in cardsAndOccurrences.Keys)
            {
                available[cardsAndOccurrence.Value] = cardsAndOccurrences[cardsAndOccurrence];
            }

            return available;
        }

        public void PlayCard(int column, Card card, Card.Location l)
        { }

        public void RemoveCard(int column, Card card, Card.Location l)
        { }

        public ErrorMessage GeterrorMessageByID(string id)
        {
            ErrorMessage errorMessage = null;

            foreach(ErrorMessage e in FourTManager.I().Game.ErrorMessageList)
                {
                    if (e.ID == id)
                        errorMessage = e;
                }

                return errorMessage;
            }

    }
}
