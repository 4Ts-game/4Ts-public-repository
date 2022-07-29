using System;
using System.Collections;
using System.Collections.Generic;
using DG.Debugging;
using FourT;
using UnityEngine;

namespace FourT
{
    public class FourTConfiguration : ScriptableObject
    {

        public string KnowledgeBaseVersion;
        [Space]
        public string SheetID;
        public string SheetTabID;

        public bool ManageFiles;
        [Space]
        public Color TechniqueColor;
        public Color TeamColor;
        public Color TechnologyColor;
        public Color TaskColor;
        public Color WildcardColor;

        [Space]
        public Sprite TechniqueIcon;
        public Sprite TeamIcon;
        public Sprite TechnologyIcon;
        public Sprite TaskIcon;
        public Sprite WildcardIcon;

        public Color GetColorByCardType(Card.CardType type)
        {
            Color Col = TechniqueColor;
            switch (type)
            {
                case Card.CardType.Technique:
                    Col = this.TechniqueColor;
                    break;
                case Card.CardType.Task:
                    Col = this.TaskColor;
                    break;
                case Card.CardType.Team:
                    Col = this.TeamColor;
                    break;
                case Card.CardType.Technology:
                    Col = this.TechnologyColor;
                    break;
                default:
                    throw new Exception("Missing color");

            }
            return Col;

        }

        public Sprite GetIconByCardType(Card.CardType type)
        {

            Sprite Img = TechniqueIcon;
            switch (type)
            {
                case Card.CardType.Technique:
                    Img = this.TechniqueIcon;
                    break;
                case Card.CardType.Task:
                    Img = this.TaskIcon;
                    break;
                case Card.CardType.Team:
                    Img = this.TeamIcon;
                    break;
                case Card.CardType.Technology:
                    Img = this.TechnologyIcon;
                    break;
                default:
                    throw new Exception("Missing Icon");

            }
            return Img;

        }

    }
}
