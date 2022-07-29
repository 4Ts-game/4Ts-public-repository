using System.Collections.Generic;
using DG.DeAudio;
using DG.Debugging;
using DG.DeInspektor.Attributes;
using DG.Tweening;
using OL;
using TMPro;
using UnityEngine;
using UnityEngine.EventSystems;
using UnityEngine.SceneManagement;
using UnityEngine.UI;
using System.Linq;
using System;
using Newtonsoft.Json;
using System.Collections;

namespace FourT
{
    public class BoardManager : MonoBehaviour
    {

        [Space]
        public TMP_Text Version;

        [DeHeader("Sipario", "FFFFFFFF", "9F9F9FFF")]

        [SerializeField]
        [DeEmptyAlert]
        [DeToggleButton]
        RectTransform Sipario;

        [DeHeader("Board", "FFFFFFFF", "9F9F9FFF")]

        [SerializeField]
        [DeEmptyAlert]
        [DeToggleButton]
        RectTransform Board;

        [SerializeField]
        [DeEmptyAlert]
        [DeToggleButton]
        int ColumnsNumber;

        [SerializeField]
        [DeEmptyAlert]
        [DeToggleButton]
        RectTransform ColumnPrefaboard;

        [SerializeField]
        [DeEmptyAlert]
        [DeToggleButton]
        public Button CheckCompleteness;

        [DeHeader("Cards Full Layout", "FFFFFFFF", "9F9F9FFF")]

        [SerializeField]
        [DeEmptyAlert]
        Transform FullLayoutCardHorizontalPrefab;

        [SerializeField]
        [DeEmptyAlert]
        Transform FullLayoutCardVerticalPrefab;

        [DeHeader("Cards Board Layout", "FFFFFFFF", "9F9F9FFF")]

        [SerializeField]
        [DeEmptyAlert]
        Transform BoardLayoutCardHorizontalPrefab;

        [SerializeField]
        [DeEmptyAlert]
        Transform BoardLayoutCardVerticalPrefab;

        [DeHeader("Card Chooser", "FFFFFFFF", "9F9F9FFF")]

        [SerializeField]
        [DeEmptyAlert]
        Transform CardChooser;

        [SerializeField]
        [DeEmptyAlert]
        TextMeshProUGUI CardChooserTitle;

        [SerializeField]
        [DeEmptyAlert]
        public Button SuggestCardsButton;
        public Button CloseSuggestCardsButton;

        [SerializeField]
        [DeEmptyAlert]
        UICardRoller SliderCardContainert;

        [SerializeField]
        [DeEmptyAlert]
        CanvasGroup SuggestionsFeedback;

        [DeHeader("Card Info Panel", "FFFFFFFF", "9F9F9FFF")]

        [SerializeField]
        [DeEmptyAlert]
        Transform CardInfoPanel;

        [SerializeField]
        [DeEmptyAlert]
        Transform CardInfoHorizontal;

        [SerializeField]
        [DeEmptyAlert]
        Transform CardInfoVerticall;

        [DeHeader("Errors", "FFFFFFFF", "9F9F9FFF")]
        [SerializeField]
        [DeEmptyAlert]
        Transform ErrorPanel;

        [SerializeField]
        [DeEmptyAlert]
        public Transform ConfirmQuit;

        [SerializeField]
        [DeEmptyAlert]
        TMP_Text ConfirmQuitText;

        [SerializeField]
        [DeEmptyAlert]
        Button ConfirmQuitButton;

        [SerializeField]
        [DeEmptyAlert]
        Sprite Inconsistent;

        [SerializeField]
        [DeEmptyAlert]
        Sprite Missing;

        [SerializeField]
        [DeEmptyAlert]
        Sprite Suggested;


        [DeHeader("Quit", "FFFFFFFF", "9F9F9FFF")]
        [SerializeField]
        [DeEmptyAlert]
        [DeToggleButton]
        RectTransform QuitPanel;

        [DeHeader("Board Texts", "FFFFFFFF", "9F9F9FFF")]

        public TMP_InputField ContextContent;
        public TMP_InputField GoalsContent;
        public TMP_InputField ContentContent;

        public TMP_Text Level;
        public TMP_Text GameName;


        Setuppable BoardSetup;

        public int ActualColumn;
        public GameObject ActualPlace;
        public Transform ActualBoardCard;

        public Card ActualCard;

        public string LastPlayedCardBoardLocation;

        public Card.CardType ChooserType;

        public bool CanAddCard = true;
        public bool MissingTechnique = false;
        public bool ErrorOnBoard = false;

        List<GameObject> Columns;

        public static BoardManager I;

        public List<CardPlaceHolder> CardPlaceHolders;

        public void Start()
        {

            string version = $"App version {Application.version}<br>Build {SimpleGameManager.Build}<br>Knowledge Base version {FourTManager.I().Config.KnowledgeBaseVersion}";
            Version.text = version;

            Level.text = "Level " + FourTManager.I().Game.Level.ToString();
            GameName.text = FourTManager.I().Game.Name;

        }

        public void Awake()
        {
            BoardSetup = Setuppable.NewInstance();
            I = this;
        }

        public void Update()
        {


            if (FourTManager.NotReadyYet())
                return;


            if (BoardSetup.NotSetuppedNorSetupping())
            {
                BoardSetup.Setupping();

                /*****************************************************/
                // Draw the board columns
                /*****************************************************/
                BuildBoard();

                Sipario.GetComponent<Transform>().DOScale(2, 2f);
                Sipario.GetComponent<CanvasGroup>().DOFade(0f, 2f).OnComplete(() =>
                {
                    Sipario.gameObject.SetActive(false);

                    if (FourTManager.I().Game.GameType == 2 )
                    {
                        SettingsManager.I.OpenCloseSettingsPanel();
                        if (!FourTMarkersManager.I.HybridIsActive)
                            SettingsManager.I.TurnOnOffHybrid();
                    }

                    if (FourTMarkersManager.I != null && FourTMarkersManager.I.HybridIsActive)
                    {
                        CheckCompleteness.gameObject.SetActive(false);
                    }

                });

                Columns = TT.FindGameObjectsChildrenWithTag(Board.gameObject, "Column");

                /*****************************************************/
                // Loop each column to find header and cards
                /*****************************************************/
                int counter = 1;
                foreach (GameObject Col in Columns)
                {
                    /*****************************************************/
                    // Find the column header and change the label
                    /*****************************************************/
                    List<GameObject> CardPlaceHolders = TT.FindGameObjectsChildrenWithTag(Col.gameObject, "CardPlaceholder");

                    Transform ColHeaderText = GetColumnHeader(Col.transform);
                    ColHeaderText.GetComponent<TextMeshProUGUI>().text = "Week " + (counter);

                    /*****************************************************/
                    // Find each card in column and add events
                    /*****************************************************/
                    foreach (GameObject CardPlaceHolder in CardPlaceHolders)
                    {
                        if (CardPlaceHolder != null)
                        {
                            Transform row = CardPlaceHolder.transform.parent;
                            Transform col = row.transform.parent;

                            Delogger.Log("CardPlaceHolder", CardPlaceHolder.name);
                            Delogger.Log("Row", row.name);

                            CardPlaceHolder.GetComponent<CardPlaceHolder>().Row = int.Parse(row.name.Replace("Row_",""));
                            CardPlaceHolder.GetComponent<CardPlaceHolder>().DigitalCol = col;

                            if (FourTManager.I().Game.Level == 2 && CardPlaceHolder.GetComponent<CardPlaceHolder>().Row == 1)
                            {

                                CardPlaceHolder.transform.GetComponent<CanvasGroup>().alpha = 0f;
                                continue;

                            }

                            //CardPlaceHolder.GetComponent<CardPlaceHolder>().HasBeenDrawed = true;

                            CardPlaceHolder r = CardPlaceHolder.GetComponent<CardPlaceHolder>();

                            switch (col.name)
                            {
                                case "Column-2":
                                    r.Col = r.Col == 1 ? 3 : 4; 
                                    break;
                                case "Column-3":
                                    r.Col = r.Col == 1 ? 5 : 6; 
                                    break;
                                case "Column-4":
                                    r.Col = r.Col == 1 ? 7 : 8; 
                                    break;
                            }

                           // CardPlaceHolders.Add(CardPlaceHolder.GetComponent<CardPlaceHolder>());


                            Button CardPlaceHolderbutton = CardPlaceHolder.GetComponent<Button>();

                            CardPlaceHolderbutton.onClick.AddListener(() =>
                            {

                                if ( (FourTMarkersManager.I != null && FourTMarkersManager.I.HybridIsActive)) //!CanAddCard ||
                                    return;

                                SetLocationAndColumnFromCardPlaceHolder(CardPlaceHolder.transform, Col.transform);

                                Card.CardType CardType = GetcardTypeByPlaceHolderName(CardPlaceHolder.name);

                                Debug.Log("CardPlaceHolder.name = " + CardPlaceHolder.name);

                                ShowCardChooser(CardType, null);

                            });
                        }

                    }

                    counter++;
                }

                ResetAllErrors();
                DrawPersistedGame();

                BoardSetup.SetupCompleted();

            }

        }

        /*****************************************************/
        // ERROR MANAGER
        /*****************************************************/

        public bool CheckIfTechniqueExist(Board game)
        {
            if (FourTManager.I().Game.Level != 1)
                return true;

                List<int> cardsIds = game.CardsPlayed();

            foreach (var x in cardsIds)
            {

                if (x == 19)
                    continue;

                Card card = Card.CardByID(x.ToString());
                if (card.Type == Card.CardType.Technique)
                    return true;
            }

            return false;
        }

        public void DisplayErrors(List<string> inconsistentErrors = null, List<string> missingErrors = null)
        {
            ResetAllErrors();

            CanAddCard = true;
            ErrorOnBoard = false;

            if (inconsistentErrors != null)
                foreach (string inconsistentError in inconsistentErrors)
                {
                    SetError(inconsistentError, 0);
                    //Set as error also the last played card
                    if(!FourTMarkersManager.I.HybridIsActive)
                        SetError(LastPlayedCardBoardLocation, 0);
                }

            if (missingErrors != null)
                foreach (string missingError in missingErrors)
                    SetError(missingError, 1);

        }


        public void ResetAllErrors()
        {
            Columns = TT.FindGameObjectsChildrenWithTag(Board.gameObject, "Column");
            foreach (GameObject Col in Columns)
            {
                //Delogger.Log("Col Name ", Col.name);

                List<GameObject> CardPlaceHolders = TT.FindGameObjectsChildrenWithTag(Col.gameObject, "CardPlaceholder");
                foreach (GameObject CardPlaceHolder in CardPlaceHolders)
                {
                    Transform error = CardPlaceHolder.transform.Find("ErrorIcon");

                    if (error)
                        error.gameObject.SetActive(false);
                }
            }
        }

        private void SetError(string inconsistentError, int type = 0)
        {
            Delogger.Log("inconsistentError", inconsistentError);
            Delogger.Log("type", type);

            Columns = TT.FindGameObjectsChildrenWithTag(Board.gameObject, "Column");
            int ColIdx = int.Parse(inconsistentError.Split(char.Parse("-"))[0].Replace("C", ""));

            GameObject Col = Columns[ColIdx - 1];

            string PlaceHolderName = inconsistentError.Substring(3, inconsistentError.Length - 3).ToLower();

            Transform PlaceHolder = FindTransform(Col.transform, PlaceHolderName);
            Transform ErrIco = PlaceHolder.transform.Find("ErrorIcon");

            Sprite ErrSprite = null;

            switch (type)
            {
                case 0:
                    ErrSprite = Inconsistent;
                    break;
                case 1:
                    ErrSprite = Missing;
                    break;
                default:
                    ErrSprite = Inconsistent;
                    break;
            }
            if (ErrIco != null)
            {
                ErrIco.GetComponent<Image>().sprite = ErrSprite;
                ErrIco.GetComponent<Button>().onClick.AddListener(() =>
                {
                    ShowErrorInfo(type);
                });

                ErrIco.gameObject.SetActive(true);
            }

            /*
             * If there is an error you can't add a card
             */
            if(type == 0)
                ErrorOnBoard = true;

        }

        public static Transform FindTransform(Transform parent, string name)
        {
            if (parent.name.Equals(name)) return parent;
            foreach (Transform child in parent)
            {
                Transform result = FindTransform(child, name);
                if (result != null) return result;
            }
            return null;
        }

        public void ShowErrorInfo(int errType)
        {
            Transform ErrorBox = ErrorPanel.transform.Find("ErrorBox");
            Transform ErrorIcon = ErrorBox.transform.Find("ErrorImage");
            TextMeshProUGUI ErrorText = ErrorBox.transform.Find("ErrorDesc").GetComponent<TextMeshProUGUI>();

            Sprite ErrSprite = null;
            string ErrorTitle = "";
            switch (errType)
            {
                case 0:
                    ErrSprite = Inconsistent;
                    //ErrorTitle = "Inconsistent Slot"; rimosso per traduzione in italiano [AC191112]
                    ErrorTitle = "Inconsistent card";
                    break;
                case 1:
                    ErrSprite = Missing;
                    //ErrorTitle = "Missing Card"; rimosso per traduzione in italiano [AC191112]
                    ErrorTitle = "Missing cards";
                    break;
                case 2:
                    ErrSprite = Suggested;
                    ErrorTitle = "Suggested card";
                    break;
                case 3:
                    ErrSprite = Inconsistent;
                    ErrorTitle = "Before playing a new card <br> you have to solve the inconsistency problem.";
                    break;
                case 4:
                    ErrSprite = Missing;
                    ErrorTitle = "At least one Technique card need to be on the board.";
                    break;
                case 5:
                    ErrSprite = Missing;
                    ErrorTitle = "Are you sure you want to exit this game? There are missing cards.";
                    break;

                default:
                    ErrSprite = Inconsistent;
                    //ErrorTitle = "Inconsistent Slot"; rimosso per traduzione in italiano [AC191112]
                    ErrorTitle = "Inconsistent card";
                    break;
            }

            ErrorIcon.GetComponent<Image>().sprite = ErrSprite;
            ErrorText.text = ErrorTitle;

            ErrorPanel.gameObject.SetActive(true);
        }

        public void ShowConfirmQuit(Action callback, string text, string label)
        {
            ConfirmQuit.gameObject.SetActive(true);
            ConfirmQuitText.text = text;
            ConfirmQuitButton.GetComponentInChildren<TMP_Text>().text = label;
            ConfirmQuitButton.onClick.AddListener(() => {
                callback();
            });
        }

        public void HideConfirmQuit()
        {
            ConfirmQuit.gameObject.SetActive(false);

        }

        public void HideErrorInfo()
        {
            ErrorPanel.gameObject.SetActive(false);
        }


        /*****************************************************/
        // Show card chooser panel
        /*****************************************************/
        public void ShowCardChooser(Card.CardType Type, List<string> filterd_IDs, bool forceShow = false, bool showDisponibility = true)
        {

            MissingTechnique = !FourTMarkersManager.I.HybridIsActive && !CheckIfTechniqueExist(FourTManager.I().Game);

            if (FourTManager.I().Game.Level == 2 && Type == Card.CardType.Technique && !FourTMarkersManager.I.HybridIsActive)
                return;

            if (!CanAddCard && !forceShow)
                return;

            if (MissingTechnique && Type != Card.CardType.Technique && FourTManager.I().Game.Level == 1)
            {
                ShowErrorInfo(4);
                return;
            }

            if (ErrorOnBoard)
            {
                ShowErrorInfo(3);
                Delogger.Log("Error On Board", ErrorOnBoard);
                return;
            }

            var cardsAvailable = FourTManager.I().Game.CardsAvailable(Type);
            ChooserType = Type;
            CardChooserTitle.text = "Choose a card <b>" + Type.ToString() + " </b>";
            CardChooserTitle.color = FourTManager.I().Config.GetColorByCardType(Type) * 1.3f;
            CardChooser.Find("TypeIcon").GetComponent<Image>().sprite = FourTManager.I().Config.GetIconByCardType(Type);
            CardChooser.Find("TypeIcon").GetComponent<Image>().color = CardChooserTitle.color;

            CanvasGroup Cg = CardChooser.GetComponent<CanvasGroup>();
            Cg.alpha = 0;

            CardChooser.transform.localScale = new Vector3(1.2f, 1.2f, 0);

            if(FourTManager.I().Game.Level == 3)
            {
                SuggestCardsButton.interactable = false;
            }

            if (FourTMarkersManager.I.HybridIsActive)
            {
                SuggestCardsButton.gameObject.SetActive(false);
                CloseSuggestCardsButton.gameObject.SetActive(false);
            }

            CardChooser.gameObject.SetActive(true);
            Cg.DOFade(1f, .5f);

            foreach (Transform Child in SliderCardContainert.transform)
                Destroy(Child.gameObject);

            CardChooser.transform.DOScale(1f, .6f).SetEase(Ease.OutExpo).OnComplete(() =>
            {
                /*****************************************************/
                // Create cards
                /*****************************************************/
                Transform CardTemplate = (Type == Card.CardType.Technique) ? FullLayoutCardHorizontalPrefab : FullLayoutCardVerticalPrefab;
                SliderCardContainert.GetComponent<CanvasGroup>().alpha = 0;

                var FTM = FourTManager.I();

                /*****************************************************/
                // Suggested Cards
                // If an array of IDs is passed then display only those cards.
                /*****************************************************/
                Dictionary<int, int> cardsToDisplay = new Dictionary<int, int>();
                bool thereAreCardsToDisplay = false;
                if (filterd_IDs != null)
                {

                    foreach (string id in filterd_IDs)
                    {
                        Delogger.Log("CARD ID", id);
                        Card c = Card.CardByID(id);
                        //todo: verify the ID is not already used.
                        List<Card> possibleCards = Card.CardsAvailableByTitle(c.Title);
                        if (possibleCards.Count() > 0) {
                            thereAreCardsToDisplay = true;
                            Card card = possibleCards[0];

                            Delogger.Log("USED CARD ID", card.ID);

                            cardsToDisplay[card.ID] = 1;
                        }
                        //Card card = Card.CardByID(id);
                    }

                    if (!thereAreCardsToDisplay)
                    {
                        TT.FadeIn(SuggestionsFeedback, 1f, true, 6f, 1f, 1, null, "SF");
                        cardsToDisplay = cardsAvailable;
                            
                        if (FourTMarkersManager.I.HybridIsActive)
                        {
                            cardsToDisplay = null;
                        }

                    }
                    else
                    {
                        CardChooserTitle.text = "Tips for card <b>" + Type.ToString() + " </b>";

                    }
                }
                else
                {
                    cardsToDisplay = cardsAvailable;
                }


                foreach (var cardAndOccurrence in cardsToDisplay)
                {

                    GameObject SliderCard = Instantiate(CardTemplate.gameObject, SliderCardContainert.transform);
                    Card card = Card.CardByID(cardAndOccurrence.Key.ToString());
                    SliderCard = DrawCard(SliderCard, card, cardAndOccurrence.Value, showDisponibility);
                    SliderCard.GetComponent<UIButtonDefault>().onClick.AddListener(() =>
                    {

                        Debug.Log("CanAddCard:: " + CanAddCard);
                        Debug.Log(FourTMarkersManager.I.HybridIsActive);


                        if ((FourTMarkersManager.I != null && FourTMarkersManager.I.HybridIsActive)) //!CanAddCard ||
                            return;
                        


                        FTM.Game.PlayCard(ActualColumn, card, Card.GetLocationFromName(ActualPlace.name));

                        PlaceCardOnBoard(card);

                        HideCardChooser();

                    });

                }

                //cardsToDisplay.Count
                SliderCardContainert.Setup(true, true);
                SliderCardContainert.GetComponent<CanvasGroup>().DOFade(1f, .6f);

                CanAddCard = false;

            });
        }

     

        public Card FindSameUnusedCard(string id)
        {
            Card c = null;
            c = Card.CardByID(id);


            return c;

        }

        public void DrawPersistedGame()
        {
            Board game = FourTManager.I().Game;

            StartCoroutine(filltext(.5f));

            foreach (var pair in game.LocationAndCard)
            {
                //GetThe Card
                Card card = Card.CardByID(pair.Value.ToString());


                string column = pair.Key.Split('-')[0];
                string position = pair.Key.Replace(column + "-", "").ToLower();

                column = column == "C1" ? "Column-1" : column == "C2" ? "Column-2" : column == "C3" ? "Column-3" : "Column-4";

                Transform col = Board.transform.Find(column);
                Transform location = TT.Search(col, position);

                //Delogger.Log(column, position);

                if (location != null)
                {
                    SetLocationAndColumnFromCardPlaceHolder(location, col);
                    PlaceCardOnBoard(card);
                }


            }
        }

        IEnumerator filltext(float seconds)
        {
            Board game = FourTManager.I().Game;

            yield return new WaitForSeconds(seconds);
            ContextContent.text = !string.IsNullOrEmpty(game.Context) ? game.Context : "";
            GoalsContent.text = !string.IsNullOrEmpty(game.Goals) ? game.Goals : "";
            ContentContent.text = !string.IsNullOrEmpty(game.Content) ? game.Content : "";
        }


        public void SavetextContents(string type)
        {

            Board board = FourTManager.I().Game;
            switch (type)
            {
                case "context":
                    board.Context = ContextContent.text;
                    break;
                case "goals":
                    board.Goals = GoalsContent.text;
                    break;
                case "content":
                    board.Content = ContentContent.text;
                    break;
            }

            saveGame();

            //Delogger.Log("Context", board.Context);
            //Delogger.Log("Goals", board.Goals);
            //Delogger.Log("Content", board.Content);


        }

        /*****************************************************/
        // Place the choosen card on the board
        /*****************************************************/
        public Transform PlaceCardOnBoard(Card card, bool anim = false, bool drawCard = true)
        {

            //if (card.Type == Card.CardType.Technique && FourTManager.I().Game.Level == 2)
            //    return null;

            Transform CardTemplate = (card.Type == Card.CardType.Technique) ? BoardLayoutCardHorizontalPrefab : BoardLayoutCardVerticalPrefab;

            ResetAllErrors();

            //ActualPlace.AddComponent<Canvas>();
            //ActualPlace.GetComponent<Canvas>().overrideSorting = true;
            //ActualPlace.GetComponent<Canvas>().sortingOrder = 1000;


            GameObject BoardCard = Instantiate(CardTemplate.gameObject, ActualPlace.transform);
            BoardCard.transform.SetSiblingIndex(0);

            BoardCard.gameObject.SetActive(false);

            if(anim)
                BoardCard.transform.localScale = new Vector3(3f, 3f, 0);

            BoardCard = DrawCard(BoardCard, card, 0);

            // Set the location of the card
            card.BoardLocation = $"C{ActualColumn.ToString().ToUpper()}-{ActualPlace.name.ToUpper()}";

            //Delogger.Log(" card.BoardLocation", card.BoardLocation);
            int LocalCardId = card.ID;

            //DOVirtual.DelayedCall(.2f, () =>
            //{

            //Debug.Log("Draw Card: drawCard ::" + drawCard);

            BoardCard.gameObject.SetActive(true);

            //if(drawCard)
            //    BoardCard.gameObject.SetActive(true);
            //else
            //{
            //    //BoardCard.gameObject.SetActive(false);
            //    FourTManager.I().Game.LocationAndCard[card.BoardLocation] = LocalCardId;
            //    return CardTemplate;
            //}

            BoardCard.transform.DOScale(1f, .6f).SetEase(Ease.OutExpo).OnComplete(() =>
                {
                    BoardCard.GetComponent<UIButtonDefault>().onClick.AddListener(() =>
                    {
                        if ( FourTMarkersManager.I != null && FourTMarkersManager.I.HybridIsActive) //!CanAddCard ||
                            return;

                        Transform CurrentPosition = EventSystem.current.currentSelectedGameObject.transform.parent;
                        Transform CurrentColumn = GetParentsWithName(CurrentPosition, "Column");
                        ShowCardInfoPanel(LocalCardId, BoardCard);
                    });

                    Destroy(ActualPlace.GetComponent<Canvas>());

                    /*****************************************************/
                    // Add card to used cards Dictionary
                    /*****************************************************/

                    FourTManager.I().Game.LocationAndCard[card.BoardLocation] = LocalCardId;

                    Delogger.Log("BoardLocation", card.BoardLocation);

                    LastPlayedCardBoardLocation = card.BoardLocation;

                    //FourTManager.I().Game.Cards.Add(card);
                    /*****************************************************
                    // DisplayErrors(InconsistentErrors, MissingErrors);
                    ****************************************************/
                    ServerJob.I.ConsistencyCheck(FourTManager.I().Game,false, () =>
                    {
                        DisplayErrors(FourTManager.I().LatestKbOut.InconsistentPositions, null);

                        //Save Game State

                        if (FourTManager.I().Persistence == null)
                            FourTManager.I().Persistence = new Persistence();

                        string savedData = FourTManager.I().Persistence.SaveGame();
                        Delogger.Log("SaveGame", savedData);
                        CanAddCard = true;
                    });

                    /*****************************************************/

                });
           // });


            return CardTemplate;
        }

        public void saveGame()
        {
            FourTManager.I().Persistence.SaveGame();
        }

        /*****************************************************/
        // Remove the selected card from the board.
        // This card will be available again for the next choise
        /*****************************************************/

        bool isDeletingCard = false;
        public void RemoveCardFromBoard()
        {

            var FTM = FourTManager.I();

            if (isDeletingCard)
                return;

            isDeletingCard = true;

            Destroy(ActualBoardCard.gameObject);

            //Delogger.Log("Remove - ActualCard.BoardLocation", ActualCard.BoardLocation);
            //Delogger.Log("LocationAndCard count before", FTM.Game.LocationAndCard.Count());


            FTM.Game.LocationAndCard.Remove(ActualCard.BoardLocation);
           // FourTManager.I().Game.Cards.Remove(ActualCard);

            //Delogger.Log("LocationAndCard count after", FTM.Game.LocationAndCard.Count());

            HideCardInfoPanel(true);
            ResetAllErrors();

            string savedData = FourTManager.I().Persistence.SaveGame();

            ServerJob.I.ConsistencyCheck(FourTManager.I().Game, false, () =>
            {
                DisplayErrors(FourTManager.I().LatestKbOut.InconsistentPositions, null);

            });
        }

        public void RemoveAllCardsFromBoard()
        {
            foreach (CardPlaceHolder cPh in CardPlaceHolders)
            {

                if (cPh.transform.childCount == 0 || cPh.transform.GetChild(0) == null)
                    continue;

                ActualBoardCard = cPh.transform.GetChild(0).transform;

                if (ActualBoardCard == null)
                    continue;

                ActualCard = Card.CardByID(cPh.CardId.ToString());

                if (ActualCard == null)
                    continue;

                RemoveCardFromBoard();
            }

        }

        public Transform GetParentsWithName(Transform el, string name)
        {
            Transform parent = el.parent;
            if (parent.name.IndexOf(name) >= 0)
                return parent;

            return GetParentsWithName(parent, name);
        }


        /*****************************************************/
        // Display the single board cards panel with the card detail
        /*****************************************************/
        public void ShowCardInfoPanel(int cardId, GameObject boardCard)
        {

            ActualBoardCard = boardCard.transform;
            ActualCard = Card.CardByID(cardId.ToString());

            Delogger.Log("Show Info - board Location", ActualCard.BoardLocation);


            CardInfoVerticall.gameObject.SetActive(false);
            CardInfoHorizontal.gameObject.SetActive(false);

            GameObject InfoCard = ActualCard.Type == Card.CardType.Technique ? CardInfoHorizontal.gameObject : CardInfoVerticall.gameObject;

            InfoCard = DrawCard(InfoCard, ActualCard, 0);

            InfoCard.SetActive(true);

            InfoCard.transform.localScale = new Vector3(.5f, .5f, 0);
            CardInfoPanel.GetComponent<CanvasGroup>().alpha = 0;

            CardInfoPanel.gameObject.SetActive(true);
            CardInfoPanel.GetComponent<CanvasGroup>().DOFade(1f, .5f);

            InfoCard.transform.DOScale(1f, .6f).SetEase(Ease.OutExpo);

            InfoCard.GetComponent<UIButtonDefault>().onClick.AddListener(() => { HideCardInfoPanel(); });

        }

        /*****************************************************/
        // Hide the single board cards panel with the card detail
        /*****************************************************/
        public void HideCardInfoPanel(bool reverse = false)
        {

            GameObject InfoCard = ActualCard.Type == Card.CardType.Technique ? CardInfoHorizontal.gameObject :  CardInfoVerticall.gameObject;


            var scale = reverse ? 3 : .5f;

            InfoCard.transform.DOScale(scale, .6f).SetEase(Ease.OutExpo);

            if (ActualCard.IsJolly)
            {
                string title = InfoCard.transform.Find("Card/JollyTitle").GetComponent<TMP_InputField>().text;
                ActualCard.Title = title;
                Delogger.Log("Title", title);

                ActualBoardCard.Find("Card/Title").GetComponent<TMP_Text>().text = ActualCard.Title;

                string desc = InfoCard.transform.Find("Card/JollyDesc").GetComponent<TMP_InputField>().text;
                ActualCard.Description = desc;
                Delogger.Log("Desc", desc);
            };

            CardInfoPanel.GetComponent<CanvasGroup>().DOFade(0f, .5f).OnComplete(() =>
            {
                CardInfoPanel.gameObject.SetActive(false);
                CanAddCard = true;
                isDeletingCard = false;
            });
        }


        /*****************************************************/
        // Hide card chooser panel
        /*****************************************************/
        public void HideCardChooser()
        {

            suggesting = false;

            CanvasGroup Cg = CardChooser.GetComponent<CanvasGroup>();
            Cg.DOFade(0f, .3f);
            CardChooser.transform.DOScale(0f, .3f).SetEase(Ease.InExpo).OnComplete(() =>
            {
                CardChooser.gameObject.SetActive(false);

                DOVirtual.DelayedCall(.5f, () =>
                {
                    CanAddCard = true;
                });

            });
        }

        /*****************************************************/
        // Draw the card content
        /*****************************************************/
        public GameObject DrawCard(GameObject cardTemplate, Card card, int occurrences, bool showDisponibility = true)
        {
            var FTM = FourTManager.I();

            bool isOnCardChooser = CardChooser.gameObject.activeSelf;

            cardTemplate.name = card.ID + "_" + (card.IsJolly ? "Jolly" : card.Title);

            Color CardColor = FTM.Config.GetColorByCardType(card.Type);
            Sprite CardIcon = FTM.Config.GetIconByCardType(card.Type);

            Transform DisplayCardBox = cardTemplate.transform;
            Transform DisplayCard = DisplayCardBox.Find("Card");

            DisplayCard.GetComponent<Image>().color = CardColor;

            if (occurrences > 0 && card.Type != Card.CardType.Technique && showDisponibility)
            {
                DisplayCard.Find("Occurrences").gameObject.SetActive(true);
                if (DisplayCard.Find("Occurences_image"))
                    DisplayCard.Find("Occurences_image").gameObject.SetActive(true);
                DisplayCard.Find("Occurrences").GetComponent<TextMeshProUGUI>().text = occurrences + "";
            }
            else
            {
                DisplayCard.Find("Occurrences").gameObject.SetActive(false);
                if (DisplayCard.Find("Occurences_image"))
                    DisplayCard.Find("Occurences_image").gameObject.SetActive(false);
            }

            DisplayCard.Find("Image").GetComponent<Image>().sprite = CardIcon;
            DisplayCard.Find("Image").GetComponent<Image>().color = FTM.Config.GetColorByCardType(card.Type) * .8f;

            DisplayCard.Find("Type").GetComponent<TextMeshProUGUI>().text = card.Type.ToString();

            if(DisplayCard.Find("Id") != null)
                DisplayCard.Find("Id").GetComponent<TextMeshProUGUI>().text = card.ID.ToString();

            if (!isOnCardChooser && card.IsJolly && DisplayCard.Find("JollyTitle"))
            {
                DisplayCard.Find("JollyTitle").gameObject.SetActive(true);

                DisplayCard.Find("JollyTitle").GetComponent<TMP_InputField>().text = card.Title;
                DisplayCard.Find("Title").GetComponent<TextMeshProUGUI>().text = "";
            } else
            {

                DisplayCard.Find("Title").GetComponent<TextMeshProUGUI>().text = card.IsJolly ? "WILDCARD" : card.Title;

                if(DisplayCard.Find("InclusionTips"))
                    DisplayCard.Find("InclusionTips").GetComponent<TextMeshProUGUI>().text = "";
            }


            if (!isOnCardChooser && card.IsJolly && DisplayCard.Find("JollyDesc"))
            {
                DisplayCard.Find("JollyDesc").gameObject.SetActive(true);
                DisplayCard.Find("JollyDesc").GetComponent<TMP_InputField>().text = card.Description;

                DisplayCard.Find("Desc").GetComponent<TextMeshProUGUI>().text = "";
                if (DisplayCard.Find("InclusionTips"))
                    DisplayCard.Find("InclusionTips").GetComponent<TextMeshProUGUI>().text = "";

            } else if (DisplayCard.Find ("Desc")) {

                string text = "";
                if (card.Type != Card.CardType.Technique) {
                    text = card.Description;
                    if (!string.IsNullOrEmpty (card.InclusionTips)) {
                        text += "<br><br>Inclusion Tips<br>";
                        text += $"<size=80%>{card.InclusionTips}</size>";
                    }
                } else {
                    text = card.Description;
                }

                DisplayCard.Find ("Desc").GetComponent<TextMeshProUGUI> ().text = text;
            }

            if (!isOnCardChooser && !card.IsJolly)
            {
                if(DisplayCard.Find("JollyTitle"))
                     DisplayCard.Find("JollyTitle").gameObject.SetActive(false);

                if(DisplayCard.Find("JollyDesc"))
                    DisplayCard.Find("JollyDesc").gameObject.SetActive(false);

            }

            if (DisplayCard.Find ("InclusionTips") && !card.IsJolly) {
                string text = !string.IsNullOrEmpty (card.InclusionTips) ?( "Inclusion Tips<br><br>" + card.InclusionTips ): "";
                DisplayCard.Find ("InclusionTips").GetComponent<TextMeshProUGUI> ().text = text;
            }

            return cardTemplate;
        }

        /*****************************************************/
        // Create the Board
        /*****************************************************/
        public void BuildBoard()
        {

            foreach (Transform Child in Board)
                Destroy(Child.gameObject);

            for (int i = 1; i <= ColumnsNumber; i++)
            {
                GameObject BoardCol = Instantiate(ColumnPrefaboard.gameObject, Board);
                BoardCol.name = "Column-" + (i);
            }
        }

        /*****************************************************/
        // Layout mapping methods
        /*****************************************************/
        public void SetLocationAndColumnFromCardPlaceHolder(Transform location, Transform column)
        {
            ActualPlace = location.gameObject;
            var s = column.name;
            ActualColumn = int.Parse(s.Substring(s.Length - 1));
        }

        /*****************************************************/
        // Find the column header of the given column
        /*****************************************************/
        public Transform GetColumnHeader(Transform Col)
        {
            Transform ColumnHeader;
            ColumnHeader = TT.Search(Col.transform, "Text");
            return ColumnHeader;
        }

        /*****************************************************/
        // Run the suggestions based on the actual card position
        /*****************************************************/
        bool suggesting = false;
        public void SuggestCards()
        {
            if (suggesting)
                return;

            suggesting = true;

            ServerJob.I.SuggestionCheckCall(
                FourTManager.I().Game,
                $"C{ActualColumn.ToString().ToUpper()}-{ActualPlace.name.ToUpper()}",
                () =>
            {
                ShowCardChooser(ChooserType, FourTManager.I().LatestKbOut.Alternatives, true, false);
            });

        }

        public void CompletenessCheck()
        {
            if(FourTManager.I().Game.Level != 3)
                ServerJob.I.CompletenessCheckCall();
        }

        /*****************************************************/
        // Filter cards by Board placeholder name
        /*****************************************************/
        public Card.CardType GetcardTypeByPlaceHolderName(string Name)
        {
            Card.CardType atype = Card.CardType.Technique;

            if (Name.ToLower().IndexOf("technique") > -1)
                atype = Card.CardType.Technique;

            else if (Name.ToLower().IndexOf("task") > -1)
                atype = Card.CardType.Task;

            else if (Name.ToLower().IndexOf("team") > -1)
                atype = Card.CardType.Team;

            else if (Name.ToLower().IndexOf("tec") > -1)
                atype = Card.CardType.Technology;

            return atype;
        }

        public void ShowQuitPanel()
        {
            QuitPanel.gameObject.SetActive(true);
        }

        public void HideQuitPanel()
        {
            QuitPanel.gameObject.SetActive(false);
        }

        public void Restart()
        {
            HideConfirmQuit();
            SettingsManager.I.Restart(true);
        }

        public void Quit()
        {
            //Delogger.Log("QUITTING", true);

            Application.Quit();
        }


    }
}