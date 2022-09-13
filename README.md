# 4Ts-public-repository

## IO6 - I4Ts Game: Developer’s Documentation
### Introduction

The application is a client native application that connects online at startup and during service (http call) to a web based server. It has been built so as to be as stable as possible, compatible with generic hardware (Windows, Mac) and resilient to low definition printed markers. The client development tool (Unity) is a commercial product but is available with a full free version.  The web service servicing the cards and their logic is built on a Prolog backend.

Full sources of all the modules composing the application (Unity project, C# code, Prolog backend, card service) are available through a public Git server here:

https://github.com/4Ts-game/4Ts-public-repository

allowing the customization and branching of the application. 

Changes in the application client require to be proficient in Unity development, but do not require Prolog competence, as that is used as a service. Changes in the educational model behind the cards require Prolog competence to adapt the service to the intended model.

The I4T Game client is developed with Unity3D version 2019.4.9f1 (go to downloads at https://unity.com). The source code of the client is written in C#. It is realized under GPL license and it is open source.
The Unity plugins installed are all free, freely available and included in the distribution.



The Hybrid game is developed using the open source “OpenCV” ArUco markers Library (https://docs.opencv.org/master/d5/dae/tutorial_aruco_detection.html) ported for C# and Unity (https://github.com/NormandErwan/ArucoUnity) by Erwan Normand.

The Online Card service is developed in PHP, javascript, CSS and HTML. 
It uses a Google sheet as a datasource converting it into a JSON object to retrieve and display the cards’ data. 
Markers are produced based on the card ID using the ArUco Marker generator realized by Oleg Kalachev (https://github.com/okalachev/arucogen).

Markers dictionaries are taken from this URL: https://raw.githubusercontent.com/opencv/opencv_contrib/master/modules/aruco/src/predefined_dictionaries.hpp.


All required downloads:

Unity: https://unity3d.com/get-unity/download/archive
Unity project sources, Prolog and its service, Card service: https://github.com/4Ts-game/4Ts-public-repository 

### The flow of code and Unity scenes in the client application

In order to edit the Unity application you need to have the appropriate Unity version and a code editor (like Visual Studio as provided by the Unity download).

The application is composed of three scenes: Intro, LoadGames and Game. The application must start from the Intro scene.

The overall namespace of the classes is FourT. Inside the Unity project, as usual the classes are defined in the Assets/_Scripts folder.

Classes are in bold and methods in italic.

### Intro scene
This is just a passthrough scene that presents a splash and loads the LoadGames scene.

### LoadGames scene
When launching the application, in all cases the first class to set up is the (unique) instance of FourTManager which will persist across scenes. 
This singleton class will load the cards either from an online spreadsheet on from the local Resources folder, and then will load persistent data of previous games if available. It also handles saving games through the Persistence class.

The main scene-specific manager is LoadGamesManager: this will set two properties that determine the game’s behaviour:

FourTManager.I().Game.Level:
	Value 0: Visually 1, with full feedback on played cards.
	Value 1: Visually 2, like 0 but does not check the technique cards.
Value 2: Visually 3, this does no checks.

FourTManager.I().Game.GameType:
	Value 0: Purely digital game.
	Value 1: Hybrid, integrated with a webcam detecting the board (corners and card locations) and cards.

Defined those, the GameScene scene is loaded.

The Persistence class: this class saves and loads games with a simple JSON serialization based on a built in service, JsonConvert.SerializeObject.

### GameScene
The main scene manager of the game scene is BoardManager. This class designs the board (eventually loaded from persistence), setups a listener for clicks in card locations and launches the appropriate classes according to the game type.

In case the board is clicked (digital version) or a physical card is placed (hybrid), the card chooser panel is activated (method ShowCardChooser) and on a choice, the method PlaceCardOnBoard is called.

On every card played, if the game’s level is not “3” (2 internally) the completeness check is called:

ServerJob.I.CompletenessCheckCall()

And also the game state gets persisted with a call to (see the documentation below).

FourTManager.I().Persistence.SaveGame();



## More classes

Assets/_Scripts/FourTConfiguration
This is a typical Unity configuration class (ScriptableObject) that keeps references to graphical assets.

Assets/_Scripts/Model/Board.cs
Board navigation methods for reaching the played cards.

Assets/_Scripts/Model/Card.cs
Properties and methods for defining cards, their type, specific properties and runtime their position on the board. Also methods for getting the card full instance given their type and/or ID.

Assets/_Scripts/FourTMarkersManager.cs
Methods for handling the hybrid functionalities: methods for scanning the markers present on the board, handling of physical action following detection of played cards, integration with the board digital representation.

Assets/_Scripts/ServerJob.cs
This is an asynchronous service class that interacts with the remote server and services, in particular gets the validation XML answer for any card configuration.

