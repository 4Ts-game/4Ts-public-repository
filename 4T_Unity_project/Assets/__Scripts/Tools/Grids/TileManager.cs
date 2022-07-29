// Author: Pietro Polsinelli - http://designAGame.eu
// Twitter https://twitter.com/ppolsinelli
// License: WTFPL, all free as in free beer :-)
// Tested with Unity 5.6.1
// Created: 01 09 2016

using System;
using System.Collections.Generic;
using UnityEngine;

namespace OL
{
    [RequireComponent(typeof(Collider2D))]
    public class TileManager : MonoBehaviour
    {
        [Header("Configuration")]
        public Transform GridRoot;
        public bool EnableDiagonals;
        public float CostToDistanceMultiplier = 1;
        public bool ShowWarnings;
        public bool DoNotKillMouseUpOverUI;

        [Header("Others - computed")]
        public string DebugText;

        public Tile Selected;
        public Dictionary<Point, Tile> Tiles = new Dictionary<Point, Tile>();
        public Dictionary<Point, Vector3> PointCenters = new Dictionary<Point, Vector3>();

        readonly Dictionary<Vector3, Tile> positionTileCache = new Dictionary<Vector3, Tile>();

        public static event Action<Tile> TileClicked = delegate { };
        public static event Action<Tile> TileSelected = delegate { };

        bool managerSetupWarn = false;

        public static TileManager I;

        public enum Direction
        {
            Any,
            Horizontal,
            Vertical
        }


        void Awake()
        {
            if (I == null)
                I = this;
        }

        void Update()
        {
            if (!managerSetupWarn && Time.timeSinceLevelLoad > 3 && Tiles.Count == 0)
            {
                managerSetupWarn = true;
                Debug.Log("Tile manager setup call seems lacking");
            }
                
        }
       
        public void Setup()
        {
            if (Tiles.Count > 0)
                return;

            var tiles = GridRoot.GetComponentsInChildren<Tile>();
            foreach (var tile in tiles)
            {
                //needed because you need the struct instance live at runtime
                tile.SetupByRuntime();

                if (!Tiles.ContainsKey(tile.TilePoint))
                {
                    //Debug.Log("TilePoint added " + tile.TilePoint);
                    Tiles.Add(tile.TilePoint, tile);
                    PointCenters[tile.TilePoint] = tile.transform.position;
                }
                else
                {
                    if (ShowWarnings)
                        Debug.Log("duplicated " + tile.TilePoint);
                }
            }
            DebugText = "tiles.Length " + tiles.Length;
        }

        public Tile FindNeighbourCostingLess(Tile t, float minimalCost, int maxDepth, int currentDepth = 0)
        {
            foreach (var neighbor in Neighbors(t))
                if (neighbor.Cost <= minimalCost)
                    return neighbor;
            currentDepth++;

            if (currentDepth <= maxDepth)
                foreach (var neighbor in Neighbors(t))
                {
                    if (neighbor == t)
                        continue;
                    var found = FindNeighbourCostingLess(neighbor, minimalCost, maxDepth, currentDepth);
                    if (found != null)
                        return found;
                }

            return null;
        }

        public void OnMouseUp()
        {
            if (!TT.IsPointerOverUIObject() || DoNotKillMouseUpOverUI)
            {
                var screenToWorldPoint = Camera.main.ScreenToWorldPoint(Input.mousePosition);
                var tile = FindTileAtPosition(screenToWorldPoint);
                TileClicked(tile);
            }
            /*else
            {

                Debug.Log("TM OnMouseUp ignoring");
            }*/
        }

        public void OnMouseDown()
        {
            //if (!EventSystem.current.IsPointerOverGameObject())
            if (!TT.IsPointerOverUIObject() || DoNotKillMouseUpOverUI)
            {
                var screenToWorldPoint = Camera.main.ScreenToWorldPoint(Input.mousePosition);
                var tile = FindTileAtPosition(screenToWorldPoint);
                TileSelected(tile);
            }
            /*else
            {
                Debug.Log("TM OnMouseDown ignoring");
            }*/
        }

        public Tile FindTileAtPosition(Vector3 position)
        {
            if (positionTileCache.ContainsKey(position))
                return positionTileCache[position];

            var minDist = float.MaxValue;
            Tile closest = null;
            foreach (var t in Tiles.Values)
                if (t != null)
                {
                    var dis = t.transform.position - position;
                    var planeDist = Mathf.Abs(dis.x) + Mathf.Abs(dis.y);
                    if (closest == null || planeDist < minDist)
                    {
                        closest = t;
                        minDist = planeDist;
                        //Debug.Log("found close at " + t);
                    }
                }
            positionTileCache[position] = closest;
            return closest;
        }

        

        public HashSet<Point> Neighbors(Point point)
        {
            var n = new HashSet<Point>();

            n.Add(point + OL.Point.North);
            n.Add(point + OL.Point.South);
            n.Add(point + OL.Point.East);
            n.Add(point + OL.Point.West);

            if (EnableDiagonals)
            {
                n.Add(point + OL.Point.North + OL.Point.East);
                n.Add(point + OL.Point.North + OL.Point.West);
                n.Add(point + OL.Point.South + OL.Point.East);
                n.Add(point + OL.Point.South + OL.Point.West);
            }

            return n;
        }

        public List<Tile> Neighbors(Tile tile)
        {
            var n = new List<Tile>();
            {
                var left = new Point(tile.GridX - 1, tile.GridY);
                if (Tiles.ContainsKey(left))
                    n.Add(Tiles[left]);
                var right = new Point(tile.GridX + 1, tile.GridY);
                if (Tiles.ContainsKey(right))
                    n.Add(Tiles[right]);
                var above = new Point(tile.GridX, tile.GridY + 1);
                if (Tiles.ContainsKey(above))
                    n.Add(Tiles[above]);
                var below = new Point(tile.GridX, tile.GridY - 1);
                if (Tiles.ContainsKey(below))
                    n.Add(Tiles[below]);
            }

            if (EnableDiagonals)
            {
                var left = new Point(tile.GridX - 1, tile.GridY + 1);
                if (Tiles.ContainsKey(left))
                    n.Add(Tiles[left]);
                var right = new Point(tile.GridX + 1, tile.GridY + 1);
                if (Tiles.ContainsKey(right))
                    n.Add(Tiles[right]);
                var above = new Point(tile.GridX - 1, tile.GridY - 1);
                if (Tiles.ContainsKey(above))
                    n.Add(Tiles[above]);
                var below = new Point(tile.GridX + 1, tile.GridY - 1);
                if (Tiles.ContainsKey(below))
                    n.Add(Tiles[below]);
            }
            return n;
        }

        // Adapted from http://www.redblobgames.com/pathfinding/a-star/implementation.html#csharp
        public Dictionary<Tile, Tile> AStar(Tile start, Tile goal, bool includeBlockedEnd = false,
            bool ignoreCosts = false)
        {
            var cameFrom = new Dictionary<Tile, Tile>();
            var costSoFar = new Dictionary<Tile, double>();

            var frontier = new PriorityQueue<Tile>();
            frontier.Enqueue(start, 0);

            cameFrom[start] = start;
            costSoFar[start] = 0;

            while (frontier.Count > 0)
            {
                var current = frontier.Dequeue();

                if (current.Equals(goal))
                    break;

                foreach (var next in Neighbors(current))
                {
                    var nextCost = next.Cost;
                    if (ignoreCosts)
                        nextCost = 0;

                    if (nextCost < 1 || includeBlockedEnd && next == goal)
                    {
                        var newCost = costSoFar[current] + nextCost * CostToDistanceMultiplier;

                        if (!costSoFar.ContainsKey(next) || newCost < costSoFar[next])
                        {
                            costSoFar[next] = newCost;
                            var priority = newCost + next.DistanceFrom(goal);
                            frontier.Enqueue(next, priority);
                            cameFrom[next] = current;
                        }
                    }
                }
            }
            return cameFrom;
        }

        public static List<Tile> AStarFindPath(Tile start, Tile goal, Dictionary<Tile, Tile> cameFrom)
        {
            var current = goal;
            var path = new List<Tile>();
            path.Add(current);
            while (current != start)
                if (cameFrom.ContainsKey(current))
                {
                    current = cameFrom[current];
                    path.Add(current);
                }
                else
                {
                    if (I.ShowWarnings)
                        Debug.Log("step not found in path " + current);
                    break;
                }
            path.Reverse();
            return path;
        }

        public static List<Tile> AStarPath(Tile start, Tile goal, bool includeBlockedEnd = false,
            bool ignoreCosts = false)
        {
            var resultingAStar = I.AStar(start, goal, includeBlockedEnd, ignoreCosts);
            return AStarFindPath(start, goal, resultingAStar);
        }

        public class PriorityQueue<T>
        {
            readonly List<KeyValuePair<T, double>> elements = new List<KeyValuePair<T, double>>();

            public int Count
            {
                get { return elements.Count; }
            }

            public void Enqueue(T item, double priority)
            {
                var keyValuePair = new KeyValuePair<T, double>(item, priority);
                elements.Add(keyValuePair);
            }

            public T Dequeue()
            {
                var bestIndex = 0;

                for (var i = 0; i < elements.Count; i++)
                    if (elements[i].Value < elements[bestIndex].Value)
                        bestIndex = i;

                var bestItem = elements[bestIndex].Key;
                elements.RemoveAt(bestIndex);
                return bestItem;
            }
        }

        public Vector3 Position(Point currentDestination)
        {
            return Tiles[currentDestination].transform.position;
        }

        public Point FindPointAtPosition(Vector3 position)
        {
            var minDist = float.MaxValue;
            var closest = new Point(0, 0);
            foreach (var keyValuePair in PointCenters)
            {
                var dis = keyValuePair.Value - position;
                var planeDist = Mathf.Abs(dis.x) + Mathf.Abs(dis.y);
                if (planeDist < minDist)
                {
                    minDist = planeDist;
                    closest = keyValuePair.Key;
                }
            }

            /*if (Tiles.ContainsKey(closest))
            {
                Color orig = Tiles[closest].Sprite.color;
                Tiles[closest].Sprite.DOColor(Color.red,.2f).OnComplete(() =>
                {
                    Tiles[closest].Sprite.color = orig;
                });
            }*/

            return closest;
        }
        
        //http://ericw.ca/notes/bresenhams-line-algorithm-in-csharp.html
        //online demo http://gpolo.awardspace.info/
        public static List<Point> GetPointsOnLine(int x0, int y0, int x1, int y1)
        {
            List<Point> points = new List<Point>();
            bool steep = Math.Abs(y1 - y0) > Math.Abs(x1 - x0);
            if (steep)
            {
                int t;
                t = x0; // swap x0 and y0
                x0 = y0;
                y0 = t;
                t = x1; // swap x1 and y1
                x1 = y1;
                y1 = t;
            }
            if (x0 > x1)
            {
                int t;
                t = x0; // swap x0 and x1
                x0 = x1;
                x1 = t;
                t = y0; // swap y0 and y1
                y0 = y1;
                y1 = t;
            }
            int dx = x1 - x0;
            int dy = Math.Abs(y1 - y0);
            int error = dx / 2;
            int ystep = (y0 < y1) ? 1 : -1;
            int y = y0;
            for (int x = x0; x <= x1; x++)
            {
                points.Add(new Point((steep ? y : x), (steep ? x : y)));
                error = error - dy;
                if (error < 0)
                {
                    y += ystep;
                    error += dx;
                }
            }
            return points;
        }

        public static bool IsPointBetweenLines(Point point, List<Point> leftMarginLine, 
            List<Point> rightMarginLine, bool includeBorders=true)
        {
            if (includeBorders)
            {
                if (leftMarginLine.Contains(point) || rightMarginLine.Contains(point))
                    return true;
            }

            Point leftAtY=new Point(0,0);
            Point rightAtY= new Point(0, 0);

            foreach (var plPoint in leftMarginLine)
            {
                if (plPoint.Y == point.Y)
                {
                    leftAtY = plPoint;
                    break;
                }
            }
            
            foreach (var pRPoint in rightMarginLine)
            {
                if (pRPoint.Y == point.Y)
                {
                    rightAtY = pRPoint;
                    break;
                }
            }

            return point.X.Between(leftAtY.X, rightAtY.X);
        }

    }
}