using System;
using System.Collections;
using System.Collections.Generic;
using DG.DeInspektor.Attributes;
using OL;
using UnityEngine;

namespace OL
{
    [RequireComponent(typeof(Collider2D))]
    public class VirtualGrid : MonoBehaviour
    {
        Camera main;
        public bool EnableDiagonals;
        public int CentralGridX;
        public int CentralGridY;
        public bool IsLeftLowerNotCentered;
        public float TileSideSize;
        public bool ShowWarnings;
        public bool DoNotKillMouseUpOverUI;

        public bool HandleMouseOver;
        Point lastMouseOverRaised;
        public bool IsMouseOverGrid;

        Dictionary<Point, float> CostPerPoint = new Dictionary<Point, float>();
        public float CostToDistanceMultiplier = 1;

        public static event Action<Point> TileClicked = delegate { };
        public static event Action<Point> TileSelected = delegate { };
        public static event Action<Point> TileWithNewMouseOver = delegate { };

        public static VirtualGrid I;

        void Awake()
        {
            if (I == null)
            {
                I = this;
                main = Camera.main;
            }
            else
            {
                Debug.Log("Multiple VirtualGrid instances - one destroyed");
                Destroy(gameObject);
            }                 
        }

        public Vector2 FindPositionAtPoint(Point point)
        {
            return FindPositionAtPoint(point.X, point.Y);
        }

        public Vector3 FindPositionAtPoint(int x, int y)
        {
            int diffX = x - CentralGridX;
            int diffY = y - CentralGridY;

            float vX = (transform.position.x + (TileSideSize * transform.localScale.x * diffX)) ;
            float vY = (transform.position.y + (TileSideSize * transform.localScale.y * diffY)) ;
            return new Vector3(vX, vY);
        }

        public Point FindPointAtPosition(Vector3 position)
        {
            float xDist = position.x - transform.position.x;
            float yDist = position.y - transform.position.y;

            int cellDistX = (int)Math.Round((xDist / transform.localScale.x) / (TileSideSize));
            int cellDistY = (int)Math.Round((yDist / transform.localScale.y) / TileSideSize);

            return new Point(CentralGridX + cellDistX, CentralGridY + cellDistY);

            /*foreach (var keyValuePair in PointCenters)
            {
                var dis = keyValuePair.Value - position;
                var planeDist = Mathf.Abs(dis.x) + Mathf.Abs(dis.y);
                if (planeDist < minDist)
                {
                    minDist = planeDist;
                    closest = keyValuePair.Key;
                }
            }*/

            /*if (Tiles.ContainsKey(closest))
            {
                Color orig = Tiles[closest].Sprite.color;
                Tiles[closest].Sprite.DOColor(Color.red,.2f).OnComplete(() =>
                {
                    Tiles[closest].Sprite.color = orig;
                });
            }*/
        }

        [DeMethodButton("PositionLabeledChildren")]
        public void PositionLabeledChildren()
        {
            var virtualGridTile = GetComponentsInChildren<VirtualGridTile>();
            foreach (var tile in virtualGridTile)
            {
                tile.transform.position = FindPositionAtPoint(tile.X, tile.Y);
                var labelText = tile.X + " " + tile.Y;
                tile.Label.text = labelText;
                tile.name = labelText;
            }
        }

        public void OnMouseUp()
        {
            if (!TT.IsPointerOverUIObject() || DoNotKillMouseUpOverUI)
            {
                var screenToWorldPoint = main.ScreenToWorldPoint(Input.mousePosition);
                var tile = FindPointAtPosition(screenToWorldPoint);
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
                var screenToWorldPoint = main.ScreenToWorldPoint(Input.mousePosition);
                var tile = FindPointAtPosition(screenToWorldPoint);
                TileSelected(tile);
            }
            /*else
            {
                Debug.Log("TM OnMouseDown ignoring");
            }*/
        }

        //only for debug purposes - not sure it works on mobile
        public void OnMouseOver()
        {
            if (!HandleMouseOver)
                return;

            IsMouseOverGrid = true;

            if (!TT.IsPointerOverUIObject() || DoNotKillMouseUpOverUI)
            {
                var screenToWorldPoint = main.ScreenToWorldPoint(Input.mousePosition);
                
                var tile = FindPointAtPosition(screenToWorldPoint);

                if (tile != lastMouseOverRaised)
                {
                    //Debug.Log("screenToWorldPoint " + screenToWorldPoint);
                    lastMouseOverRaised = tile;
                    Debug.Log("tile " + tile);
                    //Debug.Log("tile is at " + FindPositionAtPoint(tile));
                    TileWithNewMouseOver(tile);
                }
            }
        }

        public  void OnMouseExit()
        {
            if (!HandleMouseOver)
                return;

            IsMouseOverGrid = false;
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
            List<Point> rightMarginLine, bool includeBorders = true)
        {
            if (includeBorders)
            {
                if (leftMarginLine.Contains(point) || rightMarginLine.Contains(point))
                    return true;
            }

            Point leftAtY = new Point(0, 0);
            Point rightAtY = new Point(0, 0);

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


        // Adapted from http://www.redblobgames.com/pathfinding/a-star/implementation.html#csharp
        public Dictionary<Point, Point> AStar(Point start, Point goal, bool includeBlockedEnd = false,
            bool ignoreCosts = false)
        {
            var cameFrom = new Dictionary<Point, Point>();
            var costSoFar = new Dictionary<Point, double>();

            var frontier = new PriorityQueue<Point>();
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
                    float nextCost = 0;
                    if (CostPerPoint.ContainsKey(next))
                        nextCost = CostPerPoint[next];

                    if (ignoreCosts)
                        nextCost = 0;

                    if (nextCost < 1 || includeBlockedEnd && next == goal)
                    {
                        var newCost = costSoFar[current] + nextCost * CostToDistanceMultiplier;

                        if (!costSoFar.ContainsKey(next) || newCost < costSoFar[next])
                        {
                            costSoFar[next] = newCost;
                            var priority = newCost + next.Distance(goal);
                            frontier.Enqueue(next, priority);
                            cameFrom[next] = current;
                        }
                    }
                }
            }
            return cameFrom;
        }

        public HashSet<Point> Neighbors(Point point)
        {
            var n = new HashSet<Point>();

            n.Add(point + Point.North);
            n.Add(point + Point.South);
            n.Add(point + Point.East);
            n.Add(point + Point.West);

            if (EnableDiagonals)
            {
                n.Add(point + Point.North + Point.East);
                n.Add(point + Point.North + Point.West);
                n.Add(point + Point.South + Point.East);
                n.Add(point + Point.South + Point.West);
            }

            return n;
        }

        public static List<Point> AStarFindPath(Point start, Point goal, Dictionary<Point, Point> cameFrom)
        {
            var current = goal;
            var path = new List<Point>();
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

        public static List<Point> AStarPath(Point start, Point goal, bool includeBlockedEnd = false,
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

        public bool IsPointInGrid(Point p)
        {
            int width = CentralGridX * 2 + (IsLeftLowerNotCentered ? 0 : 1);
            int height = CentralGridY * 2 + (IsLeftLowerNotCentered ? 0 : 1);
            return p.X.Between(0, width - 1) && p.Y.Between(0, height - 1);
        }

        public HashSet<Point> AllPoints()
        {
            HashSet<Point> all = new HashSet<Point>();
            int width = CentralGridX * 2 + (IsLeftLowerNotCentered ? 0 : 1);
            int height = CentralGridY * 2 + (IsLeftLowerNotCentered ? 0 : 1);
            for (int i = 0; i < width; i++)
            {
                for (int j = 0; j < height; j++)
                {
                    all.Add(new Point(i, j));
                }
            }
            return all;
        }

        public void SetCost(Point p, float cost)
        {
            CostPerPoint[p] = cost;
        }

        public float Cost(Point p)
        {
            float cost = 0;
            if (CostPerPoint.ContainsKey(p))
                cost = CostPerPoint[p];
            return cost;
        }

    }
}
