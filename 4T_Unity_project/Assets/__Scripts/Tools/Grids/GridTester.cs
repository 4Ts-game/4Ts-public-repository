// Author: Pietro Polsinelli - http://designAGame.eu
// Twitter https://twitter.com/ppolsinelli
// License: WTFPL, all free as in free beer :-)
// Tested with Unity 5.6.1
// Created: 02 09 2016

using System.Collections.Generic;
using DG.DeInspektor.Attributes;
using DG.Tweening;
using UnityEngine;

namespace OL
{
    public class GridTester : MonoBehaviour
    {
        public Tile StartTile;
        bool showingPaths;

        public int x0;
        public int y0;
        public int x1;
        public int y1;
        
        void Start()
        {
            TileManager.I.Setup();
            TileManager.TileClicked += TileClicked;
        }

        public void TileClicked(Tile t)
        {
            if (showingPaths)
                return;

            if (StartTile == null)
            {
                StartTile = t;
                StartTile.Sprite.color = Color.yellow;
            }
            else
            {
                t.Sprite.color = Color.red;
                showingPaths = true;
                DOVirtual.DelayedCall(1, () =>
                {
                    var resultingAStar = TileManager.I.AStar(StartTile, t);
                    foreach (var tile in resultingAStar.Values)
                        if (tile != t && tile != StartTile)
                            tile.Sprite.color = Color.blue;

                    var path = TileManager.AStarFindPath(StartTile, t, resultingAStar);
                    foreach (var tile in path)
                        if (tile != t && tile != StartTile)
                            tile.Sprite.color = Color.green;
                }).OnComplete(() =>
                {
                    DOVirtual.DelayedCall(2, () =>
                    {
                        foreach (var tile in TileManager.I.Tiles.Values)
                            tile.Sprite.color = tile.DefaultColor;
                        StartTile = null;
                        showingPaths = false;
                    });
                });
            }
        }


        void OnDisable()
        {
            TileManager.TileClicked -= TileClicked;
        }

        [DeMethodButton("Test Bresenham")]
        public void TestBresenham()
        {
            List<Point> pointsOnLine = TileManager.GetPointsOnLine(x0, y0, x1, y1);
            foreach (var point in pointsOnLine)
            {
                SpriteRenderer sr = TileManager.I.Tiles[point].GetComponent<SpriteRenderer>();
                Color orig = sr.color;
                sr.DOColor(Color.red, 2).OnComplete(() =>
                {
                    sr.color = orig;
                });
            }
        }

        /*
         * if (GUILayout.Button("Ray test"))
            {
                /*
                 12 39
                 16 39
                 14 32
                 Hit 14 37, no hit 10 36?
                   
        Plane p = new Plane(
            TileManager.I.Position(new Point(12, 39)),
            TileManager.I.Position(new Point(16, 39)),
            TileManager.I.Position(new Point(14, 32))
        );
        Ray rHit = new Ray(
            TileManager.I.Position(new Point(14, 31)),
            TileManager.I.Position(new Point(14, 37))
        );
        Ray rNoHit = new Ray(
            TileManager.I.Position(new Point(12, 31)),
            TileManager.I.Position(new Point(10, 36))
        );
        float what;
        var hit1 = p.Raycast(rHit, out what);
        Debug.Log("hit1 " + hit1+ " what "+ what);
        var hit2 = p.Raycast(rNoHit, out what);
        Debug.Log("hit2 " + hit2 + " what " + what);
    }
         */

    }
}