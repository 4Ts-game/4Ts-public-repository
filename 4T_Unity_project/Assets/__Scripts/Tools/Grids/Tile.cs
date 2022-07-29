// Author: Pietro Polsinelli - http://designAGame.eu
// Twitter https://twitter.com/ppolsinelli
// License: WTFPL, all free as in free beer :-)
// Tested with Unity 5.6.1
// Created: 01 09 2016

using System.Collections.Generic;
using TMPro;
using UnityEngine;

namespace OL
{
    public class Tile : MonoBehaviour
    {
        public int GridX;
        public int GridY;

        public Point TilePoint;

        // 1 not walkable, 0 walkable
        public float Cost;

        public Color DefaultColor;

        public SpriteRenderer Sprite;
        public Vector2 Position;

        public void SetupByEditor(Color oddColor, Color evenColor)
        {
            Sprite = GetComponent<SpriteRenderer>();
            GridX = int.Parse(name);
            GridY = int.Parse(transform.parent.name);
            TilePoint = new Point(GridX, GridY);
            ResetColor(oddColor, evenColor);

            GameObject debugText = TT.FindGameObjectChildWithTag(gameObject,"Debug");
            if (debugText != null)
            {
                debugText.GetComponent<TextMeshPro>().text = GridX + " " + GridY;
            }
        }

        public void SetupByRuntime()
        {
            Sprite = GetComponent<SpriteRenderer>();
            name = "Tile " + GridX + " " + GridY;
            TilePoint = new Point(GridX, GridY);

            GameObject debugText = TT.FindGameObjectChildWithTag(gameObject, "Debug");
            if (debugText != null)
            {
                debugText.GetComponent<TextMeshPro>().text = GridX + " " + GridY;
            }
        }

        public void ResetColor(Color oddColor, Color evenColor)
        {
            if ((GridX + GridY) % 2 == 0)
                Sprite.color = evenColor;
            else
                Sprite.color = oddColor;

            DefaultColor = Sprite.color;
        }

        public float DistanceFrom(Tile tile)
        {
            return tile.TilePoint.Distance(TilePoint);
        }

        public void ColorByCost()
        {
            if (Mathf.Approximately(Cost, 0))
                Sprite.color = DefaultColor;
            else if (Cost > 0 && Cost < .9f)
                Sprite.color = Color.magenta;
            else if (Cost > .9f)
                Sprite.color = Color.black;
        }

        public HashSet<Tile> GetNeighbours()
        {
            var neighbours = new HashSet<Tile>();
            if (TileManager.I.Tiles.ContainsKey(TilePoint + Point.North))
                neighbours.Add(TileManager.I.Tiles[TilePoint + Point.North]);
            if (TileManager.I.Tiles.ContainsKey(TilePoint + Point.East))
                neighbours.Add(TileManager.I.Tiles[TilePoint + Point.East]);
            if (TileManager.I.Tiles.ContainsKey(TilePoint + Point.West))
                neighbours.Add(TileManager.I.Tiles[TilePoint + Point.West]);
            if (TileManager.I.Tiles.ContainsKey(TilePoint + Point.South))
                neighbours.Add(TileManager.I.Tiles[TilePoint + Point.South]);

            if (TileManager.I.EnableDiagonals)
            {
                if (TileManager.I.Tiles.ContainsKey(TilePoint + Point.North + Point.East))
                    neighbours.Add(TileManager.I.Tiles[TilePoint + Point.North + Point.East]);
                if (TileManager.I.Tiles.ContainsKey(TilePoint + Point.North + Point.West))
                    neighbours.Add(TileManager.I.Tiles[TilePoint + Point.North + Point.West]);
                if (TileManager.I.Tiles.ContainsKey(TilePoint + Point.South + Point.East))
                    neighbours.Add(TileManager.I.Tiles[TilePoint + Point.South + Point.East]);
                if (TileManager.I.Tiles.ContainsKey(TilePoint + Point.South + Point.West))
                    neighbours.Add(TileManager.I.Tiles[TilePoint + Point.South + Point.West]);
            }

            return neighbours;
        }
    }
}