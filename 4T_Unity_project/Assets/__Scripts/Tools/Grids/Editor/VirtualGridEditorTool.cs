// Author: Pietro Polsinelli - http://designAGame.eu
// Twitter https://twitter.com/ppolsinelli
// All free as in free beer :-)

using UnityEngine;
using System.Collections;
using System.Collections.Generic;
using UnityEditor;

namespace OL
{
    public class VirtualGridEditorTool : EditorWindow
    {
        float tileSideSize;
        Transform center;
        int CentralGridX;
        int CentralGridY;
        bool isLeftLowerNotCentered;
        VirtualGridTile tilePrefab;
        int density;
        HashSet<Point> points;

        [MenuItem("Tools/OL/VirtualGridGenerator")]
        static void Init()
        {
            VirtualGridEditorTool window = (VirtualGridEditorTool)EditorWindow.GetWindow(typeof(VirtualGridEditorTool));
            window.Show();
        }

        void OnGUI()
        {
            GUILayout.Label("Creates a grid", EditorStyles.boldLabel);

            center = EditorGUILayout.ObjectField("center", center, typeof(Transform), true) as Transform;
            tilePrefab = EditorGUILayout.ObjectField("tilePrefab", tilePrefab, typeof(VirtualGridTile), true) as VirtualGridTile;

            tileSideSize = EditorGUILayout.FloatField("tileSideSize", tileSideSize);
            CentralGridX = EditorGUILayout.IntField("CentralGridX", CentralGridX);
            CentralGridY = EditorGUILayout.IntField("CentralGridY", CentralGridY);
            density = EditorGUILayout.IntField("density", density);
            isLeftLowerNotCentered = EditorGUILayout.Toggle("isLeftLower", isLeftLowerNotCentered);


            if (GUILayout.Button("create grid") && density>0)
            {
                points = new HashSet<Point>();
                
                int width = CentralGridX * 2 + (isLeftLowerNotCentered ? 0 : 1);
                int height = CentralGridY * 2 + (isLeftLowerNotCentered ? 0 : 1);

                Debug.Log("Create grid " + width + " " + height);

                //3 corners
                CreateAt(width - 1, height - 1);
                CreateAt(0, height - 1);
                CreateAt(width - 1, 0);
                CreateAt(CentralGridX, CentralGridY);
                
                for (int y = 0; y < height; y += density)
                {
                    for (int x = 0; x < width; x += density)
                    {
                        CreateAt(x, y);
                    }
                }
            }
        }

        void CreateAt(int x, int y)
        {
            if (points.Contains(new Point(x,y)))
                return;

            points.Add(new Point(x, y));

            VirtualGridTile newTile = PrefabUtility.InstantiatePrefab(tilePrefab) as VirtualGridTile;
            newTile.transform.SetParent(center, false);
            newTile.transform.localScale = new Vector3(tileSideSize, tileSideSize, tileSideSize);
            newTile.gameObject.name = x + " " + y;
            newTile.Label.text = x + " " + y;
            newTile.X = x;
            newTile.Y = y;
            int diffX = x - CentralGridX;
            int diffY = y - CentralGridY;

            float vX = center.position.x + (tileSideSize * diffX);
            float vY = center.position.y + (tileSideSize * diffY);
            newTile.transform.position = new Vector2(vX, vY);
        }
    }
}
