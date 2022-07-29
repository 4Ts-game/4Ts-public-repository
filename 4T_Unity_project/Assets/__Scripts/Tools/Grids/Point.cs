// Author: Pietro Polsinelli - http://designAGame.eu
// Twitter https://twitter.com/ppolsinelli
// License: WTFPL, all free as in free beer :-)
// Tested with Unity 5.6.1
// Created: 01 09 2016

using UnityEngine;

namespace OL
{
    public struct Point
    {
        public int X;
        public int Y;

        public static Point North = new Point(0, 1);
        public static Point South = new Point(0, -1);
        public static Point East = new Point(1, 0);
        public static Point West = new Point(-1, 0);

        public static Point Infinity = new Point(int.MaxValue, int.MaxValue);

        public Point(int gridX, int gridY)
        {
            X = gridX;
            Y = gridY;
        }

        public Point(Vector2 coords) : this((int)coords.x, (int)coords.y)
        {            
        }

        public override string ToString()
        {
            return X + " " + Y;
        }

        public static Point FromString(string ser)
        {
            int X = int.Parse(ser.Substring(0, ser.IndexOf(" ")));
            int Y = int.Parse(ser.Substring(ser.IndexOf(" ")));
            return new Point(X,Y);
        }

        public float Distance(Point point)
        {
            var distance = Mathf.Sqrt(Mathf.Pow(point.X - X, 2) + Mathf.Pow(point.Y - Y, 2));
            return distance;
        }

        public int DeltaXY(Point point)
        {
            return Mathf.Abs(X - point.X) + Mathf.Abs(Y - point.Y);
        }


        public static Point operator +(Point p1, Point p2)
        {
            return new Point(p1.X + p2.X, p1.Y + p2.Y);
        }

        public static bool operator ==(Point p1, Point p2)
        {
            return p1.Equals(p2);
        }

        public static bool operator !=(Point p1, Point p2)
        {
            return !(p1==p2);
        }
        
        public override int GetHashCode()
        {
            return (X+Y).GetHashCode();
        }

        public override bool Equals(object obj)
        {
            if (!(obj is Point))
                return false;

            Point p = (Point)obj;
            return p.X == X && p.Y == Y;
        }
        
    }
}