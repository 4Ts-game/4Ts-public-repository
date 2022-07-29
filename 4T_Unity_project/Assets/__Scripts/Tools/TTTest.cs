using System.Collections;
using System.Collections.Generic;

namespace OL
{
    public class TTTest
    {
        public static Dictionary<string, bool> TestFinNumOperations()
        {
            Dictionary<string, bool> tests = new Dictionary<string, bool>();

            var result = (int)TT.FinNum.Zero.ModifyBy(2) == 2;
            tests["(int)TT.FinNum.Zero.ModifyBy(2) == 2"] = result;

            result = TT.FinNum.Zero.ModifyBy(2) == TT.FinNum.Two;
            tests["TT.FinNum.Zero.ModifyBy(2) == TT.FinNum.Two"] = result;


            result = (int)TT.FinNum.Two.ModifyBy(2) == 3;
            tests["(int)TT.FinNum.Two.ModifyBy(2) == 3"] = result;

            result = TT.FinNum.Two.ModifyBy(2) == TT.FinNum.Three;
            tests["TT.FinNum.Two.ModifyBy(2) == TT.FinNum.Three"] = result;


            result = TT.FinNum.Two.ModifyBy(-3) == 0;
            tests["TT.FinNum.Two.ModifyBy(-3) == 0"] = result;

            result = TT.FinNum.Two.ModifyBy(-2) == TT.FinNum.Zero;
            tests["TT.FinNum.Two.ModifyBy(-2) == TT.FinNum.Zero"] = result;

            /*TT.FinNum before = TT.FinNum.Two;
            before.ModifyBy(1);
            result = before == TT.FinNum.Three;
            tests["TT.FinNum before = TT.FinNum.Two;before.ModifyBy(1);result = before == TT.FinNum.Three;"] = result;*/

            TT.FinNum before = TT.FinNum.Two;
            before.ModifyBy(1);
            result = before == TT.FinNum.Two;
            tests["before = TT.FinNum.Two;before.ModifyBy(1);result = before == TT.FinNum.Two;"] = result;

            return tests;
        }
    }
}
