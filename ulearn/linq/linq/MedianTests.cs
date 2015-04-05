using NUnit.Framework;
using System;
using System.Collections.Generic;

namespace linq
{
    [TestFixture()]
    public class NUnitTestClass
    {
        public void FindMedianTest(IEnumerable<double> items, double fair)
        {
            try
            {
                Assert.AreEqual(fair, items.Median());
            }
            catch (ArgumentException)
            {
                Console.WriteLine("Too short sequence.");
            }
        }

        [Test()]
        public void OddCount()
        {
            List<double> items = new List<double>() { 1.0, 1.0, 2.0, 3.0, 3.0 };
            FindMedianTest(items, 2.0);
        }

        [Test()]
        public void EvenCount()
        {
            List<double> items = new List<double>() { 1.0, 1.0, 2.0, 3.0, 3.0, 4.0 };
            FindMedianTest(items, 2.5);
        }

        [Test()]
        public void EmptySequence()
        {
            List<double> items = new List<double>();
            FindMedianTest(items, 0);
        }
    }
}

