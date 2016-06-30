using System;
using System.Collections.Generic;
using System.Linq;

namespace fifteen
{
	public class Game
	{
        private Dictionary <Tuple<int, int>, int> map;
        private int mapSize = map.Count();
        private Dictionary <int, Tuple<int, int>> positions;

		public Game(params int[] items)
		{
			validateArgs(items);
			this.map = mapToDictionary(items);
            fillPositions();
		}

        public int this[int x, int y]
        {
            get { this.map[Tuple.Create(x, y)]; }
            set { this.map[Tuple.Create(x, y)]; }
        }

        public Tuple<int, int> GetLocation(int value)
        {
            return this.positions[value];
        }

        public void Shift(int value)
        {
            var targetPosition = positions[value];
            var zeroPosition = positions[0];
            var dx = Math.Abs(zeroPosition.Item1 - targetPosition.Item1);
            var dy = Math.Abs(zeroPosition.Item2 - targetPosition.Item2);
            if (dx * dx + dy * dy != 1)
                throw new ArgumentException();

        }

		private void validateArgs(int[] items)
		{
            if (!(isPerfectSquare(items.Length) && makesAllNumsSequence(items)))
                throw new ArgumentException();
		}

        private bool isPerfectSquare(double num)
        {
            var countSqrt = Math.Sqrt(num);
            return Math.Abs(Math.Ceiling(countSqrt) - Math.Floor(countSqrt)) < Double.Epsilon;
        }

        private bool makesAllNumsSequence(double[] nums)
        {
            if (nums.Max >= nums.Length)
                return false;
            return nums.Distinct.Count() == nums.Length;
        }

        private Dictionary<T, Tuple<int, int>> mapToDictionary<T>(T[] items)
		{
			var dimensionsCount = (int)Math.Sqrt(items.Length);
			T[][] chunks = items
				.Select((s, i) => new { Value = s, Index = i })
				.GroupBy(x => x.Index / dimensionsCount)
				.Select(grp => grp.Select(x => x.Value).ToArray())
				.ToArray();
			T[,] matrix = new T[dimensionsCount, dimensionsCount] { chunks };
            var result = new Dictionary<T, Tuple<int, int>>();
            for (var x = 0; x < dimensionsCount; x++)
                for (var y = 0; y < dimensionsCount; y++)
                    result[Tuple.Create(x, y)] = matrix[x, y];
			return result;
		}

        private void fillPositions()
        {
            for (var x = 0; x < this.mapSize; x++)
                for (var y = 0; y < this.mapSize; y++)
                {
                    var current = this.map[Tuple.Create(x, y)];
                    this.positions[current] = Tuple.Create(x, y);
                }   
        }
	}
}
