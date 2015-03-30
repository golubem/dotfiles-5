using System;
using System.Collections.Generic;

namespace yield
{
	public class DataPoint
	{
		public double X;
		public double OriginalY;
		public double ExpSmoothedY;
		public double AvgSmoothedY;
	}
	
	public static class DataTask
	{
		public static IEnumerable<DataPoint> GetData(Random random)
		{
			return GenerateOriginalData(random).SmoothExponentialy(0.8).MovingAverage(10);
		}

		public static IEnumerable<DataPoint> GenerateOriginalData(Random random)
		{
			double y;
			int x = 0;
			while (true)
			{
				x++;
				y = 10 * ((x / 50) % 2) + (random.NextDouble() - 0.5);
				yield return new DataPoint { X = x, OriginalY = y };
			}
		}

		public static IEnumerable<DataPoint> SmoothExponentialy(this IEnumerable<DataPoint> data, double alpha)
		{
			//Fix me!
			return data;
		}

		public static IEnumerable<DataPoint> MovingAverage(this IEnumerable<DataPoint> data, int windowWidth)
		{
			//Fix me!
			return data;
		}
	}
}