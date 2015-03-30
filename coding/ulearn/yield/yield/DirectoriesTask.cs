using System;
using System.Collections.Generic;
using System.IO;

namespace yield
{
	public class FileData
	{
		public string FileExtension;
		public long Size;
	}

	public static class DirectoriesTask
	{
		public static IEnumerable<FileData> EnumerateAllFiles(DirectoryInfo directoryInfo, Random random)
		{
			//Fix me!
			yield break;
		}

		private static IEnumerable<T> Shuffle<T>(this T[] items, Random random)
		{
			//Fix me!
			yield break;
		}
	}
}