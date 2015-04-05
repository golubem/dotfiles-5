Config {
    font = "xft:Tewi:pixelsize=10"
  , bgColor = "#080808"
  , fgColor = "#f1f1f1"
  , position = TopW C 100
  , border = NoBorder
  , commands = [
         Run Date "%H:%M" "date" 10
       , Run MultiCpu ["-t", "<total>%", "-L", "3", "#a9acb6"] 10
       , Run Memory ["-t", "<usedratio>%"] 10
       , Run Com "/home/mathcrosp/.xmonad/scripts/vols.sh" [] "vol" 10
       , Run Com "/home/mathcrosp/.xmonad/scripts/mute.sh" [] "mute" 10
       , Run Com "/home/mathcrosp/.xmonad/scripts/temp.sh" [] "temp" 10
       , Run Com "/home/mathcrosp/.xmonad/scripts/layout.sh" [] "layout" 10
       , Run StdinReader
       , Run Battery ["-t", " <left>%", "-L", "20", "-H", "75", "-h", "#90ee90", "-n", "#90ee90", "-l", "#cd2626"] 10
  ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = "   %StdinReader%   }{ [VOL %vol%] [CPU %multicpu%] [MEM %memory%] [%temp%] [BAT%battery%] [%layout%] [%date%]        "
}

