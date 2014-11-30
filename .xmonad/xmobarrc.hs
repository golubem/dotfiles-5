Config {
    font = "xft:Tewi:pixelsize=10"
  , bgColor = "#080808"
  , fgColor = "#c2c2c2"
  , position = TopW C 100
  , border = NoBorder
  , commands = [
         Run Date "%H:%M " "date" 10
       , Run MultiCpu ["-t", "<total>%", "-L", "3", "#a9acb6"] 10
       , Run Memory ["-t", "<usedratio>%"] 10
       , Run Com "/home/mathcrosp/.xmonad/scripts/wirel.sh" [] "wifi" 30
       , Run Com "/home/mathcrosp/.xmonad/scripts/vols.sh" [] "vol" 10
       , Run Com "/home/mathcrosp/.xmonad/scripts/mute.sh" [] "mute" 10
       , Run StdinReader
       , Run Battery ["-t", " <left>%", "-L", "20", "-H", "75", "-h", "#90ee90", "-n", "#90ee90", "-l", "#cd2626"] 10
  ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = "  %StdinReader% }{<fc=#8a8a8a> Vol.</fc> %vol%<fc=#e2e2e2> |</fc><fc=#8a8a8a> Wlan</fc> %wifi%<fc=#e2e2e2> |</fc><fc=#d1d1d1> ⭥ CPU </fc>%multicpu%<fc=#e2e2e2> |</fc><fc=#d1d1d1> ⭦ Mem. </fc>%memory%<fc=#e2e2e2> |</fc><fc=#8a8a8a> ⭫ Bat.</fc>%battery%<fc=#e2e2e2> |</fc><fc=#d1d1d1> ⭧ </fc>%date%<fc=#8a8a8a></fc>        "
}

