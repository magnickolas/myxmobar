import Xmobar
import Xmobar.Plugins.Monitors.MyAlsa (MyAlsa (..))

config :: Config
config =
  defaultConfig
    { font = "Terminus Bold 17", -- fn=0
      additionalFonts =
        [ "Twemoji 17", -- fn=1
          "Symbols Nerd Font Mono 18", -- fn=2
          "Symbols Nerd Font Mono 17", -- fn=3
          "Symbols Nerd Font Mono 16" -- fn=4
        ],
      textOffsets = [0, 0, 0, -2],
      bgColor = "#222222",
      fgColor = "#bdbdbd",
      position = BottomHM 33 0 132 0 0,
      border = NoBorder,
      borderColor = "#646464",
      -- layout
      sepChar = "%",
      alignSep = "}{",
      template = statusBar,
      lowerOnStart = False,
      hideOnStart = False,
      allDesktops = True,
      overrideRedirect = True,
      pickBroadest = False,
      persistent = True,
      commands =
        [ Run alsa,
          Run spotify,
          Run openweathermap,
          Run wifi,
          Run ram,
          Run time,
          Run layout,
          Run batteryStatus,
          Run pomo,
          Run disk,
          Run xmonadLog
        ]
    }

statusBar =
  "<hspace=3/>"
    ++ "%UnsafeXMonadLog%"
    ++ "}{ "
    ++ ("%openweathermap%" ++ lrsep)
    ++ ("%memory%" ++ lrsep)
    ++ ("%diskquote%" ++ lrsep)
    ++ ("%date%" ++ lsep)
    ++ ("%wi%" ++ rsep)
    ++ "%myalsa:default:Master%"
    ++ "%mpris2%"
    ++ (lrsep ++ "<action=`x layout extra` button=3><action=`x layout switch`>%kbd%</action></action>" ++ "  ")
    ++ "<fn=2>%battery-status%</fn>"
    ++ ("%pomo%" ++ hspace)

hspace = "<hspace=" ++ show 10 ++ "/>"

sep = "<fc=#666666>|</fc>"

lsep = hspace ++ sep

rsep = sep ++ hspace

lrsep = hspace ++ sep ++ hspace

alsa =
  MyAlsa
    "default"
    "Master"
    ( [ "-t",
        "<action=`xdotool key XF86AudioMute`>"
          ++ "<action=`xdotool key XF86AudioRaiseVolume` button=4>"
          ++ "<action=`xdotool key XF86AudioLowerVolume` button=5>"
          ++ "<fn=4><volumecpat></fn>"
          ++ ("</action>" ++ "</action>" ++ "</action>"),
        "--"
      ]
        ++ ["--volume-char-pattern", replicate 1 '\xf057f' ++ replicate 1 '\xf0580' ++ replicate 1 '\xf057e']
        ++ ["--offChar", "<fc=#606060>\xf0581</fc>"]
    )

spotify :: Monitors
spotify =
  Mpris2
    "spotify"
    ( [ "-t",
        lrsep
          ++ "<action=`xdotool key XF86AudioPrev` button=5>"
          ++ "<action=`xdotool key XF86AudioNext` button=4>"
          ++ "<action=`xdotool key XF86AudioPlay`>"
          ++ "<artist> - <title>"
          ++ ("</action>" ++ "</action>" ++ "</action>")
      ]
        ++ ["-T", "25"]
        ++ ["-E", ".."]
        ++ ["-x", ""]
    )
    3

wifi :: Monitors
wifi =
  Wireless
    ""
    ["-t", "<ssid>"]
    10

ram :: Monitors
ram =
  Memory
    ( ["-t", "<fn=2>\xf035b</fn><hspace=7/><usedratio>"]
        ++ ["--Low", "20"]
        ++ ["--High", "90"]
        ++ ["--low", "green"]
        ++ ["--high", "darkred"]
    )
    10

time :: Date
time = Date "<fc=#ABABAB>%a %d <fn=3>\xeb7c</fn> %H:%M:%S</fc>" "date" 10

layout :: Kbd
layout =
  Kbd
    [ ("us", "<fn=1>🇬🇧</fn>"),
      ("ru", "<fn=1>🇷🇺</fn>"),
      ("ua", "<fn=1>🇺🇦</fn>")
    ]

batteryStatus :: Command
batteryStatus = Com "x" ["battery-status"] "battery-status" 7

pomo :: Command
pomo = Com "x" ["pomo"] "pomo" 10

disk :: Command
disk = Com "/home/magnickolas/.xmobar/widgets/disk.sh" [] "diskquote" 300

volume :: Command
volume =
  Com "/home/magnickolas/.xmobar/widgets/volume.sh" [] "volume" 10

openweathermap :: Command
openweathermap =
  Com "/home/magnickolas/.xmobar/widgets/openweathermap.sh" [] "openweathermap" 3000

xmonadLog :: XMonadLog
xmonadLog = UnsafeXMonadLog

main :: IO ()
main = xmobar config
