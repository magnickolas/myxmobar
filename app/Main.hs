import Xmobar
import Xmobar.Plugins.Monitors.MyAlsa (MyAlsa (..))

config :: Config
config =
  defaultConfig
    { font = "Terminus Bold 17",
      additionalFonts =
        [ "Twemoji 17",
          "Symbols Nerd Font Mono 18",
          "Symbols Nerd Font Mono 17",
          "Symbols Nerd Font Mono 16"
        ],
      textOffsets = [0, 0, 0, -2],
      bgColor = "#222222",
      fgColor = "#bdbdbd",
      position = BottomH 33,
      border = NoBorder,
      borderColor = "#646464",
      -- layout
      sepChar = "%",
      alignSep = "}{",
      template =
        let width = 10
            hspace = "<hspace=" ++ show width ++ "/>"
            sep = "<fc=#666666>|</fc>"
            lsep = hspace ++ sep
            rsep = sep ++ hspace
            lrsep = hspace ++ sep ++ hspace
         in "<hspace=3/>"
              ++ "%UnsafeXMonadLog%"
              ++ "}{ "
              ++ "%UGTB%"
              ++ lrsep
              ++ "%memory%"
              ++ lrsep
              ++ "%diskquote%"
              ++ lrsep
              ++ "%date%"
              ++ lsep
              ++ "%wi%"
              ++ rsep
              ++ "%myalsa:default:Master%"
              ++ lrsep
              ++ "%mpris2%"
              ++ "  "
              ++ "<action=`x layout extra` button=3><action=`x layout switch`>%kbd%</action></action>"
              ++ "  "
              ++ "<fn=2>%battery-status%</fn>"
              ++ "  "
              ++ "%pomo%"
              ++ " ",
      lowerOnStart = False,
      hideOnStart = False,
      allDesktops = True,
      overrideRedirect = True,
      pickBroadest = False,
      persistent = True,
      commands =
        [ Run $
            MyAlsa
              "default"
              "Master"
              [ "--template",
                "<action=`xdotool key XF86AudioMute`>"
                  ++ "<action=`xdotool key XF86AudioRaiseVolume` button=4>"
                  ++ "<action=`xdotool key XF86AudioLowerVolume` button=5>"
                  ++ "<fn=4><volumecpat></fn>"
                  ++ ("</action>" ++ "</action>" ++ "</action>"),
                "--",
                "--volume-char-pattern",
                replicate 1 '\xfa7e' ++ replicate 1 '\xfa7f' ++ replicate 1 '\xfa7d',
                "--offChar",
                "<fc=#606060>\xfa80</fc>"
              ],
          Run $
            Mpris2
              "spotify"
              [ "-t",
                "<action=`xdotool key XF86AudioPrev` button=5>"
                  ++ "<action=`xdotool key XF86AudioNext` button=4>"
                  ++ "<action=`xdotool key XF86AudioPlay`>"
                  ++ "<artist> - <title>"
                  ++ ("</action>" ++ "</action>" ++ "</action>"),
                "-T",
                "25",
                "-E",
                ".."
              ]
              3,
          Run $
            WeatherX
              "UGTB"
              [ ("clear", "☀ "),
                ("sunny", "☀ "),
                ("mostly clear", "🌤"),
                ("mostly sunny", "🌤"),
                ("partly sunny", "⛅"),
                ("fair", "🌑"),
                ("cloudy", "☁"),
                ("overcast", "☁"),
                ("partly cloudy", "⛅"),
                ("mostly cloudy", "🌧"),
                ("considerable cloudiness", "⛈")
              ]
              [ "-t",
                "<fn=1><skyConditionS></fn> <tempC>°C",
                "-L",
                "5",
                "-H",
                "10",
                "--normal",
                "lightgray",
                "--high",
                "orange",
                "--low",
                "lightblue"
              ]
              9000,
          Run $
            Wireless
              ""
              [ "--template",
                "<ssid>",
                "--"
              ]
              10,
          Run $
            Memory
              [ "--template",
                "<fn=2>\xf85a</fn> <usedratio>%",
                "--Low",
                "20",
                "--High",
                "90",
                "--low",
                "darkgreen",
                "--high",
                "darkred"
              ]
              10,
          -- %a, %d %b %Y <fn=0>\xeb7c</fn> %H:%M</fc>
          Run $ Date "<fc=#ABABAB>%a %d <fn=0>\xeb7c</fn> %H:%M:%S</fc>" "date" 10,
          Run $
            Kbd
              [ ("us", "<fn=1>🇬🇧</fn>"),
                ("ru", "<fn=1>🇷🇺</fn>"),
                ("ua", "<fn=1>🇺🇦</fn>")
              ],
          Run $ Com "x" ["battery-status"] "battery-status" 7,
          Run $ Com "x" ["pomo"] "pomo" 10,
          Run $ Com "/home/magnickolas/.xmobar/widgets/disk.sh" [] "diskquote" 300,
          Run UnsafeXMonadLog
        ]
    }

main :: IO ()
main = xmobar config
