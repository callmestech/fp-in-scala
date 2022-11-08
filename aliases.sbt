import Util._

addCommandAlias(
  "checkFmt",
  "; scalafmtSbtCheck; scalafmtCheckAll"
)

onLoadMessage +=
  s"""|
      |╭─────────────────────────────────╮
      |│     List of defined ${styled("aliases")}     │
      |├─────────────┬───────────────────┤
      |│ ${styled("checkFmt")}    │    check format   │
      |╰─────────────┴───────────────────╯""".stripMargin
