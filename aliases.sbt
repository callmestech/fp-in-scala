import Util._

addCommandAlias(
  "checkFmt",
  "; scalafmtSbtCheck; scalafmtCheckAll"
)
addCommandAlias(
  "fmt",
  "; scalafmtSbt; scalafmt"
)

onLoadMessage +=
  s"""|
      |╭─────────────────────────────────╮
      |│     List of defined ${styled("aliases")}     │
      |├─────────────┬───────────────────┤
      |│ ${styled("checkFmt")}    │    check format   │
      |│ ${styled("fmt")}         │          format   │
      |╰─────────────┴───────────────────╯""".stripMargin
