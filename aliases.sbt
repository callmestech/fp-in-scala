import Util._

addCommandAlias(
  "check",
  "; scalafmtSbtCheck; scalafmtCheckAll"
)

onLoadMessage +=
  s"""|
      |╭─────────────────────────────────╮
      |│     List of defined ${styled("aliases")}     │
      |├─────────────┬───────────────────┤
      |│ ${styled("check")}       │  check format     │
      |╰─────────────┴───────────────────╯""".stripMargin