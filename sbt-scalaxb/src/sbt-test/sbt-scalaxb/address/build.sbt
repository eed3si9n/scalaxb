lazy val root = (project in file(".")).
  enablePlugins(ScalaxbPlugin).
  settings(
    name := "mavenxsd",
    scalaxbAutoPackages in (Compile, scalaxb) := true,
    scalaxbGenerateMutable in (Compile, scalaxb) := true
  )
