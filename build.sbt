name := "scalaxb"

seq(ProguardPlugin.proguardSettings :_*)

proguardOptions ++= Seq( keepAllScala )

