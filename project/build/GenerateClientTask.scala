import sbt._
import java.io.{File}

trait GenerateClientTask extends DefaultWebProject {
  def localizationsPath = path("project") / "build" / "localizations"
  def localizationPath = localizationsPath / "localization.xml"
  
  abstract class Lang
  case object En extends Lang { override def toString = "en" }
  case object Ja extends Lang { override def toString = "ja" }
  
  lazy val generateClient = generateClientTask
  
  val tmp = new File("tmp")
  
  def generateClientTask = task {
    if (tmp.exists) deleteAll(tmp)
    tmp.mkdir
    
    val localization = scala.xml.XML.loadFile(localizationPath.asFile)
    
    def localize(key: String, lang: Lang) = 
      localization.child.toList filter { x =>
        (x \ "@key").text == key && (x \ "@lang").text == lang.toString } match {
        case x :: xs => (x \ "@value").text 
        case Nil => key
      }
    
    def script(lang: Lang) = """<html>
<head>
  <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.2.6/jquery.min.js" type="text/javascript"></script>

</head>
<body>
<!-- begin -->
<form id="frmCompile" enctype="multipart/form-data" method="POST">
<ul>
  <li class='addargfli'>""" + localize("schema file", lang) + """ <input type='file' name='argf' class='inputArg'/> <a href='javascript:void(0)' class='addarg' onclick='addArgfClick();'>add</a></li>
  <li class='addargli'>""" + localize("schema URL", lang) + """ <input type='text' name='arg' size='100' class='inputArg'/> <a href='javascript:void(0)' class='addarg' onclick='addArgClick();'>add</a></li>
  <li>""" + localize("default package", lang) + """ <input type='text' name='defaultPackageName' size='30' class='inputOption'/></li>
  <li class='addnamespaceli'>""" + localize("namespace URI", lang) + """ <input type='text' name='namespaceURI' size='30' class='inputOption'/> """ + 
    localize("package", lang) + """<input type='text' name='packageName' size='30' class='inputOption'/> <a href='javascript:void(0)' class='addarg' onclick='addNamespaceClick();'>add</a></li>
  <li>""" + localize("class prefix", lang) + """ <input type='text' name='classPrefix' size='30' class='inputOption'/></li>
  <li>""" + localize("param prefix", lang) + """ <input type='text' name='paramPrefix' size='30' class='inputOption'/></li>
  <li class='addwrapcontentsli'>""" + localize("wrap contents", lang) + """ <input type='text' name='wrapContents' size='30' class='inputOption'/> <a href='javascript:void(0)' class='addarg' onclick='addWrapContentsClick();'>add</a></li>
  <li><button id="btnCompile" type="button">""" + localize("compile", lang) + """</button></li>
</ul>
</form>
<script type="text/javascript">
  // var baseURL = "http://localhost:8080/compile/";
  var baseURL = "http://scalaxb.appspot.com/compile/";
  
  function addNamespaceClick() {
    $(function() {
      $(".addnamespaceli:last").after("<li class='addnamespaceli'>""" + localize("namespace URI", lang) + """ <input type='text' name='namespaceURI' size='30' class='inputOption'/> """ +
        localize("package", lang) + """<input type='text' name='packageName' size='30' class='inputOption'/> <a href='javascript:void(0)' class='addarg' onclick='addNamespaceClick();'>add</a></li>") 
     });
     return false;      
  }
  
  function addArgfClick() {
    $(function() {
      $(".addargfli:last").after("<li class='addargfli'>""" + localize("schema file", lang) + """ <input type='file' name='argf' class='inputArg'/> <a href='javascript:void(0)' class='addarg' onclick='addArgfClick();'>add</a></li>") 
     });
     return false;
  }

  function addArgClick() {
    $(function() {
      $(".addargli:last").after("<li class='addargli'>""" + localize("schema URL", lang) + """ <input type='text' name='arg' size='100' class='inputArg'/> <a href='javascript:void(0)' class='addarg' onclick='addArgClick();'>add</a></li>") 
     });
     return false;
  }
  
  function addWrapContentsClick() {
    $(function() {
      $(".addwrapcontentsli:last").after("<li class='addwrapcontentsli'>""" + localize("wrap contents", lang) + """ <input type='text' name='wrapContents' size='30' class='inputOption'/> <a href='javascript:void(0)' class='addarg' onclick='addWrapContentsClick();'>add</a></li>") 
     });
     return false;
  }

  $(function() {
     $("#btnCompile").click(function() {
       var count = 0;
       var fileName;
       var ext = new RegExp("([\\.]\\w+)$");
       
       $(".inputArg").each(function(index) {
          if ($(this).val() != "") {
            fileName = fileName ||
              $(this).val().split("\\").pop().split("/").pop().replace(ext, ".scala");
            count++;
          }
       });
       
       switch (count) {
         case 0: 
           alert("at least one arg is required.");
           return;
         // case 1: break;
         default:
           fileName = fileName.replace(ext, ".zip");
       } // switch
       
       var frmCompile = $("#frmCompile");
       frmCompile.attr("action", baseURL + fileName);
       frmCompile.submit();
     });
   });                                  
</script>
<!-- end -->
</body>
</html>""" // "
    
    writeFile(new File(tmp, "test.en.html"), script(En))
    writeFile(new File(tmp, "test.ja.html"), script(Ja))
    
    None
  }
  
  private def deleteAll(file: File): Boolean = {
    if (file.isDirectory) {
      val children = file.listFiles
      if (children != null)
        children.foreach(deleteAll(_))
    }
    file.delete
  }
  
  private def writeFile(file: File, content: String) = {
    import java.io._
    
    if (file.exists() && !file.canWrite()) error("File " + file + " is not writable")
    val writer = new PrintWriter(new OutputStreamWriter(new FileOutputStream(file), "UTF-8"));
    try {
      writer.write(content)
    }
    finally {
      writer.close()
    }
  }
}
