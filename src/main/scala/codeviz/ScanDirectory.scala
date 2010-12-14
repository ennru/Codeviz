package codeviz

import java.io.{File, FilenameFilter, FileFilter, FileReader, BufferedReader}

object ScanDirectory {

  def apply(dir: File, 
            acceptedFiles: (File) => Boolean,
  			operation: (File) => Unit): Unit = {
    val fileFilter = new FileFilter() {
      override def accept(file: File): Boolean = {
        acceptedFiles(file)
      }
    }
    val fileArray = dir.listFiles(fileFilter)
    if (fileArray != null) {
    	fileArray.foreach(operation)
    }
  }

}
