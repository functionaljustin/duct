package examples

import java.io.File
import java.nio.file.{Path, Paths}
import scala.util.matching.Regex
import org.functionaljustin.duct.typeclasses.comonad.nonEmptyListComonad
import org.functionaljustin.duct.datatypes.LazyList
import org.functionaljustin.duct.typeclasses.functor.given_Functor_NonEmptyList

// Getting cleaner but needs some more work before video
// Note that uses LazyList with coflatMap which requires some explanation
// would be much neater if you could make a NonEmptyLazyList

object FindProject extends App:
  // Path helper functions to replace FilenameUtils
  object PathUtils:
    def getPath(path: String): String = 
      val file = new File(path)
      val parentPath = file.getParent()
      if parentPath == null then "" else parentPath + File.separator
      
    def getPrefix(path: String): String =
      if path.startsWith(File.separator) then File.separator
      else if path.matches("[A-Za-z]:.*") then path.substring(0, 2) // Windows drive prefix
      else ""

  // Given a path use coflatmap to get all the parent path names and iterate over them
  // finding the first folder that has files matching a pattern
  def findRoot(path: File, rootFiles: Set[Regex]): Option[File] = {
    val canonicalPath = path.getCanonicalPath()
    val pathString = PathUtils.getPath(canonicalPath)
    val fullPath = PathUtils.getPath(pathString)
    val pathPrefix = if(PathUtils.getPrefix(pathString).length() == 0) 
                       File.separator
                     else 
                       PathUtils.getPrefix(pathString)

    // TODO add intersperse to List as an extension method
    def restorePathName(path: List[String]): File =
      val restoredPath = path.toList.reverse.mkString(pathPrefix, File.separator, File.separator)
      new File(restoredPath)

    // Given a path File search it for the targets and return an Option of the find result
    def containsRoot(path: File): Option[File] = {
      // TODO list files in path, match with any regex in the set
      // For now ignore difference between directory and file
      println(s"Search path $path")
      val files = path.listFiles()
      if files == null then None
      else files.toList.find { file => 
          rootFiles.find { regex =>
              val res = regex.matches(file.getName())
              if res then 
                println(s"found ${file.getName()}")
              res
          }.isDefined
       }
    }

    val candidatePaths = fullPath.split(File.separatorChar).toList.reverse

    LazyList(candidatePaths*).coflatMap{
      l => 
        val path = restorePathName(l.toList)
        println(s"explore path $path")
        containsRoot(path)
    }.first(_.isDefined).flatten   
  }

  val cd = new File("/Users/justin.heyes-jones/projects/path-to-comonads/src/main/scala/org/justinhj/Pathtocomonads.scala")
  val found = findRoot(cd, Set("build.sbt".r, "Cargo.toml".r))
  found match {
    case Some(f) =>
      println(s"Root is $f")
    case None =>
      println("No root found")
  }
