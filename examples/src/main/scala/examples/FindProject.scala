package examples

import java.io.File
import org.apache.commons.io.FilenameUtils
import scala.util.matching.Regex
import org.justinhj.duct.datatypes.NonEmptyList
import org.justinhj.duct.typeclasses.comonad.nonEmptyListComonad
import org.justinhj.duct.datatypes.LazyList
import org.justinhj.duct.typeclasses.functor.given_Functor_NonEmptyList

object FindProject extends App:
  // Given a path use coflatmap to get all the parent path names and iterate over them
  // finding the first folder that has files matching a pattern
  def findRoot(path: File, rootFiles: Set[Regex]): Option[File] = {
    val pathString = FilenameUtils.getPath(path.getCanonicalPath())
    val fullPath = FilenameUtils.getPath(pathString)
    val pathPrefix = if(FilenameUtils.getPrefix(pathString).length() == 0) 
                       File.separator
                     else 
                       FilenameUtils.getPrefix(pathString)

    // TODO add intersperse to List as an extension method
    def restorePathName(path: NonEmptyList[String]): File =
      val restoredPath = path.toList.reverse.mkString(pathPrefix, File.separator, File.separator)
      new File(restoredPath)

      // TODO needs coflatMap
    val candidatePaths = fullPath.split(File.separatorChar).toList.reverse
    val nel = NonEmptyList.fromSeq(candidatePaths).get
                          
    val allPaths = nel.coflatMap(identity).map(restorePathName).toList

    println(s"found ${allPaths.length} candidate paths\n$allPaths")
    val lazyPaths = LazyList(allPaths: _*)

    // TODO this is nice and all
    lazyPaths.first(path => {
      // TODO list files in path, match with any regex in the set
      // For now ignore difference between directory and file
      println(s"Search path $path")
      val files = path.listFiles().toList
      files.find{ file => 
        rootFiles.find {
          regex =>
            //println(s"regex $regex match ${file.getName()} ${regex.matches(file.getName())}")
            val res = regex.matches(file.getName())
            if res then 
              println(s"found ${file.getName()}")
            res
        }.isDefined
      }.isDefined
    })
  }

  val cd = new File("/Users/justin.heyes-jones/projects/path-to-comonads/src/main/scala/org/justinhj/Pathtocomonads.scala")
  val found = findRoot(cd, Set("build.sbt".r, "Cargo.toml".r))
  found match {
    case Some(f) =>
      println(s"Root is $f")
    case None =>
      println("No root found")
  }