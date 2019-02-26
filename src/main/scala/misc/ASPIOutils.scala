package misc

/*
 * fol2asp
 *
 * Copyright 2016-2019 Matthias Nickles
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

import java.io._
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import scala.collection.mutable.ArrayBuffer

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import scala.language.postfixOps

import scala.sys.process._

object ASPIOutils {

  def externalCmdWithInput(cmd: String,
                           sendToStdinOpt: Option[Either[String, Array[Byte]]],
                           redirectOutputToFileOpt: Option[File] = None):
  Option[(Seq[String] /*<- stdout from process*/ , Seq[String] /*<- captured stderr*/ )] = {

    val errLines = ArrayBuffer[String]()

    def pipe2external(cmd: String): Object = if (sendToStdinOpt.isEmpty && redirectOutputToFileOpt.isEmpty)
      cmd lineStream_! ProcessLogger(line => errLines.append(line))
    else if (sendToStdinOpt.isEmpty)
      (cmd lineStream_! ProcessLogger(line => errLines.append(line))) #> redirectOutputToFileOpt.get
    else if (redirectOutputToFileOpt.isDefined)
      (cmd #< iterator2inputStream(sendToStdinOpt) lineStream_! ProcessLogger(line => errLines.append(line))) #> redirectOutputToFileOpt.get
    else
      cmd #< iterator2inputStream(sendToStdinOpt) lineStream_! ProcessLogger(line => errLines.append(line))
    // Note: some programs return non-zero codes even when completing correctly; we use lineStream_! to prevent the process to throw an exception in these cases.

    def iterator2inputStream(sendToStdinOpt: Option[Either[String, Array[Byte]]]): InputStream = {

      val pos = new PipedOutputStream

      val pis = new PipedInputStream(pos)

      sendToStdinOpt.foreach(toStdin => {

        val ba: Array[Byte] = (toStdin.right.getOrElse(toStdin.left.get.getBytes()))

        Future {  // we must do this asynchronously to avoid blocking for larger amounts of data

          pos.write(ba)

          pos.close

        }

      })

      pis

    }

    val r = pipe2external(cmd)

    if (r.isInstanceOf[Seq[String]])
      Some((r.asInstanceOf[Seq[String]], errLines))
    else
      None

  }

  def readAllLinesFromStdIn: Iterator[String] = {

    scala.io.Source.stdin.getLines

  }

  def slurpFromFile(fileName: String): String = {

    new String(Files.readAllBytes(Paths.get(fileName)), StandardCharsets.UTF_8)

  }

}