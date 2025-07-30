// Copyright 2024-2025 Forja Team
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package forja.manip

import forja.*
import forja.source.{Source, SourceRange}
import forja.dsl.*
import forja.langs.tla.{TLAReader, TLAParser}

import java.net.{InetSocketAddress, Socket}
import java.nio.charset.StandardCharsets
import java.util.concurrent.LinkedBlockingQueue
import scala.collection.mutable
import scala.compiletime.uninitialized

class DebugAdapterTests extends munit.FunSuite:
  case class DAPMessage(
      messageType: String,
      content: String,
      timestamp: Long = System.currentTimeMillis(),
  )

  class DAPClient(host: String, port: Int) extends AutoCloseable:
    private val socket: Socket = Socket()
    private var seqNum = 1
    private val asyncQueue = new LinkedBlockingQueue[DAPMessage]()
    @volatile private var running = false
    private var readerThread: Thread = uninitialized

    def connect(): Unit =
      socket.connect(InetSocketAddress(host, port), 5000)
      running = true
      readerThread = new Thread(
        () => {
          try {
            while (running && !socket.isClosed) {
              val msg = readResponse()
              asyncQueue.put(msg)
            }
          } catch {
            case _: java.io.EOFException | _: java.net.SocketTimeoutException |
                _: ujson.IncompleteParseException =>
            // Normal shutdown
            case ex: Exception =>
              println(s"[DAPClient] Reader thread exception: $ex")
          }
        },
        "dap-client-reader",
      )
      readerThread.setDaemon(true)
      readerThread.start()

    def close(): Unit =
      running = false
      if ((readerThread: AnyRef) ne null) readerThread.interrupt()
      socket.close()

    def sendRequest(command: String, args: ujson.Value = ujson.Obj()): Unit =
      val requestObj = ujson.Obj(
        "seq" -> seqNum,
        "type" -> "request",
        "command" -> command,
        "arguments" -> args,
      )
      val payloadBytes = ujson.writeToByteArray(requestObj)
      socket.getOutputStream.write(
        s"Content-Length: ${payloadBytes.length}\r\n\r\n"
          .getBytes(StandardCharsets.UTF_8),
      )
      socket.getOutputStream.write(payloadBytes)
      seqNum += 1

    def takeNMessages(n: Int): Seq[DAPMessage] =
      val messages = mutable.Buffer[DAPMessage]()
      var count = 0
      while count < n do
        val msg = asyncQueue.take()
        if msg != null then
          messages += msg
          count += 1
      messages.toSeq

    private def readResponse(): DAPMessage =
      val input = socket.getInputStream
      val buffer = StringBuilder()
      var contentLength = -1

      while contentLength == -1 do
        val read = input.read()
        if read == -1 then
          // If nothing was read, treat as normal shutdown, not a protocol error
          socket.close()
          if buffer.isEmpty then
            throw new java.io.EOFException(
              "Socket closed while reading headers (empty buffer)",
            )
          else
            // Only fail if there was partial, non-header data
            fail(
              "Socket closed with partial, non-header data: " + buffer.toString,
            )
        val char = read.toChar
        buffer.append(char)
        if buffer.toString.endsWith("\r\n\r\n") then
          val headers = buffer.toString
          headers match
            case s if s.contains("Content-Length:") =>
              val lengthPattern = """Content-Length: (\d+)""".r
              lengthPattern.findFirstMatchIn(headers) match
                case Some(m) => contentLength = m.group(1).toInt
                case None =>
                  socket.close()
                  fail("Malformed DAP header: Content-Length not found")
            case _ =>
              socket.close()
              fail("Malformed DAP header: Content-Length not found")

      val jsonBytes = Array.ofDim[Byte](contentLength)
      val bytesRead = input.readNBytes(jsonBytes, 0, contentLength)
      if bytesRead != contentLength then
        socket.close()
        fail(s"Expected $contentLength bytes, got $bytesRead")
      val json = String(jsonBytes, StandardCharsets.UTF_8)

      val js = ujson.read(json)
      val messageType =
        js.obj.get("type").map(_.str) match
          case Some("response") => "response"
          case Some("event")    => "event"
          case _                => "unknown"

      val dapMessage = DAPMessage(messageType, json)
      dapMessage

  val debugAdapterFixture = FunFixture[DebugAdapter](
    setup = { _ =>
      val host = "localhost"
      val port = 4711
      val adapter = DebugAdapter(host, port)
      Thread.sleep(1000)
      adapter
    },
    teardown = { adapter =>
      adapter.close()
    },
  )

  def createTestTLASource(): Source =
    val tlaContent = """
                       |---- MODULE TestModule ----
                       |EXTENDS Naturals
                       |
                       |VARIABLE x
                       |
                       |Init == x = 0
                       |
                       |Next == x' = x + 1
                       |
                       |Spec == Init /\ [][Next]_x
                       |
                       |====
    """.stripMargin

    Source.fromString(tlaContent)

  debugAdapterFixture.test(
    "handles Step Over request with actual compiler execution",
  ) { debugAdapter =>
    val client = DAPClient("localhost", 4711)
    try {
      client.connect()

      val compilerThread = new Thread(() => {
        val src = createTestTLASource()
        val top = TLAReader(SourceRange.entire(src))
        instrumentWithTracer(debugAdapter) {
          TLAParser(top)
        }
      })

      compilerThread.start()

      // Send all requests up front, without blocking for responses
      client.sendRequest(
        "initialize",
        ujson.Obj(
          "clientID" -> "test",
          "adapterID" -> "forja",
        ),
      )
      var messages = client.takeNMessages(1)
      assert(messages.nonEmpty, "Expected an initialize response")
      assert(
        messages.exists(_.messageType == "response"),
        "Expected an initialize response",
      )
      println(s"${messages(0).messageType}: ${messages(0).content}\n")

      client.sendRequest("attach")
      messages = client.takeNMessages(2)
      assert(messages.nonEmpty, "Expected an initialize response")
      assert(
        messages.exists(_.messageType == "response"),
        "Expected an attach response",
      )
      println(s"${messages(0).messageType}: ${messages(0).content}")
      assert(messages.exists(_.messageType == "event"), "Expected a next event")
      println(s"${messages(1).messageType}: ${messages(1).content}\n")

      client.sendRequest("next", ujson.Obj("threadId" -> 1))
      messages = client.takeNMessages(3)
      assert(messages.nonEmpty, "Expected a next response")
      assert(
        messages.exists(_.messageType == "response"),
        "Expected a next response",
      )
      println(s"${messages(0).messageType}: ${messages(0).content}")
      assert(
        messages.exists(_.messageType == "event"),
        "Expected a loadedSource event",
      )
      println(s"${messages(1).messageType}: ${messages(1).content}")
      assert(
        messages.exists(_.messageType == "event"),
        "Expected a stopped event",
      )
      println(s"${messages(2).messageType}: ${messages(2).content}\n")

      client.sendRequest("continue", ujson.Obj("threadId" -> 1))
      messages = client.takeNMessages(1)
      assert(messages.nonEmpty, "Expected a continue response")
      println(s"${messages(0).messageType}: ${messages(0).content}\n")

      println("Successfully got all expected DAP messages")

      compilerThread.join()
    } finally {
      client.close()
    }
  }
