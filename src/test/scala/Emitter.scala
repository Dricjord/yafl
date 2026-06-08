import com.dylibso.chicory
import com.dylibso.chicory.tools.wasm.Wat2Wasm

import java.io.File

import yafl.SourceFile
import yafl.emitter.Emitter
import yafl.optimizer.Optimizer
import yafl.parser.Parser
import yafl.typer.Typer

final class EmitterTests extends munit.FunSuite:

  test("argc"):
    val input = SourceFile("test", "#argc")
    val wasm = compile(input)
    val main = wasm.`export`("main")
    writeArguments(wasm, IArray(31, 11))
    assertEquals(main.apply()(0), 2L)

  test("argv"):
    val input = SourceFile("test", "(#argv 0) + (#argv 1)")
    val wasm = compile(input)
    val main = wasm.`export`("main")
    writeArguments(wasm, IArray(40, 2))
    assertEquals(main.apply()(0), 42L)

  test("integer addition"):
    val input = SourceFile("test", "40 + 2")
    val main = compile(input).`export`("main")
    assertEquals(main.apply()(0), 42L)

  test("binding"):
    // Uses a runtime value so the optimizer cannot fold the binding away.
    val input = SourceFile("test", "let x = #argv 0 ; x + x")
    val wasm = compile(input)
    val main = wasm.`export`("main")
    writeArguments(wasm, IArray(21))
    assertEquals(main.apply()(0), 42L)

  test("binding shadowing"):
    // Two bindings share the name `x`; they must get different WAT local names.
    val input = SourceFile("test", "let x = #argv 0 ; let x = x + 1 ; x")
    val wasm = compile(input)
    val main = wasm.`export`("main")
    writeArguments(wasm, IArray(41))
    assertEquals(main.apply()(0), 42L)

  /** Compiles `input` to a WebAssembly module and returns an instance of it. */
  private def compile(input: SourceFile): chicory.runtime.Instance =
    val program =  Optimizer.optimize(Typer.check(Parser.parse(input)))
    val binary = Wat2Wasm.parse(Emitter.emit(program))
    val m = chicory.wasm.Parser.parse(binary)
    chicory.runtime.Instance.builder(m).build()

  /** Initializes the command-line arguments of `wasm` to `values`. */
  private def writeArguments(wasm: chicory.runtime.Instance, values: IArray[Int]): Unit =
    val m = wasm.memory()
    m.writeI32(0, values.length)
    for i <- 0 until values.length do m.writeI32(4 + (i * 4), values(i))

end EmitterTests
