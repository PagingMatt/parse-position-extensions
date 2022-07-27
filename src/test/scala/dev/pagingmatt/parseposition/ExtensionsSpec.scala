package dev.pagingmatt.parseposition

import org.scalatest.Outcome
import org.scalatest.flatspec.FixtureAnyFlatSpec

import java.text.ParsePosition

class ExtensionsSpec extends FixtureAnyFlatSpec {
  protected type FixtureParam = ParsePosition

  private val INITIAL_PARSE_POSITION = 2
  private val INITIAL_ERROR_POSITION = 1

  override def withFixture(test: OneArgTest): Outcome = {
    val fixture: FixtureParam = {
      val position = new ParsePosition(INITIAL_PARSE_POSITION)
      position.setErrorIndex(INITIAL_ERROR_POSITION)
      position
    }

    try test(fixture)
    finally {}
  }

  "Checking if a parse position object represents a position that exists" should "return false for a negative index" in {
    position =>
      position.setIndex(-1)
      assert(!position.exists(1))
  }

  it should "return false for a 0 index of an empty input" in { position =>
    position.setIndex(0)
    assert(!position.exists(0))
  }

  it should "return true for a 0 index of a non-empty input" in { position =>
    position.setIndex(0)
    assert(position.exists(1))
  }

  it should "return true for a non-0 (in-bounds) index of a non-empty input" in {
    position =>
      assert(position.exists(4))
  }

  it should "return true for the last index of a non-empty input" in {
    position =>
      assert(position.exists(3))
  }

  it should "return false for an index past the last index of a non-empty input" in {
    position =>
      assert(!position.exists(2))
  }

  "Checking if a parse position object has an error" should "return false if the object does not have an error" in {
    position =>
      position.setErrorIndex(-1)

      assert(!position.hasError)
  }

  it should "return true if the object has an error" in { position =>
    assert(position.hasError)
  }

  "Checking if a parse position object is in the first position" should "return false if the index is greater than 0" in {
    position =>
      assert(!position.isFirst)
  }

  it should "return true if the index is 0" in { position =>
    position.setIndex(0)
    assert(position.isFirst)
  }

  "Checking if a parse position object is in the last position" should "return false if the index is smaller than the final index" in {
    position =>
      assert(!position.isLast(4))
  }

  it should "return false if the index is greater than the final index" in {
    position =>
      assert(!position.isLast(2))
  }

  it should "return false if the index is equal to the final index" in {
    position =>
      assert(position.isLast(3))
  }

  "Copying a parse position object" should "return a new object" in {
    position =>
      val copiedParsePosition = position.copy

      assert(!(position eq copiedParsePosition))
  }

  it should "copy the index" in { position =>
    val copiedParsePosition = position.copy

    assertResult(position.getIndex)(copiedParsePosition.getIndex)
  }

  it should "copy the error index" in { position =>
    val copiedParsePosition = position.copy

    assertResult(position.getErrorIndex)(
      copiedParsePosition.getErrorIndex
    )
  }

  "Iterating a parse position object" should "return a new object" in {
    position =>
      val iteratedParsePosition = position.iterate(2)

      assert(!(position eq iteratedParsePosition))
  }

  it should "iterate the index" in { position =>
    val iteratedParsePosition = position.iterate(2)

    assertResult(position.getIndex + 2)(
      iteratedParsePosition.getIndex
    )
  }

  it should "not change the error index" in { position =>
    val iteratedParsePosition = position.iterate(2)

    assertResult(position.getErrorIndex)(
      iteratedParsePosition.getErrorIndex
    )
  }

  "Moving to the next parse position object" should "return a new object" in {
    position =>
      val nextParsePosition = position.next

      assert(!(position eq nextParsePosition))
  }

  it should "iterate the index by 1" in { position =>
    val nextParsePosition = position.next

    assertResult(position.getIndex + 1)(
      nextParsePosition.getIndex
    )
  }

  it should "not change the error index" in { position =>
    val nextParsePosition = position.next

    assertResult(position.getErrorIndex)(
      nextParsePosition.getErrorIndex
    )
  }

  "Erroring a parse position object" should "return a new object" in {
    position =>
      val erroredParsePosition = position.withError(2)

      assert(!(position eq erroredParsePosition))
  }

  it should "not change the index" in { position =>
    val erroredParsePosition = position.withError(2)

    assertResult(position.getIndex)(
      erroredParsePosition.getIndex
    )
  }

  it should "not change the error index if it is already tracking an error" in {
    position =>
      val erroredParsePosition = position.withError(2)

      assertResult(position.getErrorIndex)(
        erroredParsePosition.getErrorIndex
      )
  }

  it should "change the error index if it is not tracking an error" in {
    position =>
      position.setErrorIndex(-1)
      val erroredParsePosition = position.withError(2)

      assertResult(2)(erroredParsePosition.getErrorIndex)
  }

  it should "change the error index if it is already tracking an error and ignoreExisting is true" in {
    position =>
      val erroredParsePosition =
        position.withError(2, ignoreExisting = true)

      assertResult(2)(
        erroredParsePosition.getErrorIndex
      )
  }

  it should "change the error index if it is not tracking an error and ignoreExisting is true" in {
    position =>
      val erroredParsePosition =
        position.withError(2, ignoreExisting = true)

      assertResult(2)(
        erroredParsePosition.getErrorIndex
      )
  }
}
