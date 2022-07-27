package dev.pagingmatt.parseposition

import java.text.ParsePosition

/**
 * Functional programming style extensions to java.text.ParsePosition.
 */
extension (position: ParsePosition)

  /**
   * Flags if a ParsePosition object represents a position that exists within an
   * input of given length.
   *
   * @param inputLength
   *   the length of the input being parsed.
   * @return
   *   true if the ParsePosition object represents a position that is in-bounds
   *   for an input of the given length.
   */
  def exists(inputLength: Int): Boolean =
    position.getIndex >= 0 && position.getIndex < inputLength

  /**
   * Flags if a ParsePosition object has recorded an error.
   *
   * @return
   *   true if the ParsePosition object has recorded an error, otherwise false.
   */
  def hasError: Boolean = position.getErrorIndex > -1

  /**
   * Flags if a ParsePosition object is in the first position.
   *
   * @return
   *   true if the ParsePosition object is at the first position in the input,
   *   otherwise false.
   */
  def isFirst: Boolean = position.getIndex == 0

  /**
   * Flags if a ParsePosition object is in the last position.
   *
   * @param inputLength
   *   the length of the input being parsed.
   * @return
   *   true if the ParsePosition object has reached the last position in the
   *   input, otherwise false.
   */
  def isLast(inputLength: Int): Boolean = position.getIndex == inputLength - 1

  /**
   * Copies a ParsePosition object into a new object with the same state.
   *
   * @return
   *   a new ParsePosition object with the same state.
   */
  def copy: ParsePosition = {
    val newPosition = new ParsePosition(position.getIndex)
    newPosition.setErrorIndex(position.getErrorIndex)

    newPosition
  }

  /**
   * Iterates the index of a ParsePosition object by a specified number of
   * steps.
   *
   * @param by
   *   the number of steps to iterate the index.
   * @return
   *   a new ParsePosition object with the updated index.
   */
  def iterate(by: Int): ParsePosition = {
    val newPosition = position.copy
    newPosition.setIndex(newPosition.getIndex + by)

    newPosition
  }

  /**
   * Iterates the index of a ParsePosition object by a single step.
   *
   * @return
   *   a new ParsePosition object with the updated index.
   */
  def next: ParsePosition = iterate(by = 1)

  /**
   * Sets the error index of a ParsePosition object unless it has already been
   * set or the caller specifies that any existing error should be ignored.
   *
   * @param at
   *   the index at which the error should be recorded.
   * @param ignoreExisting
   *   a flag to guarantee this index will be recorded, even if an error already
   *   exists on the ParsePosition object.
   * @return
   *   a new ParsePosition object with the updated error index.
   */
  def withError(at: Int, ignoreExisting: Boolean = false): ParsePosition = {
    val newPosition = position.copy

    if (ignoreExisting || !hasError) {
      newPosition.setErrorIndex(at)
      newPosition
    } else {
      newPosition
    }
  }
