package compiler
/** Invalid syntax exception.
 *
 *  This exception deals with errors in BrainFuck's syntax.
 */
final class InvalidSyntaxException(val message: String) extends Exception(message)
