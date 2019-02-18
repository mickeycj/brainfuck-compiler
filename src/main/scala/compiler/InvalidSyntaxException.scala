package compiler
/** Invalid syntax exception.
 *
 *  This exception deals with erros in BrainFuck's syntax.
 */
final class InvalidSyntaxException(val message: String, val cause: Throwable = None.orNull) extends Exception(message, cause)
