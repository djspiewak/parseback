package parseback

/**
 * @param base The full contents of the line
 * @param lineNo The line offset of this line within the larger input stream (0 indexed)
 * @param colNo The column offset into `base` (0 indexed)
 */
final case class Line(base: String, lineNo: Int, colNo: Int) {

  val head: Char = base charAt colNo

  def project: String = base.substring(colNo)

  def next: Option[Line] =
    if (colNo + 1 < base.length) Some(Line(base, lineNo, colNo + 1)) else None
}
