package parseback

// a Kleene algebra
private[parseback] sealed trait Nullable extends Product with Serializable {
  def ||(that: Nullable): Nullable
  def &&(that: Nullable): Nullable
  def toBoolean: Boolean
}

private[parseback] object Nullable {

  case object True extends Nullable {
    def ||(that: Nullable) = True
    def &&(that: Nullable) = that
    def toBoolean = true
  }

  case object Maybe extends Nullable {
    def ||(that: Nullable) = if (that == True) True else Maybe
    def &&(that: Nullable) = if (that == False) False else Maybe
    def toBoolean = sys.error("not intended to be called")
  }

  case object False extends Nullable {
    def ||(that: Nullable) = that
    def &&(that: Nullable) = False
    def toBoolean = false
  }
}
