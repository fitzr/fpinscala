package fpinscala.gettingstarted

import org.scalatest.FreeSpec

class GettingstartedSpec extends FreeSpec {

  "for MyModule" - {
    "exec main" in {
      MyModule.main(Array.empty)
    }
  }
}