package com.sootsafe.reporting

import org.scalatest.{Matchers, WordSpecLike}

class LatexCompilerTest extends WordSpecLike with Matchers {

  "Report compiler" must {
    "compile a basic document" in {
      val filename = getClass.getResource("/latex/releaserate.tex").getPath
      //LatexCompiler.latexToPdf(filename, "temp").isSuccess should be(true)
    }
  }

}
