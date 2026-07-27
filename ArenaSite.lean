import VersoBlog

import ArenaSite.Theme
import ArenaSite.Pages

open Verso Doc
open Verso.Genre.Blog
open scoped ArenaSite.Pages

def arenaSite : Site :=
  Site.page (%docName? ArenaSite.Pages.FrontPage) (%doc? ArenaSite.Pages.FrontPage) #[
    Dir.static "static" "static",
    Dir.page "checker"
      (%docName? ArenaSite.Pages.CheckerIndex)
      (%doc? ArenaSite.Pages.CheckerIndex)
      (checker_pages%),
    Dir.page "test"
      (%docName? ArenaSite.Pages.TestIndex)
      (%doc? ArenaSite.Pages.TestIndex)
      (test_pages%)
  ]

def main (args : List String) : IO UInt32 :=
  blogMain ArenaSite.theme arenaSite {} args
