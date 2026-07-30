namespace Arena

def testsDir : System.FilePath := "Tests"
def checkersDir : System.FilePath := "checkers"

def buildDir : System.FilePath := "_build"
def builtTestsDir : System.FilePath := buildDir / "tests"
def builtCheckersDir : System.FilePath := buildDir / "checkers"
def workRoot : System.FilePath := buildDir / "work"
def lean4exportRoot : System.FilePath := buildDir / "lean4export"

def resultsDir : System.FilePath := "_results"
def siteDataDir : System.FilePath := "site-data"
def siteDataPath : System.FilePath := siteDataDir / "arena.json"
def siteSourcesDir : System.FilePath := siteDataDir / "sources"
def siteExportsDir : System.FilePath := siteDataDir / "exports"

def tarballName : String := "lean-arena-tests.tar.gz"
def tarballPath : System.FilePath := buildDir / tarballName
def tarballInfoPath : System.FilePath := buildDir / "tarball.json"

end Arena
