namespace Arena

def testsDir : System.FilePath := "tests"
def checkersDir : System.FilePath := "checkers"

def buildDir : System.FilePath := "_build"
def builtTestsDir : System.FilePath := buildDir / "tests"
def builtCheckersDir : System.FilePath := buildDir / "checkers"
def workRoot : System.FilePath := buildDir / "work"
def lean4exportRoot : System.FilePath := buildDir / "lean4export"

def resultsDir : System.FilePath := "_results"
def siteDataPath : System.FilePath := "site-data/arena.json"

def tarballName : String := "lean-arena-tests.tar.gz"
def tarballPath : System.FilePath := buildDir / tarballName

end Arena
