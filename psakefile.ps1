Task Format {
    Exec { stack exec -- stylish-haskell -c .\stylish-haskell.yaml -i .\app\Main.hs .\lib\src\Web\Pixela.hs }
}

Task Lint {
    Exec { stack exec -- hlint app lib }
}
