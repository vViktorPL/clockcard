module Assets exposing (getImageUrl)

assetsRoot = "../../assets"

getImageUrl : String -> String
getImageUrl filename =
    assetsRoot ++ "/images/" ++ filename