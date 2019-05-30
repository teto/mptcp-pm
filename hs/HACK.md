
To list errnos
$ nix run nixpkgs.moreutils --command errno -l


getAttributes :: Get Attributes
getAttributes = fromList <$> whileM (not <$> isEmpty) getSingleAttribute
