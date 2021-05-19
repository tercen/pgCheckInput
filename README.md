#pgCheckInput

## Instal

```
# allow for older R-versions
if (getRversion() < "3.2"){ 
  devtools::install_bitbucket("pgcheckinput", username = "bnoperator")
} else {
  devtools::install_bitbucket("bnoperator/pgcheckinput")
}
```

# Publish a package on pamagene R repository

```
bntools::deployGitPackage('https://bitbucket.org/bnoperator/pgcheckinput.git', '3.3')
```
# commit to git and push
```
git add --all && git commit -a -m "++" && git push
```

# add and push tags
git tag -a 1.2 -m "++" && git push --tags