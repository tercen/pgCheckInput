#pgCheckInput

## Install

```
remotes::install_github("tercen/pgcheckinput")
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