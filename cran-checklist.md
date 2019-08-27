
# Checks before releasing to CRAN

* Build and test package on:
    - Travis Linux and OSX
    - AppVeyor (Windows Server)
    - latest R dev version on Windows
* Build packages with dependencies on syntheticbench
* Merge develop branch into release branch
* Bump version to even value in DESCRIPTION and check package startup message
* Update README.Rmd and verify generated README.md on Github (release)
* Update cran_comments.md
* Update NEWS.md and make sure to remove '(in development)' in the version title
  and update the version number
* Credit all GitHub contributions in NEWS.md
* Build docs folder using pkgdown::build_site()
* Merge branch release into master
* Submit to CRAN

# After releasing to CRAN

* Merge branch master into release
* Go to the repository release page and create a new release with tag version vx.y.z.
  Copy and paste the contents of the relevant NEWS.md section into the release notes.
* Add '(in development)' to version title in NEWS.md and update to odd version number
* Bump version to odd value in DESCRIPTION and check package startup message
* Check package startup message
* Merge release branch into develop
