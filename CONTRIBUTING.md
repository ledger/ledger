Tips for contributors
---------------------

* Please **make pull requests against `next`, not `master`**.
  Ledger follows a [git-flow](http://nvie.com/posts/a-successful-git-branching-model/) branching model,
  in which development happens on the `next` branch and is subsequently merged into `master` for releases.
* If you're making **changes to `ledger-mode`, or other files for which the Travis build is not
  relevant**, please **add `[ci skip]` to the end of the commit message**.
