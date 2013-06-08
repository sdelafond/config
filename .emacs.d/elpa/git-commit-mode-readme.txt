A major mode for editing Git commit messages.

* Formatting

Highlight the formatting of git commit messages and indicate errors according
to the guidelines for commit messages (see
http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html).

Highlight the first line (aka "summary") specially if it exceeds 54
characters.

Enable `auto-fill-mode' and set the `fill-column' to 72 according to the
aforementioned guidelines.

* Headers

Provide commands to insert standard headers into commit messages.

- C-c C-x s or C-c C-s inserts Signed-off-by (`git-commit-signoff').
- C-C C-x a inserts Acked-by (`git-commit-ack').
- C-c C-x t inserts Tested-by (`git-commit-test').
- C-c C-x r inserts Reviewed-by (`git-commit-review').
- C-c C-x o inserts Cc (`git-commit-cc').
- C-c C-x p inserts Reported-by (`git-commit-reported').

* Committing

C-c C-c finishes a commit.  By default this means to save and kill the
buffer.  Customize `git-commit-commit-function' to change this behaviour.

Check a buffer for stylistic errors before committing, and ask for
confirmation before committing with style errors.

* Magit integration

Overwrite `magit-log-edit-mode' to provide font locking and header insertion
for Magit.

Change the keymap of `magit-log-edit-mode' to use the header insertion of
`git-commit-mode'.
