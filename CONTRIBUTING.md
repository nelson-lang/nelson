# Contributing to Nelson

## Code of Conduct

Please read the
[Code of Conduct](https://github.com/Nelson-numerical-software/nelson/blob/master/CODE_OF_CONDUCT.md)
which explains the minimum behavior expectations for Nelson contributors.

## Issue Contributions

When opening issues or commenting on existing issues, please make sure
discussions are related to concrete technical issues with Nelson.

## Code Contributions

This section will guide you through the contribution process.

### Step 1: Fork

Fork the project [on GitHub](https://github.com/Nelson-numerical-software/nelson.git) and clone your fork
locally.

#### Which branch?

For developing new features and bug fixes, the `master` branch should be pulled
and built upon.

### Step 2: Branch

Create a branch and start hacking:

```text
$ git checkout -b my-branch -t origin/master
```

### Step 3: Commit

Make sure git knows your name and email address:

```text
$ git config --global user.name "J. Random User"
$ git config --global user.email "j.random.user@example.com"
```

Add and commit:

```text
$ git add my/changed/files
$ git commit
```

### Commit message guidelines

The commit message should describe what changed and why.

1. The first line should:
   - contain a short description of the change
   - be 50 characters or less
   - be entirely in lowercase with the exception of proper nouns, acronyms, and
   the words that refer to code, like function/variable names
   - be prefixed with the name of the changed subsystem and start with an
   imperative verb. Check the output of `git log --oneline files/you/changed` to
   find out what subsystems your changes touch.

2. Keep the second line blank.
3. Wrap all other lines at 72 columns.

4. If your patch fixes an open issue, you can add a reference to it at the end
of the log. Use the `Fixes:` prefix and the full issue URL. For other references
use `Refs:`.

### Step 4: Rebase

Use `git rebase` (not `git merge`) to synchronize your work with the main
repository.

```text
$ git fetch upstream
$ git rebase upstream/master
```

### Step 5: Test

Bug fixes and features should come with tests. Looking at
other tests to see how they should be structured can also help.

(See the [DEVELOPMENT.md](./DEVELOPMENT.md) for more details.)

### Step 6: Push

```text
$ git push origin my-branch
```

Pull requests are usually reviewed within a few days.

### Step 7: Discuss and update

You will probably get feedback or requests for changes to your Pull Request.
This is a big part of the submission process so don't be discouraged!

To make changes to an existing Pull Request, make the changes to your branch.
When you push that branch to your fork, GitHub will automatically update the
Pull Request.

You can push more commits to your branch:

```text
$ git add my/changed/files
$ git commit
$ git push origin my-branch
```

Or you can rebase against master:

```text
$ git fetch --all
$ git rebase origin/master
$ git push --force-with-lease origin my-branch
```

Or you can amend the last commit (for example if you want to change the commit
log).

```text
$ git add any/changed/files
$ git commit --amend
$ git push --force-with-lease origin my-branch
```

**Important:** The `git push --force-with-lease` command is one of the few ways
to delete history in git. Before you use it, make sure you understand the risks.

Feel free to post a comment in the Pull Request to ping reviewers if you are
awaiting an answer on something. If you encounter words or acronyms that
seem unfamiliar.

Note that multiple commits often get squashed when they are landed (see the
notes about [commit squashing](#commit-squashing)).

### Step 8: Landing

In order to land, a Pull Request needs to be reviewed and
[approved](#getting-approvals-for-your-pull-request) by
at least one Nelson Collaborator and pass a
[CI (Continuous Integration) test run](#ci-testing).
After that, as long as there are no objections
from a Collaborator, the Pull Request can be merged. If you find your
Pull Request waiting longer than you expect, see the
[notes about the waiting time](#waiting-until-the-pull-request-gets-landed).

When a collaborator lands your Pull Request, they will post
a comment to the Pull Request page mentioning the commit(s) it
landed as. GitHub often shows the Pull Request as `Closed` at this
point, but don't worry. If you look at the branch you raised your
Pull Request against (probably `master`), you should see a commit with
your name on it. Congratulations and thanks for your contribution!

## Additional Notes

### Commit Squashing

When the commits in your Pull Request land, they will be squashed
into one commit per logical change. Metadata will be added to the commit
message (including links to the Pull Request, links to relevant issues,
and the names of the reviewers). The commit history of your Pull Request,
however, will stay intact on the Pull Request page.

### Getting Approvals for Your Pull Request

A Pull Request is approved either by saying LGTM, which stands for
"Looks Good To Me", or by using GitHub's Approve button.
GitHub's Pull Request review feature can be used during the process.

After you push new changes to your branch, you need to get
approval for these new changes again, even if GitHub shows "Approved"
because the reviewers have hit the buttons before.

### CI Testing

Every Pull Request needs to be tested
to make sure that it works on the platforms that Nelson
supports. This is done by running the code through the CI system.

Only a Collaborator can start a CI run. Usually one of them will do it
for you as approvals for the Pull Request come in.
If not, you can ask a Collaborator to start a CI run.

### Waiting Until the Pull Request Gets Landed

A Pull Request needs to stay open for at least 48 hours (72 hours on a
weekend) from when it is submitted, even after it gets approved and
passes the CI. This is to make sure that everyone has a chance to
weigh in. If the changes are trivial, collaborators may decide it
doesn't need to wait. A Pull Request may well take longer to be
merged in.

### Check Out the Collaborator's Guide

By making a contribution to this project, I certify that:

* (a) The contribution was created in whole or in part by me and I
  have the right to submit it under the open source license
  indicated in the file; or

* (b) The contribution is based upon previous work that, to the best
  of my knowledge, is covered under an appropriate open source
  license and I have the right under that license to submit that
  work with modifications, whether created in whole or in part
  by me, under the same open source license (unless I am
  permitted to submit under a different license), as indicated
  in the file; or

* (c) The contribution was provided directly to me by some other
  person who certified (a), (b) or (c) and I have not modified
  it.

* (d) I understand and agree that this project and the contribution
  are public and that a record of the contribution (including all
  personal information I submit with it, including my sign-off) is
  maintained indefinitely and may be redistributed consistent with
  this project or the open source license(s) involved.

* (e) As contributor, you must agree a [Contributor Licensing Agreement](https://cla-assistant.io/Nelson-numerical-software/nelson).
This CLA allows to main author(s) to re-licensing Nelson (GPLv3, LGPL, ...) without need to ask all others contributors.
