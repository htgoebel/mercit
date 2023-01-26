## A Mercurial Porcelain inside Emacs

**WARNING**

**At the moment this is just a crude _hack_!
Most things will not work as expected or not work at all.
Thinks will change rapidly without notice.
This project might even be abandoned without further notice.**

Many of the features provided here would fit into emacs' `vc-dir`,
too.  If we get a chance to integrate the parts relevent for us there,
we will stop this project.

**Use on your own risk**


### About

_Mercit_ is an attempt to make the power of _Magit_ available to
Mercurial users.
Of course, concepts differ between Mercurial and Git.
Anyhow, may of the features Magit brings to your fingertips
are available in Mercurial too.

While [monkey](https://github.com/ananthakumaran/monky),
is a fresh implementation starting from scratch,
_Mergit_ is hacking Magit to work with Mercurial.
So we get the transient interface already.

#### The Plan

Rough list of most important features (and priorities):

* Status overview (mercit-status)
* „staging“ and selecting lines/chunks to commit
* discarding changes lines/chunks within Mercit
* pushing only current HEAD
* amend
* mark commits as “to fixup” and “to squash”
* rebase, histedit, topics — we'll see
* managing branches, topics, etc.
* managing „remotes”
* shelve, unshelve



### Getting Started

Within emacs run `M-x mercit-status`
to get a status buffer like the one provided by Magit.
(If the buffer is empty, try pressing `g` to refresh)

### On the Shoulders of Giants: About Magit

If you are new to Magit, then either one of the following two
articles should help understanding how it differs from other Git
clients.

#### [Visual Magit walk-through](https://emacsair.me/2017/09/01/mercit-walk-through)

If you are completely new to Magit, then this article is a good
visual introduction.

Almost everything that you see in Magit can be acted on by pressing
some key, but that's not obvious from just seeing how Magit looks.
The screenshots and accompanying text of this article explain how to
perform a variety of actions on Magit's output.

#### [Magit, the magical Git interface](https://emacsair.me/2017/09/01/the-magical-git-interface)

Magit differs significantly from other Git interfaces, and its
advantages are not immediately obvious simply from looking at a few
screenshots as presented in the preceding article.

This article discusses Magit's properties in somewhat more abstract
terms.

***
### Acknowledgments

Many thanks to all the Megit developers and maintainers.
You created a phantastic tool,
and this is why we try to bring it to Mercurial.

Thanks to all of you, may (the history of) the source be with you!
