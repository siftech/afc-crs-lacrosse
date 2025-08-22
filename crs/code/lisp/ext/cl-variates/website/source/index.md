{include header.md}
{set-property title "CL-Variates"}

<div class="contents">
<div class="system-links">

  * [Mailing Lists][3]
  * [Getting it][4]
  * [Documentation][5]
  * [News][6]
  * [Test results][tr]
  * [Changelog][7]

   [3]: #mailing-lists
   [4]: #downloads
   [5]: documentation/ (documentation link)
   [6]: #news
   [7]: changelog.html
   [tr]: test-report.html

</div>
<div class="system-description">

### What it is

CL-Variates is a portable random number generator for Common Lisp.

Common Lisp has _random_ but this can vary for each implementation. CL-Variates provides multiple random number generator that will produce the same numbers regardless of platform. It also includes additional random distributions like poisson, exponential and so forth.

[TINAA][8] documentation for CL-Variates is [available][9].

   [8]: http://common-lisp.net/project/tinaa/
   [9]: http://common-lisp.net/project/cl-variates/documentation/

{anchor mailing-lists}

### Mailing Lists

  * [cl-variates-devel][11]: A list for questions, patches, bug reports, announcements and, well, everything.

   [11]: http://common-lisp.net/cgi-bin/mailman/listinfo/cl-variates-devel

{anchor downloads}

### Where is it

A [Darcs][12] repository is available. The command is listed below:
    
   [12]: http://www.darcs.net/

    darcs get http://common-lisp.net/project/cl-variates

CL-Variates is also be [ASDF installable][16]. Its CLiki home is right [where][17] you'd expect.

   [16]: http://www.cliki.net/asdf-install
   [17]: http://www.cliki.net/cl-variates

There's also a handy [gzipped tar file][18].

   [18]: http://common-lisp.net/project/cl-variates/cl-variates_latest.tar.gz

{anchor news}

### What is happening

18 Oct 2007 -- Time flies! Simplified CL-Variates so that
it no longer requires CL-MathStats, Metatilities or any other system (yippee for simplification!). Improved internal organization some; brought test suite back to life.

14 Nov 2005
Corrected links; added tarball, etc.

9 Nov 2005
Just getting things set up here.

</div>
</div>

{include footer.md}

   [19]: http://common-lisp.net/project/cl-containers/shared/buttons/xhtml.gif (valid xhtml button)
   [20]: http://validator.w3.org/check/referer (xhtml1.1)
   [21]: http://common-lisp.net/project/cl-containers/shared/buttons/hacker.png (hacker emblem)
   [22]: http://www.catb.org/hacker-emblem/ (hacker)
   [23]: http://common-lisp.net/project/cl-containers/shared/buttons/lml2-powered.png (lml2 powered)
   [24]: http://lml2.b9.com/ (lml2 powered)
   [25]: http://common-lisp.net/project/cl-containers/shared/buttons/lambda-lisp.png (ALU emblem)
   [26]: http://www.lisp.org/ (Association of Lisp Users)
   [27]: http://common-lisp.net/project/cl-containers/shared/buttons/lisp-lizard.png (Common-Lisp.net)
   [28]: http://common-lisp.net/ (Common-Lisp.net)


