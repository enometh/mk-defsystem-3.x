;;; -*- Mode: Lisp; Package: MAKE; Syntax: Common-Lisp -*-

;;; DEFSYSTEM 3.8 Interim.

;;; defsystem.lisp --

;;; ****************************************************************
;;; MAKE -- A Portable Defsystem Implementation ********************
;;; ****************************************************************

;;; This is a portable system definition facility for Common Lisp.
;;; Though home-grown, the syntax was inspired by fond memories of the
;;; defsystem facility on Symbolics 3600's. The exhaustive lists of
;;; filename extensions for various lisps and the idea to have one
;;; "operate-on-system" function instead of separate "compile-system"
;;; and "load-system" functions were taken from Xerox Corp.'s PCL
;;; system.

;;; This system improves on both PCL and Symbolics defsystem utilities
;;; by performing a topological sort of the graph of file-dependency
;;; constraints. Thus, the components of the system need not be listed
;;; in any special order, because the defsystem command reorganizes them
;;; based on their constraints. It includes all the standard bells and
;;; whistles, such as not recompiling a binary file that is up to date
;;; (unless the user specifies that all files should be recompiled).

;;; Originally written by Mark Kantrowitz, School of Computer Science,
;;; Carnegie Mellon University, October 1989.

;;; Copyright (c) 1989 - 1999 Mark Kantrowitz. All rights reserved.
;;;               1999 - 2005 Mark Kantrowitz and Marco Antoniotti. All
;;;                           rights reserved.
;;;               2005 - 2012 Marco Antoniotti. All rights reserved.
;;;               2008 - 2020 Madhu <enometh@meer.net> All rights ~reseda~

;;; Use, copying, modification, merging, publishing, distribution
;;; and/or sale of this software, source and/or binary files and
;;; associated documentation files (the "Software") and of derivative
;;; works based upon this Software are permitted, as long as the
;;; following conditions are met:

;;;    o this copyright notice is included intact and is prominently
;;;      visible in the Software
;;;    o if modifications have been made to the source code of the
;;;      this package that have not been adopted for inclusion in the
;;;      official version of the Software as maintained by the Copyright
;;;      holders, then the modified package MUST CLEARLY identify that
;;;      such package is a non-standard and non-official version of
;;;      the Software.  Furthermore, it is strongly encouraged that any
;;;      modifications made to the Software be sent via e-mail to the
;;;      MK-DEFSYSTEM maintainers for consideration of inclusion in the
;;;      official MK-DEFSYSTEM package.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT.
;;; IN NO EVENT SHALL M. KANTROWITZ AND M. ANTONIOTTI BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Except as contained in this notice, the names of M. Kantrowitz and
;;; M. Antoniotti shall not be used in advertising or otherwise to promote
;;; the sale, use or other dealings in this Software without prior written
;;; authorization from M. Kantrowitz and M. Antoniotti.


;;; Please send bug reports, comments and suggestions to <enometh@meer.net>.

;;; ********************************
;;; Change Log *********************
;;; ********************************
;;;
;;; Note: Several of the fixes from 30-JAN-91 and 31-JAN-91 were done in
;;; September and October 1990, but not documented until January 1991.
;;;
;;; akd  = Abdel Kader Diagne <diagne@dfki.uni-sb.de>
;;; as   = Andreas Stolcke <stolcke@ICSI.Berkeley.EDU>
;;; bha  = Brian Anderson <bha@atc.boeing.com>
;;; brad = Brad Miller <miller@cs.rochester.edu>
;;; bw   = Robert Wilhelm <wilhelm@rpal.rockwell.com>
;;; djc  = Daniel J. Clancy <clancy@cs.utexas.edu>
;;; fdmm = Fernando D. Mato Mira <matomira@di.epfl.ch>
;;; gc   = Guillaume Cartier <cartier@math.uqam.ca>
;;; gi   = Gabriel Inaebnit <inaebnit@research.abb.ch>
;;; gpw  = George Williams <george@hsvaic.boeing.com>
;;; hkt  = Rick Taube <hkt@cm-next-8.stanford.edu>
;;; ik   = Ik Su Yoo <ik@ctt.bellcore.com>
;;; jk   = John_Kolojejchick@MORK.CIMDS.RI.CMU.EDU
;;; kt   = Kevin Thompson <kthompso@ptolemy.arc.nasa.gov>
;;; kc   = Kaelin Colclasure <kaelin@bridge.com>
;;; kmr  = Kevin M. Rosenberg <kevin@rosenberg.net>
;;; lmh  = Liam M. Healy <Liam.Healy@nrl.navy.mil>
;;; mc   = Matthew Cornell <cornell@unix1.cs.umass.edu>
;;; oc   = Oliver Christ <oli@adler.ims.uni-stuttgart.de>
;;; rs   = Ralph P. Sobek <ralph@vega.laas.fr>
;;; rs2  = Richard Segal <segal@cs.washington.edu>
;;; sb   = Sean Boisen <sboisen@bbn.com>
;;; ss   = Steve Strassman <straz@cambridge.apple.com>
;;; tar  = Thomas A. Russ <tar@isi.edu>
;;; toni = Anton Beschta <toni%l4@ztivax.siemens.com>
;;; yc   = Yang Chen <yangchen%iris.usc.edu@usc.edu>
;;;
;;; Thanks to Steve Strassmann <straz@media-lab.media.mit.edu> and
;;; Sean Boisen <sboisen@BBN.COM> for detailed bug reports and
;;; miscellaneous assistance. Thanks also to Gabriel Inaebnit
;;; <inaebnit@research.abb.ch> for help with VAXLisp bugs.
;;;
;;; 05-NOV-90  hkt  Changed canonicalize-system-name to make system
;;;                 names package independent. Interns them in the
;;;                 keyword package. Thus either strings or symbols may
;;;                 be used to name systems from the user's point of view.
;;; 05-NOV-90  hkt  Added definition FIND-SYSTEM to allow OOS to
;;;                 work on systems whose definition hasn't been loaded yet.
;;; 05-NOV-90  hkt  Added definitions COMPILE-SYSTEM and LOAD-SYSTEM
;;;                 as alternates to OOS for naive users.
;;; 05-NOV-90  hkt  Shadowing-import of 'defsystem in Allegro CL 3.1 [NeXT]
;;;                 into USER package instead of import.
;;; 15-NOV-90  mk   Changed package name to "MAKE", eliminating "DEFSYSTEM"
;;;                 to avoid conflicts with allegro, symbolics packages
;;;                 named "DEFSYSTEM".
;;; 30-JAN-91  mk   Modified append-directories to work with the
;;;                 logical-pathnames system.
;;; 30-JAN-91  mk   Append-directories now works with Sun CL4.0. Also, fixed
;;;                 bug wrt Lucid 4.0's pathnames (which changed from lcl3.0
;;;                 -- 4.0 uses a list for the directory slot, whereas
;;;                 3.0 required a string). Possible fix to symbolics bug.
;;; 30-JAN-91  mk   Defined NEW-REQUIRE to make redefinition of REQUIRE
;;;                 cleaner. Replaced all calls to REQUIRE in this file with
;;;                 calls to NEW-REQUIRE, which should avoid compiler warnings.
;;; 30-JAN-91  mk   In VAXLisp, when we redefine lisp:require, the compiler
;;;                 no longer automatically executes require forms when it
;;;                 encounters them in a file. The user can always wrap an
;;;                 (eval-when (compile load eval) ...) around the require
;;;                 form. Alternately, see commented out code near the
;;;                 redefinition of lisp:require which redefines it as a
;;;                 macro instead.
;;; 30-JAN-91  mk   Added parameter :version to operate-on-system. If it is
;;;                 a number, that number is used as part of the binary
;;;                 directory name as the place to store and load files.
;;;                 If NIL (the default), uses regular binary directory.
;;;                 If T, tries to find the most recent version of the
;;;                 binary directory.
;;; 30-JAN-91  mk   Added global variable *use-timeouts* (default: t), which
;;;                 specifies whether timeouts should be used in
;;;                 Y-OR-N-P-WAIT. This is provided for users whose lisps
;;;                 don't handle read-char-no-hang properly, so that they
;;;                 can set it to NIL to disable the timeouts. Usually the
;;;                 reason for this is the lisp is run on top of UNIX,
;;;                 which buffers input LINES (and provides input editing).
;;;                 To get around this we could always turn CBREAK mode
;;;                 on and off, but there's no way to do this in a portable
;;;                 manner.
;;; 30-JAN-91  mk   Fixed bug where in :test t mode it was actually providing
;;;                 the system, instead of faking it.
;;; 30-JAN-91  mk   Changed storage of system definitions to a hash table.
;;;                 Changed canonicalize-system-name to coerce the system
;;;                 names to uppercase strings. Since we're no longer using
;;;                 get, there's no need to intern the names as symbols,
;;;                 and strings don't have packages to cause problems.
;;;                 Added UNDEFSYSTEM, DEFINED-SYSTEMS, and DESCRIBE-SYSTEM.
;;;                 Added :delete-binaries command.
;;; 31-JAN-91  mk   Franz Allegro CL has a defsystem in the USER package,
;;;                 so we need to do a shadowing import to avoid name
;;;                 conflicts.
;;; 31-JAN-91  mk   Fixed bug in compile-and-load-operation where it was
;;;                 only loading newly compiled files.
;;; 31-JAN-91  mk   Added :load-time slot to components to record the
;;;                 file-write-date of the binary/source file that was loaded.
;;;                 Now knows "when" (which date version) the file was loaded.
;;;                 Added keyword :minimal-load and global *minimal-load*
;;;                 to enable defsystem to avoid reloading unmodified files.
;;;                 Note that if B depends on A, but A is up to date and
;;;                 loaded and the user specified :minimal-load T, then A
;;;                 will not be loaded even if B needs to be compiled. So
;;;                 if A is an initializations file, say, then the user should
;;;                 not specify :minimal-load T.
;;; 31-JAN-91  mk   Added :load-only slot to components. If this slot is
;;;                 specified as non-NIL, skips over any attempts to compile
;;;                 the files in the component. (Loading the file satisfies
;;;                 the need to recompile.)
;;; 31-JAN-91  mk   Eliminated use of set-alist-lookup and alist-lookup,
;;;                 replacing it with hash tables. It was too much bother,
;;;                 and rather brittle too.
;;; 31-JAN-91  mk   Defined #@ macro character for use with AFS @sys
;;;                 feature simulator. #@"directory" is then synonymous
;;;                 with (afs-binary-directory "directory").
;;; 31-JAN-91  mk   Added :private-file type of module. It is similar to
;;;                 :file, but has an absolute pathname. This allows you
;;;                 to specify a different version of a file in a system
;;;                 (e.g., if you're working on the file in your home
;;;                 directory) without completely rewriting the system
;;;                 definition.
;;; 31-JAN-91  mk   Operations on systems, such as :compile and :load,
;;;                 now propagate to subsystems the system depends on
;;;                 if *operations-propagate-to-subsystems* is T (the default)
;;;                 and the systems were defined using either defsystem
;;;                 or as a :system component of another system. Thus if
;;;                 a system depends on another, it can now recompile the
;;;                 other.
;;; 01-FEB-91  mk   Added default definitions of PROVIDE/REQUIRE/*MODULES*
;;;                 for lisps that have thrown away these definitions in
;;;                 accordance with CLtL2.
;;; 01-FEB-91  mk   Added :compile-only slot to components. Analogous to
;;;                 :load-only. If :compile-only is T, will not load the
;;;                 file on operation :compile. Either compiles or loads
;;;                 the file, but not both. In other words, compiling the
;;;                 file satisfies the demand to load it. This is useful
;;;                 for PCL defmethod and defclass definitions, which wrap
;;;                 an (eval-when (compile load eval) ...) around the body
;;;                 of the definition -- we save time by not loading the
;;;                 compiled code, since the eval-when forces it to be
;;;                 loaded. Note that this may not be entirely safe, since
;;;                 CLtL2 has added a :load keyword to compile-file, and
;;;                 some lisps may maintain a separate environment for
;;;                 the compiler. This feature is for the person who asked
;;;                 that a :COMPILE-SATISFIES-LOAD keyword be added to
;;;                 modules. It's named :COMPILE-ONLY instead to match
;;;                 :LOAD-ONLY.
;;; 11-FEB-91  mk   Now adds :mk-defsystem to features list, to allow
;;;                 special cased loading of defsystem if not already
;;;                 present.
;;; 19-FEB-91  duff Added filename extension for hp9000/300's running Lucid.
;;; 26-FEB-91  mk   Distinguish between toplevel systems (defined with
;;;                 defsystem) and systems defined as a :system module
;;;                 of a defsystem. The former can depend only on systems,
;;;                 while the latter can depend on anything at the same
;;;                 level.
;;; 12-MAR-91  mk   Added :subsystem component type to be a system with
;;;                 pathnames relative to its parent component.
;;; 12-MAR-91  mk   Uncommented :device :absolute for CMU pathnames, so
;;;                 that the leading slash is included.
;;; 12-MAR-91  brad Patches for Allegro 4.0.1 on Sparc.
;;; 12-MAR-91  mk   Changed definition of format-justified-string so that
;;;                 it no longer depends on the ~<~> format directives,
;;;                 because Allegro 4.0.1 has a bug which doesn't support
;;;                 them. Anyway, the new definition is twice as fast
;;;                 and conses half as much as FORMAT.
;;; 12-MAR-91 toni  Remove nils from list in expand-component-components.
;;; 12-MAR-91 bw    If the default-package and system have the same name,
;;;                 and the package is not loaded, this could lead to
;;;                 infinite loops, so we bomb out with an error.
;;;                 Fixed bug in default packages.
;;; 13-MAR-91 mk    Added global *providing-blocks-load-propagation* to
;;;                 control whether system dependencies are loaded if they
;;;                 have already been provided.
;;; 13-MAR-91 brad  In-package is a macro in CLtL2 lisps, so we change
;;;                 the package manually in operate-on-component.
;;; 15-MAR-91 mk    Modified *central-registry* to be either a single
;;;                 directory pathname, or a list of directory pathnames
;;;                 to be checked in order.
;;; 15-MAR-91 rs    Added afs-source-directory to handle versions when
;;;                 compiling C code under lisp. Other minor changes to
;;;                 translate-version and operate-on-system.
;;; 21-MAR-91 gi    Fixed bug in defined-systems.
;;; 22-MAR-91 mk    Replaced append-directories with new version that works
;;;                 by actually appending the directories, after massaging
;;;                 them into the proper format. This should work for all
;;;                 CLtL2-compliant lisps.
;;; 09-APR-91 djc   Missing package prefix for lp:pathname-host-type.
;;;                 Modified component-full-pathname to work for logical
;;;                 pathnames.
;;; 09-APR-91 mk    Added *dont-redefine-require* to control whether
;;;                 REQUIRE is redefined. Fixed minor bugs in redefinition
;;;                 of require.
;;; 12-APR-91 mk    (pathname-host nil) causes an error in MCL 2.0b1
;;; 12-APR-91 mc    Ported to MCL2.0b1.
;;; 16-APR-91 mk    Fixed bug in needs-loading where load-time and
;;;                 file-write-date got swapped.
;;; 16-APR-91 mk    If the component is load-only, defsystem shouldn't
;;;                 tell you that there is no binary and ask you if you
;;;                 want to load the source.
;;; 17-APR-91 mc    Two additional operations for MCL.
;;; 21-APR-91 mk    Added feature requested by ik. *files-missing-is-an-error*
;;;                 new global variable which controls whether files (source
;;;                 and binary) missing cause a continuable error or just a
;;;                 warning.
;;; 21-APR-91 mk    Modified load-file-operation to allow compilation of source
;;;                 files during load if the binary files are old or
;;;                 non-existent. This adds a :compile-during-load keyword to
;;;                 oos, and load-system. Global *compile-during-load* sets
;;;                 the default (currently :query).
;;; 21-APR-91 mk    Modified find-system so that there is a preference for
;;;                 loading system files from disk, even if the system is
;;;                 already defined in the environment.
;;; 25-APR-91 mk    Removed load-time slot from component defstruct and added
;;;                 function COMPONENT-LOAD-TIME to store the load times in a
;;;                 hash table. This is safer than the old definition because
;;;                 it doesn't wipe out load times every time the system is
;;;                 redefined.
;;; 25-APR-91 mk    Completely rewrote load-file-operation. Fixed some bugs
;;;                 in :compile-during-load and in the behavior of defsystem
;;;                 when multiple users are compiling and loading a system
;;;                 instead of just a single user.
;;; 16-MAY-91 mk    Modified FIND-SYSTEM to do the right thing if the system
;;;                 definition file cannot be found.
;;; 16-MAY-91 mk    Added globals *source-pathname-default* and
;;;                 *binary-pathname-default* to contain default values for
;;;                 :source-pathname and :binary-pathname. For example, set
;;;                 *source-pathname-default* to "" to avoid having to type
;;;                 :source-pathname "" all the time.
;;; 27-MAY-91 mk    Fixed bug in new-append-directories where directory
;;;                 components of the form "foo4.0" would appear as "foo4",
;;;                 since pathname-name truncates the type. Changed
;;;                 pathname-name to file-namestring.
;;;  3-JUN-91 gc    Small bug in new-append-directories; replace (when
;;;                 abs-name) with (when (not (null-string abs-name)))
;;;  4-JUN-91 mk    Additional small change to new-append-directories for
;;;                 getting the device from the relative pname if the abs
;;;                 pname is "". This is to fix a small behavior in CMU CL old
;;;                 compiler. Also changed (when (not (null-string abs-name)))
;;;                 to have an (and abs-name) in there.
;;;  8-JAN-92 sb    Added filename extension for defsystem under Lucid Common
;;;                 Lisp/SGO 3.0.1+.
;;;  8-JAN-92 mk    Changed the definition of prompt-string to work around an
;;;                 AKCL bug. Essentially, AKCL doesn't default the colinc to
;;;                 1 if the colnum is provided, so we hard code it.
;;;  8-JAN-92 rs    (pathname-directory (pathname "")) returns '(:relative) in
;;;                 Lucid, instead of NIL. Changed new-append-directories and
;;;                 test-new-append-directories to reflect this.
;;;  8-JAN-92 mk    Fixed problem related to *load-source-if-no-binary*.
;;;                 compile-and-load-source-if-no-binary wasn't checking for
;;;                 the existence of the binary if this variable was true,
;;;                 causing the file to not be compiled.
;;;  8-JAN-92 mk    Fixed problem with null-string being called on a pathname
;;;                 by returning NIL if the argument isn't a string.
;;;  3-NOV-93 mk    In Allegro 4.2, pathname device is :unspecific by default.
;;; 11-NOV-93 fdmm  Fixed package definition lock problem when redefining
;;;                 REQUIRE on ACL.
;;; 11-NOV-93 fdmm  Added machine and software types for SGI and IRIX. It is
;;;                 important to distinguish the OS version and CPU type in
;;;                 SGI+ACL, since ACL 4.1 on IRIX 4.x and ACL 4.2 on IRIX 5.x
;;;                 have incompatible .fasl files.
;;; 01-APR-94 fdmm  Fixed warning problem when redefining REQUIRE on LispWorks.
;;; 01-NOV-94 fdmm  Replaced (software-type) call in ACL by code extracting
;;;                 the interesting parts from (software-version) [deleted
;;;                 machine name and id].
;;; 03-NOV-94 fdmm  Added a hook (*compile-file-function*), that is funcalled
;;;                 by compile-file-operation, so as to support other languages
;;;                 running on top of Common Lisp.
;;;                 The default is to compile  Common Lisp.
;;; 03-NOV-94 fdmm  Added SCHEME-COMPILE-FILE, so that defsystem can now
;;;                 compile Pseudoscheme files.
;;; 04-NOV-94 fdmm  Added the exported generic function SET-LANGUAGE, to
;;;                 have a clean, easy to extend  interface for telling
;;;                 defsystem which language to assume for compilation.
;;;                 Currently supported arguments: :common-lisp, :scheme.
;;; 11-NOV-94 kc    Ported to Allegro CL for Windows 2.0 (ACLPC) and CLISP.
;;; 18-NOV-94 fdmm  Changed the entry *filename-extensions* for LispWorks
;;;                 to support any platform.
;;;                 Added entries for :mcl and :clisp too.
;;; 16-DEC-94 fdmm  Added and entry for CMU CL on SGI to *filename-extensions*.
;;; 16-DEC-94 fdmm  Added OS version identification for CMU CL on SGI.
;;; 16-DEC-94 fdmm  For CMU CL 17 : Bypassed make-pathnames call fix
;;;                 in NEW-APPEND-DIRECTORIES.
;;; 16-DEC-94 fdmm  Added HOME-SUBDIRECTORY to fix CMU's ignorance about `~'
;;;                 when specifying registries.
;;; 16-DEC-94 fdmm  For CMU CL 17 : Bypassed :device fix in make-pathnames call
;;;                 in COMPONENT-FULL-PATHNAME. This fix was also reported
;;;                 by kc on 12-NOV-94. CMU CL 17 now supports CLtL2 pathnames.
;;; 16-DEC-94 fdmm  Removed a quote before the call to read in the readmacro
;;;                 #@. This fixes a really annoying misfeature (couldn't do
;;;                 #@(concatenate 'string "foo/" "bar"), for example).
;;; 03-JAN-95 fdmm  Do not include :pcl in *features* if :clos is there.
;;;  2-MAR-95 mk    Modified fdmm's *central-registry* change to use
;;;                 user-homedir-pathname and to be a bit more generic in the
;;;                 pathnames.
;;;  2-MAR-95 mk    Modified fdmm's updates to *filename-extensions* to handle
;;;                 any CMU CL binary extensions.
;;;  2-MAR-95 mk    Make kc's port to ACLPC a little more generic.
;;;  2-MAR-95 mk    djc reported a bug, in which GET-SYSTEM was not returning
;;;                 a system despite the system's just having been loaded.
;;;                 The system name specified in the :depends-on was a
;;;                 lowercase string. I am assuming that the system name
;;;                 in the defsystem form was a symbol (I haven't verified
;;;                 that this was the case with djc, but it is the only
;;;                 reasonable conclusion). So, CANONICALIZE-SYSTEM-NAME
;;;                 was storing the system in the hash table as an
;;;                 uppercase string, but attempting to retrieve it as a
;;;                 lowercase string. This behavior actually isn't a bug,
;;;                 but a user error. It was intended as a feature to
;;;                 allow users to use strings for system names when
;;;                 they wanted to distinguish between two different systems
;;;                 named "foo.system" and "Foo.system". However, this
;;;                 user error indicates that this was a bad design decision.
;;;                 Accordingly, CANONICALIZE-SYSTEM-NAME now uppercases
;;;                 even strings for retrieving systems, and the comparison
;;;                 in *modules* is now case-insensitive. The result of
;;;                 this change is if the user cannot have distinct
;;;                 systems in "Foo.system" and "foo.system" named "Foo" and
;;;                 "foo", because they will clobber each other. There is
;;;                 still case-sensitivity on the filenames (i.e., if the
;;;                 system file is named "Foo.system" and you use "foo" in
;;;                 the :depends-on, it won't find it). We didn't take the
;;;                 further step of requiring system filenames to be lowercase
;;;                 because we actually find this kind of case-sensitivity
;;;                 to be useful, when maintaining two different versions
;;;                 of the same system.
;;;  7-MAR-95 mk    Added simplistic handling of logical pathnames. Also
;;;                 modified new-append-directories so that it'll try to
;;;                 split up pathname directories that are strings into a
;;;                 list of the directory components. Such directories aren't
;;;                 ANSI CL, but some non-conforming implementations do it.
;;;  7-MAR-95 mk    Added :proclamations to defsystem form, which can be used
;;;                 to set the compiler optimization level before compilation.
;;;                 For example,
;;;                  :proclamations '(optimize (safety 3) (speed 3) (space 0))
;;;  7-MAR-95 mk    Defsystem now tells the user when it reloads the system
;;;                 definition.
;;;  7-MAR-95 mk    Fixed problem pointed out by yc. If
;;;                 *source-pathname-default* is "" and there is no explicit
;;;                 :source-pathname specified for a file, the file could
;;;                 wind up with an empty file name. In other words, this
;;;                 global default shouldn't apply to :file components. Added
;;;                 explicit test for null strings, and when present replaced
;;;                 them with NIL (for binary as well as source, and also for
;;;                 :private-file components).
;;;  7-MAR-95 tar   Fixed defsystem to work on TI Explorers (TI CL).
;;;  7-MAR-95 jk    Added machine-type-translation for Decstation 5000/200
;;;                 under Allegro 3.1
;;;  7-MAR-95 as    Fixed bug in AKCL-1-615 in which defsystem added a
;;;                 subdirectory "RELATIVE" to all filenames.
;;;  7-MAR-95 mk    Added new test to test-new-append-directories to catch the
;;;                 error fixed by as. Essentially, this error occurs when the
;;;                 absolute-pathname has no directory (i.e., it has a single
;;;                 pathname component as in "foo" and not "foo/bar"). If
;;;                 RELATIVE ever shows up in the Result, we now know to
;;;                 add an extra conditionalization to prevent abs-keyword
;;;                 from being set to :relative.
;;;  7-MAR-95 ss    Miscellaneous fixes for MCL 2.0 final.
;;;                 *compile-file-verbose* not in MCL, *version variables
;;;                 need to occur before AFS-SOURCE-DIRECTORY definition,
;;;                 and certain code needed to be in the CCL: package.
;;;  8-MAR-95 mk    Y-OR-N-P-WAIT uses a busy-waiting. On Lisp systems where
;;;                 the time functions cons, such as CMU CL, this can cause a
;;;                 lot of ugly garbage collection messages. Modified the
;;;                 waiting to include calls to SLEEP, which should reduce
;;;                 some of the consing.
;;;  8-MAR-95 mk    Replaced fdmm's SET-LANGUAGE enhancement with a more
;;;                 general extension, along the lines suggested by akd.
;;;                 Defsystem now allows components to specify a :language
;;;                 slot, such as :language :lisp, :language :scheme. This
;;;                 slot is inherited (with the default being :lisp), and is
;;;                 used to obtain compilation and loading functions for
;;;                 components, as well as source and binary extensions. The
;;;                 compilation and loading functions can be overridden by
;;;                 specifying a :compiler or :loader in the system
;;;                 definition. Also added :documentation slot to the system
;;;                 definition.
;;;                    Where this comes in real handy is if one has a
;;;                 compiler-compiler implemented in Lisp, and wants the
;;;                 system to use the compiler-compiler to create a parser
;;;                 from a grammar and then compile parser. To do this one
;;;                 would create a module with components that looked
;;;                 something like this:
;;;		  ((:module cc :components ("compiler-compiler"))
;;;		   (:module gr :compiler 'cc :loader #'ignore
;;;			    :source-extension "gra"
;;;			    :binary-extension "lisp"
;;;			    :depends-on (cc)
;;;			    :components ("sample-grammar"))
;;;		   (:module parser :depends-on (gr)
;;;			    :components ("sample-grammar")))
;;;                 Defsystem would then compile and load the compiler, use
;;;                 it (the function cc) to compile the grammar into a parser,
;;;                 and then compile the parser. The only tricky part is
;;;                 cc is defined by the system, and one can't include #'cc
;;;                 in the system definition. However, one could include
;;;                 a call to mk:define-language in the compiler-compiler file,
;;;                 and define :cc as a language. This is the prefered method.
;;;  8-MAR-95 mk    New definition of topological-sort suggested by rs2. This
;;;                 version avoids the call to SORT, but in practice isn't
;;;                 much faster. However, it avoids the need to maintain a
;;;                 TIME slot in the topsort-node structure.
;;;  8-MAR-95 mk    rs2 also pointed out that the calls to MAKE-PATHNAME and
;;;                 NAMESTRING in COMPONENT-FULL-PATHNAME are a major reason
;;;                 why defsystem is slow. Accordingly, I've changed
;;;                 COMPONENT-FULL-PATHNAME to include a call to NAMESTRING
;;;                 (and removed all other calls to NAMESTRING), and also made
;;;                 a few changes to minimize the number of calls to
;;;                 COMPONENT-FULL-PATHNAME, such as memoizing it. See To Do
;;;                 below for other related comments.
;;;  8-MAR-95 mk    Added special hack requested by Steve Strassman, which
;;;                 allows one to specify absolute pathnames in the shorthand
;;;                 for a list of components, and have defsystem recognize
;;;                 which are absolute and which are relative.
;;;                 I actually think this would be a good idea, but I haven't
;;;                 tested it, so it is disabled by default. Search for
;;;                 *enable-straz-absolute-string-hack* to enable it.
;;;  8-MAR-95 kt    Fixed problem with EXPORT in AKCL 1.603, in which it wasn't
;;;                 properly exporting the value of the global export
;;;                 variables.
;;;  8-MAR-95 mk    Added UNMUNGE-LUCID to fix nasty problem with COMPILE-FILE
;;;                 in Lucid. Lucid apparently tries to merge the :output-file
;;;                 with the source file when the :output-file is a relative
;;;                 pathname. Wierd, and definitely non-standard.
;;;  9-MAR-95 mk    Changed ALLEGRO-MAKE-SYSTEM-FASL to also include the files
;;;                 in any systems the system depends on, as per a
;;;                 request of oc.
;;;  9-MAR-95 mk    Some version of CMU CL couldn't hack a call to
;;;                 MAKE-PATHNAME with :host NIL. I'm not sure which version
;;;                 it is, but the current version doesn't have this problem.
;;;                 If given :host nil, it defaults the host to
;;;                 COMMON-LISP::*UNIX-HOST*. So I haven't "fixed" this
;;;                 problem.
;;;  9-MAR-95 mk    Integrated top-level commands for Allegro designed by bha
;;;                 into the code, with slight modifications.
;;;  9-MAR-95 mk    Instead of having COMPUTE-SYSTEM-PATH check the current
;;;                 directory in a hard-coded fashion, include the current
;;;                 directory in the *central-registry*, as suggested by
;;;                 bha and others.
;;;  9-MAR-95 bha   Support for Logical Pathnames in Allegro.
;;;  9-MAR-95 mk    Added modified version of bha's DEFSYSPATH idea.
;;; 13-MAR-95 mk    Added a macro for the simple serial case, where a system
;;;                 (or module) is simple a list of files, each of which
;;;                 depends on the previous one. If the value of :components
;;;                 is a list beginning with :serial, it expands each
;;;                 component and makes it depend on the previous component.
;;;                 For example, (:serial "foo" "bar" "baz") would create a
;;;                 set of components where "baz" depended on "bar" and "bar"
;;;                 on "foo".
;;; 13-MAR-95 mk    *** Now version 3.0. This version is a interim bug-fix and
;;;                 update, since I do not have the time right now to complete
;;;                 the complete overhaul and redesign.
;;;                 Major changes in 3.0 include CMU CL 17, CLISP, ACLPC, TI,
;;;                 LispWorks and ACL(SGI) support, bug fixes for ACL 4.1/4.2.
;;; 14-MAR-95 fdmm  Finally added the bit of code to discriminate cleanly
;;;                 among different lisps without relying on (software-version)
;;;                 idiosyncracies.
;;;                 You can now customize COMPILER-TYPE-TRANSLATION so that
;;;                 AFS-BINARY-DIRECTORY can return a different value for
;;;                 different lisps on the same platform.
;;;                 If you use only one compiler, do not care about supporting
;;;                 code for multiple versions of it, and want less verbose
;;;                 directory names, just set *MULTIPLE-LISP-SUPPORT* to nil.
;;; 17-MAR-95 lmh   Added EVAL-WHEN for one of the MAKE-PACKAGE calls.
;;;                 CMU CL's RUN-PROGRAM is in the extensions package.
;;;                 ABSOLUTE-FILE-NAMESTRING-P was missing :test keyword
;;;                 Rearranged conditionalization in DIRECTORY-TO-LIST to
;;;                 suppress compiler warnings in CMU CL.
;;; 17-MAR-95 mk    Added conditionalizations to avoid certain CMU CL compiler
;;;                 warnings reported by lmh.
;;; 19990610  ma    Added shadowing of 'HARDCOPY-SYSTEM' for LW Personal Ed.

;;; 19991211  ma    NEW VERSION 4.0 started.
;;; 19991211  ma    Merged in changes requested by T. Russ of
;;;                 ISI. Please refer to the special "ISI" comments to
;;;                 understand these changes
;;; 20000228 ma     The symbols FIND-SYSTEM, LOAD-SYSTEM, DEFSYSTEM,
;;;                 COMPILE-SYSTEM and HARDCOPY-SYSTEM are no longer
;;;                 imported in the COMMON-LISP-USER package.
;;;                 Cfr. the definitions of *EXPORTS* and
;;;                 *SPECIAL-EXPORTS*.
;;; 2000-07-21 rlt  Add COMPILER-OPTIONS to defstruct to allow user to
;;;                 specify special compiler options for a particular
;;;                 component.
;;; 2002-01-08 kmr  Changed allegro symbols to lowercase to support
;;;                 case-sensitive images
;;;
;;; 2008-09-12 dsm  allegro-module-provider hack
;;;
;;; 2018-11-18 dsm  only set component-root-dir for subsystems, modules,
;;;                 files if not defined in the defsystem form.
;;;
;;; 2018-12-24 dsm  bail on read-char-wait on non-interactive streams
;;;                 (slime)
;;;
;;; 2018-12-24 dsm  allow :empty values for :source-extension and
;;;                 :binary-extension
;;;
;;; 2019-05-17 dsm  mkcl-1.1.1
;;;
;;; 2019-06-27 dsm  fix load-file-operation for static-file: mkcl's
;;;                 file-write-date gives an error on a dummy load op
;;;                 on a non-existent file
;;;
;;; 2019-09-01 dsm  fix traverse-pathname
;;;
;;; 2019-11-17 dsm  handle component-extensions - use
;;;                 component-extension-i local t on the parent when
;;;                 setting the extensions
;;;
;;; 2019-11-27 dsm  clean-system add a :propagate keyword argument
;;;                 which rebinds *operations-propagate-to-subsystems*
;;;                 and defaults to nil.
;;;
;;; 2020-01-18 dsm  system-relative-pathname hack
;;;
;;; 2020-01-22 dsm  export *file-local-variables* bind it around
;;;                 operations on :file and :private-file.
;;;
;;; 2020-01-27 dsm  package-inferred-hack nonsense
;;;
;;; 2020-02-29 dsm  fix component-full-pathname-i for Closure CL
;;;
;;; 2020-03-07 dsm  :description :license :if-feature support
;;;
;;; 2020-04-04 dsm  Deprecate, i.e. do not export
;;;                 system-definition-pathname (which computes a
;;;                 "modulename.system" pathname relative to the
;;;                 system source directory.  Record the truename of
;;;                 the system definition file when the system is
;;;                 loaded in
;;;                 *defsystem-to-defsystem-file-map*. Rewrite
;;;                 compute-system-path to fallback to this value when
;;;                 the system cannot be found in *central-registry*
;;;                 by the old means. make the second arg to
;;;                 compute-system-path optional (so it can be used
;;;                 instead of deprecated system-definition-pathname
;;;
;;; 2020-05-31 dsm  Try to fix ecl situations where the user calls
;;;                 (ext:install-c-compiler) and
;;;                 (ext:install-bytecodes-compiler) at runtime to
;;;                 change the compiler: make *filename-extensions* a
;;;                 defparameter so the user can reload defsystem.lisp
;;;                 to pick up the change between .fas and .fasc. Use
;;;                 the symbol compile-file instead of #'compile-file
;;;                 in (define-language :lisp) because ecl switches
;;;                 the fdefinitions for compile-file.
;;;
;;; 2020-06-15 dsm  Fix lispworks call-system-showing-output
;;;
;;; 2020-08-02 dsm  publish mk-oos find-system-pathname (TODO)
;;;                 missing-deps
;;;
;;; 2020-08-08 dsm  WIP. publish package-inferred-hack-dump-defsystem-file
;;;                 and asd-hack-dump-defsystem-file which assumes a
;;;                 convention of one definition per asd file and one
;;;                 asd file per directory. TODO split this commit
;;;                 into defsystem-extras.lisp
;;;
;;;
;;; 2020-10-15 dsm  support (:feature <feature-name> DEP) in :depends-on
;;;
;;; 2021-09-30 dsm  RUN-SHELL-COMMAND for CLISP
;;;
;;;
;;; 2021-11-08 dsm  (make-language output-files slot): low-level hook
;;;                 which takes a component object and produces a
;;;                 list of pathnames to be cleaned up. also clean up
;;;                 CLISP .lib and .cfp files in the delete-binaries op
;;;                 via delete-binaries-compute-output-files
;;;
;;; 2022-01-23 dsm  abcl support. courtesy marcoxa's c-l.net branch.
;;;
;;; 2022-05-03 dsm  stab at already-defined-systems
;;;
;;; 2022-10-23 dsm  handle output-translations. (SETQ
;;;                 MAKE:*OUTPUT-TRANSLATIONS-STRATEGY* :USER-CACHE)
;;;                 to actually support multiple lisps. Set
;;;                 CL-USER::*USER-CACHE-DIR* to ~/.cache outside
;;;                 defsystem, and system definitions without an
;;;                 explict :binary-pathname will have their binaries
;;;                 placed under this directory, in a location which
;;;                 is determined by the lisp implementation version,
;;;                 architecture and the truename of the file being
;;;                 compiled. (see comments below)
;;;
;;;
;;;                 *translate-output-for-user-cache-use-sxhash* if T
;;;                 replaces the full subpath of the source root
;;;                  within the cache directory direcory with
;;;                 a single directory component representing the
;;;                 sxhash of the source root directory.
;;;
;;; 2022-11-24 dsm  use a single mk-defsystem-module-provider-function for
;;;                 all lisps that provide module-provider-hooks
;;;
;;; 2022-12-05 dsm  clasp support. (some fixes compile time package
;;;                 issues fixed)
;;;
;;; 2022-12-05 dsm  make sure *dont-redefine-require* nil is honoured
;;;                 at runtime (for clasp)
;;;
;;; 2022-12-11 dsm  try to fix run-shell-command on allegro
;;;
;;; 2023-01-29 dsm  give specialized language loaders a chance to work
;;;                 without the stated source file present
;;;
;;; 2023-05-21 dsm  package-inferred-hack: handle pathname-complications where
;;;                 a :pathname argument is specified in asd form
;;;
;;; 2023-10-12 dsm  add a :no-lib-prefix boolean keyword argument to
;;;                 mk::mklib to skip prefixing "lib" to the name of
;;;                 the target (.so or .a) library file. so we have to
;;;                 retard add a :no-library-suffix to be able to
;;;                 produce files compatibile with make-build.  add
;;;                 :override to bypass the whole file name production
;;;                 logic altogether
;;;
;;; 2023-11-22 dsm  package-inferred-hack: handle all defsystem forms
;;;                 in asd file.
;;;
;;; 2024-03-24 dsm  component-full-pathname-i: try to return the
;;;                 directory if the component is a module. the
;;;                 string-concat hack is not likely to work.
;;;
;;; 2024-05-31 dsm  workarounds for mkcl defstruct redefinition problems.

;;;                 Redefining a struct as of mkcl-1.11.178 does not
;;;                 work even if the structure definition does not
;;;                 change. this causes a problem when trying to use
;;;                 mk-defsystem to mangae defsystem.lisp.  mk:oos
;;;                 :LOAD and :COMPILE operations redefine the
;;;                 defstructs and clobber all structure objects
;;;
;;;                 (:COMPILE-ONLY): new component-operation which
;;;                 skips the load operation.
;;;
;;;                 (MKLIB): takes a new keyword argument
;;;                 :skip-ensure-compiled which lets it skip the step
;;;                 where the system and (all subsystems) are compiled
;;;                 twice (first to produce first the fasl file and
;;;                 then to produce the shared objects for each
;;;                 component).  calls to find-system now supply :load
;;;                 by default.
;;;
;;;                 (ecl-compile-one-component): unused. call
;;;                 compile-file twice to first produce the fasl file
;;;                 and then produce the shared object (with :fasl-p
;;;                 nil for mkcl and :system-p t for ecl)
;;;
;;;                 (reset-pristine) clears all mk-defsystem state, so
;;;                 systems can be read again and new objects can be
;;;                 created.
;;;
;;; 2024-06-01 dsm  :delete-binaries: also delete ecl and mkcl shared
;;;                 artefacts. (arguably this should be done only when
;;;                 *ecl-compile-file-system-p* is t)


;;;---------------------------------------------------------------------------
;;; ISI Comments
;;;
;;; 19991211 Marco Antoniotti
;;; These comments come from the "ISI Branch".  I believe I did
;;; include the :load-always extension correctly.  The other commets
;;; seem superseded by other changes made to the system in the
;;; following years.  Some others are now useless with newer systems
;;; (e.g. filename truncation for new Windows based CL
;;; implementations.)

;;;  1-OCT-92 tar   Fixed problem with TI Lisp machines and append-directory.
;;;  1-OCT-92 tar   Made major modifications to compile-file-operation and
;;;                 load-file-operation to reduce the number of probe-file
;;;                 and write-date inquiries.  This makes the system run much
;;;                 faster through slow network connections.
;;; 13-OCT-92 tar   Added :load-always slot to components. If this slot is
;;;                 specified as non-NIL, always loads the component.
;;;                 This does not trigger dependent compilation.
;;;                 (This can be useful when macro definitions needed
;;;                 during compilation are changed by later files.  In
;;;                 this case, not reloading up-to-date files can
;;;                 cause different results.)
;;; 28-OCT-93 tar   Allegro 4.2 causes an error on (pathname-device nil)
;;; 14-SEP-94 tar   Disable importing of symbols into (CL-)USER package
;;;                 to minimize conflicts with other defsystem utilities.
;;; 10-NOV-94 tar   Added filename truncation code to support Franz Allegro
;;;                 PC with it's 8 character filename limitation.
;;; 15-MAY-98 tar   Changed host attribute for pathnames to support LispWorks
;;;                 (Windows) pathnames which reference other Drives.  Also
;;;                 updated file name convention.
;;;  9-NOV-98 tar   Updated new-append-directories for Lucid 5.0
;;;


;;; ********************************
;;; Ports **************************
;;; ********************************
;;;
;;;    DEFSYSTEM has been tested (successfully) in the following lisps:
;;;       CMU Common Lisp (M2.9 15-Aug-90, Compiler M1.8 15-Aug-90)
;;;       CMU Common Lisp (14-Dec-90 beta, Python Compiler 0.0 PMAX/Mach)
;;;       CMU Common Lisp 17f (Python 1.0)
;;;       Franz Allegro Common Lisp 3.1.12 (ExCL 3/30/90)
;;;       Franz Allegro Common Lisp 4.0/4.1/4.2
;;;       Franz Allegro Common Lisp for Windows (2.0)
;;;       Lucid Common Lisp (Version 2.1 6-DEC-87)
;;;       Lucid Common Lisp (3.0 [SPARC,SUN3])
;;;       Lucid Common Lisp (4.0 [SPARC,SUN3])
;;;       VAXLisp (v2.2) [VAX/VMS]
;;;       VAXLisp (v3.1)
;;;       Harlequin LispWorks
;;;       CLISP (CLISP3 [SPARC])
;;;       Symbolics XL12000 (Genera 8.3)
;;;       Scieneer Common Lisp (SCL) 1.1
;;;       Macintosh Common Lisp
;;;       ECL
;;;       ABCL
;;;
;;;    DEFSYSTEM needs to be tested in the following lisps:
;;;       OpenMCL
;;;       Symbolics Common Lisp (8.0)
;;;       KCL (June 3, 1987 or later)
;;;       AKCL (1.86, June 30, 1987 or later)
;;;       TI (Release 4.1 or later)
;;;       Ibuki Common Lisp (01/01, October 15, 1987)
;;;       Golden Common Lisp (3.1 IBM-PC)
;;;       HP Common Lisp (same as Lucid?)
;;;       Procyon Common Lisp

;;; ********************************
;;; To Do **************************
;;; ********************************
;;;
;;; COMPONENT-FULL-PATHNAME is a major source of slowness in the system
;;; because of all the calls to the expensive operations MAKE-PATHNAME
;;; and NAMESTRING. To improve performance, DEFSYSTEM should be reworked
;;; to avoid any need to call MAKE-PATHNAME and NAMESTRING, as the logical
;;; pathnames package does. Unfortunately, I don't have the time to do this
;;; right now. Instead, I installed a temporary improvement by memoizing
;;; COMPONENT-FULL-PATHNAME to cache previous calls to the function on
;;; a component by component and type by type basis. The cache is
;;; cleared before each call to OOS, in case filename extensions change.
;;; But DEFSYSTEM should really be reworked to avoid this problem and
;;; ensure greater portability and to also handle logical pathnames.
;;;
;;; Also, PROBE-FILE and FILE-WRITE-DATE are other sources of slowness.
;;; Perhaps by also memoizing FILE-WRITE-DATE and reimplementing PROBE-FILE
;;; in terms of FILE-WRITE-DATE, can achieve a further speed-up. This was
;;; suggested by Steven Feist (feist@ils.nwu.edu).
;;;
;;; True CLtL2 logical pathnames support -- can't do it, because CLtL2
;;; doesn't have all the necessary primitives, and even in Allegro CL 4.2
;;;   (namestring #l"foo:bar;baz.lisp")
;;; does not work properly.
;;;
;;; Create separate stand-alone documentation for defsystem, and also
;;; a test suite.
;;;
;;; Change SYSTEM to be a class instead of a struct, and make it a little
;;; more generic, so that it permits alternate system definitions.
;;; Replace OPERATE-ON-SYSTEM with MAP-SYSTEM (args: function, system-name,
;;; &rest options)
;;;
;;; Add a patch directory mechanism. Perhaps have several directories
;;; with code in them, and the first one with the specified file wins?
;;; LOAD-PATCHES function.
;;;
;;; Need way to load old binaries even if source is newer.
;;;
;;; Allow defpackage forms/package definitions in the defsystem? If
;;; a package not defined, look for and load a file named package.pkg?
;;;
;;; need to port for GNU CL (ala kcl)?
;;;
;;; Someone asked whether one can have :file components at top-level. I believe
;;; this is the case, but should double-check that it is possible (and if
;;; not, make it so).
;;;
;;; A common error/misconception seems to involve assuming that :system
;;; components should include the name of the system file, and that
;;; defsystem will automatically load the file containing the system
;;; definition and propagate operations to it. Perhaps this would be a
;;; nice feature to add.
;;;
;;; If a module is :load-only t, then it should not execute its :finally-do
;;; and :initially-do clauses during compilation operations, unless the
;;; module's files happen to be loaded during the operation.
;;;
;;; System Class. Customizable delimiters.
;;;
;;; Load a system (while not loading anything already loaded)
;;; and inform the user of out of date fasls with the choice
;;; to load the old fasl or recompile and then load the new
;;; fasl?
;;;
;;; modify compile-file-operation to handle a query keyword....
;;;
;;; Perhaps systems should keep around the file-write-date of the system
;;; definition file, to prevent excessive reloading of the system definition?
;;;
;;; load-file-operation needs to be completely reworked to simplify the
;;; logic of when files get loaded or not.
;;;
;;; Need to revamp output: Nesting and indenting verbose output doesn't
;;; seem cool, especially when output overflows the 80-column margins.
;;;
;;; Document various ways of writing a system. simple (short) form
;;; (where :components is just a list of filenames) in addition to verbose.
;;; Put documentation strings in code.
;;;
;;; :load-time for modules and systems -- maybe record the time the system
;;; was loaded/compiled here and print it in describe-system?
;;;
;;; Make it easy to define new functions that operate on a system. For
;;; example, a function that prints out a list of files that have changed,
;;; hardcopy-system, edit-system, etc.
;;;
;;; If a user wants to have identical systems for different lisps, do we
;;; force the user to use logical pathnames? Or maybe we should write a
;;; generic-pathnames package that parses any pathname format into a
;;; uniform underlying format (i.e., pull the relevant code out of
;;; logical-pathnames.lisp and clean it up a bit).
;;;
;;;    Verify that Mac pathnames now work with append-directories.
;;;
;;; A common human error is to violate the modularization by making a file
;;; in one module depend on a file in another module, instead of making
;;; one module depend on the other. This is caught because the dependency
;;; isn't found. However, is there any way to provide a more informative
;;; error message? Probably not, especially if the system has multiple
;;; files of the same name.
;;;
;;; For a module none of whose files needed to be compiled, have it print out
;;; "no files need recompilation".
;;;
;;; Write a system date/time to a file? (version information) I.e., if the
;;; filesystem supports file version numbers, write an auxiliary file to
;;; the system definition file that specifies versions of the system and
;;; the version numbers of the associated files.
;;;
;;; Add idea of a patch directory.
;;;
;;; In verbose printout, have it log a date/time at start and end of
;;; compilation:
;;;     Compiling system "test" on 31-Jan-91 21:46:47
;;;     by Defsystem version v2.0 01-FEB-91.
;;;
;;; Define other :force options:
;;;    :query    allows user to specify that a file not normally compiled
;;;              should be. OR
;;;    :confirm  allows user to specify that a file normally compiled
;;;              shouldn't be. AND
;;;
;;; We currently assume that compilation-load dependencies and if-changed
;;; dependencies are identical. However, in some cases this might not be
;;; true. For example, if we change a macro we have to recompile functions
;;; that depend on it (except in lisps that automatically do this, such
;;; as the new CMU Common Lisp), but not if we change a function. Splitting
;;; these apart (with appropriate defaulting) would be nice, but not worth
;;; doing immediately since it may save only a couple of file recompilations,
;;; while making defsystem much more complex than it already is.
;;;
;;; Current dependencies are limited to siblings. Maybe we should allow
;;; nephews and uncles? So long as it is still a DAG, we can sort it.
;;; Answer: No. The current setup enforces a structure on the modularity.
;;; Otherwise, why should we have modules if we're going to ignore it?
;;;
;;; Currently a file is recompiled more or less if the source is newer
;;; than the binary or if the file depends on a file that has changed
;;; (i.e., was recompiled in this session of a system operation).
;;; Neil Goldman <goldman@isi.edu> has pointed out that whether a file
;;; needs recompilation is really independent of the current session of
;;; a system operation, and depends only on the file-write-dates of the
;;; source and binary files for a system. Thus a file should require
;;; recompilation in the following circumstances:
;;;   1. If a file's source is newer than its binary, or
;;;   2. If a file's source is not newer than its binary, but the file
;;;      depends directly or indirectly on a module (or file) that is newer.
;;;      For a regular file use the file-write-date (FWD) of the source or
;;;      binary, whichever is more recent. For a load-only file, use the only
;;;      available FWD. For a module, use the most recent (max) FWD of any of
;;;      its components.
;;; The impact of this is that instead of using a boolean CHANGED variable
;;; throughout the code, we need to allow CHANGED to be NIL/T/<FWD> or
;;; maybe just the FWD timestamp, and to use the value of CHANGED in
;;; needs-compilation decisions. (Use of NIL/T as values is an optimization.
;;; The FWD timestamp which indicates the most recent time of any changes
;;; should be sufficient.) This will affect not just the
;;; compile-file-operation, but also the load-file-operation because of
;;; compilation during load. Also, since FWDs will be used more prevalently,
;;; we probably should couple this change with the inclusion of load-times
;;; in the component defstruct. This is a tricky and involved change, and
;;; requires more thought, since there are subtle cases where it might not
;;; be correct. For now, the change will have to wait until the DEFSYSTEM
;;; redesign.

;;; ********************************************************************
;;; How to Use this System *********************************************
;;; ********************************************************************

;;; To use this system,
;;; 1. If you want to have a central registry of system definitions,
;;;    modify the value of the variable *central-registry* below.
;;; 2. Load this file (defsystem.lisp) in either source or compiled form,
;;; 3. Load the file containing the "defsystem" definition of your system,
;;; 4. Use the function "operate-on-system" to do things to your system.

;;; For more information, see the documentation and examples in
;;; lisp-utilities.ps.

;;; ********************************
;;; Usage Comments *****************
;;; ********************************

;;; If you use symbols in the system definition file, they get interned in
;;; the COMMON-LISP-USER package, which can lead to name conflicts when
;;; the system itself seeks to export the same symbol to the COMMON-LISP-USER
;;; package. The workaround is to use strings instead of symbols for the
;;; names of components in the system definition file. In the major overhaul,
;;; perhaps the user should be precluded from using symbols for such
;;; identifiers.
;;;
;;; If you include a tilde in the :source-pathname in Allegro, as in "~/lisp",
;;; file name expansion is much slower than if you use the full pathname,
;;; as in "/user/USERID/lisp".
;;;

;;#+clasp
;;(in-package "CL-USER")


;;; ****************************************************************
;;; Lisp Code ******************************************************
;;; ****************************************************************

;;; ********************************
;;; Massage CLtL2 onto *features* **
;;; ********************************
;;; Let's be smart about CLtL2 compatible Lisps:

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+(or (and allegro-version>= (version>= 4 0)) :mcl :sbcl)
  (pushnew :cltl2 *features*))


;;; ********************************
;;; Provide/Require/*modules* ******
;;; ********************************

;;; Since CLtL2 has dropped require and provide from the language, some
;;; lisps may not have the functions PROVIDE and REQUIRE and the
;;; global *MODULES*. So if lisp::provide and user::provide are not
;;; defined, we define our own.

;;; Hmmm. CMU CL old compiler gives bogus warnings here about functions
;;; and variables not being declared or bound, apparently because it
;;; sees that (or (fboundp 'lisp::require) (fboundp 'user::require)) returns
;;; T, so it doesn't really bother when compiling the body of the unless.
;;; The new compiler does this properly, so I'm not going to bother
;;; working around this.

;;; Some Lisp implementations return bogus warnings about assuming
;;; *MODULE-FILES* and *LIBRARY* to be special, and CANONICALIZE-MODULE-NAME
;;; and MODULE-FILES being undefined. Don't worry about them.

;;; Now that ANSI CL includes PROVIDE and REQUIRE again, is this code
;;; necessary?

#-(or :CMU
      :vms
      :mcl
      :lispworks
      :clisp
      :gcl
      :sbcl
      :cormanlisp
      :scl
      :clozure-common-lisp
      :ecl
      :mkcl
      (and allegro-version>= (version>= 4 1))

      :abcl :armedbear
      :clasp
      )
(eval-when #-(or :lucid)
           (:compile-toplevel :load-toplevel :execute)
	   #+(or :lucid)
           (compile load eval)

  (unless (or (fboundp 'lisp::require)
	      (fboundp 'user::require)

	      #+(and :excl (and allegro-version>= (version>= 4 0)))
	      (fboundp 'cltl1::require)

	      #+:lispworks
	      (fboundp 'system::require))

    #-:lispworks
    (in-package "LISP")
    #+:lispworks
    (in-package "SYSTEM")

    (export '(*modules* provide require))

    ;; Documentation strings taken almost literally from CLtL1.

    (defvar *modules* ()
      "List of names of the modules that have been loaded into Lisp so far.
     It is used by PROVIDE and REQUIRE.")

    ;; We provide two different ways to define modules. The default way
    ;; is to put either a source or binary file with the same name
    ;; as the module in the library directory. The other way is to define
    ;; the list of files in the module with defmodule.

    ;; The directory listed in *library* is implementation dependent,
    ;; and is intended to be used by Lisp manufacturers as a place to
    ;; store their implementation dependent packages.
    ;; Lisp users should use systems and *central-registry* to store
    ;; their packages -- it is intended that *central-registry* is
    ;; set by the user, while *library* is set by the lisp.

    (defvar *library* nil		; "/usr/local/lisp/Modules/"
      "Directory within the file system containing files, where the name
     of a file is the same as the name of the module it contains.")

    (defvar *module-files* (make-hash-table :test #'equal)
      "Hash table mapping from module names to list of files for the
     module. REQUIRE loads these files in order.")

    (defun canonicalize-module-name (name)
      ;; if symbol, string-downcase the printrep to make nicer filenames.
      (if (stringp name) name (string-downcase (string name))))

    (defmacro defmodule (name &rest files)
      "Defines a module NAME to load the specified FILES in order."
      `(setf (gethash (canonicalize-module-name ,name) *module-files*)
	     ',files))
    (defun module-files (name)
      (gethash name *module-files*))

    (defun provide (name)
      "Adds a new module name to the list of modules maintained in the
     variable *modules*, thereby indicating that the module has been
     loaded. Name may be a string or symbol -- strings are case-senstive,
     while symbols are treated like lowercase strings. Returns T if
     NAME was not already present, NIL otherwise."
      (let ((module (canonicalize-module-name name)))
	(unless (find module *modules* :test #'string=)
	  ;; Module not present. Add it and return T to signify that it
	  ;; was added.
	  (push module *modules*)
	  t)))

    (defun require (name &optional pathname)
      "Tests whether a module is already present. If the module is not
     present, loads the appropriate file or set of files. The pathname
     argument, if present, is a single pathname or list of pathnames
     whose files are to be loaded in order, left to right. If the
     pathname is nil, the system first checks if a module was defined
     using defmodule and uses the pathnames so defined. If that fails,
     it looks in the library directory for a file with name the same
     as that of the module. Returns T if it loads the module."
      (let ((module (canonicalize-module-name name)))
	(unless (find module *modules* :test #'string=)
	  ;; Module is not already present.
	  (when (and pathname (not (listp pathname)))
	    ;; If there's a pathname or pathnames, ensure that it's a list.
	    (setf pathname (list pathname)))
	  (unless pathname
	    ;; If there's no pathname, try for a defmodule definition.
	    (setf pathname (module-files module)))
	  (unless pathname
	    ;; If there's still no pathname, try the library directory.
	    (when *library*
	      (setf pathname (concatenate 'string *library* module))
	      ;; Test if the file exists.
	      ;; We assume that the lisp will default the file type
	      ;; appropriately. If it doesn't, use #+".fasl" or some
	      ;; such in the concatenate form above.
	      (if (probe-file pathname)
		  ;; If it exists, ensure we've got a list
		  (setf pathname (list pathname))
		  ;; If the library file doesn't exist, we don't want
		  ;; a load error.
		  (setf pathname nil))))
	  ;; Now that we've got the list of pathnames, let's load them.
	  (dolist (pname pathname t)
	    (load pname :verbose nil))))))
  ) ; eval-when

;;; ********************************
;;; Set up Package *****************
;;; ********************************


;;; Unfortunately, lots of lisps have their own defsystems, some more
;;; primitive than others, all uncompatible, and all in the DEFSYSTEM
;;; package. To avoid name conflicts, we've decided to name this the
;;; MAKE package. A nice side-effect is that the short nickname
;;; MK is my initials.

#+(or clisp cormanlisp ecl mkcl (and gcl defpackage) sbcl abcl clasp)
(defpackage "MAKE" (:use "COMMON-LISP") (:nicknames "MK"))

;;; For CLtL2 compatible lisps...
#+(and :excl :allegro-v4.0 :cltl2)
(defpackage "MAKE" (:nicknames "MK" "make" "mk") (:use :common-lisp)
	    (:import-from cltl1 *modules* provide require))

;;; *** Marco Antoniotti <marcoxa@icsi.berkeley.edu> 19970105
;;; In Allegro 4.1, 'provide' and 'require' are not external in
;;; 'CLTL1'.  However they are in 'COMMON-LISP'.  Hence the change.
#+(and :excl :allegro-v4.1 :cltl2)
(defpackage "MAKE" (:nicknames "MK" "make" "mk") (:use :common-lisp) )

#+(and :excl :allegro-version>= (version>= 4 2))
(defpackage "MAKE" (:nicknames "MK" "make" "mk") (:use :common-lisp))

#+:lispworks
(defpackage "MAKE" (:nicknames "MK") (:use "COMMON-LISP")
	    (:import-from "SYSTEM" *modules* provide require)
	    (:export "DEFSYSTEM" "COMPILE-SYSTEM" "LOAD-SYSTEM"
		     "DEFINE-LANGUAGE" "*MULTIPLE-LISP-SUPPORT*"))

#+:mcl
(defpackage "MAKE" (:nicknames "MK") (:use "COMMON-LISP")
  (:import-from ccl *modules* provide require))

#+:clozure-common-lisp
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defpackage "MAKE" (:nicknames "MK") (:use "COMMON-LISP")
    (:import-from ccl *modules* provide require)))


;;; *** Marco Antoniotti <marcoxa@icsi.berkeley.edu> 19951012
;;; The code below, is originally executed also for CMUCL. However I
;;; believe this is wrong, since CMUCL comes with its own defpackage.
;;; I added the extra :CMU in the 'or'.
#+(and :cltl2 (not (or :cmu :clisp :sbcl :abcl
		       (and :excl (or :allegro-v4.0 :allegro-v4.1))
		       :mcl)))
(eval-when (compile load eval)
  (unless (find-package "MAKE")
    (make-package "MAKE" :nicknames '("MK") :use '("COMMON-LISP"))))

;;; *** Marco Antoniotti <marcoxa@icsi.berkeley.edu> 19951012
;;; Here I add the proper defpackage for CMU
#+:CMU
(defpackage "MAKE" (:use "COMMON-LISP" "CONDITIONS")
  (:nicknames "MK"))

#+:sbcl
(defpackage "MAKE" (:use "COMMON-LISP")
  (:nicknames "MK"))

#+:scl
(defpackage :make (:use :common-lisp)
  (:nicknames :mk))



;;;; madhu 220223 if we defined the MAKE package before loading this
;;;; file (to set some defvars before the defvars are defined), then
;;;; abcl fails because a subsequent defpackage form does not pick up
;;;; :USE and :NICKNAMES.
(eval-when (load eval compile)
(let ((cl (find-package "CL")))
  (unless (find cl (package-use-list "MAKE"))
    (warn "package: ADDING CL to MAKE use list")
    (use-package '("CL") "MAKE")))

(let ((nicknames (package-nicknames "MAKE")))
  (unless (find "MK" nicknames :test #'equal)
    (warn "package: ADDING nickname MK to MAKE")
    (rename-package (find-package "MAKE")  "MAKE"
		    (cons "MK" nicknames))))
)

#-(or :abcl :sbcl :cltl2 :lispworks :ecl :mkcl :scl :clozure-common-lisp :clasp cmu)
(in-package "MAKE" :nicknames '("MK"))

#+(or :cltl2 :lispworks :scl :clozure-common-lisp)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (in-package "MAKE"))

#+(or ecl mkcl clasp cmu)
(in-package "MAKE")

#+:abcl
(in-package "MAKE")

;;; *** Marco Antoniotti <marcoxa@icsi.berkeley.edu> 19970105
;;; 'provide' is not esternal in 'CLTL1' in Allegro v 4.1
#+(and :excl :allegro-v4.0 :cltl2)
(cltl1:provide 'make)
#+(and :excl :allegro-v4.0 :cltl2)
(provide 'make)

#+:openmcl
(cl:provide 'make)

#+(and :mcl (not :openmcl))
(ccl:provide 'make)

#+(and :cltl2 (not (or (and :excl (or :allegro-v4.0 :allegro-v4.1)) :mcl)))
(provide 'make)

#+:lispworks
(provide 'make)

#-(or :cltl2 :lispworks)
(provide 'make)

#+(or :abcl :armedbear)
(provide 'make)

#+clasp
(provide 'make)

(pushnew :mk-defsystem *features*)

;;; Some compatibility issues.  Mostly for CormanLisp.
;;; 2002-02-20 Marco Antoniotti

#+cormanlisp
(defun compile-file-pathname (pathname-designator)
 (merge-pathnames (make-pathname :type "fasl")
		  (etypecase pathname-designator
		    (pathname pathname-designator)
		    (string (parse-namestring pathname-designator))
		    ;; We need FILE-STREAM here as well.
		    )))

#+cormanlisp
(defun file-namestring (pathname-designator)
  (let ((p (etypecase pathname-designator
	     (pathname pathname-designator)
	     (string (parse-namestring pathname-designator))
	     ;; We need FILE-STREAM here as well.
	     )))
    (namestring (make-pathname :directory ()
			       :name (pathname-name p)
			       :type (pathname-type p)
			       :version (pathname-version p)))))

;;; The external interface consists of *exports* and *other-exports*.

;;; AKCL (at least 1.603) grabs all the (export) forms and puts them up top in
;;; the compile form, so that you can't use a defvar with a default value and
;;; then a succeeding export as well.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *special-exports* nil)
  (defvar *exports* nil)
  (defvar *other-exports* nil)

  (export (setq *exports*
                '(operate-on-system
                  oos
                  afs-binary-directory afs-source-directory
                  files-in-system)))
  (export (setq *special-exports*
                '()))
  (export (setq *other-exports*
                '(*central-registry*
                  *bin-subdir*

                  add-registry-location
                  list-central-registry-directories
                  print-central-registry-directories
                  find-system
                  defsystem compile-system load-system hardcopy-system

;; DUBIOUS ;madhu 200404
;;                  system-definition-pathname

                  missing-component
                  missing-component-name
                  missing-component-component
                  missing-module
                  missing-system

                  register-foreign-system

                  machine-type-translation
                  software-type-translation
                  compiler-type-translation
                  ;; require
                  define-language
                  allegro-make-system-fasl
                  files-which-need-compilation
                  undefsystem
                  defined-systems
                  describe-system clean-system edit-system ;hardcopy-system
                  system-source-size make-system-tag-table
                  *defsystem-version*
                  *compile-during-load*
                  *minimal-load*
                  *dont-redefine-require*
                  *files-missing-is-an-error*
                  *reload-systems-from-disk*
                  *source-pathname-default*
                  *binary-pathname-default*
                  *multiple-lisp-support*

                  run-unix-program
                  *default-shell*
                  run-shell-command
                  )))
  )


;;; We import these symbols into the USER package to make them
;;; easier to use. Since some lisps have already defined defsystem
;;; in the user package, we may have to shadowing-import it.
#|
#-(or :sbcl :cmu :ccl :allegro :excl :lispworks :symbolics)
(eval-when (compile load eval)
  (import *exports* #-(or :cltl2 :lispworks) "USER"
	            #+(or :cltl2 :lispworks) "COMMON-LISP-USER")
  (import *special-exports* #-(or :cltl2 :lispworks) "USER"
	                    #+(or :cltl2 :lispworks) "COMMON-LISP-USER"))
#+(or :sbcl :cmu :ccl :allegro :excl :lispworks :symbolics)
(eval-when (compile load eval)
  (import *exports* #-(or :cltl2 :lispworks) "USER"
	            #+(or :cltl2 :lispworks) "COMMON-LISP-USER")
  (shadowing-import *special-exports*
		    #-(or :cltl2 :lispworks) "USER"
		    #+(or :cltl2 :lispworks) "COMMON-LISP-USER"))
|#

#-(or :PCL :CLOS :scl)
(when (find-package "PCL")
  (pushnew :pcl *modules*)
  (pushnew :pcl *features*))


;;; ********************************
;;; Defsystem Version **************
;;; ********************************
(defparameter *defsystem-version* "3.8 Interim, 20200712 (locally patched)"
  "Current version number/date for MK:DEFSYSTEM.")


;;; ********************************
;;; Customizable System Parameters *
;;; ********************************

(defvar *dont-redefine-require*
  #+cmu (if (find-symbol "*MODULE-PROVIDER-FUNCTIONS*" "EXT") t nil)
  #+:clozure-common-lisp t
  #+(or clisp sbcl) t
  #+allegro t
  #+mkcl t
  #+ecl t
  #+abcl t
  #+clasp t
  #-(or abcl cmu sbcl clisp allegro :clozure-common-lisp mkcl ecl
	clasp)
  nil
  "If T, prevents the redefinition of REQUIRE.
This is useful for lisps that treat REQUIRE specially in the compiler.")


(defvar *multiple-lisp-support* t
  "If T, afs-binary-directory will try to return a name dependent
on the particular lisp compiler version being used.")


;;; home-subdirectory --
;;; HOME-SUBDIRECTORY is used only in *central-registry* below.
;;; Note that CMU CL 17e does not understand the ~/ shorthand for home
;;; directories.
;;;
;;; Note:
;;; 20020220 Marco Antoniotti
;;; The #-cormanlisp version is the original one, which is broken anyway, since
;;; it is UNIX dependent.
;;; I added the kludgy #+cormalisp (v 1.5) one, since it is missing
;;; the ANSI USER-HOMEDIR-PATHNAME function.

#-cormanlisp
(defun home-subdirectory (directory)
  (concatenate 'string
	#+(or :sbcl :cmu :scl)
	"home:"
	#-(or :sbcl :cmu :scl)
	(let ((homedir (user-homedir-pathname)))
	  (or (and homedir (namestring homedir))
	      "~/"))
	directory))


#+cormanlisp
(defun home-subdirectory (directory)
  (declare (type string directory))
  (concatenate 'string "C:\\" directory))


;;; The following function is available for users to add
;;;   (setq mk:*central-registry* (defsys-env-search-path))
;;; to Lisp init files in order to use the value of the DEFSYSPATH
;;; instead of directly coding it in the file.

#+:allegro
(defun defsys-env-search-path ()
  "This function grabs the value of the DEFSYSPATH environment variable
   and breaks the search path into a list of paths."
  (remove-duplicates (split-string (sys:getenv "DEFSYSPATH") :item #\:)
		     :test #'string-equal))


;;; Change this variable to set up the location of a central
;;; repository for system definitions if you want one.
;;; This is a defvar to allow users to change the value in their
;;; lisp init files without worrying about it reverting if they
;;; reload defsystem for some reason.

;;; Note that if a form is included in the registry list, it will be evaluated
;;; in COMPUTE-SYSTEM-PATH to return the appropriate directory to check.

(defvar *central-registry*
  `(;; Current directory
    "./"
    #+:LUCID     (working-directory)
    #+ACLPC      (current-directory)
    #+:allegro   (excl:current-directory)
    #+:clisp     (ext:default-directory)
    #+:sbcl      (progn *default-pathname-defaults*)
    #+:clasp     (ext:getcwd)
    #+(or :cmu :scl)       (ext:default-directory)
    ;; *** Marco Antoniotti <marcoxa@icsi.berkeley.edu>
    ;; Somehow it is better to qualify default-directory in CMU with
    ;; the appropriate package (i.e. "EXTENSIONS".)
    ;; Same for Allegro.
    #+(and :lispworks
           (not :lispworks4)
           (not :lispworks5)
           (not :lispworks6)
	   (not :lispworks7))
    ,(multiple-value-bind (major minor)
			  #-:lispworks-personal-edition
			  (system::lispworks-version)
			  #+:lispworks-personal-edition
			  (values system::*major-version-number*
				  system::*minor-version-number*)
       (if (or (> major 3)
	       (and (= major 3) (> minor 2))
	       (and (= major 3) (= minor 2)
		    (equal (lisp-implementation-version) "3.2.1")))
	   `(make-pathname :directory
			   ,(find-symbol "*CURRENT-WORKING-DIRECTORY*"
					 (find-package "SYSTEM")))
           (find-symbol "*CURRENT-WORKING-DIRECTORY*"
                        (find-package "LW"))))
    #+(or :lispworks4 :lispworks5 :lispworks6 :lispworks7)
    (hcl:get-working-directory)
    ;; Home directory
    #-sbcl
    (mk::home-subdirectory "lisp/systems/")

    ;; Global registry
    #+unix (pathname "/usr/local/lisp/Registry/")
    )
  "Central directory of system definitions.
May be either a single directory pathname, or a list of directory
pathnames to be checked after the local directory.")


(defun add-registry-location (pathname)
  "Adds a path to the central registry."
  (pushnew pathname *central-registry* :test #'equal))


(defun registry-pathname (registry)
  "Return the pathname represented by the element of *CENTRAL-REGISTRY*."
  (typecase registry
    (string (pathname registry))
    (pathname registry)
    (otherwise (pathname (eval registry)))))


(defun print-central-registry-directories (&optional (stream *standard-output*))
  (dolist (registry *central-registry*)
    (print (registry-pathname registry) stream)))


(defun list-central-registry-directories ()
  (mapcar #'registry-pathname *central-registry*))


(defvar *bin-subdir* ".bin/"
  "The subdirectory of an AFS directory where the binaries are really kept.")


;;; These variables set up defaults for operate-on-system, and are used
;;; for communication in lieu of parameter passing. Yes, this is bad,
;;; but it keeps the interface small. Also, in the case of the -if-no-binary
;;; variables, parameter passing would require multiple value returns
;;; from some functions. Why make life complicated?

(defvar *tell-user-when-done* nil
  "If T, system will print ...DONE at the end of an operation")

(defvar *oos-verbose* nil
  "Operate on System Verbose Mode")

(defvar *oos-test* nil
  "Operate on System Test Mode")

(defvar *load-source-if-no-binary* nil
  "If T, system will try loading the source if the binary is missing")

(defvar *bother-user-if-no-binary* t
  "If T, the system will ask the user whether to load the source if
   the binary is missing")

(defvar *load-source-instead-of-binary* nil
  "If T, the system will load the source file instead of the binary.")

(defvar *compile-during-load* :query
  "If T, the system will compile source files during load if the
binary file is missing. If :query, it will ask the user for
permission first.")

(defvar *minimal-load* nil
  "If T, the system tries to avoid reloading files that were already loaded
and up to date.")

(defvar *files-missing-is-an-error* t
  "If both the source and binary files are missing, signal a continuable
   error instead of just a warning.")

(defvar *operations-propagate-to-subsystems* t
  "If T, operations like :COMPILE and :LOAD propagate to subsystems
   of a system that are defined either using a component-type of :system
   or by another defsystem form.")

;;; Particular to CMULisp

(defvar *compile-error-file-type* "err"
  "File type of compilation error file in cmulisp")

(defvar *cmu-errors-to-terminal* t
  "Argument to :errors-to-terminal in compile-file in cmulisp")

(defvar *cmu-errors-to-file* t
  "If T, cmulisp will write an error file during compilation")


;;; ********************************
;;; Global Variables ***************
;;; ********************************

;;; Massage people's *features* into better shape.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (dolist (feature *features*)
    (when (and (symbolp feature)   ; 3600
               (equal (symbol-name feature) "CMU"))
      (pushnew :CMU *features*)))

  #+Lucid
  (when (search "IBM RT PC" (machine-type))
    (pushnew :ibm-rt-pc *features*))
  )


;;; *filename-extensions* is a cons of the source and binary extensions.
(defparameter *filename-extensions*
  (car `(#+(and Symbolics Lispm)              ("lisp" . "bin")
         #+(and dec common vax (not ultrix))  ("LSP"  . "FAS")
         #+(and dec common vax ultrix)        ("lsp"  . "fas")
 	 #+ACLPC                              ("lsp"  . "fsl")
 	 #+CLISP                              ("lisp" . "fas")
         #+KCL                                ("lsp"  . "o")
;;         #+ECL                                ("lsp"  . "so")
         #+IBCL                               ("lsp"  . "o")
         #+Xerox                              ("lisp" . "dfasl")
	 ;; Lucid on Silicon Graphics
	 #+(and Lucid MIPS)                   ("lisp" . "mbin")
	 ;; the entry for (and lucid hp300) must precede
	 ;; that of (and lucid mc68000) for hp9000/300's running lucid,
	 ;; since *features* on hp9000/300's also include the :mc68000
	 ;; feature.
	 #+(and lucid hp300)                  ("lisp" . "6bin")
         #+(and Lucid MC68000)                ("lisp" . "lbin")
         #+(and Lucid Vax)                    ("lisp" . "vbin")
         #+(and Lucid Prime)                  ("lisp" . "pbin")
         #+(and Lucid SUNRise)                ("lisp" . "sbin")
         #+(and Lucid SPARC)                  ("lisp" . "sbin")
         #+(and Lucid :IBM-RT-PC)             ("lisp" . "bbin")
	 ;; PA is Precision Architecture, HP's 9000/800 RISC cpu
	 #+(and Lucid PA)		      ("lisp" . "hbin")
         #+excl ("cl"   . ,(pathname-type (compile-file-pathname "foo.cl")))
         #+(or :cmu :scl)  ("lisp" . ,(or (c:backend-fasl-file-type c:*backend*) "fasl"))
;	 #+(and :CMU (not (or :sgi :sparc)))  ("lisp" . "fasl")
;        #+(and :CMU :sgi)                    ("lisp" . "sgif")
;        #+(and :CMU :sparc)                  ("lisp" . "sparcf")
	 #+PRIME                              ("lisp" . "pbin")
         #+HP                                 ("l"    . "b")
         #+TI ("lisp" . #.(string (si::local-binary-file-type)))
         #+:gclisp                            ("LSP"  . "F2S")
         #+pyramid                            ("clisp" . "o")

	 ;; Harlequin LispWorks
	 #+:lispworks 	      ("lisp" . ,COMPILER:*FASL-EXTENSION-STRING*)
;        #+(and :sun4 :lispworks)             ("lisp" . "wfasl")
;        #+(and :mips :lispworks)             ("lisp" . "mfasl")
         #+:mcl                               ("lisp" . ,(pathname-type (compile-file-pathname "foo.lisp")))
         #+:coral                             ("lisp" . "fasl")

         ;; Otherwise,
         ("lisp" . ,(pathname-type (compile-file-pathname "foo.lisp")))))
  "Filename extensions for Common Lisp.
A cons of the form (Source-Extension . Binary-Extension). If the
system is unknown (as in *features* not known), defaults to lisp and
fasl.")

(defvar *system-extension*
  ;; MS-DOS systems can only handle three character extensions.
  #-ACLPC "system"
  #+ACLPC "sys"
  "The filename extension to use with systems.")


;;; The above variables and code should be extended to allow a list of
;;; valid extensions for each lisp implementation, instead of a single
;;; extension. When writing a file, the first extension should be used.
;;; But when searching for a file, every extension in the list should
;;; be used. For example, CMU Common Lisp recognizes "lisp" "l" "cl" and
;;; "lsp" (*load-source-types*) as source code extensions, and
;;; (c:backend-fasl-file-type c:*backend*)
;;; (c:backend-byte-fasl-file-type c:*backend*)
;;; and "fasl" as binary (object) file extensions (*load-object-types*).

;;; Note that the above code is used below in the LANGUAGE defstruct.

;;; There is no real support for this variable being nil, so don't change it.
;;; Note that in any event, the toplevel system (defined with defsystem)
;;; will have its dependencies delayed. Not having dependencies delayed
;;; might be useful if we define several systems within one defsystem.

(defvar *system-dependencies-delayed* t
  "If T, system dependencies are expanded at run time")


;;; Replace this with consp, dammit!
(defun non-empty-listp (list)
  (and list (listp list)))


;;; ********************************
;;; Component Operation Definition *
;;; ********************************
(eval-when (:compile-toplevel :load-toplevel :execute)

(defvar *version-dir* nil
  "The version subdir. bound in operate-on-system.")

(defvar *version-replace* nil
  "The version replace. bound in operate-on-system.")

(defvar *version* nil
  "Default version."))

(defvar *component-operations* (make-hash-table :test #'equal)
  "Hash table of (operation-name function) pairs.")

(defun component-operation (name &optional operation)
  (if operation
      (setf (gethash name *component-operations*) operation)
      (gethash name *component-operations*)))


;;; ********************************
;;; AFS @sys immitator *************
;;; ********************************

;;; mc 11-Apr-91: Bashes MCL's point reader, so commented out.
#-(or :allegro :mcl :ecl :mkcl :clisp)
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Define #@"foo" as a shorthand for (afs-binary-directory "foo").
  ;; For example,
  ;;    <cl> #@"foo"
  ;;    "foo/.bin/rt_mach/"
  (set-dispatch-macro-character
   #\# #\@
   #'(lambda (stream char arg)
       (declare (ignore char arg))
       `(afs-binary-directory ,(read stream t nil t)))))


(defvar *find-irix-version-script*
    "\"1,4 d\\
s/^[^M]*IRIX Execution Environment 1, *[a-zA-Z]* *\\([^ ]*\\)/\\1/p\\
/./,$ d\\
\"")


(defun operating-system-version ()
  #+(and :sgi :excl)
  (let* ((full-version (software-version))
	 (blank-pos (search " " full-version))
	 (os (subseq full-version 0 blank-pos))
	 (version-rest (subseq full-version
			       (1+ blank-pos)))
	 os-version)
    (setq blank-pos (search " " version-rest))
    (setq version-rest (subseq version-rest
			       (1+ blank-pos)))
    (setq blank-pos (search " " version-rest))
    (setq os-version (subseq version-rest 0 blank-pos))
    (setq version-rest (subseq version-rest
			       (1+ blank-pos)))
    (setq blank-pos (search " " version-rest))
    (setq version-rest (subseq version-rest
			       (1+ blank-pos)))
    (concatenate 'string
      os " " os-version))      ; " " version-rest
  #+(and :sgi :cmu :sbcl)
  (concatenate 'string
    (software-type)
    (software-version))
  #+(and :lispworks :irix)
  (let ((soft-type (software-type)))
    (if (equalp soft-type "IRIX5")
        (progn
          (foreign:call-system
	    (format nil "versions ~A | sed -e ~A > ~A"
                         "eoe1"
                         *find-irix-version-script*
                         "irix-version")
	    "/bin/csh")
          (with-open-file (s "irix-version")
                          (format nil "IRIX ~S"
				  (read s))))
      soft-type))
  #-(or (and :excl :sgi) (and :cmu :sgi) (and :lispworks :irix))
  (software-type))


(defun compiler-version ()
  #+:lispworks (concatenate 'string
		"lispworks" " " (lisp-implementation-version))
  #+excl      (concatenate 'string
		"excl" " " excl::*common-lisp-version-number*)
  #+sbcl      (concatenate 'string
			   "sbcl" " " (lisp-implementation-version))
  #+cmu       (concatenate 'string
		"cmu" " " (lisp-implementation-version))
  #+scl       (concatenate 'string
		"scl" " " (lisp-implementation-version))

  #+kcl       "kcl"
  #+IBCL      "ibcl"
  #+akcl      "akcl"
  #+gcl       "gcl"
  #+ecl       "ecl"
  #+mkcl      "mkcl"
  #+lucid     "lucid"
  #+ACLPC     "aclpc"
  #+CLISP     "clisp"
  #+Xerox     "xerox"
  #+symbolics "symbolics"
  #+mcl       "mcl"
  #+coral     "coral"
  #+gclisp    "gclisp"
  #+(or abcl armedbear)      "abcl"
  #+clasp     "clasp"
  )


(defun afs-component (machine software &optional lisp)
  (format nil "~@[~A~]~@[_~A~]~@[_~A~]"
	    machine
	    (or software "mach")
	    lisp))


(defun afs-binary-directory (root-directory)
  ;; Function for obtaining the directory AFS's @sys feature would have
  ;; chosen when we're not in AFS. This function is useful as the argument
  ;; to :binary-pathname in defsystem. For example,
  ;; :binary-pathname (afs-binary-directory "scanner/")
  (let ((machine (machine-type-translation
		  #-(and :sgi :allegro-version>= (version>= 4 2))
		  (machine-type)
		  #+(and :sgi :allegro-version>= (version>= 4 2))
		  (machine-version)))
	(software (software-type-translation
		   #-(and :sgi (or :cmu :sbcl :scl
				   (and :allegro-version>= (version>= 4 2))))
		   (software-type)
		   #+(and :sgi (or :cmu :sbcl :scl
				   (and :allegro-version>= (version>= 4 2))))
		   (operating-system-version)))
	(lisp (compiler-type-translation (compiler-version))))
    ;; pmax_mach rt_mach sun3_35 sun3_mach vax_mach
    (setq root-directory (namestring root-directory))
    (setq root-directory (ensure-trailing-slash root-directory))
    (format nil "~A~@[~A~]~@[~A/~]"
	    root-directory
	    *bin-subdir*
	    (if *multiple-lisp-support*
		(afs-component machine software lisp)
	      (afs-component machine software)))))



(defun afs-source-directory (root-directory &optional version-flag)
  ;; Function for obtaining the directory AFS's @sys feature would have
  ;; chosen when we're not in AFS. This function is useful as the argument
  ;; to :source-pathname in defsystem.
  (setq root-directory (namestring root-directory))
  (setq root-directory (ensure-trailing-slash root-directory))
  (format nil "~A~@[~A/~]"
          root-directory
          (and version-flag (translate-version *version*))))


(defun null-string (s)
  (when (stringp s)
    (string-equal s "")))


(defun ensure-trailing-slash (dir)
  (if (and dir
	   (not (null-string dir))
	   (not (char= (char dir
			     (1- (length dir)))
		       #\/))
	   (not (char= (char dir
			     (1- (length dir)))
		       #\\))
	   )
      (concatenate 'string dir "/")
      dir))





(defvar *machine-type-alist* (make-hash-table :test #'equal)
  "Hash table for retrieving the machine-type")

(defun machine-type-translation (name &optional operation)
  (if operation
      (setf (gethash (string-upcase name) *machine-type-alist*) operation)
      (gethash (string-upcase name) *machine-type-alist*)))


(machine-type-translation "IBM RT PC"                        "rt")
(machine-type-translation "DEC 3100"                         "pmax")
(machine-type-translation "DEC VAX-11"                       "vax")
(machine-type-translation "DECstation"                       "pmax")
(machine-type-translation "Sun3"                             "sun3")
(machine-type-translation "Sun-4"                            "sun4")
(machine-type-translation "MIPS Risc"                        "mips")
(machine-type-translation "SGI"                              "sgi")
(machine-type-translation "Silicon Graphics Iris 4D"         "sgi")
(machine-type-translation "Silicon Graphics Iris 4D (R3000)" "sgi")
(machine-type-translation "Silicon Graphics Iris 4D (R4000)" "sgi")
(machine-type-translation "Silicon Graphics Iris 4D (R4400)" "sgi")
(machine-type-translation "IP22"                             "sgi")
;;; MIPS R4000 Processor Chip Revision: 3.0
;;; MIPS R4400 Processor Chip Revision: 5.0
;;; MIPS R4600 Processor Chip Revision: 1.0
(machine-type-translation "IP20"                             "sgi")
;;; MIPS R4000 Processor Chip Revision: 3.0
(machine-type-translation "IP17"                             "sgi")
;;; MIPS R4000 Processor Chip Revision: 2.2
(machine-type-translation "IP12"                             "sgi")
;;; MIPS R2000A/R3000 Processor Chip Revision: 3.0
(machine-type-translation "IP7"                              "sgi")
;;; MIPS R2000A/R3000 Processor Chip Revision: 3.0

(machine-type-translation "x86"                              "x86")
;;; ACL
(machine-type-translation "IBM PC Compatible"                "x86")
;;; LW
(machine-type-translation "I686"                             "x86")
;;; LW
(machine-type-translation "PC/386"                           "x86")
;;; CLisp Win32

#+(and :lucid :sun :mc68000)
(machine-type-translation "unknown"     "sun3")


(defvar *software-type-alist* (make-hash-table :test #'equal)
  "Hash table for retrieving the software-type")

(defun software-type-translation (name &optional operation)
  (if operation
      (setf (gethash (string-upcase name) *software-type-alist*) operation)
      (gethash (string-upcase name) *software-type-alist*)))


(software-type-translation "BSD UNIX"      "mach") ; "unix"
(software-type-translation "Ultrix"        "mach") ; "ultrix"
(software-type-translation "SunOS"         "SunOS")
(software-type-translation "MACH/4.3BSD"   "mach")
(software-type-translation "IRIX System V" "irix") ; (software-type)
(software-type-translation "IRIX5"         "irix5")
;;(software-type-translation "IRIX liasg5 5.2 02282016 IP22 mips" "irix5") ; (software-version)

(software-type-translation "IRIX 5.2" "irix5")
(software-type-translation "IRIX 5.3" "irix5")
(software-type-translation "IRIX5.2"  "irix5")
(software-type-translation "IRIX5.3"  "irix5")

(software-type-translation "Linux" "linux") ; Lispworks for Linux
(software-type-translation "Linux 2.x, Redhat 6.x and 7.x" "linux") ; ACL
(software-type-translation "Microsoft Windows 9x/Me and NT/2000/XP" "win32")
(software-type-translation "Windows NT" "win32") ; LW for Windows
(software-type-translation "ANSI C program" "ansi-c") ; CLISP
(software-type-translation "C compiler" "ansi-c") ; CLISP for Win32

(software-type-translation nil             "")

#+:lucid
(software-type-translation "Unix"
			   #+:lcl4.0 "4.0"
			   #+(and :lcl3.0 (not :lcl4.0)) "3.0")


(defvar *compiler-type-alist* (make-hash-table :test #'equal)
  "Hash table for retrieving the Common Lisp type")

(defun compiler-type-translation (name &optional operation)
  (if operation
      (setf (gethash (string-upcase name) *compiler-type-alist*) operation)
    (gethash (string-upcase name) *compiler-type-alist*)))


(compiler-type-translation "lispworks 3.2.1"         "lispworks")
(compiler-type-translation "lispworks 3.2.60 beta 6" "lispworks")
(compiler-type-translation "lispworks 4.2.0"         "lispworks")


#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (or (find :case-sensitive common-lisp:*features*)
	      (find :case-insensitive common-lisp:*features*))
    (if (or (eq excl:*current-case-mode* :case-sensitive-lower)
	    (eq excl:*current-case-mode* :case-sensitive-upper))
	(push :case-sensitive common-lisp:*features*)
      (push :case-insensitive common-lisp:*features*))))


#+(and allegro case-sensitive ics)
(compiler-type-translation "excl 6.1" "excl-m")
#+(and allegro case-sensitive (not ics))
(compiler-type-translation "excl 6.1" "excl-m8")

#+(and allegro case-insensitive ics)
(compiler-type-translation "excl 6.1" "excl-a")
#+(and allegro case-insensitive (not ics))
(compiler-type-translation "excl 6.1" "excl-a8")

(compiler-type-translation "excl 4.2" "excl")
(compiler-type-translation "excl 4.1" "excl")
(compiler-type-translation "cmu 17f" "cmu")
(compiler-type-translation "cmu 17e" "cmu")
(compiler-type-translation "cmu 17d" "cmu")


;;; ********************************
;;; System Names *******************
;;; ********************************

;;; If you use strings for system names, be sure to use the same case
;;; as it appears on disk, if the filesystem is case sensitive.

(defun canonicalize-system-name (name)
  ;; Originally we were storing systems using GET. This meant that the
  ;; name of a system had to be a symbol, so we interned the symbols
  ;; in the keyword package to avoid package dependencies. Now that we're
  ;; storing the systems in a hash table, we've switched to using strings.
  ;; Since the hash table is case sensitive, we use uppercase strings.
  ;; (Names of modules and files may be symbols or strings.)
  #||(if (keywordp name)
      name
      (intern (string-upcase (string name)) "KEYWORD"))||#
  (if (stringp name) (string-upcase name) (string-upcase (string name))))


(defvar *defined-systems* (make-hash-table :test #'equal)
  "Hash table containing the definitions of all known systems.")


(defun get-system (name)
  "Returns the definition of the system named NAME."
  (gethash (canonicalize-system-name name) *defined-systems*))


(defsetf get-system (name) (value)
  `(setf (gethash (canonicalize-system-name ,name) *defined-systems*) ,value))


(defun undefsystem (name)
  "Removes the definition of the system named NAME."
  (remhash (canonicalize-system-name name) *defined-systems*))


(defun defined-systems ()
  "Returns a list of defined systems."
  (let ((result nil))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (push value result))
	     *defined-systems*)
    result))


(defun defined-names-and-systems ()
  "Returns a a-list of defined systems along with their names."
  (loop for sname being the hash-keys of *defined-systems*
        using (hash-value s)
        collect (cons sname s)))


;;; ********************************
;;; Directory Pathname Hacking *****
;;; ********************************

;;; Unix example: An absolute directory starts with / while a
;;; relative directory doesn't. A directory ends with /, while
;;; a file's pathname doesn't. This is important 'cause
;;; (pathname-directory "foo/bar") will return "foo" and not "foo/".

;;; I haven't been able to test the fix to the problem with symbolics
;;; hosts. Essentially, append-directories seems to have been tacking
;;; the default host onto the front of the pathname (e.g., mk::source-pathname
;;; gets a "B:" on front) and this overrides the :host specified in the
;;; component. The value of :host should override that specified in
;;; the :source-pathname and the default file server. If this doesn't
;;; fix things, specifying the host in the root pathname "F:>root-dir>"
;;; may be a good workaround.

;;; Need to verify that merging of pathnames where modules are located
;;; on different devices (in VMS-based VAXLisp) now works.

;;; Merge-pathnames works for VMS systems. In VMS systems, the directory
;;; part is enclosed in square brackets, e.g.,
;;; 	"[root.child.child_child]" or "[root.][child.][child_child]"
;;; To concatenate directories merge-pathnames works as follows:
;;; 	(merge-pathnames "" "[root]")               ==> "[root]"
;;; 	(merge-pathnames "[root.]" "[son]file.ext") ==> "[root.son]file.ext"
;;; 	(merge-pathnames "[root.]file.ext" "[son]") ==> "[root.son]file.ext"
;;; 	(merge-pathnames "[root]file.ext" "[son]")  ==> "[root]file.ext"
;;; Thus the problem with the #-VMS code was that it was merging x y into
;;; [[x]][y] instead of [x][y] or [x]y.

;;; Miscellaneous notes:
;;;   On GCLisp, the following are equivalent:
;;;       "\\root\\subdir\\BAZ"
;;;       "/root/subdir/BAZ"
;;;   On VAXLisp, the following are equivalent:
;;;       "[root.subdir]BAZ"
;;;       "[root.][subdir]BAZ"
;;; Use #+:vaxlisp for VAXLisp 3.0, #+(and vms dec common vax) for v2.2

(defun new-append-directories (absolute-dir relative-dir)
  ;; Version of append-directories for CLtL2-compliant lisps. In particular,
  ;; they must conform to section 23.1.3 "Structured Directories". We are
  ;; willing to fix minor aberations in this function, but not major ones.
  ;; Tested in Allegro CL 4.0 (SPARC), Allegro CL 3.1.12 (DEC 3100),
  ;; CMU CL old and new compilers, Lucid 3.0, Lucid 4.0.
  (setf absolute-dir (or absolute-dir "")
	relative-dir (or relative-dir ""))
  (let* ((abs-dir (pathname absolute-dir))
	 (rel-dir (pathname relative-dir))
	 (host (pathname-host abs-dir))
	 (device (if (null-string absolute-dir) ; fix for CMU CL old compiler
		     (pathname-device rel-dir)
		   (pathname-device abs-dir)))
	 (abs-directory (directory-to-list (pathname-directory abs-dir)))
	 (abs-keyword (when (keywordp (car abs-directory))
			(pop abs-directory)))
	 ;; Stig (July 2001):
	 ;; Somehow CLISP dies on the next line, but NIL is ok.
	 (abs-name (ignore-errors (file-namestring abs-dir))) ; was pathname-name
	 (rel-directory (directory-to-list (pathname-directory rel-dir)))
	 (rel-keyword (when (keywordp (car rel-directory))
			(pop rel-directory)))
	 ;; rtoy: Why should any Lisp want rel-file?  Shouldn't using
	 ;; rel-name and rel-type work for every Lisp?
	 ;; ;madhu 200229 add clozure ;madhu 221124 ecl (lpn), lispworks
         #-(or :MCL :sbcl :clisp :cmu :ecl :clozure-common-lisp) (rel-file (file-namestring rel-dir))
	 ;; Stig (July 2001);
	 ;; These values seems to help clisp as well
	 #+(or :MCL :sbcl :clisp :cmu :ecl :clozure-common-lisp :lispworks) (rel-name (pathname-name rel-dir))
	 #+(or :MCL :sbcl :clisp :cmu :ecl :clozure-common-lisp :lispworks) (rel-type (pathname-type rel-dir))
	 (directory nil))

    ;; TI Common Lisp pathnames can return garbage for file names because
    ;; of bizarreness in the merging of defaults.  The following code makes
    ;; sure that the name is a valid name by comparing it with the
    ;; pathname-name.  It also strips TI specific extensions and handles
    ;; the necessary case conversion.  TI maps upper back into lower case
    ;; for unix files!
    #+TI (if (search (pathname-name abs-dir) abs-name :test #'string-equal)
	     (setf abs-name (string-right-trim "." (string-upcase abs-name)))
	     (setf abs-name nil))
    #+TI (if (search (pathname-name rel-dir) rel-file :test #'string-equal)
	     (setf rel-file (string-right-trim "." (string-upcase rel-file)))
	     (setf rel-file nil))
    ;; Allegro v4.0/4.1 parses "/foo" into :directory '(:absolute :root)
    ;; and filename "foo". The namestring of a pathname with
    ;; directory '(:absolute :root "foo") ignores everything after the
    ;; :root.
    #+(and allegro-version>= (version>= 4 0))
    (when (eq (car abs-directory) :root) (pop abs-directory))
    #+(and allegro-version>= (version>= 4 0))
    (when (eq (car rel-directory) :root) (pop rel-directory))

    (when (and abs-name (not (null-string abs-name))) ; was abs-name
      (cond ((and (null abs-directory) (null abs-keyword))
	     #-(or :lucid :kcl :akcl TI) (setf abs-keyword :relative)
	     (setf abs-directory (list abs-name)))
	    (t
	     (setf abs-directory (append abs-directory (list abs-name))))))
    (when (and (null abs-directory)
	       (or (null abs-keyword)
		   ;; In Lucid, an abs-dir of nil gets a keyword of
		   ;; :relative since (pathname-directory (pathname ""))
		   ;; returns (:relative) instead of nil.
		   #+:lucid (eq abs-keyword :relative))
	       rel-keyword)
      ;; The following feature switches seem necessary in CMUCL
      ;; Marco Antoniotti 19990707
      #+(or :sbcl :CMU)
      (if (typep abs-dir 'logical-pathname)
	  (setf abs-keyword :absolute)
	  (setf abs-keyword rel-keyword))
      #-(or :sbcl :CMU)
      (setf abs-keyword rel-keyword))
    (setf directory (append abs-directory rel-directory))
    (when abs-keyword (setf directory (cons abs-keyword directory)))
    (namestring
     (make-pathname :host host
		    :device device
                    :directory
                    directory
		    :name
		    #-(or :sbcl :MCL :clisp :cmu :ecl :clozure-common-lisp :lispworks) rel-file
		    #+(or :sbcl :MCL :clisp :cmu :ecl :clozure-common-lisp :lispworks) rel-name

		    #+(or :sbcl :MCL :clisp :cmu :ecl :clozure-common-lisp :lispworks) :type
		    #+(or :sbcl :MCL :clisp :cmu :ecl :clozure-common-lisp :lispworks) rel-type
		    ))))


(defun directory-to-list (directory)
  ;; The directory should be a list, but nonstandard implementations have
  ;; been known to use a vector or even a string.
  (cond ((listp directory)
	 directory)
	((stringp directory)
	 (cond ((find #\; directory)
		;; It's probably a logical pathname, so split at the
		;; semicolons:
		(split-string directory :item #\;))
               #+MCL
	       ((and (find #\: directory)
		     (not (find #\/ directory)))
		;; It's probably a MCL pathname, so split at the colons.
		(split-string directory :item #\:))
	       (t
		;; It's probably a unix pathname, so split at the slash.
		(split-string directory :item #\/))))
	(t
	 (coerce directory 'list))))


(defparameter *append-dirs-tests*
  '("~/foo/" "baz/bar.lisp"
     "~/foo" "baz/bar.lisp"
     "/foo/bar/" "baz/barf.lisp"
     "/foo/bar/" "/baz/barf.lisp"
     "foo/bar/" "baz/barf.lisp"
     "foo/bar" "baz/barf.lisp"
     "foo/bar" "/baz/barf.lisp"
     "foo/bar/" "/baz/barf.lisp"
     "/foo/bar/" nil
     "foo/bar/" nil
     "foo/bar" nil
     "foo" nil
     "foo" ""
     nil "baz/barf.lisp"
     nil "/baz/barf.lisp"
     nil nil))


(defun test-new-append-directories (&optional (test-dirs *append-dirs-tests*))
  (do* ((dir-list test-dirs (cddr dir-list))
	(abs-dir (car dir-list) (car dir-list))
	(rel-dir (cadr dir-list) (cadr dir-list)))
      ((null dir-list) (values))
    (format t "~&ABS: ~S ~18TREL: ~S ~41TResult: ~S"
	    abs-dir rel-dir (new-append-directories abs-dir rel-dir))))


#||
<cl> (test-new-append-directories)

ABS: "~/foo/"     REL: "baz/bar.lisp"    Result: "/usr0/mkant/foo/baz/bar.lisp"
ABS: "~/foo"      REL: "baz/bar.lisp"    Result: "/usr0/mkant/foo/baz/bar.lisp"
ABS: "/foo/bar/"  REL: "baz/barf.lisp"   Result: "/foo/bar/baz/barf.lisp"
ABS: "/foo/bar/"  REL: "/baz/barf.lisp"  Result: "/foo/bar/baz/barf.lisp"
ABS: "foo/bar/"   REL: "baz/barf.lisp"   Result: "foo/bar/baz/barf.lisp"
ABS: "foo/bar"    REL: "baz/barf.lisp"   Result: "foo/bar/baz/barf.lisp"
ABS: "foo/bar"    REL: "/baz/barf.lisp"  Result: "foo/bar/baz/barf.lisp"
ABS: "foo/bar/"   REL: "/baz/barf.lisp"  Result: "foo/bar/baz/barf.lisp"
ABS: "/foo/bar/"  REL: NIL               Result: "/foo/bar/"
ABS: "foo/bar/"   REL: NIL               Result: "foo/bar/"
ABS: "foo/bar"    REL: NIL               Result: "foo/bar/"
ABS: "foo"        REL: NIL               Result: "foo/"
ABS: "foo"        REL: ""                Result: "foo/"
ABS: NIL          REL: "baz/barf.lisp"   Result: "baz/barf.lisp"
ABS: NIL          REL: "/baz/barf.lisp"  Result: "/baz/barf.lisp"
ABS: NIL          REL: NIL               Result: ""

||#


(defun append-directories (absolute-directory relative-directory)
  "There is no CL primitive for tacking a subdirectory onto a directory.
   We need such a function because defsystem has both absolute and
   relative pathnames in the modules. This is a somewhat ugly hack which
   seems to work most of the time. We assume that ABSOLUTE-DIRECTORY
   is a directory, with no filename stuck on the end. Relative-directory,
   however, may have a filename stuck on the end."
  (when (or absolute-directory relative-directory)
    (cond
     ;; KMR commented out because: when appending two logical pathnames,
     ;; using this code translates the first logical pathname then appends
     ;; the second logical pathname -- an error.
     #|
      ;; We need a reliable way to determine if a pathname is logical.
      ;; Allegro 4.1 does not recognize the syntax of a logical pathname
      ;;  as being logical unless its logical host is already defined.

      #+(or (and allegro-version>= (version>= 4 1))
	    :logical-pathnames-mk)
      ((and absolute-directory
	    (logical-pathname-p absolute-directory)
	    relative-directory)
       ;; For use with logical pathnames package.
       (append-logical-directories-mk absolute-directory relative-directory))
     |#
;madhu 221124
;;      #+NIL				; madhu 060906
      ((namestring-probably-logical absolute-directory)
       ;; A simplistic stab at handling logical pathnames
       (append-logical-pnames absolute-directory relative-directory))
      (t
       ;; In VMS, merge-pathnames actually does what we want!!!
       #+:VMS
       (namestring (merge-pathnames (or absolute-directory "")
				    (or relative-directory "")))
       #+:macl1.3.2
       (namestring (make-pathname :directory absolute-directory
				  :name relative-directory))
       ;; Cross your fingers and pray.
       #-(or :VMS :macl1.3.2)
       (new-append-directories absolute-directory relative-directory)))))


#+:logical-pathnames-mk
(defun append-logical-directories-mk (absolute-dir relative-dir)
  (lp:append-logical-directories absolute-dir relative-dir))


;;; append-logical-pathnames-mk --
;;; The following is probably still bogus and it does not solve the
;;; problem of appending two logical pathnames.
;;; Anyway, as per suggetsion by KMR, the function is not called
;;; anymore.
;;; Hopefully this will not cause problems for ACL.

#+(and (and allegro-version>= (version>= 4 1))
       (not :logical-pathnames-mk))
(defun append-logical-directories-mk (absolute-dir relative-dir)
  ;; We know absolute-dir and relative-dir are non nil.  Moreover
  ;; absolute-dir is a logical pathname.
  (setq absolute-dir (logical-pathname absolute-dir))
  (etypecase relative-dir
    (string (setq relative-dir (parse-namestring relative-dir)))
    (pathname #| do nothing |#))

  (translate-logical-pathname
   (merge-pathnames relative-dir absolute-dir)))


#| Old version 2002-03-02
#+(and (and allegro-version>= (version>= 4 1))
       (not :logical-pathnames-mk))
(defun append-logical-directories-mk (absolute-dir relative-dir)
  ;; We know absolute-dir and relative-dir are non nil.  Moreover
  ;; absolute-dir is a logical pathname.
  (setq absolute-dir (logical-pathname absolute-dir))
  (etypecase relative-dir
    (string (setq relative-dir (parse-namestring relative-dir)))
    (pathname #| do nothing |#))

  (translate-logical-pathname
   (make-pathname
    :host (or (pathname-host absolute-dir)
	      (pathname-host relative-dir))
    :directory (append (pathname-directory absolute-dir)
		       (cdr (pathname-directory relative-dir)))
    :name (or (pathname-name absolute-dir)
	      (pathname-name relative-dir))
    :type (or (pathname-type absolute-dir)
	      (pathname-type relative-dir))
    :version (or (pathname-version absolute-dir)
		 (pathname-version relative-dir)))))

;; Old version
#+(and (and allegro-version>= (version>= 4 1))
       (not :logical-pathnames-mk))
(defun append-logical-directories-mk (absolute-dir relative-dir)
  (when (or absolute-dir relative-dir)
    (setq absolute-dir (logical-pathname (or absolute-dir ""))
	  relative-dir (logical-pathname (or relative-dir "")))
    (translate-logical-pathname
     (make-pathname
      :host (or (pathname-host absolute-dir)
		(pathname-host relative-dir))
      :directory (append (pathname-directory absolute-dir)
			 (cdr (pathname-directory relative-dir)))
      :name (or (pathname-name absolute-dir)
		(pathname-name relative-dir))
      :type (or (pathname-type absolute-dir)
		(pathname-type relative-dir))
      :version (or (pathname-version absolute-dir)
		   (pathname-version relative-dir))))))
|#

;;; determines if string or pathname object is logical
#+:logical-pathnames-mk
(defun logical-pathname-p (thing)
  (eq (lp:pathname-host-type thing) :logical))

;;; From Kevin Layer for 4.1final.
#+(and (and allegro-version>= (version>= 4 1))
       (not :logical-pathnames-mk))
(defun logical-pathname-p (thing)
  (typep (parse-namestring thing) 'logical-pathname))

(defun pathname-logical-p (thing)
  (typecase thing
    (logical-pathname t)
    #+clisp ; CLisp has non conformant Logical Pathnames.
    (pathname (pathname-logical-p (namestring thing)))
    (string (and (= 1 (count #\: thing)) ; Shortcut.
		 (ignore-errors (translate-logical-pathname thing))
		 t))
    (t nil)))

;;; This affects only one thing.
;;; 19990707 Marco Antoniotti
;;; old version

(defun namestring-probably-logical (namestring)
  (and (stringp namestring)
       ;; unix pathnames don't have embedded semicolons
       (find #\; namestring)))
#||
;;; New version
(defun namestring-probably-logical (namestring)
  (and (stringp namestring)
       (typep (parse-namestring namestring) 'logical-pathname)))


;;; New new version
;;; 20000321 Marco Antoniotti
(defun namestring-probably-logical (namestring)
  (pathname-logical-p namestring))
||#


#|| This is incorrect, as it strives to keep strings around, when it
    shouldn't.  MERGE-PATHNAMES already DTRT.
(defun append-logical-pnames (absolute relative)
  (declare (type (or null string pathname) absolute relative))
  (let ((abs (if absolute
		 #-clisp (namestring absolute)
		 #+clisp absolute ;; Stig (July 2001): hack to avoid CLISP from translating the whole string
		 ""))
	(rel (if relative (namestring relative) ""))
	)
    ;; Make sure the absolute directory ends with a semicolon unless
    ;; the pieces are null strings
    (unless (or (null-string abs) (null-string rel)
		(char= (char abs (1- (length abs)))
		       #\;))
      (setq abs (concatenate 'string abs ";")))
    ;; Return the concatenate pathnames
    (concatenate 'string abs rel)))
||#


(defun append-logical-pnames (absolute relative)
  (declare (type (or null string pathname) absolute relative))
  (let ((abs (if absolute
                 (pathname absolute)
                 (make-pathname :directory (list :absolute)
                                :name nil
                                :type nil)
                 ))
	(rel (if relative
                 (pathname relative)
                 (make-pathname :directory (list :relative)
                                :name nil
                                :type nil)
                 ))
	)
    ;; The following is messed up because CMUCL and LW use different
    ;; defaults for host (in particular LW uses NIL).  Thus
    ;; MERGE-PATHNAMES has legitimate different behaviors on both
    ;; implementations. Of course this is disgusting, but that is the
    ;; way it is and the rest tries to circumvent this crap.
    (etypecase abs
      (logical-pathname
       (etypecase rel
	 (logical-pathname
	  (namestring (merge-pathnames rel abs)))
	 (pathname
	  ;; The following potentially translates the logical pathname
	  ;; very early, but we cannot avoid it.
	  (namestring (merge-pathnames rel (translate-logical-pathname abs) nil))) ;madhu: add nil Tue Jun 28 12:02:55 2005
	 ))
      (pathname
       (namestring (merge-pathnames rel abs)))
      )))

#||
;;; This was a try at appending a subdirectory onto a directory.
;;; It failed. We're keeping this around to prevent future mistakes
;;; of a similar sort.
(defun merge-directories (absolute-directory relative-directory)
  ;; replace concatenate with something more intelligent
  ;; i.e., concatenation won't work with some directories.
  ;; it should also behave well if the parent directory
  ;; has a filename at the end, or if the relative-directory ain't relative
  (when absolute-directory
    (setq absolute-directory (pathname-directory absolute-directory)))
  (concatenate 'string
	       (or absolute-directory "")
	       (or relative-directory "")))
||#

#||
<cl> (defun d (d n) (namestring (make-pathname :directory d :name n)))

D
<cl> (d "~/foo/" "baz/bar.lisp")
"/usr0/mkant/foo/baz/bar.lisp"

<cl> (d "~/foo" "baz/bar.lisp")
"/usr0/mkant/foo/baz/bar.lisp"

<cl> (d "/foo/bar/" "baz/barf.lisp")
"/foo/bar/baz/barf.lisp"

<cl> (d "foo/bar/" "baz/barf.lisp")
"foo/bar/baz/barf.lisp"

<cl> (d "foo/bar" "baz/barf.lisp")
"foo/bar/baz/barf.lisp"

<cl> (d "foo/bar" "/baz/barf.lisp")
"foo/bar//baz/barf.lisp"

<cl> (d "foo/bar" nil)
"foo/bar/"

<cl> (d nil "baz/barf.lisp")
"baz/barf.lisp"

<cl> (d nil nil)
""

||#

;;; The following is a change proposed by DTC for SCL.
;;; Maybe it could be used all the time.

#-scl
(defun new-file-type (pathname type)
  ;; why not (make-pathname :type type :defaults pathname)?
  (make-pathname
   :host (pathname-host pathname)
   :device (pathname-device pathname)
   :directory (pathname-directory pathname)
   :name (pathname-name pathname)
   :type type
   :version (pathname-version pathname)))


#+scl
(defun new-file-type (pathname type)
  ;; why not (make-pathname :type type :defaults pathname)?
  (make-pathname
   :host (pathname-host pathname :case :common)
   :device (pathname-device pathname :case :common)
   :directory (pathname-directory pathname :case :common)
   :name (pathname-name pathname :case :common)
   :type (string-upcase type)
   :version (pathname-version pathname :case :common)))



;;; ********************************
;;; Component Defstruct ************
;;; ********************************

(defvar *source-pathname-default* nil
  "Default value of :source-pathname keyword in DEFSYSTEM. Set this to
   \"\" to avoid having to type :source-pathname \"\" all the time.")

(defvar *binary-pathname-default* nil
  "Default value of :binary-pathname keyword in DEFSYSTEM.")


(defstruct (topological-sort-node (:conc-name topsort-))
  (color :white :type (member :gray :black :white))
  )


(defparameter *component-evaluated-slots*
  '(:source-root-dir :source-pathname :source-extension
    :binary-root-dir :binary-pathname :binary-extension))


(defparameter *component-form-slots*
  '(:initially-do :finally-do :compile-form :load-form))


(defstruct (component (:include topological-sort-node)
                      (:print-function print-component))
  (type :file     ; to pacify the CMUCL compiler (:type is alway supplied)
	:type (member :defsystem
		      :system
		      :subsystem
		      :module
		      :file
		      :private-file
		      ))
  (name nil :type (or symbol string))
  (indent 0 :type (mod 1024))		; Number of characters of indent in
					; verbose output to the user.
  host					; The pathname host (i.e., "/../a").
  device				; The pathname device.
  source-root-dir			; Relative or absolute (starts
					; with "/"), directory or file
					; (ends with "/").
  (source-pathname *source-pathname-default*)
  source-extension			; A string, e.g., "lisp"
					; if NIL, inherit
  (binary-pathname *binary-pathname-default*)
  binary-root-dir
  binary-extension			; A string, e.g., "fasl". If
					; NIL, uses default for
					; machine-type.
  package				; Package for use-package.

  ;; The following three slots are used to provide for alternate compilation
  ;; and loading functions for the files contained within a component. If
  ;; a component has a compiler or a loader specified, those functions are
  ;; used. Otherwise the functions are derived from the language. If no
  ;; language is specified, it defaults to Common Lisp (:lisp). Other current
  ;; possible languages include :scheme (PseudoScheme) and :c, but the user
  ;; can define additional language mappings. Compilation functions should
  ;; accept a pathname argument and a :output-file keyword; loading functions
  ;; just a pathname argument. The default functions are #'compile-file and
  ;; #'load. Unlike fdmm's SET-LANGUAGE macro, this allows a defsystem to
  ;; mix languages.
  (language nil :type (or null symbol))
  (compiler nil :type (or null symbol function))
  (loader   nil :type (or null symbol function))
  (compiler-options nil :type list)	; A list of compiler options to
                                        ; use for compiling this
                                        ; component.  These must be
                                        ; keyword options supported by
                                        ; the compiler.

  (components () :type list)		; A list of components
					; comprising this component's
					; definition.
  (depends-on () :type list)		; A list of the components
					; this one depends on. may
					; refer only to the components
					; at the same level as this
					; one.
  proclamations				; Compiler options, such as
					; '(optimize (safety 3)).
  (initially-do (lambda () nil))        ; Form to evaluate before the
					; operation.
  (finally-do (lambda () nil))		; Form to evaluate after the operation.
  (compile-form (lambda () nil))        ; For foreign libraries.
  (load-form (lambda () nil))           ; For foreign libraries.

  ;; load-time				; The file-write-date of the
					; binary/source file loaded.

  ;; If load-only is T, will not compile the file on operation :compile.
  ;; In other words, for files which are :load-only T, loading the file
  ;; satisfies any demand to recompile.
  load-only				; If T, will not compile this
					; file on operation :compile.
  ;; If compile-only is T, will not load the file on operation :compile.
  ;; Either compiles or loads the file, but not both. In other words,
  ;; compiling the file satisfies the demand to load it. This is useful
  ;; for PCL defmethod and defclass definitions, which wrap a
  ;; (eval-when (compile load eval) ...) around the body of the definition.
  ;; This saves time in some lisps.
  compile-only				; If T, will not load this
					; file on operation :compile.
  #|| ISI Extension ||#
  load-always				; If T, will force loading
					; even if file has not
					; changed.
  ;; PVE: add banner
  (banner nil :type (or null string))

  (documentation nil :type (or null string)) ; Optional documentation slot
  (long-documentation nil :type (or null string)) ; Optional long documentation slot

  ;; Added AUTHOR, MAINTAINER, VERSION and LICENCE slots.
  (author nil :type (or null string))
  (licence nil :type (or null string))
  (maintainer nil :type (or null string))
  (version nil :type (or null string))
  (description nil :type (or null string))
  (license nil :type (or null string))

  (homepage nil :type (or null string))
  (bug-tracker nil :type (or null string))
  (mailto nil :type (or null string))
  (long-name nil :type (or null string))
  (source-control nil :type list)
  (entry-point nil :type t)

  ;; Added NON-REQUIRED-P slot.  Useful for optional items.
  (non-required-p nil :type boolean)	; If T a missing file or
					; sub-directory will not cause
					; an error.
  (if-feature nil :type (or null keyword cons))
  )


;;; To allow dependencies from "foreign systems" like ASDF or one of
;;; the proprietary ones like ACL or LW.

(defstruct (foreign-system (:include component (type :system)))
  kind ; This is a keyword: (member :asdf :pcl :lispworks-common-defsystem ...)
  object ; The actual foreign system object.
  )


(defun register-foreign-system (name &key representation kind)
  (declare (type (or symbol string) name))
  (let ((fs (make-foreign-system :name name
                                 :kind kind
                                 :object representation)))
    (setf (get-system name) fs)))



(define-condition missing-component (simple-condition)
  ((name :reader missing-component-name
         :initarg :name)
   (component :reader missing-component-component
              :initarg :component)
   )
  #-gcl (:default-initargs :component nil)
  (:report (lambda (mmc stream)
	     (format stream "MK:DEFSYSTEM: missing component ~S for ~S."
                     (missing-component-name mmc)
                     (missing-component-component mmc))))
  )

(define-condition missing-module (missing-component)
  ()
  (:report (lambda (mmc stream)
	     (format stream "MK:DEFSYSTEM: missing module ~S for ~S."
                     (missing-component-name mmc)
                     (missing-component-component mmc))))
  )

(define-condition missing-system (missing-module)
  ()
  (:report (lambda (msc stream)
	     (format stream "MK:DEFSYSTEM: missing system ~S~@[ for S~]."
                     (missing-component-name msc)
                     (missing-component-component msc))))
  )



(defvar *file-load-time-table* (make-hash-table :test #'equal)
  "Hash table of file-write-dates for the system definitions and files in the system definitions.")


(defun component-load-time (component)
  (when component
    (etypecase component
      (string    (gethash component *file-load-time-table*))
      (pathname (gethash (namestring component) *file-load-time-table*))
      (component
       (ecase (component-type component)
	 (:defsystem
	  (let* ((name (component-name component))
		 (path (when name (compute-system-path name nil))))
	    (declare (type (or string pathname null) path))
	    (when path
	      (gethash (namestring path) *file-load-time-table*))))
	 ((:file :private-file)
	  ;; Use only :source pathname to identify component's
	  ;; load time.
	  (let ((path (component-full-pathname component :source)))
	    (when path
	      (gethash path *file-load-time-table*)))))))))

#-(or :cmu)
(defsetf component-load-time (component) (value)
  `(when ,component
    (etypecase ,component
      (string   (setf (gethash ,component *file-load-time-table*) ,value))
      (pathname (setf (gethash (namestring (the pathname ,component))
			       *file-load-time-table*)
		      ,value))
      (component
       (ecase (component-type ,component)
	 (:defsystem
	  (let* ((name (component-name ,component))
		 (path (when name (compute-system-path name nil))))
	    (declare (type (or string pathname null) path))
	    (when path
	      (setf (gethash (namestring path) *file-load-time-table*)
		    ,value))))
	 ((:file :private-file)
	  ;; Use only :source pathname to identify file.
	  (let ((path (component-full-pathname ,component :source)))
	    (when path
	      (setf (gethash path *file-load-time-table*)
		    ,value)))))))
    ,value))

#+(or :cmu)
(defun (setf component-load-time) (value component)
  (declare
   (type (or null string pathname component) component)
   (type (or unsigned-byte null) value))
  (when component
    (etypecase component
      (string   (setf (gethash component *file-load-time-table*) value))
      (pathname (setf (gethash (namestring (the pathname component))
			       *file-load-time-table*)
		      value))
      (component
       (ecase (component-type component)
	 (:defsystem
	     (let* ((name (component-name component))
		    (path (when name (compute-system-path name nil))))
	       (declare (type (or string pathname null) path))
	       (when path
		 (setf (gethash (namestring path) *file-load-time-table*)
		       value))))
	 ((:file :private-file)
	  ;; Use only :source pathname to identify file.
	  (let ((path (component-full-pathname component :source)))
	    (when path
	      (setf (gethash path *file-load-time-table*)
		    value)))))))
    value))


;;; compute-system-path --

;; ;madhu 200404 - rewrite compute-system-path to use
;; *defsystem-to-defsystem-file-map* to store the path where the
;; defsystem was loaded from

(defvar *defsystem-to-defsystem-file-map* (make-hash-table :test #'equal))

(defun compute-system-path-1 (module-name definition-pname)
  ;;madhu 170723 - handle retarded module-names "cffi/c2fi" ccl: blows
  ;;up on probe-file. clisp blows up on make-pathname :name
  (let* ((module-string-name-0
          (etypecase module-name
            (symbol (string-downcase
                     (string module-name)))
            (string (string-downcase module-name))))

	 (module-string-name (let ((p (position #\/ module-string-name-0)))
			       (if p
				   "bogus" #+nil
				   (subseq module-string-name-0 0 p)
				   module-string-name-0)))
         (file-pathname
	  (make-pathname :name module-string-name
			 :type *system-extension*))

         (lib-file-pathname
	  (make-pathname :directory (list :relative module-string-name)
                         :name module-string-name
			 :type *system-extension*))
         )
    (or (when definition-pname		; given pathname for system def
	  (probe-file definition-pname))
	;; Then the central registry. Note that we also check the current
	;; directory in the registry, but the above check is hard-coded.
	(cond (*central-registry*
	       (if (listp *central-registry*)
		   (dolist (registry *central-registry*)
		     (let* ((reg-path (registry-pathname registry))
                            (file (or (probe-file
                                       (append-directories
                                        reg-path file-pathname))
                                      (probe-file
                                       (append-directories
                                        reg-path lib-file-pathname)))))
		       (when file (return file))))
		   (or (probe-file (append-directories *central-registry*
						       file-pathname))
                       (probe-file (append-directories *central-registry*
						       lib-file-pathname))
                       ))
               )
	      (t
	       ;; No central registry. Assume current working directory.
	       ;; Maybe this should be an error?
	       (or (probe-file file-pathname)
                   (probe-file lib-file-pathname)))))
    ))


(defun compute-system-path (module-name &optional definition-pname)
  (if (typep module-name 'component)
      (setq module-name (component-name module-name)))
  (let ((ret1 (compute-system-path-1 module-name definition-pname))
	(ret2 (gethash (canonicalize-system-name module-name)
		       *defsystem-to-defsystem-file-map*)))
    (cond (ret1
	   (cond (ret2
		   (cond ((equalp ret1 (probe-file ret2)) ret1)
			 (t (warn "MK:COMPUTE-SYSTEM-PATH: system was last loaded from ~A (not ~A): using previously loaded path" ret2 ret1)
			    ret2)))
		 (t ret1)))
	  (t (cond (ret2 ret2)
		   (t nil))))))


(defun system-definition-pathname (system-name)
  (let ((system (ignore-errors (find-system system-name :error))))
    (if system
        (let ((system-def-pathname
               (make-pathname
		:type "system"
		:defaults (pathname (component-full-pathname system :source))))
              )
          (values system-def-pathname
                  (probe-file system-def-pathname)))
        (values nil nil))))




#|

 (defun compute-system-path (module-name definition-pname)
  (let* ((filename (format nil "~A.~A"
			   (if (symbolp module-name)
			       (string-downcase (string module-name))
			     module-name)
			   *system-extension*)))
    (or (when definition-pname		; given pathname for system def
	  (probe-file definition-pname))
	;; Then the central registry. Note that we also check the current
	;; directory in the registry, but the above check is hard-coded.
	(cond (*central-registry*
	       (if (listp *central-registry*)
		   (dolist (registry *central-registry*)
		     (let ((file (probe-file
				  (append-directories
                                   (registry-pathname registry) filename))))
		       (when file (return file))))
		 (probe-file (append-directories *central-registry*
						 filename))))
	      (t
	       ;; No central registry. Assume current working directory.
	       ;; Maybe this should be an error?
	       (probe-file filename))))))
|#


(defvar *reload-systems-from-disk* t
  "If T, always tries to reload newer system definitions from disk.
   Otherwise first tries to find the system definition in the current
   environment.")

(defun find-system (system-name &optional (mode :ask) definition-pname)
  "Returns the system named SYSTEM-NAME.
If not already loaded, loads it, depending on the value of
*RELOAD-SYSTEMS-FROM-DISK* and of the value of MODE. MODE can be :ASK,
:ERROR, :LOAD-OR-NIL, or :LOAD. :ASK is the default.
This allows OPERATE-ON-SYSTEM to work on non-loaded as well as
loaded system definitions. DEFINITION-PNAME is the pathname for
the system definition, if provided."
  (ecase mode
    (:ask
     (or (get-system system-name)
	 (when (y-or-n-p-wait
		#\y 20
		"System ~A not loaded. Shall I try loading it? "
		system-name)
	   (find-system system-name :load definition-pname))))
    (:error
     (or (get-system system-name)
	 (error 'missing-system :name system-name)))
    (:load-or-nil
     (let ((system (get-system system-name)))
       ;; (break "System ~S ~S." system-name system)
       (or (unless *reload-systems-from-disk* system)
	   ;; If SYSTEM-NAME is a symbol, it will lowercase the
	   ;; symbol's string.
	   ;; If SYSTEM-NAME is a string, it doesn't change the case of the
	   ;; string. So if case matters in the filename, use strings, not
	   ;; symbols, wherever the system is named.
           (when (foreign-system-p system)
             (warn "Foreign system ~S cannot be reloaded by MK:DEFSYSTEM."
		   system)
             (return-from find-system nil))
	   (let ((path (compute-system-path system-name definition-pname)))
	     (when (and path
			(or (null system)
			    (null (component-load-time path))
			    (< (component-load-time path)
			       (file-write-date path))))
	       (tell-user-generic
		(format nil "Loading system ~A from file ~A"
			system-name
			path))
	       (load path)
	       (setf system (get-system system-name))
	       (when system
		 (setf (component-load-time path)
		       (file-write-date path))))
	     system)
	   system)))
    (:load
     (or (unless *reload-systems-from-disk* (get-system system-name))
         (when (foreign-system-p (get-system system-name))
           (warn "Foreign system ~S cannot be reloaded by MK:DEFSYSTEM."
		 (get-system system-name))
           (return-from find-system nil))
	 (or (find-system system-name :load-or-nil definition-pname)
	     (error "Can't find system named ~s." system-name))))))


(defun print-component (component stream depth)
  (declare (ignore depth))
  (format stream "#<~:@(~A~): ~A>"
          (component-type component)
          (component-name component)))


(defun describe-system (name &optional (stream *standard-output*))
  "Prints a description of the system to the stream. If NAME is the
   name of a system, gets it and prints a description of the system.
   If NAME is a component, prints a description of the component."
  (let ((system (if (typep name 'component) name (find-system name :load))))
    (format stream "~&~A ~A: ~
                    ~@[~&   Host: ~A~]~
                    ~@[~&   Device: ~A~]~
                    ~@[~&   Package: ~A~]~
                    ~&   Source: ~@[~A~] ~@[~A~] ~@[~A~]~
                    ~&   Binary: ~@[~A~] ~@[~A~] ~@[~A~]~
                    ~@[~&   Depends On: ~A ~]~&   Components:~{~15T~A~&~}"
	    (component-type system)
	    (component-name system)
	    (component-host system)
	    (component-device system)
	    (component-package system)
	    (component-root-dir system :source)
	    (component-pathname system :source)
	    (component-extension system :source)
	    (component-root-dir system :binary)
	    (component-pathname system :binary)
	    (component-extension system :binary)
	    (component-depends-on system)
	    (component-components system))
    #||(when recursive
      (dolist (component (component-components system))
	(describe-system component stream recursive)))||#
    system))


(defun canonicalize-component-name (component)
  ;; Within the component, the name is a string.
  (if (typep (component-name component) 'string)
      ;; Unnecessary to change it, so just return it, same case
      (component-name component)
    ;; Otherwise, make it a downcase string -- important since file
    ;; names are often constructed from component names, and unix
    ;; prefers lowercase as a default.
    (setf (component-name component)
	  (string-downcase (string (component-name component))))))


(defun component-pathname (component type)
  (when component
    (ecase type
      (:source (component-source-pathname component))
      (:binary (component-binary-pathname component))
      (:error  (component-error-pathname component)))))


(defun component-error-pathname (component)
  (let ((binary (component-pathname component :binary)))
    (new-file-type binary *compile-error-file-type*)))

(defsetf component-pathname (component type) (value)
  `(when ,component
     (ecase ,type
       (:source (setf (component-source-pathname ,component) ,value))
       (:binary (setf (component-binary-pathname ,component) ,value)))))


(defun component-root-dir (component type)
  (when component
    (ecase type
      (:source (component-source-root-dir component))
      ((:binary :error) (component-binary-root-dir component))
      )))

(defsetf component-root-dir (component type) (value)
  `(when ,component
     (ecase ,type
       (:source (setf (component-source-root-dir ,component) ,value))
       (:binary (setf (component-binary-root-dir ,component) ,value)))))


(defvar *source-pathnames-table* (make-hash-table :test #'equal)
  "Table which maps from components to full source pathnames.")


(defvar *binary-pathnames-table* (make-hash-table :test #'equal)
  "Table which maps from components to full binary pathnames.")


(defparameter *reset-full-pathname-table* t
  "If T, clears the full-pathname tables before each call to OPERATE-ON-SYSTEM.
Setting this to NIL may yield faster performance after multiple calls
to LOAD-SYSTEM and COMPILE-SYSTEM, but could result in changes to
system and language definitions to not take effect, and so should be
used with caution.")


(defun clear-full-pathname-tables ()
  (clrhash *source-pathnames-table*)
  (clrhash *binary-pathnames-table*))


(defun component-full-pathname (component type &optional (version *version*))
  (when component
    (case type
      (:source
       (let ((old (gethash component *source-pathnames-table*)))
	 (or old
	     (let ((new (component-full-pathname-i component type version)))
	       (setf (gethash component *source-pathnames-table*) new)
	       new))))
      (:binary
        (let ((old (gethash component *binary-pathnames-table*)))
	 (or old
	     (let ((new (component-full-pathname-i component type version)))
	       (setf (gethash component *binary-pathnames-table*) new)
	       new))))
      (otherwise
       (component-full-pathname-i component type version)))))


;;madhu 200229  long standing userspace kludge.
;; clozure escapes the final dot in pathname-name or pathname-type
;; (namestring (make-pathname :name "foo.lisp" :type "bar.lisp"))
;;  "foo.lisp.bar\\.lisp"
;;  (namestring (make-pathname :name "foo.lisp" :type nil))
;; "foo\\.lisp"
;;  (namestring (make-pathname :name "foo.lisp" :type "bar\\."))
;; (pathname  "foo.lisp.bar\\.")
;; (fix-clozure-namestring (namestring (make-pathname :name "/a.b/c" :type "d.d/foo.bar.lisp")
;; (fix-clozure-namestring (namestring (probe-file "/dev/shm/\\\\.")))
;;(elt  (directory "/dev/shm/*" :follow-links nil) 5)
#+nil
(fix-clozure-namestring  "foo.lisp.bar\\.\\.lisp\\.var")

(defun fix-clozure-namestring (string)
  #+clozure
  (let ((p (search "\\." string :from-end t)))
    (if p
	(let ((end (length string)) ret)
	  (loop
	    (push (subseq string (1+ p) end) ret)
	    (setq end p)
	    (unless (setq p (search "\\." string :from-end t :end2 p))
	      (push (subseq string 0 end) ret)
	      (return (apply #'concatenate 'string ret)))))
	string))
  #-clozure
  string)

(defun component-full-pathname-i (component type
                                            &optional (version *version*)
					    &aux version-dir version-replace)
  ;; If the pathname-type is :binary and the root pathname is null,
  ;; distribute the binaries among the sources (= use :source pathname).
  ;; This assumes that the component's :source pathname has been set
  ;; before the :binary one.
  (if version
      (multiple-value-setq (version-dir version-replace)
	(translate-version version))
      (setq version-dir *version-dir* version-replace *version-replace*))
  ;; (format *trace-output* "~&>>>> VERSION COMPUTED ~S ~S~%" version-dir version-replace)
  (let ((pathname
	 (append-directories
	  (if version-replace
	      version-dir
	      (append-directories (component-root-dir component type)
				  version-dir))
	  (component-pathname component type)))
	(module-p (if (member (component-type component) '(:module)) t)))

    ;; ;madhu 240324 recipe for disaster
    (when module-p
      (setq pathname
	    (concatenate 'string pathname
			 (if (pathname-logical-p (pathname pathname))
			     ";"
			     "/"))))


    ;; When a logical pathname is used, it must first be translated to
    ;; a physical pathname. This isn't strictly correct. What should happen
    ;; is we fill in the appropriate slots of the logical pathname, and
    ;; then return the logical pathname for use by compile-file & friends.
    ;; But calling translate-logical-pathname to return the actual pathname
    ;; should do for now.

    ;; (format t "pathname = ~A~%" pathname)
    ;; (format t "type = ~S~%" (component-extension component type))

    ;; 20000303 Marco Antoniotti
    ;; Changed the following according to suggestion by Ray Toy.  I
    ;; just collapsed the tests for "logical-pathname-ness" into a
    ;; single test (heavy, but probably very portable) and added the
    ;; :name argument to the MAKE-PATHNAME in the MERGE-PATHNAMES
    ;; beacuse of possible null names (e.g. :defsystem components)
    ;; causing problems with the subsequenct call to NAMESTRING.
    ;; (format *trace-output* "~&>>>> PATHNAME is ~S~%" pathname)

    ;; 20050309 Marco Antoniotti
    ;; The treatment of PATHNAME-HOST and PATHNAME-DEVICE in the call
    ;; to MAKE-PATHNAME in the T branch is bogus.   COMPONENT-DEVICE
    ;; and COMPONENT-HOST must respect the ANSI definition, hence,
    ;; they cannot be PATHNAMEs.  The simplification of the code is
    ;; useful.  SCL compatibility may be broken, but I doubt it will.

    ;; 20050310 Marco Antoniotti
    ;; After a suggestion by David Tolpin, the code is simplified even
    ;; more, and the logic should be now more clear: use the user
    ;; supplied pieces of the pathname if non nil.

    ;; 20050613 Marco Antoniotti
    ;; Added COMPONENT-NAME extraction to :NAME part, in case the
    ;; PATHNAME-NAME is NIL.

    ;; ;madhu 060520 (PATHNAME pathname) or search-hosts are logical
    (cond ((pathname-logical-p (pathname pathname)) ; See definition of test above.
	  (unless module-p
	   (setf pathname
		 (merge-pathnames pathname
				  (make-pathname ;madhu 080208 (lw)
				   :name (or (pathname-name pathname)
					     (component-name component))
				   :type (component-extension component
							      type)))))
	   (namestring (translate-logical-pathname pathname)))
	  (t
	   (fix-clozure-namestring
	   (namestring
	    (make-pathname :host (or (component-host component)
				     (pathname-host pathname))
			   #+nil ;; madhu 050203
			   (or (when (component-host component)
				 ;; MCL2.0b1 and ACLPC cause an error on
				 ;; (pathname-host nil)
				 (pathname-host (component-host component)
						#+scl :case #+scl :common))
			       #+cmu
			       lisp::*unix-host*)


			   :directory (pathname-directory pathname
							  #+scl :case
							  #+scl :common
							  )

			   ;madhu 060520 cannot have pathname-name
			   ;here because cmucl (ansi) parses the
			   ;string PATHNAME "foo.new" into "foo" of
			   ;type
			   ;
			   ; madhu 061005 - private file is an
			   ; exception as one cannot rely on
			   ; component-name there. (silly argument for
			   ; pathname-name error)
			   ;;madhu 080206 (fix from marcoxa)
			   ;;madhu 170724 give ecl a chance to handle
			   ;; files with periods in them (:file
			   ;; "glib.package")
			   :name
                          (unless module-p
			   (or
			    ;; handle (:file "src/package")
			    ;; handle (:file "glib.package")
			    ;; handle (:file "src/glib.package")
			    (cond ((find #\. (component-name component))
				   (cond ((find #\/ (component-name component))
					  (file-namestring
					   (component-name component)))
					 (t (component-name component))))
				  (t
				   (cond ((find #\/ (component-name component))
					  nil)
					 (t nil))))
			    (pathname-name pathname
                                           #+scl :case
                                           #+scl :common
                                           )
                            (component-name component)))
			   :type
			  (unless module-p
			   #-scl (component-extension component type)
			   #+scl (string-upcase
				  (component-extension component type)))

			   :device
			   #+sbcl
			   :unspecific
			   #-(or :sbcl)
			   (or (component-device component)
			       (pathname-device pathname
						#+scl :case
						#+scl :common
						))
			   ;; :version :newest
			   )))))))


#-lispworks
(defun translate-version (version)
  ;; Value returns the version directory and whether it replaces
  ;; the entire root (t) or is a subdirectory.
  ;; Version may be nil to signify no subdirectory,
  ;; a symbol, such as alpha, beta, omega, :alpha, mark, which
  ;; specifies a subdirectory of the root, or
  ;; a string, which replaces the root.
  (cond ((null version)
	 (values "" nil))
	((symbolp version)
	 (values (let ((sversion (string version)))
		   (if (find-if #'lower-case-p sversion)
		       sversion
		       (string-downcase sversion)))
		 nil))
	((stringp version)
	 (values version t))
	(t (error "~&; Illegal version ~S" version))))


;;; Looks like LW has a bug in MERGE-PATHNAMES.
;;;
;;;  (merge-pathnames "" "LP:foo;bar;") ==> "LP:"
;;;
;;; Which is incorrect.
;;; The change here ensures that the result of TRANSLATE-VERSION is
;;; appropriate.

#+lispworks
(defun translate-version (version)
  ;; Value returns the version directory and whether it replaces
  ;; the entire root (t) or is a subdirectory.
  ;; Version may be nil to signify no subdirectory,
  ;; a symbol, such as alpha, beta, omega, :alpha, mark, which
  ;; specifies a subdirectory of the root, or
  ;; a string, which replaces the root.
  (cond ((null version)
	 (values (pathname "") nil))
	((symbolp version)
	 (values (let ((sversion (string version)))
		   (if (find-if #'lower-case-p sversion)
		       (pathname sversion)
		       (pathname (string-downcase sversion))))
		 nil))
	((stringp version)
	 (values (pathname version) t))
	(t (error "~&; Illegal version ~S" version))))


(defun component-extension-i (component type &key local)
  (ecase type
    (:source (or (component-source-extension component)
		 (unless local
		   (default-source-extension component)) ; system default
		 ;; (and (component-language component))
		 ))
    (:binary (or (component-binary-extension component)
		 (unless local
		   (default-binary-extension component)) ; system default
		 ;; (and (component-language component))
		 ))
    (:error  *compile-error-file-type*)))

;madhu 181224
(defun component-extension (component type &key local)
  (let ((ret (component-extension-i component type :local local)))
    (if (eq ret :empty) nil ret)))

(defsetf component-extension (component type) (value)
  `(ecase ,type
     (:source (setf (component-source-extension ,component) ,value))
     (:binary (setf (component-binary-extension ,component) ,value))
     (:error  (setf *compile-error-file-type* ,value))))


;;; ********************************
;;; System Definition **************
;;; ********************************

(defun create-component (type name definition-body &optional parent (indent 0))
  (let ((component (apply #'make-component
			  :type type
			  :name name
			  :indent indent
			  definition-body)))
    ;; handle :feature in depends-on ;madhu 201114
    (when (some #'consp (component-depends-on component))
      (labels ((resolve-dependency-spec (dep-spec)
		 (cond ((atom dep-spec) dep-spec)
		       ((consp dep-spec)
			(cond ((eql (car dep-spec) :require)
			       (assert (endp (cddr dep-spec)))
			       (warn "mk-defystem: dependency-spec: Ignoring asdf misfeature ~S. Use ~S directly."
				     `(:require ,(cdr dep-spec))
				     (second dep-spec))
			       (resolve-dependency-spec (second dep-spec)))
			      ((eql (car dep-spec) :version)
			       (assert (endp (cdddr dep-spec)))
			       (warn "mk-defsystem: dependency-spec: Ignoring asdf misfeature ~S. Use ~S directly."
				     `(:version ,(cdr dep-spec))
				     (second dep-spec))
			       (resolve-dependency-spec (second dep-spec)))
			      (t
			       (assert (eql (car dep-spec) :feature))
			       (assert (endp (cdddr dep-spec)))
			       (when (featurep (second dep-spec))
				 (resolve-dependency-spec (third dep-spec)))))))))
	(setf (component-depends-on component)
	      (remove-if #'null (mapcar #'resolve-dependency-spec
					(component-depends-on component))))))
    ;; Set up :load-only attribute
    (unless (find :load-only definition-body)
      ;; If the :load-only attribute wasn't specified,
      ;; inherit it from the parent. If no parent, default it to nil.
      (setf (component-load-only component)
	    (when parent
	      (component-load-only parent))))
    ;; Set up :compile-only attribute
    (unless (find :compile-only definition-body)
      ;; If the :compile-only attribute wasn't specified,
      ;; inherit it from the parent. If no parent, default it to nil.
      (setf (component-compile-only component)
	    (when parent
	      (component-compile-only parent))))

    ;; Set up :compiler-options attribute
    (unless (find :compiler-options definition-body)
      ;; If the :compiler-option attribute wasn't specified,
      ;; inherit it from the parent.  If no parent, default it to NIL.
      (setf (component-compiler-options component)
	    (when parent
	      (component-compiler-options parent))))

    #|| ISI Extension ||#
    ;; Set up :load-always attribute
    (unless (find :load-always definition-body)
      ;; If the :load-always attribute wasn't specified,
      ;; inherit it from the parent. If no parent, default it to nil.
      (setf (component-load-always component)
	    (when parent
	      (component-load-always parent))))

    ;; Initializations/after makes
    (canonicalize-component-name component)

    ;; Inherit package from parent if not specified.
    (setf (component-package component)
	  (or (component-package component)
	      (when parent (component-package parent))))

    ;; Type specific setup:
    (when (or (eq type :defsystem) (eq type :system) (eq type :subsystem))
      (setf (get-system name) component)
      #|(unless (component-language component)
	(setf (component-language component) :lisp))|#)

    ;; Set up the component's pathname
    (create-component-pathnames component parent)

    ;; If there are any components of the component, expand them too.
    (expand-component-components component (+ indent 2))

    ;; Make depends-on refer to structs instead of names.
    (link-component-depends-on (component-components component))

    ;; Design Decision: Topologically sort the dependency graph at
    ;; time of definition instead of at time of use. Probably saves a
    ;; little bit of time for the user.

    ;; Topological Sort the components at this level.
    (setf (component-components component)
          (topological-sort (component-components component)))

    ;; Return the component.
    component))


;;; preprocess-component-definition --
;;; New function introduced to manipulate the "evaluated" slots as per
;;; SDS' suggestions.
;;; 20050824

(defun preprocess-component-definition (definition-body)
  `(list* ,@(loop for slot in *component-evaluated-slots*
	          for value = (getf definition-body slot)
	          when value
                    do (remf definition-body slot)
                    and nconc `(,slot ,value))
	  #+nil				; madhu 061005 - this code
					; (1.102 revision) below is
					; nonsense
	  ,@(loop for slot in *component-form-slots*
		  for form = (getf definition-body slot)
	          do (remf definition-body slot)
                  nconc `(,slot (lambda () ,form)))
	  ',definition-body))


;;; defsystem --
;;; The main macro.
;;;
;;; 2002-11-22 Marco Antoniotti
;;; Added code to achieve a first cut "pathname less" operation,
;;; following the ideas in ASDF.  If the DEFSYSTEM form is loaded from
;;; a file, then the location of the file (intended as a directory) is
;;; computed from *LOAD-PATHNAME* and stored as the :SOURCE-PATHNAME
;;; of the system.

(defmacro defsystem (name &body definition-body) ;madhu 090209
  (unless (find :source-pathname definition-body)
    (setf definition-body
	  (list* :source-pathname
		 '(when *load-pathname*
		   (make-pathname :name nil
                                  :type nil
                                  :defaults *load-pathname*
		    ))
		 definition-body)))
  (let* ((can-name (canonicalize-system-name name))
	 (orig (gethash can-name *defsystem-to-defsystem-file-map*))
	 (val (or *load-truename* *compile-file-truename*)))
    (when (and orig (not (equalp val orig)))
      (warn "Defsystem ~A is being defined in ~A (previously ~A)" can-name
	    val orig))
    (setf (gethash can-name *defsystem-to-defsystem-file-map*) val))
  `(create-component :defsystem ',name
                     ,(preprocess-component-definition definition-body)
                     nil
                     0))


(defun create-component-pathnames (component parent)
  ;; Set up language-specific defaults

  (setf (component-language component)
	(or (component-language component) ; for local defaulting
	    (when parent		; parent's default
	      (component-language parent))))

  (setf (component-compiler component)
	(or (component-compiler component) ; for local defaulting
	    (when parent		; parent's default
	      (component-compiler parent))))
  (setf (component-loader component)
	(or (component-loader component) ; for local defaulting
	    (when parent		; parent's default
	      (component-loader parent))))

  ;; Evaluate the root dir arg
  (setf (component-root-dir component :source)
	(eval (component-root-dir component :source)))
  (setf (component-root-dir component :binary)
	(eval (component-root-dir component :binary)))

  ;; Evaluate the pathname arg
  (setf (component-pathname component :source)
	(eval (component-pathname component :source)))
  (setf (component-pathname component :binary)
	(eval (component-pathname component :binary)))


  ;; Pass along the host and devices
  (setf (component-host component)
	(or (component-host component)
	    (when parent (component-host parent))
	    (pathname-host *default-pathname-defaults*)))
  (setf (component-device component)
	(or (component-device component)
	    (when parent (component-device parent))))

  ;; Set up extension defaults
  (setf (component-extension component :source)
	(or (component-extension-i component :source
                                 :local #| (component-language component) |#
                                 t
                                 ) ; local default
            (when (component-language component)
              (default-source-extension component))
	    (when parent		; parent's default
	      (component-extension-i parent :source :local t))))
  (setf (component-extension component :binary)
	(or (component-extension-i component :binary
                                 :local #| (component-language component) |#
                                 t
                                 ) ; local default
            (when (component-language component)
              (default-binary-extension component))
	    (when parent		; parent's default
	      (component-extension-i parent :binary :local t))))

  ;; Set up pathname defaults -- expand with parent
  ;; We must set up the source pathname before the binary pathname
  ;; to allow distribution of binaries among the sources to work.
  (generate-component-pathname component parent :source)
  (generate-component-pathname component parent :binary))


;;;madhu 221123 handle output-translations
;;;

(defvar CL-USER::*USER-CACHE-DIR*
  (merge-pathnames ".cache/" (user-homedir-pathname)))

(dolist (sym '(CL-USER::*USER-CACHE-DIR*))
  (unless (documentation sym 'variable)
    (setf (documentation sym  'variable)
	  "Used by MK-DEFSYSTEM-3 OUTPUT-TRANSLATIONS-STRATEGY :USER-CACHE.
If this variable is to be used, it should set before the
defsystem.lisp file is loaded. The value defined here is not reliable")))

#+nil
(probe-file CL-USER::*USER-CACHE-DIR*)

(export '*output-translations-strategy*)
(defvar *output-translations-strategy* :default
  ":DEFAULT is the legacy behaviour (and all that /that/ means.).

A value of :USER-CACHE puts binary output objects under ~/.cache (see
CL-USER::USER-CACHE-DIR) /under some circumstances/:

:BINARY-ROOT-DIR and :BINARY-PATHNAME parameters are NOT specified in
the defsystem form.

If the :SOURCE-PATHNAME is not specified in the defsystem form, the
defsystem file MUST explicitly be LOADed so it defaults to a valid
absolute path.  Interactively evaluating the defsystem form will NOT
WORK.  The system MUST contain files relative to the defsystem file
location.

For a defsystem file at /a/b/c/d.system we try to put binaries under
~/.cache/fasl/<system-name>/binary-<lisp-name>/<lisp-version>/a/b/c.

If MK:*TRANSLATE-OUTPUT-FOR-USER-CACHE-USE-SXHASH* is non-NIL, then the
binaries would be under
~/.cache/fasl/<system-name>/binary-<lisp-name>/<lisp-version>/SX
where SX is the sxhash of the pathname /a/b/c
")

(defun slynk-unique-dir-name ()
  "Taken from https://github.com/joaotavora/sly slynk/slynk-loader.lisp
on 221122. Returns a relative pathname of the form
binary-<lisp>/<version>_<arch>."
  (let (
	(*implementation-features*
  '(:allegro :lispworks :sbcl :clozure :cmu :clisp :ccl :corman :cormanlisp
    :armedbear :gcl :ecl :scl :mkcl :clasp))

	(*os-features*
  '(:macosx :linux :windows :mswindows :win32 :solaris :darwin :sunos :hpux
    :unix))

	(*architecture-features*
  '(:powerpc :ppc :ppc64 :x86 :x86-64 :x86_64 :amd64 :i686 :i586 :i486 :pc386 :iapx386
    :sparc64 :sparc :hppa64 :hppa :arm :armv5l :armv6l :armv7l :arm64 :aarch64
    :pentium3 :pentium4
    :mips :mipsel
    :java-1.4 :java-1.5 :java-1.6 :java-1.7)))
    (labels (
	   #+ecl
	   ( ecl-version-string ()
  (format nil "~A~@[-~A~]"
          (lisp-implementation-version)
          (when (find-symbol "LISP-IMPLEMENTATION-VCS-ID" :ext)
            (let ((vcs-id (funcall (read-from-string "ext:lisp-implementation-vcs-id"))))
              (when (>= (length vcs-id) 8)
                (subseq vcs-id 0 8))))))

	   #+clasp
	   (clasp-version-string ()
  (format nil "~A~@[-~A~]"
          (lisp-implementation-version)
          (core:lisp-implementation-id)))

	   (lisp-version-string ()
  #+(or clozure cmu) (substitute-if #\_ (lambda (x) (find x " /"))
                                    (lisp-implementation-version))
  #+(or cormanlisp scl mkcl) (lisp-implementation-version)
  #+sbcl (format nil "~a~:[~;-no-threads~]"
                 (lisp-implementation-version)
                 #+sb-thread nil
                 #-sb-thread t)
  #+lispworks (lisp-implementation-version)
  #+allegro   (format nil "~@{~a~}"
                      excl::*common-lisp-version-number*
                      (if (string= 'lisp "LISP") "A" "M")     ; ANSI vs MoDeRn
                      (if (member :smp *features*) "s" "")
                      (if (member :64bit *features*) "-64bit" "")
                      (excl:ics-target-case
                       (:-ics "")
                       (:+ics "-ics")))
  #+clisp     (let ((s (lisp-implementation-version)))
                (subseq s 0 (position #\space s)))
  #+armedbear (lisp-implementation-version)
  #+ecl (ecl-version-string)
  #+clasp (clasp-version-string)))
  "Return a name that can be used as a directory name that is
unique to a Lisp implementation, Lisp implementation version,
operating system, and hardware architecture."
  (flet ((first-of (features)
           (loop for f in features
                 when (find f *features*) return it))
         (maybe-warn (value fstring &rest args)
           (cond (value)
                 (t (apply #'warn fstring args)
                    "unknown"))))
    (let ((lisp (maybe-warn (first-of *implementation-features*)
                            "No implementation feature found in ~a."
                            *implementation-features*))
          (os   (maybe-warn (first-of *os-features*)
                            "No os feature found in ~a." *os-features*))
          (arch (maybe-warn (first-of *architecture-features*)
                            "No architecture feature found in ~a."
                            *architecture-features*))
          (version (maybe-warn (lisp-version-string)
                               "Don't know how to get Lisp ~
                                implementation version.")))
      (make-pathname
       :directory (list :relative
			(format nil "binary-~(~A~)" lisp)
			(format nil "~(~@{~a~^-~}~)" version os arch))))))))

#+nil
(pathname-directory (slynk-unique-dir-name))

(export '*translate-output-for-user-cache-use-sxhash*)
(defvar *translate-output-for-user-cache-use-sxhash* T)

(defun translate-output-for-user-cache-dir (src &rest strings)
  (assert (eql :ABSOLUTE (car (pathname-directory src))) nil
      "See Documentation/Comments before using OUTPUT-TRANSLATIONS-STRATEGY :USER-CACHE")
  (assert (eql :ABSOLUTE (car (pathname-directory CL-USER::*USER-CACHE-DIR*))) nil
      "See Documentation/Comments before using OUTPUT-TRANSLATIONS-STRATEGY :USER-CACHE")
  (make-pathname
   :name nil :version nil :type nil
   :directory
   (append (pathname-directory CL-USER::*USER-CACHE-DIR*)
	   strings
	   (cdr (pathname-directory (slynk-unique-dir-name)))
	   (if *translate-output-for-user-cache-use-sxhash*
	       (list (format nil "~D" (sxhash src)))
	       (cdr (pathname-directory src))))
   :defaults CL-USER::*USER-CACHE-DIR*))

#+nil
(translate-output-for-user-cache-dir "/path/to/system/a/b/c"
				     "fasl" "system")


;;; generate-component-pathnames --
;;; maybe file's inheriting of pathnames should be moved elsewhere?

(defun generate-component-pathname (component parent pathname-type &aux ots-p)
  ;; Pieces together a pathname for the component based on its component-type.
  ;; Assumes source defined first.
  ;; Null binary pathnames inherit from source instead of the component's
  ;; name. This allows binaries to be distributed among the source if
  ;; binary pathnames are not specified. Or if the root directory is
  ;; specified for binaries, but no module directories, it inherits
  ;; parallel directory structure.
  (case (component-type component)
    ((:defsystem :system)		; Absolute Pathname
     ;; Set the root-dir to be the absolute pathname
     ;; ;madhu 221122 - unless defined  - oops misssed a spot
     (unless (component-root-dir component pathname-type)
       (setf (component-root-dir component pathname-type)
	     (or (component-pathname component pathname-type)
		 (when (eq pathname-type :binary)
		   ;; When the binary root is nil, use source.
		   (setq ots-p t)
		   (component-root-dir component :source))) ))

     ;;madhu 221123 handle output-translations
     (when (and (eq *output-translations-strategy* :user-cache)
		(eq pathname-type :binary))
       (cond (ots-p
	      (let* ((bin (component-root-dir component :binary))
		     (src (component-root-dir component :source))
		     (rep (translate-output-for-user-cache-dir
			   src "fasl" (canonicalize-system-name
				       (component-name component)))))
		(setf (component-root-dir component :binary) rep)
		(format t "defsystem output-translations-strategy :user-cache
source ~S
overriding binary root ~S with ~S~&"
			src bin rep)))
	     (t (format t "defsystem output-translation-strategy :user-cache
ignored for supplied binary-root ~S: ~S~&"
			(component-name component)
			(component-root-dir component :binary)))))

     ;; Set the relative pathname to be nil
     (setf (component-pathname component pathname-type)
	   nil));; should this be "" instead?
    ;; If the name of the component-pathname is nil, it
    ;; defaults to the name of the component. Use "" to
    ;; avoid this defaulting.
    (:private-file                      ; Absolute Pathname
     ;; Root-dir is the directory part of the pathname
     (setf (component-root-dir component pathname-type)
	   ""
	   #+ignore(or (when (component-pathname component pathname-type)
			 (pathname-directory
			  (component-pathname component pathname-type)))
		       (when (eq pathname-type :binary)
			 ;; When the binary root is nil, use source.
			 (component-root-dir component :source)))
	   )
     ;; If *SOURCE-PATHNAME-DEFAULT* or *BINARY-PATHNAME-DEFAULT* is "",
     ;; then COMPONENT-SOURCE-PATHNAME or COMPONENT-BINARY-PATHNAME could
     ;; wind up being "", which is wrong for :file components. So replace
     ;; them with NIL.
     (when (null-string (component-pathname component pathname-type))
       (setf (component-pathname component pathname-type) nil))
     ;; The relative pathname is the name part
     (setf (component-pathname component pathname-type)
	   (or (when (and (eq pathname-type :binary)
			  (null (component-pathname component :binary)))
		 ;; When the binary-pathname is nil use source.
		 (component-pathname component :source))
	       (or (when (component-pathname component pathname-type)
                     ;; (pathname-name )
		     (component-pathname component pathname-type))
		   (component-name component)))))
    ((:module :subsystem)			; Pathname relative to parent.
     ;; Inherit root-dir from parent
     (unless (component-root-dir component pathname-type)
       ;; unless defined ;madhu 181119
       (setf (component-root-dir component pathname-type)
	     (component-root-dir parent pathname-type)))
     ;; Tack the relative-dir onto the pathname
     (setf (component-pathname component pathname-type)
	   (or (when (and (eq pathname-type :binary)
			  (null (component-pathname component :binary)))
		 ;; When the binary-pathname is nil use source.
		 (component-pathname component :source))
	       (append-directories
		(component-pathname parent pathname-type)
		(or (component-pathname component pathname-type)
		    (component-name component))))))
    (:file				; Pathname relative to parent.
     ;; Inherit root-dir from parent
     (unless (component-root-dir component pathname-type)
       ;; unless defined ;madhu 181119
       (setf (component-root-dir component pathname-type)
	     (component-root-dir parent pathname-type)))
     ;; If *SOURCE-PATHNAME-DEFAULT* or *BINARY-PATHNAME-DEFAULT* is "",
     ;; then COMPONENT-SOURCE-PATHNAME or COMPONENT-BINARY-PATHNAME could
     ;; wind up being "", which is wrong for :file components. So replace
     ;; them with NIL.
     (when (null-string (component-pathname component pathname-type))
       (setf (component-pathname component pathname-type) nil))
     ;; Tack the relative-dir onto the pathname
     (setf (component-pathname component pathname-type)
	   (or (append-directories
		(component-pathname parent pathname-type)
		(or (component-pathname component pathname-type)
		    (component-name component)
		    (when (eq pathname-type :binary)
		      ;; When the binary-pathname is nil use source.
		      (component-pathname component :source)))))))
    ))

(defun featurep (x &optional (*features* *features*))
  "Checks whether a feature expression X is true with respect to the *FEATURES* set,
as per the CLHS standard for #+ and #-. Beware that just like the
CLHS, we assume symbols from the KEYWORD package are used, but that
unless you're using #+/#- your reader will not have magically used the
KEYWORD package, so you need specify keywords explicitly."
    (cond
      ((atom x) (and (member x *features*) t))
      ((eq :not (car x)) (assert (null (cddr x))) (not (featurep (cadr x))))
      ((eq :or (car x)) (some #'featurep (cdr x)))
      ((eq :and (car x)) (every #'featurep (cdr x)))
      (t (error "~S: malformed feature specification ~S" 'featurep x))))

#|| ;; old version
(defun expand-component-components (component &optional (indent 0))
  (let ((definitions (component-components component)))
    (setf (component-components component)
	  (remove-if #'null
		     (mapcar #'(lambda (definition)
				 (expand-component-definition definition
							      component
							      indent))
			     definitions)))))
||#

;;; new version
(defun expand-component-components (component &optional (indent 0))
  (let ((definitions
	  (remove-if-not (lambda (c)
			   (let ((feat (and (consp c) (getf c :if-feature)))) ;component-if-feature
			     (if feat
				 (featurep feat)
				 t)))
			 (component-components component))))
    (if (eq (car definitions) :serial)
	(setf (component-components component)
	      (expand-serial-component-chain (cdr definitions)
					     component indent))
	(setf (component-components component)
	      (expand-component-definitions definitions component indent)))))


(defun expand-component-definitions (definitions parent &optional (indent 0))
  (let ((components nil))
    (dolist (definition definitions)
      (let ((new (expand-component-definition definition parent indent)))
	(when new (push new components))))
    (nreverse components)))


(defun expand-serial-component-chain (definitions parent &optional (indent 0))
  (let ((previous nil)
	(components nil))
    (dolist (definition definitions)
      (let ((new (expand-component-definition definition parent indent)))
	(when new
	  ;; Make this component depend on the previous one. Since
	  ;; we don't know the form of the definition, we have to
	  ;; expand it first.
	  (when previous (pushnew previous (component-depends-on new)))
	  ;; The dependencies will be linked later, so we use the name
	  ;; instead of the actual component.
	  (setq previous (component-name new))
	  ;; Save the new component.
	  (push new components))))
    ;; Return the list of expanded components, in appropriate order.
    (nreverse components)))


(defparameter *enable-straz-absolute-string-hack* nil
  "Special hack requested by Steve Strassman, where the shorthand
   that specifies a list of components as a list of strings also
   recognizes absolute pathnames and treats them as files of type
   :private-file instead of type :file. Defaults to NIL, because I
   haven't tested this.")


(defun absolute-file-namestring-p (string)
  ;; If a FILE namestring starts with a slash, or is a logical pathname
  ;; as implied by the existence of a colon in the filename, assume it
  ;; represents an absolute pathname.
  (or (find #\: string :test #'char=)
      (and (not (null-string string))
	   (char= (char string 0) #\/))))


(defun expand-component-definition (definition parent &optional (indent 0))
  ;; Should do some checking for malformed definitions here.
  (cond ((null definition) nil)
        ((stringp definition)
         ;; Strings are assumed to be of type :file
	 (if (and *enable-straz-absolute-string-hack*
		  (absolute-file-namestring-p definition))
	     ;; Special hack for Straz
	     (create-component :private-file definition nil parent indent)
	     ;; Normal behavior
	     (create-component :file definition nil parent indent)))
        ((and (listp definition)
              (not (member (car definition)
			   '(:defsystem :system :subsystem
			      :module :file :private-file))))
         ;; Lists whose first element is not a component type
         ;; are assumed to be of type :file
         (create-component :file
			   (first definition)
			   ;; (preprocess-component-definition (rest definition)) ; Not working.
                           (rest definition)
			   parent
			   indent))
        ((listp definition)
         ;; Otherwise, it is (we hope) a normal form definition
         (create-component (first definition)   ; type
                           (second definition)  ; name

			   ;; definition body
                           ;; (preprocess-component-definition (cddr definition)) ; Not working.
                           (cddr definition)

                           parent             ; parent
			   indent)            ; indent
         )))


(defun link-component-depends-on (components)
  (dolist (component components)
    (unless (and *system-dependencies-delayed*
                 (eq (component-type component) :defsystem))
      (setf (component-depends-on component)
            (mapcar #'(lambda (dependency)
			(let ((parent (find (string dependency) components
					    :key #'component-name
					    :test #'string-equal)))
			  (cond (parent parent)
				;; make it more intelligent about the following
				(t (warn "Dependency ~S of component ~S not found."
					 dependency component)))))

                    (component-depends-on component))))))


;;; ********************************
;;; Topological Sort the Graph *****
;;; ********************************

;;; New version of topological sort suggested by rs2. Even though
;;; this version avoids the call to sort, in practice it isn't faster. It
;;; does, however, eliminate the need to have a TIME slot in the
;;; topological-sort-node defstruct.

(defun topological-sort (list &aux (sorted-list nil))
  (labels ((dfs-visit (znode)
             (setf (topsort-color znode) :gray)
             (unless (and *system-dependencies-delayed*
                          (eq (component-type znode) :system))
               (dolist (child (component-depends-on znode))
                 (cond ((eq (topsort-color child) :white)
                        (dfs-visit child))
                       ((eq (topsort-color child) :gray)
                        (format t "~&Detected cycle containing ~A" child)))))
             (setf (topsort-color znode) :black)
             (push znode sorted-list)))
    (dolist (znode list)
      (setf (topsort-color znode) :white))
    (dolist (znode list)
      (when (eq (topsort-color znode) :white)
        (dfs-visit znode)))
    (nreverse sorted-list)))

#||
;;; Older version of topological sort.
(defun topological-sort (list &aux (time 0))
  ;; The algorithm works by calling depth-first-search to compute the
  ;; blackening times for each vertex, and then sorts the vertices into
  ;; reverse order by blackening time.
  (labels ((dfs-visit (node)
	      (setf (topsort-color node) 'gray)
	      (unless (and *system-dependencies-delayed*
			   (eq (component-type node) :defsystem))
		(dolist (child (component-depends-on node))
		  (cond ((eq (topsort-color child) 'white)
			 (dfs-visit child))
			((eq (topsort-color child) 'gray)
			 (format t "~&Detected cycle containing ~A" child)))))
		      (setf (topsort-color node) 'black)
		      (setf (topsort-time node) time)
		      (incf time)))
    (dolist (node list)
      (setf (topsort-color node) 'white))
    (dolist (node list)
      (when (eq (topsort-color node) 'white)
        (dfs-visit node)))
    (sort list #'< :key #'topsort-time)))
||#

;;; ********************************
;;; Output to User *****************
;;; ********************************
;;; All output to the user is via the tell-user functions.

(defun split-string (string &key (item #\space) (test #'char=))
  ;; Splits the string into substrings at spaces.
  (let ((len (length string))
	(index 0) result)
    (dotimes (i len
		(progn (unless (= index len)
			 (push (subseq string index) result))
		       (reverse result)))
      (when (funcall test (char string i) item)
	(unless (= index i);; two spaces in a row
	  (push (subseq string index i) result))
	(setf index (1+ i))))))

;; probably should remove the ",1" entirely. But AKCL 1.243 dies on it
;; because of an AKCL bug.
;; KGK suggests using an 8 instead, but 1 does nicely.

(defun prompt-string (component)
  (format nil "; ~:[~;TEST:~]~V,1@T "
	  *oos-test*
	  (component-indent component)))

#||
(defun format-justified-string (prompt contents)
  (format t (concatenate 'string
			 "~%"
			 prompt
			 "-~{~<~%" prompt " ~1,80:; ~A~>~^~}")
	  (split-string contents))
  (finish-output *standard-output*))
||#

(defun format-justified-string (prompt contents &optional (width 80)
				       (stream *standard-output*))
  (let ((prompt-length (+ 2 (length prompt))))
    (cond ((< (+ prompt-length (length contents)) width)
	   (format stream "~%~A- ~A" prompt contents))
	  (t
	   (format stream "~%~A-" prompt)
	   (do* ((cursor prompt-length)
		 (contents (split-string contents) (cdr contents))
		 (content (car contents) (car contents))
		 (content-length (1+ (length content)) (1+ (length content))))
	       ((null contents))
	     (cond ((< (+ cursor content-length) width)
		    (incf cursor content-length)
		    (format stream " ~A" content))
		   (t
		    (setf cursor (+ prompt-length content-length))
		    (format stream "~%~A  ~A" prompt content)))))))
  (finish-output stream))


(defun tell-user (what component &optional type no-dots force)
  (when (or *oos-verbose* force)
    (format-justified-string (prompt-string component)
     (format nil "~A ~(~A~) ~@[\"~A\"~] ~:[~;...~]"
	     ;; To have better messages, wrap the following around the
	     ;; case statement:
	     ;;(if (find (component-type component)
	     ;;    '(:defsystem :system :subsystem :module))
	     ;;  "Checking"
	     ;;  (case ...))
	     ;; This gets around the problem of DEFSYSTEM reporting
	     ;; that it's loading a module, when it eventually never
	     ;; loads any of the files of the module.
	     (case what
	       ((compile :compile)
		(if (component-load-only component)
		    ;; If it is :load-only t, we're loading.
		    "Loading"
		    ;; Otherwise we're compiling.
		    "Compiling"))
	       ((load :load) "Loading")
	       (otherwise what))
	     (component-type component)
	     (or (when type
		   (component-full-pathname component type))
		 (component-name component))
	     (and *tell-user-when-done*
		  (not no-dots))))))


(defun tell-user-done (component &optional force no-dots)
  ;; test is no longer really used, but we're leaving it in.
  (when (and *tell-user-when-done*
	     (or *oos-verbose* force))
    (format t "~&~A~:[~;...~] Done."
	    (prompt-string component) (not no-dots))
    (finish-output *standard-output*)))


(defmacro with-tell-user ((what component &optional type no-dots force) &body body)
  `(progn
     (tell-user ,what ,component ,type ,no-dots ,force)
     ,@body
     (tell-user-done ,component ,force ,no-dots)))


(defun tell-user-no-files (component &optional force)
  (when (or *oos-verbose* force)
    (format-justified-string (prompt-string component)
      (format nil "Source file ~A ~
             ~:[and binary file ~A ~;~]not found, not loading."
	      (component-full-pathname component :source)
	      (or *load-source-if-no-binary* *load-source-instead-of-binary*)
	      (component-full-pathname component :binary)))))


(defun tell-user-require-system (name parent)
  (when *oos-verbose*
    (format t "~&; ~:[~;TEST:~] - System ~A requires ~S"
	    *oos-test* (component-name parent) name)
    (finish-output *standard-output*)))


(defun tell-user-generic (string)
  (when *oos-verbose*
    (format t "~&; ~:[~;TEST:~] - ~A"
	    *oos-test* string)
    (finish-output *standard-output*)))


;;; ********************************
;;; Y-OR-N-P-WAIT ******************
;;; ********************************
;;; Y-OR-N-P-WAIT is like Y-OR-N-P, but will timeout after a specified
;;; number of seconds. I should really replace this with a call to
;;; the Y-OR-N-P-WAIT defined in the query.cl package and include that
;;; instead.

(defparameter *use-timeouts* t
  "If T, timeouts in Y-OR-N-P-WAIT are enabled. Otherwise it behaves
   like Y-OR-N-P. This is provided for users whose lisps don't handle
   read-char-no-hang properly.")

(defparameter *clear-input-before-query* t
  "If T, y-or-n-p-wait will clear the input before printing the prompt
   and asking the user for input.")

;;; The higher *sleep-amount* is, the less consing, but the lower the
;;; responsiveness.
(defparameter *sleep-amount* #-CMU 0.1 #+CMU 1.0
    "Amount of time to sleep between checking query-io. In multiprocessing
     Lisps, this allows other processes to continue while we busy-wait. If
     0, skips call to SLEEP.")


(defun internal-real-time-in-seconds ()
  (get-universal-time))


(defun read-char-wait (&optional (timeout 20) input-stream
                                 (eof-error-p t) eof-value
                                 &aux peek)
  (if (ignore-errors (interactive-stream-p input-stream)) ; blows on ecl
      (do ((start (internal-real-time-in-seconds)))
	  ((or (setq peek (listen input-stream))
               (< (+ start timeout) (internal-real-time-in-seconds)))
	   (when peek
	     ;; was read-char-no-hang
	     (read-char input-stream eof-error-p eof-value)))
	(unless (zerop *sleep-amount*)
	  (sleep *sleep-amount*)))
      ;;else
      (if eof-error-p
	  (error "mk::read-char-wait on non-interactive stream")
	  (progn (when *oos-verbose*
		   (format t "mk::read-char-wait on non-interactive-stream ~S~&"
			   input-stream)
		   eof-value)))))


;;; Lots of lisps, especially those that run on top of UNIX, do not get
;;; their input one character at a time, but a whole line at a time because
;;; of the buffering done by the UNIX system. This causes y-or-n-p-wait
;;; to not always work as expected.
;;;
;;; I wish lisp did all its own buffering (turning off UNIX input line
;;; buffering by putting the UNIX into CBREAK mode). Of course, this means
;;; that we lose input editing, but why can't the lisp implement this?

(defun y-or-n-p-wait (&optional (default #\y) (timeout 20)
				format-string &rest args)
  "Y-OR-N-P-WAIT prints the message, if any, and reads characters from
   *QUERY-IO* until the user enters y, Y or space as an affirmative, or either
   n or N as a negative answer, or the timeout occurs. It asks again if
   you enter any other characters."
  (when *clear-input-before-query* (clear-input *query-io*))
  (when format-string
    (fresh-line *query-io*)
    (apply #'format *query-io* format-string args)
    ;; FINISH-OUTPUT needed for CMU and other places which don't handle
    ;; output streams nicely. This prevents it from continuing and
    ;; reading the query until the prompt has been printed.
    (finish-output *query-io*))
  (loop
   (let* ((read-char (if *use-timeouts*
			 (read-char-wait timeout *query-io* nil nil)
			 (read-char *query-io*)))
	  (char (or read-char default)))
     ;; We need to ignore #\newline because otherwise the bugs in
     ;; clear-input will cause y-or-n-p-wait to print the "Type ..."
     ;; message every time... *sigh*
     ;; Anyway, we might want to use this to ignore whitespace once
     ;; clear-input is fixed.
     (unless (find char '(#\tab #\newline #\return))
       (when (null read-char)
	 (format *query-io* "~@[~A~]" default)
	 (finish-output *query-io*))
       (cond ((null char) (return t))
	     ((find char '(#\y #\Y #\space) :test #'char=) (return t))
	     ((find char '(#\n #\N) :test #'char=) (return nil))
	     (t
	      (when *clear-input-before-query* (clear-input *query-io*))
	      (format *query-io* "~&Type \"y\" for yes or \"n\" for no. ")
	      (when format-string
		(fresh-line *query-io*)
		(apply #'format *query-io* format-string args))
	      (finish-output *query-io*)))))))

#||
(y-or-n-p-wait #\y 20 "What? ")
(progn (format t "~&hi") (finish-output)
       (y-or-n-p-wait #\y 10 "1? ")
       (y-or-n-p-wait #\n 10 "2? "))
||#


;;; ----------------------------------------------------------------------
;;;
;;; madhu: Concatedfasl target 101216
;;;
;;; alternative interfaces to files-in-system-and-dependents
;;;
;;;  *load-concated-fasl-instead*  (TODO)
;;;  *concated-fasl-suffix*
;;;  %concated-fasl-pathname
;;;  %mk-traverse
;;;  system-map-files (&optional function &key type (:source :binary)
;;;  mklib
;;; *recursively-handle-deps* (TODO ELIM)
;;;
;;; ;madhu 170729: ecl support. ECL cannot concatenate fasls
;;; directly. Object files have to be built first:
;;; - bind *ecl-compile-file-system-p* to T when compiling the system
;;;   to additionally generate .o files
;;; - mklib has to use the .o files to build the fas files
;;;
;;; i.e. (let ((mk::*ecl-compile-file-system-p* t)) (compile-system :foo))
;;; (mklib :foo :fasl)
;;; (mklib :foo :shared-library)
;;; (mklib :foo :static-library)
;;;
;;; When making ECL shared or static libraries (for linking with C),
;;; each library is given its own init function.  Any dashes in the
;;; system name are replaced with an underscore in the init function
;;;

(defun %mk-traverse (system function &optional collect-results
		     *recursively-handle-deps*)
  ;; One _could_ use mk::*operations-propagate-to-subsystems* instead
  ;; of *recursively-handle-deps* to control what happens on
  ;; dependencies but the default value may not be appropriate.
  (declare (special *recursively-handle-deps*))
  (labels ((walk-components (component &aux result)
	     (setq result (funcall function component))
	     (ecase (mk::component-type component)
	       ((:file :private-file)
		(when collect-results
		  (if (or (eql collect-results t)
			  (funcall collect-results component))
		      (list result))))
	       ((:module :system :subsystem :defsystem)
		(let* ((deps (mk::component-depends-on component))
		       (handle-deps-p
			(cond
			  ((null deps) nil)
			  ((eq *recursively-handle-deps* :never)
			   (format t "Always Skipping deps: skipped ~A."
				   deps)
			   nil)
			  ((eq *recursively-handle-deps* :module)
			   (warn "deps = ~S component=~S" deps component)
			   (eq (mk::component-type component) :module))
			  (*recursively-handle-deps* t)
			  (t
			   (restart-case
			       (cerror "Skip deps." "System ~S depends on: ~S."
				       component deps)
			     (handle-this-once ()
			       :report "Handle deps."
			       t)
			     (recursively-open-deps ()
			       :report "Always recursively handle deps."
			       (setq *recursively-handle-deps* t))
			     (discard-all-deps ()
			       :report "Never handle deps."
			       (setq *recursively-handle-deps* :never)
			       nil))))))
		  (nconc
		   (when handle-deps-p
		     (loop for dep in deps
			   for depsys = (ignore-errors (mk:find-system dep :load))
			   unless depsys
			   do
			   (warn "Not handling unknown system: ~S." dep)
			   else nconc (walk-components depsys)))
		   (loop for x in (mk::component-components component)
			 nconc (walk-components x))))))))
    (walk-components (etypecase system
		       (mk::component
			(ecase (mk::component-type system)
			  (:defsystem system)))
		       ((or string symbol) (mk:find-system system :load))))))

;; if function is nil return a list of files
(defun system-map-files (system &optional function &key
			 recursively-handle-deps
			 (type :source) &aux ret)
  ;; "Rename as SYSTEM-MAP-PATHNAMES if docstring is ever written."
  (%mk-traverse
   system
   (lambda (component)
     (when (find (mk::component-type component) '(:file :private-file))
       (let ((arg (and (mk::component-pathname component type)
		       (mk::component-full-pathname component type))))
	 (when arg
	   (if function
	       (funcall function (pathname arg))
	       (push  (pathname arg) ret))))))

   nil
   recursively-handle-deps)
  (nreverse ret))

(defvar *load-concated-fasl-instead* t)
(defvar *concated-fasl-suffix* "-library")

(defvar *ecl-compile-file-system-p* nil
  "If Non-NIL supply :SYSTEM-P to to ecl's COMPILE-FILE to produce a
  native object file.
For MKCL supply :FASL-P NIL to MKCL's COMPILE-FILE
")

(defun ecl-munge-o (pathname)
  (make-pathname :type "o" :defaults pathname))

(defun ecl-compile-one-component (component-obj)
  (let* ((source (component-full-pathname component-obj :source))
	 (binary (component-full-pathname component-obj :binary))
	 (object (ecl-munge-o binary)))
    (list
     (compile-file source :output-file binary)
     (compile-file source :output-file object
		   #+ecl :system-p #+ecl t
		   #+mkcl :fasl-p #+mkcl t))))

(defun %concated-fasl-pathname (system &key (defaults *default-pathname-defaults*) #+(or ecl mkcl) (ecl-build-type :fasl) no-lib-prefix no-library-suffix)
  (make-pathname
   :name (concatenate 'string
		      #+(or ecl mkcl)
		      (case ecl-build-type
			((:shared-library :static-library)
			 (if no-lib-prefix "" "lib"))
			(otherwise ""))
                      (MK::COMPONENT-NAME system)
		      (or #+(or ecl mkcl)
			  (when (eq ecl-build-type :exe)
			    "")
			  (if no-library-suffix ""
			      *concated-fasl-suffix*)))
   :type (or #+(or ecl mkcl)
	     (ecase ecl-build-type
	       ((nil :fasl) "fas")
	       (:exe "exe")
	       (:shared-library "so")
	       (:static-library "a"))
	     (mk::component-binary-extension system)
             #+cmu
             (c:backend-fasl-file-type c:*target-backend*)
             #-cmu
             (pathname-type (compile-file-pathname "foo")))
   :version :newest
   :defaults (etypecase defaults
               (symbol
                (ecase defaults
                  ((nil :system)
                   (mk::component-full-pathname system :binary))))
               (string (pathname defaults))
               (pathname defaults))))

(defun ensure-system (system)
  (etypecase system
    (mk::component
     (ecase (mk::component-type system)
       (:defsystem system)))
    (symbol (mk:find-system system))
    (string (mk:find-system system))))

(defun concated-fasl-list (system &key (recursively-handle-deps t) &aux fasls)
  (labels ((handle-load-only (component)
	     (assert (find (mk::component-type component)
			   '(:file :private-file)))
	     (let* ((source-pathname
		     (mk::component-full-pathname component :source))
		    (binary-pathname
		     (progn
		       #+(or ecl mkcl)(error "cannot handle :load-only when building ecl libraries")
		       (mk::component-full-pathname component :binary)))
		    (binary-truename (probe-file binary-pathname))
		    (compilation-required-p
		     (cond (binary-truename
			    (< (file-write-date binary-truename)
			       (file-write-date (truename source-pathname))))
			   (t t))))
	       (restart-case
		   (cerror "Skip this file." "Component ~S is load-only."
			   component)
		 (use-existing ()
		   :test
		   (lambda (condition)
		     (declare (ignore condition))
		     (push binary-truename fasls))
		   :report
		   (lambda (stream)
		     (format stream "Use already compiled (~:[Current~;Out of Date~]) binary: ~A."
			     compilation-required-p binary-truename))
		   binary-truename)
		 (compile-and-use ()
		   :report
		   (lambda (stream)
		     (format stream "Compile ~A to ~A and use that fasl?"
			     source-pathname binary-pathname))
		   (multiple-value-bind (output-truename warnings-p failure-p)
		       (compile-file source-pathname
				     :output-file binary-pathname)
		     (declare (ignore warnings-p))
		     (cond (failure-p
			    (cerror "Skip this file."
				    "Compilation of ~A to produce ~A failed."
				    source-pathname binary-pathname))
			   (t (push output-truename fasls))))))))
	   (handle-one (component)
	     (when (find (mk::component-type component) '(:file :private-file))
	       (if (component-load-only component)
		   (handle-load-only component)
		   (pushnew component fasls)))))
    (%mk-traverse (ensure-system system)
		  #'handle-one
		  nil
		  recursively-handle-deps)
    fasls))

#+nil
(concated-fasl-list :hello :recursively-handle-deps t)

;madhu 190516
;;(user::package-add-nicknames "COMPILER" "C")
(defun mklib (system &key (defaults *default-pathname-defaults*)
	      no-lib-prefix no-library-suffix override
	      (recursively-handle-deps t)
	      #+(or ecl mkcl) (ecl-build-type :fasl)
	      #+(or ecl mkcl) init-function-name
	      #+(or ecl mkcl) build-program-args
	      skip-ensure-compiled)
  "Creates a concatenated fasl file named \"SYSTEM-LIBRARY.FASL\" from
the fasl files of the given mk-defsystem system SYSTEM.  The system
should already be compiled.  The concatenated file is written in the
DEFAULTS directory.  LIBRARY is taken from *concated-fasl-suffix*
which defaults to \"-library\".

ECL NOTES: If ECL-BUILD-TYPE is :SHARED-LIBRARY or :STATIC-LIBRARY,
C:BUILD-SHARED-LIBRARY or C:BUILD-SHARED-LIBRARY is called with an
:INIT-NAME of INIT-FUNCTION-NAME. If unspecified, a name of the form
`init_lib_<system>' is derived from the system name, after converting
dashes in the system name to underscores.

In these cases the name of the output file is of the form
\"LIB<SYSTEM>-LIBRARY.{so,a}\".

Use :NO-LIB-PREFIX T to skip produce a plain
\"<SYSTEM>-library.{so,a}\"

Use :NO-LIBRARY-SUFFIX to skip appending *concated-fasl-suffix* to
the basename of the target file.

Use :OVERRIDE <TARGET PATHNAME> to skip all these shennanigans and
just write to the given pathname.

If SKIP-ENSURE-COMPILED is non-NIL assume that the ECL or MKCL user
has already compiled the objects with mk:*ecl-compile-file-system-p*
set to t to produce the appropriate object artifacts.  This is
primarily meant used to be used when mk-defystem.system is a
dependency of the and mkcl (as of 1.178) should be instructed not to
reload this module which clobbers all objects.
"
  (let* ((system (ensure-system system))
	   (pathname (or override
			 (%concated-fasl-pathname
			  system :defaults defaults
			  :no-lib-prefix no-lib-prefix
			  :no-library-suffix no-library-suffix
			  #+(or ecl mkcl) :ecl-build-type
			  #+(or ecl mkcl) ecl-build-type)))
	   #-(or lispworks ecl mkcl)
	   (buf (make-array 2048 :element-type '(unsigned-byte 8)))
	 (fasls (concated-fasl-list system :recursively-handle-deps recursively-handle-deps)))
      #+lispworks
      (labels ((addobj (path) (SCM::CONCATENATE-OBJECT-FILE path))
	       (addcom (component)
		 (let ((file (mk::component-full-pathname component :binary)))
		   (assert file)
		   (addobj file)))
	       (addall () (mapcar #'addcom (nreverse fasls))))
	(SCM::INVOKE-WITH-CONCATENATED-FASL-FILE
	 #'addall pathname COMPILER:*HOST-TARGET-MACHINE*)
        pathname)

      ;; for ecl and mkcl - make sure the system is compiled
      #+(or ecl mkcl)
     (unless skip-ensure-compiled
      (ecase ecl-build-type
	((nil :fasl)
	 (compile-system system))
	((:shared-library :static-library)
	 (let ((*operations-propagate-to-subsystems* t)
	       (*ecl-compile-file-system-p* t))
	   (make::compile-system system)))))

      #+(or ecl mkcl)
      (apply
       (ecase ecl-build-type
	 ((nil :fasl) #'compiler:build-fasl)
	 (:exe #'compiler:build-program)
	 (:shared-library #'compiler:build-shared-library)
	 (:static-library #'compiler:build-static-library))
       pathname
       #+ecl :lisp-files
       #+mkcl :lisp-object-files (loop for component in (reverse fasls)
			 for o = (ecl-munge-o (mk::component-full-pathname
					  component :binary))
			 collect o
			 do (format t "Cating: ~S~&" o ))
       (append (when (case ecl-build-type
		       ((:shared-library :static-library) t))
		 (list :init-name
		       (or init-function-name
			   (concatenate 'string "init_lib_"
					(substitute #\_ #\- (mk::component-name system))))))
	       build-program-args))
      #-(or lispworks ecl mkcl)
      (with-open-file (out pathname :element-type '(unsigned-byte 8)
			   :direction :output :if-exists :supersede
			   :if-does-not-exist :create)
	(loop for component in (nreverse fasls)
	      for file = (mk::component-full-pathname component :binary)
	      do (assert file)
	      (restart-case
		  (with-open-file (in (truename file)
				      :element-type '(unsigned-byte 8))
		    (format t "~%; ~s" file)
		    (loop as x = (read-sequence buf in)
			  until (= x 0)
			  do (write-sequence buf out :end x)))
		(skip-this-file () :report "Skip this file.")))
	pathname)))

;;;
;;; ECL EXAMPLE
;;;

#+(or ecl mkcl)
(defun ecl-mklib (system &optional (ecl-build-type :shared-library)
		  force-compile
		  defaults
		  (recursively-handle-deps t)
		  init-function-name
		  build-program-args)
  ;; RECURSIVELY-HANDLE-DEPS if T bundle everything. if :NEVER bundle
  ;; nothing. if NIL NIL ask interactively
  (let ((make::*operations-propagate-to-subsystems* t)
	(make::*ecl-compile-file-system-p* t))
    (make::compile-system system :force force-compile))
  (make::mklib system :ecl-build-type ecl-build-type :defaults defaults
	       :recursively-handle-deps recursively-handle-deps
	       :init-function-name init-function-name
	       :build-program-args build-program-args))

(defun mk-concat-sources (system pathname &optional
			  make::*recursively-handle-deps*
			  &key (language :lisp)) ; wont work for grovel etc.
  "Concatenate the source files of system onto PATHNAME."
  (declare (special make::*recursively-handle-deps*))
  (let (list
	(buf (make-array 2048 :element-type '(unsigned-byte 8))))
    (flet ((collect-files (component)
	     (when (and (eq (mk::component-type component) :file)
			(if language
			    (eq (mk::component-language component) language)))
	       (push component list))))
      (%mk-traverse system #'collect-files nil nil)
      (with-open-file (out pathname :element-type '(unsigned-byte 8)
			   :direction :output :if-exists :supersede
			   :if-does-not-exist :create)
	(loop for component in (nreverse list)
	      for file = (mk::component-full-pathname component :source)
	      do (assert file)
	      (restart-case
		  (with-open-file (in (truename file)
				      :element-type '(unsigned-byte 8))
		    (format t "~%; ~s" file)
		    (loop as x = (read-sequence buf in)
			  until (= x 0)
			  do (write-sequence buf out :end x)))
		(skip-this-file () :report "Skip this file.")))
	pathname))))

;;;===========================================================================
;;; Running the operations.

(defvar %%component%% nil)

(export '(%%component%%)) ; Just a placeholder. Move it to the export list.


(defmacro with-special-component-vars ((c) &body forms)
  `(let ((%%component%% ,c))
    (declare (special %%component%%))
    ,@forms))


;;; ********************************
;;; Operate on System **************
;;; ********************************
;;; Operate-on-system
;;; Operation is :compile, 'compile, :load or 'load
;;; Force is :all or :new-source or :new-source-and-dependents or a list of
;;; specific modules.
;;;    :all (or T) forces a recompilation of every file in the system
;;;    :new-source-and-dependents compiles only those files whose
;;;          sources have changed or who depend on recompiled files.
;;;    :new-source compiles only those files whose sources have changed
;;;    A list of modules means that only those modules and their
;;;    dependents are recompiled.
;;; Test is T to print out what it would do without actually doing it.
;;;      Note: it automatically sets verbose to T if test is T.
;;; Verbose is T to print out what it is doing (compiling, loading of
;;;      modules and files) as it does it.
;;; Dribble should be the pathname of the dribble file if you want to
;;; dribble the compilation.
;;; Load-source-instead-of-binary is T to load .lisp instead of binary files.
;;; Version may be nil to signify no subdirectory,
;;; a symbol, such as alpha, beta, omega, :alpha, mark, which
;;; specifies a subdirectory of the root, or
;;; a string, which replaces the root.

(defun operate-on-system (name operation
			       &key
			       force
			       (version *version*)
			       (test *oos-test*) (verbose *oos-verbose*)
                               (load-source-instead-of-binary
				*load-source-instead-of-binary*)
                               (load-source-if-no-binary
				*load-source-if-no-binary*)
			       (bother-user-if-no-binary
				*bother-user-if-no-binary*)
			       (compile-during-load *compile-during-load*)
			       dribble
			       (minimal-load *minimal-load*)
			       (override-compilation-unit t)
			       )
  (declare #-(or :cltl2 :ansi-cl) (ignore override-compilation-unit))
  (unwind-protect
      ;; Protect the undribble.
      (#+(and (or :cltl2 :ansi-cl) (not :gcl)) with-compilation-unit
	 #+(and (or :cltl2 :ansi-cl) (not :gcl)) (:override override-compilation-unit)
	 #-(and (or :cltl2 :ansi-cl) (not :gcl)) progn
	(when *reset-full-pathname-table* (clear-full-pathname-tables))
	(when dribble (dribble dribble))
	(when test (setq verbose t))
	(when (null force)		; defaults
	  (case operation
	    ((load :load) (setq force :all))
	    ((compile :compile) (setq force :new-source-and-dependents))
	    (t (setq force :all))))
	;; Some CL implementations have a variable called *compile-verbose*
	;; or *compile-file-verbose*.
	(multiple-value-bind (*version-dir* *version-replace*)
	    (translate-version version)
	  ;; CL implementations may uniformly default this to nil
	  (let ((*load-verbose* #-common-lisp-controller t
				#+common-lisp-controller nil) ; nil
		#-(or MCL CMU CLISP ECL MKCL :sbcl lispworks scl clozure)
		(*compile-file-verbose* t) ; nil
		#+common-lisp-controller
		(*compile-print* nil)
		#+(and common-lisp-controller cmu)
		(ext:*compile-progress* nil)
		#+(and common-lisp-controller cmu)
		(ext:*require-verbose* nil)
		#+(and common-lisp-controller cmu)
		(ext:*gc-verbose* nil)

		(*compile-verbose* #-common-lisp-controller t
				   #+common-lisp-controller nil) ; nil
		(*version* version)
		(*oos-verbose* verbose)
		(*oos-test* test)
		(*load-source-if-no-binary* load-source-if-no-binary)
		(*compile-during-load* compile-during-load)
		(*bother-user-if-no-binary* bother-user-if-no-binary)
		(*load-source-instead-of-binary* load-source-instead-of-binary)
		(*minimal-load* minimal-load)
		(system (if (and (component-p name)
                                 (member (component-type name)
					 '(:system :defsystem :subsystem)))
                            name
                            (find-system name :load))))
;; ;madhu 190517 FIXME!
	    #-(or :abcl CMU CLISP :sbcl :lispworks :cormanlisp scl MKCL ECL :clasp)
	    (declare (special *compile-verbose* #-MCL *compile-file-verbose*)
		     #-openmcl (ignore *compile-verbose*
				       #-MCL *compile-file-verbose*)
		     #-openmcl (optimize (inhibit-warnings 3)))
	    (unless (component-operation operation)
	      (error "Operation ~A undefined." operation))

	    (operate-on-component system operation force))))
    (when dribble (dribble))))


(defun compile-system (name &key force
			    (version *version*)
			    (test *oos-test*)
                            (verbose *oos-verbose*)
			    (load-source-instead-of-binary
			     *load-source-instead-of-binary*)
			    (load-source-if-no-binary
			     *load-source-if-no-binary*)
			    (bother-user-if-no-binary
			     *bother-user-if-no-binary*)
			    (compile-during-load *compile-during-load*)
			    dribble
			    (minimal-load *minimal-load*))
  ;; For users who are confused by OOS.
  (operate-on-system
   name :compile
   :force force
   :version version
   :test test
   :verbose verbose
   :load-source-instead-of-binary load-source-instead-of-binary
   :load-source-if-no-binary load-source-if-no-binary
   :bother-user-if-no-binary bother-user-if-no-binary
   :compile-during-load compile-during-load
   :dribble dribble
   :minimal-load minimal-load))

(defun load-system (name &key force
			 (version *version*)
			 (test *oos-test*) (verbose *oos-verbose*)
			 (load-source-instead-of-binary
			  *load-source-instead-of-binary*)
			 (load-source-if-no-binary *load-source-if-no-binary*)
			 (bother-user-if-no-binary *bother-user-if-no-binary*)
			 (compile-during-load *compile-during-load*)
			 dribble
			 (minimal-load *minimal-load*))
  ;; For users who are confused by OOS.
  (operate-on-system
   name :load
   :force force
   :version version
   :test test
   :verbose verbose
   :load-source-instead-of-binary load-source-instead-of-binary
   :load-source-if-no-binary load-source-if-no-binary
   :bother-user-if-no-binary bother-user-if-no-binary
   :compile-during-load compile-during-load
   :dribble dribble
   :minimal-load minimal-load))

(defun clean-system (name &key (force :all)
			 (version *version*)
			 (test *oos-test*) (verbose *oos-verbose*)
			 dribble
		     ((:propagate *operations-propagate-to-subsystems*) nil))
  "Deletes all the binaries in the system."
  ;; For users who are confused by OOS.
  (operate-on-system
   name :delete-binaries
   :force force
   :version version
   :test test
   :verbose verbose
   :dribble dribble))

(defun edit-system
    (name &key force
	       (version *version*)
	       (test *oos-test*)
	       (verbose *oos-verbose*)
	       dribble)

  (operate-on-system
   name :edit
   :force force
   :version version
   :test test
   :verbose verbose
   :dribble dribble))

(defun hardcopy-system
    (name &key force
	       (version *version*)
	       (test *oos-test*)
	       (verbose *oos-verbose*)
	       dribble)

  (operate-on-system
   name :hardcopy
   :force force
   :version version
   :test test
   :verbose verbose
   :dribble dribble))


;;; ensure-external-system-def-loaded component --
;;; Let's treat definition clauses of the form
;;;
;;; 	(:system "name")
;;; i.e.
;;;
;;;	(:system "name" :components nil)
;;;
;;; in a special way.
;;; When encountered, MK:DEFSYSTEM tries to FIND-SYSTEM
;;; the system named "name" (by forcing a reload from disk).
;;; This may be more "natural".

(defun ensure-external-system-def-loaded (component)
  (assert (member (component-type component)
		  '(:subsystem :system)))
  (when (null (component-components component))
    (let* ((cname (component-name component)))
      (declare (ignorable cname))
      ;; First we ensure that we reload the system definition.
      (undefsystem cname)
      (let* ((*reload-systems-from-disk* t)
	     (system-component
              
              ;; (find-system (component-name component) :load)

              #| Marco Antoniotti 20090416: may break in strange ways due to circularities. |#
              (ecase (component-type component)
                (:system
                 (find-system (component-name component) :load))
                (:subsystem
                 #|
                 (warn "Circular dependencies may happen with ~
                        :SUBSYSTEM components.")|#
                 (find-system (component-name component) :load
                              (merge-pathnames
                               (make-pathname :name cname
                                              :type "system"
                                              :directory nil)
                               (component-full-pathname component
                                                        :source))
                              )))
              )
             )
	;; Now we have a problem.
	;; We have just ensured that a system definition is
	;; loaded, however, the COMPONENT at hand is different
	;; from SYSTEM-COMPONENT.
	;; To fix this problem we just use the following
	;; kludge.  This should prevent re-entering in this
	;; code branch, while actually preparing the COMPONENT
	;; for operation.
	(setf (component-components component)
	      (list system-component))
	))))


(defvar *file-local-variables* nil)
(export '*file-local-variables*)

(defun operate-on-component (component operation force &aux changed)
  ;; Returns T if something changed and had to be compiled.
  (let ((type (component-type component))
	(old-package (package-name *package*)))

    (unwind-protect
	;; Protect old-package.
	(progn
	  ;; Use the correct package.
	  (when (component-package component)
	    (tell-user-generic (format nil "Using package ~A"
				       (component-package component)))
	    (unless *oos-test*
	      (unless (find-package (component-package component))
		;; If the package name is the same as the name of the system,
		;; and the package is not defined, this would lead to an
		;; infinite loop, so bomb out with an error.
		(when (string-equal (string (component-package component))
				    (component-name component))
		  (format t "~%Component ~A not loaded:~%"
			  (component-name component))
		  (error  "  Package ~A is not defined"
			  (component-package component)))
		;; If package not found, try using REQUIRE to load it.
		(new-require (component-package component)))
	      ;; This was USE-PACKAGE, but should be IN-PACKAGE.
	      ;; Actually, CLtL2 lisps define in-package as a macro,
	      ;; so we'll set the package manually.
	      ;; (in-package (component-package component))
	      (let ((package (find-package (component-package component))))
		(when package
		  (setf *package* package)))))

	  ;; Marco Antoniotti 20040609
	  ;; New feature.  Try to FIND-SYSTEM :system components if
	  ;; they have no local :components definition.
	  ;; OPERATE-ON-SYSTEM-DEPENDENCIES should still work as
	  ;; advertised, given the small change made there.

	  (when (or (eq type :system) (eq type :subsystem))
	    (ensure-external-system-def-loaded component))

	  (when (or (eq type :defsystem) (eq type :system))
	    (block skip
	      (handler-bind
		  ((error
		    (lambda (e)
		      (declare (ignorable e))
		      (tell-user-generic
		       (format nil "Some system dependency for ~A failed."
			       (component-name component)))
		      (when (component-non-required-p component)
			(tell-user-generic
			 (format nil "Dependency ignored as ~A in non required."
				 (component-name component)))
			(return-from skip)))))
		(operate-on-system-dependencies component
						operation
						force))))

	  ;; Do any compiler proclamations
	  (when (component-proclamations component)
	    (tell-user-generic (format nil "Doing proclamations for ~A"
				       (component-name component)))
	    (unless *oos-test*
              (proclaim (component-proclamations component))))

	  ;; Do any initial actions
	  (when (component-initially-do component)
	    (tell-user-generic (format nil "Doing initializations for ~A"
				       (component-name component)))
	    (unless *oos-test*
              (with-special-component-vars (component)
                 (let ((initially-do (component-initially-do component)))
                   (if (functionp initially-do)
                       (funcall initially-do)
                       (eval initially-do))))
              ))

	  ;; If operation is :compile and load-only is T, this would change
	  ;; the operation to load. Only, this would mean that a module would
	  ;; be considered to have changed if it was :load-only and had to be
	  ;; loaded, and then dependents would be recompiled -- this doesn't
	  ;; seem right. So instead, we propagate the :load-only attribute
	  ;; to the components, and modify compile-file-operation so that
	  ;; it won't compile the files (and modify tell-user to say "Loading"
	  ;; instead of "Compiling" for load-only modules).
	  #||
	  (when (and (find operation '(:compile compile))
		     (component-load-only component))
	    (setf operation :load))
	  ||#

	  ;; Do operation and set changed flag if necessary.
	  (setq changed
		(case type
		  ((:file :private-file)
		   (progv *file-local-variables*
		       (mapcar #'symbol-value *file-local-variables*)
		     (funcall (component-operation operation) component force)))
		  ((:module :system :subsystem :defsystem)
		   (operate-on-components component operation force changed))))

	  ;; Do any final actions
	  (when (component-finally-do component)
	    (tell-user-generic (format nil "Doing finalizations for ~A"
				       (component-name component)))
	    (unless *oos-test*
              (with-special-component-vars (component)
                 (let ((finally-do (component-finally-do component)))
                   (if (functionp finally-do)
                       (funcall finally-do)
                       (eval finally-do))))
                ))

	  ;; add the banner if needed
	  #+(or cmu scl)
	  (when (component-banner component)
	    (unless (stringp (component-banner component))
	      (error "The banner should be a string, it is: ~S"
	             (component-banner component)))
	    (setf (getf ext:*herald-items*
			(intern (string-upcase  (component-name component))
				(find-package :keyword)))
		  (list
		     (component-banner component)))))

      ;; Reset the package. (Cleanup form of unwind-protect.)
      ;;(in-package old-package)
      (setf *package* (find-package old-package)))

    ;; Provide the loaded system
    (when (or (eq type :defsystem) (eq type :system) (eq type :subsystem))
      (tell-user-generic (format nil "Providing system ~A~%"
				 (component-name component)))
      (or *oos-test*
	  (provide (canonicalize-system-name (component-name component))))))

  ;; Return non-NIL if something changed in this component and hence had
  ;; to be recompiled. This is only used as a boolean.
  changed)

(defvar *force* nil)
(defvar *providing-blocks-load-propagation* t
  "If T, if a system dependency exists on *modules*, it is not loaded.")

(defun operate-on-system-dependencies (component operation &optional force)
  (when *system-dependencies-delayed*
    (let ((*force* force))
      (dolist (system (component-depends-on component))
	;; For each system that this system depends on, if it is a
	;; defined system (either via defsystem or component type :system),
	;; and propagation is turned on, propagates the operation to the
	;; subsystem. Otherwise runs require (my version) on that system
	;; to load it (needed since we may be depending on a lisp
	;; dependent package).
	;; Explores the system tree in a DFS manner.

	;; Do not try to do anything with non system components.
        (cond ((and *operations-propagate-to-subsystems*
                    (not (listp system))
		    (or (stringp system) (symbolp system))
                    ;; The subsystem is a defined system.
                    (find-system system :load-or-nil))
               ;; Call OOS on it. Since *system-dependencies-delayed* is
               ;; T, the :depends-on slot is filled with the names of
               ;; systems, not defstructs.
               ;; Aside from system, operation, force, for everything else
               ;; we rely on the globals.
               (unless (and *providing-blocks-load-propagation*
                            ;; If *providing-blocks-load-propagation* is T,
                            ;; the system dependency must not exist in the
                            ;; *modules* for it to be loaded. Note that
                            ;; the dependencies are implicitly systems.
                            (find operation '(load :load))
                            ;; (or (eq force :all) (eq force t))
                            (find (canonicalize-system-name system)
                                  *modules* :test #'string-equal))

                 (operate-on-system system operation :force force)))

              ((listp system)
               ;; If the SYSTEM is a list then its contents are as follows.
               ;;
               ;;    (<name> &key <definition-pathname> <action>
               ;;                 <version> (<required-p> t)
               ;;

               (destructuring-bind (system-name &key
                                                definition-pathname
                                                action
                                                version
                                                (required-p t))
                   system
                 (tell-user-require-system
                  (if (and (null system-name)
                           (null definition-pathname))
                      action
                      system)
                  component)
                 (handler-case
                     (or *oos-test*
                         (new-require system-name
                                      nil
                                      (eval definition-pathname)
                                      action
                                      (or version *version*)))
                   (error (e)
                     (tell-user-generic
                      (format nil "System dependency ~S for ~A failed."
                              system
                              (component-name component)))
                     (if required-p
                         (error e)
                         (tell-user-generic
                          "Failed dependency ignored as non required."))
                     )
                   #|
                   (error (mce)
                     (when required-p (error mce)))|#
                   )
                 ))

              ((and (component-p system)
                    (not (member (component-type system)
                                 '(:defsystem :subsystem :system))))
               ;; Do nothing for non system components.
               )
              (t
               (tell-user-require-system system component)
               (or *oos-test* (new-require system))))
        ))))


;;; Modules can depend only on siblings. If a module should depend
;;; on an uncle, then the parent module should depend on that uncle
;;; instead. Likewise a module should depend on a sibling, not a niece
;;; or nephew. Modules also cannot depend on cousins. Modules cannot
;;; depend on parents, since that is circular.

(defun module-depends-on-changed (module changed)
  (dolist (dependent (component-depends-on module))
    (when (member dependent changed)
      (return t))))

(defun operate-on-components (component operation force changed)
  (with-tell-user (operation component)
    (if (component-components component)
	(dolist (module (component-components component))
	  (when (operate-on-component module operation
		  (cond ((and (module-depends-on-changed module changed)
			      #||(some #'(lambda (dependent)
					(member dependent changed))
				    (component-depends-on module))||#
			      (or (non-empty-listp force)
				  (eq force :new-source-and-dependents)))
			 ;; The component depends on a changed file
			 ;; and force agrees.
			 (if (eq force :new-source-and-dependents)
			     :new-source-all
			   :all))
			((and (non-empty-listp force)
			      (member (component-name module) force
				      :test #'string-equal :key #'string))
			 ;; Force is a list of modules
			 ;; and the component is one of them.
			 :all)
			(t force)))
	    (push module changed)))
	(case operation
	  ((compile :compile)
	   (with-special-component-vars (component)
             (let ((compile-form (component-compile-form component)))
               (if (functionp compile-form)
	           (funcall compile-form)
	           (eval compile-form)))))
	  ((load :load)
	   (with-special-component-vars (component)
             (let ((load-form (component-load-form component)))
               (if (functionp load-form)
	           (funcall load-form)
                   (eval load-form)))
	     )))))
  ;; This is only used as a boolean.
  changed)

;;; ********************************
;;; New Require ********************
;;; ********************************

;;; This needs cleaning.  Obviously the code is a left over from the
;;; time people did not know how to use packages in a proper way or
;;; CLs were shaky in their implementation.

;;; First of all we need this. (Commented out for the time being)
;;; (shadow '(cl:require))


(defvar *old-require* nil)

;;; All calls to require in this file have been replaced with calls
;;; to new-require to avoid compiler warnings and make this less of
;;; a tangled mess.

(defun new-require (module-name
		    &optional
		    pathname
		    definition-pname
		    default-action
		    (version *version*))
  ;; If the pathname is present, this behaves like the old require.
  (unless (and module-name
	       ;; madhu: Allegro cannot coerce pathnames to strings
	       ;; via (string #p"foo") and module-name turns out to be
	       ;; a pathname when REQUIRE is used internally to load
	       ;; internal modules.
	       #+allegro
	       (and (pathnamep module-name)
		    (setq module-name (namestring module-name)))
	       (find (string module-name)
		     *modules* :test #'string-equal))
    (cond (pathname
           (funcall *old-require* module-name pathname))

          ;; If the system is defined, load it.
          ((find-system module-name :load-or-nil definition-pname)
           (operate-on-system
            module-name :load
            :force *force*
            :version version
            :test *oos-test*
            :verbose *oos-verbose*
            :load-source-if-no-binary *load-source-if-no-binary*
            :bother-user-if-no-binary *bother-user-if-no-binary*
            :compile-during-load *compile-during-load*
            :load-source-instead-of-binary *load-source-instead-of-binary*
            :minimal-load *minimal-load*))

          ;; If there's a default action, do it. This could be a progn which
          ;; loads a file that does everything.
          ((and default-action
                (eval default-action)))

          ;; If no system definition file, try regular require.
          ;; had last arg  PATHNAME, but this wasn't really necessary.
	  ((let ((val #+clozure-common-lisp
		   (handler-case (funcall *old-require* module-name)
		     (ccl::simple-file-error (c)
		       (tell-user-generic (format nil "CCL barfs on ~A: ~A ~ignoring" module-name c))))
		   #-clozure-common-lisp
		   (funcall *old-require* module-name)))
	     (format t "===> MADHU: called ~S on ~S~&===> MADHU: RETURNED ~S~&"
		     *old-require* module-name val)
	     val))

          ;; If no default action, print a warning or error message.
          (t
           #||
	       (format t "~&Warning: System ~A doesn't seem to be defined..."
	               module-name)
	       ||#
           (error 'missing-system :name module-name)))
    ))


;;; Note that in some lisps, when the compiler sees a REQUIRE form at
;;; top level it immediately executes it. This is as if an
;;; (eval-when (compile load eval) ...) were wrapped around the REQUIRE
;;; form. I don't see any easy way to do this without making REQUIRE
;;; a macro.
;;;
;;; For example, in VAXLisp, if a (require 'streams) form is at the top of
;;; a file in the system, compiling the system doesn't wind up loading the
;;; streams module. If the (require 'streams) form is included within an
;;; (eval-when (compile load eval) ...) then everything is OK.
;;;
;;; So perhaps we should replace the redefinition of lisp:require
;;; with the following macro definition:
#||
(unless *old-require*
  (setf *old-require*
	(symbol-function #-(or :lispworks
			       :sbcl
			       (and :excl :allegro-v4.0)) 'lisp:require
			 #+:sbcl 'cl:require
			 #+:lispworks 'system:::require
			 #+(and :excl :allegro-v4.0) 'cltl1:require))

  (let (#+:CCL (ccl:*warn-if-redefine-kernel* nil))
    ;; Note that lots of lisps barf if we redefine a function from
    ;; the LISP package. So what we do is define a macro with an
    ;; unused name, and use (setf macro-function) to redefine
    ;; lisp:require without compiler warnings. If the lisp doesn't
    ;; do the right thing, try just replacing require-as-macro
    ;; with lisp:require.
    (defmacro require-as-macro (module-name
				&optional pathname definition-pname
				default-action (version '*version*))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (new-require ,module-name ,pathname ,definition-pname
		      ,default-action ,version)))
    (setf (macro-function #-(and :excl :sbcl :allegro-v4.0) 'lisp:require
			  #+:sbcl 'cl:require
			  #+(and :excl :allegro-v4.0) 'cltl1:require)
	  (macro-function 'require-as-macro))))
||#
;;; This will almost certainly fix the problem, but will cause problems
;;; if anybody does a funcall on #'require.

;;; Redefine old require to call the new require.
(eval-when #-(or :lucid) (:load-toplevel :execute)
	   #+(or :lucid) (load eval)
 (unless *old-require*
  (setf *old-require*
	(symbol-function
	 #-(or (and :excl :allegro-v4.0)
               :mcl
               :sbcl
               :lispworks
               :clozure-common-lisp
	       :ecl
	       :mkcl
	       :abcl
	       :clasp
	       )
	 'lisp:require
	 #+(and :excl :allegro-v4.0) 'cltl1:require
	 #+:sbcl 'cl:require
	 #+:lispworks3.1 'common-lisp::require
	 #+(and :lispworks (not :lispworks3.1)) 'system::require
	 #+(or :openmcl :clozure-common-lisp)'cl:require
	 #+(and :mcl (not :openmcl)) 'ccl:require
	 #+(or :ecl :mkcl) 'cl:require
	 #+(or :abcl :armedbear) 'cl:require
	 #+(or :clasp) 'cl:require
	 )))

  (unless *dont-redefine-require*
    (let (#+(or :mcl (and :CCL (not :lispworks)))
	  (ccl:*warn-if-redefine-kernel* nil))
      #-(or (and allegro-version>= (version>= 4 1)) :lispworks :ecl :mkcl)
      (setf (symbol-function
	     #-(or (and :excl :allegro-v4.0)
                   :mcl
                   :sbcl
                   :lispworks
                   :clozure-common-lisp
		   :clasp
		   :abcl) 'lisp:require
	     #+(and :excl :allegro-v4.0) 'cltl1:require
	     #+:lispworks3.1 'common-lisp::require
	     #+:sbcl 'cl:require
	     #+(and :lispworks (not :lispworks3.1)) 'system::require
	     #+:openmcl 'cl:require
	     #+(and :mcl (not :openmcl)) 'ccl:require
	     #+(or :abcl :armedbear) 'cl:require
	     #+(or :clasp) 'cl:require
	     )
	    (symbol-function 'new-require))
      #+:mkcl
      (progn
	(si:reopen-package "CL")
	(setf (symbol-function 'cl:require) (symbol-function 'new-require))
	(si:close-package "CL")
	)
      #+:lispworks
      (let ((warn-packs system::*packages-for-warn-on-redefinition*))
	(declare (special system::*packages-for-warn-on-redefinition*))
	(setq system::*packages-for-warn-on-redefinition* nil)
	(setf (symbol-function
	       #+:lispworks3.1 'common-lisp::require
	       #-:lispworks3.1 'system::require
	       )
	      (lambda (&rest args) (apply 'new-require args))
	      #+nil
	      (symbol-function 'new-require))
	(setq system::*packages-for-warn-on-redefinition* warn-packs))
      #+(and allegro-version>= (version>= 4 1))
      (excl:without-package-locks
       (setf (symbol-function 'lisp:require)
	 (symbol-function 'new-require)))
      #+(and :ecl nil)
      (ext:without-package-locks	; not there in 15.3.7
	  (setf (symbol-function 'cl:require) (symbol-function 'new-require)))
      #+ecl
      (progn (ext:package-lock "CL" nil)
	     (setf (symbol-function 'cl:require)
		   (symbol-function 'new-require))
	     (ext:package-lock "CL" t))
 ;;;madhu 190518 FIXME MKCL package-locks
      )))


;;; Well, let's add some more REQUIRE hacking; specifically for SBCL,
;;; and, eventually, for CMUCL.

;;madhu 221124 use a single module provider function for all lisps
(defun mk-defsystem-module-provider (name)
  (let ((module-name (string-downcase (string name))))
    (when (mk:find-system module-name :load-or-nil)
      (mk:load-system module-name
		      :compile-during-load t
		      :verbose nil))))


#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
(pushnew 'mk-defsystem-module-provider sb-ext:*module-provider-functions*))

#+#.(cl:if (cl:and (cl:find-package "EXT") (cl:find-symbol "*MODULE-PROVIDER-FUNCTIONS*" "EXT")) '(and) '(or))
(pushnew 'mk-defsystem-module-provider ext:*module-provider-functions*)

#+:clozure-common-lisp
(pushnew 'mk-defsystem-module-provider ccl:*module-provider-functions*)


#+allegro
(progn
(defvar *allegro-mk-defsystem-module-provider-hack-dummy-load-pathname*
  "/tmp/acldummy.cl")
;; system wide resource
(unless (probe-file *allegro-mk-defsystem-module-provider-hack-dummy-load-pathname*)
  (with-open-file (stream
		   *allegro-mk-defsystem-module-provider-hack-dummy-load-pathname*
		   :direction :output)
    (write-string
";;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;; Dummy file
\(in-package :cl-user)
\(defvar *acldummyloadcount* 0)
\(defvar *acldummyloadmodules* nil)
\(incf *acldummyloadcount*)" stream))))

#+nil
(delete-file *allegro-mk-defsystem-module-provider-hack-dummy-load-pathname*)

#+allegro
(defun mk::allegro-mk-defsystem-module-provider-hack ; madhu 20080504
    (supplied-name search-list order check-bundle check-lower-case)
  ;;(declare (ignore search-list order check-bundle))
  (warn "XXX: ~S."
	(pairlis '(supplied-name search-list order check-bundle check-lower-case)
		 (list supplied-name search-list order check-bundle check-lower-case)))
  (let* ((name (if (pathnamep supplied-name)
		   (namestring supplied-name)
		   (string supplied-name)))
	 (module-name (string-downcase name))
	 (dummy-return (mk::compute-system-path module-name nil)))
    (when (mk:find-system module-name :load-or-nil)
      (mk:operate-on-system module-name :load
			    :force mk::*force*
			    :version mk::*version*
			    :test mk::*oos-test*
			    :verbose mk::*oos-verbose*
			    :load-source-if-no-binary mk::*load-source-if-no-binary*
			    :bother-user-if-no-binary mk::*bother-user-if-no-binary*
			    :compile-during-load mk::*compile-during-load*
			    :load-source-instead-of-binary
			    mk::*load-source-instead-of-binary*
			    :minimal-load mk::*minimal-load*)
      (warn "XXX: Returning: ~S." dummy-return)
      ;; this fails when you do not have a .system file.
      (or dummy-return
	  *allegro-mk-defsystem-module-provider-hack-dummy-load-pathname*))))

#+allegro
(let ((form '(:CALL mk::allegro-mk-defsystem-module-provider-hack)))
  (unless (find form sys::*require-search-list* :test #'equal)
    (setq sys::*require-search-list*
	  (append sys::*require-search-list* (list form)))))

#+mkcl
(pushnew 'mk-defsystem-module-provider mk-ext:*module-provider-functions*)

#+abcl
(pushnew 'mk-defsystem-module-provider system::*module-provider-functions*)

#+clasp
(pushnew 'mk-defsystem-module-provider ext:*module-provider-functions*)


;;; ********************************
;;; Language-Dependent Characteristics
;;; ********************************
;;; This section is used for defining language-specific behavior of
;;; defsystem. If the user changes a language definition, it should
;;; take effect immediately -- they shouldn't have to reload the
;;; system definition file for the changes to take effect.

(defvar *language-table* (make-hash-table :test #'equal)
  "Hash table that maps from languages to language structures.")
(defun find-language (name)
  (gethash name *language-table*))

;;madhu 221205 need an eval-when because of top-level calls to
;;define-language in this file
(eval-when (:load-toplevel :execute :compile-toplevel)
(defstruct (language (:print-function print-language))
  name			; The name of the language (a keyword)
  compiler		; The function used to compile files in the language
  loader		; The function used to load files in the language
  source-extension	; Filename extensions for source files
  binary-extension	; Filename extensions for binary files

  output-files		; Function that produces a list of output
			; files produced by the compiler, which can be
			; cleaned up.
))

(defun print-language (language stream depth)
  (declare (ignore depth))
  (format stream "#<~:@(~A~): ~A ~A>"
          (language-name language)
          (language-source-extension language)
	  (language-binary-extension language)))

(defun compile-function (component)
  (or (component-compiler component)
      (let ((language (find-language (or (component-language component)
					 :lisp))))
	(when language (language-compiler language)))
      #'compile-file))

(defun load-function (component)
  (or (component-loader component)
      (let ((language (find-language (or (component-language component)
					 :lisp))))
	(when language (language-loader language)))
      #'load))

(defun default-source-extension (component)
  (let ((language (find-language (or (component-language component)
				     :lisp))))
    (or (when language (language-source-extension language))
	(car *filename-extensions*))))

(defun default-binary-extension (component)
  (let ((language (find-language (or (component-language component)
				     :lisp))))
    (or (when language (language-binary-extension language))
	(cdr *filename-extensions*))))

(eval-when (:load-toplevel :execute :compile-toplevel)
(defmacro define-language (name
                           &key
                           compiler
                           loader
                           (source-extension (car *filename-extensions*))
                           (binary-extension (cdr *filename-extensions*))
			   output-files
                           )
  (let ((language (gensym "LANGUAGE")))
    `(let ((,language (make-language :name ,name
				     :compiler ,compiler
				     :loader ,loader
				     :source-extension ,source-extension
				     :binary-extension ,binary-extension
				     :output-files ,output-files)))
       (setf (gethash ,name *language-table*) ,language)
       ,name))))

#||
;;; Test System for verifying multi-language capabilities.
(defsystem foo
  :language :lisp
  :components ((:module c :language :c :components ("foo" "bar"))
	       (:module lisp :components ("baz" "barf"))))

||#

;;; *** Lisp Language Definition
(define-language :lisp
  :compiler #-ecl #'compile-file #+ecl 'compile-file
  :source-extension (car *filename-extensions*)
  :binary-extension (cdr *filename-extensions*))

;;; *** PseudoScheme Language Definition
(defun scheme-compile-file (filename &rest args)
  (let ((scheme-package (find-package '#:scheme)))
    (apply (symbol-function (find-symbol (symbol-name 'compile-file)
					 scheme-package))
	   filename
	   (funcall (symbol-function
		     (find-symbol (symbol-name '#:interaction-environment)
				  scheme-package)))
	   args)))

(define-language :scheme
  :compiler #'scheme-compile-file
  :loader #'load
  :source-extension "scm"
  :binary-extension "bin")

;;; *** C Language Definition

;;; This is very basic. Somebody else who needs it can add in support
;;; for header files, libraries, different C compilers, etc. For example,
;;; we might add a COMPILER-OPTIONS slot to the component defstruct.

(defparameter *c-compiler* "gcc")
#-(or symbolics (and :lispworks :harlequin-pc-lisp ))

(defun run-unix-program (program arguments)
  ;; arguments should be a list of strings, where each element is a
  ;; command-line option to send to the program.
  #+:lucid (run-program program :arguments arguments)
  #+:allegro (excl:run-shell-command
	      (format nil "~A~@[ ~{~A~^ ~}~]"
		      program arguments))
  #+(or :kcl :ecl :mkcl :clasp) (system (format nil "~A~@[ ~{~A~^ ~}~]" program arguments))
  #+(or :cmu :scl) (extensions:run-program program arguments)
  #+:openmcl (ccl:run-program program arguments)
  #+:sbcl (sb-ext:run-program program arguments)
  #+:lispworks (system:call-system-showing-output
		(format nil "~A~@[ ~{~A~^ ~}~]" program arguments))
  #+clisp (#+lisp=cl ext:run-program #-lisp=cl lisp:run-program
                     program :arguments arguments)
  #+abcl (ext:run-shell-command cmd-string)
  #+clasp (ext:run-program program arguments :wait nil)
  )

#+(or symbolics (and :lispworks :harlequin-pc-lisp))
(defun run-unix-program (program arguments)
  (declare (ignore program arguments))
  (error "MK::RUN-UNIX-PROGRAM: this does not seem to be a UN*X system.")
  )


;;; This is inspired by various versions - all very UNIX/Linux
;;; dependent - appearing in ASDF and UFFI.  The original versions and Copyrights
;;; are by Dan Barlow, Kevin Rosenberg and many others.
;;; This version should be more liberal.

(defvar *default-shell* "/bin/sh")

#+(or windows ms-windows win32)
(eval-when (:load-toplevel :execute)
  ;; Lets assume a "standard" Cygwin installation.
  (if (probe-file (pathname "C:\\cygwin\\bin\\sh.exe"))
      (setf *default-shell* "C:\\cygwin\\bin\\sh.exe")
      (setf *default-shell* nil)))


(defun run-shell-command (command-control-string
                          arguments
                          &key
                          (output *trace-output*)
                          (shell *default-shell*)
                          )
   "Executes a shell 'command' in an underlying process.
RUN-SHELL-COMMAND interpolate ARGS into CONTROL-STRING as if by FORMAT, and
synchronously execute the result using a Bourne-compatible shell, with
output to *trace-output*.  Returns the shell's exit code."

   (declare (ignorable shell))

  (let ((command (apply #'format nil command-control-string arguments)))
    #+sbcl
    (sb-impl::process-exit-code
     (sb-ext:run-program shell
                         (list "-c" command)
                         :input nil
                         :output output))
    
    #+(or cmu scl)
    (ext:process-exit-code
     (ext:run-program shell
                      (list "-c" command)
                      :input nil
                      :output output))

    #+(and mkcl (not windows))
    (nth-value 2 (mkcl:run-program shell (list "-c" command) :input nil :output output))

    #+(and mkcl windows)
    (nth-value 2 (mkcl:run-program "cmd" (list "/c" command) :input nil :output output))

    #+allegro
    (let (process-output ignored process-pid
			 exit-code ignored pid signal)
      (declare (ignorable ignored pid signal))
      (if (and (streamp output)  (excl.osi::stream-to-fd output))
	  (multiple-value-setq (exit-code)
	    (excl:run-shell-command command :input nil
				    :output output
				    :error-output :output
				    :wait t))
	  (unwind-protect
	       (progn
		 (multiple-value-setq (process-output ignored process-pid)
		   (excl:run-shell-command command :input nil
					   :output :stream
					   :error-output :output
					   :wait nil))
		 (when process-pid
		   (multiple-value-setq (exit-code pid signal)
		     (sys:reap-os-subprocess :pid process-pid :wait t))
		   (loop for line = (read-line process-output nil)
			 while line do (write-line line output)))
		 exit-code)
	    (when process-output (ignore-errors (close process-output))))))

    #+(and lispworks win32)
    (system:call-system-showing-output (format nil "cmd /c ~A" command)
				       :show-cmd nil :prefix ""
                                       :output-stream output)

    #+(and lispworks (not win32))
    (system:call-system-showing-output command
                                       :shell-type shell
				       :show-cmd nil :prefix ""
                                       :output-stream output)

    #+clisp
    (let (input)
      (unwind-protect
	   (progn
	     (setq input (posix:mkstemp (format nil "~A/~A" (or (ext:getenv #+windows "TEMP" #+unix "TMPDIR")  "/tmp") (gensym))
					:direction :input))
	     (let* ((cmd (format nil "~A >> ~A" command (truename input)))
		    (ret  (ext:run-program shell :arguments `("-c" ,cmd) :wait t :output :terminal))
		    (block-size 4096)
		    (buffer (make-array block-size :element-type (stream-element-type input)))
		    count
		    (actual-block-size (length buffer)))
	       (loop for read-limit = (if count
					  (let ((num (- count total-bytes-written)))
					    (assert (>= num 0))
					    (if (zerop num)
						(loop-finish)
						(min actual-block-size num)))
					  actual-block-size)
		     for bytes-read = (read-sequence buffer input :end read-limit)
		     do (write-sequence buffer output :end bytes-read)
		     until (< bytes-read read-limit)
		     summing bytes-read into total-bytes-written
		     finally (return total-bytes-written))
	       (if ret (if (minusp ret) (- 128 ret) ret) 0)))
	(and input (delete-file input))))

    #+openmcl
    (nth-value 1
	       (ccl:external-process-status
		(ccl:run-program shell
                                 (list "-c" command)
				 :input nil
                                 :output output
				 :wait t)))

    #+ecl (nth-value 1 (ext:run-program shell (list "-c" command) :input nil :output output :error output))

    #+clasp
    (let (pid2 status2 code2)
      (multiple-value-bind (stream code proc)
	  (ext:run-program shell (list "-c" command) :input nil :output :stream :error :output :wait nil)
	(when proc
	  (handler-case
	      (multiple-value-setq (status2 code2 pid2)
		(si:waitpid (ext:external-process-pid proc) t))
	    ;; ;madhu 230111 ext:;external-process-wait always throws
	    ;; an exception in clasp-2.0, but si:waitpid seems to work
	    (ext:segmentation-violation (e)
	      (warn "Handling ~S during external-process-wait on pid ~A"
		    e
		    (ext:external-process-pid proc))))
	  (when stream
	    (unwind-protect
		 (progn (loop for line = (read-line stream nil) while line
			      do (write-line line output))
			code2)
	      (ignore-errors (close stream)))))))

    #+abcl (ext:run-shell-command command) ; Just a place holder FTTB.

    #-(or ecl openmcl clisp lispworks allegro mkcl scl cmu sbcl clasp)
    (error "RUN-SHELL-PROGRAM not implemented for this Lisp")
    ))

#||
(with-output-to-string (stream)
  (run-shell-command "ls /dev/shm/" nil :output stream))
(with-output-to-string (*standard-output*)
  (run-shell-command "ls /dev/shm/" nil :output *standard-output*))
||#

#||
(defun c-compile-file (filename &rest args &key output-file error-file)
  ;; gcc -c foo.c -o foo.o
  (declare (ignore args))
  (run-unix-program *c-compiler*
		    (format nil "-c ~A~@[ -o ~A~]"
			    filename
			    output-file)))
||#

#||
(defun c-compile-file (filename &rest args &key output-file error-file)
  ;; gcc -c foo.c -o foo.o
  (declare (ignore args error-file))
  (run-unix-program *c-compiler*
		    `("-c" ,filename ,@(if output-file `("-o" ,output-file)))))
||#


;;; The following code was inserted to improve C compiler support (at
;;; least under Linux/GCC).
;;; Thanks to Espen S Johnsen.
;;;
;;; 20001118 Marco Antoniotti.

(defun default-output-pathname (path1 path2 type)
  (if (eq path1 t)
      (translate-logical-pathname
       (merge-pathnames (make-pathname :type type) (pathname path2)))
      (translate-logical-pathname (pathname path1))))


(defun run-compiler (program
		     arguments
		     output-file
		     error-file
		     error-output
		     verbose)
  #-(or cmu scl) (declare (ignore error-file error-output))

  (flet ((make-useable-stream (&rest streams)
	   (apply #'make-broadcast-stream (delete nil streams)))
	 )
    (let (#+(or cmu scl) (error-file error-file)
	  #+(or cmu scl) (error-file-stream nil)
	  (verbose-stream nil)
	  (old-timestamp (file-write-date output-file))
	  (fatal-error nil)
	  (output-file-written nil)
	  )
      (unwind-protect
	   (progn
	     #+(or cmu scl)
	     (setf error-file
		   (when error-file
		     (default-output-pathname error-file
			                      output-file
                     		              *compile-error-file-type*))

		   error-file-stream
		   (and error-file
			(open error-file
			      :direction :output
			      :if-exists :supersede)))

	     (setf verbose-stream
		   (make-useable-stream
		    #+cmu error-file-stream
		    (and verbose *trace-output*)))

	     (format verbose-stream "Running ~A~@[ ~{~A~^ ~}~]~%"
		     program
		     arguments)

	     (setf fatal-error
		   #-(or cmu scl)
		   (and (run-unix-program program arguments) nil) ; Incomplete.
		   #+(or cmu scl)
		   (let* ((error-output
			   (make-useable-stream error-file-stream
						(if (eq error-output t)
						    *error-output*
						  error-output)))
			  (process
			   (ext:run-program program arguments
					    :error error-output)))
		     (not (zerop (ext:process-exit-code process)))))

	     (setf output-file-written
		   (and (probe-file output-file)
			(not (eql old-timestamp
				  (file-write-date output-file)))))


	     (when output-file-written
	       (format verbose-stream "~A written~%" output-file))
	     (format verbose-stream "Running of ~A finished~%"
		     program)
	     (values (and output-file-written output-file)
		     fatal-error
		     fatal-error))

	#+(or cmu scl)
	(when error-file
	  (close error-file-stream)
	  (unless (or fatal-error (not output-file-written))
	    (delete-file error-file)))

	(values (and output-file-written output-file)
		fatal-error
		fatal-error)))))


;;; C Language definitions.

(defun c-compile-file (filename &rest args
				&key
				(output-file t)
				(error-file t)
				(error-output t)
				(verbose *compile-verbose*)
				debug
				link
				optimize
				cflags
				definitions
				include-paths
				library-paths
				libraries
				(error t))
  (declare (ignore args))

  (flet ((map-options (flag options &optional (func #'identity))
	   (mapcar #'(lambda (option)
		       (format nil "~A~A" flag (funcall func option)))
		   options))
	 )
    (let* ((output-file (default-output-pathname output-file filename "o"))
	   (arguments
	    `(,@(when (not link) '("-c"))
	      ,@(when debug '("-g"))
	      ,@(when optimize (list (format nil "-O~D" optimize)))
	      ,@cflags
	      ,@(map-options
		 "-D" definitions
		 #'(lambda (definition)
		     (if (atom definition)
			 definition
		       (apply #'format nil "~A=~A" definition))))
	      ,@(map-options "-I" include-paths #'truename)
	      ,(namestring (truename filename))
	      "-o"
	      ,(namestring (translate-logical-pathname output-file))
	      ,@(map-options "-L" library-paths #'truename)
	      ,@(map-options "-l" libraries))))

      (multiple-value-bind (output-file warnings fatal-errors)
	  (run-compiler *c-compiler*
			arguments
			output-file
			error-file
			error-output
			verbose)
	(if (and error (or (not output-file) fatal-errors))
	    (error "Compilation failed")
	    (values output-file warnings fatal-errors))))))


(define-language :c
  :compiler #'c-compile-file
  :loader #+:lucid #'load-foreign-files
          #+:allegro #'load
          #+(or :cmu :scl) #'alien:load-foreign
          #+:sbcl #'sb-alien:load-foreign
	  #+(and :lispworks :unix (not :linux) (not :macosx)) #'link-load:read-foreign-modules
	  #+(and :lispworks :unix (or :linux :macosx)) #'fli:register-module
	  #+(and :lispworks :win32) #'fli:register-module
          #+(or :ecl :gcl :kcl :mkcl) #'load ; should be enough.
          #-(or :lucid
		:allegro
		:cmu
		:sbcl
		:scl
		:lispworks
		:ecl :gcl :kcl :mkcl)
	  (lambda (&rest args)
	    (declare (ignore args))
	    (cerror "Continue returning NIL."
		    "Loader not defined for C foreign libraries in ~A ~A."
		    (lisp-implementation-type)
		    (lisp-implementation-version)))
  :source-extension "c"
  :binary-extension "o")


;;; Fortran Language definitions.
;;; From Matlisp.

(export '(*fortran-compiler* *fortran-options*))

(defparameter *fortran-compiler* "g77")
(defparameter *fortran-options* '("-O"))

(defun fortran-compile-file (filename &rest args
				      &key output-file error-file
				      &allow-other-keys)
  (declare (ignore error-file args))
  (let ((arg-list
	 (append *fortran-options*
		 `("-c" ,filename ,@(if output-file `("-o" ,output-file))))))
    (run-unix-program *fortran-compiler* arg-list)))


(define-language :fortran
    :compiler #'fortran-compile-file
    :loader #'identity
    :source-extension "f"
    :binary-extension "o")


;;; AR support.
;; How to create a library (archive) of object files

(export '(*ar-program* build-lib))

(defparameter *ar-program* "ar")

(defun build-lib (libname directory)
  (let ((args (list "rv" (truename libname))))
    (format t ";;; Building archive ~A~%" libname)
    (run-unix-program *ar-program*
		      (append args
			      (mapcar #'truename (directory directory))))))


;;; ********************************
;;; Component Operations ***********
;;; ********************************
;;; Define :compile/compile and :load/load operations
(eval-when (:load-toplevel :execute)
(component-operation :compile  'compile-and-load-operation)
(component-operation 'compile  'compile-and-load-operation)
(component-operation :load     'load-file-operation)
(component-operation 'load     'load-file-operation)
(component-operation 'compile-only 'compile-file-operation)
(component-operation :compile-only 'compile-file-operation)
)


(defun compile-and-load-operation (component force)
  ;; FORCE was CHANGED. this caused defsystem during compilation to only
  ;; load files that it immediately compiled.
  (let ((changed (compile-file-operation component force)))
    ;; Return T if the file had to be recompiled and reloaded.
    (if (and changed (component-compile-only component))
	;; For files which are :compile-only T, compiling the file
	;; satisfies the need to load.
	changed
	;; If the file wasn't compiled, or :compile-only is nil,
	;; check to see if it needs to be loaded.
	(and (load-file-operation component force) ; FORCE was CHANGED ???
	     changed))))


(defun unmunge-lucid (namestring)
  ;; Lucid's implementation of COMPILE-FILE is non-standard, in that
  ;; when the :output-file is a relative pathname, it tries to munge
  ;; it with the directory of the source file. For example,
  ;; (compile-file "src/globals.lisp" :output-file "bin/globals.sbin")
  ;; tries to stick the file in "./src/bin/globals.sbin" instead of
  ;; "./bin/globals.sbin" like any normal lisp. This hack seems to fix the
  ;; problem. I wouldn't have expected this problem to occur with any
  ;; use of defsystem, but some defsystem users are depending on
  ;; using relative pathnames (at least three folks reported the problem).
  (cond ((null-string namestring) namestring)
	((char= (char namestring 0) #\/)
	 ;; It's an absolute namestring
	 namestring)
	(t
	 ;; Ugly, but seems to fix the problem.
	 (concatenate 'string "./" namestring))))

#+gcl
(defun ensure-directories-exist (arg0 &key verbose)
  (declare (ignore arg0 verbose))
  ())

(defun compile-file-operation--internal (component force)
  ;; Returns T if the file had to be compiled.
  (let* ((must-compile
	 ;; For files which are :load-only T, loading the file
	 ;; satisfies the demand to recompile.
	 (and (null (component-load-only component)) ; not load-only
	      (or (find force '(:all :new-source-all t) :test #'eq)
		  (and (find force '(:new-source :new-source-and-dependents)
			     :test #'eq)
		       (needs-compilation component nil)))))
	(source-pname (component-full-pathname component :source))
	(source-exists (probe-file source-pname)))

    (cond ((or (and (not source-exists) (component-language component) t) ;madhu 230129
	       (and must-compile source-exists))
	   (with-tell-user ("Compiling source" component :source)
	     (let ((output-file
		    #+:lucid
		     (unmunge-lucid (component-full-pathname component
							     :binary))
		     #-:lucid
		     (component-full-pathname component :binary)))

	       ;; make certain the directory we need to write to
	       ;; exists [pvaneynd@debian.org 20001114]
	       ;; Added PATHNAME-HOST following suggestion by John
	       ;; DeSoi [marcoxa@sourceforge.net 20020529]

	       (ensure-directories-exist
		(make-pathname
		 :host (pathname-host output-file)
		 :directory (pathname-directory output-file)))

	       (or *oos-test*
		   (apply (compile-function component)
			  source-pname
			  :output-file
			  output-file

			  #+(or :cmu :scl)
			  :error-file

			  #+(or :cmu :scl)
			  (and *cmu-errors-to-file*
			       (component-full-pathname component :error))

			  #+cmu
			  :error-output
			  #+cmu
			  *cmu-errors-to-terminal*

			  (component-compiler-options component)
			  ))))
	   must-compile)
	  (must-compile
	   (tell-user "Source file not found. Not compiling"
		      component :source :no-dots :force)
	   nil)
	  (t nil))))

#+(or ecl mkcl)
(defun compile-file-operation--ecl (component force)
  "Call compile-file-operation with :system-p t to produce a .o file"
  (let* ((source-pname (component-full-pathname component :source))
	 (output-file (ecl-munge-o (component-full-pathname component :binary)))
	 (must-compile (and (null (component-load-only component)) ; not load-only
			    (or (find force '(:all :new-source-all t) :test #'eq)
				(and (find force '(:new-source :new-source-and-dependents)
					   :test #'eq)
				     (and (probe-file source-pname)
					  (or (null (probe-file output-file))
						(< (file-write-date output-file)
						   (file-write-date source-pname)))))))))

    (cond ((and must-compile (probe-file source-pname))
	   (with-tell-user ("Compiling source" component :source)
	       (ensure-directories-exist
		(make-pathname
		 :host (pathname-host output-file)
		 :directory (pathname-directory output-file)))

	       (or *oos-test*
		   (apply (compile-function component)
			  source-pname
			  :output-file
			  output-file

			  #+(or :cmu :scl)
			  :error-file

			  #+(or :cmu :scl)
			  (and *cmu-errors-to-file*
			       (component-full-pathname component :error))

			  #+cmu
			  :error-output
			  #+cmu
			  *cmu-errors-to-terminal*

			  ;madhu 190517 - mkcl has no system-p
			  #+ecl :system-p #+ecl t
			  #+mkcl :fasl-p #+mkcl nil

			  (component-compiler-options component)
			  ))
	   must-compile))
	  (must-compile
	   (tell-user "Source file not found. Not compiling"
		      component :source :no-dots :force)
	   nil)
	  (t nil))))

(defun compile-file-operation (component force)
  (compile-file-operation--internal component force)
  ;; ;madhu 170729 ecl: additionally compile a ".o"
  #+(or ecl mkcl)
  (if *ecl-compile-file-system-p*
      (compile-file-operation--ecl component force)))

;;; compiled-file-p --
;;; See CLOCC/PORT/sys.lisp:compiled-file-p

(eval-when (:load-toplevel :execute :compile-toplevel)
  (when (find-package "PORT")
    (import (find-symbol "COMPILED-FILE-P" "PORT"))))

(unless (fboundp 'compiled-file-p)
  (defun compiled-file-p (file-name)
    "Return T if the FILE-NAME is a filename designator for a valid compiled.
Signal an error when it is not a filename designator.
Return NIL when the file does not exist, or is not readable,
or does not contain valid compiled code."
    #+clisp
    (with-open-file (in file-name :direction :input :if-does-not-exist nil)
      (handler-bind ((error (lambda (c) (declare (ignore c))
				    (return-from compiled-file-p nil))))
	(and in (char= #\( (peek-char nil in nil #\a))
	     (let ((form (read in nil nil)))
	       (and (consp form)
		    (eq (car form) 'SYSTEM::VERSION)
		    (null (eval form)))))))
    #-clisp (declare (ignorable file-name))
    #-clisp t))


(defun needs-compilation (component force)
  ;; If there is no binary, or it is older than the source
  ;; file, then the component needs to be compiled.
  ;; Otherwise we only need to recompile if it depends on a file that changed.
  (declare (ignore force))
  (let ((source-pname (component-full-pathname component :source))
        (binary-pname (component-full-pathname component :binary)))
    (and
     ;; source must exist
     (probe-file source-pname)
     (or
      ;; We force recompilation.
      #|(find force '(:all :new-source-all) :test #'eq)|#
      ;; no binary
      (null (probe-file binary-pname))
      ;; old binary
      (< (file-write-date binary-pname)
         (file-write-date source-pname))
      ;; invalid binary
      (not (compiled-file-p binary-pname))))))


(defun needs-loading (component &optional (check-source t) (check-binary t))
  ;; Compares the component's load-time against the file-write-date of
  ;; the files on disk.
  (let ((load-time (component-load-time component))
	(source-pname (component-full-pathname component :source))
	(binary-pname (component-full-pathname component :binary)))
    (or
     #|| ISI Extension ||#
     (component-load-always component)

     ;; File never loaded.
     (null load-time)
     ;; Binary is newer.
     (when (and check-binary
		(probe-file binary-pname))
       (< load-time
	  (file-write-date binary-pname)))
     ;; Source is newer.
     (when (and check-source
		(probe-file source-pname))
       (< load-time
	  (file-write-date source-pname))))))

;;; Need to completely rework this function...
(defun load-file-operation (component force)
  ;; Returns T if the file had to be loaded
  (let* ((binary-pname (component-full-pathname component :binary))
	 (source-pname (component-full-pathname component :source))
	 (binary-exists (probe-file binary-pname))
	 (source-exists (probe-file source-pname))
	 (source-needs-loading (needs-loading component t nil))
	 (binary-needs-loading (needs-loading component nil t))
	 ;; needs-compilation has an implicit source-exists in it.
	 (needs-compilation (if (component-load-only component)
				source-needs-loading
				(or (and (not source-exists) (component-language component) t) ;madhu 230129
				    (needs-compilation component force))))
	 (check-for-new-source
	  ;; If force is :new-source*, we're checking for files
	  ;; whose source is newer than the compiled versions.
	  (find force '(:new-source :new-source-and-dependents :new-source-all)
		:test #'eq))
	 (load-binary (or (find force '(:all :new-source-all t) :test #'eq)
			  binary-needs-loading))
	 (load-source
	  (or *load-source-instead-of-binary*
	      (and load-binary (component-load-only component))
	      (and check-for-new-source needs-compilation)))
	 (compile-and-load
	  (and needs-compilation
               (or load-binary check-for-new-source)
	       (compile-and-load-source-if-no-binary component)))
         )
    ;; When we're trying to minimize the files loaded to only those
    ;; that need be, restrict the values of load-source and load-binary
    ;; so that we only load the component if the files are newer than
    ;; the load-time.
    (when (and *minimal-load*
               (not (find force '(:all :new-source-all)
		          :test #'eq)))
      (when load-source (setf load-source source-needs-loading))
      (when load-binary (setf load-binary binary-needs-loading)))

    (when (or load-source load-binary compile-and-load)
      (cond (compile-and-load
	     ;; If we're loading the binary and it is old or nonexistent,
	     ;; and the user says yes, compile and load the source.
	     (compile-file-operation component t)
	     (with-tell-user ("Loading binary"   component :binary)
	       (or *oos-test*
		   (progn
		     (funcall (load-function component) binary-pname)
		     (setf (component-load-time component)
			   (ignore-errors;madhu 190627 MKCL static-file signals
			     (file-write-date binary-pname))))))
	     t)
	    ((and source-exists
		  (or (and load-source	; implicit needs-comp...
			   (or *load-source-instead-of-binary*
			       (component-load-only component)
			       (not *compile-during-load*)))
		      (and load-binary
                           (not binary-exists)
			   (load-source-if-no-binary component))))
	     ;; Load the source if the source exists and:
	     ;;   o  we're loading binary and it doesn't exist
	     ;;   o  we're forcing it
	     ;;   o  we're loading new source and user wasn't asked to compile
	     (with-tell-user ("Loading source" component :source)
	       (or *oos-test*
		   (progn
		     (funcall (load-function component) source-pname)
		     (setf (component-load-time component)
			   (file-write-date source-pname)))))
	     t)
	    ((and binary-exists load-binary)
	     (with-tell-user ("Loading binary"   component :binary)
	       (or *oos-test*
		   (progn
		     (funcall (load-function component) binary-pname)
		     (setf (component-load-time component)
			   (file-write-date binary-pname)))))
	     t)
	    ((and (not binary-exists) (not source-exists))
	     (tell-user-no-files component :force)
	     (when *files-missing-is-an-error*
	       (cerror "Continue, ignoring missing files."
		       "~&Source file ~S ~:[and binary file ~S ~;~]do not exist."
		       source-pname
		       (or *load-source-if-no-binary*
			   *load-source-instead-of-binary*)
		       binary-pname))
	     nil)
	    (t
	     nil)))))

(eval-when (:load-toplevel :execute)
(component-operation :clean    'delete-binaries-operation)
(component-operation 'clean    'delete-binaries-operation)
(component-operation :delete-binaries     'delete-binaries-operation)
(component-operation 'delete-binaries     'delete-binaries-operation)
)

#+nil
(component-language (find-component ':cffi-libffi  '("libffi" "libffi-types")))

#+clisp
(defun delete-binaries-clisp-output-files (c)
  (let* ((binary-pname (component-full-pathname c :binary))
	 (lib (make-pathname :type "lib" :defaults binary-pname))
	 (cfp (make-pathname :type "cfp" :defaults binary-pname)))
    (list lib cfp)))

(defun delete-binaries-c-shared-objects (c)
  (let* ((binary-pname (component-full-pathname c :binary))
	 (o (ecl-munge-o binary-pname)))
    (list o)))

(defun delete-binaries-compute-output-files (c)
  (append
   #+clisp
   (delete-binaries-clisp-output-files c)
   #+(or mkcl ecl)
   (delete-binaries-c-shared-objects c)
   (let (lang lang-name output-files-function output-files)
     (when (and (setq lang-name (component-language c))
		(setq lang (find-language lang-name))
		(setq output-files-function (language-output-files lang))
		(setq output-files (funcall output-files-function c)))
       output-files))))

(defun delete-binaries-operation (component force)
  (when (or (eq force :all)
	    (eq force t)
	    (and (find force '(:new-source :new-source-and-dependents
					   :new-source-all)
		       :test #'eq)
		 (needs-compilation component nil)))
    (let ((binary-pname (component-full-pathname component :binary)))
      (when (probe-file binary-pname)
	(with-tell-user ("Deleting binary"   component :binary)
			(or *oos-test*
			    (delete-file binary-pname)))))
    (dolist (f (delete-binaries-compute-output-files component))
      (when (probe-file f)
	(with-tell-user ("Deleting output files" component :binary)
			(or *oos-test*
			    (delete-file f)))))))

;; when the operation = :compile, we can assume the binary exists in test mode.
;;	((and *oos-test*
;;	      (eq operation :compile)
;;	      (probe-file (component-full-pathname component :source)))
;;	 (with-tell-user ("Loading binary"   component :binary)))

(defun binary-exists (component)
  (probe-file (component-full-pathname component :binary)))

;;; or old-binary
(defun compile-and-load-source-if-no-binary (component)
  (when (not (or *load-source-instead-of-binary*
		 (and *load-source-if-no-binary*
		      (not (binary-exists component)))))
    (cond ((component-load-only component)
	   #||
	   (let ((prompt (prompt-string component)))
	     (format t "~A- File ~A is load-only, ~
                        ~&~A  not compiling."
		     prompt
		     (component-full-pathname component :source)
		     prompt))
	   ||#
	   nil)
	  ((eq *compile-during-load* :query)
	   (let* ((prompt (prompt-string component))
		  (compile-source
		   (y-or-n-p-wait
		    #\y 30
		    "~A- Binary file ~A is old or does not exist. ~
                     ~&~A  Compile (and load) source file ~A instead? "
		    prompt
		    (component-full-pathname component :binary)
		    prompt
		    (component-full-pathname component :source))))
	     (unless (y-or-n-p-wait
		      #\y 30
		      "~A- Should I bother you if this happens again? "
		      prompt)
	       (setq *compile-during-load*
		     (y-or-n-p-wait
		      #\y 30
		      "~A- Should I compile while loading the system? "
		      prompt)))		; was compile-source, then t
	     compile-source))
	  (*compile-during-load*)
	  (t nil))))

(defun load-source-if-no-binary (component)
  (and (not *load-source-instead-of-binary*)
       (or (and *load-source-if-no-binary*
		(not (binary-exists component)))
	   (component-load-only component)
	   (when *bother-user-if-no-binary*
	     (let* ((prompt (prompt-string component))
		    (load-source
		     (y-or-n-p-wait #\y 30
		      "~A- Binary file ~A does not exist. ~
                       ~&~A  Load source file ~A instead? "
		      prompt
		      (component-full-pathname component :binary)
		      prompt
		      (component-full-pathname component :source))))
	       (setq *bother-user-if-no-binary*
		     (y-or-n-p-wait #\n 30
		      "~A- Should I bother you if this happens again? "
		      prompt ))
	       (unless *bother-user-if-no-binary*
		 (setq *load-source-if-no-binary* load-source))
	       load-source)))))

;;; ********************************
;;; Allegro Toplevel Commands ******
;;; ********************************
;;; Creates toplevel command aliases for Allegro CL.
#+:allegro
(top-level:alias ("compile-system" 8)
  (system &key force (minimal-load mk:*minimal-load*)
	  test verbose version)
  "Compile the specified system"

  (mk:compile-system system :force force
		     :minimal-load minimal-load
		     :test test :verbose verbose
		     :version version))

#+:allegro
(top-level:alias ("load-system" 5)
  (system &key force (minimal-load mk:*minimal-load*)
	  (compile-during-load mk:*compile-during-load*)
	  test verbose version)
  "Compile the specified system"

  (mk:load-system system :force force
		  :minimal-load minimal-load
		  :compile-during-load compile-during-load
		  :test test :verbose verbose
		  :version version))

#+:allegro
(top-level:alias ("show-system" 5) (system)
  "Show information about the specified system."

  (mk:describe-system system))

#+:allegro
(top-level:alias ("describe-system" 9) (system)
  "Show information about the specified system."

  (mk:describe-system system))

#+:allegro
(top-level:alias ("system-source-size" 9) (system)
  "Show size information about source files in the specified system."

  (mk:system-source-size system))

#+:allegro
(top-level:alias ("clean-system" 6)
  (system &key force test verbose version)
  "Delete binaries in the specified system."

  (mk:clean-system system :force force
		   :test test :verbose verbose
		   :version version))

#+:allegro
(top-level:alias ("edit-system" 7)
  (system &key force test verbose version)
  "Load system source files into Emacs."

  (mk:edit-system system :force force
		  :test test :verbose verbose
		  :version version))

#+:allegro
(top-level:alias ("hardcopy-system" 9)
  (system &key force test verbose version)
  "Hardcopy files in the specified system."

  (mk:hardcopy-system system :force force
		      :test test :verbose verbose
		      :version version))

#+:allegro
(top-level:alias ("make-system-tag-table" 13) (system)
  "Make an Emacs TAGS file for source files in specified system."

  (mk:make-system-tag-table system))


;;; ********************************
;;; Allegro Make System Fasl *******
;;; ********************************
#+:excl
(defun allegro-make-system-fasl (system destination
					&optional (include-dependents t))
  (excl:shell
   (format nil "rm -f ~A; cat~{ ~A~} > ~A"
	   destination
	   (if include-dependents
	       (files-in-system-and-dependents system :all :binary)
	       (files-in-system system :all :binary))
	   destination)))

(defun files-which-need-compilation (system)
  (mapcar #'(lambda (comp) (component-full-pathname comp :source))
	  (remove nil
		  (file-components-in-component
		   (find-system system :load) :new-source))))

(defun files-in-system-and-dependents (name &optional (force :all)
					    (type :source) version)
  ;; Returns a list of the pathnames in system and dependents in load order.
  (let ((system (find-system name :load)))
    (multiple-value-bind (*version-dir* *version-replace*)
	(translate-version version)
      (let ((*version* version))
	(let ((result (file-pathnames-in-component system type force)))
	  (dolist (dependent (reverse (component-depends-on system)))
	    (setq result
		  (append (files-in-system-and-dependents dependent
							  force type version)
			  result)))
	  result)))))

(defun files-in-system (name &optional (force :all) (type :source) version)
  ;; Returns a list of the pathnames in system in load order.
  (let ((system (if (and (component-p name)
                         (member (component-type name) '(:defsystem :system :subsystem)))
                    name
                    (find-system name :load))))
    (multiple-value-bind (*version-dir* *version-replace*)
	(translate-version version)
      (let ((*version* version))
	(file-pathnames-in-component system type force)))))

(defun file-pathnames-in-component (component type &optional (force :all))
  (mapcar #'(lambda (comp) (component-full-pathname comp type))
	  (file-components-in-component component force)))

(defun file-components-in-component (component &optional (force :all)
					       &aux result changed)
  (case (component-type component)
    ((:file :private-file)
     (when (setq changed
		 (or (find force '(:all t) :test #'eq)
		     (and (not (non-empty-listp force))
			  (needs-compilation component nil))))
       (setq result
	     (list component))))
    ((:module :system :subsystem :defsystem)
     (dolist (module (component-components component))
       (multiple-value-bind (r c)
	   (file-components-in-component
	    module
	    (cond ((and (some #'(lambda (dependent)
				  (member dependent changed))
			      (component-depends-on module))
			(or (non-empty-listp force)
			    (eq force :new-source-and-dependents)))
		   ;; The component depends on a changed file and force agrees.
		   :all)
		  ((and (non-empty-listp force)
			(member (component-name module) force
				:test #'string-equal :key #'string))
		   ;; Force is a list of modules and the component is
		   ;; one of them.
		   :all)
		  (t force)))
	 (when c
	   (push module changed)
	   (setq result (append result r)))))))
  (values result changed))

(setf (symbol-function 'oos) (symbol-function 'operate-on-system))

;;; ********************************
;;; Additional Component Operations
;;; ********************************

;;; *** Edit Operation ***

;;; Should this conditionalization be (or :mcl (and :CCL (not :lispworks)))?
#|
				     #+:ccl
				     (defun edit-operation (component force)
"Always returns nil, i.e. component not changed."
(declare (ignore force))
;;
(let* ((full-pathname (make::component-full-pathname component :source))
(already-editing\? #+:mcl (dolist (w (CCL:windows :class
'fred-window))
(when (equal (CCL:window-filename w)
full-pathname)
(return w)))
#-:mcl nil))
(if already-editing\?
#+:mcl (CCL:window-select already-editing\?) #-:mcl nil
(ed full-pathname)))
nil)

				     #+:allegro
				     (defun edit-operation (component force)
"Edit a component - always returns nil, i.e. component not changed."
(declare (ignore force))
(let ((full-pathname (component-full-pathname component :source)))
(ed full-pathname))
nil)

				     #+(or :ccl :allegro)
				     (make::component-operation :edit 'edit-operation)
				     #+(or :ccl :allegro)
				     (make::component-operation 'edit 'edit-operation)
				     |#

;;; *** Hardcopy System ***
(defparameter *print-command* "enscript -2Gr" ; "lpr"
  "Command to use for printing files on UNIX systems.")
#+:allegro
(defun hardcopy-operation (component force)
  "Hardcopy a component - always returns nil, i.e. component not changed."
  (declare (ignore force))
  (let ((full-pathname (component-full-pathname component :source)))
    (excl:run-shell-command (format nil "~A ~A"
				    *print-command* full-pathname)))
  nil)

#+:allegro
(make::component-operation :hardcopy 'hardcopy-operation)
#+:allegro
(make::component-operation 'hardcopy 'hardcopy-operation)


;;; *** System Source Size ***

(defun system-source-size (system-name &optional (force :all))
  "Prints a short report and returns the size in bytes of the source files in
   <system-name>."
  (let* ((file-list (files-in-system system-name force :source))
         (total-size (file-list-size file-list)))
    (format t "~&~a/~a (~:d file~:p) totals ~:d byte~:p (~:d kB)"
            system-name force (length file-list)
            total-size (round total-size 1024))
    total-size))

(defun file-list-size (file-list)
  "Returns the size in bytes of the files in <file-list>."
  ;;
  (let ((total-size 0))
    (dolist (file file-list)
      (with-open-file (stream file)
        (incf total-size (file-length stream))))
    total-size))

;;; *** System Tag Table ***

#+:allegro
(defun make-system-tag-table (system-name)
  "Makes an Emacs tag table using the GNU etags program."
  (let ((files-in-system (files-in-system system-name :all :source)))

    (format t "~&Making tag table...")
    (excl:run-shell-command (format nil "etags ~{~a ~}" files-in-system))
    (format t "done.~%")))


;madhu 181224
;;
;; :LANGUAGE STATIC-FILE
;;

;; (LOAD filespec &rest args &key verbose print if-does-not-exist external-format) => generalized-boolean
(defun mk-static-file-loader (filespec &rest args)
  (format t "~S~%" `(mk-static-file-loader ,filespec ,@args))
  (values t))

;; (COMPILE-FILE input-file &key output-file verbose print external-format) => output-truename, warnings-p, failure-p
(defun mk-static-file-compiler (input-file &rest args &key output-file &allow-other-keys)
  (format t "~S~%" `(mk-static-file-compiler ,input-file ,@args))
  (values (probe-file output-file) nil nil))

(mk:define-language :static-file
		    :loader 'mk-static-file-loader
		    :compiler 'mk-static-file-compiler
		    :source-extension :empty
		    :binary-extension :empty)

(defun mk::find-component (base path)
  (when (setq base (typecase base
		     (mk::component base)
		     (t (mk:find-system base))))
    (if (null path)
	base
	(let* ((path (if (atom path) (list path) path))
	       (components (mk::component-components base)))
	  (loop
	     (cond ((or (endp path) (endp components))
		    (return nil))
		   ((string-equal (component-name (car components))
				  (car path))
		    (cond ((cdr path)
			   (setq components (mk::component-components
					     (car components)))
			   (setq path (cdr path)))
			  (t (return (car components)))))
		   (t (setq components (cdr components)))))))))

#||
(defun mk::find-component-loose (base path)
  (when (setq base (typecase base
		     (mk::component base)
		     (t (mk:find-system base))))
    (if (null path)
	base
	(prog ((path (if (atom path) (list path) path)))
	  (%mk-traverse base (lambda (component)
			       (

	       (components (mk::component-components base)))
	  (loop
	     (cond ((or (endp path) (endp components))
		    (return nil))
		   ((equal (mk::canonicalize-component-name (car components))
			   (if (stringp (car path))
			       (car path)
			       (string-downcase (string (car path)))))
		    (cond ((cdr path)
			   (setq components (mk::component-components
					     (car components)))
			   (setq path (cdr path)))
			  (t (return (car components)))))
		   (t (setq components (cdr components)))))))))
||#

(defun system-relative-pathname (system path)
  (let ((system (typecase system
		  (component system)
		  (t (find-system system)))))
    (merge-pathnames path
		     (component-root-dir system :source))))

(defun system-source-directory (system)
  (system-relative-pathname system ""))

(defun system-source-file (system)
  (find-system-definition-pathname system))


;;; ----------------------------------------------------------------------
;;;
;;; PACKAGE-INFERRED-HACK
;;; Tue Aug 13 17:08:12 2019 +0530
;;;

;; The names of the recognized defpackage forms.
(defparameter *defpackage-forms* '(defpackage define-package))

(defun package-names (package)
  (cons (package-name package) (package-nicknames package)))

(defun initial-package-inferred-systems-table ()
  ;; Mark all existing packages are preloaded.
  (let ((h (make-hash-table :test 'equal)))
    (dolist (p (list-all-packages))
      (dolist (n (package-names p))
        (setf (gethash n h) t)))
    h))

;; Mapping from package names to systems that provide them.
(defvar *package-inferred-systems* (initial-package-inferred-systems-table))

;; Is a given form recognizable as a defpackage form?
(defun defpackage-form-p (form)
  (and (consp form)
       (member (car form) *defpackage-forms* :test #'string=)))

;; Find the first defpackage form in a stream, if any
(defun stream-defpackage-form (stream)
  (loop :for form = (read stream nil nil) :while form
        :when (defpackage-form-p form) :return form))

(defun file-defpackage-form (file)
  "Return the first DEFPACKAGE form in FILE."
  (with-open-file (f file)
    (stream-defpackage-form f)))

(define-condition package-inferred-system-missing-package-error ()
  ((system :initarg :system :reader error-system)
   (pathname :initarg :pathname :reader error-pathname))
  (:report (lambda (c s)
             (format s "~@<No package form found while ~
                                     trying to define package-inferred-system ~A from file ~A~>"
                     (error-system c) (error-pathname c)))))

(defun package-dependencies (defpackage-form)
  "Return a list of packages depended on by the package
defined in DEFPACKAGE-FORM.  A package is depended upon if
the DEFPACKAGE-FORM uses it or imports a symbol from it."
  (assert (defpackage-form-p defpackage-form))
  (remove-duplicates
   (let (ret)
     (flet ((dep (x) (push x ret)))
       (loop :for (option . arguments) :in (cddr defpackage-form) :do
	     (ecase option
	       ((:use :mix :reexport :use-reexport :mix-reexport)
		(dolist (p arguments) (dep (string p))))
	       ((:import-from :shadowing-import-from)
		(dep (string (first arguments))))
	       ((:nicknames :documentation :shadow :export :intern :unintern :recycle)))))
     (nreverse ret))
   :from-end t :test 'equal))

(defun package-designator-name (package)
  "Normalize a package designator to a string"
  (etypecase package
    (package (package-name package))
    (string package)
    (symbol (string package))))

(defun register-system-packages (system packages)
  "Register SYSTEM as providing PACKAGES."
  (let ((name (or (eq system t) (component-name (find-system system)))))
    (dolist (p (if (and packages (atom packages)) (list packages) packages))
      (setf (gethash (package-designator-name p) *package-inferred-systems*) name))))

(defvar *asd-hack-system-name* nil
  "Name of the system which is being generated/")

(defun package-name-system (package-name &optional (system *asd-hack-system-name*))
  "Return the name of the SYSTEM providing PACKAGE-NAME, if such exists,
otherwise return a default system name computed from PACKAGE-NAME."
  (check-type package-name string)
  (or (let ((name (gethash package-name *package-inferred-systems*)))
	(if (equalp system name)
	    nil
	    name))
      (string-downcase package-name)))

;; Given a file in package-inferred-system style, find its dependencies
(defun package-inferred-system-file-dependencies (file &optional (system *asd-hack-system-name*))
  (let ((defpackage-form (file-defpackage-form file)))
  (if defpackage-form
    (remove t (mapcar 'package-name-system (package-dependencies defpackage-form)))
    (error 'package-inferred-system-missing-package-error :system system :pathname file))))

(defun package-inferred-prefixp (prefix pkg-spec &aux idx)
  "Return the suffix"
  (unless (stringp pkg-spec) (setq pkg-spec (string-downcase pkg-spec)))
  (cond ((or (null (setq idx (mismatch prefix pkg-spec :test #'equalp)))
	     (>= idx (length prefix)))
	 (subseq pkg-spec (length prefix)))
	;; ;madhu 231228 - handle uses of the demenented package
	;; naming system: "d.lisp" defines a package "a.b.c/d" which
	;; uses package "a.b.c/e" defined in e.lisp.
	((and (> idx 0) (eql (elt pkg-spec idx) #\/))
	 (subseq pkg-spec (1+ idx)))))

(defun package-inferred-hack-get-pathname-on-disk (pkg-spec prefix dir)
  ;;(assert (cl-user::prefixp  prefix pkg-spec) nil "~A" (list pkg-spec prefix dir))
  (let ((x (package-inferred-prefixp prefix pkg-spec)))
    (when x
      (let ((p (concatenate 'string dir x ".lisp")))
	p))))

(defun package-inferred-hack-generate-file-list
    (path prefix &optional dir (depth 0) &aux ignored-deps)
  (unless dir
    (assert (zerop depth))
    (setq dir (directory-namestring path)))
  (let ((ret (loop for dep in  (package-inferred-system-file-dependencies path)
		   for p = (package-inferred-hack-get-pathname-on-disk
			    dep prefix dir)
		   unless p do (progn (pushnew dep ignored-deps :test #'equalp))
		   if p
		   append
		   (multiple-value-bind (ret1 ignored-deps1)
		       (package-inferred-hack-generate-file-list p prefix dir (1+ depth))
		     (setq ignored-deps (union ignored-deps ignored-deps1 :test #'equalp))
		     ret1)

		   append `((:file ,dep
			     ,@(when p
				 (let* ((x (package-inferred-system-file-dependencies p))
					(y (remove nil
						   (mapcar (lambda (a)
							     (package-inferred-prefixp prefix a))
							   x))))
				   (when y `(:depends-on ,y)))))))))
    (setq ret (delete-duplicates ret :test #'equal :key #'second))
    (values
     (if (zerop depth)
	 (remove nil
		 (mapcar (lambda (x &aux a)
			   ;;(assert (cl-user::prefixp prefix x))
			   (cond ((setq a (package-inferred-prefixp prefix (second x)))
				  (setf (second x) a) x)
				 (t nil)))
			 ret))
	 ret)
     ignored-deps)))

#+nil
(package-inferred-system-file-dependencies "~/cl/extern/rove/main.lisp")

#+nil
(package-inferred-hack-generate-file-list "/home/madhu/cl/extern/rove/main.lisp" "rove/")

;;; ----------------------------------------------------------------------
;;;
;;; CLEAN COMPONENTS-LIST form (generated from ASD)
;;;

(defun frob-plist-path (plist)
  "Replace :PATHNAME with :SOURCE-PATHNAME"
  (let ((path (getf plist :pathname '%notthere%)))
    (unless (eq path '%notthere%)
      (let ((cons (member :pathname plist)))
	(assert (eq path (second cons)))
	(rplaca cons :source-pathname)))
    plist))

#+nil
(frob-plist-path '(:file :barf :pathname "tmp/" :depends-on ("x" "y" "z")))

(defun frob-plist-serial (plist)
  "Remove :SERIAL T from PLIST"
  (let ((serial (getf plist :serial '%notthere%)))
    (unless (eq serial '%notthere%)
      (remf plist :serial)
      (let ((components-list (getf plist :components '%notthere%)))
	(unless (eq components-list '%notthere%)
	  (let ((cons (member :components plist)))
	    (assert (eq components-list (second cons)))
	    (setf (second cons) (cons :serial components-list)))))))

  plist)

#+nil
(frob-plist-serial '(:module :barf :serial t :pathname "tmp/" :components ("x" "y" "z")))

(defun frob-plist-grovel (plist)
  `(:file ,@(cdr plist) :language :cffi-grovel))

#+nil
(progn
(frob-plist-grovel '(:cffi-grovel-file "grovel-example" :depends-on ("package")))
(frob-plist-grovel '(cffi-grovel:grovel-file "grovel")))

(defun frob-plist-wrapper (plist)
  `(:file ,@(cdr plist) :language :cffi-wrapper))

;; may need to reload this after cffi-grovel is loaded
(defun clean-components-list (list)
  (let (ret)
    (dolist (val list)
      (cond ((atom val) (push val ret))
	    (t (let ((x (car val)) pkg)
		 (cond ((or (eql x :cffi-grovel-file)
			    (and (setq pkg (cl:find-package "CFFI-GROVEL"))
				 (eql x (find-symbol "GROVEL-FILE" pkg))))
			(push (frob-plist-grovel val) ret))
		       ((or (eql x :cffi-wrapper-file)
			    (and (setq pkg (cl:find-package "CFFI-GROVEL"))
				 (eql x (find-symbol "WRAPPER-FILE" pkg))))
			(push (frob-plist-wrapper val) ret))
		       ((eql x :module)
			(setq val (frob-plist-path val))
			(push (frob-plist-serial val) ret))
		       ((eql x :file)
			(push (frob-plist-path val) ret))
		       ((eql x :test-file)
			(setf (car val) :file)
			(push (frob-plist-path val) ret))
		       ((member x '(:doc-file :static-file)) nil)
		       (t (error "FIXME")))))))
    (nreverse ret)))

#+nil
(clean-components-list '((:module foo :serial t :pathname "barf" :depends-on (a b c))
			 (:static-file "barf")
			 (:cffi-grovel-file "xyz")))

;;; ----------------------------------------------------------------------
;;;
;;; PRETTY PRINT DEFSYSTEMS
;;;

(defvar +tab-stop+ 8)
(defvar +indent-tabs-mode+ t)

(defun write-indent (nindent-chars stream &key (tab-stop +tab-stop+)
		     (indent-tabs-mode +indent-tabs-mode+))
  (let ((i 0)
	(ntabs (if indent-tabs-mode
		   (floor nindent-chars tab-stop)
		   0)))
    (loop (cond ((> i nindent-chars) (error "sanity"))
		((= i nindent-chars) (return))
		((and (> ntabs 0) (>= (- nindent-chars i) tab-stop))
		 (incf i tab-stop)
		 (decf ntabs)
		 (write-char #\Tab stream))
		(t (write-char #\Space stream)
		   (incf i))))))

#+nil
(progn (write-indent 9 *standard-output*)
       (terpri *standard-output*))

(defun format-mk-plist (plist stream &key (indent 0))
  (loop for x on plist by #'cddr for (key val) = x do
	(assert (keywordp key))
	(format stream "~(~S~) " key)
	(cond ((atom val)
	       (if (find key '(:module :if-feature))
		   (format stream "~(~S~)" val)
		   (format stream "~S" val)))
	      ((eql key :components)
	       (format-mk-components-list (clean-components-list val)
					  stream :indent (+ indent 11 1)))
	      ((eql key :depends-on)
	       (format-mk-depends-on-list val
					  stream :indent (+ indent 11 1)))
	      ((eql key :if-feature)
	       (format stream "~(~S~)" val))
	      (t (format stream "~S" val)))
	(when (cddr x)
	  (terpri stream)
	  (write-indent indent stream))))

#+nil
(format-mk-plist '(:a 1 :b 2 :components (x y z) :c 3 :depends-on (e "f" g)) *standard-output*)

#+nil
(format-mk-plist '(:file "a" :depends-on ("b" "c")) *standard-output*)

(defun format-mk-name (name stream &key non-bare)
  (check-type name (or keyword symbol string))
  (cond ((and (symbolp name)
	      (or (not (symbol-package name))
		  (and (not (keywordp name))
		       non-bare)))
	 (format stream ":~(~A~)" name))
	(t (format stream "~(~S~)" name))))

#+nil
(with-output-to-string (*standard-output*)
  (mk::format-mk-name 'foo *standard-output* :non-bare t))

(defun format-mk-feature-expression (exp stream &key (indent 0))
  (etypecase exp
    (keyword (format stream "~(~S~)" exp))
    (cons (ecase (car exp)
	    ((:and :or :not)
	     (format stream "(~(~S~) " (car exp))
	     (loop for x on (cdr exp) for sub = (car x) do
		   (format-mk-feature-expression sub stream
						 :indent (+ indent 1))
		   (when (cdr x)
		     (terpri stream)
		     (write-indent (+ indent 1) stream)))
	     (format stream ")"))))))

#+nil
(with-output-to-string (stream)
  (format-mk-feature-expression '(:and (:or :sbcl :ecl :clasp)
				  (:not :usocket-iolib))
				stream))


(defun format-mk-depends-on-list (list stream &key (indent 0))
  (format stream "(")
  (loop for x on list for item = (car x)
	do
	(cond ((consp item)
	       (ecase (car item)
		 (:version
		  (warn "ignoring version depends-on ~S" item)
		  (format-mk-name (second item) stream))
		 (:feature
		  (format stream "(:feature ")
		  (format-mk-feature-expression (second item) stream
						:indent (+ indent 2))
		  (cond ((consp (second item))
			 (terpri stream)
			 (write-indent (+ indent 2) stream))
			(t (format stream " ")))
		  ;; the grammar for :feature dependency-def seems to
		  ;; be wrong. it is specified as ( :feature
		  ;; feature-expression dependency-def ) but it should
		  ;; be ( :feature feature-expression
		  ;; simple-component-name ). using the latter form
		  ;; here.
		  (if (consp (third item))
		      (warn "FORMAT-DEPENDS-ON: SKIPPING FORM ~S" item)
		      (format-mk-name (third item) stream))
		  (format stream ")"))))
	      (t (format-mk-name item stream)))
	(when (cdr x)
	  (terpri stream)
	  (write-indent (+ indent 1) stream)))
  (format stream ")"))

#+nil
(format-mk-depends-on-list '("y" "z") *standard-output* :indent 0)

#+nil
(with-output-to-string (*standard-output*)
  (format-mk-depends-on-list '(:split-sequence
			       (:feature (:and (:or :sbcl :ecl :clasp)
					  (:not :usocket-iolib))
				:sb-bsd-sockets)
			       (:feature :usocket-iolib
				:iolib))
			     *standard-output* :indent 0))

(defun format-mk-components-list (list stream &key (indent 0))
  (format stream "(")
  (loop for x on list for item = (car x)
	do
	(format-mk-item item stream :indent (+ indent 1))
	(when (cdr x)
	  (terpri stream)
	  (write-indent (+ indent 1) stream)))
  (format stream ")"))

(defun format-mk-item (val stream &key (indent 0))
  "VAL is a string or a list like (:file ... ) or (:module ...)"
  (etypecase val
    (atom (format stream "~S" val))
    (cons (assert (keywordp (car val)))
	  (format stream "(")
	  (format-mk-plist val stream :indent (+ indent 1))
	  (format stream ")"))))

(defun format-mk-form (mk-form stream &key (indent 0) source-dir binary-dir
		       (source-ext "lisp"))
  (destructuring-bind (sym name &rest key-val-args) mk-form
    (assert name)
    (ecase sym
      ((defsystem)
       (format stream "(mk:defsystem ")
       (format-mk-name name stream)
       (terpri stream)
       (and source-dir (format stream "  :source-pathname ~A~&" source-dir))
       (and binary-dir (format stream "  :binary-pathname ~A~&" binary-dir))
       (and source-ext (format stream "  :source-extension ~S" source-ext))
       (when key-val-args
	 (terpri stream)
	 (write-indent (+ indent 2) stream)
	 (format-mk-plist key-val-args stream :indent (+ indent 2)))
       (format stream ")")))))

;;;
;;; dump defsystem files from asd definitions
;;;
(defvar *asd-forms* '(defsystem))

;; Is a given form recognizable as a defsystem form?
(defun asd-form-p (form)
  (and (consp form)
       (member (car form) *asd-forms* :key #'symbol-name :test #'string-equal)))

;; Find the first defsystem form in a stream, if any
(defun stream-asd-form (stream)
  ;;madhu 231123 FIXME: a NIL form scuttle the READ
  (loop :for form = (read stream nil nil) :while form
        :when (asd-form-p form) :return form))

(defun file-asd-form (file)
  "Return the first DEFSYSTEM form in FILE."
  (with-open-file (f file)
    (stream-asd-form f)))

;madhu 231123
(defun file-asd-forms (file)
  "Retrun the DEFSYSTEM forms in FILE."
  (with-open-file (stream file)
    (loop :for form = (read stream nil 'eof) :while (not (eql form 'eof))
          :when (asd-form-p form) :collect form)))


(defun extract-subdirs (asd-definition-path prefix &optional pathname-complication)
  ;; ;madhu 230521 asd form specifies a :pathname. Assume it is a
  ;; string terminated with /, denoting a subdir where the sources
  ;; are found, and that merge-pathnames can handle
  ;;;correctly. i.e. (merge-pathnames "foo/src/" "/a/b/c/c.asd") =
  ;; ;#P"/a/b/c/foo/src/c.asd"
  (let* ((enough-namestring (enough-namestring (if pathname-complication
						   (merge-pathnames pathname-complication asd-definition-path)
						   asd-definition-path)
					       prefix))
	 (dir (pathname-directory enough-namestring)))
    (when dir
      (assert (eq (car dir) :relative))
      (copy-seq (cdr dir)))))

(defun ensure-mergable-string (pathname-complication)
  ;; ;madhu 240410 "add a trailing / to a non-empty :pathname string
  ;; if it does not elready exist.  This is necessary because of the
  ;; assumptions in extract-subdirs
  (if (and (stringp pathname-complication)
	   (not (equal pathname-complication ""))
	   (not (eql (elt pathname-complication
			  (1- (length pathname-complication)))
		     #\/)))
      (concatenate 'string pathname-complication "/")
      pathname-complication))

(defun make-mk-form-1 (components-form subdirs)
  (if (endp subdirs)
      components-form
      `((:module ,(car subdirs)
	 ,@(unless (string= (string-downcase (car subdirs))
			    (car subdirs))
	     `(:source-pathname ,(car subdirs)))
	 :components ,(make-mk-form-1 components-form (cdr subdirs))))))

(defun make-mk-form (asd-form subdirs asd-path)
  (let* ((depends-on (getf asd-form :depends-on))
	 (components (getf asd-form :components))
	 (pathname-complication (ensure-mergable-string (getf asd-form :pathname)))
	 (package-inferred-p
	  (let ((p (string (getf asd-form :class))))
	    (or (eq p :package-inferred-system)
		(equalp (string p) (string :package-inferred-system))
		;;madhu 231228 handle arbitrary strings?
		#+nil p))))
    ;; apparently some users just stick in a :class
    ;; :package-inferred-system without really meaning it
    (when (and package-inferred-p (not components))
      ;;(assert (= (length depends-on) 1))
      (let* ((prefix (concatenate 'string (string (second asd-form)) "/"))
	     new-depends-on new-components)
	(loop for dep in depends-on
	      for pkg-path =
	      (package-inferred-hack-get-pathname-on-disk
	       dep prefix
	       (directory-namestring
		(if pathname-complication
		    (merge-pathnames pathname-complication asd-path)
		    asd-path)))
	      do
;;	(assert pkg-path nil "Could not infer the path to the package file")
	(if (null pkg-path)
	    (progn (format t "MAKE-MK-FORM:: Could not infer the path to the package file for ~S.~&" dep)
		   #+nil(pushnew (string dep) new-depends-on))
	(multiple-value-bind (ret1 ignored-deps1)
	    (package-inferred-hack-generate-file-list pkg-path prefix)
	  (setq new-depends-on (append new-depends-on ignored-deps1))
	  (setq new-components (append new-components
				       ret1
				       `((:file ,(package-inferred-prefixp
						  prefix
						  dep))))))))
	(if new-depends-on (setq depends-on new-depends-on))
	(if new-components (setq components new-components))))
    `(defsystem ,(second asd-form)
       ,@(if depends-on `(:depends-on ,depends-on))
       ,@(if components `(:components
			  ,(make-mk-form-1 components
					   subdirs))))))

(defun get-asd-file-list (dir)
  (mk::split-string
   (with-output-to-string (stream)
     (let ((ret (mk::run-shell-command "LANG=C IFS=$'\n'; find ~S -name '*.asd' -type f" (list (namestring (translate-logical-pathname dir)))
				   :output stream)))
       (assert (zerop ret) nil "Error running program: ~A" (get-output-stream-string stream))))
   :item #\Newline))

#+nil
(get-asd-file-list "/home/madhu/cl/extern/cffi/")

(defun compare-system-names (a b)
  (let ((as (string a)) (bs (string b)))
    (let ((la (length as)) (lb (length bs)))
      (or (> la lb)
	  (and (= la lb)
	       (string> as bs))))))

;;;
;;; generate defsystem files from asd definitions
;;;

(defun asd-hack-dump-defsystem-file (target-file name root-dir &key
				     (asd-file-list
				      (get-asd-file-list root-dir))
				     skip-source-p
				     (skip-binary-p skip-source-p)
				     (if-exists :supersede)
				     root-dir-form)
  (let* ((*asd-hack-system-name* name)
	  (source-dir (format nil "*~(~A~)-source-dir*" name))
	 (binary-dir (format nil "*~(~A~)-binary-dir*" name))
	 (forms (sort (loop for f in asd-file-list
	    append   (loop  for asd-form in (file-asd-forms f)
			    for pathname-complication = (ensure-mergable-string (getf asd-form :pathname))
			    for subdirs = (extract-subdirs f (translate-logical-pathname root-dir)
							   pathname-complication)
			    for mk-form = (with-simple-restart (skip "Skip")
					    (make-mk-form asd-form subdirs f))
			    when mk-form collect it))
		      #'compare-system-names
		      :key #'second)))
    (with-open-file (stream target-file :direction :output
			    :if-exists if-exists)
      (format stream "(in-package \"CL-USER\")~%")
      (unless skip-source-p
	(format stream "(defvar ~A " source-dir)
	(etypecase root-dir-form
	  (string (write-string root-dir-form stream))
	  (cons (format stream "~S" root-dir-form))
	  (null (format stream "~:[~S~;(translate-logical-pathname ~S)~]"
			(pathname-logical-p root-dir) root-dir)))
	(write-line ")" stream))
      (unless skip-binary-p
	(format stream "(defvar ~A (binary-directory ~A))~%"
		binary-dir source-dir))
      (terpri stream)
      (loop for form in forms do
	    (apply #'format-mk-form form stream
		   (append
		     ;; FIXME: should these be forced to be physical pathnames?
		    (unless skip-source-p `(:source-dir ,source-dir))
		    (unless skip-binary-p `(:binary-dir ,binary-dir))))
	    (format stream "~%~%#+nil~%(mk:oos ")
	    (format-mk-name (second form) stream :non-bare t)
	    (format stream " :load :compile-during-load t)~%~%")))))


;; a bogus ASDF package maybe needed to READ asd files before dumping them
(defun make-fake-asdf-package ()
 (defpackage "ASDF"
  (:use)
  (:export "FIND-SYSTEM" "LOAD-SYSTEM" "DEFSYSTEM" "CLEAR-SYSTEM"
   "TEST-OP" "C-SOURCE-FILE" "PERFORM" "LOAD-OP" "LOAD-SOURCE-OP" "OUTPUT-FILES" "COMPILE-OP" "COMPONENT-PATHNAME"
   "TEST-SYSTEM" "PROGRAM-OP"
   "INITIALIZE-SOURCE-REGISTRY"
   "OOS"
   "PACKAGE-INFERRED-SYSTEM"
   "REGISTER-SYSTEM-PACKAGES"))
)

;; if we dont trust your asd files we may need to wrap the dump forms in
(defmacro without-the-sharpdot-reader (&body body)
  `(let ((*readtable* (copy-readtable)) (*read-eval* nil))
     (set-dispatch-macro-character #\# #\.
				   (lambda (s c n)
				     (format t "IGNORING ~S" (read s))))
     ,@body))

#+nil
(asd-hack-dump-defsystem-file "/dev/shm/cl-cffi-gtk.system" "cl-cffi-gtk"
			      "/home/madhu/cl/extern/cl-cffi-gtk/")

#+nil
(asd-hack-dump-defsystem-file "/dev/shm/dbus.system"
			      "dbus"
			      "/home/madhu/cl/extern/dbus"
			      '("/home/madhu/cl/extern/dbus/dbus.system"))


;;; ----------------------------------------------------------------------
;;;
;;;
;;;

(defun find-system-definition-pathname (spec)
  (if (typep spec 'component)
      (setq spec (component-name spec)))
  (or (compute-system-path spec)
      (system-definition-pathname spec)))

(defun mk-oos (&rest args)
  "MK-OOS SYSTEM-NAME &OPTIONAL OP &KEY (:PROPAGATE NIL) &ALLOW-OTHER_KEYS

A convenience function around MK:OOS. The default invocation \(MK-OOS
SYSTEM) compiles \(and loads) the files in the system SYSTEM-NAME -
all files are recompiled but dependent systems are not touched.  This
works by defaulting OOS keyword arguments :FORCE to T ,
COMPILE-DURING-LOAD to T, and by binding
*OPERATIONS-PROPAGATE-TO-SUBSYSTEMS* to NIL.

OP can be :COMPILE or :LOAD.  :PROPAGATE T can be supplied to force
recompilations of all dependent systems. Other keyword arguments to
OOS if supplied are passed on to OOS"
  (let* ((extra-args nil)
	 (propagate-cons (member :propagate args))
	 (propagate-supplied-p (if propagate-cons t))
	 (*operations-propagate-to-subsystems*
	  (if propagate-supplied-p
	      (second propagate-cons)
	      nil))
	 (real-args nil))
    (flet ((frob (k v)
	     (let ((elt (member k (cddr args))))
	       (if (not elt)
		   (setq extra-args (append `(,k ,v) extra-args))))))
      (frob :compile-during-load t)
      (frob :force t))
    (setq real-args (append (list (car args))
			    (list (if (endp (cdr args))
				 :compile
				 (cadr args)))
			    extra-args))
    (when propagate-supplied-p (remf real-args :propagate))
    (format t "MK-OOS ~{~A~^ ~}~&" real-args)
    (apply #'oos real-args)))

(defun missing-deps (system)
  (let ((hash (make-hash-table :test #'equal)) ret)
    (labels ((alldeps (system depth)
	       (assert (component-p system))
	       (assert (eq (component-type system) :defsystem))
	       (incf (gethash system hash 0))
	       (loop for system-dep-spec in (component-depends-on system)
		     for system-dep = (find-system system-dep-spec :load-or-nil)
		     do (if system-dep
			    (alldeps system-dep (1+ depth))
			    (incf (gethash system-dep-spec hash 0))))))
      (alldeps (ensure-system system) 0)
      (maphash (lambda (k v)
		 (unless (component-p k)
		   (push (cons k v) ret)))
	       hash)
      (values (mapcar 'car ret) (mapcar 'cdr ret)))))

(export '(find-system-definition-pathname mk-oos missing-deps))



#+nil
(missing-deps :lem)

;madhu 181114
(defun plist-partition (plist keys)
  ;; Return as values plist-with-keys plist-without-keys
  (let (ret sans)
    (loop (multiple-value-bind (indicator value tail)
	      (get-properties plist keys)
            (unless tail
              (return (values ret (nreconc sans plist))))
	    (setq ret (list* indicator value ret))
            (loop until (eq plist tail) do
		  (push (pop plist) sans)
		  (push (pop plist) sans))
            (setq plist (cddr plist))))))

(export '(plist-partition))

;; ;madhu 211105 TODO - strange behaviour with cl-quickcheck.system:
;; ;which uses a lowercase string to name the system. loading the
;; ;sysdef file (which is not available through central-registry) and
;; ;requiring 'cl-quickcheck puts a cl-quickcheck into *modules* in lw
;; ;and clisp but does not load the system.

;;madhu 220503  - for mcclim additions
(defun already-loaded-systems ()
  "Return a list of the names of the systems that have been successfully
loaded so far."
  (mapcar #'component-name
	  (remove-if-not (lambda (s)
			   (and (find (canonicalize-system-name
				       (component-name s))
				      *modules* :test #'equalp)
				;;#+nil
				(component-load-time s)))
			 (defined-systems))))

#+nil
(defun mk::coerce-name (name)
  (typecase name
    (mk::component (mk::component-name name))
    (symbol (string-downcase name))
    (string name)
    (t (error "Invalid component designator: ~S" name))))

#+nil
(already-loaded-systems)

(defun reset-pristine ()
  (clrhash *defined-systems*)
  (clrhash *defsystem-to-defsystem-file-map*)
  (clrhash *binary-pathnames-table*)
  (clrhash *source-pathnames-table*)
  (clrhash *language-table*))

;;; end of file -- defsystem.lisp --
