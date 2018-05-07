;;; anki-editor-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "anki-editor" "anki-editor.el" (0 0 0 0))
;;; Generated autoloads from anki-editor.el

(autoload 'anki-editor-submit "anki-editor" "\
Send notes in current buffer to Anki.

For each note heading, if there's no note id in property drawer,
create a note, otherwise, update fields and tags of the existing
note.

If one fails, the failure reason will be set in property drawer
of that heading.

\(fn)" t nil)

(autoload 'anki-editor-insert-deck "anki-editor" "\
Insert a deck heading.
With PREFIX, only insert the deck name at point.

\(fn &optional PREFIX)" t nil)

(autoload 'anki-editor-insert-note "anki-editor" "\
Insert the skeleton of a note.

The contents to be insrted are structured with a note heading
along with subheadings that correspond to fields.

Where the note is inserted depends on where the point is.

When the point is somewhere inside a note heading, the new note
is inserted below this note with same heading level.

Or when the point is outside any note heading but inside a
heading that isn't tagged with 'deck' but under a deck heading,
the new note is one level lower to and is inserted at the bottom
of this heading.

Or when the point is inside a deck heading, the behavior is the
same as above.

Otherwise, it's inserted below current heading at point.

\(fn)" t nil)

(autoload 'anki-editor-add-tags "anki-editor" "\
Add tags to property drawer of current heading with autocompletion.

\(fn)" t nil)

(autoload 'anki-editor-cloze-region "anki-editor" "\
Cloze region with number ARG.

\(fn &optional ARG)" t nil)

(autoload 'anki-editor-export-subtree-to-html "anki-editor" "\
Export subtree of the element at point to HTML.

\(fn)" t nil)

(autoload 'anki-editor-convert-region-to-html "anki-editor" "\
Convert and replace region to HTML.

\(fn)" t nil)

(autoload 'anki-editor-anki-connect-upgrade "anki-editor" "\
Upgrade AnkiConnect to the latest version.

This will display a confirmation dialog box in Anki asking if you
want to continue.  The upgrading is done by downloading the latest
code in the master branch of its Github repo.

This is useful when new version of this package depends on the
bugfixes or new features of AnkiConnect.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "anki-editor" '("anki-editor-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; anki-editor-autoloads.el ends here
