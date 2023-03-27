- **Added:**
  .editorconfig file (used by many source code editors
  to set things like indent size)
- **Changed:**
  Some default key bindings were changed,
  notably Alt is now the default modifier
  for navigation (except on macOS) just like with VsCoq
  (`#17421 <https://github.com/coq/coq/pull/17421>`_,
  by Sylvain Chiron).
- **Changed:**
  Preferences dialog is improved: more margins,
  tree of categories, sections in the categories,
  spin button for numbers, preservation of the last
  selected category, and more
  (`#17421 <https://github.com/coq/coq/pull/17421>`_,
  by Sylvain Chiron).
- **Fixed:**
  Changing allowed modifiers in the preferences dialog
  now has directly an effect
  (`#17421 <https://github.com/coq/coq/pull/17421>`_,
  by Sylvain Chiron).
- **Changed:**
  Changing modifiers for the View menu only applies
  to togglable items
  (`#17421 <https://github.com/coq/coq/pull/17421>`_,
  by Sylvain Chiron).
- **Fixed:**
  No more preferences are applied before clicking Apply or OK
  (`#17421 <https://github.com/coq/coq/pull/17421>`_,
  by Sylvain Chiron).
- **Removed:**
  Files from the configwin library which was barely used
  (`#17421 <https://github.com/coq/coq/pull/17421>`_,
  by Sylvain Chiron).
- **Changed:**
  The default browser command for Linux now uses xdg-open
  (`#17421 <https://github.com/coq/coq/pull/17421>`_,
  by Sylvain Chiron).
- **Fixed:**
  Loading dropped files whose name contain special characters
  (`#17421 <https://github.com/coq/coq/pull/17421>`_,
  fixes `#3977 <https://github.com/coq/coq/issues/3977>`_,
  by Sylvain Chiron).
- **Fixed:**
  Zoom fit feature, which didn’t work
  (`#17421 <https://github.com/coq/coq/pull/17421>`_,
  by Sylvain Chiron).
- **Added:**
  Document tabs are now reordable
  (`#17421 <https://github.com/coq/coq/pull/17421>`_,
  by Sylvain Chiron).
- **Added:**
  “Select All” menu item in the Edit menu
  (`#17421 <https://github.com/coq/coq/pull/17421>`_,
  fixes `#16141 <https://github.com/coq/coq/issues/16141>`_
  by Sylvain Chiron).
- **Changed:**
  Find/replace UI was improved: margins,
  icons for found/not found
  (`#17421 <https://github.com/coq/coq/pull/17421>`_,
  fixes `#11024 <https://github.com/coq/coq/issues/11024>`_
  by Sylvain Chiron).
