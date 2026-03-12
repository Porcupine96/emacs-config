#!/bin/bash

# Fix Emacs 30 seq-empty-p error by removing conflicting external seq package
# seq is now built-in to Emacs 30, so the external package causes conflicts

echo "Removing external seq package from straight.el..."
rm -rf ~/.emacs.d/straight/build/seq
rm -rf ~/.emacs.d/straight/repos/seq

echo "Cleaning native compilation cache..."
find ~/.emacs.d/eln-cache -name "*seq*.eln" -delete

echo "Rebuilding straight.el packages..."
# The rebuild will happen automatically on next Emacs start

echo "Done! The seq package conflict has been resolved."
echo "Start Emacs to verify the fix."
