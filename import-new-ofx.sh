#!/bin/bash
# Directories
OFX_DIR="$HOME/Documents/finance"
NEW_DIR=".new"
OLD_DIR=".old"
# Programs
# fixofx.py from wesabe
FIXOFX="/home/ryan/src/wesabe/fixofx/fixofx.py"
MUNGEOFX="/home/ryan/Projects/ofxtoolkit/mungeofx.pl"
MERGEOFX="/home/ryan/Projects/ofxtoolkit/mergeofx.pl"
RENAMEOFX="/home/ryan/Projects/ofxtoolkit/renameofx.pl"

shopt -s nullglob

mkdir -p "$OFX_DIR"

{ cd "$OFX_DIR"

  mkdir -p "$NEW_DIR" "$OLD_DIR"

  # Clean stale temp files
  rm -f "$OLD_DIR"/TEMPOFX.*

  #tempfiles=""

  # Fix OFX files, move them from old to new, and rename them
  { cd "$NEW_DIR"
    if [ "`echo *.ofx *.qfx`" != "" ]; then
      echo "Fixing..."
      for x in *.ofx *.qfx; do
        tempfile=`mktemp -p "../${OLD_DIR}" -t TEMPOFX.XXXXXXXXX`
        "$FIXOFX" < "$x" > "$tempfile" || { echo "fixofx failed on $x"; exit 1; }
      done
    else
      echo "No new files to import."
    fi
    cd ..
  }

  # Munge and merge OFX files
  { cd "$OLD_DIR"
    if [ "`echo TEMPOFX.*`" != "" ]; then
      echo "Munging..."
      "$MUNGEOFX" TEMPOFX.* || { echo "Munging failed"; exit 1; }

      echo "Merging..."
    else
      echo "Updating existing files..."
    fi
    "$MERGEOFX" --output-directory "$OFX_DIR" --update TEMPOFX.* || { echo "Merge failed"; exit 1; }

    if [ "`echo TEMPOFX.*`" != "" ]; then
      echo "Renaming..."
      "$RENAMEOFX" TEMPOFX.* || { echo "renameofx failed"; exit 1; }
      trash ../"$NEW_DIR"/*.[oq]fx
    fi
    cd ..
  }
}

echo "Done."

