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

{ pushd "$OFX_DIR"

  mkdir -p "$NEW_DIR" "$OLD_DIR"

  # Clean stale temp files
  rm -f "$OLD_DIR"/TEMPOFX.*

  #tempfiles=""

  # Fix OFX files, move them from old to new, and rename them
  { pushd "$NEW_DIR"
    if [ "`echo *.ofx *.qfx`" != "" ]; then
      for x in *.ofx *.qfx; do
        tempfile=`mktemp -p "../${OLD_DIR}" -t TEMPOFX.XXXXXXXXX`
        #tempfiles="$tempfiles $tempfile"
        echo "Fixing $x..."
        "$FIXOFX" < "$x" > "$tempfile" || { echo "fixofx failed on $x"; exit 1; }
      done
    else
      echo "No new files. Exiting."
      exit 0;
    fi
    popd
  }

  #exit 0;

  # Munge and merge OFX files
  { pushd "$OLD_DIR"

    echo "Munging..."
    "$MUNGEOFX" TEMPOFX.* || { echo "Munging failed"; exit 1; }

    echo "Merging..."
    "$MERGEOFX" --output-directory "$OFX_DIR" --update TEMPOFX.* || { echo "Merge failed"; exit 1; }

    echo "Renaming..."
    "$RENAMEOFX" TEMPOFX.* || { echo "renameofx failed"; exit 1; }
    trash ../"$NEW_DIR"/*.[oq]fx

    popd
  }
  rm -f "$OLD_DIR"/TEMPOFX.*
  popd
}

echo "Done."

