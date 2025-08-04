#!/usr/bin/env bash

set -euo pipefail

if [ $# -ne 1 ]; then
  echo "Usage: $0 <path-to-ComposOp.hs>"
  exit 1
fi

FILE="$1"

if ! grep -q 'import Control.Monad.Identity' "$FILE"; then
  echo "The expected import line 'import Control.Monad.Identity' was not found in $FILE"
  exit 1
fi

# Insert the new import after the Identity import
sed 's/import Control.Monad.Identity/&°import Control.Monad (MonadPlus(..), ap)/' "$FILE" \
  | tr '°' '\n' > "$FILE.bak"

mv "$FILE.bak" "$FILE"

echo "Inserted import into $FILE"
