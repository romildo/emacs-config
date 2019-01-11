#!/bin/sh

PREVIEWER=okular

PREVIEW_FILE=$(mktemp --suffix=.ps)

cat >"$PREVIEW_FILE" && $PREVIEWER "$PREVIEW_FILE" || \
    notify-send 'Print preview failed' --urgency=critical \
    --expire-time=2000

rm "$PREVIEW_FILE"
