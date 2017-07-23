#!/bin/sh

PREVIEW_FILE=$(mktemp --suffix=.ps)
cat >"$PREVIEW_FILE" && evince "$PREVIEW_FILE" || \
    notify-send 'Print preview failed' --urgency=critical \
    --expire-time=2000
rm "$PREVIEW_FILE"
