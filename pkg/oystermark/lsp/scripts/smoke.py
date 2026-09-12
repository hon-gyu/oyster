# /// script
# requires-python = ">=3.11"
# dependencies = []
# ///
# type: ignore
"""Smoke-check the LSP adapter over a real stdio session.

`tests/lsp/` drives `Lsp_lib.Server` in process, which leaves `main.ml` —
capability advertisement and request dispatch — uncovered.  That gap is not
theoretical: the daily-note command was once handled in `on_request_unhandled`,
which linol never consults for `ExecuteCommand`, so a correct handler sat
unreachable while every test passed.  Nothing in the suite could see it,
because the bug was in which hook the handler hung off.

This script talks to the built binary the way an editor does and asserts the
effects an editor would observe.  Run it by hand after touching `main.ml`:

    dune build pkg/oystermark/lsp/main.exe
    uv run --script pkg/oystermark/lsp/scripts/smoke.py

Pass a path to check a different binary — the installed one, say:

    uv run --script pkg/oystermark/lsp/scripts/smoke.py $(which oystermark-lsp)
"""

import json
import os
import shutil
import subprocess
import sys
import tempfile
from typing import IO

DEFAULT_BINARY = "_build/default/pkg/oystermark/lsp/main.exe"
DAILY_NOTE_COMMAND = "oystermark.dailyNote.open"

NOTE = "note.md"
NOTE_TEXT = "# Hello\n\nsome prose.\n"


# Session
# =======


class Session:
    """One stdio LSP session, with the server's own requests recorded."""

    def __init__(self, binary, root):
        self.proc = subprocess.Popen(
            [binary],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
        )
        assert self.proc.stdin is not None and self.proc.stdout is not None
        self.stdin: IO[bytes] = self.proc.stdin
        self.stdout: IO[bytes] = self.proc.stdout
        self.root = root
        self.requests = []  # methods the server sent us, in order
        self.messages = []  # window/showMessage texts, in order
        self.next_id = 1

    def send(self, msg):
        body = json.dumps(msg).encode()
        self.stdin.write(b"Content-Length: %d\r\n\r\n" % len(body) + body)
        self.stdin.flush()

    def read(self):
        length = None
        while True:
            line = self.stdout.readline()
            if not line:
                return None
            line = line.strip()
            if not line:
                break
            if line.lower().startswith(b"content-length:"):
                length = int(line.split(b":")[1])
        if length is None:
            return None
        return json.loads(self.stdout.read(length))

    def request(self, method, params):
        """Send a request and pump until its response, acking anything the
        server asks of us on the way — a server request left unanswered can
        stall the exchange being measured."""
        id_ = self.next_id
        self.next_id += 1
        self.send({"jsonrpc": "2.0", "id": id_, "method": method, "params": params})
        while True:
            msg = self.read()
            if msg is None:
                raise RuntimeError(f"server closed the connection during {method}")
            if "method" in msg and msg.get("id") is not None:
                self.requests.append(msg["method"])
                result = (
                    {"applied": True}
                    if msg["method"] == "workspace/applyEdit"
                    else {"success": True}
                )
                self.send({"jsonrpc": "2.0", "id": msg["id"], "result": result})
            elif msg.get("method") == "window/showMessage":
                self.messages.append(msg["params"]["message"])
            elif msg.get("id") == id_:
                if "error" in msg:
                    raise RuntimeError(f"{method} failed: {msg['error']}")
                return msg.get("result")

    def notify(self, method, params):
        self.send({"jsonrpc": "2.0", "method": method, "params": params})

    def uri(self, rel_path):
        return "file://" + os.path.join(self.root, rel_path)

    def close(self):
        self.proc.kill()


# Checks
# ======

failures = []


def check(label, ok, detail="", hint=""):
    """[detail] is context worth seeing either way; [hint] is guidance that
    only matters once the check has failed."""
    note = detail or (hint if not ok else "")
    print(f"  {'PASS' if ok else 'FAIL'}  {label}{'  — ' + note if note else ''}")
    if not ok:
        failures.append(label)


def run(binary, show_document=True):
    root = tempfile.mkdtemp(prefix="oysterlsp-smoke-")
    with open(os.path.join(root, NOTE), "w") as f:
        f.write(NOTE_TEXT)

    s = Session(binary, root)
    try:
        # A bare vault: no oysterlsp.json, no initializationOptions.  Daily
        # notes must still work off their defaults.
        window = {"showDocument": {"support": True}} if show_document else {}
        caps = s.request(
            "initialize",
            {
                "processId": None,
                "rootUri": "file://" + root,
                "capabilities": {"window": window},
            },
        )["capabilities"]

        print("\ncapabilities")
        check("codeActionProvider", caps.get("codeActionProvider") is not None)
        check("codeLensProvider", caps.get("codeLensProvider") is not None)
        triggers = (caps.get("completionProvider") or {}).get(
            "triggerCharacters", []
        )
        check(
            "completionProvider triggers on the Markdown destination",
            "(" in triggers,
            f"got {triggers}",
            hint="without it, completion inside [label]( never fires",
        )
        commands = (caps.get("executeCommandProvider") or {}).get("commands", [])
        check(
            "executeCommandProvider lists the daily-note command",
            DAILY_NOTE_COMMAND in commands,
            f"got {commands}",
        )

        s.notify("initialized", {})
        s.notify(
            "textDocument/didOpen",
            {
                "textDocument": {
                    "uri": s.uri(NOTE),
                    "languageId": "markdown",
                    "version": 1,
                    "text": NOTE_TEXT,
                }
            },
        )

        # Code actions: the daily-note menu, plus the command block offer.
        print("\ntextDocument/codeAction")
        actions = s.request(
            "textDocument/codeAction",
            {
                "textDocument": {"uri": s.uri(NOTE)},
                "range": {
                    "start": {"line": 2, "character": 0},
                    "end": {"line": 2, "character": 0},
                },
                "context": {"diagnostics": []},
            },
        )
        titles = [a["title"] for a in actions]
        check("daily-note actions offered", any("daily note" in t for t in titles))
        check(
            "command-block insert offered",
            any("command block" in t for t in titles),
            f"got {titles}",
        )

        # A creating action carries the edit that makes the note itself: the
        # CreateFile puts it on disk and the text edit against it is what makes
        # a client open it, which is the only route to a document where
        # window/showDocument is missing.  See `docs/feature-daily-notes.mld`.
        creating = next(
            (a for a in actions if a["title"].startswith("Create") and a.get("edit")),
            None,
        )
        if creating is None:
            check("a creating action carries an edit", False)
        else:
            changes = creating["edit"].get("documentChanges", [])
            kinds = [c.get("kind", "textEdit") for c in changes]
            check(
                "the create edit creates the note and opens it",
                kinds == ["create", "textEdit"],
                f"got {kinds}",
                hint="a CreateFile alone lands on disk and opens nothing",
            )

        # The seam that had no coverage: a command must reach the handler and
        # produce its protocol effects.
        print("\nworkspace/executeCommand")
        daily = next((a for a in actions if a.get("command")), None)
        if daily is None:
            check("a code action carries a command", False)
        else:
            arguments = daily["command"]["arguments"]
            before, said = len(s.requests), len(s.messages)
            result = s.request(
                "workspace/executeCommand",
                {"command": daily["command"]["command"], "arguments": arguments},
            )
            sent, messages = s.requests[before:], s.messages[said:]
            check(
                "reaches the handler (result is not null)",
                result is not None,
                hint="null means the request was dispatched to a hook that is "
                "not overridden — check which linol method handles it",
            )
            # The action already created the note through its own edit, so its
            # command is asked to open only.
            check("makes no second edit", "workspace/applyEdit" not in sent)
            if show_document:
                check("sends window/showDocument", "window/showDocument" in sent)
                check("stays quiet when the client can focus", not messages)
            else:
                check(
                    "does not send window/showDocument",
                    "window/showDocument" not in sent,
                )
                # The third argument said an edit already opened the note, so
                # there is nothing to tell a reader who is looking at it.
                check(
                    "stays quiet about a note its edit opened",
                    not messages,
                    f"said {messages}",
                )

            # A lens carries no edit — a CodeLens is a command and nothing else
            # — so it asks the command to create, and gets the older behaviour:
            # an applyEdit, and a message where focusing is impossible.
            path = arguments[0]
            before, said = len(s.requests), len(s.messages)
            s.request(
                "workspace/executeCommand",
                {"command": daily["command"]["command"], "arguments": [path, True]},
            )
            sent, messages = s.requests[before:], s.messages[said:]
            check("asked to create, it sends applyEdit", "workspace/applyEdit" in sent)
            if not show_document:
                # Silence would be indistinguishable from failure: nothing
                # opened and the caller was given no edit to open it with.
                check(
                    "reports the path instead",
                    any(root in m for m in messages),
                    f"said {messages}",
                )

        # Completion answers a CompletionList, not a bare array, and its items
        # carry a textEdit rather than an insertText.  Both are shapes only the
        # wire can show: in process the result is an OCaml value that cannot be
        # malformed.  See `docs/feature-completion-markdown-links.mld`.
        print("\ntextDocument/completion")
        typed = NOTE_TEXT + "\nSee [label](no"
        s.notify(
            "textDocument/didChange",
            {
                "textDocument": {"uri": s.uri(NOTE), "version": 2},
                "contentChanges": [{"text": typed}],
            },
        )
        completion = s.request(
            "textDocument/completion",
            {
                "textDocument": {"uri": s.uri(NOTE)},
                "position": {"line": 4, "character": 14},
            },
        )
        items = (
            completion.get("items", []) if isinstance(completion, dict) else completion
        ) or []
        check(
            "the vault's own note is offered as a path",
            any(i["label"] == NOTE for i in items),
            f"got {[i['label'] for i in items]}",
        )
        edits = [i.get("textEdit") for i in items]
        check(
            "items carry a textEdit with a range",
            bool(edits) and all(e and "range" in e for e in edits),
            hint="an insertText lets the client choose what to replace, and "
            "VS Code's word rules split on /",
        )
        if edits and edits[0]:
            start = edits[0]["range"]["start"]
            check(
                "the range starts at the destination, not the cursor",
                (start["line"], start["character"]) == (4, 12),
                f"got {start}",
            )

        # Code lenses carry the same commands; a client that renders them is
        # the whole point of the command block.
        print("\ntextDocument/codeLens")
        panel = "```oysterlsp\ndaily/today\n```\n"
        s.notify(
            "textDocument/didChange",
            {
                "textDocument": {"uri": s.uri(NOTE), "version": 3},
                "contentChanges": [{"text": NOTE_TEXT + "\n" + panel}],
            },
        )
        lenses = s.request(
            "textDocument/codeLens", {"textDocument": {"uri": s.uri(NOTE)}}
        )
        check(
            "a command-block line gets a lens with a command",
            any(lens.get("command", {}).get("command") for lens in lenses),
            f"got {len(lenses)} lens(es)",
        )
    finally:
        s.close()
        shutil.rmtree(root, ignore_errors=True)


def run_disabled(binary):
    """A vault whose `oysterlsp.json` says `"disable": true`.

    The message at initialize is the whole of what an editor can see here —
    every answer below it is empty by design — and it is sent from `main.ml`,
    so nothing in `tests/lsp/` can reach it.  See
    `docs/feature-configuration.mld`, section Disable."""
    root = tempfile.mkdtemp(prefix="oysterlsp-smoke-off-")
    with open(os.path.join(root, NOTE), "w") as f:
        f.write(NOTE_TEXT)
    with open(os.path.join(root, "oysterlsp.json"), "w") as f:
        json.dump({"disable": True}, f)

    s = Session(binary, root)
    try:
        s.request(
            "initialize",
            {
                "processId": None,
                "rootUri": "file://" + root,
                "capabilities": {"window": {"showDocument": {"support": True}}},
            },
        )
        s.notify("initialized", {})

        print("\ninitialize")
        check(
            "says it is disabled",
            any("disabled" in m for m in s.messages),
            f"said {s.messages}",
            hint="a server that answers nothing and says nothing is "
            "indistinguishable from a broken one",
        )

        print("\nrequests")
        s.notify(
            "textDocument/didOpen",
            {
                "textDocument": {
                    "uri": s.uri(NOTE),
                    "languageId": "markdown",
                    "version": 1,
                    "text": NOTE_TEXT,
                }
            },
        )
        actions = s.request(
            "textDocument/codeAction",
            {
                "textDocument": {"uri": s.uri(NOTE)},
                "range": {
                    "start": {"line": 2, "character": 0},
                    "end": {"line": 2, "character": 0},
                },
                "context": {"diagnostics": []},
            },
        )
        check("no code actions", not actions, f"got {[a['title'] for a in actions]}")
        lenses = s.request(
            "textDocument/codeLens", {"textDocument": {"uri": s.uri(NOTE)}}
        )
        check("no code lenses", not lenses, f"got {len(lenses or [])} lens(es)")
        hover = s.request(
            "textDocument/hover",
            {
                "textDocument": {"uri": s.uri(NOTE)},
                "position": {"line": 0, "character": 3},
            },
        )
        check("no hover", hover is None, f"got {hover}")
    finally:
        s.close()
        shutil.rmtree(root, ignore_errors=True)


def run_file_operations(binary):
    """A folder moved in the editor's explorer.

    `workspace/willRenameFiles` and `workspace/didRenameFiles` only arrive if
    the capability asks for them, and linol has no dedicated hook for either.
    See `docs/feature-rename.mld`, section File operations."""
    root = tempfile.mkdtemp(prefix="oysterlsp-smoke-move-")
    os.makedirs(os.path.join(root, "d"))
    with open(os.path.join(root, "top.md"), "w") as f:
        f.write("[[a]] [x](d/a.md)\n")
    with open(os.path.join(root, "d", "a.md"), "w") as f:
        f.write("# A\n")

    s = Session(binary, root)
    try:
        caps = s.request(
            "initialize",
            {"processId": None, "rootUri": "file://" + root, "capabilities": {}},
        )["capabilities"]
        s.notify("initialized", {})

        print("\ncapabilities")
        operations = (caps.get("workspace") or {}).get("fileOperations") or {}
        check("asks for willRename", "willRename" in operations, f"got {operations}")
        check("asks for didRename", "didRename" in operations)

        files = [{"oldUri": s.uri("d"), "newUri": s.uri("e")}]
        print("\nworkspace/willRenameFiles")
        edit = s.request("workspace/willRenameFiles", {"files": files}) or {}
        texts = [
            e["newText"]
            for change in edit.get("documentChanges", [])
            for e in change.get("edits", [])
        ]
        check("rewrites the path the move breaks", texts == ["e/a.md"], f"got {texts}")

        print("\nworkspace/didRenameFiles")
        os.rename(os.path.join(root, "d"), os.path.join(root, "e"))
        s.notify("workspace/didRenameFiles", {"files": files})
        # linol answers definition only for an open document.
        s.notify(
            "textDocument/didOpen",
            {
                "textDocument": {
                    "uri": s.uri("top.md"),
                    "languageId": "markdown",
                    "version": 1,
                    "text": "[[a]] [x](d/a.md)\n",
                }
            },
        )
        locations = s.request(
            "textDocument/definition",
            {
                "textDocument": {"uri": s.uri("top.md")},
                "position": {"line": 0, "character": 2},
            },
        )
        uris = [loc["uri"] for loc in locations or []]
        check(
            "the index follows the move",
            uris == [s.uri("e/a.md")],
            f"got {uris}",
            hint="a stale index still points at the old path",
        )
    finally:
        s.close()
        shutil.rmtree(root, ignore_errors=True)


if __name__ == "__main__":
    binary = sys.argv[1] if len(sys.argv) > 1 else DEFAULT_BINARY
    if not os.path.exists(binary):
        sys.exit(
            f"no such binary: {binary}\n(build it with: dune build {DEFAULT_BINARY})"
        )
    print(f"smoke-checking {binary}")
    print("\n### client that supports window/showDocument")
    run(binary)
    # The other half of the world, and the one several editors are in: the
    # command cannot focus, and must say so rather than finish in silence.
    print("\n### client that does not support window/showDocument")
    run(binary, show_document=False)
    # The vault that asked not to be served: one message, and nothing else.
    print("\n### vault with \"disable\": true")
    run_disabled(binary)
    print("\n### folder moved in the editor")
    run_file_operations(binary)
    print(
        f"\n{len(failures)} failure(s)"
        + (": " + ", ".join(failures) if failures else "")
    )
    sys.exit(1 if failures else 0)
