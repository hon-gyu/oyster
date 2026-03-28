Setup: create a minimal vault with one markdown file.

  $ mkdir vault
  $ cat > vault/hello.md << 'EOF'
  > # Hello World
  > EOF

Render once so we have output to serve:

  $ oystermark vault vault _out --pipeline none --theme none > /dev/null

Output directory should contain the rendered HTML:

  $ find _out -name '*.html' | sort
  _out/hello/index.html

  $ grep -c '<h1' _out/hello/index.html
  1

Serve mode
====================

Start the server in the background and verify HTTP responses:

  $ oystermark vault vault _out --pipeline none --theme none --serve --port 9876 > /dev/null 2>&1 &
  $ SERVER=$!
  $ sleep 2

Existing file returns 200:

  $ curl -s -o /dev/null -w '%{http_code}' http://localhost:9876/hello/index.html
  200

Directory with trailing slash serves index.html:

  $ curl -s -o /dev/null -w '%{http_code}' http://localhost:9876/hello/
  200

Directory without trailing slash redirects (301):

  $ curl -s -o /dev/null -w '%{http_code}' http://localhost:9876/hello
  301

Missing file returns 404:

  $ curl -s -o /dev/null -w '%{http_code}' http://localhost:9876/nonexistent
  404

Content-type for HTML:

  $ curl -s -I http://localhost:9876/hello/index.html | grep -i content-type
  content-type: text/html; charset=utf-8

  $ kill $SERVER 2>/dev/null; wait $SERVER 2>/dev/null
  [143]

Watch mode
====================

Start watch mode in the background, modify a file, and verify re-render:

  $ oystermark vault vault _out --pipeline none --theme none --watch > watch.log 2>&1 &
  $ WATCH=$!
  $ sleep 2

Record the original content:

  $ grep -c 'Hello World' _out/hello/index.html
  1

Modify the source file:

  $ cat > vault/hello.md << 'EOF'
  > # Updated Title
  > EOF
  $ sleep 3

Watch should have detected the change and re-rendered:

  $ grep -c 'Change detected' watch.log
  1

Output should now contain the updated content:

  $ grep -c 'Updated Title' _out/hello/index.html
  1

  $ kill $WATCH 2>/dev/null; wait $WATCH 2>/dev/null
  [143]
