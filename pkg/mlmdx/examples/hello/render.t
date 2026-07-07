Render hello.mlmdx through the full chain
(.mlmdx -> mlmdx-pp -> html_of_jsx.ppx -> JSX.render -> HTML):

  $ ./render.exe
  <h1>4</h1><p>Some <strong>bold</strong> prose and an inline value: 42.</p><p>A component inline: <span class="greeting">Hello, inline! x2</span> — and standalone:</p><p><span class="greeting">Hello, World! x21</span></p><p>Host JSX inline: <b class="loud">rendered <strong>bold</strong></b>.</p><div class="box"><h2>Host block</h2><p>Markdown <strong>inside</strong> a host block.</p></div><section class="panel"><h2>Panel</h2><p>Panel <strong>children</strong> with expr.</p></section>
  <h1 class="title">4</h1><p>Some <strong>bold</strong> prose and an inline value: 42.</p><p>A component inline: <span class="greeting">Hello, inline! x2</span> — and standalone:</p><p><span class="greeting">Hello, World! x21</span></p><p>Host JSX inline: <b class="loud">rendered <strong>bold</strong></b>.</p><div class="box"><h2>Host block</h2><p>Markdown <strong>inside</strong> a host block.</p></div><section class="panel"><h2>Panel</h2><p>Panel <strong>children</strong> with expr.</p></section>
