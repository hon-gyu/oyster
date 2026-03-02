import esbuild from "esbuild";

const watch = process.argv.includes("--watch");

const ctx = await esbuild.context({
  entryPoints: ["src/main.ts"],
  bundle: true,
  external: ["obsidian", "fs", "path", "os"],
  format: "cjs",
  target: "es2018",
  outfile: "../data/vault/tt/.obsidian/plugins/link-resolver/main.js",
  sourcemap: "inline",
  logLevel: "info",
});

if (watch) {
  await ctx.watch();
  console.log("Watching for changes...");
} else {
  await ctx.rebuild();
  await ctx.dispose();
}
