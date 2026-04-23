import { defineConfig } from "vite";
import fs from "fs";
import path from "path";

// Scans public/books/ at dev-server start and on file changes.
// Writes public/books/manifest.json — a plain JSON array of .txt filenames.
// Drop a file in public/books/ and the list updates without restarting Vite.
function bookManifestPlugin() {
  const booksDir    = path.resolve("public/books");
  const manifestPath = path.join(booksDir, "manifest.json");

  function writeManifest() {
    fs.mkdirSync(booksDir, { recursive: true });
    const files = fs.readdirSync(booksDir)
      .filter(f => f.toLowerCase().endsWith(".txt"))
      .sort();
    fs.writeFileSync(manifestPath, JSON.stringify(files, null, 2));
    console.log(`[book-manifest] ${files.length} book(s) indexed.`);
  }

  return {
    name: "book-manifest",
    // Runs before the dev server starts serving files.
    configureServer(server) {
      writeManifest();
      // Re-generate when files are added or removed from books/.
      server.watcher.on("add",   f => { if (f.includes("books")) writeManifest(); });
      server.watcher.on("unlink",f => { if (f.includes("books")) writeManifest(); });
    },
    // Also runs during `vite build`.
    buildStart() {
      writeManifest();
    },
  };
}

export default defineConfig({
  appType: "mpa",
  plugins: [bookManifestPlugin()],
});