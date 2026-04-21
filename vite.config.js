import { defineConfig } from "vite";

export default defineConfig({
  // 'spa' (default) adds a catch-all fallback that returns index.html for
  // any path that doesn't match a file — which is why your txt loads were
  // returning HTML. 'mpa' disables that fallback entirely.
  // Static assets in public/ are served before this setting applies anyway,
  // but being explicit prevents the same confusion with any future routes.
  appType: "mpa",

  server: {
    port: 5173,
    // Return 404 for missing assets rather than falling back to index.html.
    // With appType: "mpa" this is the default behavior, but stating it
    // explicitly documents intent.
    fs: {
      // Allow serving files from the project root and public/.
      allow: [".."],
    },
  },

  build: {
    outDir: "dist",
    // Vite copies public/ to dist/ automatically on build.
    // No additional config needed for the books directory.
  },

  // If your PureScript output is compiled to output/ by spago and then
  // bundled, point Vite at the entry point. Adjust this path to match
  // whatever your build pipeline produces.
  // If you're using a separate bundler step (esbuild, parcel), remove this.
  resolve: {
    alias: {
      // Uncomment if your spago output lives somewhere non-standard:
      // "@output": "/absolute/path/to/output",
    },
  },
});