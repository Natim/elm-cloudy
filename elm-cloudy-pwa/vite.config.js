import { defineConfig } from "vite";

export default defineConfig({
  base: "/elm-cloudy/",
  publicDir: "public",
  build: {
    outDir: "dist",
    emptyOutDir: true,
  },
});
