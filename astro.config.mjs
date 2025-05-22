// @ts-check
import { defineConfig } from 'astro/config';
import mdx from '@astrojs/mdx';
import remarkMath from 'remark-math';
import rehypeKatex from 'rehype-katex';

// https://astro.build/config
export default defineConfig({
  integrations: [mdx()],
  markdown: {
    remarkPlugins: [remarkMath],
    rehypePlugins: [rehypeKatex],
    shikiConfig: {
      themes: {
        light: 'catppuccin-latte',
        dark: 'catppuccin-macchiato'
      },
    },
  },
  vite: {
    publicDir: 'public',
    build: {
      assetsInlineLimit: 0,
    },
  },
});
