import { defineCollection, z } from 'astro:content';

const blog = defineCollection({
  type: 'content',
  schema: z.object({
    title: z.string(),
    description: z.string().optional(),
    date: z.date(),
    tags: z.array(z.string()).optional(),
    draft: z.boolean().optional().default(false),
  }),
});

const projects = defineCollection({
  type: 'content',
  schema: z.object({
    projects: z.array(z.object({
      title: z.string(),
      description: z.string(),
      technologies: z.array(z.string()),
      link: z.string().url(),
      date: z.date().optional(),
    })),
  }),
});

export const collections = { blog, projects }; 