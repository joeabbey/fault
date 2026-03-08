import type { PageLoad } from './$types';

export const load: PageLoad = async ({ params, fetch }) => {
	const response = await fetch(`/docs/${params.slug}.md`);
	if (!response.ok) {
		throw new Error(`Doc not found: ${params.slug}`);
	}
	const markdown = await response.text();
	return { slug: params.slug, markdown };
};
