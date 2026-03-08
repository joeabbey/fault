import { sveltekit } from '@sveltejs/kit/vite';
import tailwindcss from '@tailwindcss/vite';
import { defineConfig } from 'vite';
import { searchForWorkspaceRoot } from 'vite';

export default defineConfig({
	plugins: [tailwindcss(), sveltekit()],
	server: {
		fs: {
			allow: [searchForWorkspaceRoot(process.cwd()), 'vendor']
		},
		proxy: {
			'/api': {
				target: 'http://localhost:8082',
				changeOrigin: true
			}
		}
	}
});
