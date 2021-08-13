const PurescriptPlugin = require("esbuild-plugin-purescript");
const path = require("path");

const watch = process.env.DEV ? {
    onRebuild(error, result) {
        if (error) console.error('watch build failed:', error)
        else console.log('watch build succeeded:', result)
    }
} : null;

require('esbuild').build({
    entryPoints: ['src/index.js'],
    bundle: true,
    outfile: 'dist/out.js',
    plugins: [PurescriptPlugin()],
    watch,
    minify: true
}).catch(() => process.exit(1))