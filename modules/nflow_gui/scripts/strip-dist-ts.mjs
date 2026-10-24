#!/usr/bin/env node
// Remove generated TypeScript artefacts from dist/libraries (release helper)
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const scriptDir = path.dirname(fileURLToPath(import.meta.url));
const distRoot = process.argv[2]
  ? path.resolve(process.argv[2])
  : path.join(scriptDir, '..', 'dist');

function removeIfExists(p) {
  try {
    if (fs.existsSync(p)) {
      fs.unlinkSync(p);
      console.log(`Removed ${p}`);
    }
  } catch (e) {
    console.warn(`Failed to remove ${p}: ${e && e.message ? e.message : e}`);
  }
}

function walkAndRemove(dir) {
  if (!fs.existsSync(dir)) return;
  const entries = fs.readdirSync(dir, { withFileTypes: true });
  for (const e of entries) {
    const full = path.join(dir, e.name);
    if (e.isDirectory()) walkAndRemove(full);
    else if (e.isFile() && /\.ts$/.test(e.name) && !/\.d\.ts$/.test(e.name)) removeIfExists(full);
  }
}

walkAndRemove(distRoot);
console.log('strip-dist-ts: done');
