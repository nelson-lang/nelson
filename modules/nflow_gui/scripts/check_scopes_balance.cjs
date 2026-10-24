const fs = require('fs');
const path = 'modules/nflow_gui/src/ts/blocks/renderers/scopes.ts';
try {
  const s = fs.readFileSync(path, 'utf8');
  const opens = (s.match(/{/g)||[]).length;
  const closes = (s.match(/}/g)||[]).length;
  const backticks = (s.match(/`/g)||[]).length;
  console.log('file:', path);
  console.log('opens:{', opens, 'closes:}', closes, 'backticks:', backticks);
  console.log('last 400 chars:\n', s.slice(-400));
} catch (err) {
  console.error(err);
  process.exit(1);
}
