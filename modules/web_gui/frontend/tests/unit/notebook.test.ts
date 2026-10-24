//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// @vitest-environment node
// (These are pure-logic tests; the node realm lets fflate recognize its own
// Uint8Array in the .nlx zip round-trip, which the jsdom realm breaks.)
import { describe, it, expect } from 'vitest';
import { blocksToIpynb, blocksToHtml, blocksToMarkdown } from '../../src/notebook/exporters';
import { controlAssignment } from '../../src/notebook/runMode';
import { serializeNlx, parseNlx } from '../../src/notebook/nlxFormat';
import {
  blocksToM,
  mToBlocks,
  mScriptToBlocks,
  isMarkedLiveM,
  blocksToDocx,
  ipynbToBlocks,
  blocksToIpynb as toIpynb,
} from '../../src/notebook/exporters';
import { unzipSync, strFromU8 } from 'fflate';
import { toggleNotebookMenu } from '../../src/notebook/menuState';
//=============================================================================
const sampleDoc = [
  { type: 'heading', props: { level: 1 }, content: [{ type: 'text', text: 'Notebook' }] },
  { type: 'paragraph', content: [{ type: 'text', text: 'Mixing text and code.' }] },
  { type: 'equation', props: { latex: 'y = \\sin(x)' } },
  {
    type: 'codecell',
    props: {
      code: 'x = linspace(0, 1, 10);\nplot(x)',
      output: 'ans =\n   42\n',
      figures: JSON.stringify(['data:image/png;base64,QUJD']),
      errorMsg: '',
    },
  },
];
//=============================================================================
describe('Notebook toolbar menus', () => {
  it('keeps only one menu open and toggles the active menu closed', () => {
    expect(toggleNotebookMenu(null, 'save')).toBe('save');
    expect(toggleNotebookMenu('save', 'insert')).toBe('insert');
    expect(toggleNotebookMenu('insert', 'insert')).toBeNull();
  });
});
//=============================================================================
describe('Notebook .ipynb export', () => {
  const nb = JSON.parse(blocksToIpynb(sampleDoc));

  it('produces a valid nbformat 4.5 envelope', () => {
    expect(nb.nbformat).toBe(4);
    expect(nb.nbformat_minor).toBe(5);
    expect(Array.isArray(nb.cells)).toBe(true);
    expect(nb.metadata.kernelspec.name).toBe('nelson');
  });

  it('gives every cell a unique id (required at minor >= 5)', () => {
    const ids = nb.cells.map((c: { id: string }) => c.id);
    expect(ids.every((id: string) => /^[a-zA-Z0-9-_]{1,64}$/.test(id))).toBe(true);
    expect(new Set(ids).size).toBe(ids.length);
  });

  it('merges text blocks into one markdown cell and keeps code as a code cell', () => {
    expect(nb.cells.map((c: { cell_type: string }) => c.cell_type)).toEqual(['markdown', 'code']);
    const code = nb.cells[1];
    expect(Array.isArray(code.source)).toBe(true);
    expect(code.execution_count).toBeNull();
  });

  it('serializes code source as per-line strings ending in newline', () => {
    const code = nb.cells[1];
    expect(code.source[0]).toBe('x = linspace(0, 1, 10);\n');
    expect(code.source[1]).toBe('plot(x)');
  });

  it('emits stdout as a stream output and the figure as display_data (png)', () => {
    const outputs = nb.cells[1].outputs;
    expect(outputs[0].output_type).toBe('stream');
    expect(outputs[0].name).toBe('stdout');
    const display = outputs.find((o: { output_type: string }) => o.output_type === 'display_data');
    expect(display).toBeDefined();
    expect(display.data['image/png']).toBe('QUJD');
  });
});
//=============================================================================
describe('Notebook control assignments', () => {
  const asg = (type: string, props: Record<string, unknown>) => controlAssignment({ type, props });

  it('slider / number field assign a scalar', () => {
    expect(asg('slider', { varName: 'A', value: 2.5 })).toBe('A = 2.5');
    expect(asg('numberfield', { varName: 'n', value: 7 })).toBe('n = 7');
  });

  it('check box / state button assign a logical', () => {
    expect(asg('checkbox', { varName: 'flag', checked: true })).toBe('flag = true');
    expect(asg('statebutton', { varName: 's', on: false })).toBe('s = false');
  });

  it('range slider assigns a 2-element vector', () => {
    expect(asg('rangeslider', { varName: 'r', low: 2, high: 8 })).toBe('r = [2 8]');
  });

  it('drop-down parses a numeric value, else quotes the string', () => {
    expect(asg('dropdown', { varName: 'k', value: '5' })).toBe('k = 5');
    expect(asg('textfield', { varName: 't', value: 'hello' })).toBe('t = "hello"');
  });

  it('color picker converts hex to a 0..1 RGB triplet', () => {
    expect(asg('colorpicker', { varName: 'c', value: '#ff0000' })).toBe('c = [1.000 0.000 0.000]');
  });

  it('variable selector aliases the picked variable, or nothing', () => {
    expect(asg('varselect', { varName: 'v', value: 'A' })).toBe('v = A');
    expect(asg('varselect', { varName: 'v', value: '' })).toBe('');
  });

  it('a control without a variable contributes nothing', () => {
    expect(asg('runbutton', { caption: 'Run' })).toBe('');
  });
});
//=============================================================================
describe('Notebook HTML / Markdown / Word export', () => {
  const doc = [
    { type: 'heading', props: { level: 1 }, content: [{ type: 'text', text: 'Title' }] },
    { type: 'paragraph', content: [{ type: 'text', text: 'Body & <tag>.' }] },
    { type: 'slider', props: { label: 'Amp', varName: 'A', value: 2 } },
    {
      type: 'codecell',
      props: {
        code: 'plot(x)',
        output: 'ok\n',
        figures: JSON.stringify(['data:image/png;base64,QUJD']),
      },
    },
  ];

  it('produces a standalone HTML document with escaped text and an image', () => {
    const html = blocksToHtml(doc);
    expect(html.startsWith('<!doctype html>')).toBe(true);
    expect(html).toContain('<h1>Title</h1>');
    expect(html).toContain('Body &amp; &lt;tag&gt;.');
    expect(html).toContain('<pre class="code">plot(x)</pre>');
    expect(html).toContain('<img src="data:image/png;base64,QUJD"');
    expect(html).toContain('Amp');
    expect(html).toContain('= 2');
  });

  it('produces Markdown with fenced code and an image reference', () => {
    const md = blocksToMarkdown(doc);
    expect(md).toContain('# Title');
    expect(md).toContain('```matlab\nplot(x)\n```');
    expect(md).toContain('![figure](data:image/png;base64,QUJD)');
  });
});
//=============================================================================
describe('Notebook plain-text .m round-trip', () => {
  it('serializes to a runnable .m with markers and parses back', () => {
    const doc = [
      { type: 'heading', props: { level: 2 }, content: [{ type: 'text', text: 'Section' }] },
      { type: 'paragraph', content: [{ type: 'text', text: 'Some text.' }] },
      { type: 'codecell', props: { code: 'x = 1;\ny = x + 1', output: 'drop', figures: '[]' } },
      { type: 'slider', props: { label: 'A', varName: 'A', value: 3 } },
    ];
    const m = blocksToM(doc);
    expect(m).toContain('%% [heading:2] Section');
    expect(m).toContain('%% [code]');
    expect(m).toContain('x = 1;');
    expect(m).toContain('%% [block] {"type":"slider"');

    const back = mToBlocks(m);
    expect(back.map((b) => b.type)).toEqual(['heading', 'paragraph', 'codecell', 'slider']);
    const code = back.find((b) => b.type === 'codecell');
    expect(code.props.code).toBe('x = 1;\ny = x + 1');
    expect(code.props.output).toBeUndefined(); // outputs are not stored in .m
    const slider = back.find((b) => b.type === 'slider');
    expect(slider.props.value).toBe(3);
  });
});
//=============================================================================
describe('Notebook — convert a plain .m script', () => {
  it('distinguishes a marked Notebook .m from a plain script', () => {
    expect(isMarkedLiveM('%% [heading:1] Title\n')).toBe(true);
    expect(isMarkedLiveM('%% Section one\nx = 1\n')).toBe(false);
    expect(isMarkedLiveM('x = 1;\nplot(x)')).toBe(false);
  });

  it('turns %% sections into headings, % comments into text, code into cells', () => {
    const m = [
      '%% Introduction',
      '% This script plots a line.',
      '% Second comment line.',
      'x = linspace(0, 1, 10);',
      'y = 2 * x;',
      '',
      '%% Plot',
      'plot(x, y)',
    ].join('\n');
    const blocks = mScriptToBlocks(m);
    expect(blocks.map((b) => b.type)).toEqual([
      'heading',
      'paragraph',
      'codecell',
      'heading',
      'codecell',
    ]);
    expect(blocks[0].content).toBe('Introduction');
    expect(blocks[1].content).toBe('This script plots a line.\nSecond comment line.');
    expect(blocks[2].props.code).toBe('x = linspace(0, 1, 10);\ny = 2 * x;');
    expect(blocks[3].content).toBe('Plot');
    expect(blocks[4].props.code).toBe('plot(x, y)');
  });

  it('handles a script with no sections as a single code cell', () => {
    const blocks = mScriptToBlocks('a = 1;\nb = a + 1\n');
    expect(blocks).toHaveLength(1);
    expect(blocks[0].type).toBe('codecell');
    expect(blocks[0].props.code).toBe('a = 1;\nb = a + 1');
  });
});
//=============================================================================
describe('Notebook .docx export', () => {
  const PNG1x1 =
    'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg==';
  const doc = [
    { type: 'heading', props: { level: 1 }, content: [{ type: 'text', text: 'T' }] },
    {
      type: 'codecell',
      props: {
        code: 'a = 1',
        output: 'x',
        figures: JSON.stringify([`data:image/png;base64,${PNG1x1}`]),
      },
    },
  ];

  it('produces a valid OOXML package with a word/document.xml body', () => {
    const bytes = blocksToDocx(doc);
    expect(bytes[0]).toBe(0x50);
    const files = unzipSync(bytes);
    expect(Object.keys(files)).toContain('[Content_Types].xml');
    expect(Object.keys(files)).toContain('word/document.xml');
    const xml = strFromU8(files['word/document.xml']);
    expect(xml).toContain('<w:document');
    expect(xml).toContain('a = 1');
  });

  it('embeds inline PNG figures (media part, relationship, and a drawing)', () => {
    const files = unzipSync(blocksToDocx(doc));
    expect(Object.keys(files).some((k) => /^word\/media\/fig\d+\.png$/.test(k))).toBe(true);
    const rels = strFromU8(files['word/_rels/document.xml.rels']);
    expect(rels).toContain('relationships/image');
    const xml = strFromU8(files['word/document.xml']);
    expect(xml).toContain('<w:drawing>');
    expect(xml).toContain('r:embed="rIdImg1"');
  });
});
//=============================================================================
describe('Notebook Jupyter import', () => {
  it('parses a notebook (exported by Nelson) back into blocks', () => {
    const doc = [
      { type: 'heading', props: { level: 2 }, content: [{ type: 'text', text: 'Sec' }] },
      { type: 'paragraph', content: [{ type: 'text', text: 'Hello.' }] },
      {
        type: 'codecell',
        props: {
          code: 'y = 1',
          output: 'done\n',
          figures: JSON.stringify(['data:image/png;base64,QUJD']),
        },
      },
    ];
    const nb = toIpynb(doc);
    const back = ipynbToBlocks(nb);
    expect(back).not.toBeNull();
    const types = back!.map((b) => b.type);
    expect(types).toContain('heading');
    expect(types).toContain('paragraph');
    expect(types).toContain('codecell');
    const code = back!.find((b) => b.type === 'codecell');
    expect(code.props.code).toBe('y = 1');
    expect(code.props.output).toContain('done');
    expect(JSON.parse(code.props.figures)[0]).toBe('data:image/png;base64,QUJD');
  });

  it('returns null for non-notebook input', () => {
    expect(ipynbToBlocks('not json')).toBeNull();
    expect(ipynbToBlocks('{"foo":1}')).toBeNull();
  });
});
//=============================================================================
describe('Notebook .nlx zip container', () => {
  const doc = [
    { type: 'heading', props: { level: 1 }, content: [{ type: 'text', text: 'Doc' }] },
    { type: 'slider', props: { label: 'A', varName: 'A', value: 2 } },
    {
      type: 'codecell',
      props: {
        code: 'plot(x)',
        output: 'done\n',
        figures: JSON.stringify(['data:image/png;base64,QUJD']),
        runRevision: 4,
        errorMsg: '',
      },
    },
  ];

  it('round-trips the document through a real zip', () => {
    const bytes = serializeNlx(doc);
    // zip local file header magic "PK\u0003\u0004"
    expect(bytes[0]).toBe(0x50);
    expect(bytes[1]).toBe(0x4b);
    const back = parseNlx(bytes);
    expect(back).not.toBeNull();
    expect(back!.map((b) => b.type)).toEqual(['heading', 'slider', 'codecell']);
    const code = back!.find((b) => b.type === 'codecell');
    expect(JSON.parse(code.props.figures)).toEqual(['data:image/png;base64,QUJD']);
    expect(code.props.output).toBe('done\n');
    expect(code.props.figureRefs).toBeUndefined();
  });

  it('keeps the source (document.json) apart from the output (output.json)', () => {
    const files = unzipSync(serializeNlx(doc));
    expect(files['document.json']).toBeDefined();
    expect(files['output.json']).toBeDefined();
    // document.json is source only: the code cell keeps its code, not its result
    const document = JSON.parse(strFromU8(files['document.json']));
    const srcCell = document.blocks.find((b: { type: string }) => b.type === 'codecell');
    expect(srcCell.props.code).toBe('plot(x)');
    expect(srcCell.props.output).toBeUndefined();
    expect(srcCell.props.figures).toBeUndefined();
    expect(srcCell.props.figureRefs).toBeUndefined();
    expect(srcCell.props.runRevision).toBeUndefined();
    // output.json holds the result; the figure bytes live in media/
    const output = JSON.parse(strFromU8(files['output.json']));
    const saved = Object.values(output.outputs)[0] as {
      output: string;
      figureRefs: { path: string }[];
    };
    expect(saved.output).toBe('done\n');
    expect(files[saved.figureRefs[0].path]).toBeDefined();
  });

  it('still reads a legacy plain-JSON .nlx', () => {
    const json = new TextEncoder().encode(JSON.stringify({ blocks: doc }));
    const back = parseNlx(json);
    expect(back).not.toBeNull();
    expect(back!.length).toBe(3);
  });
});
//=============================================================================
