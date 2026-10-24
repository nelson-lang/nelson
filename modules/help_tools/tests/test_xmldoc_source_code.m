%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <source_code> in Typst and Markdown: a whole-file reference (no start/end)
% is only cited by path, a bounded excerpt is printed verbatim.
%=============================================================================
source = tempname();
mkdir([source, '/snippets']);
filewrite([source, '/chapter.xml'], {'<?xml version="1.0" encoding="UTF-8"?>';
'<xmldoc>';
'  <language>en_US</language>';
'  <module_name>src_mod</module_name>';
'  <chapter>Source chapter</chapter>';
'</xmldoc>'});
filewrite([source, '/snippets/demo.m'], {'function demo()';
'  % BEGIN excerpt';
'  x = 1 + 2; % keep #this $verbatim';
'  % END excerpt';
'end'});
filewrite([source, '/with_source.xml'], {'<?xml version="1.0" encoding="UTF-8"?>';
'<xmldoc>';
'  <language>en_US</language>';
'  <keyword>with_source</keyword>';
'  <short_description>Page with source listings.</short_description>';
'  <description>';
'    <p>Whole file:</p>';
'    <source_code path="snippets/demo.m" label="Manifest"/>';
'    <p>Excerpt:</p>';
'    <source_code path="snippets/demo.m" label="Excerpt" language="matlab" start="BEGIN excerpt" end="END excerpt"/>';
'  </description>';
'</xmldoc>'});
%=============================================================================
destination = tempname();
mkdir(destination);
[status, msg] = xmldoctotypst(source, destination, 'src_mod', true);
asserts.istrue(status, msg);
content = fileread([destination, '/with_source.typ']);
% whole file: reference only, no listing
asserts.istrue(contains(content, '#source-ref("snippets/demo.m", title: "Manifest")'));
asserts.isfalse(contains(content, 'function demo()'));
% bounded excerpt: listing in a raw block, verbatim (no Typst escaping)
asserts.istrue(contains(content, '#source-code("snippets/demo.m", title: "Excerpt")['));
asserts.istrue(contains(content, ['``````matlab', newline, '  % BEGIN excerpt', newline, '  x = 1 + 2; % keep #this $verbatim', newline, '  % END excerpt', newline, '``````']));
rmdir(destination, 's');
%=============================================================================
destination = tempname();
mkdir(destination);
asserts.istrue(xmldoctomd(source, destination, 'src_mod', true));
content = fileread([destination, '/with_source.md']);
asserts.istrue(contains(content, '**Manifest:** `snippets/demo.m`'));
asserts.isfalse(contains(content, 'function demo()'));
asserts.istrue(contains(content, '<summary>Excerpt: <code>snippets/demo.m</code></summary>'));
asserts.istrue(contains(content, ['```matlab', newline, '  % BEGIN excerpt', newline, '  x = 1 + 2; % keep #this $verbatim', newline, '  % END excerpt', newline, '```']));
rmdir(destination, 's');
rmdir(source, 's');
%=============================================================================
