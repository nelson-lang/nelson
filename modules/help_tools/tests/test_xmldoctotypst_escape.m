%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Characters that Typst markup interprets must be escaped in narrative text,
% kept verbatim in raw blocks, and strings must be valid Typst literals.
%=============================================================================
source = tempname();
mkdir(source);
chapter = {'<?xml version="1.0" encoding="UTF-8"?>';
'<xmldoc>';
'  <language>en_US</language>';
'  <module_name>escape_mod</module_name>';
'  <chapter>Escape chapter</chapter>';
'  <chapter_description><p>Uses <b>bold</b> and a 50% <i>ratio</i>.</p></chapter_description>';
'</xmldoc>'};
filewrite([source, '/chapter.xml'], chapter);
page = {'<?xml version="1.0" encoding="UTF-8"?>';
'<xmldoc>';
'  <language>en_US</language>';
'  <keyword>my_func</keyword>';
'  <short_description>Price is $5 #tag a_b *c* [d] &lt;e&gt; @f x//y back\slash</short_description>';
'  <syntax><syntax_item>y = my_func(a * b, "s")</syntax_item></syntax>';
'  <param_input><param_input_item><param_name>a_b</param_name>';
'    <param_description>a value: see <link linkend="${other_mod}other_fn">other_fn</link> and <code>x = "a\b"</code>.</param_description>';
'  </param_input_item></param_input>';
'  <description>';
'    <p>Formula <latex>\frac{a}{b}</latex> here.</p>';
'    <table><thead><tr><th>Name</th><th>Value</th></tr></thead>';
'      <tbody><tr><td>a|b</td><td>1</td></tr><tr><td colspan="2">both</td></tr></tbody></table>';
'    <ul><li>first</li><li>second</li></ul>';
'  </description>';
'  <examples><example_item><example_item_type>nelson</example_item_type>';
'    <example_item_description>Run it</example_item_description>';
'    <example_item_data><![CDATA[x = 1; % #not a tag, $not math_]]></example_item_data>';
'  </example_item></examples>';
'</xmldoc>'};
filewrite([source, '/my_func.xml'], page);
% a page name containing ".typ" inside: the extension must be stripped by length
dotted = {'<?xml version="1.0" encoding="UTF-8"?>';
'<xmldoc>';
'  <language>en_US</language>';
'  <keyword>calc.type</keyword>';
'  <short_description>Dotted name.</short_description>';
'  <see_also><see_also_item><link linkend="my_func">my_func</link></see_also_item></see_also>';
'</xmldoc>'};
filewrite([source, '/calc.type.xml'], dotted);
%=============================================================================
destination = tempname();
mkdir(destination);
[status, msg] = xmldoctotypst(source, destination, 'escape_mod', true);
asserts.istrue(status, msg);
content = fileread([destination, '/my_func.typ']);
%=============================================================================
asserts.istrue(contains(content, '= my\_func <escape_mod:my_func>'));
asserts.istrue(contains(content, 'Price is \$5 \#tag a\_b \*c\* \[d\] \<e\> \@f x\/\/y back\\slash'));
asserts.istrue(contains(content, '- #raw("y = my_func(a * b, \"s\")")'));
asserts.istrue(contains(content, '/ a\_b: a value: see #nlink(<other_mod:other_fn>)[other\_fn]; and #raw("x = \"a\\b\"");.'));
asserts.istrue(contains(content, '#latex("\\frac{a}{b}")'));
asserts.istrue(contains(content, 'columns: 2,'));
asserts.istrue(contains(content, 'table.header([Name], [Value], ),'));
asserts.istrue(contains(content, '[a|b], [1], '));
asserts.istrue(contains(content, 'table.cell(colspan: 2)[both], '));
asserts.istrue(contains(content, [newline, '- first']));
asserts.istrue(contains(content, [newline, '- second']));
% raw block content is verbatim
asserts.istrue(contains(content, ['``````matlab', newline, 'x = 1; % #not a tag, $not math_', newline, '``````']));
%=============================================================================
content = fileread([destination, '/main.typ']);
asserts.istrue(contains(content, 'Uses #strong[bold]; and a 50% #emph[ratio];.'));
asserts.istrue(contains(content, '- #nlink(<escape_mod:my_func>)[my\_func]: Price is \$5'));
asserts.istrue(contains(content, '- #nlink(<escape_mod:calc.type>)[calc.type]: Dotted name.'));
asserts.istrue(contains(content, '#include "calc.type.typ"'));
%=============================================================================
content = fileread([destination, '/calc.type.typ']);
asserts.istrue(contains(content, '= calc.type <escape_mod:calc.type>'));
asserts.istrue(contains(content, '#nlink(<escape_mod:my_func>)[my\_func]'));
content = fileread([destination, '/toc.typ']);
asserts.istrue(contains(content, '- #nlink(<escape_mod:calc.type>)[calc.type]'));
%=============================================================================
rmdir(destination, 's');
rmdir(source, 's');
%=============================================================================
