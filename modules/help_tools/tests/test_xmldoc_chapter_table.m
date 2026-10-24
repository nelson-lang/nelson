%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% a <table> and a nested <b> in chapter_description must survive in the
% chapter summary page (index.html) and its markdown twin (README.md)
source = [modulepath('help_tools', 'root'), '/tests/fixtures/xml_chapter_table'];
asserts.istrue(xmldocchecker(source));
%=============================================================================
destination_html = tempname();
mkdir(destination_html);
[status, msg] = xmldoctohtml(source, destination_html, 'chapter_table_help', true);
asserts.istrue(status, msg);
toc_html = fileread([destination_html, '/index.html']);
asserts.istrue(contains(toc_html, '<table style="width:100%">'));
asserts.istrue(contains(toc_html, '<th>Function</th>'));
asserts.istrue(contains(toc_html, '<th>What it does</th>'));
asserts.istrue(contains(toc_html, '<td>Renders a chapter table in the summary.</td>'));
asserts.istrue(contains(toc_html, '<b>tablefun</b>'));
asserts.istrue(contains(toc_html, '<b>bold word</b>'));
asserts.isequal(numel(strfind(toc_html, '<tr>')), 2);
asserts.isequal(numel(strfind(toc_html, '<td>')), 2);
%=============================================================================
destination_md = tempname();
mkdir(destination_md);
asserts.istrue(xmldoctomd(source, destination_md, 'chapter_table_help', true));
readme = fileread([destination_md, '/README.md']);
asserts.istrue(contains(readme, '**bold word**'));
asserts.istrue(contains(readme, ['| Function | What it does |', newline, '| --- | --- |', newline]));
asserts.istrue(contains(readme, ['| **tablefun** | Renders a chapter table in the summary. |', newline]));
%=============================================================================
rmdir(destination_html, 's');
rmdir(destination_md, 's');
%=============================================================================
