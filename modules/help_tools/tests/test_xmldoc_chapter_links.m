%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Chapter description links must survive both summary renderers.
fixture = tempname();
source = [fixture, '/source'];
mkdir([source, '/nested']);
cleanupFixture = onCleanup(@() removeFixture(fixture));
chapter = ['<?xml version="1.0" encoding="UTF-8"?><xmldoc>', ...
  '<language>en_US</language><module_name>chapter_links</module_name>', ...
  '<chapter>Chapter links</chapter><chapter_description>', ...
  '<p>Start with <link linkend="guide">Plain &amp; safe</link>, ', ...
  '<link linkend="${chapter_links}guide">Qualified</link> and ', ...
  '<link linkend="{chapter_links}nested/detail">Nested target</link>.</p>', ...
  '<table><tr><th>Topic</th></tr><tr><td>', ...
  '<link linkend="guide">Table target</link></td></tr></table>', ...
  '<p><link>Label without target</link></p>', ...
  '</chapter_description></xmldoc>'];
filewrite([source, '/chapter.xml'], chapter);
child = ['<?xml version="1.0" encoding="UTF-8"?><xmldoc>', ...
  '<language>en_US</language><chapter>Nested chapter</chapter>', ...
  '<chapter_description><p><link linkend="guide">Root from child</link>', ...
  '</p></chapter_description></xmldoc>'];
filewrite([source, '/nested/chapter.xml'], child);
page = ['<?xml version="1.0" encoding="UTF-8"?><xmldoc>', ...
  '<language>en_US</language><keyword>guide</keyword>', ...
  '<short_description>A real destination.</short_description>', ...
  '<description><p>Guide content.</p></description></xmldoc>'];
filewrite([source, '/guide.xml'], page);
filewrite([source, '/nested/detail.xml'], strrep(page, '<keyword>guide', '<keyword>detail'));
asserts.istrue(xmldocchecker(source));
asserts.istrue(xmldoclinkchecker(source));

destination = [fixture, '/html/chapter_links'];
mkdir(destination);
[status, msg] = xmldoctohtml(source, destination, 'chapter_links', true);
asserts.istrue(status, msg);
summary = fileread([destination, '/index.html']);
asserts.istrue(contains(summary, '<a href="guide.html">Plain &amp; safe</a>'));
asserts.istrue(contains(summary, '<a href="../chapter_links/guide.html">Qualified</a>'));
asserts.istrue(contains(summary, '<a href="../chapter_links/nested/detail.html">Nested target</a>'));
asserts.istrue(contains(summary, '<a href="guide.html">Table target</a>'));
asserts.istrue(contains(summary, '<a href="guide.html">Root from child</a>'));
asserts.isfalse(isempty(regexp(summary, '<p>\s*Label without target\s*</p>', 'match')));
asserts.isfalse(contains(summary, 'href=".html"'));
asserts.istrue(isfile([destination, '/guide.html']));
asserts.istrue(isfile([destination, '/../chapter_links/nested/detail.html']));

destination = [fixture, '/markdown/chapter_links'];
mkdir(destination);
asserts.istrue(xmldoctomd(source, destination, 'chapter_links', true));
summary = fileread([destination, '/README.md']);
asserts.istrue(contains(summary, '[Plain & safe](guide.md)'));
asserts.istrue(contains(summary, '[Qualified](../chapter_links/guide.md)'));
asserts.istrue(contains(summary, '[Nested target](../chapter_links/nested/detail.md)'));
asserts.istrue(contains(summary, '| [Table target](guide.md) |'));
asserts.istrue(contains(summary, '[Root from child](guide.md)'));
asserts.istrue(contains(summary, 'Label without target'));
asserts.isfalse(contains(summary, '](.md)'));
asserts.istrue(isfile([destination, '/guide.md']));
asserts.istrue(isfile([destination, '/../chapter_links/nested/detail.md']));
clear cleanupFixture;
asserts.isfalse(isdir(fixture));

function removeFixture(directory)
  if ~isdir(directory)
    return
  end
  target = fullpath(directory);
  parent = strrep(fullpath(fileparts(target)), '\', '/');
  temporaryRoot = regexprep(strrep(fullpath(tempdir()), '\', '/'), '/+$', '');
  if ispc()
    asserts.istrue(strcmpi(parent, temporaryRoot));
  else
    asserts.istrue(strcmp(parent, temporaryRoot));
  end
  % Only the canonical, newly created child of the temporary root is removed.
  rmdir(target, 's');
end
