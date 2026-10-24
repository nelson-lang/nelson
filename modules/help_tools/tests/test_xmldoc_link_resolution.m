%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <link linkend="..."> is resolved from the keyword index: pages in chapter
% sub-directories and pages whose file name differs from their keyword are
% reached at their real path, in the current module and across modules.
%=============================================================================
fixture = tempname();
cleanupFixture = onCleanup(@() removeFixture(fixture));
alpha = [fixture, '/alpha'];
beta = [fixture, '/beta'];
mkdir([alpha, '/1_basics']);
mkdir(beta);

chapter = ['<?xml version="1.0" encoding="UTF-8"?><xmldoc>', ...
  '<language>en_US</language><module_name>MODULE</module_name>', ...
  '<chapter>TITLE</chapter><chapter_description><p>', ...
  '<link linkend="${alpha}nested_page">Nested from chapter</link></p>', ...
  '</chapter_description></xmldoc>'];
filewrite([alpha, '/chapter.xml'], strrep(strrep(chapter, 'MODULE', 'alpha'), 'TITLE', 'Alpha'));
filewrite([beta, '/chapter.xml'], strrep(strrep(chapter, 'MODULE', 'beta'), 'TITLE', 'Beta'));
filewrite([alpha, '/1_basics/chapter.xml'], ['<?xml version="1.0" encoding="UTF-8"?><xmldoc>', ...
  '<language>en_US</language><chapter>Basics</chapter></xmldoc>']);

page = ['<?xml version="1.0" encoding="UTF-8"?><xmldoc>', ...
  '<language>en_US</language><keyword>KEYWORD</keyword>', ...
  '<short_description>DESCRIPTION</short_description>', ...
  '<description><p>Content.</p></description>', ...
  'SEE_ALSO</xmldoc>'];
see_also = ['<see_also>', ...
  '<see_also_item><link linkend="${alpha}nested_page">nested_page</link></see_also_item>', ...
  '<see_also_item><link linkend="nested_page">nested_page</link></see_also_item>', ...
  '<see_also_item><link linkend="${alpha}dotted.name">dotted.name</link></see_also_item>', ...
  '<see_also_item><link linkend="${beta}beta_page">beta_page</link></see_also_item>', ...
  '<see_also_item><link linkend="${alpha}beta_page">beta_page</link></see_also_item>', ...
  '<see_also_item><link linkend="${alpha}does_not_exist">does_not_exist</link></see_also_item>', ...
  '</see_also>'];
root_page = strrep(strrep(strrep(page, 'KEYWORD', 'root_page'), 'DESCRIPTION', 'Root page.'), 'SEE_ALSO', see_also);
filewrite([alpha, '/root_page.xml'], root_page);
nested_page = strrep(strrep(strrep(page, 'KEYWORD', 'nested_page'), 'DESCRIPTION', 'Nested page.'), ...
  'SEE_ALSO', '<see_also><see_also_item><link linkend="${alpha}root_page">root_page</link></see_also_item></see_also>');
filewrite([alpha, '/1_basics/nested_page.xml'], nested_page);
dotted_page = strrep(strrep(strrep(page, 'KEYWORD', 'dotted.name'), 'DESCRIPTION', 'Dotted keyword.'), 'SEE_ALSO', '');
filewrite([alpha, '/dotted_name.xml'], dotted_page);
% an alias declared by an earlier page must not capture the keyword of a real page
holder_page = strrep(strrep(strrep(page, 'KEYWORD', 'holder'), 'DESCRIPTION', 'Alias holder.'), 'SEE_ALSO', '');
holder_page = strrep(holder_page, '<keyword>holder</keyword>', ...
  '<keyword>holder</keyword><keyword_alias>dotted.name</keyword_alias>');
filewrite([alpha, '/aaa_holder.xml'], holder_page);
beta_page = strrep(strrep(strrep(page, 'KEYWORD', 'beta_page'), 'DESCRIPTION', 'Beta page.'), 'SEE_ALSO', '');
filewrite([beta, '/beta_page.xml'], beta_page);
asserts.istrue(xmldocchecker(alpha));
asserts.istrue(xmldocchecker(beta));

% markdown: alpha built with beta indexed
destination = [fixture, '/markdown/alpha'];
mkdir(destination);
[status, msg, warnings] = xmldocbuild(alpha, destination, 'alpha', 'md', true, {beta});
asserts.istrue(status, msg);
root_md = fileread([destination, '/root_page.md']);
asserts.istrue(contains(root_md, '[nested_page](../alpha/1_basics/nested_page.md)'));
asserts.istrue(contains(root_md, '[nested_page](1_basics/nested_page.md)'));
asserts.istrue(contains(root_md, '[dotted.name](../alpha/dotted_name.md)'));
asserts.istrue(contains(root_md, '[beta_page](../beta/beta_page.md)'));
asserts.isfalse(contains(root_md, 'alpha/nested_page.md)'));
asserts.isfalse(contains(root_md, 'dotted.name.md'));
% unknown target keeps the legacy path and is reported
asserts.istrue(contains(root_md, '[does_not_exist](../alpha/does_not_exist.md)'));
asserts.istrue(iscell(warnings));
asserts.istrue(any(contains(warnings, 'does_not_exist')));
% wrong module in the linkend: resolved in the module that defines the keyword
asserts.istrue(any(contains(warnings, '${alpha}beta_page')));
asserts.istrue(length(warnings) == 2);
% page in a sub-directory links back to the root with the right prefix
nested_md = fileread([destination, '/1_basics/nested_page.md']);
asserts.istrue(contains(nested_md, '[root_page](../../alpha/root_page.md)'));
% chapter description links of the module summary
readme = fileread([destination, '/README.md']);
asserts.istrue(contains(readme, '[Nested from chapter](../alpha/1_basics/nested_page.md)'));

% html output follows the same resolution
destination = [fixture, '/html/alpha'];
mkdir(destination);
[status, msg] = xmldoctohtml(alpha, destination, 'alpha', true, {beta});
asserts.istrue(status, msg);
root_html = fileread([destination, '/root_page.html']);
asserts.istrue(contains(root_html, 'href="../alpha/1_basics/nested_page.html"'));
asserts.istrue(contains(root_html, 'href="1_basics/nested_page.html"'));
asserts.istrue(contains(root_html, 'href="../alpha/dotted_name.html"'));
asserts.istrue(contains(root_html, 'href="../beta/beta_page.html"'));
nested_html = fileread([destination, '/1_basics/nested_page.html']);
asserts.istrue(contains(nested_html, 'href="../../alpha/root_page.html"'));
index_html = fileread([destination, '/index.html']);
asserts.istrue(contains(index_html, '<a href="../alpha/1_basics/nested_page.html">Nested from chapter</a>'));

% without index directories, cross-module links fall back to the legacy path
destination = [fixture, '/markdown_alone/alpha'];
mkdir(destination);
[status, msg, warnings] = xmldocbuild(alpha, destination, 'alpha', 'md', true);
asserts.istrue(status, msg);
root_md = fileread([destination, '/root_page.md']);
asserts.istrue(contains(root_md, '[nested_page](../alpha/1_basics/nested_page.md)'));
asserts.istrue(contains(root_md, '[beta_page](../beta/beta_page.md)'));
asserts.istrue(any(contains(warnings, '${beta}beta_page')));

clear cleanupFixture;
asserts.isfalse(isdir(fixture));
%=============================================================================
function removeFixture(directory)
  if isdir(directory)
    rmdir(directory, 's');
  end
end
%=============================================================================
