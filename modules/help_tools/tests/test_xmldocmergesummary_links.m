%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% The merged help summary lives at the root of the help tree: links of chapter
% descriptions must be resolved to "./module/page.html" from the keyword index,
% including pages stored in chapter sub-directories and aliases.
%=============================================================================
fixture = tempname();
cleanupFixture = onCleanup(@() removeFixture(fixture));
alpha = [fixture, '/src/alpha'];
beta = [fixture, '/src/beta'];
mkdir([alpha, '/1_basics']);
mkdir(beta);

chapter = ['<?xml version="1.0" encoding="UTF-8"?><xmldoc>', ...
  '<language>en_US</language><module_name>MODULE</module_name>', ...
  '<chapter>TITLE</chapter><chapter_description><p>', ...
  '<link linkend="${alpha}nested_page">Nested</link>, ', ...
  '<link linkend="${beta}beta_page">Beta</link>, ', ...
  '<link linkend="root_page">Unqualified</link> and ', ...
  '<link linkend="${alpha}dotted.name">Dotted</link>.</p>', ...
  '</chapter_description></xmldoc>'];
filewrite([alpha, '/chapter.xml'], strrep(strrep(chapter, 'MODULE', 'alpha'), 'TITLE', 'Alpha'));
filewrite([beta, '/chapter.xml'], strrep(strrep(chapter, 'MODULE', 'beta'), 'TITLE', 'Beta'));
filewrite([alpha, '/1_basics/chapter.xml'], ['<?xml version="1.0" encoding="UTF-8"?><xmldoc>', ...
  '<language>en_US</language><chapter>Basics</chapter></xmldoc>']);
page = ['<?xml version="1.0" encoding="UTF-8"?><xmldoc>', ...
  '<language>en_US</language><keyword>KEYWORD</keyword>', ...
  '<short_description>A page.</short_description>', ...
  '<description><p>Content.</p></description></xmldoc>'];
filewrite([alpha, '/root_page.xml'], strrep(page, 'KEYWORD', 'root_page'));
filewrite([alpha, '/1_basics/nested_page.xml'], strrep(page, 'KEYWORD', 'nested_page'));
filewrite([alpha, '/dotted_name.xml'], strrep(page, 'KEYWORD', 'dotted.name'));
% an alias declared by an earlier page must not capture the keyword of a real page
filewrite([alpha, '/aaa_holder.xml'], strrep(page, '<keyword>KEYWORD</keyword>', ...
  '<keyword>holder</keyword><keyword_alias>dotted.name</keyword_alias>'));
filewrite([beta, '/beta_page.xml'], strrep(page, 'KEYWORD', 'beta_page'));

destination = [fixture, '/html'];
mkdir([destination, '/alpha']);
mkdir([destination, '/beta']);
[status, msg] = xmldoctohtml(alpha, [destination, '/alpha'], 'alpha', true, {beta});
asserts.istrue(status, msg);
[status, msg] = xmldoctohtml(beta, [destination, '/beta'], 'beta', true, {alpha});
asserts.istrue(status, msg);
alpha_toc = fileread([destination, '/alpha/help_toc_summary.xml']);
asserts.istrue(contains(alpha_toc, 'name="dotted.name" link="aaa_holder.html"'));
asserts.istrue(contains(alpha_toc, 'alias="true"'));

xmldocmergesummary(destination, {[destination, '/alpha/help_toc_summary.xml']; [destination, '/beta/help_toc_summary.xml']});
merged = fileread([destination, '/help_summary.xml']);
asserts.istrue(contains(merged, 'linkend="${alpha}nested_page" href="./alpha/1_basics/nested_page.html"'));
asserts.istrue(contains(merged, 'linkend="${beta}beta_page" href="./beta/beta_page.html"'));
asserts.istrue(contains(merged, 'linkend="root_page" href="./alpha/root_page.html"'));
asserts.istrue(contains(merged, 'linkend="${alpha}dotted.name" href="./alpha/dotted_name.html"'));

xmltransform([destination, '/help_summary.xml'], ...
  [modulepath('help_tools'), '/resources/nelson_summary2html.xslt'], [destination, '/summary.html']);
summary = fileread([destination, '/summary.html']);
asserts.istrue(contains(summary, '<a href="./alpha/1_basics/nested_page.html">Nested</a>'));
asserts.istrue(contains(summary, '<a href="./beta/beta_page.html">Beta</a>'));
asserts.istrue(contains(summary, '<a href="./alpha/dotted_name.html">Dotted</a>'));
% chapter links never use the module-relative form from the root summary
asserts.isfalse(contains(summary, 'href="../alpha'));
asserts.isfalse(contains(summary, 'href="../beta'));
asserts.istrue(isfile([destination, '/alpha/1_basics/nested_page.html']));

clear cleanupFixture;
asserts.isfalse(isdir(fixture));
%=============================================================================
function removeFixture(directory)
  if isdir(directory)
    rmdir(directory, 's');
  end
end
%=============================================================================
