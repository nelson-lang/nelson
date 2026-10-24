%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
source = [modulepath('help_tools', 'root'), '/tests/xml_nested'];
asserts.istrue(xmldocchecker(source));
%=============================================================================
destination_html = tempname();
mkdir(destination_html);
[status, msg] = xmldoctohtml(source, destination_html, 'nested_help', true);
asserts.istrue(status, msg);
asserts.istrue(isfile([destination_html, '/plots/surf.html']));
asserts.istrue(isfile([destination_html, '/plots/advanced/mesh.html']));
asserts.istrue(isfile([destination_html, '/rootplot.html']));
asserts.isfalse(isfile([destination_html, '/surf.html']));
asserts.isfalse(isfile([destination_html, '/mesh.html']));
rootplot_html = fileread([destination_html, '/rootplot.html']);
asserts.istrue(contains(rootplot_html, 'root_plot_alias'));
asserts.isfalse(contains(rootplot_html, 'block:rootplot'));
toc = fileread([destination_html, '/help_toc_summary.xml']);
asserts.istrue(contains(toc, 'block:rootplot'));
asserts.istrue(contains(toc, 'link="plots/surf.html"'));
asserts.istrue(contains(toc, 'link="plots/advanced/mesh.html"'));
toc_html = fileread([destination_html, '/index.html']);
plots_position = strfind(toc_html, 'Plots');
rootplot_position = strfind(toc_html, 'rootplot');
asserts.istrue(plots_position(1) < rootplot_position(1));
% "block:" aliases are lookup-only entries: kept in the TOC XML (doc, nflow)
% but never rendered as duplicated index entries.
asserts.isfalse(contains(toc_html, 'block:rootplot'));
asserts.istrue(contains(toc_html, 'root_plot_alias'));
toc_sidebar_html = [destination_html, '/toc_sidebar.html'];
xmltransform([destination_html, '/help_toc_summary.xml'], ...
  [modulepath('help_tools'), '/resources/nelson_toc2html.xslt'], toc_sidebar_html);
toc_sidebar = fileread(toc_sidebar_html);
asserts.istrue(contains(toc_sidebar, '>rootplot<'));
asserts.isfalse(contains(toc_sidebar, 'block:rootplot'));
surf_html = fileread([destination_html, '/plots/surf.html']);
asserts.istrue(contains(surf_html, 'href="../index.html"'));
asserts.istrue(contains(surf_html, 'href="../highlight.css"'));
mesh_html = fileread([destination_html, '/plots/advanced/mesh.html']);
asserts.istrue(contains(mesh_html, 'href="../../index.html"'));
asserts.istrue(contains(mesh_html, 'href="../../highlight.css"'));
%=============================================================================
destination_md = tempname();
mkdir(destination_md);
asserts.istrue(xmldoctomd(source, destination_md, 'nested_help', true));
asserts.istrue(isfile([destination_md, '/plots/surf.md']));
asserts.istrue(isfile([destination_md, '/plots/advanced/mesh.md']));
asserts.istrue(isfile([destination_md, '/rootplot.md']));
asserts.isfalse(isfile([destination_md, '/surf.md']));
asserts.isfalse(isfile([destination_md, '/mesh.md']));
summary = fileread([destination_md, '/SUMMARY.md']);
asserts.istrue(contains(summary, 'plots/surf.md'));
asserts.isfalse(contains(summary, '](surf.md)'));
plots_position = strfind(summary, '- Plots');
rootplot_position = strfind(summary, 'rootplot.md');
asserts.istrue(plots_position(1) < rootplot_position(1));
asserts.isfalse(contains(summary, 'block:rootplot'));
readme = fileread([destination_md, '/README.md']);
asserts.isfalse(contains(readme, 'block:rootplot'));
asserts.istrue(contains(readme, '## Plots'));
asserts.istrue(contains(readme, '### Advanced Plots'));
plots_position = strfind(readme, '## Plots');
functions_position = strfind(readme, '## Functions');
asserts.istrue(plots_position(1) < functions_position(1));
%=============================================================================
destination_bad = tempname();
mkdir(destination_bad);
[status, msg] = xmldocbuild([modulepath('help_tools', 'root'), '/tests/xml_nested_no_chapter'], ...
  destination_bad, 'nested_help', 'html', true);
asserts.isfalse(status);
asserts.istrue(contains(msg, sprintf(_('No chapter.xml file in directory containing XML files: %s'), '')));
%=============================================================================
destination_collision = tempname();
mkdir(destination_collision);
[status, msg] = xmldocbuild([modulepath('help_tools', 'root'), '/tests/xml_nested_alias_collision'], ...
  destination_collision, 'nested_help', 'html', true);
asserts.istrue(status, msg);
asserts.istrue(isfile([destination_collision, '/foo.html']));
asserts.istrue(isfile([destination_collision, '/plots/foo.html']));
rmdir(destination_html, 's');
rmdir(destination_md, 's');
rmdir(destination_bad, 's');
rmdir(destination_collision, 's');
%=============================================================================
