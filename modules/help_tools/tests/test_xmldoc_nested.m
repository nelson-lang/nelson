%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(~isempty(getenv('CONDA_PREFIX')) && ismac(), ...
  'Skipping xmltransform tests in conda environment on macOS due to libxslt issues.');
%=============================================================================
source = [modulepath('help_tools', 'root'), '/tests/xml_nested'];
assert_istrue(xmldocchecker(source));
%=============================================================================
destination_html = [tempdir(), 'test_xmldoc_nested_html'];
if isdir(destination_html)
  rmdir(destination_html, 's');
end
mkdir(destination_html);
[status, msg] = xmldoctohtml(source, destination_html, 'nested_help', true);
assert_istrue(status, msg);
assert_istrue(isfile([destination_html, '/plots/surf.html']));
assert_istrue(isfile([destination_html, '/plots/advanced/mesh.html']));
assert_istrue(isfile([destination_html, '/rootplot.html']));
assert_istrue(isfile([destination_html, '/surf.html']));
assert_istrue(isfile([destination_html, '/mesh.html']));
toc = fileread([destination_html, '/help_toc_summary.xml']);
assert_istrue(contains(toc, 'link="plots/surf.html"'));
assert_istrue(contains(toc, 'link="plots/advanced/mesh.html"'));
toc_html = fileread([destination_html, '/help_toc.html']);
plots_position = strfind(toc_html, 'Plots');
rootplot_position = strfind(toc_html, 'rootplot');
assert_istrue(plots_position(1) < rootplot_position(1));
surf_html = fileread([destination_html, '/plots/surf.html']);
assert_istrue(contains(surf_html, 'href="../index.html"'));
assert_istrue(contains(surf_html, 'href="../highlight.css"'));
mesh_html = fileread([destination_html, '/plots/advanced/mesh.html']);
assert_istrue(contains(mesh_html, 'href="../../index.html"'));
assert_istrue(contains(mesh_html, 'href="../../highlight.css"'));
alias_html = fileread([destination_html, '/surf.html']);
assert_istrue(contains(alias_html, 'plots/surf.html'));
%=============================================================================
destination_md = [tempdir(), 'test_xmldoc_nested_md'];
if isdir(destination_md)
  rmdir(destination_md, 's');
end
mkdir(destination_md);
assert_istrue(xmldoctomd(source, destination_md, 'nested_help', true));
assert_istrue(isfile([destination_md, '/plots/surf.md']));
assert_istrue(isfile([destination_md, '/plots/advanced/mesh.md']));
assert_istrue(isfile([destination_md, '/rootplot.md']));
assert_istrue(isfile([destination_md, '/surf.md']));
summary = fileread([destination_md, '/SUMMARY.md']);
assert_istrue(contains(summary, 'plots/surf.md'));
assert_isfalse(contains(summary, '](surf.md)'));
plots_position = strfind(summary, '- Plots');
rootplot_position = strfind(summary, 'rootplot.md');
assert_istrue(plots_position(1) < rootplot_position(1));
readme = fileread([destination_md, '/README.md']);
assert_istrue(contains(readme, '## Plots'));
assert_istrue(contains(readme, '### Advanced Plots'));
plots_position = strfind(readme, '## Plots');
functions_position = strfind(readme, '## Functions');
assert_istrue(plots_position(1) < functions_position(1));
%=============================================================================
destination_bad = [tempdir(), 'test_xmldoc_nested_bad'];
if isdir(destination_bad)
  rmdir(destination_bad, 's');
end
mkdir(destination_bad);
[status, msg] = xmldocbuild([modulepath('help_tools', 'root'), '/tests/xml_nested_no_chapter'], ...
  destination_bad, 'nested_help', 'html', true);
assert_isfalse(status);
assert_istrue(contains(msg, 'No chapter.xml'));
%=============================================================================
destination_collision = [tempdir(), 'test_xmldoc_nested_collision'];
if isdir(destination_collision)
  rmdir(destination_collision, 's');
end
mkdir(destination_collision);
[status, msg] = xmldocbuild([modulepath('help_tools', 'root'), '/tests/xml_nested_alias_collision'], ...
  destination_collision, 'nested_help', 'html', true);
assert_isfalse(status);
assert_istrue(contains(msg, 'alias collision'));
%=============================================================================
