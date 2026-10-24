%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--TIMEOUT 600-->
% <--SEQUENTIAL TEST REQUIRED-->
skip_testsuite(~isfile([modulepath('compiler'), '/help/en_US/xml/chapter.xml']), 'Help XML sources are not available in this build');
ncc('--help');
root = modulepath('compiler');
english = examples(root, 'en_US');
french = examples(root, 'fr_FR');
asserts.isequal(english, french);
asserts.isequal(numel(english), 3);
if ~ispc() && ~ismac()
  run_examples(english);
end

function code = examples(root, language)
  filename = fullfile(root, 'help', language, 'xml', 'compiler_linux_runtime_tutorial.xml');
  doc = xmlread(filename, 'AllowDoctype', false);
  nodes = doc.getElementsByTagName('example_item_data');
  code = {};
  for k = 0:double(nodes.getLength()) - 1
    code{end + 1} = char(nodes.item(k).getTextContent());
  end
end

function run_examples(code)
  eval(code{1});
  cleanup = onCleanup(@() rmdir(work, 's'));
  eval(code{2});
  rmfile(entryA); rmfile(entryB);
  rmdir(first.Options.OutputDir, 's'); rmdir(second.Options.OutputDir, 's');
  output = evalc(code{3});
  asserts.istrue(contains(output, 'LINUX_SHARED_RUNTIME_TUTORIAL_OK'), output);
  clear cleanup;
end
