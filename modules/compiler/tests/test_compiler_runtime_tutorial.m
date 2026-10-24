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
if ispc()
  run_examples(english);
end

function code = examples(root, language)
  doc = xmlread(fullfile(root, 'help', language, 'xml', 'compiler_runtime_tutorial.xml'), 'AllowDoctype', false);
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
  delete(entryA);
  delete(entryB);
  text = evalc(code{3});
  asserts.istrue(contains(text, 'SHARED_ONE_OK') && contains(text, 'SHARED_TWO_OK'), text);
  inventory = jsondecode(fileread(fullfile(target, 'runtime.json')));
  asserts.istrue(any(strcmp(inventory.modules, 'trigonometric_functions')));
  uninstall = dir(fullfile(target, 'unins*.exe'));
  asserts.isequal(numel(uninstall), 1);
  [status, output] = system(['"', fullfile(target, uninstall.name), '" /VERYSILENT /SUPPRESSMSGBOXES /NORESTART'], 90);
  asserts.istrue(status == 0, output);
  asserts.istrue(isfile(first.Files{1}) && isfile(second.Files{1}));
  clear cleanup;
end
