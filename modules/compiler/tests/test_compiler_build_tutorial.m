%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--RELEASE ONLY-->
skip_testsuite(~isfile([modulepath('compiler'), '/help/en_US/xml/chapter.xml']), 'Help XML sources are not available in this build');
ncc('--help');
root = modulepath('compiler');
english = examples(root, 'en_US');
french = examples(root, 'fr_FR');
asserts.isequal(english, french);
asserts.isequal(numel(english), 3);
run_examples(english);

function code = examples(root, language)
  filename = fullfile(root, 'help', language, 'xml', 'compiler_build_tutorial.xml');
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
  asserts.istrue(isa(result, 'compiler.build.Results'));
  asserts.istrue(result.Options.TreatInputsAsNumeric);
  asserts.isequal(result.Options.SupportPackages, {'none'});
  asserts.isequal(numel(result.Files), 2);
  asserts.isequal(numel(external.Files), 3);
  asserts.isfalse(external.Options.EmbedArchive);
  asserts.istrue(endsWith(external.Files{2}, '.nca'));
  asserts.isequal(report.kind, 'nelson-standalone-build');
  asserts.isequal(report.applicationName, result.Options.ExecutableName);
  asserts.istrue(isfile(reportPath));
  asserts.istrue(height(result.RuntimeDependencies.Required) > 0);
  asserts.isequal(height(result.RuntimeDependencies.Optional), 0);
  if ispc()
    asserts.isequal(windowed.BuildType, 'standaloneWindowsApplication');
    asserts.isequal(numel(windowed.Files), 2);
  end
  rmdir(source, 's');
  output = evalc(code{3});
  expected = 2 * (2 + double(ispc())); % Console output and its appended log copy.
  asserts.isequal(numel(strfind(output, 'TUTORIAL_RESULT=30')), expected);
  clear cleanup;
end
