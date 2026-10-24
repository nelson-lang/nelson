%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(~isfile([modulepath('compiler'), '/help/en_US/xml/chapter.xml']), 'Help XML sources are not available in this build');
ncc('--help');
root = modulepath('compiler');
english = tutorial_examples(root, 'en_US', 'compiler_standalone_tutorial');
french = tutorial_examples(root, 'fr_FR', 'compiler_standalone_tutorial');
asserts.isequal(english, french);
asserts.isequal(numel(english), 4);
run_examples(english);
english = tutorial_examples(root, 'en_US', 'compiler_embedded_data_tutorial');
french = tutorial_examples(root, 'fr_FR', 'compiler_embedded_data_tutorial');
asserts.isequal(english, french);
asserts.isequal(numel(english), 3);
run_data_examples(english);

function code = tutorial_examples(root, language, name)
  filename = fullfile(root, 'help', language, 'xml', [name, '.xml']);
  document = xmlread(filename, 'AllowDoctype', false);
  examples = document.getElementsByTagName('example_item_data');
  code = {};
  for k = 0:double(examples.getLength()) - 1
    code{end + 1} = char(examples.item(k).getTextContent());
  end
end

function run_data_examples(code)
  eval(code{1});
  cleanup = onCleanup(@() rmdir(work, 's'));
  eval(code{2});
  output = evalc(code{3});
  asserts.isequal(numel(strfind(output, 'EMBEDDED_SUM=10')), 2);
  asserts.isequal(result.RuntimeDirectory, '');
  asserts.istrue(isfolder(bundled.RuntimeDirectory));
  clear cleanup;
end

function run_examples(code)
  eval(code{1});
  cleanup = onCleanup(@() rmdir(work, 's'));
  for k = 2:numel(code)
    eval(code{k});
    if k > 2
      asserts.isequal(status, 0);
      asserts.istrue(contains(output, 'TUTORIAL_RESULT=30'));
    end
  end
  asserts.isequal(result.RuntimeDirectory, '');
  asserts.isequal(numel(result.Files), 1);
  asserts.isequal(numel(bundledResult.Files), 2);
  asserts.istrue(isfolder(bundledResult.RuntimeDirectory));
  asserts.isequal(options.RuntimeMode, 'installed');
  asserts.isequal(result.Options.RuntimeMode, 'installed');
  asserts.istrue(plan.complete);
  rmdir(source, 's');
  relocated = fullfile(work, 'relocated');
  mkdir(relocated);
  [ok, message] = movefile(fullfile(work, 'installed'), relocated);
  asserts.istrue(ok, message);
  [ok, message] = movefile(fullfile(work, 'bundled'), relocated);
  asserts.istrue(ok, message);
  previousRuntime = getenv('NELSONC_RUNTIME_ROOT');
  restoreRuntime = onCleanup(@() setenv('NELSONC_RUNTIME_ROOT', previousRuntime));
  setenv('NELSONC_RUNTIME_ROOT', nelsonroot());
  [parent, stem, extension] = fileparts(result.Executable);
  for mode = {'installed', 'bundled'}
    executable = fullfile(relocated, mode{1}, [stem, extension]);
    [status, output] = system(['"', executable, '" 5'], 60);
    asserts.istrue(status == 0, output);
    asserts.istrue(contains(output, 'TUTORIAL_RESULT=30'));
  end
  clear restoreRuntime;
  clear cleanup;
end
