%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
srcDir = [tempname(), '/'];
mkdir(srcDir);
cleanupSrcDir = onCleanup(@() rmdir(srcDir, 's')); %#ok<NASGU>
%=============================================================================
cppLines = {'#include "Error.hpp"'; ...
  'void f()'; ...
  '{'; ...
  '    const char* a = _("hello from cpp");'; ...
  '    Error("Nelson:test:alpha", "alpha template");'; ...
  '    Error("Nelson:test:dup", "first template");'; ...
  '    Warning("Nelson:test:dup", "second template");'; ...
  '}'};
filewrite([srcDir, 'sample.cpp'], strjoin(cppLines, newline()));
mLines = {'function y = sample(x)'; ...
  '  msg = _(''hello from m'');'; ...
  '  y = x'';'; ...
  '  error(''Nelson:test:beta'', ''beta template'');'; ...
  'end'};
filewrite([srcDir, 'sample.m'], strjoin(mLines, newline()));
%=============================================================================
uiJson = [tempname(), '.json'];
i18nHelpers('extractUI', srcDir, uiJson);
uiContent = fileread(uiJson);
asserts.istrue(contains(uiContent, '"hello from cpp"'));
asserts.istrue(contains(uiContent, '"hello from m"'));
asserts.isfalse(contains(uiContent, 'alpha template'));
%=============================================================================
errorsJson = [tempname(), '.json'];
i18nHelpers('extractErrors', srcDir, errorsJson);
errorsContent = fileread(errorsJson);
asserts.istrue(contains(errorsContent, '"Nelson:test:alpha": "alpha template"'));
asserts.istrue(contains(errorsContent, '"Nelson:test:beta": "beta template"'));
% an identifier mapped to several distinct templates is dropped (overloaded)
asserts.isfalse(contains(errorsContent, 'Nelson:test:dup'));
%=============================================================================
% reportDuplicates: same-module identifiers sharing one template are grouped.
dupErrors = [tempname(), '.json'];
dupLines = {'{'; ...
  '  "Nelson:mod:idOne": "%s expected.",'; ...
  '  "Nelson:mod:idTwo": "%s expected.",'; ...
  '  "Nelson:mod:other": "something else.",'; ...
  '  "Nelson:another:solo": "%s expected."'; ...
  '}'};
filewrite(dupErrors, strjoin(dupLines, newline()));
dupReport = [tempname(), '.txt'];
nbGroups = i18nHelpers('reportDuplicates', dupErrors, dupReport);
% only the two Nelson:mod ids share a template within the same module
asserts.isequal(nbGroups, 1);
reportContent = fileread(dupReport);
asserts.istrue(contains(reportContent, 'MODULE mod | TEMPLATE %s expected.'));
asserts.istrue(contains(reportContent, 'Nelson:mod:idOne'));
asserts.istrue(contains(reportContent, 'Nelson:mod:idTwo'));
% the cross-module solo id and the distinct-template id are not reported
asserts.isfalse(contains(reportContent, 'Nelson:another:solo'));
asserts.isfalse(contains(reportContent, 'Nelson:mod:other'));
%=============================================================================
rmfile(uiJson);
rmfile(errorsJson);
rmfile(dupErrors);
rmfile(dupReport);
%=============================================================================
invalidJson = [srcDir, 'not_a_dir/output.json'];
cmd = ['i18nHelpers(''extractUI'', ''', srcDir, ''', ''', invalidJson, ''')'];
asserts.checkerror(cmd, ['Cannot open output file: ', invalidJson]);
%=============================================================================
% 'namespace': keep only the keys with a given prefix (in place).
nsJson = [tempname(), '.json'];
filewrite(nsJson, ['{', newline(), ...
  '  "Nelson:mymod:one": "a",', newline(), ...
  '  "Nelson:mymod:two": "b",', newline(), ...
  '  "Nelson:other:three": "c"', newline(), '}', newline()]);
i18nHelpers('namespace', nsJson, 'Nelson:mymod:');
nsContent = fileread(nsJson);
asserts.istrue(contains(nsContent, 'Nelson:mymod:one'));
asserts.istrue(contains(nsContent, 'Nelson:mymod:two'));
asserts.isfalse(contains(nsContent, 'Nelson:other:three'));
rmfile(nsJson);
%=============================================================================
% 'register'/'unregister': [status, message] form, no error raised.
[st, msg] = i18nHelpers('register', 'nomodule_i18n_test', tempdir());
asserts.istrue(st);
asserts.isequal(msg, '');
[st2, msg2] = i18nHelpers('unregister', 'nomodule_i18n_test');
asserts.istrue(st2);
asserts.isequal(msg2, '');
%=============================================================================
