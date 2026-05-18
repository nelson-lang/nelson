%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
missingPot = [tempdir(), 'nelson_missing_i18n_source.pot'];
jsonFile = [tempdir(), 'nelson_i18n_helpers_test.json'];
cmd = ['i18nHelpers(''convert'', ''', missingPot, ''', ''', jsonFile, ''')'];
assert_checkerror(cmd, ...
  [_('Cannot open input file: '), missingPot]);
%=============================================================================
potFile = [tempdir(), 'nelson_i18n_helpers_test.pot'];
content = ['msgid ""', newline(), ...
  '"first "', newline(), ...
  '"message"', newline(), ...
  'msgstr ""', newline(), ...
  newline(), ...
  'msgid "second"', newline(), ...
  'msgstr ""', newline()];
filewrite(potFile, content);
i18nHelpers('convert', potFile, jsonFile);
jsonContent = fileread(jsonFile);
assert_istrue(contains(jsonContent, '"first message"'));
assert_istrue(contains(jsonContent, '"second"'));
%=============================================================================
