%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
manifest = struct();
asserts.isfalse(__nelsonc_application_help__(manifest, {'--help'}));
manifest.helpText = 'CUSTOM_HELP 100% %s';
for flag = {'-?', '/?', '--help'}
  output = evalc('shown = __nelsonc_application_help__(manifest, flag);');
  asserts.istrue(shown);
  asserts.isequal(output, manifest.helpText);
end
for inputs = {{}, {'normal'}, {'--help', 'normal'}, {'normal', '--help'}, {'-h'}}
  asserts.isfalse(__nelsonc_application_help__(manifest, inputs{1}));
end
manifest.helpText = '';
asserts.istrue(__nelsonc_application_help__(manifest, {'--help'}));
for value = {42, true, {}, ['ab'; 'cd'], ['a', char(0)], repmat('x', 1, 1048577)}
  manifest.helpText = value{1};
  asserts.checkerror('__nelsonc_application_help__(manifest, {})', 'Invalid application help text.');
end
