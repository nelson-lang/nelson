%=============================================================================
% Copyright (c) 2017-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
jsonPath = [modulepath('error_manager'), '/i18n/en_US/errors.json'];
assert_istrue(isfile(jsonPath));
st = jsondecode(jsonPath, '-file');
fieldnames = fieldnames(st);
for i = 1:numel(fieldnames)
    fieldname = fieldnames{i};
    assert_isfalse(isempty(st.(fieldname)));
end
%=============================================================================
